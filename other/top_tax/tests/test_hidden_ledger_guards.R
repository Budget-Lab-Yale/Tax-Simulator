#-------------------------------------------------------------------------------
# test_hidden_ledger_guards.R
#
# Unit tests for the hidden-ledger build (config/scenarios/behavior/wealth/
# avoidance.R + config/scenarios/behavior/evasion/debacker.R + the calc_estate
# concealment input): concealment-fraction math, positive-leg flow scaling with
# SECA companions, the R6 kg_lt overlay, the R4 estate_concealed_frac column,
# the R3 evasion->wealth link, the conservation identity, CHI env parsing, the
# CHI=0 no-op property, and every do_wealth hard-stop guard (order guard,
# missing net_worth MTR).
#
# Run via sbatch other/top_tax/tests/test_hidden_ledger_guards.sbatch (repo root).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

`%||%` = function(a, b) if (is.null(a)) b else a

# Column constants (estate.R is the single source of truth; wealth.R aliases it)
source('./src/calc/functions/tax/estate.R')
source('./src/calc/functions/tax/wealth.R')

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}
expect_error = function(expr, pattern, label) {
  err = tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  ok  = !is.null(err) && grepl(pattern, err, fixed = TRUE)
  if (!is.null(err) && !ok) cat('  got instead: ', substr(err, 1, 160), '\n')
  check(ok, label)
}

# Source the avoidance module with default env (no CHI overrides set)
Sys.unsetenv('WEALTH_CHI_PUB'); Sys.unsetenv('WEALTH_CHI_PRIV')
src_avoidance = function() sys.source('./config/scenarios/behavior/wealth/avoidance.R',
                                      envir = globalenv())
src_avoidance()

#-------------------------------------------------------------------------------
# 1. CHI env parsing (R7)
#-------------------------------------------------------------------------------

check(WEALTH_CHI_PUB == 1.0 && WEALTH_CHI_PRIV == 0.5,
      'CHI defaults: WEALTH_CHI_PUB = 1.0, WEALTH_CHI_PRIV = 0.5')

Sys.setenv(WEALTH_CHI_PUB = '0.0', WEALTH_CHI_PRIV = '0.75')
src_avoidance()
check(WEALTH_CHI_PUB == 0.0 && WEALTH_CHI_PRIV == 0.75,
      'CHI env override read (0.0 / 0.75)')
Sys.unsetenv('WEALTH_CHI_PUB'); Sys.unsetenv('WEALTH_CHI_PRIV')
src_avoidance()  # restore defaults

#-------------------------------------------------------------------------------
# Synthetic frame builder. Two asset classes, several income legs, weight/id.
#-------------------------------------------------------------------------------

zero_cols = function(cols) as_tibble(setNames(rep(list(0), length(cols)), cols))

make_frame = function(n) {
  base = tibble(id = 1:n, year = 2026L, weight = rep(100, n))
  # all balance-sheet columns default 0, all income legs default 0
  bs = zero_cols(c(WEALTH_MARKETABLE_COLS, WEALTH_CLOSELY_HELD_COLS,
                   WEALTH_DEBT_COLS))[rep(1, n), ]
  flows = zero_cols(c('txbl_int', 'div_ord', 'div_pref', 'kg_lt', 'kg_lt_basis',
                      'part_active', 'part_passive', 'scorp_active',
                      'scorp_passive', 'sole_prop', 'rent',
                      'sole_prop1', 'sole_prop2', 'part_se1', 'part_se2'))[rep(1, n), ]
  bind_cols(base, bs, flows)
}

# 5 records:
#  1: top, marketable + closely-held wealth + income legs        (concealment)
#  2: below exemption (mtr = 0)                                  (no-op)
#  3: top, closely-held income + evasion factor < 1             (evasion link)
#  4: top, LOSS legs (negative)                                 (losses untouched)
#  5: top, companion split legs (sole_prop1/2, part_se1/2)      (companions)
tu = make_frame(5)
tu$value.equities      = c(1e8, 4e7, 1e8, 1e8, 1e8)   # marketable
tu$value.pass_throughs = c(1e8, 4e7, 1e8, 1e8, 1e8)   # closely-held
tu$div_ord     = c(1e6, 5e5, 0,    -1e5, 0)
tu$txbl_int    = c(2e5, 1e5, 0,    0,    0)
tu$kg_lt       = c(3e6, 2e6, 0,    0,    0)
tu$kg_lt_basis = c(1e6, 0,   0,    0,    0)   # must be left untouched (R6)
tu$part_active = c(5e5, 3e5, 1e6,  -5e5, 4e5)
tu$part_se1    = c(0,   0,   0,    0,    3e5)
tu$part_se2    = c(0,   0,   0,    0,    1e5)
tu$sole_prop   = c(0,   0,   0,    0,    2e5)
tu$sole_prop1  = c(0,   0,   0,    0,    2e5)
tu$rent        = c(1e5, 0,   0,    0,    0)
tu$evasion_g_schc = c(1, 1, 1,   1, 1)
tu$evasion_g_pt   = c(1, 1, 0.9, 1, 1)   # record 3: 10% of PT income evaded
tu$evasion_g_rent = c(1, 1, 1,   1, 1)

static_mtrs = tibble(id = 1:5, year = 2026L,
                     mtr_net_worth = c(0.02, 0, 0.02, 0.02, 0.02))

si = function(modules = c('evasion/debacker', 'wealth/avoidance')) list(
  ID = 'hl_test',
  behavior_modules = modules,
  output_path = file.path(tempdir(), 'hl_guard_test'))

out = do_wealth(tu, NULL, static_mtrs, si(), NULL)

# Expected concealment fractions at the top rate
f_pub  = 1 - exp(0.02 * WEALTH_AVOID_PUBLIC_E)
f_priv = 1 - exp(0.02 * WEALTH_AVOID_PRIVATE_E)
c_pub  = 1.0 * f_pub
c_priv = 0.5 * f_priv

#-------------------------------------------------------------------------------
# 2. Flow concealment math (record 1)
#-------------------------------------------------------------------------------

check(abs(out$div_ord[1]  - 1e6 * (1 - c_pub)) < 1e-6, 'div_ord scaled by (1 - c_pub)')
check(abs(out$txbl_int[1] - 2e5 * (1 - c_pub)) < 1e-6, 'txbl_int scaled by (1 - c_pub)')
check(abs(out$kg_lt[1]    - 3e6 * (1 - c_pub)) < 1e-6, 'kg_lt scaled by (1 - c_pub) [R6 overlay]')
check(abs(out$part_active[1] - 5e5 * (1 - c_priv)) < 1e-6, 'part_active scaled by (1 - c_priv)')
check(abs(out$rent[1]        - 1e5 * (1 - c_priv)) < 1e-6, 'rent scaled by (1 - c_priv)')
check(out$kg_lt_basis[1] == 1e6, 'kg_lt_basis untouched (no basis change under R6)')

# estate_concealed_frac (record 1): (c_pub*mkt + c_priv*clh) / gross
ecf1 = (c_pub * 1e8 + c_priv * 1e8) / 2e8
check('estate_concealed_frac' %in% names(out), 'estate_concealed_frac column present')
check(abs(out$estate_concealed_frac[1] - ecf1) < 1e-9, 'estate_concealed_frac math (record 1)')

# net_worth (record 1, no evasion): mkt*exp(mtr*pub_e) + clh*exp(mtr*priv_e)
nw1 = 1e8 * exp(0.02 * WEALTH_AVOID_PUBLIC_E) + 1e8 * exp(0.02 * WEALTH_AVOID_PRIVATE_E)
check(abs(out$net_worth[1] - nw1) < 1e-3, 'reported net_worth = full avoidance response (record 1)')

#-------------------------------------------------------------------------------
# 3. Below-exemption no-op (record 2, mtr_net_worth = 0)
#-------------------------------------------------------------------------------

check(abs(out$div_ord[2] - 5e5) < 1e-9 && abs(out$part_active[2] - 3e5) < 1e-9,
      'below-exemption record: flows unchanged (mtr = 0 => c = 0)')
check(out$estate_concealed_frac[2] == 0, 'below-exemption record: estate_concealed_frac = 0')
check(abs(out$net_worth[2] - (4e7 + 4e7)) < 1e-6,
      'below-exemption record: net_worth = raw (no avoidance at mtr = 0)')

#-------------------------------------------------------------------------------
# 4. R3 evasion->wealth link (record 3: 10% of PT income evaded)
#-------------------------------------------------------------------------------

# Only closely-held term is shaved by evaded = 0.10; marketable term unaffected.
nw3 = 1e8 * exp(0.02 * WEALTH_AVOID_PUBLIC_E) +
      1e8 * exp(0.02 * WEALTH_AVOID_PRIVATE_E) * (1 - 0.10)
check(abs(out$net_worth[3] - nw3) < 1e-3,
      'evasion link shaves closely-held net_worth by evaded income share (0.10)')

#-------------------------------------------------------------------------------
# 5. Loss legs untouched (record 4: negative part_active / div_ord)
#-------------------------------------------------------------------------------

check(abs(out$part_active[4] - (-5e5)) < 1e-9 && abs(out$div_ord[4] - (-1e5)) < 1e-9,
      'negative (loss) legs are not concealed (positive-leg gated)')

#-------------------------------------------------------------------------------
# 6. SECA companions ride the parent gate (record 5)
#-------------------------------------------------------------------------------

check(abs(out$sole_prop[5]  - 2e5 * (1 - c_priv)) < 1e-6 &&
      abs(out$sole_prop1[5] - 2e5 * (1 - c_priv)) < 1e-6,
      'sole_prop + sole_prop1 co-scaled by (1 - c_priv)')
check(abs(out$part_active[5] - 4e5 * (1 - c_priv)) < 1e-6 &&
      abs(out$part_se1[5]    - 3e5 * (1 - c_priv)) < 1e-6 &&
      abs(out$part_se2[5]    - 1e5 * (1 - c_priv)) < 1e-6,
      'part_active + part_se1/2 co-scaled by (1 - c_priv)')

#-------------------------------------------------------------------------------
# 7. Module returns full frame; mtr / evasion temps dropped; diagnostics written
#-------------------------------------------------------------------------------

check(!('mtr_net_worth' %in% names(out)) &&
      !any(c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent') %in% names(out)),
      'joined MTR and transient evasion factors dropped from returned frame')
diag_path = file.path(si()$output_path, 'conventional', 'supplemental',
                      'hidden_ledger_2026.csv')
check(file.exists(diag_path), 'hidden_ledger diagnostics CSV written')
if (file.exists(diag_path)) {
  d = read_csv(diag_path, show_col_types = FALSE)
  check(d$chi_pub == 1.0 && d$chi_priv == 0.5 && d$conservation_max_leg_err < 1e-6,
        'diagnostics stamp CHI + conservation error within tolerance')
}

#-------------------------------------------------------------------------------
# 8. CHI = 0 no-op: concealment off, but reported net_worth still avoided
#-------------------------------------------------------------------------------

WEALTH_CHI_PUB  <<- 0
WEALTH_CHI_PRIV <<- 0
out0 = do_wealth(tu, NULL, static_mtrs, si(), NULL)
check(all(abs(out0$div_ord - tu$div_ord) < 1e-9) &&
      all(abs(out0$part_active - tu$part_active) < 1e-9) &&
      all(abs(out0$kg_lt - tu$kg_lt) < 1e-9),
      'CHI = 0: all reported flows identical to input (concealment off)')
check(all(out0$estate_concealed_frac == 0),
      'CHI = 0: estate_concealed_frac = 0 everywhere')
check(abs(out0$net_worth[1] - nw1) < 1e-3,
      'CHI = 0: reported net_worth STILL shrinks by the full avoidance response')
WEALTH_CHI_PUB  <<- 1.0
WEALTH_CHI_PRIV <<- 0.5

#-------------------------------------------------------------------------------
# 9. Guards
#-------------------------------------------------------------------------------

expect_error(
  do_wealth(tu, NULL, static_mtrs,
            si(c('wealth/avoidance', 'evasion/debacker')), NULL),
  'must run BEFORE',
  'guard: wealth/avoidance before evasion/ hard-stops (order guard)')

expect_error(
  do_wealth(tu, NULL, static_mtrs,
            si(c('wealth/avoidance', 'kg_dynamics/turnover')), NULL),
  'kg_dynamics must run BEFORE',
  'guard: wealth/avoidance before kg_dynamics hard-stops (R6 ordering)')

# kg_dynamics before wealth/avoidance (correct R6 order) must NOT stop
ok_kg = tryCatch({ do_wealth(tu, NULL, static_mtrs,
                             si(c('kg_dynamics/turnover', 'wealth/avoidance')), NULL); TRUE },
                 error = function(e) FALSE)
check(ok_kg, 'correct order (kg_dynamics before wealth) runs without error')

# Correct order must NOT stop
ok_order = tryCatch({ do_wealth(tu, NULL, static_mtrs,
                                si(c('evasion/debacker', 'wealth/avoidance')), NULL); TRUE },
                    error = function(e) FALSE)
check(ok_order, 'correct order (evasion before wealth) runs without error')

# wealth/avoidance alone (evasion_g_* columns absent) must NOT stop, and the
# R3 link must be inert (net_worth = full avoidance response, no extra shave)
tu_noev = tu %>% select(-starts_with('evasion_g_'))
out_alone = tryCatch(do_wealth(tu_noev, NULL, static_mtrs,
                               si(c('wealth/avoidance')), NULL),
                     error = function(e) NULL)
check(!is.null(out_alone), 'wealth/avoidance without an evasion module runs without error')
check(!is.null(out_alone) && abs(out_alone$net_worth[3] -
        (1e8 * exp(0.02 * WEALTH_AVOID_PUBLIC_E) +
         1e8 * exp(0.02 * WEALTH_AVOID_PRIVATE_E))) < 1e-3,
      'evasion link inert when evasion_g_* absent (evaded defaults to 0)')

expect_error(
  do_wealth(tu, NULL, static_mtrs %>% select(-mtr_net_worth), si(), NULL),
  'requires a registered',
  'guard: missing mtr_net_worth hard-stops')

expect_error(
  do_wealth(tu, NULL, NULL, si(), NULL),
  'requires a registered',
  'guard: NULL static_mtrs hard-stops')

unlink(si()$output_path, recursive = TRUE)

#-------------------------------------------------------------------------------

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL TESTS PASSED' else
            paste0(n_fail, ' TEST(S) FAILED')))
if (n_fail > 0) quit(status = 1)

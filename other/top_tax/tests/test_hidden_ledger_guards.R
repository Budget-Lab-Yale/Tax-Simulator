#-------------------------------------------------------------------------------
# test_hidden_ledger_guards.R
#
# Unit tests for the hidden-ledger build across its THREE modules
# (src/behavior/evasion/debacker.R, wealth/avoidance.R, and
# estate/avoidance.R -- the estate reporting response was split into its own
# module 2026-07-16): concealment-fraction math, positive-leg flow scaling with
# SECA companions, the R6 kg_lt overlay, the R4 estate_concealed_frac column,
# the R3 evasion->wealth link, the conservation identity, CHI env parsing, the
# CHI=0 no-op property, the KS estate own-rate response, and every hard-stop
# guard (order guards, missing MTRs, and the wealth->estate activation
# contract).
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

# Source the modules with default env (no CHI overrides set)
Sys.unsetenv('WEALTH_CHI_PUB'); Sys.unsetenv('WEALTH_CHI_PRIV')
src_avoidance = function() {
  sys.source('./src/behavior/wealth/avoidance.R', envir = globalenv())
  sys.source('./src/behavior/estate/avoidance.R', envir = globalenv())
}
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

# mtr_estate equal on both legs (ratio = 1) => the estate own-rate response
# is an exact no-op here, so every pre-existing expectation below is
# unchanged. The response itself is tested in section 11.
static_mtrs = tibble(id = 1:5, year = 2026L,
                     mtr_net_worth = c(0.02, 0, 0.02, 0.02, 0.02),
                     mtr_estate    = c(0.40, 0, 0.40, 0.40, 0.40))
base_mtrs   = tibble(id = 1:5, year = 2026L,
                     mtr_estate    = c(0.40, 0, 0.40, 0.40, 0.40))

si = function(modules = c('evasion/debacker', 'wealth/avoidance',
                          'estate/avoidance')) list(
  ID = 'hl_test',
  behavior_modules = modules,
  output_path = file.path(tempdir(), 'hl_guard_test'))

run_stack = function(frame, bm = base_mtrs, sm = static_mtrs, s = si()) {
  do_wealth(frame, bm, sm, s, NULL) %>% do_estate(bm, sm, s, NULL)
}

out_w = do_wealth(tu, base_mtrs, static_mtrs, si(), NULL)
out   = do_estate(out_w, base_mtrs, static_mtrs, si(), NULL)

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

# Handoff contract: do_wealth persists the concealment fractions + keeps the
# evasion factors for do_estate; do_estate consumes and drops them all.
check(all(c('wealth_c_pub', 'wealth_c_priv', 'evasion_g_schc') %in% names(out_w)),
      'do_wealth persists wealth_c_* and keeps evasion_g_* for do_estate')
check(abs(out_w$wealth_c_pub[1] - c_pub) < 1e-12 &&
      abs(out_w$wealth_c_priv[1] - c_priv) < 1e-12,
      'persisted wealth_c_* match the concealment fractions')

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
# Estate sees the union of wealth concealment and the 10% income-evasion link:
# c_priv + (1 - c_priv) * 0.10, with no overlap double-counted.
estate_c_priv3 = c_priv + (1 - c_priv) * 0.10
ecf3 = (c_pub * 1e8 + estate_c_priv3 * 1e8) / 2e8
check(abs(out$estate_concealed_frac[3] - ecf3) < 1e-9,
      'evasion link also hides closely-held assets from the estate base')

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
# 7. Returned frames; temps dropped end-to-end; diagnostics written
#-------------------------------------------------------------------------------

check(!('mtr_net_worth' %in% names(out)) &&
      !any(c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent',
             'wealth_c_pub', 'wealth_c_priv',
             'mtr_estate_S', 'mtr_estate_B') %in% names(out)),
      'joined MTRs and transient upstream factors dropped from final frame')
diag_path = file.path(si()$output_path, 'conventional', 'supplemental',
                      'hidden_ledger_2026.csv')
check(file.exists(diag_path), 'hidden_ledger (wealth) diagnostics CSV written')
if (file.exists(diag_path)) {
  d = read_csv(diag_path, show_col_types = FALSE)
  check(d$chi_pub == 1.0 && d$chi_priv == 0.5 && d$conservation_max_leg_err < 1e-6,
        'diagnostics stamp CHI + conservation error within tolerance')
}
ediag_path = file.path(si()$output_path, 'conventional', 'supplemental',
                       'estate_avoidance_2026.csv')
check(file.exists(ediag_path), 'estate_avoidance diagnostics CSV written')
if (file.exists(ediag_path)) {
  d = read_csv(ediag_path, show_col_types = FALSE)
  check(d$estate_report_eps == ESTATE_REPORT_EPS &&
        abs(d$estate_own_rate_f_wmean) < 1e-12,
        'estate diagnostics stamp eps; own-rate f = 0 under unchanged law')
}

#-------------------------------------------------------------------------------
# 8. CHI = 0 no-op: concealment off, but reported net_worth still avoided
#-------------------------------------------------------------------------------

WEALTH_CHI_PUB  <<- 0
WEALTH_CHI_PRIV <<- 0
out0 = run_stack(tu)
check(all(abs(out0$div_ord - tu$div_ord) < 1e-9) &&
      all(abs(out0$part_active - tu$part_active) < 1e-9) &&
      all(abs(out0$kg_lt - tu$kg_lt) < 1e-9),
      'CHI = 0: all reported flows identical to input (concealment off)')
# With CHI=0, wealth-tax concealment is off but the independent income-evasion
# link still hides record 3's closely-held assets from the estate base.
check(abs(out0$estate_concealed_frac[3] - 0.05) < 1e-9 &&
      all(out0$estate_concealed_frac[-3] == 0),
      'CHI = 0: estate concealment retains only the income-evasion link')
check(abs(out0$net_worth[1] - nw1) < 1e-3,
      'CHI = 0: reported net_worth STILL shrinks by the full avoidance response')
WEALTH_CHI_PUB  <<- 1.0
WEALTH_CHI_PRIV <<- 0.5

#-------------------------------------------------------------------------------
# 9. Guards
#-------------------------------------------------------------------------------

expect_error(
  do_wealth(tu, NULL, static_mtrs,
            si(c('wealth/avoidance', 'evasion/debacker', 'estate/avoidance')), NULL),
  'must run BEFORE',
  'guard: wealth/avoidance before evasion/ hard-stops (order guard)')

expect_error(
  do_wealth(tu, NULL, static_mtrs,
            si(c('wealth/avoidance', 'kg_dynamics/turnover', 'estate/avoidance')), NULL),
  'kg_dynamics must run BEFORE',
  'guard: wealth/avoidance before kg_dynamics hard-stops (R6 ordering)')

# NEW (activation contract): wealth/avoidance without estate/avoidance later
# in the stack hard-stops -- the exact misconfiguration behind the 2026-07-16
# estate activation bug.
expect_error(
  do_wealth(tu, base_mtrs, static_mtrs,
            si(c('evasion/debacker', 'wealth/avoidance')), NULL),
  'requires estate/avoidance LATER',
  'guard: wealth/avoidance without estate/avoidance hard-stops')
expect_error(
  do_wealth(tu, base_mtrs, static_mtrs,
            si(c('estate/avoidance', 'wealth/avoidance')), NULL),
  'requires estate/avoidance LATER',
  'guard: estate/avoidance BEFORE wealth/avoidance hard-stops (do_wealth side)')

# NEW: do_estate order guards (must run after evasion/ and wealth/)
expect_error(
  do_estate(tu, base_mtrs, static_mtrs,
            si(c('estate/avoidance', 'evasion/debacker')), NULL),
  'must run BEFORE',
  'guard: estate/avoidance before evasion/ hard-stops')
expect_error(
  do_estate(tu, base_mtrs, static_mtrs,
            si(c('estate/avoidance', 'wealth/avoidance')), NULL),
  'must run BEFORE',
  'guard: estate/avoidance before wealth/ hard-stops')

# kg_dynamics before wealth/avoidance (correct R6 order) must NOT stop
ok_kg = tryCatch({ run_stack(tu, s = si(c('kg_dynamics/turnover',
                                          'wealth/avoidance',
                                          'estate/avoidance'))); TRUE },
                 error = function(e) FALSE)
check(ok_kg, 'correct order (kg_dynamics before wealth before estate) runs without error')

# Correct order must NOT stop
ok_order = tryCatch({ run_stack(tu); TRUE }, error = function(e) FALSE)
check(ok_order, 'correct order (evasion, wealth, estate) runs without error')

# estate/avoidance ALONE (no upstream modules, no persisted columns) must run:
# both cross-base drivers default to inert and only the own-rate margin lives.
tu_noev = tu %>% select(-starts_with('evasion_g_'))
out_alone = tryCatch(do_estate(tu_noev, base_mtrs, static_mtrs,
                               si(c('estate/avoidance')), NULL),
                     error = function(e) NULL)
check(!is.null(out_alone) && all(out_alone$estate_concealed_frac == 0),
      'estate/avoidance alone: unchanged law + no upstream drivers => all zeros')

# wealth stack without an evasion module must run and leave the R3 link inert
out_wo_ev = tryCatch(run_stack(tu_noev,
                               s = si(c('wealth/avoidance', 'estate/avoidance'))),
                     error = function(e) NULL)
check(!is.null(out_wo_ev), 'wealth+estate without an evasion module runs without error')
check(!is.null(out_wo_ev) && abs(out_wo_ev$net_worth[3] -
        (1e8 * exp(0.02 * WEALTH_AVOID_PUBLIC_E) +
         1e8 * exp(0.02 * WEALTH_AVOID_PRIVATE_E))) < 1e-3,
      'evasion link inert when evasion_g_* absent (evaded defaults to 0)')
check(!is.null(out_wo_ev) && abs(out_wo_ev$estate_concealed_frac[3] -
        (c_pub + c_priv) / 2) < 1e-9,
      'estate evasion link inert when evasion_g_* absent')

#-------------------------------------------------------------------------------
# 10. Evasion net-of-tax denominator guard
#-------------------------------------------------------------------------------

sys.source('./src/behavior/evasion/debacker.R', envir = globalenv())
g_guard = evasion_response_factor(
  mtr          = c(0.4, 0.4, NA_real_, Inf),
  mtr_baseline = c(1.0, 1 - EVASION_NET_RATE_EPS / 2, 0.3, 0.3),
  e            = EVASION_E_PT)
check(all(g_guard == 1),
      'degenerate/non-finite evasion MTR inputs produce no response')
g_regular = evasion_response_factor(0.4, 0.3, EVASION_E_PT)
check(is.finite(g_regular) && g_regular > 0 && g_regular != 1,
      'regular evasion MTR inputs still produce a finite response')

expect_error(
  do_wealth(tu, base_mtrs, static_mtrs %>% select(-mtr_net_worth), si(), NULL),
  'requires a registered',
  'guard: missing mtr_net_worth hard-stops')

expect_error(
  do_wealth(tu, base_mtrs, NULL, si(), NULL),
  'requires a registered',
  'guard: NULL static_mtrs hard-stops')

#-------------------------------------------------------------------------------
# 11. Estate own-rate response (exact KS net-of-tax power form, do_estate)
#-------------------------------------------------------------------------------

# Guards: missing estate MTR legs hard-stop (now on do_estate)
expect_error(
  do_estate(out_w, base_mtrs, static_mtrs %>% select(-mtr_estate), si(), NULL),
  'estate own-rate response requires a registered',
  'guard: missing mtr_estate in static MTRs hard-stops')
expect_error(
  do_estate(out_w, NULL, static_mtrs, si(), NULL),
  'BASELINE MTRs',
  'guard: NULL baseline_mtrs hard-stops (estate leg is load-bearing)')
expect_error(
  do_estate(out_w, base_mtrs %>% select(-mtr_estate), static_mtrs, si(), NULL),
  'BASELINE MTRs',
  'guard: missing mtr_estate in baseline MTRs hard-stops')

# Ratio = 1 no-op is implicitly covered by every section above (both legs at
# 0.40); make it explicit once:
check(abs(out$estate_concealed_frac[1] -
          (c_pub * 1e8 + c_priv * 1e8) / 2e8) < 1e-9,
      'estate own-rate: unchanged estate law (ratio = 1) is an exact no-op')

# (i) Rate HIKE: tau 0.40 -> 0.50 on record 1. f = 1 - (0.5/0.6)^eps stacks
# multiplicatively on the retained share of the wealth/evasion union.
sm_hike = static_mtrs %>% mutate(mtr_estate = c(0.50, 0, 0.40, 0.40, 0.40))
out_h   = do_estate(out_w, base_mtrs, sm_hike, si(), NULL)
ret_h   = ((1 - 0.50) / (1 - 0.40)) ^ ESTATE_REPORT_EPS
union1  = (c_pub * 1e8 + c_priv * 1e8) / 2e8
exp_h   = 1 - (1 - union1) * ret_h
check(abs(out_h$estate_concealed_frac[1] - exp_h) < 1e-9,
      'estate own-rate: rate hike stacks multiplicatively on the union')
check(out_h$estate_concealed_frac[1] > union1,
      'estate own-rate: rate hike strictly increases the concealed fraction')
check(all(abs(out_h$estate_concealed_frac[-1] -
              out$estate_concealed_frac[-1]) < 1e-12),
      'estate own-rate: records with unchanged estate MTR are untouched')
# Firewall: the response must not touch flows or reported net worth
check(abs(out_h$net_worth[1] - out_w$net_worth[1]) < 1e-9 &&
      abs(out_h$kg_lt[1] - out_w$kg_lt[1]) < 1e-9,
      'estate own-rate: firewall (net_worth / flows unaffected)')

# (ii) REPEAL: tau 0.40 -> 0 on record 3 => retained > 1, previously
# unreported estate surfaces (combined fraction falls below the union).
sm_rep = static_mtrs %>% mutate(mtr_estate = c(0.40, 0, 0, 0.40, 0.40))
out_r  = do_estate(out_w, base_mtrs, sm_rep, si(), NULL)
ret_r  = (1 / (1 - 0.40)) ^ ESTATE_REPORT_EPS
estate_c_priv3 = c_priv + (1 - c_priv) * 0.10
union3 = (c_pub * 1e8 + estate_c_priv3 * 1e8) / 2e8
exp_r  = 1 - (1 - union3) * ret_r
check(abs(out_r$estate_concealed_frac[3] - exp_r) < 1e-9 &&
      out_r$estate_concealed_frac[3] < union3,
      'estate own-rate: repeal surfaces reported estate (retained > 1)')

# (iii) NEWLY TAXABLE: tau_B = 0 -> tau_S = 0.40 on record 2 (union = 0
# there: wealth MTR 0, no evasion) => fraction = 1 - (1 - 0.4)^eps exactly.
sm_new = static_mtrs %>% mutate(mtr_estate = c(0.40, 0.40, 0.40, 0.40, 0.40))
out_n  = do_estate(out_w, base_mtrs, sm_new, si(), NULL)
exp_n  = 1 - (1 - 0.40) ^ ESTATE_REPORT_EPS
check(abs(out_n$estate_concealed_frac[2] - exp_n) < 1e-9,
      'estate own-rate: newly taxable record uses (1 - tau_S)^eps directly')

# Returned frame drops the joined estate MTR legs
check(!any(c('mtr_estate_S', 'mtr_estate_B') %in% names(out_h)),
      'estate own-rate: joined estate MTR legs dropped from returned frame')

unlink(si()$output_path, recursive = TRUE)

#-------------------------------------------------------------------------------

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL TESTS PASSED' else
            paste0(n_fail, ' TEST(S) FAILED')))
if (n_fail > 0) quit(status = 1)

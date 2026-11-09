#-------------------------------------------------------------------------------
# test_sigma_guards.R
#
# Unit tests for the sigma conversion machinery (src/sim/sigma_conversion.R +
# config/scenarios/behavior/conversion/sigma.R): the shared pure function's
# gate/pool/wedge/clamp logic, the record-to-cell conservation identity, the
# record applier (SECA companions), the threshold extractor, and every
# hard-stop guard (kg required, pinned order, required MTRs, missing state,
# sigma stamp drift).
#
# Run via sbatch other/top_tax/tests/test_sigma_guards.sbatch (from repo root).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(data.table)
  library(tibble)
})

`%||%` = function(a, b) if (is.null(a)) b else a

source('./src/sim/cohort_bathtub.R')
for (f in sort(list.files('./src/sim/kg', full.names = TRUE))) source(f)
source('./src/sim/sigma_conversion.R')
sys.source('./config/scenarios/behavior/conversion/sigma.R',
           envir = environment())

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}
expect_error = function(expr, pattern, label) {
  err = tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  ok  = !is.null(err) && grepl(pattern, err, fixed = TRUE)
  if (!is.null(err) && !ok) cat('  got instead: ', substr(err, 1, 120), '\n')
  check(ok, label)
}

#-------------------------------------------------------------------------------
# 1. sigma_compute_conversions: gate / pool / wedge / clamp / NA safety
#-------------------------------------------------------------------------------

ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
tau_eq_B = setNames(rep(0.10, length(ages)), as.character(ages))
tau_eq_S = setNames(rep(0.12, length(ages)), as.character(ages))
thresholds = tibble(filing_status = c(1, 2, 4),
                    sigma_thresh  = c(500000, 600000, 500000))

pool = tibble(
  id            = 1:7,
  weight        = c(10, 10, 10, 10, 10, 10, 10),
  filing_status = c(2,  2,  1,  2,  2,  2,  2),
  age_cohort    = c(50, 55, 60, 50, 45, 50, 50),
  #                 gated  gated  below  no-biz gated  NA-mtr  huge-wedge
  wages1        = c(8e5,   5e5,   3e5,   9e5,   0,     4e5,    1e5),
  wages2        = c(1e5,   0,     0,     1e5,   0,     0,      0),
  part_active   = c(2e5,   0,     1e5,   0,     3e5,   2e5,    1e5),
  scorp_active  = c(0,     4e5,   0,     0,     0,     0,      0),
  sole_prop     = c(0,     0,     0,     0,     1e5,   0,      0),
  txbl_inc      = c(1e6,   7e5,   4e5,   1e6,   8e5,   1e6,    1e6),
  mtr_wages1        = c(0.42, 0.42, 0.37, 0.42, 0.42, NA,   0.37 + 60),
  mtr_wages2        = c(0.42, 0.42, 0.37, 0.42, 0.42, 0.42, 0.42),
  mtr_part_active   = c(0.45, 0.45, 0.40, 0.45, 0.45, 0.45, 0.45),
  mtr_scorp_active  = c(0.42, 0.42, 0.37, 0.42, 0.42, 0.42, 0.42),
  mtr_sole_prop1    = c(0.45, 0.45, 0.40, 0.45, 0.45, 0.45, 0.45),
  mtr_wages1_baseline       = c(0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37),
  mtr_wages2_baseline       = c(0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37),
  mtr_part_active_baseline  = c(0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40),
  mtr_scorp_active_baseline = c(0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37),
  mtr_sole_prop1_baseline   = c(0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40)
)

sig = 0.6
conv = sigma_compute_conversions(pool, thresholds, tau_eq_B, tau_eq_S, sig)

# Record 1: joint, gated. dW wage legs = 0.05 - 0.02 = 0.03; PT part leg
# dW = 0.05 - 0.02 = 0.03 on pool 0.75 * 2e5.
check(abs(conv$conv_w1[1] - sig * 0.03 * 8e5)        < 1e-9, 'wage1 conversion math')
check(abs(conv$conv_w2[1] - sig * 0.03 * 1e5)        < 1e-9, 'wage2 conversion math')
check(abs(conv$conv_part[1] - sig * 0.03 * 0.75 * 2e5) < 1e-9, '0.75 PT pool haircut')
check(conv$gate[1] && conv$gate[2],                   'gate admits qualifying records')
check(!conv$gate[3],                                  'gate rejects below-threshold record')
check(!conv$gate[4] && conv$conv_total[4] == 0,       'gate rejects no-active-business record')
check(conv$gate[5] && conv$conv_w1[5] == 0 && conv$conv_part[5] > 0,
      'zero wage leg contributes nothing; PT legs still convert')
check(conv$conv_w1[6] == 0 && conv$conv_part[6] > 0,  'NA MTR leg is inert, other legs live')
check(abs(conv$conv_w1[7] - 1e5) < 1e-9,              'conversion clamped at the full leg')

# Negative wedge (rate cut): conversion reverses, clamped at -leg
pool_neg = pool[1, ] %>%
  mutate(mtr_wages1 = 0.30, mtr_part_active = 0.33)
conv_neg = sigma_compute_conversions(pool_neg, thresholds, tau_eq_B, tau_eq_S, sig)
check(conv_neg$conv_w1 < 0 && abs(conv_neg$conv_w1 - sig * (-0.07 - 0.02) * 8e5) < 1e-9,
      'narrowing wedge produces negative conversion (memoryless, symmetric)')

# Conservation identity: sum of record conversions == sum of cell inflow
inflow = sigma_aggregate_inflow(conv, ages)
check(abs(sum(inflow) - sum(conv$weight * conv$conv_total)) < 1e-6,
      'record-to-cell conservation identity (aggregate)')
check(abs(inflow['50'] - sum(conv$weight[conv$age_cohort == 50] *
                             conv$conv_total[conv$age_cohort == 50])) < 1e-6,
      'record-to-cell conservation identity (single cell)')

#-------------------------------------------------------------------------------
# 2. sigma_apply_conversions: legs reduced, SECA companions co-scaled
#-------------------------------------------------------------------------------

tu = tibble(
  id = 1:2,
  wages1 = c(8e5, 5e5), wages2 = c(1e5, 0), wages = c(9.1e5, 5e5),
  part_active = c(2e5, 0), part_se1 = c(1.5e5, 0), part_se2 = c(0.5e5, 0),
  sole_prop = c(0, 1e5), sole_prop1 = c(0, 1e5), sole_prop2 = c(0, 0),
  scorp_active = c(0, 4e5)
)
cv = tibble(
  id = 1:2, conv_w1 = c(1e4, 0), conv_w2 = c(2e3, 0),
  conv_part = c(3e4, 0), conv_sole = c(0, 5e3), conv_scorp = c(0, 2e4)
)
applied = sigma_apply_conversions(tu, cv)
check(abs(applied$wages1[1] - (8e5 - 1e4)) < 1e-9,  'wages1 reduced')
check(abs(applied$wages[1]  - (9.1e5 - 1.2e4)) < 1e-9, 'wages reduced coherently (residual preserved)')
check(abs(applied$part_active[1] - 1.7e5) < 1e-9,   'part_active reduced')
check(abs(applied$part_se1[1] - 1.5e5 * (1.7e5 / 2e5)) < 1e-9 &&
      abs(applied$part_se2[1] - 0.5e5 * (1.7e5 / 2e5)) < 1e-9,
      'part_se1/2 co-scaled with part_active (SECA companions)')
check(abs(applied$sole_prop[2] - 9.5e4) < 1e-9 &&
      abs(applied$sole_prop1[2] - 9.5e4) < 1e-9,
      'sole_prop + sole_prop1 co-scaled')
check(abs(applied$scorp_active[2] - 3.8e5) < 1e-9,  'scorp_active reduced (no companion)')

#-------------------------------------------------------------------------------
# 3. sigma_top_thresholds
#-------------------------------------------------------------------------------

tl = expand_grid(year = 2026:2027, filing_status = 1:4) %>%
  mutate(`ord.brackets1` = 0, `ord.brackets2` = 50000,
         `ord.brackets7` = if_else(filing_status == 2, 800000, 650000))
th = sigma_top_thresholds(tl, 2026:2027)
check(nrow(th) == 8 && all(th$sigma_thresh[th$filing_status == 2] == 800000) &&
      all(th$sigma_thresh[th$filing_status != 2] == 650000),
      'sigma_top_thresholds picks the highest-indexed finite bracket')

expect_error(sigma_top_thresholds(tl %>% select(-starts_with('ord.')), 2026),
             'no ord.brackets columns',
             'threshold extractor stops without bracket columns')

#-------------------------------------------------------------------------------
# 4. do_conversion guards
#-------------------------------------------------------------------------------

mtr_frame = tibble(id = 1, year = 2026,
                   mtr_wages1 = 0.4, mtr_wages2 = 0.4, mtr_part_active = 0.4,
                   mtr_sole_prop1 = 0.4, mtr_scorp_active = 0.4)
tu_min = tibble(id = 1, year = 2026)

si = function(modules) list(
  ID = 'guard_test',
  behavior_modules = modules,
  output_path = file.path(tempdir(), 'sigma_guard_test'),
  years = 2026
)

expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame,
                si(c('conversion/sigma', 'evasion/debacker')), NULL),
  'requires kg_dynamics',
  'guard: sigma without kg_dynamics hard-stops')

expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame,
                si(c('conversion/sigma', 'kg_dynamics/turnover')), NULL),
  'pinned order',
  'guard: sigma before kg_dynamics hard-stops (wrong order)')

expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame,
                si(c('kg_dynamics/turnover', 'evasion/debacker',
                     'conversion/sigma')), NULL),
  'pinned order',
  'guard: evasion before sigma hard-stops (wrong order)')

expect_error(
  do_conversion(tu_min, mtr_frame %>% select(-mtr_scorp_active),
                mtr_frame %>% select(-mtr_scorp_active),
                si(c('kg_dynamics/turnover', 'conversion/sigma')), NULL),
  'mtr_scorp_active',
  'guard: missing required MTR hard-stops')

expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame,
                si(c('kg_dynamics/turnover', 'conversion/sigma')), NULL),
  'missing kg bathtub state file',
  'guard: missing state file hard-stops')

# 5. sigma stamp drift: state exists but tracker sigma differs from env
si_drift = si(c('kg_dynamics/turnover', 'conversion/sigma'))
state_dir = file.path(si_drift$output_path, 'conventional', 'supplemental',
                      'kg_dynamics_state')
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(regime = list(), cell_table = tibble(age = ages, tau_eq_B = 0.1,
                                                  tau_eq_S = 0.12),
             sigma = list(sigma = SIGMA_CONV + 0.1, conv_total = 0,
                          thresholds = thresholds)),
        file.path(state_dir, '2026.rds'))
expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame, si_drift, NULL),
  'SIGMA_CONV drift',
  'guard: sigma stamp drift between pre-pass and module hard-stops')

# 6. missing tracker (kg state written without sigma)
saveRDS(list(regime = list(), cell_table = tibble(age = ages, tau_eq_B = 0,
                                                  tau_eq_S = 0)),
        file.path(state_dir, '2026.rds'))
expect_error(
  do_conversion(tu_min, mtr_frame, mtr_frame, si_drift, NULL),
  'no sigma tracker',
  'guard: kg state without sigma tracker hard-stops')

unlink(si_drift$output_path, recursive = TRUE)

#-------------------------------------------------------------------------------

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL TESTS PASSED' else
            paste0(n_fail, ' TEST(S) FAILED')))
if (n_fail > 0) quit(status = 1)

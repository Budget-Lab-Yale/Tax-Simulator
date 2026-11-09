#-------------------------------------------------------------------------------
# eta_estate_check.R
#
# eta anchor re-check for the estate-margins build (plan verification #9,
# author-directed 2026-07-12). The estate offset puts (1 - e_B) on the
# CURRENT-LAW Bellman death value, so pass-2 responses to pure CG reforms
# shrink for estate-taxable cells — and eta = KG_DYN_DEFAULT_ETA (2.3992) was
# pinned as *the* long-run realization elasticity WITHOUT that term.
#
# This script re-measures the calibrator's long-run moment (calibrate.R
# step 4: +1pp uniform permanent CG perturbation, internal bathtub
# dlog(R)/dtau at sim-year 30, timeable-share invariant) three ways:
#   (1) e OFF (the pre-build measurement, reproduces the pinned anchor),
#   (2) e ON at the current eta — both passes get e_B (a CG perturbation
#       leaves estate law unchanged, so e_S = e_B), the production setup,
#   (3) the e-ON eta* that restores the e-OFF semi-elasticity (bisection) —
#       i.e. what eta would be re-pinned to.
# The full-sim dilution factor cancels in the (1)-vs-(2) comparison, so the
# DRIFT is measured cleanly even though the levels are internal.
#
# Usage: Rscript other/kg_model_tests/eta_estate_check.R <baseline_root>
#   <baseline_root> must contain baseline/static/detail/{year}.csv with
#   mtr_kg_lt AND mtr_estate_ded for >= 30 years (post-build baseline).
# Sbatch-only, never the login node.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE))))

return_vars <<- list()
list.files('./src', recursive = TRUE) %>%
  walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
         source(file.path('./src', .x)))

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Usage: Rscript other/kg_model_tests/eta_estate_check.R <baseline_root>')
}
BASELINE_ROOT = args[1]
MACRO_ROOT    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline'

AGES_BATHTUB = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN

LONG_RUN_PERTURB = 0.01
LONG_RUN_OFFSET  = 29
F_REF            = 0.5
ETA_CURRENT      = KG_DYN_DEFAULT_ETA

detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS = sort(as.integer(sub('\\.csv$', '', detail_files)))
stopifnot(length(YEARS) >= LONG_RUN_OFFSET + 1)
LONG_RUN_ANCHOR = YEARS[1] + LONG_RUN_OFFSET

cat(sprintf('Baseline root: %s (%d years, anchor %d); eta = %.4f\n',
            BASELINE_ROOT, length(YEARS), LONG_RUN_ANCHOR, ETA_CURRENT))

#--- Load cells, tau, and the estate exposure (calibrate.R step 1 + e) --------
td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
            'kg_lt', 'q_death1', 'q_death2',
            ESTATE_ASSET_COLS,
            KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS) %>% unique()
cpiu_by_year = kg_dyn_load_cpiu_levels(MACRO_ROOT, YEARS)

baseline_cells = list(); tau_B = list(); e_B = list()
for (t in YEARS) {
  td = file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv')) %>%
    fread(select = td_cols, showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(`pref.kg_sec121_excl` = if_else(filing_status == 2, 500000, 250000),
           `pref.kg_death_gain_excl` = 0,
           year = t) %>%
    kg_dyn_attach_record_attrs(cpiu_by_year = cpiu_by_year)
  bl = file.path(BASELINE_ROOT, 'baseline', 'static', 'detail',
                 paste0(t, '.csv')) %>%
    fread(select = c('id', 'mtr_kg_lt', 'mtr_estate_ded'),
          showProgress = FALSE) %>%
    as_tibble()
  j = td %>% inner_join(bl, by = 'id')
  baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(j, AGES_BATHTUB)
  tau_B[[as.character(t)]] = kg_dyn_aggregate_cell_mtr(j, AGES_BATHTUB)
  e_B  [[as.character(t)]] = kg_dyn_aggregate_cell_estate(j, AGES_BATHTUB)
}
cat('  loaded\n')

tau_S_long = lapply(tau_B, function(v) v + LONG_RUN_PERTURB)

life_ext    = kg_dyn_load_life_table_extension(years = YEARS)
grid_ext    = kg_dyn_build_extended_grid(baseline_cells, life_ext, YEARS,
                                         ages_bellman = AGES_BELLMAN)
grid_packed = kg_dyn_pack_baseline_grid(grid_ext, YEARS,
                                        ages_bellman = AGES_BELLMAN)
tau_B_mat      = kg_dyn_pack_tau(tau_B,      YEARS, ages_bellman = AGES_BELLMAN)
tau_S_long_mat = kg_dyn_pack_tau(tau_S_long, YEARS, ages_bellman = AGES_BELLMAN)
e_B_mat        = kg_dyn_pack_tau(e_B,        YEARS, ages_bellman = AGES_BELLMAN)
beta_by_year   = kg_dyn_load_beta_series(MACRO_ROOT, YEARS)

gw = pmax(baseline_cells[[as.character(YEARS[1])]]$G_B, 0)
cat(sprintf('  gain-weighted e_B (%d): %.4f; max cell %.4f\n', YEARS[1],
            sum(e_B[[1]] * gw) / sum(gw), max(e_B_mat)))

zero_route_vec = rep(0, length(AGES_BATHTUB))
A = build_aging_matrix(AGES_BATHTUB)
omega = kg_dyn_build_heir_matrix(
  heir_dist = rep(1 / length(AGES_BATHTUB), length(AGES_BATHTUB)),
  ages = AGES_BATHTUB)
bathtub_ages_chr = as.character(AGES_BATHTUB)

#--- Long-run response at a given eta, with or without e (calibrate.R step 4) --
eval_long_run = function(eta_val, use_e) {
  e_mat = if (use_e) e_B_mat else NULL
  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells, tau_S_mat = tau_S_long_mat,
    years = YEARS, tau_B_mat = tau_B_mat, timeable_share = F_REF,
    timing_window = KG_DYN_TIMING_WINDOW, ref_wedge = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub = AGES_BATHTUB)
  # BOTH passes get e_B: the CG perturbation leaves estate law unchanged
  # (e_S = e_B), exactly the production configuration for a pure CG reform.
  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               eta = eta_val, beta_by_year = beta_by_year,
                               e_mat = e_mat)
  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_long_mat, c_phi_mat = 0,
                               kappa_mat = pass1$kappa, eta = eta_val,
                               beta_by_year = beta_by_year, e_mat = e_mat)
  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  R_B_anchor = R_S_anchor = NA_real_
  for (j in seq_along(YEARS)) {
    t = YEARS[j]; bt = baseline_cells[[as.character(t)]]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t = bt, r_ordinary_S = pass2$r_D[bathtub_ages_chr, j],
      R_planned_B_col = planned_timing$R_planned_B[, j],
      R_planned_S_col = planned_timing$R_planned_S[, j])
    step = kg_dyn_step_recurrence(
      delta_prev = delta, baseline_t = bt, A = A, omega = omega,
      r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr),
      delta_route_vec = zero_route_vec)
    if (t == LONG_RUN_ANCHOR) {
      R_B_anchor = sum(bt$R_B)
      R_S_anchor = sum(step$r_S * (bt$G_B + delta))
    }
    delta = step$delta_next
  }
  log(R_S_anchor / R_B_anchor) / LONG_RUN_PERTURB
}

semi_off = eval_long_run(ETA_CURRENT, use_e = FALSE)
semi_on  = eval_long_run(ETA_CURRENT, use_e = TRUE)
drift    = (semi_on - semi_off) / abs(semi_off)
cat(sprintf('\nlong-run semi at eta = %.4f: e OFF %.4f | e ON %.4f | drift %+.2f%%\n',
            ETA_CURRENT, semi_off, semi_on, 100 * drift))

#--- eta* that restores the e-OFF semi under e ON (what a re-pin would set) ----
target = semi_off
lo = ETA_CURRENT * 0.5; hi = ETA_CURRENT * 2
f_lo = eval_long_run(lo, TRUE) - target   # response DECREASING in eta
f_hi = eval_long_run(hi, TRUE) - target
if (f_lo * f_hi > 0) {
  cat('bracket failed (drift beyond [0.5x, 2x] eta) — widen manually\n')
} else {
  for (i in 1:40) {
    mid = (lo + hi) / 2
    fm  = eval_long_run(mid, TRUE) - target
    if (abs(fm) < 1e-4 || (hi - lo) < 1e-4) break
    if ((f_lo > 0) == (fm > 0)) { lo = mid; f_lo = fm } else { hi = mid }
  }
  cat(sprintf('re-pin eta* (e ON, restores e-OFF semi): %.4f (current %.4f, %+.2f%%)\n',
              mid, ETA_CURRENT, 100 * (mid / ETA_CURRENT - 1)))
}
cat(sprintf('\nVERDICT: %s\n',
            if (abs(drift) < 0.01) 'drift < 1%: eta re-pin NOT required (record and keep 2.3992)'
            else 'drift >= 1%: re-pin eta per the spec-v3 migration protocol'))

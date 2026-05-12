#-------------------------------------------------------------------------------
# calibrate_eta.R
#
# Eta calibration for the kg_dynamics behavioral curvature parameter under
# the Bellman-based effective tax price (PDF Algorithms 1 and 2). Run AFTER
# a full-sample baseline simulator pass that registered mtr_vars = "kg_lt"
# (so baseline static detail carries mtr_kg_lt per record).
#
# Methodology:
#   1. Aggregate baseline cell MTRs (gain-stock-weighted) from baseline static
#      detail joined with Tax-Data.
#   2. Construct a uniform 1pp perturbation: tau_S(a, t) = tau_B(a, t) + 0.01
#      for every cell x year.
#   3. For a given candidate eta, run the Bellman pre-pass (life-table
#      extension, accrual rates, baseline Bellman, scenario Bellman) plus
#      the bathtub recurrence for 30 years under step-up regime (c_phi = 0).
#   4. Compute year-30 implied elasticity:
#        eta_30 = log(R_S / R_B) / log((tau_avg_B + 0.01) / tau_avg_B)
#      where tau_avg_B is the realization-weighted average baseline cell MTR
#      across all cells x all years.
#   5. Bisect eta until eta_30 hits -0.6.
#
# Why offline: the Bellman + bathtub takes a fraction of a second per
# iteration; bisection converges in ~12 iters. Total runtime ~seconds.
#
# Output: prints recommended eta. Paste into KG_DYN_DEFAULT_ETA in
# src/sim/kg_dynamics.R.
#
# CLI:
#   Rscript other/kg_model_tests/calibrate_eta.R <baseline_root>
#
#   <baseline_root> is the path to a full-sample Tax-Simulator vintage that
#   contains baseline/static/detail/{year}.csv with mtr_kg_lt for years
#   2026..2055. Typically the staging output of slurm_run.sh on a runscript
#   with mtr_vars=kg_lt registered for baseline.
#-------------------------------------------------------------------------------


suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(magrittr)
})

# Source production utilities (kg_dyn_*)
source('./src/sim/kg_dynamics.R')


#-------------------------------------------------------------------------------
# Inputs
#-------------------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Usage: Rscript other/kg_model_tests/calibrate_eta.R <baseline_root>')
}
BASELINE_ROOT = args[1]

TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline'
AGES_BATHTUB  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN
TARGET_ETA    = -0.62
PERTURBATION  = 0.01      # 1pp uniform MTR perturbation

# Derive YEARS from what the baseline run actually has on disk. Anchor the
# elasticity check at year 30 if available, otherwise the last sim year.
detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS         = sort(as.integer(sub('\\.csv$', '', detail_files)))
ANCHOR_YEAR   = if (length(YEARS) >= 30) min(YEARS) + 29 else max(YEARS)
ANCHOR_LABEL  = ANCHOR_YEAR - min(YEARS) + 1


#-------------------------------------------------------------------------------
# Step 1: Load Tax-Data + baseline MTRs, aggregate to cell quantities
#-------------------------------------------------------------------------------

cat("Loading Tax-Data and baseline MTRs for", length(YEARS), "years (full sample)...\n")

td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
            'kg_lt', 'q_death1', 'q_death2',
            KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

baseline_cells = list()
tau_B          = list()

for (t in YEARS) {

  td = file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv')) %>%
    fread(select = td_cols, showProgress = FALSE) %>%
    as_tibble() %>%
    kg_dyn_attach_record_attrs()

  bl = file.path(BASELINE_ROOT, 'baseline', 'static', 'detail',
                 paste0(t, '.csv')) %>%
    fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
    as_tibble()

  td_with_mtr = td %>% inner_join(bl, by = 'id')

  baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td_with_mtr, AGES_BATHTUB)
  tau_B[[as.character(t)]]          = kg_dyn_aggregate_cell_mtr(td_with_mtr, AGES_BATHTUB)
}

cat("  loaded\n")


#-------------------------------------------------------------------------------
# Step 2: Construct perturbation (tau_S = tau_B + 0.01 per cell)
#-------------------------------------------------------------------------------

tau_S = lapply(tau_B, function(v) v + PERTURBATION)


#-------------------------------------------------------------------------------
# Step 3: Build Bellman pre-pass inputs that don't depend on eta
#-------------------------------------------------------------------------------

cat("Building extended grid, accrual rates, tau matrices...\n")

life_ext    = kg_dyn_load_life_table_extension(years = YEARS)
grid_ext    = kg_dyn_build_extended_grid(baseline_cells, life_ext, YEARS,
                                         ages_bellman = AGES_BELLMAN)
g_mat       = kg_dyn_compute_growth_rates(grid_ext, YEARS,
                                          ages_bellman = AGES_BELLMAN,
                                          rule = KG_DYN_G_RULE)
grid_packed = kg_dyn_pack_baseline_grid(grid_ext, YEARS,
                                        ages_bellman = AGES_BELLMAN)
tau_B_mat   = kg_dyn_pack_tau(tau_B, YEARS, ages_bellman = AGES_BELLMAN)
tau_S_mat   = kg_dyn_pack_tau(tau_S, YEARS, ages_bellman = AGES_BELLMAN)

# Step-up regime applies under current law; c_phi = 0 throughout. We bisect
# eta using a 1pp perturbation under step-up to match the elasticity target.
c_phi_S_by_year = rep(0, length(YEARS))


#-------------------------------------------------------------------------------
# Step 4: Compute gain-stock-weighted average baseline tau across all years
# (used as the elasticity denominator anchor)
#-------------------------------------------------------------------------------

# Realization-weighted: numerator weights cell tau by R_B (realized gains)
# rather than G_B (unrealized stock). Realizers concentrate at higher MTRs,
# so this anchor is higher than the stock-weighted equivalent and gives the
# elasticity calibration the right denominator.
num = sum(sapply(YEARS, function(t) {
  sum(baseline_cells[[as.character(t)]]$R_B *
      as.numeric(tau_B[[as.character(t)]]))
}))
den = sum(sapply(YEARS, function(t) sum(baseline_cells[[as.character(t)]]$R_B)))
TAU_AVG_B = num / den

cat(sprintf("Average baseline tau (realization-weighted, across all years): %.4f\n", TAU_AVG_B))
cat(sprintf("Anchor: log((tau_avg_B + perturbation)/tau_avg_B) = %.5f\n",
            log((TAU_AVG_B + PERTURBATION) / TAU_AVG_B)))


#-------------------------------------------------------------------------------
# Step 5: Recurrence + Bellman loop, parameterized by eta. Step-up regime.
#-------------------------------------------------------------------------------

A     = kg_dyn_build_aging_matrix(AGES_BATHTUB)
omega = kg_dyn_build_heir_matrix(AGES_BATHTUB)
bathtub_ages_chr = as.character(AGES_BATHTUB)

eta_at_anchor = function(eta_val) {

  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, g_mat, tau_B_mat,
                                         c_phi_B = 0,
                                         eta = eta_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta = KG_DYN_BETA)
  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, g_mat, tau_S_mat,
                                         mu_mat = pass1$mu,
                                         c_phi_S_by_year = c_phi_S_by_year,
                                         eta = eta_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta = KG_DYN_BETA)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  R_B_total = 0
  R_S_total = 0

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]
    bt = baseline_cells[[as.character(t)]]

    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    lambda_I = KG_DYN_PHI_I * bt$r_B
    r_S_vec  = setNames(lambda_I + r_D_S_bt, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev  = delta,
      baseline_t  = bt,
      A           = A,
      omega       = omega,
      r_S_vec     = r_S_vec,
      delta_route = 0,
      phi_I       = KG_DYN_PHI_I
    )

    if (t == ANCHOR_YEAR) {
      G_S = bt$G_B + delta
      R_B_total = sum(bt$R_B)
      R_S_total = sum(step$r_S * G_S)
    }

    delta = step$delta_next
  }

  log(R_S_total / R_B_total) / log((TAU_AVG_B + PERTURBATION) / TAU_AVG_B)
}


#-------------------------------------------------------------------------------
# Step 6: Bisect eta to hit target
#-------------------------------------------------------------------------------

cat(sprintf("\nCalibrating eta to hit elasticity = %.2f at sim-year %d (calendar %d)...\n\n",
            TARGET_ETA, ANCHOR_LABEL, ANCHOR_YEAR))

# Coarse grid sweep. The Bellman P is more responsive than the old bracket P
# (anticipation channel), so eta needs to be smaller in magnitude. Wider grid
# to be safe.
eta_grid = c(0.5, 1, 2, 4, 8, 16, 32, 64)
cat("Coarse sweep:\n")
sweep_results = sapply(eta_grid, function(e) {
  v = eta_at_anchor(e)
  cat(sprintf("  eta = %6.2f  ->  eta_30 = %.4f\n", e, v))
  v
})

below = which(sweep_results > TARGET_ETA)   # less negative
above = which(sweep_results < TARGET_ETA)   # more negative
if (length(below) == 0 || length(above) == 0) {
  stop("Grid does not bracket target. Extend eta_grid or check inputs.")
}

i_lo = max(below); i_hi = min(above)
e_lo = eta_grid[i_lo]; e_hi = eta_grid[i_hi]

cat(sprintf("\nBracketing: [%.2f, %.2f]\n", e_lo, e_hi))

for (iter in 1:30) {
  e_mid = (e_lo + e_hi) / 2
  v_mid = eta_at_anchor(e_mid)
  cat(sprintf("  iter %2d  eta = %.5f  eta_30 = %.5f\n", iter, e_mid, v_mid))
  if (abs(v_mid - TARGET_ETA) < 1e-4) break
  if (v_mid > TARGET_ETA) e_lo = e_mid else e_hi = e_mid
}

eta_star = (e_lo + e_hi) / 2
final    = eta_at_anchor(eta_star)

cat(sprintf("\nCalibrated eta = %.4f  (eta_30 = %.4f, target = %.4f)\n",
            eta_star, final, TARGET_ETA))
cat("\nUpdate KG_DYN_DEFAULT_ETA in src/sim/kg_dynamics.R to this value.\n",
    sep = "")


#-------------------------------------------------------------------------------
# Bonus: profile of implied semi-elasticity by year (1pp perturbation)
#-------------------------------------------------------------------------------

profile_years = function(eta_val) {

  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, g_mat, tau_B_mat,
                                         c_phi_B = 0,
                                         eta = eta_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta = KG_DYN_BETA)
  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, g_mat, tau_S_mat,
                                         mu_mat = pass1$mu,
                                         c_phi_S_by_year = c_phi_S_by_year,
                                         eta = eta_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta = KG_DYN_BETA)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  out = tibble(sim_year = integer(), year = integer(), eta_t = numeric())

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]; bt = baseline_cells[[as.character(t)]]
    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    lambda_I = KG_DYN_PHI_I * bt$r_B
    r_S_vec  = setNames(lambda_I + r_D_S_bt, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec, 0, KG_DYN_PHI_I)
    G_S = bt$G_B + delta
    R_B_t = sum(bt$R_B)
    R_S_t = sum(step$r_S * G_S)
    eta_t = log(R_S_t / R_B_t) / log((TAU_AVG_B + PERTURBATION) / TAU_AVG_B)
    out = bind_rows(out, tibble(sim_year = j, year = t, eta_t = round(eta_t, 4)))
    delta = step$delta_next
  }
  out
}

cat("\nElasticity profile by sim year (calibrated eta):\n")
print(as.data.frame(profile_years(eta_star) %>%
                      filter(sim_year %in% c(1, 5, 10, 20, 30))),
      row.names = FALSE)

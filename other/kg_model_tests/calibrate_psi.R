#-------------------------------------------------------------------------------
# calibrate_psi.R
#
# Calibrates the global curvature parameter psi of the kg_dynamics
# representative-cell Bellman to a semi-elasticity target. Run AFTER a
# full-sample baseline simulator pass that registered mtr_vars = "kg_lt"
# (so baseline static detail carries mtr_kg_lt per record).
#
# Methodology:
#   1. Aggregate baseline cell MTRs (realization-weighted) from baseline
#      static detail joined with Tax-Data.
#   2. Construct a uniform 1pp perturbation: tau_S(a, t) = tau_B(a, t) + 0.01
#      for every cell x year.
#   3. For a given candidate psi, run the Bellman pre-pass (life-table
#      extension, baseline Bellman, scenario Bellman) plus the bathtub
#      recurrence under step-up regime (c_phi = 0).
#   4. Compute the implied aggregate semi-elasticity at sim year 30:
#        semi_elast_30 = log(R_S_30 / R_B_30) / (tau_S_30 - tau_B_30)
#      where tau_*_30 is the realization-weighted average cell MTR at
#      sim year 30 (= baseline start year + 29). The semi-elasticity
#      ramps over the first ~10 years as the bathtub accumulates stock
#      pressure, then plateaus; year 30 is safely in the permanent-
#      response steady state that the literature target reflects.
#   5. Bisect psi until the year-30 semi_elast hits the target.
#
# Target: dlog(R)/dtau = -0.6 / 0.238 ~= -2.52. Semi-elasticity wrt tau,
# matching the convention used by the legacy reduced-form module kg/62.R
# (literature arc elasticity -0.62 evaluated at a baseline tau of 0.238).
# The 0.238 anchor is a fixed convention; do NOT re-evaluate it at the
# current Tax-Data's realization-weighted tau.
#
# Required: the baseline run must have at least 30 years of static detail
# (anchoring on a shorter horizon would lock in a transient, not the
# permanent response). The script will fail-fast otherwise.
#
# Output: prints recommended psi. Paste into KG_DYN_DEFAULT_PSI in
# src/sim/kg_dynamics.R.
#
# CLI:
#   Rscript other/kg_model_tests/calibrate_psi.R <baseline_root>
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
  stop('Usage: Rscript other/kg_model_tests/calibrate_psi.R <baseline_root>')
}
BASELINE_ROOT = args[1]

TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline'
AGES_BATHTUB  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN
TARGET        = -0.6 / 0.238   # semi-elasticity dlog(R)/dtau target (~-2.52)
PERTURBATION  = 0.01           # 1pp uniform MTR perturbation

# Derive YEARS from what the baseline run actually has on disk. The
# anchor is fixed at sim year 30: the semi-elasticity ramps over the
# first ~10 years as the bathtub accumulates stock pressure, then
# plateaus; year 30 gets us safely into the permanent-response steady
# state that the literature target reflects. A shorter horizon would
# anchor on a transient response, so fail fast.
detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS         = sort(as.integer(sub('\\.csv$', '', detail_files)))
if (length(YEARS) < 30) {
  stop(sprintf(
    paste0('calibrate_psi requires at least 30 years of baseline data ',
           '(anchoring on the permanent semi-elasticity at sim year 30); ',
           'found %d years at %s. Re-run with a 30-year baseline (e.g. ',
           'tests/kg_dynamics_baseline_30yr.csv).'),
    length(YEARS), BASELINE_ROOT))
}
ANCHOR_YEAR   = min(YEARS) + 29
ANCHOR_LABEL  = 30


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
# Step 3: Build Bellman pre-pass inputs that don't depend on psi
#-------------------------------------------------------------------------------

cat("Building extended grid, tau matrices...\n")

life_ext    = kg_dyn_load_life_table_extension(years = YEARS)
grid_ext    = kg_dyn_build_extended_grid(baseline_cells, life_ext, YEARS,
                                         ages_bellman = AGES_BELLMAN)
grid_packed = kg_dyn_pack_baseline_grid(grid_ext, YEARS,
                                        ages_bellman = AGES_BELLMAN)
tau_B_mat   = kg_dyn_pack_tau(tau_B, YEARS, ages_bellman = AGES_BELLMAN)
tau_S_mat   = kg_dyn_pack_tau(tau_S, YEARS, ages_bellman = AGES_BELLMAN)

# Step-up regime applies under current law; c_phi = 0 throughout.
c_phi_S_by_year = rep(0, length(YEARS))


#-------------------------------------------------------------------------------
# Step 4: Compute realization-weighted average baseline tau across all years
# (used for the dtau denominator at the anchor)
#-------------------------------------------------------------------------------

num = sum(sapply(YEARS, function(t) {
  sum(baseline_cells[[as.character(t)]]$R_B *
      as.numeric(tau_B[[as.character(t)]]))
}))
den = sum(sapply(YEARS, function(t) sum(baseline_cells[[as.character(t)]]$R_B)))
TAU_AVG_B = num / den

cat(sprintf("Average baseline tau (realization-weighted, across all years): %.4f\n",
            TAU_AVG_B))
cat(sprintf("Target semi-elasticity at 1pp perturbation: dlog(R)/dtau = %.4f\n",
            TARGET))


#-------------------------------------------------------------------------------
# Step 5: Recurrence + Bellman loop, parameterized by psi. Step-up regime.
#-------------------------------------------------------------------------------

A     = kg_dyn_build_aging_matrix(AGES_BATHTUB)
omega = kg_dyn_build_heir_matrix(AGES_BATHTUB)
bathtub_ages_chr = as.character(AGES_BATHTUB)

semi_at_anchor = function(psi_val) {

  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, tau_B_mat,
                                         c_phi_B = 0,
                                         psi   = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta  = KG_DYN_BETA)
  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, tau_S_mat,
                                         kappa_mat = pass1$kappa,
                                         c_phi_S_by_year = c_phi_S_by_year,
                                         psi   = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta  = KG_DYN_BETA)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  R_B_total = 0
  R_S_total = 0

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]
    bt = baseline_cells[[as.character(t)]]

    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    lambda_T = KG_DYN_PHI_I * bt$r_B
    r_S_vec  = setNames(lambda_T + r_D_S_bt, bathtub_ages_chr)

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

  log(R_S_total / R_B_total) / PERTURBATION
}


#-------------------------------------------------------------------------------
# Step 6: Bisect psi to hit target
#-------------------------------------------------------------------------------

cat(sprintf("\nCalibrating psi to hit semi-elasticity = %.4f at sim-year %d (calendar %d)...\n\n",
            TARGET, ANCHOR_LABEL, ANCHOR_YEAR))

# Coarse grid sweep. At the static interior FOC dr_D/dtau ~ -1/psi, so for
# a -2.5 semi-elasticity with baseline r_D ~ 0.02, psi is in the tens.
# Sweep wide to be safe and find a bracket (continuation-value effects
# bend the local slope, so empirically the answer is a bit larger).
psi_grid = c(0.8, 1.6, 3.2, 6.4, 12.8, 25.6, 51.2, 102.4)
cat("Coarse sweep:\n")
sweep_results = sapply(psi_grid, function(p) {
  v = semi_at_anchor(p)
  cat(sprintf("  psi = %6.3f  ->  semi_elast = %.4f\n", p, v))
  v
})

# semi_elast is negative; target is negative; bigger |target| means smaller psi.
# As psi increases, semi_elast moves toward 0 (less negative).
above = which(sweep_results > TARGET)   # less negative
below = which(sweep_results < TARGET)   # more negative
if (length(above) == 0 || length(below) == 0) {
  stop("Grid does not bracket target. Extend psi_grid or check inputs.")
}

# Bracket: small psi gives very negative response; large psi gives mildly negative
i_below_target = max(below)              # largest psi where response is too negative
i_above_target = min(above)              # smallest psi where response is too mild
p_lo = psi_grid[i_below_target]
p_hi = psi_grid[i_above_target]

cat(sprintf("\nBracketing: [%.3f, %.3f]\n", p_lo, p_hi))

for (iter in 1:30) {
  p_mid = (p_lo + p_hi) / 2
  v_mid = semi_at_anchor(p_mid)
  cat(sprintf("  iter %2d  psi = %.5f  semi_elast = %.5f\n", iter, p_mid, v_mid))
  if (abs(v_mid - TARGET) < 1e-4) break
  if (v_mid < TARGET) p_lo = p_mid else p_hi = p_mid
}

psi_star = (p_lo + p_hi) / 2
final    = semi_at_anchor(psi_star)

cat(sprintf("\nCalibrated psi = %.4f  (semi_elast = %.4f, target = %.4f)\n",
            psi_star, final, TARGET))
cat("\nUpdate KG_DYN_DEFAULT_PSI in src/sim/kg_dynamics.R to this value.\n")


#-------------------------------------------------------------------------------
# Bonus: profile of implied semi-elasticity by year (1pp perturbation)
#-------------------------------------------------------------------------------

profile_years = function(psi_val) {

  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, tau_B_mat,
                                         c_phi_B = 0,
                                         psi   = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta  = KG_DYN_BETA)
  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, tau_S_mat,
                                         kappa_mat = pass1$kappa,
                                         c_phi_S_by_year = c_phi_S_by_year,
                                         psi   = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         beta  = KG_DYN_BETA)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  out   = tibble(sim_year = integer(), year = integer(),
                 semi_elast = numeric())

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]; bt = baseline_cells[[as.character(t)]]
    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    lambda_T = KG_DYN_PHI_I * bt$r_B
    r_S_vec  = setNames(lambda_T + r_D_S_bt, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec, 0, KG_DYN_PHI_I)
    G_S        = bt$G_B + delta
    R_B_t      = sum(bt$R_B)
    R_S_t      = sum(step$r_S * G_S)
    semi_t     = log(R_S_t / R_B_t) / PERTURBATION
    out        = bind_rows(out, tibble(sim_year   = j,
                                       year       = t,
                                       semi_elast = round(semi_t, 4)))
    delta = step$delta_next
  }
  out
}

cat("\nSemi-elasticity profile by sim year (calibrated psi):\n")
print(as.data.frame(profile_years(psi_star) %>%
                      filter(sim_year %in% c(1, 5, 10, 20, 30))),
      row.names = FALSE)

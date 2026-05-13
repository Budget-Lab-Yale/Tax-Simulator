#-------------------------------------------------------------------------------
# sweep_planned_share.R
#
# Sweeps KG planned-realization shares. For each candidate share, recalibrates
# psi against the permanent 1pp semi-elasticity target, then evaluates a delayed
# reform output root and reports announcement-year timing responses.
#
# CLI:
#   Rscript other/kg_model_tests/sweep_planned_share.R \
#     <baseline_root> <delayed_root> [<macro_root>] [<planned_share_csv>]
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(magrittr)
})

source('./src/sim/kg_dynamics.R')

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop(paste0(
    'Usage: Rscript other/kg_model_tests/sweep_planned_share.R ',
    '<baseline_root> <delayed_root> [<macro_root>] [<planned_share_csv>]'))
}

BASELINE_ROOT = args[1]
DELAYED_ROOT  = args[2]
MACRO_ROOT    = if (length(args) >= 3) args[3] else
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
SHARE_GRID    = if (length(args) >= 4) {
  as.numeric(strsplit(args[4], ',', fixed = TRUE)[[1]])
} else {
  seq(0, 0.5, by = 0.05)
}

TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline'
AGES_BATHTUB  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN
TARGET        = -0.6 / 0.238
PERTURBATION  = 0.01

load_baseline_inputs = function(root, years) {
  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'kg_lt', 'q_death1', 'q_death2',
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  baseline_cells = list()
  tau_B = list()

  for (t in years) {
    td = file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      kg_dyn_attach_record_attrs()

    bl = file.path(root, 'baseline', 'static', 'detail', paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    td_with_mtr = td %>% inner_join(bl, by = 'id')
    baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td_with_mtr, AGES_BATHTUB)
    tau_B[[as.character(t)]] = kg_dyn_aggregate_cell_mtr(td_with_mtr, AGES_BATHTUB)
  }

  list(baseline_cells = baseline_cells, tau_B = tau_B)
}

load_reform_tau = function(root, years) {
  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'kg_lt', 'q_death1', 'q_death2',
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  out = list()
  for (t in years) {
    td = file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      kg_dyn_attach_record_attrs()

    rf = file.path(root, 'static', 'detail', paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    out[[as.character(t)]] = td %>%
      inner_join(rf, by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(AGES_BATHTUB)
  }
  out
}

build_inputs = function(baseline_cells, tau_B, years) {
  life_ext = kg_dyn_load_life_table_extension(years = years)
  grid_ext = kg_dyn_build_extended_grid(baseline_cells, life_ext, years,
                                        ages_bellman = AGES_BELLMAN)
  list(
    grid_packed = kg_dyn_pack_baseline_grid(grid_ext, years,
                                            ages_bellman = AGES_BELLMAN),
    tau_B_mat   = kg_dyn_pack_tau(tau_B, years, ages_bellman = AGES_BELLMAN),
    beta        = kg_dyn_load_beta_series(MACRO_ROOT, years)
  )
}

run_stepup_response = function(psi_val, planned_share, baseline_cells,
                               grid_packed, tau_B_mat, tau_S_mat,
                               beta_by_year, years, anchor_year = NULL) {
  planned = kg_dyn_build_planned_timing(baseline_cells, tau_S_mat, years,
                                        tau_B_mat = tau_B_mat,
                                        planned_share = planned_share,
                                        timing_window = KG_DYN_TIMING_WINDOW,
                                        ages_bathtub = AGES_BATHTUB)
  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, tau_B_mat,
                                         c_phi_B = 0,
                                         psi = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         planned_share = planned_share,
                                         beta_by_year = beta_by_year)
  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, tau_S_mat,
                                         kappa_mat = pass1$kappa,
                                         c_phi_S_by_year = rep(0, length(years)),
                                         psi = psi_val,
                                         phi_I = KG_DYN_PHI_I,
                                         planned_share = planned_share,
                                         beta_by_year = beta_by_year)

  A = kg_dyn_build_aging_matrix(AGES_BATHTUB)
  omega = kg_dyn_build_heir_matrix(AGES_BATHTUB)
  ages_chr = as.character(AGES_BATHTUB)
  delta = setNames(rep(0, length(AGES_BATHTUB)), ages_chr)
  out = tibble()

  for (j in seq_along(years)) {
    t = years[j]
    bt = baseline_cells[[as.character(t)]]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t      = bt,
      r_ordinary_S    = pass2$r_D[ages_chr, j],
      R_planned_B_col = planned$R_planned_B[, j],
      R_planned_S_col = planned$R_planned_S[, j],
      fixed_share     = KG_DYN_PHI_I
    )
    r_S_vec = setNames(rate_info$r_S, ages_chr)
    G_S = bt$G_B + delta
    young = bt$age >= 18 & bt$age <= 39

    out = bind_rows(out, tibble(
      year = t,
      R_B_total = sum(bt$R_B),
      R_S_total = sum(r_S_vec * G_S),
      R_B_young = sum(bt$R_B[young]),
      R_S_young = sum(r_S_vec[young] * G_S[young]),
      planned_timing_shift = sum(planned$planned_timing_shift[, j])
    ))

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec, 0, KG_DYN_PHI_I)
    delta = step$delta_next
  }

  if (!is.null(anchor_year)) {
    out %>% filter(year == anchor_year)
  } else {
    out
  }
}

calibrate_psi_for_share = function(planned_share, baseline_cells, grid_packed,
                                   tau_B_mat, beta_by_year, years, anchor_year) {
  tau_S = tau_B_mat + PERTURBATION

  semi = function(psi_val) {
    res = run_stepup_response(psi_val, planned_share, baseline_cells,
                              grid_packed, tau_B_mat, tau_S,
                              beta_by_year, years, anchor_year)
    log(res$R_S_total / res$R_B_total) / PERTURBATION
  }

  psi_grid = c(0.8, 1.6, 3.2, 6.4, 12.8, 25.6, 51.2, 102.4)
  vals = sapply(psi_grid, semi)
  above = which(vals > TARGET)
  below = which(vals < TARGET)
  if (length(above) == 0 || length(below) == 0) {
    stop('psi grid does not bracket target for planned_share = ', planned_share)
  }

  p_lo = psi_grid[max(below)]
  p_hi = psi_grid[min(above)]
  for (iter in 1:30) {
    p_mid = (p_lo + p_hi) / 2
    v_mid = semi(p_mid)
    if (abs(v_mid - TARGET) < 1e-4) break
    if (v_mid < TARGET) p_lo = p_mid else p_hi = p_mid
  }
  (p_lo + p_hi) / 2
}

detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS = sort(as.integer(sub('\\.csv$', '', detail_files)))
if (length(YEARS) < 30) stop('baseline_root must include at least 30 years')
ANCHOR_YEAR = min(YEARS) + 29

delayed_files = list.files(file.path(DELAYED_ROOT, 'static/detail'),
                           pattern = '^[0-9]+\\.csv$')
DELAYED_YEARS = sort(as.integer(sub('\\.csv$', '', delayed_files)))
if (length(DELAYED_YEARS) == 0) stop('delayed_root has no static/detail files')
ANNOUNCE_YEAR = min(DELAYED_YEARS)

cat('Loading baseline inputs...\n')
base = load_baseline_inputs(BASELINE_ROOT, YEARS)
cal = build_inputs(base$baseline_cells, base$tau_B, YEARS)

cat('Loading delayed reform tau...\n')
delayed_tau = load_reform_tau(DELAYED_ROOT, DELAYED_YEARS)
delayed_inputs = build_inputs(base$baseline_cells[as.character(DELAYED_YEARS)],
                              base$tau_B[as.character(DELAYED_YEARS)],
                              DELAYED_YEARS)
tau_delay_mat = kg_dyn_pack_tau(delayed_tau, DELAYED_YEARS,
                                ages_bellman = AGES_BELLMAN)

results = bind_rows(lapply(SHARE_GRID, function(s) {
  kg_dyn_validate_realization_buckets(fixed_share = KG_DYN_PHI_I,
                                      planned_share = s,
                                      timing_window = KG_DYN_TIMING_WINDOW)
  cat(sprintf('planned_share = %.3f\n', s))
  psi = calibrate_psi_for_share(s, base$baseline_cells, cal$grid_packed,
                                cal$tau_B_mat, cal$beta, YEARS, ANCHOR_YEAR)
  delayed = run_stepup_response(psi, s,
                                base$baseline_cells[as.character(DELAYED_YEARS)],
                                delayed_inputs$grid_packed,
                                delayed_inputs$tau_B_mat,
                                tau_delay_mat,
                                delayed_inputs$beta,
                                DELAYED_YEARS,
                                ANNOUNCE_YEAR)
  tibble(
    planned_share = s,
    psi = psi,
    announce_year = ANNOUNCE_YEAR,
    total_pct_delta = 100 * (delayed$R_S_total / delayed$R_B_total - 1),
    young_pct_delta = 100 * (delayed$R_S_young / delayed$R_B_young - 1),
    planned_timing_shift = delayed$planned_timing_shift
  )
}))

print(as.data.frame(results), row.names = FALSE)

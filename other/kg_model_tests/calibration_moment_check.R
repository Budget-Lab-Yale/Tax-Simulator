#-------------------------------------------------------------------------------
# calibration_moment_check.R  --  Tier A of the calibration staleness watch
#
# Recomputes the kg_dynamics calibrator's LONG-RUN INTERNAL moment (calibrate.R
# step 4: +1pp uniform permanent CG perturbation, internal bathtub dlog(R)/dtau
# at sim-year 30) at the SHIPPED eta, with the full CURRENT structure threaded
# (estate offset e_B; wealth-carry h is 0 on a no-wealth-tax baseline, so the
# baseline root used here exercises the estate term). It then compares the
# recomputed moment against the checked-in reference value in
# calibration_reference.csv and flags drift.
#
# This is the CHEAP tick (minutes on an existing >=30-yr baseline): it does NOT
# re-pin eta on the full simulator -- it only detects that the miniature's
# internal moment at the shipped eta has moved, i.e. that some Bellman-primitive
# / estate / structural change has invalidated the calibration and a full-sim
# eta_dial re-pin is due. Generalized from eta_estate_check.R.
#
# Usage:
#   Rscript calibration_moment_check.R <baseline_root> [--seed]
#     <baseline_root>  a vintage with baseline/static/detail/{year}.csv carrying
#                      mtr_kg_lt AND mtr_estate_ded for >= 30 years.
#     --seed           write the recomputed moment into calibration_reference.csv
#                      (KG_DYN_DEFAULT_ETA row) instead of comparing -- use once,
#                      from a known-clean state, to establish the reference.
# Exit code: 0 = PASS (drift < 1% or seeded); 3 = DRIFT (>= 1%); 1 = usage/error.
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
SEED = '--seed' %in% args
args = args[args != '--seed']
if (length(args) < 1) {
  stop('Usage: Rscript calibration_moment_check.R <baseline_root> [--seed]')
}
BASELINE_ROOT = args[1]
MACRO_ROOT    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026070814/baseline'
REF_CSV       = 'other/kg_model_tests/moment_reference.csv'
DRIFT_TOL     = 0.01

AGES_BATHTUB = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN

LONG_RUN_PERTURB = 0.01
LONG_RUN_OFFSET  = 29
F_REF            = 0.5

# Active-form parametrization (Tier-A watch covers BOTH etas). The internal
# long-run moment is recomputed at the shipped eta for the LIVE form
# (KG_RESPONSE_FORM); the eval_long_run solves below pick up the form through
# kg_dyn_solve_bellman's default (= KG_DYN_RESPONSE_FORM), so a logs run
# exercises the power cost automatically. The reference row compared against is
# the form's own constant in moment_reference.csv.
#
# That file replaced calibration_reference.csv on 2026-07-26. The old one also
# carried each constant's shipped value, the files it was pinned against, and the
# data vintages it was derived under -- all of which config/calibrations/kg/ now
# owns, and all of which had drifted out of agreement with it (its sigma row named
# a different Tax-Data vintage than conversion.yaml, and its file paths still
# pointed at config/scenarios/behavior/). What is left is the one thing only this
# diagnostic knows: the reference moment to compare a recomputation against. The
# shipped eta is read from the live configuration instead of duplicated.
FORM         = KG_DYN_RESPONSE_FORM
CONST_NAME   = if (identical(FORM, 'logs')) 'KG_DYN_DEFAULT_ETA_LOGS' else
                                            'KG_DYN_DEFAULT_ETA'
ETA_CURRENT  = kg_dyn_active_eta()
if (!is.finite(ETA_CURRENT))
  stop(sprintf('%s (the %s-form eta) is not set (NA) -- pin it first.',
               CONST_NAME, FORM))
cat(sprintf('response form: %s  |  checking constant: %s\n', FORM, CONST_NAME))

detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS = sort(as.integer(sub('\\.csv$', '', detail_files)))
stopifnot(length(YEARS) >= LONG_RUN_OFFSET + 1)
LONG_RUN_ANCHOR = YEARS[1] + LONG_RUN_OFFSET

cat(sprintf('Baseline root: %s (%d years, anchor %d); shipped eta = %.4f\n',
            BASELINE_ROOT, length(YEARS), LONG_RUN_ANCHOR, ETA_CURRENT))

#--- Load cells, tau, and the estate exposure (calibrate.R step 1 + e) ---------
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

zero_route_vec = rep(0, length(AGES_BATHTUB))
A = build_aging_matrix(AGES_BATHTUB)
omega = kg_dyn_build_heir_matrix(
  heir_dist = rep(1 / length(AGES_BATHTUB), length(AGES_BATHTUB)),
  ages = AGES_BATHTUB)
bathtub_ages_chr = as.character(AGES_BATHTUB)

#--- Long-run internal moment at the shipped eta, estate e ON (calibrate step 4) --
eval_long_run = function(eta_val, use_e) {
  e_mat = if (use_e) e_B_mat else NULL
  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells, tau_S_mat = tau_S_long_mat,
    years = YEARS, tau_B_mat = tau_B_mat, timeable_share = F_REF,
    timing_window = KG_DYN_TIMING_WINDOW, ref_wedge = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub = AGES_BATHTUB)
  # BOTH passes get e_B: a CG perturbation leaves estate law unchanged (e_S = e_B),
  # exactly the production config for a pure CG reform.
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

moment = eval_long_run(ETA_CURRENT, use_e = TRUE)
cat(sprintf('\ninternal long-run semi at shipped eta %.4f (estate e ON): %.4f\n',
            ETA_CURRENT, moment))

#--- Seed or compare against calibration_reference.csv -------------------------
ref = suppressWarnings(tryCatch(read_csv(REF_CSV, show_col_types = FALSE),
                                error = function(e) NULL))
if (is.null(ref) || !('constant' %in% names(ref)))
  stop(sprintf('cannot read %s (run with a valid reference file)', REF_CSV))
row_i = which(ref$constant == CONST_NAME)
if (length(row_i) != 1)
  stop(sprintf('moment_reference.csv needs exactly one %s row', CONST_NAME))

code_sha = tryCatch(system('git rev-parse --short HEAD', intern = TRUE),
                    error = function(e) NA_character_)

if (SEED) {
  ref$reference_moment[row_i] = sprintf('internal_long_run_semi=%.4f', moment)
  ref$derived_date[row_i]     = as.character(Sys.Date())
  if (!is.na(code_sha)) ref$code_sha[row_i] = code_sha
  write_csv(ref, REF_CSV)
  cat(sprintf('SEEDED: wrote reference moment %.4f to %s (%s row, sha %s)\n',
              moment, REF_CSV, CONST_NAME, code_sha))
  quit(status = 0)
}

ref_str = ref$reference_moment[row_i]
ref_val = suppressWarnings(as.numeric(sub('.*=', '', ref_str)))
if (!is.finite(ref_val))
  stop(sprintf('%s reference_moment not parseable: "%s" -- seed first',
               CONST_NAME, ref_str))
drift = (moment - ref_val) / abs(ref_val)
cat(sprintf('reference moment: %.4f (%s, %s)\n',
            ref_val, ref$derived_date[row_i], ref$code_sha[row_i]))
cat(sprintf('drift: %+.2f%% (tol +-%.0f%%)\n', 100 * drift, 100 * DRIFT_TOL))
if (abs(drift) < DRIFT_TOL) {
  cat('VERDICT: PASS -- eta calibration within tolerance\n')
  quit(status = 0)
} else {
  cat('VERDICT: DRIFT -- internal moment moved; run the eta_dial full-sim re-pin\n')
  cat('  (other/top_tax/eta_dial/, then --seed this check from the new clean state)\n')
  quit(status = 3)
}

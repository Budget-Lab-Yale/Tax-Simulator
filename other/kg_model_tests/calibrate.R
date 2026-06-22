#-------------------------------------------------------------------------------
# calibrate.R
#
# Joint calibration of the kg_dynamics representative-cell Bellman to two
# moments:
#
#   1. Long-run (permanent) semi-elasticity: dlog(R)/dtau at sim-year 30
#      under a uniform +1pp permanent perturbation. Target -0.6 / 0.238 ~=
#      -2.52 (literature arc elasticity -0.62 anchored at a fixed baseline
#      tau of 0.238). Identifies KG_DYN_DEFAULT_PSI.
#
#   2. Short-run (transitory anticipation) semi-elasticity: dlog(R(t))/dtau(t+1)
#      at the announcement year under a +5pp delayed permanent shock (tau
#      unchanged in year 1, +5pp from year 2 onward). Target +5.04, i.e.,
#      twice the magnitude of the long-run target with the sign flipped
#      (future-tax-up -> realize-today). Identifies KG_DYN_SHARE_PLANNED.
#
# Methodology (nested bisection):
#   Outer loop -- bisect planned_share in [0, 0.5] against the short-run
#                  target. Larger planned_share -> larger short-run response.
#   Inner loop -- for each candidate planned_share, bisect psi to satisfy
#                  the long-run target (the existing single-parameter
#                  calibration logic, with the ordinary bucket shrunk to
#                  (1 - fixed_share - planned_share) * r_B).
#
# Convergence: warm-starts the inner psi bisection from the previous outer
# iteration's solution. Typical cost is ~10-20 outer iterations x 5-10 inner
# iterations -- well under an hour on a full-sample baseline.
#
# Output: prints recommended KG_DYN_DEFAULT_PSI and KG_DYN_SHARE_PLANNED.
# Paste both into src/sim/kg_dynamics.R.
#
# CLI:
#   Rscript other/kg_model_tests/calibrate.R <baseline_root> [<macro_root>]
#
#   <baseline_root> is the path to a full-sample Tax-Simulator vintage that
#   contains baseline/static/detail/{year}.csv with mtr_kg_lt for >= 30
#   years. Typically the staging output of slurm_run.sh on a runscript with
#   mtr_vars=kg_lt registered for baseline.
#
#   <macro_root> (optional) is the path to a Macro-Projections vintage's
#   baseline directory. The Bellman uses the real-rate discount factor
#   series derived from tsy_10y / cpiu. Defaults to the vintage in
#   config/interfaces/interface_versions.yaml.
#-------------------------------------------------------------------------------


suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(magrittr)
})

source('./src/sim/kg_dynamics.R')


#-------------------------------------------------------------------------------
# Inputs and calibration targets
#-------------------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Usage: Rscript other/kg_model_tests/calibrate.R <baseline_root> [<macro_root>]')
}
BASELINE_ROOT = args[1]
MACRO_ROOT    = if (length(args) >= 2) args[2] else
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'

TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026050315/baseline'
AGES_BATHTUB  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX
AGES_BELLMAN  = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN

# Nominal (literature) elasticity targets — what the FULL SIM should deliver.
LONG_RUN_NOMINAL   = -0.6 / 0.238    # -2.52, permanent semi-elasticity target
SHORT_RUN_RATIO    = 2               # short-run / |long-run| magnitude
SHORT_RUN_NOMINAL  = -SHORT_RUN_RATIO * LONG_RUN_NOMINAL   # +5.04

# Empirical dilution factors: ratio of full-sim measured elasticity at the
# anchor year to the bathtub-internal elasticity the calibrator computes.
# The calibrator's standalone Bellman + bathtub recurrence omits per-record
# clamps in kg_dyn_apply_to_records, AGI/AMT/NIIT-driven MTR-distribution
# effects, and baseline-anchor-tau drift (literature -2.52 is anchored at
# tau=0.238 but the sim's kg-weighted average baseline mtr_kg_lt is lower).
# The internal bathtub target is inflated by 1/dilution so the full sim
# delivers the nominal literature target.
#
# Re-measured 2026-06-22 under the KG_APPLIER_ALLOCATION = '0.5' default
# (vintage kg_recal_2pp_05, full sample, via other/kg_model_tests/
# measure_dilution.R). dilution = E_full / E_int, both at the (psi, planned_share)
# the measurement run used (21.2272, 0.3921 -> E_int -3.2075 / +5.8293):
#   rate_up_2pp at sim year 30 (2055):    E_full = -3.62  (was -1.98 under 'R')
#   delayed     at announcement yr (2026): E_full = +8.97  (was +4.36 under 'R')
# Under 0.5 the full sim AMPLIFIES the bathtub response (dilution > 1) rather
# than damping it: the lock-in/carryover stock realization (extra_R) lands on a
# broader record set, and the small realization-weighted dtau denominator turns
# that into a large elasticity swing. Prior 'R'-rule values: 0.786 / 0.865.
#
# Iteration 2 (2026-06-22): the iter-1 values (1.1275 / 1.5391, measured at the
# old psi/ps 21.2272/0.3921) were extrapolated too far -- a verify run at the
# iter-1 solution (29.3290 / 0.2102, vintage kg_recal_2pp_05_verify) gave
# full-sim E_full = -2.43 long (ok) / +4.13 short (18% under nominal). The
# dilution is psi/ps-dependent, especially short-run. These values are
# RE-MEASURED at that verify point (E_int -2.2358 / +3.2758), so they anchor
# the bisection where the solution lives.
KG_DYN_DILUTION_LONG  = 1.0890
KG_DYN_DILUTION_SHORT = 1.2599

# Internal bathtub targets the bisection actually chases.
LONG_RUN_TARGET    = LONG_RUN_NOMINAL  / KG_DYN_DILUTION_LONG   # ≈ -3.21
LONG_RUN_PERTURB   = 0.01            # 1pp uniform permanent shock
LONG_RUN_OFFSET    = 29              # measure at YEARS[1] + 29 (sim year 30)

SHORT_RUN_TARGET   = SHORT_RUN_NOMINAL / KG_DYN_DILUTION_SHORT  # ≈ +5.83
SHORT_RUN_PERTURB  = 0.05            # 5pp delayed (announced at t, hits t+1)
SHORT_RUN_OFFSET   = 0               # measure at YEARS[1] (announcement year)

PSI_TOL            = 1e-4
SHORT_TOL          = 1e-3
MAX_OUTER          = 25
MAX_INNER          = 30
PS_LO_INIT         = 0
PS_HI_INIT         = 0.5
PSI_GRID_INIT      = c(0.8, 1.6, 3.2, 6.4, 12.8, 25.6, 51.2, 102.4)

detail_files = list.files(file.path(BASELINE_ROOT, 'baseline/static/detail'),
                          pattern = '^[0-9]+\\.csv$')
YEARS         = sort(as.integer(sub('\\.csv$', '', detail_files)))
if (length(YEARS) < LONG_RUN_OFFSET + 1) {
  stop(sprintf(paste0('calibrate.R requires at least %d years of baseline ',
                      'data to anchor the long-run semi-elasticity at sim-',
                      'year %d; found %d years at %s.'),
               LONG_RUN_OFFSET + 1, LONG_RUN_OFFSET + 1,
               length(YEARS), BASELINE_ROOT))
}
LONG_RUN_ANCHOR  = YEARS[1] + LONG_RUN_OFFSET
SHORT_RUN_ANCHOR = YEARS[1] + SHORT_RUN_OFFSET


#-------------------------------------------------------------------------------
# Step 1: Load Tax-Data + baseline MTRs, aggregate to cell quantities
#-------------------------------------------------------------------------------

cat("Loading Tax-Data and baseline MTRs for", length(YEARS), "years (full sample)...\n")

td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
            'kg_lt', 'q_death1', 'q_death2',
            KG_DYN_ESTATE_ASSET_VALUE_COLS,
            KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS) %>%
  unique()

cpiu_by_year = kg_dyn_load_cpiu_levels(MACRO_ROOT, YEARS)

baseline_cells = list()
tau_B          = list()

for (t in YEARS) {

  # §121 exclusion is filing-status-mapped at runtime via tax_law; here we
  # inject the flat baseline values directly since the calibrator builds
  # td from Tax-Data csv without going through the tax_law parser.
  # Calibration scenarios run under step-up everywhere, so §121 only feeds
  # gain.primary_home_above_cap (a diagnostic for this run).
  td = file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv')) %>%
    fread(select = td_cols, showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(`pref.kg_sec121_excl` = if_else(filing_status == 2, 500000, 250000),
           year = t) %>%
    kg_dyn_attach_record_attrs(cpiu_by_year = cpiu_by_year)

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
# Step 2: Construct the two scenario tau paths
#-------------------------------------------------------------------------------

tau_S_long  = lapply(tau_B, function(v) v + LONG_RUN_PERTURB)
tau_S_short = lapply(seq_along(YEARS), function(j) {
  v = tau_B[[as.character(YEARS[j])]]
  if (j > 1) v = v + SHORT_RUN_PERTURB
  v
})
names(tau_S_short) = as.character(YEARS)


#-------------------------------------------------------------------------------
# Step 3: Build Bellman pre-pass inputs that don't depend on (psi, planned_share)
#-------------------------------------------------------------------------------

cat("Building extended grid, tau matrices, and real-rate beta series...\n")

life_ext         = kg_dyn_load_life_table_extension(years = YEARS)
grid_ext         = kg_dyn_build_extended_grid(baseline_cells, life_ext, YEARS,
                                              ages_bellman = AGES_BELLMAN)
grid_packed      = kg_dyn_pack_baseline_grid(grid_ext, YEARS,
                                             ages_bellman = AGES_BELLMAN)
tau_B_mat        = kg_dyn_pack_tau(tau_B,        YEARS, ages_bellman = AGES_BELLMAN)
tau_S_long_mat   = kg_dyn_pack_tau(tau_S_long,   YEARS, ages_bellman = AGES_BELLMAN)
tau_S_short_mat  = kg_dyn_pack_tau(tau_S_short,  YEARS, ages_bellman = AGES_BELLMAN)
beta_by_year     = kg_dyn_load_beta_series(MACRO_ROOT, YEARS)

cat(sprintf("  beta range: [%.4f, %.4f] (real-rate discount, from %s)\n",
            min(beta_by_year), max(beta_by_year), MACRO_ROOT))

zero_route_vec   = rep(0, length(AGES_BATHTUB))   # step-up: no carryover routing
A                = kg_dyn_build_aging_matrix(AGES_BATHTUB)
# omega is inert under step-up calibration (delta_route = 0), so any valid
# row-stochastic vector works. Use a uniform vector to avoid taking a
# dependency on Estate-Tax-Distribution from the standalone calibrator.
omega            = kg_dyn_build_heir_matrix(
  heir_dist = rep(1 / length(AGES_BATHTUB), length(AGES_BATHTUB)),
  ages      = AGES_BATHTUB
)
bathtub_ages_chr = as.character(AGES_BATHTUB)


#-------------------------------------------------------------------------------
# Step 4: Evaluation helpers (response at a chosen anchor year)
#-------------------------------------------------------------------------------

eval_response = function(psi_val, ps_val, scenario_tau_mat,
                         anchor_year, perturbation) {

  kg_dyn_validate_realization_buckets(fixed_share   = KG_DYN_PHI_I,
                                      planned_share = ps_val,
                                      timing_window = KG_DYN_TIMING_WINDOW,
                                      ref_wedge     = KG_DYN_TIMING_REF_WEDGE)

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = scenario_tau_mat,
    years          = YEARS,
    tau_B_mat      = tau_B_mat,
    planned_share  = ps_val,
    timing_window  = KG_DYN_TIMING_WINDOW,
    ref_wedge      = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub   = AGES_BATHTUB
  )

  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               psi           = psi_val,
                               phi_I         = KG_DYN_PHI_I,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, scenario_tau_mat,
                               c_phi_mat     = 0,
                               kappa_mat     = pass1$kappa,
                               psi           = psi_val,
                               phi_I         = KG_DYN_PHI_I,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  R_B_anchor = NA_real_
  R_S_anchor = NA_real_

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]
    bt = baseline_cells[[as.character(t)]]

    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t       = bt,
      r_ordinary_S     = r_D_S_bt,
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j],
      fixed_share      = KG_DYN_PHI_I
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = zero_route_vec,
      phi_I           = KG_DYN_PHI_I
    )

    if (t == anchor_year) {
      G_S        = bt$G_B + delta
      R_B_anchor = sum(bt$R_B)
      R_S_anchor = sum(step$r_S * G_S)
    }

    delta = step$delta_next
  }

  log(R_S_anchor / R_B_anchor) / perturbation
}

eval_long_run  = function(psi_val, ps_val) {
  eval_response(psi_val, ps_val, tau_S_long_mat,
                LONG_RUN_ANCHOR,  LONG_RUN_PERTURB)
}
eval_short_run = function(psi_val, ps_val) {
  eval_response(psi_val, ps_val, tau_S_short_mat,
                SHORT_RUN_ANCHOR, SHORT_RUN_PERTURB)
}


#-------------------------------------------------------------------------------
# Step 5: Inner psi bisection (long-run target, at given planned_share)
#-------------------------------------------------------------------------------

# Bracket psi from a coarse grid (or a tight window around a warm-start guess).
bracket_psi = function(ps_val, warm = NULL) {

  if (!is.null(warm)) {
    psi_lo = max(warm * 0.4, min(PSI_GRID_INIT))
    psi_hi = warm * 2.5
    v_lo = eval_long_run(psi_lo, ps_val)
    v_hi = eval_long_run(psi_hi, ps_val)
    # Verify the warm-start bracket actually straddles the target. If not,
    # fall through to the coarse grid.
    if ((v_lo - LONG_RUN_TARGET) * (v_hi - LONG_RUN_TARGET) <= 0) {
      return(list(lo = psi_lo, hi = psi_hi))
    }
  }

  vals = sapply(PSI_GRID_INIT, function(p) eval_long_run(p, ps_val))
  above = which(vals > LONG_RUN_TARGET)   # less negative response
  below = which(vals < LONG_RUN_TARGET)   # more negative response
  if (length(above) == 0 || length(below) == 0) {
    stop(sprintf(paste0('psi grid does not bracket long-run target at ',
                        'planned_share = %.4f (vals: [%s])'),
                 ps_val,
                 paste(sprintf('%.3f', vals), collapse = ', ')))
  }
  list(lo = PSI_GRID_INIT[max(below)], hi = PSI_GRID_INIT[min(above)])
}

inner_bisect_psi = function(ps_val, warm = NULL) {

  br = bracket_psi(ps_val, warm = warm)
  p_lo = br$lo
  p_hi = br$hi
  v_mid = NA_real_

  for (iter in 1:MAX_INNER) {
    p_mid = (p_lo + p_hi) / 2
    v_mid = eval_long_run(p_mid, ps_val)
    if (abs(v_mid - LONG_RUN_TARGET) < PSI_TOL) break
    if (v_mid < LONG_RUN_TARGET) p_lo = p_mid else p_hi = p_mid
  }
  list(psi = (p_lo + p_hi) / 2, semi = v_mid, iters = iter)
}


#-------------------------------------------------------------------------------
# Step 6: Outer planned_share bisection (short-run target)
#-------------------------------------------------------------------------------

cat(sprintf(
  '\nTargets:\n  long-run  d log(R)/dtau              = %+7.4f  (sim-year %2d)\n  short-run d log(R(t))/dtau(t+1) = %+7.4f  (sim-year %2d)\n\n',
  LONG_RUN_TARGET,  LONG_RUN_OFFSET + 1,
  SHORT_RUN_TARGET, SHORT_RUN_OFFSET + 1))

# Verify that the short-run target is bracketable in [PS_LO_INIT, PS_HI_INIT].
cat(sprintf('Outer bracket check on planned_share in [%.2f, %.2f]:\n',
            PS_LO_INIT, PS_HI_INIT))
inner_lo = inner_bisect_psi(PS_LO_INIT, warm = NULL)
short_lo = eval_short_run(inner_lo$psi, PS_LO_INIT)
cat(sprintf('  planned_share = %.4f  psi = %.4f  short_run = %+7.4f\n',
            PS_LO_INIT, inner_lo$psi, short_lo))

inner_hi = inner_bisect_psi(PS_HI_INIT, warm = inner_lo$psi)
short_hi = eval_short_run(inner_hi$psi, PS_HI_INIT)
cat(sprintf('  planned_share = %.4f  psi = %.4f  short_run = %+7.4f\n\n',
            PS_HI_INIT, inner_hi$psi, short_hi))

if ((short_lo - SHORT_RUN_TARGET) * (short_hi - SHORT_RUN_TARGET) > 0) {
  stop(sprintf(paste0('short-run target %.4f not bracketed by planned_share ',
                      'range [%.2f, %.2f] (short_lo = %.4f, short_hi = %.4f). ',
                      'Adjust PS_LO_INIT / PS_HI_INIT or revisit ',
                      'SHORT_RUN_RATIO / KG_DYN_TIMING_REF_WEDGE.'),
               SHORT_RUN_TARGET, PS_LO_INIT, PS_HI_INIT, short_lo, short_hi))
}

# Outer bisection. Larger planned_share -> larger (more positive) short_run.
ps_lo = PS_LO_INIT
ps_hi = PS_HI_INIT
psi_warm = inner_hi$psi
ps_star  = NA_real_
psi_star = NA_real_
short_star = NA_real_

for (iter in 1:MAX_OUTER) {
  ps_mid = (ps_lo + ps_hi) / 2

  inner = inner_bisect_psi(ps_mid, warm = psi_warm)
  psi_warm = inner$psi

  short = eval_short_run(inner$psi, ps_mid)

  cat(sprintf(
    '  outer %2d  planned_share = %.4f  psi = %.4f  long = %+7.4f  short = %+7.4f  (inner iters = %d)\n',
    iter, ps_mid, inner$psi, inner$semi, short, inner$iters))

  ps_star    = ps_mid
  psi_star   = inner$psi
  short_star = short

  if (abs(short - SHORT_RUN_TARGET) < SHORT_TOL) break

  if (short < SHORT_RUN_TARGET) ps_lo = ps_mid else ps_hi = ps_mid
}


#-------------------------------------------------------------------------------
# Step 7: Report
#-------------------------------------------------------------------------------

cat(sprintf('\nCalibrated:\n'))
cat(sprintf('  KG_DYN_DEFAULT_PSI    = %.4f  (long-run  semi = %+7.4f  target %+7.4f)\n',
            psi_star, eval_long_run(psi_star, ps_star), LONG_RUN_TARGET))
cat(sprintf('  KG_DYN_SHARE_PLANNED  = %.4f  (short-run semi = %+7.4f  target %+7.4f)\n',
            ps_star, short_star, SHORT_RUN_TARGET))
cat('\nUpdate KG_DYN_DEFAULT_PSI and KG_DYN_SHARE_PLANNED in src/sim/kg_dynamics.R.\n')

# Ready-to-paste provenance stamp (kg_dyn_check_calibration_provenance compares
# the live config against this and warns on drift). applier_allocation is the
# rule the dilutions were measured under = the live KG_APPLIER_ALLOCATION here.
td_vint    = sub('.*/Tax-Data/v[0-9]+/([0-9A-Za-z_]+)/.*',         '\\1', TAX_DATA_ROOT)
macro_vint = sub('.*/Macro-Projections/v[0-9]+/([0-9A-Za-z_]+)/.*', '\\1', MACRO_ROOT)
cat(sprintf(paste0(
  '\n--- paste into KG_DYN_CALIB_PROVENANCE (and bump spec_version on logic changes) ---\n',
  'KG_DYN_CALIB_PROVENANCE = list(\n',
  '  date               = \'%s\',\n',
  '  spec_version       = %dL,\n',
  '  psi                = %s,\n',
  '  planned_share      = %s,\n',
  '  applier_allocation = \'%s\',\n',
  '  phi_I              = %s,\n',
  '  ref_wedge          = %s,\n',
  '  timing_window      = %dL,\n',
  '  tax_data_vintage   = \'%s\',\n',
  '  macro_vintage      = \'%s\'\n',
  ')\n'),
  as.character(Sys.Date()), KG_DYN_SPEC_VERSION,
  format(round(psi_star, 4)), format(round(ps_star, 4)),
  KG_DYN_APPLIER_ALLOCATION,
  format(KG_DYN_PHI_I), format(KG_DYN_TIMING_REF_WEDGE),
  as.integer(KG_DYN_TIMING_WINDOW), td_vint, macro_vint))


#-------------------------------------------------------------------------------
# Bonus: long-run semi-elasticity profile by sim year at the calibrated point
#-------------------------------------------------------------------------------

profile_years = function(psi_val, ps_val) {

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = tau_S_long_mat,
    years          = YEARS,
    tau_B_mat      = tau_B_mat,
    planned_share  = ps_val,
    timing_window  = KG_DYN_TIMING_WINDOW,
    ref_wedge      = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub   = AGES_BATHTUB
  )

  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               psi           = psi_val,
                               phi_I         = KG_DYN_PHI_I,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_long_mat,
                               c_phi_mat     = 0,
                               kappa_mat     = pass1$kappa,
                               psi           = psi_val,
                               phi_I         = KG_DYN_PHI_I,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  out   = tibble(sim_year = integer(), year = integer(), semi_elast = numeric())

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]; bt = baseline_cells[[as.character(t)]]
    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t       = bt,
      r_ordinary_S     = r_D_S_bt,
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j],
      fixed_share      = KG_DYN_PHI_I
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec,
                                  zero_route_vec, KG_DYN_PHI_I)
    G_S    = bt$G_B + delta
    R_B_t  = sum(bt$R_B)
    R_S_t  = sum(step$r_S * G_S)
    out    = bind_rows(out, tibble(sim_year   = j,
                                   year       = t,
                                   semi_elast = round(log(R_S_t / R_B_t) /
                                                      LONG_RUN_PERTURB, 4)))
    delta = step$delta_next
  }
  out
}

cat('\nLong-run semi-elasticity profile by sim year (at calibrated point):\n')
print(as.data.frame(profile_years(psi_star, ps_star) %>%
                      filter(sim_year %in% c(1, 5, 10, 20, 30))),
      row.names = FALSE)

#-------------------------------------------------------------------------------
# calibrate.R
#
# Joint calibration of the kg_dynamics representative-cell Bellman (spec v2:
# ENTROPY realization cost, nested Phi/omega bucket reparameterization) to two
# moments:
#
#   1. Long-run (permanent) semi-elasticity: dlog(R)/dtau at sim-year 30
#      under a uniform +1pp permanent perturbation. Target -0.6 / 0.238 ~=
#      -2.52 (literature arc elasticity -0.62 anchored at a fixed baseline
#      tau of 0.238). Identifies eta (KG_DYN_DEFAULT_ETA).
#
#   2. Short-run (transitory anticipation) semi-elasticity: dlog(R(t))/dtau(t+1)
#      at the announcement year under a +5pp delayed permanent shock (tau
#      unchanged in year 1, +5pp from year 2 onward). Target +5.04, i.e.,
#      twice the magnitude of the long-run target with the sign flipped
#      (future-tax-up -> realize-today). Identifies omega (KG_DYN_TIMEABLE_FRAC).
#
# Reparameterization: Phi = KG_DYN_SHARE_INERT (inert share) is HELD FIXED
# (env-overridable for sensitivity); the calibrator moves only omega. The two
# derived bucket primitives are phi_I = Phi*(1-omega) and planned_share =
# Phi*omega, so the ordinary Bellman share 1 - Phi is CONSTANT in omega -- which
# makes the long-run moment omega-invariant and eta/omega identify nearly
# independently.
#
# Methodology (nested bisection):
#   Outer loop -- bisect omega in [0, 1] against the short-run target. Larger
#                  omega -> larger planned_share -> larger short-run response.
#   Inner loop -- for each candidate omega, bisect eta to satisfy the long-run
#                  target. Response is DECREASING in eta (steeper exp response =
#                  more negative semi-elasticity), so the bracket/bisection
#                  direction is FLIPPED vs. the old psi calibration -- see
#                  bracket_eta / inner_bisect_eta.
#
# Convergence: warm-starts the inner eta bisection from the previous outer
# iteration's solution. Because long-run is omega-invariant, the inner loop
# collapses to ~1 iteration after the first outer step (a health signal).
#
# Output: prints recommended eta (KG_DYN_DEFAULT_ETA) and omega
# (KG_DYN_TIMEABLE_FRAC). Paste both into src/sim/kg_dynamics.R.
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


suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE))))

# kg_dyn_aggregate_cells reaches into corp_incidence.R and wealth_dynamics.R, so
# source the whole src tree (the reconstitute_environment / check_core.R
# convention) rather than a hand-picked file list.
return_vars <<- list()   # some src post-processing files reference this at source time
list.files('./src', recursive = TRUE) %>%
  walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
         source(file.path('./src', .x)))


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
#
# SPEC v2 (2026-07): re-measured against the entropy model in the Phase-4
# dilution loop (measure_dilution.sbatch); update both here each iteration
# until the full-sim semis land on nominal.
#   iter 1 (psi-era priors 1.0890 / 1.2599): eta=4.5837, omega=0.5254 ->
#          full sim kg_eta_recal_iter1: E_full_long -2.568 (ok, <0.05),
#          E_full_short +5.153 (short-run 0.113 over the +/-0.10 band).
#   iter 2 (values below, measure_dilution_17427646.out): re-measured
#          1.1095 / 1.2880 at the iter-1 operating point -> calibrate gave
#          eta=4.4984, omega=0.5132 -> full sim kg_eta_recal_iter2:
#          E_full_long -2.5232, E_full_short +5.0391 -- BOTH on nominal.
#          CONVERGED. The iter-2 verify re-measured 1.1104 / 1.2872 (drift
#          <0.1%, i.e. dilution stable), so these iter-2 dilutions are final;
#          they are the values eta*/omega* were calibrated under -- do not
#          swap in the re-measured pair without re-running calibrate.
KG_DYN_DILUTION_LONG  = 1.1095
KG_DYN_DILUTION_SHORT = 1.2880

# Internal bathtub targets the bisection actually chases.
LONG_RUN_TARGET    = LONG_RUN_NOMINAL  / KG_DYN_DILUTION_LONG   # ≈ -3.21
LONG_RUN_PERTURB   = 0.01            # 1pp uniform permanent shock
LONG_RUN_OFFSET    = 29              # measure at YEARS[1] + 29 (sim year 30)

SHORT_RUN_TARGET   = SHORT_RUN_NOMINAL / KG_DYN_DILUTION_SHORT  # ≈ +5.83
SHORT_RUN_PERTURB  = 0.05            # 5pp delayed (announced at t, hits t+1)
SHORT_RUN_OFFSET   = 0               # measure at YEARS[1] (announcement year)

ETA_TOL            = 1e-4
SHORT_TOL          = 1e-3
MAX_OUTER          = 25
MAX_INNER          = 30

# Nested reparameterization. Phi (inert share) is held fixed at the live
# KG_DYN_SHARE_INERT (env-overridable via KG_SHARE_INERT for sensitivity); the
# calibrator moves omega in [0, 1]. Derived bucket primitives:
PHI                = KG_DYN_SHARE_INERT
phi_of             = function(om) PHI * (1 - om)   # fixed/nonresponsive share
ps_of              = function(om) PHI * om         # mechanically-timeable share
OMEGA_LO_INIT      = 0
OMEGA_HI_INIT      = 1

# eta bracket grid. Expected eta* ~ 4-8 (solving the internal long-run target
# through the MC amplification); extend the top only if the bracket stop() fires
# high. Response is DECREASING in eta.
ETA_GRID_INIT      = c(0.5, 1, 2, 4, 8, 16, 32)

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
            ESTATE_ASSET_COLS,
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
A                = build_aging_matrix(AGES_BATHTUB)
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

# eval_response(eta_val, omega_val, ...): omega sets BOTH bucket primitives
# phi_I = phi_of(omega) and planned_share = ps_of(omega) (their sum is the fixed
# Phi). The 'omega' argument here is the timeable fraction -- distinct from the
# heir-aging matrix `omega` (the global from Step 3), which is unused under
# step-up calibration (delta_route = 0). We rename the local to `omega_frac`.
eval_response = function(eta_val, omega_frac, scenario_tau_mat,
                         anchor_year, perturbation) {

  phi_val = phi_of(omega_frac)
  ps_val  = ps_of(omega_frac)

  kg_dyn_validate_realization_buckets(fixed_share   = phi_val,
                                      planned_share = ps_val,
                                      timing_window = KG_DYN_TIMING_WINDOW,
                                      ref_wedge     = KG_DYN_TIMING_REF_WEDGE,
                                      share_inert   = PHI,
                                      timeable_frac = omega_frac)

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
                               eta           = eta_val,
                               phi_I         = phi_val,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, scenario_tau_mat,
                               c_phi_mat     = 0,
                               kappa_mat     = pass1$kappa,
                               eta           = eta_val,
                               phi_I         = phi_val,
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
      fixed_share      = phi_val
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = zero_route_vec,
      phi_I           = phi_val
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

eval_long_run  = function(eta_val, omega_frac) {
  eval_response(eta_val, omega_frac, tau_S_long_mat,
                LONG_RUN_ANCHOR,  LONG_RUN_PERTURB)
}
eval_short_run = function(eta_val, omega_frac) {
  eval_response(eta_val, omega_frac, tau_S_short_mat,
                SHORT_RUN_ANCHOR, SHORT_RUN_PERTURB)
}


#-------------------------------------------------------------------------------
# Step 5: Inner eta bisection (long-run target, at given omega)
#
# DIRECTION FLIP vs. the old psi calibration: the long-run response is
# DECREASING in eta (bigger eta => steeper exp => more negative semi). So the
# lo/hi bracket and the bisection update are the mirror of the quadratic-psi
# version. The straddle product-<=0 test is direction-agnostic; only the
# grid-bracket pick and the update inequality flip.
#-------------------------------------------------------------------------------

# Bracket eta from a coarse grid (or a tight window around a warm-start guess).
bracket_eta = function(omega_frac, warm = NULL) {

  if (!is.null(warm)) {
    eta_lo = max(warm * 0.4, min(ETA_GRID_INIT))
    eta_hi = warm * 2.5
    v_lo = eval_long_run(eta_lo, omega_frac)
    v_hi = eval_long_run(eta_hi, omega_frac)
    # Verify the warm-start bracket actually straddles the target. If not,
    # fall through to the coarse grid.
    if ((v_lo - LONG_RUN_TARGET) * (v_hi - LONG_RUN_TARGET) <= 0) {
      return(list(lo = eta_lo, hi = eta_hi))
    }
  }

  vals = sapply(ETA_GRID_INIT, function(e) eval_long_run(e, omega_frac))
  above = which(vals > LONG_RUN_TARGET)   # less negative response (small eta)
  below = which(vals < LONG_RUN_TARGET)   # more negative response (large eta)
  if (length(above) == 0 || length(below) == 0) {
    stop(sprintf(paste0('eta grid does not bracket long-run target at ',
                        'omega = %.4f (vals: [%s]); extend ETA_GRID_INIT.'),
                 omega_frac,
                 paste(sprintf('%.3f', vals), collapse = ', ')))
  }
  # Response decreasing in eta: the largest eta still above (less negative) is
  # the low end; the smallest eta below (more negative) is the high end.
  list(lo = ETA_GRID_INIT[max(above)], hi = ETA_GRID_INIT[min(below)])
}

inner_bisect_eta = function(omega_frac, warm = NULL) {

  br = bracket_eta(omega_frac, warm = warm)
  e_lo = br$lo
  e_hi = br$hi
  v_mid = NA_real_

  for (iter in 1:MAX_INNER) {
    e_mid = (e_lo + e_hi) / 2
    v_mid = eval_long_run(e_mid, omega_frac)
    if (abs(v_mid - LONG_RUN_TARGET) < ETA_TOL) break
    # Decreasing in eta: too negative (below target) => eta too big => lower hi.
    if (v_mid < LONG_RUN_TARGET) e_hi = e_mid else e_lo = e_mid
  }
  list(eta = (e_lo + e_hi) / 2, semi = v_mid, iters = iter)
}


#-------------------------------------------------------------------------------
# Step 6: Outer omega bisection (short-run target)
#-------------------------------------------------------------------------------

cat(sprintf(
  '\nPhi (inert share, fixed) = %.4f;  ordinary Bellman share = %.4f\n', PHI, 1 - PHI))
cat(sprintf(
  'Targets:\n  long-run  d log(R)/dtau              = %+7.4f  (sim-year %2d)\n  short-run d log(R(t))/dtau(t+1) = %+7.4f  (sim-year %2d)\n\n',
  LONG_RUN_TARGET,  LONG_RUN_OFFSET + 1,
  SHORT_RUN_TARGET, SHORT_RUN_OFFSET + 1))

# Verify that the short-run target is bracketable in [OMEGA_LO_INIT, OMEGA_HI_INIT].
cat(sprintf('Outer bracket check on omega in [%.2f, %.2f]:\n',
            OMEGA_LO_INIT, OMEGA_HI_INIT))
inner_lo = inner_bisect_eta(OMEGA_LO_INIT, warm = NULL)
short_lo = eval_short_run(inner_lo$eta, OMEGA_LO_INIT)
cat(sprintf('  omega = %.4f  (phi_I = %.4f, planned = %.4f)  eta = %.4f  short_run = %+7.4f\n',
            OMEGA_LO_INIT, phi_of(OMEGA_LO_INIT), ps_of(OMEGA_LO_INIT),
            inner_lo$eta, short_lo))

inner_hi = inner_bisect_eta(OMEGA_HI_INIT, warm = inner_lo$eta)
short_hi = eval_short_run(inner_hi$eta, OMEGA_HI_INIT)
cat(sprintf('  omega = %.4f  (phi_I = %.4f, planned = %.4f)  eta = %.4f  short_run = %+7.4f\n\n',
            OMEGA_HI_INIT, phi_of(OMEGA_HI_INIT), ps_of(OMEGA_HI_INIT),
            inner_hi$eta, short_hi))

if ((short_lo - SHORT_RUN_TARGET) * (short_hi - SHORT_RUN_TARGET) > 0) {
  stop(sprintf(paste0('short-run target %.4f not bracketed by omega range ',
                      '[%.2f, %.2f] (short_lo = %.4f, short_hi = %.4f). Adjust ',
                      'OMEGA_LO_INIT / OMEGA_HI_INIT or Phi (KG_SHARE_INERT), or ',
                      'revisit SHORT_RUN_RATIO / KG_DYN_TIMING_REF_WEDGE.'),
               SHORT_RUN_TARGET, OMEGA_LO_INIT, OMEGA_HI_INIT, short_lo, short_hi))
}

# Outer bisection. Larger omega -> larger planned_share -> larger (more
# positive) short_run. eta is (nearly) omega-invariant, so warm-starting the
# inner bisection makes it collapse to ~1 iteration -- a health signal.
om_lo = OMEGA_LO_INIT
om_hi = OMEGA_HI_INIT
eta_warm = inner_hi$eta
om_star  = NA_real_
eta_star = NA_real_
short_star = NA_real_

for (iter in 1:MAX_OUTER) {
  om_mid = (om_lo + om_hi) / 2

  inner = inner_bisect_eta(om_mid, warm = eta_warm)
  eta_warm = inner$eta

  short = eval_short_run(inner$eta, om_mid)

  cat(sprintf(
    '  outer %2d  omega = %.4f  (phi_I = %.4f, planned = %.4f)  eta = %.4f  long = %+7.4f  short = %+7.4f  (inner iters = %d)\n',
    iter, om_mid, phi_of(om_mid), ps_of(om_mid), inner$eta, inner$semi,
    short, inner$iters))

  om_star    = om_mid
  eta_star   = inner$eta
  short_star = short

  if (abs(short - SHORT_RUN_TARGET) < SHORT_TOL) break

  if (short < SHORT_RUN_TARGET) om_lo = om_mid else om_hi = om_mid
}


#-------------------------------------------------------------------------------
# Step 7: Report
#-------------------------------------------------------------------------------

phi_star = phi_of(om_star)
ps_star  = ps_of(om_star)
E_int_long  = eval_long_run(eta_star, om_star)
E_int_short = short_star

# Count of frozen r_D_B = 0 cells (r_D_B = (1-Phi)*r_B, so exactly the r_B = 0
# cells; these are pinned at r_D = 0 and contribute no scenario response).
n_frozen = sum(grid_packed$r_B[bathtub_ages_chr, , drop = FALSE] == 0)
n_cells  = length(grid_packed$r_B[bathtub_ages_chr, , drop = FALSE])

cat(sprintf('\nCalibrated (spec v2, entropy cost; Phi = %.4f fixed):\n', PHI))
cat(sprintf('  KG_DYN_DEFAULT_ETA    = %.4f  (long-run  semi = %+7.4f  target %+7.4f)\n',
            eta_star, E_int_long, LONG_RUN_TARGET))
cat(sprintf('  KG_DYN_TIMEABLE_FRAC  = %.4f  (short-run semi = %+7.4f  target %+7.4f)\n',
            om_star, E_int_short, SHORT_RUN_TARGET))
cat(sprintf('  derived phi_I         = %.4f\n', phi_star))
cat(sprintf('  derived planned_share = %.4f\n', ps_star))
cat(sprintf('  frozen r_D_B=0 cells  = %d of %d bathtub cells (r_B = 0)\n',
            n_frozen, n_cells))
cat(sprintf(paste0('\n  E_int_long  = %+.4f  (measure_dilution.sbatch arg 3)\n',
                   '  E_int_short = %+.4f  (measure_dilution.sbatch arg 4)\n'),
            E_int_long, E_int_short))
cat('\nUpdate KG_DYN_DEFAULT_ETA and KG_DYN_TIMEABLE_FRAC in src/sim/kg_dynamics.R.\n')

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
  '  eta                = %s,\n',
  '  timeable_frac      = %s,\n',
  '  share_inert        = %s,\n',
  '  applier_allocation = \'%s\',\n',
  '  ref_wedge          = %s,\n',
  '  timing_window      = %dL,\n',
  '  tax_data_vintage   = \'%s\',\n',
  '  macro_vintage      = \'%s\'\n',
  ')\n',
  '# also set KG_DYN_DEFAULT_ETA default to %s and KG_DYN_TIMEABLE_FRAC to %s\n'),
  as.character(Sys.Date()), KG_DYN_SPEC_VERSION,
  format(round(eta_star, 4)), format(round(om_star, 4)), format(PHI),
  KG_DYN_APPLIER_ALLOCATION,
  format(KG_DYN_TIMING_REF_WEDGE),
  as.integer(KG_DYN_TIMING_WINDOW), td_vint, macro_vint,
  format(round(eta_star, 4)), format(round(om_star, 4))))


#-------------------------------------------------------------------------------
# Bonus: long-run semi-elasticity profile by sim year at the calibrated point
#-------------------------------------------------------------------------------

profile_years = function(eta_val, omega_frac) {

  phi_val = phi_of(omega_frac)
  ps_val  = ps_of(omega_frac)

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
                               eta           = eta_val,
                               phi_I         = phi_val,
                               planned_share = ps_val,
                               beta_by_year  = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_long_mat,
                               c_phi_mat     = 0,
                               kappa_mat     = pass1$kappa,
                               eta           = eta_val,
                               phi_I         = phi_val,
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
      fixed_share      = phi_val
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec,
                                  zero_route_vec, phi_val)
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
print(as.data.frame(profile_years(eta_star, om_star) %>%
                      filter(sim_year %in% c(1, 5, 10, 20, 30))),
      row.names = FALSE)

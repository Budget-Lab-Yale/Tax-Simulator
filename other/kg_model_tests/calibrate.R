#-------------------------------------------------------------------------------
# calibrate.R
#
# Calibration of the kg_dynamics representative-cell Bellman (spec v3: ENTROPY
# realization cost, SINGLE POOL -- no responsive/inert split, no fixed floor) to
# two moments:
#
#   1. Long-run (permanent) semi-elasticity: dlog(R)/dtau at sim-year 30
#      under a uniform +1pp permanent perturbation. Target -0.6 / 0.238 ~=
#      -2.52 (literature arc elasticity -0.62 anchored at a fixed baseline
#      tau of 0.238). Identifies eta (KG_DYN_DEFAULT_ETA) -- and since the whole
#      pool responds, eta IS the long-run semi-elasticity directly.
#
#   2. Short-run (transitory anticipation) semi-elasticity: dlog(R(t))/dtau(t+1)
#      at the announcement year under a +5pp delayed permanent shock (tau
#      unchanged in year 1, +5pp from year 2 onward). Target +5.04, i.e.,
#      twice the magnitude of the long-run target with the sign flipped
#      (future-tax-up -> realize-today). Identifies KG_DYN_TIMEABLE_SHARE.
#
# Single pool: the Bellman responds on ALL gains (r_D_B = r_B, no carve-out),
# and a fraction f = KG_DYN_TIMEABLE_SHARE of ALL realizations retimes across
# the window as an additive overlay (r_S = r_ordinary_S + (r_planned_S -
# r_planned_B)). The overlay nets to zero under a UNIFORM permanent shock (no
# year is cheaper), so the long-run moment is EXACTLY timeable-share invariant.
#
# Methodology (two SEQUENTIAL 1-D bisections -- the v2 nested loop collapses):
#   Step 5 -- bisect eta in the ETA_GRID against the long-run target (at an
#             arbitrary reference share F_REF; f-invariance is asserted first).
#             Response is DECREASING in eta (steeper exp = more negative semi).
#   Step 6 -- with eta* fixed, bisect f in [0, 1] against the short-run target.
#             Short-run is INCREASING in f (more timeable dollars pull-forward),
#             and also carries the full-pool Bellman's own anticipation at eta*,
#             so f supplies only the residual. If the pure-Bellman short-run at
#             f=0 already exceeds target, the bracket fails -- that signals the
#             short-run anchor is inconsistent with the full-pool level response
#             (revisit SHORT_RUN_RATIO), not a bug.
#
# Output: prints recommended eta (KG_DYN_DEFAULT_ETA) and f
# (KG_DYN_TIMEABLE_SHARE). Paste both into src/sim/kg_dynamics.R.
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
# anchor year to the bathtub-internal elasticity the calibrator computes. The
# standalone Bellman + bathtub recurrence omits per-record clamps in
# kg_dyn_apply_to_records, AGI/AMT/NIIT-driven MTR-distribution effects, and
# baseline-anchor-tau drift (literature -2.52 is anchored at tau=0.238 but the
# sim's kg-weighted average baseline mtr_kg_lt is lower). The internal bathtub
# target is inflated by 1/dilution so the full sim delivers the nominal target.
#
# Re-measured each iteration by measure_dilution.sbatch (dilution = E_full /
# E_int at the current operating point); update both here and re-run calibrate
# until the full-sim semis land on nominal. The values below are the v2
# converged priors used to SEED the v3 (single-pool) recalibration -- REPLACE
# with the v3 re-measurement before treating the calibrated eta/timeable_share
# as final.
KG_DYN_DILUTION_LONG  = 1.1277
KG_DYN_DILUTION_SHORT = 1.0864

# Internal bathtub targets the bisection actually chases (nominal / dilution).
LONG_RUN_TARGET    = LONG_RUN_NOMINAL  / KG_DYN_DILUTION_LONG
LONG_RUN_PERTURB   = 0.01            # 1pp uniform permanent shock
LONG_RUN_OFFSET    = 29              # measure at YEARS[1] + 29 (sim year 30)

SHORT_RUN_TARGET   = SHORT_RUN_NOMINAL / KG_DYN_DILUTION_SHORT
SHORT_RUN_PERTURB  = 0.05            # 5pp delayed (announced at t, hits t+1)
SHORT_RUN_OFFSET   = 0               # measure at YEARS[1] (announcement year)

ETA_TOL            = 1e-4
SHORT_TOL          = 1e-3
MAX_ETA_ITER       = 30
MAX_F_ITER         = 30

# Single pool: eta is calibrated against the (timeable-share invariant) long-run
# moment at this arbitrary reference share; the invariance is asserted at
# runtime before the eta bisection.
F_REF              = 0.5
F_LO_INIT          = 0
F_HI_INIT          = 1

# eta bracket grid. In the single pool eta ~ the long-run semi-elasticity
# directly (order ~2.5, well below v2's responsive-half ~4.5); extend the top
# only if the bracket stop() fires high. Response is DECREASING in eta.
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
# Step 3: Build Bellman pre-pass inputs that don't depend on (eta, timeable_share)
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

# eval_response(eta_val, timeable_share, ...): single pool -- the Bellman runs
# on the full baseline rate (no phi_I/planned carve-out) and timeable_share
# drives the additive timing overlay. The heir-aging matrix `omega` (the global
# from Step 3) is unused under step-up calibration (delta_route = 0).
eval_response = function(eta_val, timeable_share, scenario_tau_mat,
                         anchor_year, perturbation) {

  kg_dyn_validate_timing_params(timeable_share = timeable_share,
                                timing_window  = KG_DYN_TIMING_WINDOW,
                                ref_wedge      = KG_DYN_TIMING_REF_WEDGE)

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = scenario_tau_mat,
    years          = YEARS,
    tau_B_mat      = tau_B_mat,
    timeable_share = timeable_share,
    timing_window  = KG_DYN_TIMING_WINDOW,
    ref_wedge      = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub   = AGES_BATHTUB
  )

  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               eta          = eta_val,
                               beta_by_year = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, scenario_tau_mat,
                               c_phi_mat    = 0,
                               kappa_mat    = pass1$kappa,
                               eta          = eta_val,
                               beta_by_year = beta_by_year)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  R_B_anchor = NA_real_
  R_S_anchor = NA_real_

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]
    bt = baseline_cells[[as.character(t)]]

    rate_info = kg_dyn_build_scenario_rate(
      baseline_t       = bt,
      r_ordinary_S     = pass2$r_D[bathtub_ages_chr, j],
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j]
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = zero_route_vec
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

eval_long_run  = function(eta_val, timeable_share = F_REF) {
  eval_response(eta_val, timeable_share, tau_S_long_mat,
                LONG_RUN_ANCHOR,  LONG_RUN_PERTURB)
}
eval_short_run = function(eta_val, timeable_share) {
  eval_response(eta_val, timeable_share, tau_S_short_mat,
                SHORT_RUN_ANCHOR, SHORT_RUN_PERTURB)
}


#-------------------------------------------------------------------------------
# Step 5: eta bisection against the long-run target
#
# The long-run response is DECREASING in eta (bigger eta => steeper exp => more
# negative semi), so the lo/hi bracket and the bisection update are mirrored vs.
# an increasing target. The straddle product-<=0 test is direction-agnostic;
# only the grid-bracket pick and the update inequality flip.
#-------------------------------------------------------------------------------

# Bracket eta from a coarse grid (or a tight window around a warm-start guess).
bracket_eta = function(warm = NULL) {

  if (!is.null(warm)) {
    eta_lo = max(warm * 0.4, min(ETA_GRID_INIT))
    eta_hi = warm * 2.5
    v_lo = eval_long_run(eta_lo)
    v_hi = eval_long_run(eta_hi)
    if ((v_lo - LONG_RUN_TARGET) * (v_hi - LONG_RUN_TARGET) <= 0) {
      return(list(lo = eta_lo, hi = eta_hi))
    }
  }

  vals = sapply(ETA_GRID_INIT, function(e) eval_long_run(e))
  above = which(vals > LONG_RUN_TARGET)   # less negative response (small eta)
  below = which(vals < LONG_RUN_TARGET)   # more negative response (large eta)
  if (length(above) == 0 || length(below) == 0) {
    stop(sprintf(paste0('eta grid does not bracket long-run target (vals: ',
                        '[%s]); extend ETA_GRID_INIT.'),
                 paste(sprintf('%.3f', vals), collapse = ', ')))
  }
  # Response decreasing in eta: the largest eta still above (less negative) is
  # the low end; the smallest eta below (more negative) is the high end.
  list(lo = ETA_GRID_INIT[max(above)], hi = ETA_GRID_INIT[min(below)])
}

bisect_eta = function(warm = NULL) {

  br = bracket_eta(warm = warm)
  e_lo = br$lo
  e_hi = br$hi
  v_mid = NA_real_

  for (iter in 1:MAX_ETA_ITER) {
    e_mid = (e_lo + e_hi) / 2
    v_mid = eval_long_run(e_mid)
    if (abs(v_mid - LONG_RUN_TARGET) < ETA_TOL) break
    # Decreasing in eta: too negative (below target) => eta too big => lower hi.
    if (v_mid < LONG_RUN_TARGET) e_hi = e_mid else e_lo = e_mid
  }
  list(eta = (e_lo + e_hi) / 2, semi = v_mid, iters = iter)
}

cat(sprintf(
  'Targets:\n  long-run  d log(R)/dtau              = %+7.4f  (sim-year %2d)\n  short-run d log(R(t))/dtau(t+1) = %+7.4f  (sim-year %2d)\n\n',
  LONG_RUN_TARGET,  LONG_RUN_OFFSET + 1,
  SHORT_RUN_TARGET, SHORT_RUN_OFFSET + 1))

# Health check -- the single-pool design claim: the long-run moment is EXACTLY
# invariant to the timeable share (a uniform permanent shock leaves no year
# cheaper, so nothing retimes). Verify at f=0 and f=1 before calibrating eta.
lr_f0 = eval_long_run(2, timeable_share = 0)
lr_f1 = eval_long_run(2, timeable_share = 1)
cat(sprintf('f-invariance check (eta=2): long-run at f=0 %+7.5f vs f=1 %+7.5f  (|diff| %.2e)\n',
            lr_f0, lr_f1, abs(lr_f0 - lr_f1)))
if (abs(lr_f0 - lr_f1) > 1e-8) {
  stop('calibrate.R: long-run moment is NOT timeable-share invariant ',
       '(|diff| > 1e-8). The timing overlay is leaking into the permanent ',
       'margin -- check kg_dyn_build_scenario_rate / build_planned_timing.')
}

cat('\nCalibrating eta against the long-run moment...\n')
eta_fit    = bisect_eta(warm = NULL)
eta_star   = eta_fit$eta
E_int_long = eta_fit$semi
cat(sprintf('  KG_DYN_DEFAULT_ETA = %.4f  (long-run semi = %+7.4f  target %+7.4f, %d iters)\n',
            eta_star, E_int_long, LONG_RUN_TARGET, eta_fit$iters))


#-------------------------------------------------------------------------------
# Step 6: timeable-share bisection against the short-run target (at eta*)
#
# Short-run is INCREASING in f. At f=0 it is the pure full-pool Bellman
# anticipation at eta*; f adds the residual mechanical retiming. If the f=0
# response already exceeds target the bracket fails -- that is the flagged
# risk, not a bug (revisit SHORT_RUN_RATIO / the short-run anchor).
#-------------------------------------------------------------------------------

cat(sprintf('\nOuter bracket check on timeable share in [%.2f, %.2f] (eta = %.4f):\n',
            F_LO_INIT, F_HI_INIT, eta_star))
short_lo = eval_short_run(eta_star, F_LO_INIT)
short_hi = eval_short_run(eta_star, F_HI_INIT)
cat(sprintf('  f = %.4f  short_run = %+7.4f  (pure full-pool Bellman anticipation)\n',
            F_LO_INIT, short_lo))
cat(sprintf('  f = %.4f  short_run = %+7.4f\n\n', F_HI_INIT, short_hi))

if ((short_lo - SHORT_RUN_TARGET) * (short_hi - SHORT_RUN_TARGET) > 0) {
  stop(sprintf(paste0('short-run target %.4f not bracketed by f in [%.2f, %.2f] ',
                      '(short_lo = %.4f, short_hi = %.4f). If short_lo already ',
                      'exceeds the target, the full-pool Bellman alone ',
                      'overshoots the short-run at eta* -- revisit ',
                      'SHORT_RUN_RATIO / KG_DYN_TIMING_REF_WEDGE.'),
               SHORT_RUN_TARGET, F_LO_INIT, F_HI_INIT, short_lo, short_hi))
}

f_lo = F_LO_INIT
f_hi = F_HI_INIT
f_star     = NA_real_
short_star = NA_real_

for (iter in 1:MAX_F_ITER) {
  f_mid = (f_lo + f_hi) / 2
  short = eval_short_run(eta_star, f_mid)

  cat(sprintf('  f-iter %2d  timeable_share = %.4f  short = %+7.4f\n',
              iter, f_mid, short))

  f_star     = f_mid
  short_star = short

  if (abs(short - SHORT_RUN_TARGET) < SHORT_TOL) break

  # Increasing in f: too small (below target) => raise f.
  if (short < SHORT_RUN_TARGET) f_lo = f_mid else f_hi = f_mid
}


#-------------------------------------------------------------------------------
# Step 7: Report
#-------------------------------------------------------------------------------

E_int_short = short_star

# Frozen r_D_B = 0 cells: single pool r_D_B = r_B, so exactly the r_B = 0 cells;
# these stay at r_D = 0 and contribute no scenario response.
n_frozen = sum(grid_packed$r_B[bathtub_ages_chr, , drop = FALSE] == 0)
n_cells  = length(grid_packed$r_B[bathtub_ages_chr, , drop = FALSE])

cat('\nCalibrated (spec v3, entropy cost, single pool):\n')
cat(sprintf('  KG_DYN_DEFAULT_ETA     = %.4f  (long-run  semi = %+7.4f  target %+7.4f)\n',
            eta_star, E_int_long, LONG_RUN_TARGET))
cat(sprintf('  KG_DYN_TIMEABLE_SHARE  = %.4f  (short-run semi = %+7.4f  target %+7.4f)\n',
            f_star, E_int_short, SHORT_RUN_TARGET))
cat(sprintf('  frozen r_D_B=0 cells   = %d of %d bathtub cells (r_B = 0)\n',
            n_frozen, n_cells))
cat(sprintf(paste0('\n  E_int_long  = %+.4f  (measure_dilution.sbatch arg 3)\n',
                   '  E_int_short = %+.4f  (measure_dilution.sbatch arg 4)\n'),
            E_int_long, E_int_short))
cat('\nUpdate KG_DYN_DEFAULT_ETA and KG_DYN_TIMEABLE_SHARE in src/sim/kg_dynamics.R.\n')

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
  '  timeable_share     = %s,\n',
  '  applier_allocation = \'%s\',\n',
  '  ref_wedge          = %s,\n',
  '  timing_window      = %dL,\n',
  '  tax_data_vintage   = \'%s\',\n',
  '  macro_vintage      = \'%s\'\n',
  ')\n',
  '# also set KG_DYN_DEFAULT_ETA default to %s and KG_DYN_TIMEABLE_SHARE to %s\n'),
  as.character(Sys.Date()), KG_DYN_SPEC_VERSION,
  format(round(eta_star, 4)), format(round(f_star, 4)),
  KG_DYN_APPLIER_ALLOCATION,
  format(KG_DYN_TIMING_REF_WEDGE),
  as.integer(KG_DYN_TIMING_WINDOW), td_vint, macro_vint,
  format(round(eta_star, 4)), format(round(f_star, 4))))


#-------------------------------------------------------------------------------
# Bonus: long-run semi-elasticity profile by sim year at the calibrated point
#-------------------------------------------------------------------------------

profile_years = function(eta_val, timeable_share) {

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = tau_S_long_mat,
    years          = YEARS,
    tau_B_mat      = tau_B_mat,
    timeable_share = timeable_share,
    timing_window  = KG_DYN_TIMING_WINDOW,
    ref_wedge      = KG_DYN_TIMING_REF_WEDGE,
    ages_bathtub   = AGES_BATHTUB
  )

  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               eta          = eta_val,
                               beta_by_year = beta_by_year)
  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_long_mat,
                               c_phi_mat    = 0,
                               kappa_mat    = pass1$kappa,
                               eta          = eta_val,
                               beta_by_year = beta_by_year)

  delta = setNames(rep(0, length(AGES_BATHTUB)), bathtub_ages_chr)
  out   = tibble(sim_year = integer(), year = integer(), semi_elast = numeric())

  for (j in seq_along(YEARS)) {
    t  = YEARS[j]; bt = baseline_cells[[as.character(t)]]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t       = bt,
      r_ordinary_S     = pass2$r_D[bathtub_ages_chr, j],
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j]
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(delta, bt, A, omega, r_S_vec, zero_route_vec)
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
print(as.data.frame(profile_years(eta_star, f_star) %>%
                      filter(sim_year %in% c(1, 5, 10, 20, 30))),
      row.names = FALSE)

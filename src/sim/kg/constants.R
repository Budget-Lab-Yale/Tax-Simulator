#-------------------------------------------------------------------------------
# kg/constants.R
#
# Contains constants for the capital gains realization model
#-------------------------------------------------------------------------------

# Taxpayers choose how much of their unrealized gain to realize each year. The
# model solves that choice for a representative cell of each age, and tracks the
# resulting change in the stock of unrealized gains over time.
#
# Per dollar of unrealized gain, the cell chooses a realization rate r to
# maximize
#
#   W(a,t) = max_r { kappa*r - C(r) - tau*r
#                    + (1 - r) * [beta*(1-m)*W(a+1,t+1) + beta*m*F(a,t)] }
#
# where kappa is the benefit of realizing, tau the tax rate on the gain, m the
# mortality rate, beta the discount factor, and F the value of the tax forgiven
# at death. C is the cost of realizing away from the cell's baseline rate. The
# marginal cost of realizing is then
#
#   MC(a,t) = tau + beta*(1-m)*W(a+1,t+1) + beta*m*F
#
# The whole baseline realization rate responds. There is no split between a
# responsive and an inert share and no floor.
#
# The cost C is anchored so that its derivative is zero at the baseline rate.
# That has two convenient consequences: solving the baseline recovers kappa
# exactly, and the scenario's realization rate has a closed form. Which form C
# takes is set by kg.response_form in the kg settings; see below.
#
# Short-run retiming is handled separately, as an overlay: a calibrated share of
# all baseline realizations moves across a window of years around the reform.
# The overlay nets to zero under a permanent uniform rate change, so it leaves
# the long-run response alone.
#
# The work is split in two. kg_dyn_run_bathtub_pass() solves the baseline once,
# then solves each scenario year and steps the stock of gains forward, writing
# one state file per scenario-year. The behavior module in
# src/behavior/kg_dynamics/turnover.R then reads those files and turns the cell
# quantities into per-record adjustments to kg_lt.
#
# The five tracked asset classes are currently pooled into one. Splitting them
# out is on the roadmap.
#
# Notes: other/kg_model_tests/



#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

KG_DYN_AGE_MIN          = 18
KG_DYN_AGE_MAX          = 80      # top code, matching the rest of the model
KG_DYN_AGE_MAX_BELLMAN  = 119     # SSA life tables reach certain death at 119

# The discount factor used when the solver runs on its own in unit tests is
# beta_fallback in the kg settings. A simulation instead builds a year-varying
# real rate from the 10-year Treasury rate, deflated by CPI-U growth.

# Two functional forms are available for the cost of realizing away from the
# baseline rate. Both fit the same local elasticity, and both give the same
# answer at the baseline, so they differ only in how far they extrapolate away
# from it. That matters most for the revenue-maximizing rate. Which one is used
# is set by response_form in the kg settings.
#
#   'logs'   assumes a constant elasticity with respect to the net-of-tax rate,
#            the specification Agersnap and Zidar prefer:
#            r = r_B * ((1 - MC) / (1 - MC_B)) ^ eta_tilde
#   'levels' assumes a constant semi-elasticity, so that log realizations are
#            linear in the marginal cost:
#            r = r_B * exp(-eta * (MC - MC_B))
#
# Each form carries its own calibrated pair of parameters. Results are not
# comparable across forms, so regenerate a vintage before comparing.
kg_dyn_response_form = function() {
  v = kg_setting('response_form')
  if (!v %in% c('levels', 'logs'))
    stop(sprintf(paste0("kg_dynamics: assumption kg.response_form must be ",
                        "'levels' or 'logs'; got '%s'."), v))
  v
}

# The net-of-tax form is undefined as the marginal cost approaches 1. Refuse to
# run, rather than clamp, if any cell reaches this. Marginal costs in practice
# sit far below it, so a hit means the discount series or the death continuation
# is broken. The levels form has no such limit.
KG_DYN_LOGS_MC_CAP      = 0.98

# The model has two margins. On the permanent margin, all gains respond through
# the choice of realization rate, so eta is the long-run elasticity directly. On
# the short-run margin, a calibrated share of all baseline realizations retimes
# across a window of years around the reform.
#
# That timeable share is calibrated against the short-run announcement moment,
# taking eta as given. The two identify separately: the retiming overlay nets to
# zero under a permanent uniform rate change, so the long-run moment does not
# depend on the share. Each response form has its own share. Both live in
# config/calibrations/kg/bathtub.yaml, and a value of NA there leaves the model
# uncalibrated, which kg_dyn_run_bathtub_pass() refuses to simulate.

# Deemed realizations are scaled down by deemed_avoidance in the kg settings, for
# the valuation games and noncompliance that JCT and Treasury assume and this
# model does not represent. This is a measurement parameter rather than tax law.
# It applies only when allocating to records, and not to the burden the taxpayer
# internalizes when choosing whether to realize during life. The default of 0.25
# matches the estate side's discount on closely held assets.
#
# TODO: this is the same object as the estate tax's reporting factor by asset
# class. The two should be reconciled on one discount per class.

# The share of retimeable dollars that actually moves toward the best year in the
# window is the rate difference between the two years over timing_ref_wedge in
# the kg settings, capped at 1. At the shipped wedge, 5 points moves the whole
# bucket and 1 point moves a fifth of it. The rate difference is measured on law
# alone, so it is exactly zero when the schedule faced during life is unchanged.
# Measuring it after the mechanical adjustments instead would let a few basis
# points of income effect retime dollars against noise.

# Age distribution of heirs, weighted by dollars inherited, from the 2022 SCF.
# Restricted to transfers at death and weighted by the recency probabilities in
# Gale and Sabelhaus (2024). Built by
# other/kg_model_tests/build_heir_distribution.R, which defines the filters.
# Treated as a constant because the survey does not vary by year over the
# projection horizon.
KG_DYN_HEIR_DISTRIBUTION_PATH = './resources/heir_distribution_scf2022.csv'

# The elasticity parameters, eta for the levels form and eta_logs for the
# net-of-tax form, are in config/calibrations/kg/bathtub.yaml. Both are pinned by
# running the full simulator across a grid of candidate values, measuring the
# realized elasticity at simulation year 30, and inverting for the value that
# hits a target of -2.52, the literature realization elasticity of -0.6 divided
# by a top rate of 0.238. Re-pinning is arithmetic given the grid; the protocol
# and the fitted grids are in other/top_tax/eta_dial/ and
# other/kg_model_tests/form_ab/.
#
# Response is increasing in the size of eta. Re-pin whenever the Tax-Data
# vintage, the timeable share, the reference wedge, the discount series, the
# allocation to records, or the choice problem itself changes. The staleness
# check on the entries in bathtub.yaml catches this at parse time.
#
# The net-of-tax parameter is smaller than the levels one because the full-sim
# slope is steeper in that form, so a smaller value reaches the same target.

# Resolve the calibrated pair for whichever response form is active, so that
# nothing downstream has to know which form it is running under. A value of NA
# leaves the model uncalibrated, which kg_dyn_run_bathtub_pass() then refuses to
# simulate.
kg_dyn_as_pinned = function(v) {
  if (identical(as.character(v), 'NA')) NA_real_ else as.numeric(v)
}
kg_dyn_active_eta = function(form = kg_dyn_response_form()) {
  kg_dyn_as_pinned(
    if (identical(form, 'logs')) kg_bathtub('eta_logs')
    else                         kg_bathtub('eta'))
}
kg_dyn_active_timeable_share = function(form = kg_dyn_response_form()) {
  kg_dyn_as_pinned(
    if (identical(form, 'logs')) kg_bathtub('timeable_share_logs')
    else                         kg_bathtub('timeable_share'))
}

# Two settings control how the change in the stock of gains is spread within an
# age cell. Both are in config/calibrations/kg/settings.yaml.
#
# dg_allocation sets the effective mortality rate the death and survivor flows
# use. Under 'G' the change is spread in proportion to holdings of unrealized
# gains, and under 'R' in proportion to realizations, falling back to holdings
# where there are none. It matters only where gains are taxed at death, since
# under step-up the death channel is off.
#
# applier_allocation sets how the realized stock is spread across the records in
# a cell: 0 in proportion to realizations, 1 in proportion to holdings, or a
# blend in between. A realized dollar needs a holder who sells, so neither proxy
# is right on its own and the shipped value is the midpoint. Revenue under
# carryover is monotone in this, highest at 0; revenue under deemed realization
# does not depend on it. Note that the death flows keep using dg_allocation
# regardless: whose gains die follows holdings, while this controls whose get
# sold. The elasticity calibration is conditioned on the shipped 0.5, so re-pin
# if it changes.

KG_DYN_ASSET_CLASSES    = c('equities', 'pass_throughs',
                            'primary_home', 'other_home', 're_fund')
KG_DYN_ASSET_VALUE_COLS = paste0('value.', KG_DYN_ASSET_CLASSES)
KG_DYN_ASSET_BASIS_COLS = paste0('basis.', KG_DYN_ASSET_CLASSES)
KG_DYN_ASSET_GAIN_COLS  = paste0('gain.',  KG_DYN_ASSET_CLASSES)

# Gross estate assets are listed once, as ESTATE_ASSET_COLS in
# src/calc/functions/tax/estate.R. This file and wealth_dynamics.R both read it
# from there, so that all three agree on what counts as an estate asset.

# The coefficients giving the probability that an estate goes to charity are
# char_* in config/calibrations/kg/settings.yaml. No source for the four fitted
# coefficients is recorded anywhere in the repository; the config flags this.

# Trustees Report Alternative 2, averaged over men and women, since the model
# does not distinguish them. Supplies mortality past age 80.
KG_DYN_LIFE_TABLE_M_PATH = './resources/PerLifeTables_M_Alt2_TR2024.csv'
KG_DYN_LIFE_TABLE_F_PATH = './resources/PerLifeTables_F_Alt2_TR2024.csv'


# How gains are treated at death, by asset class. Tax law carries one
# pref.kg_death_regime_{class} per class, and kg_dyn_build_regime_mix() averages
# them to the cell, weighting by the stock of gains held.
KG_DYN_REGIME_TRIPLET = list(
  '0' = list(vanish = 1, route = 0, realize = 0),  # step_up
  '1' = list(vanish = 0, route = 1, realize = 0),  # carryover
  '2' = list(vanish = 0, route = 0, realize = 1)   # deemed_realization
)



#-------------------------------------------------------------------------------
# Realization timing
#-------------------------------------------------------------------------------

kg_dyn_validate_timing_params = function(
    timeable_share = kg_dyn_active_timeable_share(),
    timing_window  = kg_setting('timing_window'),
    ref_wedge      = kg_setting('timing_ref_wedge')) {

  # Checks the timing parameters. The timeable share is the fraction of all
  # realizations that retimes across the window, and must be a single number
  # between 0 and 1. NA is allowed here, meaning uncalibrated, and
  # kg_dyn_run_bathtub_pass() stops on it later.
  if (!is.na(timeable_share) &&
      (length(timeable_share) != 1 || !is.finite(timeable_share) ||
       timeable_share < 0 || timeable_share > 1)) {
    stop(sprintf(paste0('kg_dynamics: kg.timeable_share must be a ',
                        'finite scalar in [0, 1] (or NA when uncalibrated); got %s.'),
                 paste(format(timeable_share), collapse = ', ')))
  }
  if (length(timing_window) != 1 || is.na(timing_window) ||
      timing_window < 0 || timing_window != as.integer(timing_window)) {
    stop('kg_dynamics: kg.timing_window must be a nonnegative integer.')
  }
  if (length(ref_wedge) != 1 || !is.finite(ref_wedge) || ref_wedge <= 0) {
    stop('kg_dynamics: kg.timing_ref_wedge must be a positive finite number.')
  }

  invisible(TRUE)
}

# Not called at source time: these values belong to a scenario and no scenario is
# active yet. kg_dyn_run_bathtub_pass() calls it instead.




#-------------------------------------------------------------------------------
# kg_dynamics.R
#
# Capital-gains dynamics behavioral module. Implements the law of motion for
# the policy-induced delta in unrealized capital gains via two representative-
# cell Bellman states: an ordinary state whose control is the discretionary
# realization rate r_D, and a forced-window state whose control is the
# immediate-realization share q.
#
# Architecture:
#   1. Bathtub pre-pass (kg_dyn_run_bathtub_pass): for each scenario, build an
#      extended age grid [18, A_max_bellman=119] using PerLifeTables mortality
#      past age 80; solve the baseline Bellman once (Pass 1) to recover
#      kappa(a,t), W_B(a,t), MC_B(a,t); then for each year solve the scenario
#      Bellman (Pass 2) to get r_D,S(a,t); apply the bathtub recurrence
#      (survivor + inheritance flows); persist one state file per year per
#      scenario.
#   2. Behavior module (config/scenarios/behavior/kg_dynamics/turnover.R):
#      pure allocator. Reads its year's state file and translates cell-level
#      quantities to per-record kg_lt adjustments via kg_dyn_apply_to_records.
#
# Ordinary Bellman primitives. The representative cell maximizes per dollar of
# unrealized gain:
#   W^j(a,t) = max_{r_D in [0, 1 - r_exog_B]} {
#       kappa(a,t)*r_D - (psi/2)*r_D^2
#     - tau^j(a,t)*r_D
#     + (1 - r_exog_B - r_D) *
#         [beta*(1-m) W^j(a+1,t+1) + beta*m*F^j(a,t)]
#   }
# where r_exog_B = lambda*r_B is the baseline forced-window realization share
# outside the ordinary Bellman bucket,
# F^j = (1 - c_phi^j)*tau^j is the death-state tax-liability forgiveness
# value (c_phi^j is the regime's holder-internalized burden share: 0 step-up,
# theta carryover, 1 deemed). Marginal cost of realization:
#   MC^j(a,t) = tau^j + beta*(1-m)*W^j(a+1,t+1) + beta*m*F^j.
# Interior FOC: r_D = (kappa - MC)/psi, clipped to [0, 1 - r_exog_B].
# kappa(a,t) is recovered from baseline so r_D^B is the ordinary bucket:
#   kappa = MC^B + psi * r_D^B   (at corner cells with r_D^B = 0, kappa = MC^B).
#
# Forced-window Bellman primitives. Entrants start in F1 and choose q, the share
# that realizes now instead of waiting one year. F0 is the deadline state and
# must realize:
#   F0^j(a,t+1) = -tau^j(a,t+1)
#   F1^j(a,t) = max_{q in [0,1]} {
#       q*(-tau^j(a,t)) + (1-q)*beta*F0^j(a,t+1)
#     + alpha_B(a,t)*q - (ref_wedge/2)*(q - q_B)^2
#   }
# alpha_B(a,t) is recovered so the baseline FOC reproduces q_B. q_B is set
# above 0.5 to keep baseline entrant inference stable in sparse cells.
#
# Current implementation collapses the five tracked wealth classes into a
# single asset bucket; per-asset-class disaggregation is on the roadmap.
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

KG_DYN_AGE_MIN          = 18
KG_DYN_AGE_MAX          = 80      # bathtub topcode (matches simulator)
KG_DYN_AGE_MAX_BELLMAN  = 119     # Bellman extended-grid terminal age; SSA
                                  # PerLifeTables hit q(x)=1 at 119
KG_DYN_BETA             = 0.978   # fallback annual discount factor, used
                                  # only by isolated solver unit tests that
                                  # don't attach Macro-Projections. Production
                                  # paths build a year-varying real-rate
                                  # discount series from tsy_10y / cpiu via
                                  # kg_dyn_load_beta_series; see Bellman
                                  # primitives below. 0.978 corresponds to a
                                  # ~2.2% real rate, the rough mid-horizon
                                  # value implied by the default Macro-
                                  # Projections vintage.
# Baseline realization bucket shares. The fixed share is no longer supported in
# the forced-window Bellman; keep the constant only for compatibility with old
# call sites and fail fast if it is set nonzero. The planned share is the
# forced-window share lambda. The remainder is the ordinary Bellman-controlled
# share.
KG_DYN_SHARE_FIXED      = 0
KG_DYN_SHARE_PLANNED    = 0.6152
KG_DYN_TIMING_WINDOW    = 1L
KG_DYN_FORCED_Q_B       = 0.5

# Reference wedge controlling the convex timing cost in the forced-window
# Bellman. Default 5pp means a 5pp current-vs-deadline value advantage moves
# the bounded FOC solution for q by 1.
KG_DYN_TIMING_REF_WEDGE = 0.05

# Backward-compatible alias used by existing callers and diagnostics. Nonzero
# fixed/nonresponsive buckets are unsupported in the forced-window Bellman.
KG_DYN_PHI_I            = KG_DYN_SHARE_FIXED
KG_DYN_HEIR_SHIFT       = 30      # average decedent-to-heir age gap
KG_DYN_HEIR_SIGMA       = 5       # std dev of heir age distribution

# Default psi (global curvature of the quadratic realization benefit).
# Jointly calibrated with KG_DYN_SHARE_PLANNED by
# other/kg_model_tests/calibrate.R against two moments:
#   - long-run permanent semi-elasticity dlog(R)/dtau ~= -0.6/0.238 under
#     a 1pp uniform tau bump on the step-up baseline, anchored at sim year
#     30 (the response ramps over the first ~10 years as the bathtub
#     accumulates stock, then plateaus);
#   - short-run announced-shock semi-elasticity dlog(R(t))/dtau(t+1) under
#     a 5pp delayed permanent shock, anchored at sim year 1 (twice the
#     long-run magnitude with opposite sign).
# Set to NA_real_ here to force fail-fast at run time if someone tries to
# run kg_dynamics without an up-to-date calibration. Re-run calibrate.R
# whenever Tax-Data vintage, bucket shares, ref_wedge, the discount series
# (Macro-Projections vintage), or any Bellman primitive (mortality
# weighting, age-tail r_B treatment, etc.) changes, then paste the printed
# values below.
KG_DYN_DEFAULT_PSI      = 33.5688

# Within-cell allocation rule for the policy-induced delta dG.
# Determines which "effective cell mortality" the recurrence uses for
# stock-allocation in the death and survivor channels.
#
#   "G" — dG within a cell is allocated proportional to G_unit (each
#         holder's share of the cell's gain stock).  Effective rate
#         m_eff = sum(w*m*G) / sum(w*G).  This is the simplest rule and
#         corresponds to the inheritance-flow story (heirs receive a share
#         of decedent stock proportional to their existing G).
#
#   "R" — dG within a cell is allocated proportional to positive realized
#         gains (kg_lt > 0).  Effective rate m_eff = sum(w*m*R)/sum(w*R).
#         Corresponds to the lock-in story (deferred realizations stay
#         with the records that were going to realize the most).
#         Falls back to "G" when R_B = 0 (e.g., young heir cohorts under
#         carryover that haven't yet realized).
#
# Both rules give a per-record-correct sum under their respective allocation
# assumption. The choice affects carryover and (mildly) deemed scoring;
# step-up scenarios are unchanged because the death channel is shut off.
KG_DYN_DG_ALLOCATION    = 'G'

KG_DYN_ASSET_VALUE_COLS = c('value.equities', 'value.pass_throughs',
                            'value.primary_home', 'value.other_home',
                            'value.re_fund')
KG_DYN_ASSET_BASIS_COLS = c('basis.equities', 'basis.pass_throughs',
                            'basis.primary_home', 'basis.other_home',
                            'basis.re_fund')

# Life-table paths for ages 81+ in the Bellman extended grid. Year-varying
# Trustees Report Alternative 2 projections, blended 50/50 male/female since
# the cohort module does not track gender.
KG_DYN_LIFE_TABLE_M_PATH = './resources/PerLifeTables_M_Alt2_TR2024.csv'
KG_DYN_LIFE_TABLE_F_PATH = './resources/PerLifeTables_F_Alt2_TR2024.csv'


# Death-regime taxonomy. YAML pref.kg_death_regime is an integer code;
# KG_DYN_REGIME_BY_CODE maps it to a name; KG_DYN_REGIMES carries the
# canonical (delta_vanish, delta_route, delta_realize, c_phi_default) tuple
# for each name. The bequest motive theta is supplied separately and overrides
# c_phi_default for carryover.
#
# c_phi is the death-state burden share the current holder internalizes:
# 0 step-up (full forgiveness), theta carryover (partial), 1 deemed (no
# forgiveness). The Bellman maps it to the forgiveness value F = (1-c_phi)*tau
# (= tau under step-up, (1-theta)*tau under carryover, 0 under deemed).
KG_DYN_REGIME_BY_CODE = c('0' = 'step_up',
                          '1' = 'carryover',
                          '2' = 'deemed_realization')

KG_DYN_REGIMES = list(
  step_up            = list(c_phi_default = 0,
                            delta_vanish  = 1, delta_route = 0, delta_realize = 0),
  carryover          = list(c_phi_default = NA,           # set from theta
                            delta_vanish  = 0, delta_route = 1, delta_realize = 0),
  deemed_realization = list(c_phi_default = 1,
                            delta_vanish  = 0, delta_route = 0, delta_realize = 1)
)



#-------------------------------------------------------------------------------
# Record-level helpers
#-------------------------------------------------------------------------------

kg_dyn_attach_record_attrs = function(tax_units) {

  #----------------------------------------------------------------------------
  # Adds three derived columns to tax_units used by the bathtub recurrence:
  #   G_unit       : per-record unrealized gain stock, sum_k max(0, value_k -
  #                  basis_k) across the five tracked wealth classes
  #   m_household  : household death probability. q_death1*q_death2 for joint
  #                  filers; q_death1 otherwise
  #   age_cohort   : cohort age. max(age1, age2) for joint, age1 otherwise.
  #                  Top-coded at KG_DYN_AGE_MAX, bottom-coded at AGE_MIN.
  #
  # Returns: tax_units augmented with G_unit, m_household, age_cohort.
  #----------------------------------------------------------------------------

  values = as.matrix(tax_units[, KG_DYN_ASSET_VALUE_COLS])
  basis  = as.matrix(tax_units[, KG_DYN_ASSET_BASIS_COLS])
  diffs  = values - basis
  diffs[is.na(diffs)] = 0
  diffs[diffs < 0]    = 0

  tax_units %>%
    mutate(
      G_unit      = rowSums(diffs),
      m_household = if_else(filing_status == 2 & !is.na(q_death2),
                            q_death1 * q_death2,
                            q_death1),
      m_household = if_else(is.na(m_household), 0, m_household),
      age_cohort  = if_else(filing_status == 2,
                            pmax(age1, age2, na.rm = TRUE),
                            age1),
      age_cohort  = pmax(KG_DYN_AGE_MIN, pmin(KG_DYN_AGE_MAX, age_cohort))
    )
}



#-------------------------------------------------------------------------------
# Cell aggregation (with sparse-cell fallback)
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cells = function(tax_units, ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Weight-aggregates per-record (G_unit, kg_lt, m_household) to age cells.
  # tax_units must already have G_unit, m_household, age_cohort attached.
  #
  # Note on R_B convention: R_B is the cell's positive realized gains
  # (sum(weight * pmax(kg_lt, 0))), not the signed sum. The spec defines
  # baseline realizations as positive flows out of the unrealized-gain stock,
  # and using positive-only sums keeps r_B >= 0 and ensures the per-record
  # allocation shares (pmax(kg_lt, 0) / R_B) sum to 1.
  #
  # Sparse-cell fallback (spec §5.1): cells with G_B > 0 but R_B = 0 inherit
  # the gain-stock-weighted aggregate r_B across all age cells. This prevents
  # young heir cohorts (carryover / deemed inflows) from getting r_S = 0
  # forever just because they had no historical realization activity.
  #
  # Returns: tibble with age, G_B, R_B, r_B, m, mG_record, mR_record.
  #----------------------------------------------------------------------------

  agg = tax_units %>%
    group_by(age_cohort) %>%
    summarise(G_B       = sum(weight * G_unit,                       na.rm = TRUE),
              R_B       = sum(weight * pmax(kg_lt, 0),               na.rm = TRUE),
              m_num     = sum(weight * m_household,                  na.rm = TRUE),
              mG_record = sum(weight * m_household * G_unit,         na.rm = TRUE),
              mR_record = sum(weight * m_household * pmax(kg_lt, 0), na.rm = TRUE),
              w_total   = sum(weight,                                na.rm = TRUE),
              .groups   = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(G_B, R_B, m_num, mG_record, mR_record, w_total),
                  ~ if_else(is.na(.), 0, .)),
           m   = if_else(w_total > 0, m_num / w_total, 0),
           r_B = if_else(G_B     > 0, R_B   / G_B,     0))

  # Pooled rate for sparse cells: only consider cells with R_B > 0 so the
  # cells we're imputing don't drag the imputation toward zero. Should be a
  # no-op under the full-sample requirement enforced in run_bathtub_pass(),
  # but kept for safety on edge cases (e.g. carryover heir cohorts at the
  # youngest ages, where a single-year sample may still be empty).
  ok         = out$R_B > 0
  r_B_pooled = if (any(ok)) sum(out$R_B[ok]) / sum(out$G_B[ok]) else 0

  out %>%
    mutate(r_B = if_else(G_B > 0 & R_B == 0, r_B_pooled, r_B)) %>%
    select(age, G_B, R_B, r_B, m, mG_record, mR_record) %>%
    arrange(age)
}



#-------------------------------------------------------------------------------
# Aging and heir matrices
#-------------------------------------------------------------------------------

kg_dyn_build_heir_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    shift = KG_DYN_HEIR_SHIFT,
                                    sigma = KG_DYN_HEIR_SIGMA) {

  #----------------------------------------------------------------------------
  # Row-stochastic heir-allocation matrix omega[a, h] = share of decedent-age-a
  # gains routed to heir-age h. Centered at a - shift with Gaussian noise
  # sigma. Placeholder until estate module hookup.
  #----------------------------------------------------------------------------

  W = outer(ages, ages, function(a, h) dnorm(h, mean = a - shift, sd = sigma))
  W = W / rowSums(W)
  stopifnot(all(abs(rowSums(W) - 1) < 1e-12))
  rownames(W) = colnames(W) = ages
  W
}



kg_dyn_build_aging_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # A[a, h] = 1 if h = a + 1; A[a_max, a_max] = 1 (topcode loops). Spec §3.4.
  #----------------------------------------------------------------------------

  n = length(ages)
  A = matrix(0, n, n, dimnames = list(ages, ages))
  for (i in seq_len(n - 1)) A[i, i + 1] = 1
  A[n, n] = 1
  A
}



#-------------------------------------------------------------------------------
# Life-table extension (ages 81 to A_max_bellman, year-varying)
#-------------------------------------------------------------------------------

kg_dyn_load_life_table_extension = function(years,
                                            ages_ext = (KG_DYN_AGE_MAX + 1):
                                                       KG_DYN_AGE_MAX_BELLMAN,
                                            path_M = KG_DYN_LIFE_TABLE_M_PATH,
                                            path_F = KG_DYN_LIFE_TABLE_F_PATH) {

  #----------------------------------------------------------------------------
  # Loads SSA Trustees Report Alternative-2 mortality projections for ages
  # past the simulator topcode. The simulator's own life table (per-year cell
  # aggregates, ages 18-80) is used as-is for [18, 80]; this loader supplies
  # the post-topcode tail [81, 119] that the Bellman backward induction needs
  # for a true terminal condition (q(119) = 1 in the SSA tables).
  #
  # The PerLifeTables_*_Alt2_TR2024.csv files have a 5-line header followed
  # by Year,x,q(x),l(x),d(x),... rows. We pull q(x) for the requested years
  # and ages, blend male/female 50/50 (cohort module is gender-blind), and
  # return a matrix indexed [age, year].
  #
  # Parameters:
  #   - years    : integer vector of simulation years
  #   - ages_ext : ages past KG_DYN_AGE_MAX
  #   - path_M   : path to male-cohort life table
  #   - path_F   : path to female-cohort life table
  #
  # Returns: matrix of dim length(ages_ext) x length(years), entry [a, t] is
  #          gender-blended q(x) at age a in year t.
  #----------------------------------------------------------------------------

  load_one = function(path) {
    # The PerLifeTables files start with 4 lines of metadata, then a header
    # row (Year,x,q(x),...), then data. skip=4 lands fread on the header
    # row, header=TRUE consumes it. Column names from the file are odd
    # ("q(x)", "12a(x)", etc.); we slice to the first three by position and
    # rename to clean lowercase.
    raw = fread(path, skip = 4, header = TRUE, showProgress = FALSE)
    out = data.table(year = as.integer(raw[[1]]),
                     x    = as.integer(raw[[2]]),
                     q    = as.numeric(raw[[3]]))
    as_tibble(out) %>% filter(year %in% years, x %in% ages_ext)
  }

  M = load_one(path_M)
  Fm = load_one(path_F)

  stopifnot(nrow(M) == length(ages_ext) * length(years),
            nrow(Fm) == length(ages_ext) * length(years))

  blended = M %>%
    rename(q_M = q) %>%
    inner_join(Fm %>% rename(q_F = q), by = c('year', 'x')) %>%
    mutate(q = 0.5 * q_M + 0.5 * q_F)

  out = matrix(NA_real_, nrow = length(ages_ext), ncol = length(years),
               dimnames = list(as.character(ages_ext), as.character(years)))
  for (i in seq_len(nrow(blended))) {
    out[as.character(blended$x[i]), as.character(blended$year[i])] = blended$q[i]
  }
  stopifnot(all(!is.na(out)))
  out
}



#-------------------------------------------------------------------------------
# Real-rate discount factor series (year-varying)
#-------------------------------------------------------------------------------

kg_dyn_load_beta_series = function(macro_root, years) {

  #----------------------------------------------------------------------------
  # Builds the per-year Bellman discount factor from Macro-Projections.
  # Uses the real 10-year Treasury yield (Fisher-deflated by year-t YoY CPI-U
  # growth):
  #
  #   infl_t     = cpiu_t / cpiu_{t-1} - 1
  #   r_real_t   = (1 + tsy_10y_t / 100) / (1 + infl_t) - 1
  #   beta_t     = 1 / (1 + r_real_t)
  #
  # The Bellman compares "realize today and pay tau now" vs. "hold the asset
  # (whose nominal price grows with inflation) and pay tau on a nominally
  # larger gain later." Inflation cancels except in the discount of the tax
  # wedge, so the economically correct discount is the real rate; using
  # nominal tsy_10y would double-count inflation.
  #
  # historical.csv covers <= 2025, projections.csv covers >= 2026; both have
  # cpiu and tsy_10y columns on a continuous index, so we bind them and
  # compute YoY growth directly. The implied 2025->2026 inflation reflects
  # the projection team's near-term assumption.
  #
  # Parameters:
  #   - macro_root : path to a Macro-Projections vintage's baseline directory
  #                  (must contain historical.csv and projections.csv)
  #   - years      : integer vector of simulation years; must all be present
  #                  in the macro data
  #
  # Returns: named numeric vector beta_t, names = as.character(years),
  #          length = length(years).
  #----------------------------------------------------------------------------

  cpiu_tsy = c('historical.csv', 'projections.csv') %>%
    file.path(macro_root, .) %>%
    map(~ read_csv(.x, show_col_types = FALSE) %>%
              select(year, cpiu, tsy_10y)) %>%
    bind_rows() %>%
    arrange(year) %>%
    mutate(infl_t   = cpiu / lag(cpiu) - 1,
           r_real   = (1 + tsy_10y / 100) / (1 + infl_t) - 1,
           beta     = 1 / (1 + r_real))

  beta_df = cpiu_tsy %>% filter(year %in% years) %>% select(year, beta)

  missing = setdiff(years, beta_df$year)
  if (length(missing) > 0) {
    stop('kg_dyn_load_beta_series: years ',
         paste(missing, collapse = ', '),
         ' not present in macro_projections at ', macro_root)
  }
  if (any(is.na(beta_df$beta))) {
    stop('kg_dyn_load_beta_series: NA in real-rate discount factor for years ',
         paste(beta_df$year[is.na(beta_df$beta)], collapse = ', '),
         ' (likely missing prior-year cpiu for YoY differencing).')
  }

  beta_df = beta_df %>% arrange(match(year, years))
  setNames(beta_df$beta, as.character(beta_df$year))
}



kg_dyn_build_extended_grid = function(baseline_cells, life_ext, years,
                                       ages_bellman = KG_DYN_AGE_MIN:
                                                      KG_DYN_AGE_MAX_BELLMAN) {

  #----------------------------------------------------------------------------
  # Stitches the simulator's [18, 80] cell aggregates together with the
  # external life-table tail [81, 119] into a single per-year extended-grid
  # tibble keyed by age. The bathtub recurrence stays on [18, 80]; only the
  # Bellman uses the extended grid (to get a true mortality-driven terminal
  # condition).
  #
  # For ages 81+:
  #   - m is populated from life_ext (SSA Alternative-2 projections)
  #   - r_B is held flat at r_B(80, t), the observed topcode-pool rate. The
  #     simulator's age-80 cell already pools all 80+ taxpayers, so r_B(80)
  #     is the empirically-correct rate for the 80+ cohort and the simplest
  #     extrapolation is to assume it continues to age 119. Without this,
  #     ages 81+ would have r_B = 0, which makes the Bellman's continuation
  #     value at age 80 purely death-driven (no nontax realization benefit
  #     past the topcode) and over-states the regime-induced acceleration
  #     under deemed in older cohorts.
  #   - G_B, R_B, mG_record, mR_record stay at 0 (no observed stock past
  #     the topcode; the per-dollar Bellman doesn't need cell totals).
  #
  # Parameters:
  #   - baseline_cells : list keyed by year-string, each entry a tibble from
  #                      kg_dyn_aggregate_cells (ages 18-80)
  #   - life_ext       : matrix from kg_dyn_load_life_table_extension
  #                      (ages 81-119 x years)
  #   - years          : integer vector of simulation years
  #   - ages_bellman   : full extended-grid age range
  #
  # Returns: named list keyed by year-string, each entry a tibble on
  #          ages_bellman with columns (age, G_B, R_B, r_B, m, mG_record,
  #          mR_record).
  #----------------------------------------------------------------------------

  ages_ext = setdiff(ages_bellman, KG_DYN_AGE_MIN:KG_DYN_AGE_MAX)

  out = list()
  for (t in years) {
    key = as.character(t)
    inner = baseline_cells[[key]]
    r_B_topcode = inner$r_B[inner$age == KG_DYN_AGE_MAX]
    ext = tibble(age       = ages_ext,
                 G_B       = 0,
                 R_B       = 0,
                 r_B       = r_B_topcode,
                 m         = as.numeric(life_ext[as.character(ages_ext), key]),
                 mG_record = 0,
                 mR_record = 0)
    out[[key]] = bind_rows(inner, ext %>% select(names(inner))) %>%
      arrange(age)
  }
  out
}



#-------------------------------------------------------------------------------
# Bellman backward induction
#
# Pass 1 (baseline) solves W_B and recovers kappa(a, t) so the observed
# ordinary realization bucket r_D_B = (1 - planned_share)*r_B
# is the Pass-1 cell's optimal choice.
# Pass 2 (scenario) solves W_S using kappa(a, t) from Pass 1 and the
# scenario-specific (tau_S, c_phi_S) pair, producing r_D_S(a, t) via the
# clipped quadratic FOC r_D = clip((kappa - MC)/psi, 0, 1 - r_exog_B).
#
# Both passes solve on the extended age grid [18, 119], outer loop backward
# in time, inner loop backward in age. Terminal condition:
#   W[A_max+1, t+1] = 0          (age cap; m(119) ~ 1, so this binds quickly)
# For year t_max, we additionally need W[a+1, t_max+1]. We seed that column
# by running a stationary backward-induction sweep at t_max using year-t_max
# primitives extended forward indefinitely.
#-------------------------------------------------------------------------------

kg_dyn_pack_tau = function(tau_list, years,
                            ages_bellman = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN) {

  #----------------------------------------------------------------------------
  # Packs the per-year tau lists from kg_dyn_load_bathtub_inputs (each a
  # named vector over [18, 80]) into a single matrix on the Bellman grid.
  # Ages 81+ inherit tau(80) since the topcode pool already represents 80+.
  #----------------------------------------------------------------------------

  out = matrix(0, length(ages_bellman), length(years),
               dimnames = list(as.character(ages_bellman), as.character(years)))
  for (t in years) {
    key = as.character(t)
    v = tau_list[[key]]
    out[as.character(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX), key] = as.numeric(v)
    out[as.character((KG_DYN_AGE_MAX + 1):KG_DYN_AGE_MAX_BELLMAN), key] =
      as.numeric(v[as.character(KG_DYN_AGE_MAX)])
  }
  out
}



kg_dyn_pack_baseline_grid = function(grid_ext, years,
                                     ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  #----------------------------------------------------------------------------
  # Stacks per-year extended-grid tibbles into named matrices indexed
  # [age, year], for the columns the Bellman needs (m, r_B).
  #
  # m is gain-stock-weighted: m = sum(w*m_household*G_unit) / sum(w*G_unit).
  # The Bellman is normalized per dollar of unrealized gain, so the mortality
  # input must be the probability that the *dollar's* holder dies, not the
  # average taxpayer in the cell. Wealthier holders die less, so taxpayer-
  # weighted m is biased upward by 2-3x relative to gain-weighted m in
  # practice. The recurrence (see kg_dyn_step_recurrence) already adopts the
  # same convention; this brings the Bellman in line.
  #
  # Falls back to taxpayer-weighted m where G_B = 0 (ages 81+ on the
  # extended grid, and any cells without observed gain stock). At those ages
  # the gain-weighted average is undefined, and the SSA life-table mortality
  # (which is what grid_ext$m holds for 81+) is the right input anyway.
  #----------------------------------------------------------------------------

  n_ages  = length(ages_bellman)
  n_years = length(years)
  ages_chr = as.character(ages_bellman)
  years_chr = as.character(years)

  m   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = grid_ext[[t_chr]]
    m_gw = ifelse(bt$G_B > 0, bt$mG_record / bt$G_B, bt$m)
    m  [, t_chr] = pmin(pmax(m_gw, 0), 1)
    r_B[, t_chr] = bt$r_B
  }
  list(m = m, r_B = r_B)
}



kg_dyn_bellman_sweep_age = function(W_next, m_col, r_B_col, tau_col,
                                     c_phi, psi, phi_I, beta,
                                     planned_share = KG_DYN_SHARE_PLANNED,
                                     kappa_col = NULL, stationary = FALSE) {

  #----------------------------------------------------------------------------
  # One age-backward sweep through [a_min, a_max] for a single year column.
  # Used for both the stationary terminal solve and the regular year-by-year
  # backward induction.
  #
  # When kappa_col is NULL, this is a Pass 1 (baseline) sweep: r_D_B is
  # derived directly from r_B after removing the forced-window bucket,
  # and the cell intercept kappa is recovered so r_D_B is the optimal choice
  # given MC_B = tau + beta*(1-m)*W_next + beta*m*F.
  #
  # When kappa_col is supplied, this is a Pass 2 (scenario) sweep:
  # MC_S is computed from scenario primitives and the quadratic FOC gives
  # r_D = clip((kappa - MC_S)/psi, 0, 1 - r_exog_B) at each cell.
  #
  # When stationary = FALSE (default, regular year-by-year case), the
  # continuation value at age a uses W_next[a+1] -- next year's W at age
  # a+1. This is the "year t uses W[a+1, t+1]" recursion.
  #
  # When stationary = TRUE (used at the t_max terminal column), the
  # continuation value at age a uses the freshly-computed W[a+1] from
  # within this very sweep -- i.e., a stationary backward solve over age
  # alone, treating primitives as constant forward. W_next is ignored.
  #
  # In both cases, at the top age (i == n_ages) the continuation is 0
  # by the terminal-condition convention W[A_max+1, .] = 0.
  #
  # The cell's per-dollar value satisfies:
  #   W(a,t) = kappa*r_D - (psi/2)*r_D^2 - tau*r_D
  #          + (1 - r_exog_B - r_D) * [beta*(1-m)*W_next + beta*m*F]
  # with F = (1 - c_phi)*tau the death-state forgiveness value.
  #
  # Returns a list with numeric vectors of length n_ages:
  #   W      : W at this column
  #   MC     : marginal cost of realizing one more dollar at this column
  #   r_D    : discretionary realization rate
  #   kappa  : cell intercept (recovered in Pass 1; passed through in Pass 2)
  #----------------------------------------------------------------------------

  n_ages = length(m_col)
  W     = numeric(n_ages)
  MC    = numeric(n_ages)
  r_D   = numeric(n_ages)
  kappa = numeric(n_ages)

  is_baseline_pass = is.null(kappa_col)

  for (i in n_ages:1) {
    m_i     = m_col[i]
    tau_i   = tau_col[i]
    F_i     = (1 - c_phi) * tau_i

    # Continuation: at top age, 0 (terminal condition). Below top age,
    # either next-year value (regular) or freshly-computed same-sweep
    # value at age i+1 (stationary).
    W_next_i = if (i == n_ages) 0 else if (stationary) W[i + 1] else W_next[i + 1]

    # Marginal cost of realizing one more dollar today: tax paid now,
    # plus the discounted survivor value forgone, plus the discounted
    # death-state forgiveness value forgone.
    MC_i = tau_i + beta * (1 - m_i) * W_next_i + beta * m_i * F_i

    r_exog_B = planned_share * r_B_col[i]
    r_D_cap  = max(1 - r_exog_B, 0)

    if (is_baseline_pass) {
      # Target observed ordinary realization rate after removing the forced-
      # window baseline bucket.
      r_D_B_target = min(max(r_B_col[i] - r_exog_B, 0), r_D_cap)
      r_D_i = r_D_B_target
      # Recover kappa from the interior FOC b'(r_D) = MC, i.e.
      # kappa = MC + psi * r_D. At corner cells (r_D = 0) this gives
      # kappa = MC, meaning the cell sits exactly at the lower corner.
      kappa_i = MC_i + psi * r_D_i
    } else {
      kappa_i = kappa_col[i]
      # Interior solution to kappa - psi*r_D - MC = 0, clipped.
      r_D_unclipped = (kappa_i - MC_i) / psi
      r_D_i = min(max(r_D_unclipped, 0), r_D_cap)
    }

    # Quadratic benefit; net of tax cost; continuation on the remaining stock.
    benefit   = kappa_i * r_D_i - 0.5 * psi * r_D_i * r_D_i
    tax_cost  = tau_i * r_D_i
    remaining = max(1 - r_exog_B - r_D_i, 0)
    cont      = remaining * (beta * (1 - m_i) * W_next_i + beta * m_i * F_i)

    W[i]     = benefit - tax_cost + cont
    MC[i]    = MC_i
    r_D[i]   = r_D_i
    kappa[i] = kappa_i
  }

  list(W = W, MC = MC, r_D = r_D, kappa = kappa)
}



kg_dyn_solve_bellman_baseline = function(grid_packed, tau_B_mat,
                                          c_phi_B = 0,
                                          psi          = KG_DYN_DEFAULT_PSI,
                                          phi_I        = KG_DYN_PHI_I,
                                          planned_share = KG_DYN_SHARE_PLANNED,
                                          beta_by_year = NULL) {

  #----------------------------------------------------------------------------
  # Pass 1 backward induction. Recovers kappa(a, t), W_B, MC_B on the
  # extended age grid by forcing the cell's optimal r_D to equal the
  # observed ordinary realization bucket after the forced-window bucket is
  # removed from r_B.
  # Under current-law step-up the baseline regime has c_phi = 0, so the
  # death-state forgiveness value F = tau (full forgiveness).
  #
  # Parameters:
  #   - grid_packed  : list with m, r_B matrices (output of
  #                     kg_dyn_pack_baseline_grid)
  #   - tau_B_mat    : baseline tau matrix [age, year]
  #   - c_phi_B      : death-state burden share for baseline (0 under current-
  #                     law step-up; passed in for completeness)
  #   - psi, phi_I, planned_share : behavioral / bucket-share params
  #   - beta_by_year : numeric vector of per-year discount factors (length
  #                     n_years). Each beta_by_year[j] discounts between
  #                     year j and year j+1. If NULL, falls back to a
  #                     constant KG_DYN_BETA vector (kept for isolated
  #                     solver unit tests).
  #
  # Returns: list(W = W_mat, MC = MC_mat, kappa = kappa_mat, r_D = r_D_B_mat),
  # each indexed [age, year].
  #----------------------------------------------------------------------------

  m_mat   = grid_packed$m
  r_B_mat = grid_packed$r_B
  n_ages  = nrow(m_mat); n_years = ncol(m_mat)
  ages_chr  = rownames(m_mat); years_chr = colnames(m_mat)

  if (is.null(beta_by_year)) beta_by_year = rep(KG_DYN_BETA, n_years)
  stopifnot(length(beta_by_year) == n_years)

  W     = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  MC    = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  kappa = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_D   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))

  # Terminal year column: stationary backward solve in age, with the
  # continuation at age a pulling W[a+1] from the same just-computed sweep.
  # Use beta_by_year[n_years] as the steady-state discount factor (the
  # stationary solve treats year-n_years primitives as constant forward).
  t_max_idx = n_years
  res = kg_dyn_bellman_sweep_age(
    W_next    = NULL,
    m_col     = m_mat  [, t_max_idx],
    r_B_col   = r_B_mat[, t_max_idx],
    tau_col   = tau_B_mat[, t_max_idx],
    c_phi     = c_phi_B,
    psi       = psi, phi_I = phi_I, beta = beta_by_year[t_max_idx],
    planned_share = planned_share,
    kappa_col = NULL,
    stationary = TRUE
  )
  W    [, t_max_idx] = res$W
  MC   [, t_max_idx] = res$MC
  kappa[, t_max_idx] = res$kappa
  r_D  [, t_max_idx] = res$r_D

  # March backward in time. beta_by_year[j] discounts between year j and j+1.
  if (n_years >= 2) {
    for (j in (n_years - 1):1) {
      res = kg_dyn_bellman_sweep_age(
        W_next    = W[, j + 1],
        m_col     = m_mat  [, j],
        r_B_col   = r_B_mat[, j],
        tau_col   = tau_B_mat[, j],
        c_phi     = c_phi_B,
        psi       = psi, phi_I = phi_I, beta = beta_by_year[j],
        planned_share = planned_share,
        kappa_col = NULL
      )
      W    [, j] = res$W
      MC   [, j] = res$MC
      kappa[, j] = res$kappa
      r_D  [, j] = res$r_D
    }
  }

  list(W = W, MC = MC, kappa = kappa, r_D = r_D)
}



kg_dyn_solve_bellman_scenario = function(grid_packed, tau_S_mat,
                                          kappa_mat, c_phi_S_by_year,
                                          psi          = KG_DYN_DEFAULT_PSI,
                                          phi_I        = KG_DYN_PHI_I,
                                          planned_share = KG_DYN_SHARE_PLANNED,
                                          beta_by_year = NULL) {

  #----------------------------------------------------------------------------
  # Pass 2 backward induction. With kappa(a, t) recovered from Pass 1,
  # solve the clipped quadratic FOC r_D = clip((kappa - MC_S)/psi, 0,
  # 1 - r_exog_B) at each cell. c_phi can vary year by year (e.g., a
  # carryover regime phased in mid-horizon), so c_phi_S_by_year is a
  # numeric vector aligned with the year columns.
  #
  # Parameters:
  #   - grid_packed     : same as Pass 1
  #   - tau_S_mat       : scenario tau matrix [age, year]
  #   - kappa_mat       : kappa matrix from Pass 1
  #   - c_phi_S_by_year : numeric vector length n_years
  #   - beta_by_year    : per-year discount factors (length n_years). NULL
  #                        falls back to constant KG_DYN_BETA (unit-test
  #                        convenience only); production paths pass a real
  #                        vector built by kg_dyn_load_beta_series.
  #
  # Returns: list(W, MC, r_D), each [age, year]. r_D is the scenario
  # discretionary realization rate.
  #----------------------------------------------------------------------------

  m_mat   = grid_packed$m
  r_B_mat = grid_packed$r_B
  n_ages  = nrow(m_mat); n_years = ncol(m_mat)
  ages_chr  = rownames(m_mat); years_chr = colnames(m_mat)

  if (is.null(beta_by_year)) beta_by_year = rep(KG_DYN_BETA, n_years)
  stopifnot(length(beta_by_year) == n_years)

  W   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  MC  = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_D = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))

  # Terminal stationary solve at t_max using year-t_max primitives, including
  # the year-t_max discount factor.
  t_max_idx = n_years
  res = kg_dyn_bellman_sweep_age(
    W_next    = NULL,
    m_col     = m_mat  [, t_max_idx],
    r_B_col   = r_B_mat[, t_max_idx],
    tau_col   = tau_S_mat[, t_max_idx],
    c_phi     = c_phi_S_by_year[t_max_idx],
    psi       = psi, phi_I = phi_I, beta = beta_by_year[t_max_idx],
    planned_share = planned_share,
    kappa_col = kappa_mat[, t_max_idx],
    stationary = TRUE
  )
  W  [, t_max_idx] = res$W
  MC [, t_max_idx] = res$MC
  r_D[, t_max_idx] = res$r_D

  if (n_years >= 2) {
    for (j in (n_years - 1):1) {
      res = kg_dyn_bellman_sweep_age(
        W_next    = W[, j + 1],
        m_col     = m_mat  [, j],
        r_B_col   = r_B_mat[, j],
        tau_col   = tau_S_mat[, j],
        c_phi     = c_phi_S_by_year[j],
        psi       = psi, phi_I = phi_I, beta = beta_by_year[j],
        planned_share = planned_share,
        kappa_col = kappa_mat[, j]
      )
      W  [, j] = res$W
      MC [, j] = res$MC
      r_D[, j] = res$r_D
    }
  }

  list(W = W, MC = MC, r_D = r_D)
}



#-------------------------------------------------------------------------------
# Forced-window Bellman-state helpers
#-------------------------------------------------------------------------------

kg_dyn_validate_realization_buckets = function(fixed_share   = KG_DYN_PHI_I,
                                               planned_share = KG_DYN_SHARE_PLANNED,
                                               timing_window = KG_DYN_TIMING_WINDOW,
                                               ref_wedge     = KG_DYN_TIMING_REF_WEDGE) {

  if (!is.finite(fixed_share) || !is.finite(planned_share)) {
    stop('kg_dynamics: realization bucket shares must be finite.')
  }
  if (abs(fixed_share) > 1e-12) {
    stop('kg_dynamics: fixed_share must be zero in the forced-window Bellman.')
  }
  if (planned_share < 0 || planned_share > 1) {
    stop(sprintf(
      paste0('kg_dynamics: invalid forced-window share: %.4f. Expected ',
             'planned_share in [0, 1].'),
      planned_share))
  }
  if (length(timing_window) != 1 || is.na(timing_window) ||
      timing_window != 1L) {
    stop('kg_dynamics: forced-window v1 requires KG_DYN_TIMING_WINDOW = 1.')
  }
  if (length(ref_wedge) != 1 || !is.finite(ref_wedge) || ref_wedge <= 0) {
    stop('kg_dynamics: KG_DYN_TIMING_REF_WEDGE must be a positive finite number.')
  }

  invisible(TRUE)
}



kg_dyn_solve_forced_window_state = function(baseline_cells, tau_S_mat, years,
                                            tau_B_mat = NULL,
                                            planned_share = KG_DYN_SHARE_PLANNED,
                                            timing_window = KG_DYN_TIMING_WINDOW,
                                            ref_wedge = KG_DYN_TIMING_REF_WEDGE,
                                            q_B = KG_DYN_FORCED_Q_B,
                                            beta_by_year = NULL,
                                            ages_bathtub = KG_DYN_AGE_MIN:
                                                           KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Solves the forced-window realization state for the one-year timing window.
  # The observed baseline forced realizations are lambda * R_B. We use the
  # steady-state shortcut to skip the entrant-flow inversion:
  #
  #   E_B(t) := lambda * R_B(t)
  #
  # This is exact when R_forced_B is stationary year-over-year and is a
  # well-behaved approximation otherwise; the alternative recurrence
  # E_B(t) = (lambda*R_B(t) - (1-q_B)*E_B(t-1))/q_B amplifies sparse-cell noise
  # at low q_B and forces q_B up to the point that the short-run anticipation
  # target becomes unreachable. The downstream applier consumes r_S/r_B ratios
  # and reform-vs-baseline deltas, so the modest year-over-year deviation
  # introduced by this shortcut does not propagate into per-record adjustments.
  #
  # F1 entrants choose q, the share realizing immediately rather than waiting
  # one year to the F0 deadline, which must realize. This is a Bellman control:
  #
  #   F1 = max_q q*V_now + (1-q)*V_wait
  #            + forced_intercept*q - 0.5*ref_wedge*(q - q_B)^2
  #
  # where V_now = -tau(t), V_wait = beta(t)*F0(t+1), and F0(t+1) =
  # -tau(t+1). The baseline intercept is inverted so that the baseline FOC
  # reproduces q_B. Scenario q is the bounded FOC solution using the fixed
  # baseline intercept:
  #
  #   q_S(t) = argmax_q F1_S(q)
  #
  #   R_forced_S(t) = q_S(t) * E_B(t) + [1 - q_S(t-1)] * E_B(t-1)
  #----------------------------------------------------------------------------

  kg_dyn_validate_realization_buckets(planned_share = planned_share,
                                      timing_window = timing_window,
                                      ref_wedge     = ref_wedge)
  if (length(q_B) != 1 || !is.finite(q_B) || q_B <= 0 || q_B >= 1) {
    stop('kg_dynamics: KG_DYN_FORCED_Q_B must be strictly between 0 and 1.')
  }
  if (is.null(tau_B_mat)) {
    stop('kg_dynamics: tau_B_mat is required for forced-window state solves.')
  }

  ages_chr  = as.character(ages_bathtub)
  years_chr = as.character(years)
  n_ages    = length(ages_bathtub)
  n_years   = length(years)
  if (is.null(beta_by_year)) beta_by_year = rep(1, n_years)
  stopifnot(length(beta_by_year) == n_years)

  R_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = baseline_cells[[t_chr]]
    R_B[, t_chr] = bt$R_B[match(ages_bathtub, bt$age)]
  }

  R_forced_B = planned_share * R_B
  E_forced_B = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  q_forced_B = matrix(q_B, n_ages, n_years,
                      dimnames = list(ages_chr, years_chr))
  q_forced_S = matrix(q_B, n_ages, n_years,
                      dimnames = list(ages_chr, years_chr))
  forced_intercept = matrix(0, n_ages, n_years,
                            dimnames = list(ages_chr, years_chr))
  timing_advantage = matrix(0, n_ages, n_years,
                            dimnames = list(ages_chr, years_chr))
  F0_forced_B = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  F0_forced_S = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  F1_forced_B = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  F1_forced_S = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))

  # Steady-state shortcut: each year's entrant cohort equals that year's
  # observed forced realization mass. See function header for the rationale.
  E_forced_B = R_forced_B

  tau_B_bt = tau_B_mat[ages_chr, years_chr, drop = FALSE]
  tau_S_bt = tau_S_mat[ages_chr, years_chr, drop = FALSE]
  F0_forced_B = -tau_B_bt
  F0_forced_S = -tau_S_bt
  forced_objective = function(q, now_value, wait_value, intercept) {
    q * now_value + (1 - q) * wait_value +
      intercept * q - 0.5 * ref_wedge * (q - q_B)^2
  }
  if (n_years >= 2) {
    for (j in 1:(n_years - 1)) {
      V_now_B  = -tau_B_bt[, j]
      V_wait_B = beta_by_year[j] * F0_forced_B[, j + 1]
      V_now_S  = -tau_S_bt[, j]
      V_wait_S = beta_by_year[j] * F0_forced_S[, j + 1]

      advantage_B = V_now_B - V_wait_B
      advantage_S = V_now_S - V_wait_S
      forced_intercept[, j] = -advantage_B
      timing_advantage[, j] = advantage_S + forced_intercept[, j]
      q_forced_S[, j] = pmin(pmax(q_B + timing_advantage[, j] / ref_wedge, 0), 1)

      F1_forced_B[, j] = forced_objective(q_B, V_now_B, V_wait_B,
                                          forced_intercept[, j])
      F1_forced_S[, j] = forced_objective(q_forced_S[, j], V_now_S,
                                          V_wait_S, forced_intercept[, j])
    }
  }
  F1_forced_B[, n_years] = F0_forced_B[, n_years]
  F1_forced_S[, n_years] = F0_forced_S[, n_years]

  # Apply the realization formula to both q paths using shared E_forced_B and
  # year-0 carry-in. The "_model" baseline output is the model's view of
  # baseline forced realizations under q = q_B, distinct from the observed
  # R_forced_B = planned_share * R_B. The downstream rate_factor uses the
  # model baseline as the denominator so baseline_check yields exactly 1.
  apply_q_path = function(q_path) {
    R = q_path * E_forced_B
    if (n_years >= 1) {
      R[, 1] = R[, 1] + (1 - q_B) * E_forced_B[, 1]
    }
    if (n_years >= 2) {
      for (j in 2:n_years) {
        R[, j] = R[, j] + (1 - q_path[, j - 1]) * E_forced_B[, j - 1]
      }
    }
    R
  }
  R_forced_B_model = apply_q_path(q_forced_B)
  R_forced_S       = apply_q_path(q_forced_S)

  out = list(E_forced_B = E_forced_B,
             q_forced_B = q_forced_B,
             q_forced_S = q_forced_S,
             R_forced_B = R_forced_B,
             R_forced_B_model = R_forced_B_model,
             R_forced_S = R_forced_S,
             forced_timing_shift = R_forced_S - R_forced_B_model,
             forced_intercept = forced_intercept,
             forced_timing_advantage = timing_advantage,
             F0_forced_B = F0_forced_B,
             F0_forced_S = F0_forced_S,
             F1_forced_B = F1_forced_B,
             F1_forced_S = F1_forced_S)

  # Compatibility aliases for older diagnostics and calibration scripts. These
  # names are aliases only; the forced-state outputs above are authoritative.
  out$R_planned_B = out$R_forced_B
  out$R_planned_S = out$R_forced_S
  out$planned_timing_shift = out$forced_timing_shift
  out
}



kg_dyn_build_planned_timing = function(...) {
  kg_dyn_solve_forced_window_state(...)
}



kg_dyn_build_scenario_rate = function(baseline_t, r_ordinary_S,
                                      R_forced_B_col = NULL,
                                      R_forced_B_model_col = NULL,
                                      R_forced_S_col = NULL,
                                      R_planned_B_col = NULL,
                                      R_planned_S_col = NULL,
                                      fixed_share = KG_DYN_PHI_I) {

  if (abs(fixed_share) > 1e-12) {
    stop('kg_dynamics: fixed_share must be zero in kg_dyn_build_scenario_rate.')
  }
  if (is.null(R_forced_B_col)) R_forced_B_col = R_planned_B_col
  if (is.null(R_forced_S_col)) R_forced_S_col = R_planned_S_col
  if (is.null(R_forced_B_col) || is.null(R_forced_S_col)) {
    stop('kg_dynamics: forced-window realization columns are required.')
  }
  # Model baseline defaults to observed when an old caller doesn't supply it.
  # This preserves prior behavior for any external test code that doesn't
  # know about the model-baseline reference; production callers supply the
  # model column explicitly.
  if (is.null(R_forced_B_model_col)) R_forced_B_model_col = R_forced_B_col

  G_B = baseline_t$G_B
  r_B = baseline_t$r_B

  r_fixed_B        = rep(0, length(r_B))
  r_forced_B       = ifelse(G_B > 0, R_forced_B_col       / G_B, 0)
  r_forced_B_model = ifelse(G_B > 0, R_forced_B_model_col / G_B, 0)
  r_forced_S       = ifelse(G_B > 0, R_forced_S_col       / G_B, 0)
  r_ordinary_B     = pmax(r_B - r_forced_B, 0)

  r_S_unclipped = r_ordinary_S + r_forced_S
  r_S           = pmin(pmax(r_S_unclipped, 0), 1)

  # Model baseline total rate. r_ordinary_B reproduces observed by Bellman
  # construction (kappa is inverted to match), so the only swap is the
  # forced bucket's observed-vs-model gap.
  r_B_model = pmin(pmax(r_ordinary_B + r_forced_B_model, 0), 1)

  list(r_S              = r_S,
       r_S_unclipped    = r_S_unclipped,
       timing_clipped   = abs(r_S - r_S_unclipped) > 1e-12,
       r_B_model        = r_B_model,
       r_fixed_B        = r_fixed_B,
       r_planned_B      = r_forced_B,
       r_planned_S      = r_forced_S,
       r_forced_B       = r_forced_B,
       r_forced_B_model = r_forced_B_model,
       r_forced_S       = r_forced_S,
       r_ordinary_B     = r_ordinary_B,
       r_ordinary_S     = r_ordinary_S)
}



#-------------------------------------------------------------------------------
# Bathtub recurrence step
#-------------------------------------------------------------------------------

kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  r_S_vec, delta_route,
                                  phi_I = KG_DYN_PHI_I) {

  #----------------------------------------------------------------------------
  # One-step bathtub recurrence for delta_G. Operates on cell vectors indexed
  # by age (on the bathtub grid [18, 80]).
  #
  # The scenario realization rate r_S is supplied directly by the caller.
  # Upstream, it combines the Bellman ordinary bucket and forced-window state.
  #
  # Topcode note: the age=80 cell pools every taxpayer age 80+ into one
  # bucket and uses a single weight-averaged m_80. This is refreshed from
  # each year's Tax-Data, so it tracks the true 80+ population mix over
  # time. The remaining approximation is within-pool heterogeneity --
  # someone who's been in the topcode 15 years (true age ~95) has much
  # higher individual mortality than someone newly aged in. Pooled m_80
  # smooths this out. Small effect in practice because most pool weight is
  # on early-80s, but worth flagging if reforms shift the topcode age mix.
  #
  # Parameters:
  #   - delta_prev (num[a]) : start-of-year delta_G (zero on first year)
  #   - baseline_t (tbl)    : output of kg_dyn_aggregate_cells for year t
  #   - A          (mat)    : aging matrix
  #   - omega      (mat)    : heir matrix
  #   - r_S_vec    (num[a]) : scenario realization rate from Bellman
  #   - delta_route (num)   : routing share for carryover stock transfer
  #   - phi_I      (num)    : compatibility fixed share; must be zero
  #
  # Returns: list(delta_next, r_S, lambda_I, r_V_B, r_V_S, delta_surv,
  # delta_inh).
  #----------------------------------------------------------------------------

  G_B       = baseline_t$G_B
  r_B       = baseline_t$r_B
  R_B       = baseline_t$R_B
  m         = baseline_t$m
  mG_record = baseline_t$mG_record
  mR_record = baseline_t$mR_record

  # ----------------------------------------------------------------------------
  # Why we use an effective cell mortality m_eff = sum(w*m*X) / sum(w*X).
  # ----------------------------------------------------------------------------
  #
  # The death channel needs the cell's *decedent stock contribution* --
  # the sum across records of (death prob) * (per-record gain stock):
  #
  #     D = sum_i w_i * m_i * (G_unit_i + dG_i)
  #
  # The naive cell-level form m * (G_B + dG) replaces this with the
  # cell-mean m times the cell-total stock. That equals the per-record
  # sum only if Cov(m, G_unit | cell) = 0 -- and within an age cell that
  # covariance is large and negative (wealth-mortality gradient: wealthier
  # holders carry more G AND die less). At the G-weighted aggregate, the
  # cell-mean form overstates D by ~2.7x in our data.
  #
  # To avoid materializing per-record state for the recurrence, we adopt
  # an assumption about how cell-level dG is split across records, then
  # compute the per-record sum analytically. Two rules are supported via
  # KG_DYN_DG_ALLOCATION (see constants block):
  #
  #   "G": dG_i proportional to G_unit_i. Then
  #          D = mG_record * (G_B + dG) / G_B = m_eff_G * (G_B + dG)
  #        with m_eff_G = mG_record / G_B.
  #
  #   "R": dG_i proportional to pmax(kg_lt_i, 0). m_eff_R = mR_record / R_B
  #        for the realization-weighted lock-in story. Falls back to "G"
  #        when R_B = 0.
  #
  # In both cases m_eff IS the per-record sum -- not an approximation --
  # under the corresponding allocation rule.
  m_eff_G = if_else(G_B > 0, mG_record / G_B, m)
  m_eff_R = if_else(R_B > 0, mR_record / R_B, m_eff_G)

  m_eff = switch(KG_DYN_DG_ALLOCATION,
                 G = m_eff_G,
                 R = m_eff_R,
                 stop("Unknown KG_DYN_DG_ALLOCATION rule: ", KG_DYN_DG_ALLOCATION))
  m_eff = pmin(pmax(m_eff, 0), 1)

  # Channel-decomposition diagnostics. Keep lambda_I for the existing state
  # contract; it is zero in the forced-window Bellman.
  lambda_I = phi_I * r_B
  r_V_B    = pmax(r_B     - lambda_I, 0)
  r_V_S    = pmax(r_S_vec - lambda_I, 0)
  r_S      = pmin(pmax(r_S_vec, 0), 1)

  # Survivor flow (spec §3.2)
  inner      = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m_eff) * inner
  delta_surv = as.numeric(crossprod(A, contrib_a))

  # Inheritance flow (spec §3.3.1)
  if (delta_route > 0) {
    decedent_stock = m_eff * (G_B + delta_prev)
    delta_inh      = delta_route * as.numeric(crossprod(omega, decedent_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  list(delta_next = delta_surv + delta_inh,
       r_S        = r_S,
       lambda_I   = lambda_I,
       r_V_B      = r_V_B,
       r_V_S      = r_V_S,
       delta_surv = delta_surv,
       delta_inh  = delta_inh)
}



#-------------------------------------------------------------------------------
# Regime resolution (named lookup)
#-------------------------------------------------------------------------------

kg_dyn_resolve_regime = function(regime_code, theta) {

  #----------------------------------------------------------------------------
  # Maps the integer regime code from pref.kg_death_regime to the canonical
  # (name, c_phi, delta_vanish, delta_route, delta_realize) tuple. Spec §3.3.
  #
  #   0 = step_up           : c_phi = 0,     vanish = 1
  #   1 = carryover         : c_phi = theta, route = 1
  #   2 = deemed_realization: c_phi = 1,     realize = 1
  #----------------------------------------------------------------------------

  regime_name = KG_DYN_REGIME_BY_CODE[as.character(regime_code)]
  if (is.na(regime_name)) {
    stop(paste0('Unknown kg_death_regime: ', regime_code,
                ' (expected 0=step_up, 1=carryover, 2=deemed_realization)'))
  }

  base  = KG_DYN_REGIMES[[regime_name]]
  c_phi = if (regime_name == 'carryover') theta else base$c_phi_default

  list(name          = regime_name,
       c_phi         = c_phi,
       delta_vanish  = base$delta_vanish,
       delta_route   = base$delta_route,
       delta_realize = base$delta_realize)
}



#-------------------------------------------------------------------------------
# Per-record applier (pure allocator)
#
# Reads the precomputed cell_table from the bathtub state file and translates
# cell-level quantities (rate_factor, extra_R, deemed_factor) to per-record
# kg_lt adjustments.
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, delta_realize,
                                    decedent_random) {

  #----------------------------------------------------------------------------
  # Distributes cell-level reform-vs-baseline realization adjustments to
  # individual tax-unit kg_lt (spec §7.3) and applies per-record fractional
  # deemed-realization burden. Three channels:
  #
  #   rate-channel   : multiply each record's positive kg_lt by rate_factor
  #                    = r_S/r_B (clamped to 1 when r_B = 0)
  #   lock-in extra  : pro-rata share of cell-level extra_R = r_S * dG;
  #                    allocated by positive-kg_lt share if R_B > 0, else by
  #                    G_unit share, else skip
  #   deemed-channel : per-record m_household * G_unit, scaled by cell-level
  #                    deemed_factor = (G_B + dG)/G_B to incorporate
  #                    accumulated stock
  #
  # decedent_flag is a stochastic side product for distribution analysis:
  # u < m_household marks a record as decedent. Uses the precomputed uniform
  # draws from globals$random_numbers, same draw across scenarios.
  #
  # Parameters:
  #   - tax_units (df)        : with G_unit, m_household, age_cohort attached
  #   - cell_table (tbl)      : output of bathtub pre-pass; has age, G_B, R_B,
  #                             rate_factor, extra_R, deemed_factor
  #   - delta_realize (num)   : routing share for forced realization at death
  #   - decedent_random (num) : uniform[0,1] draws, length = nrow(tax_units)
  #
  # Returns: tax_units with modified kg_lt and added decedent_flag.
  #----------------------------------------------------------------------------

  tax_units %>%
    left_join(cell_table, by = c('age_cohort' = 'age')) %>%
    mutate(
      allocation = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      kg_lt_rate    = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt),
      kg_lt_carry   = extra_R * allocation,
      kg_lt_deemed  = delta_realize * m_household * G_unit * deemed_factor,
      kg_lt         = kg_lt_rate + kg_lt_carry + kg_lt_deemed,
      decedent_flag = as.integer(decedent_random < m_household)
    ) %>%
    select(-rate_factor, -extra_R, -deemed_factor, -allocation,
           -kg_lt_rate, -kg_lt_carry, -kg_lt_deemed,
           -R_B, -G_B,
           # Bellman diagnostic columns from the cell_table left_join;
           # not consumed downstream, drop to avoid polluting tax_units schema.
           -any_of(c('r_B', 'r_B_model', 'r_S', 'r_S_unclipped', 'timing_clipped',
                     'lambda_I', 'r_V_B', 'r_V_S',
                     'r_fixed_B', 'r_planned_B', 'r_planned_S',
                     'r_forced_B', 'r_forced_B_model', 'r_forced_S',
                     'r_ordinary_B', 'r_ordinary_S',
                     'R_planned_B', 'R_planned_S', 'planned_timing_shift',
                     'R_forced_B', 'R_forced_B_model', 'R_forced_S',
                     'forced_timing_shift',
                     'E_forced_B', 'q_forced_B', 'q_forced_S',
                     'forced_intercept', 'forced_timing_advantage',
                     'F0_forced_B', 'F0_forced_S', 'F1_forced_B', 'F1_forced_S',
                     'm', 'mG_record', 'mR_record',
                     'dG', 'tau_B', 'tau_S', 'W_B', 'W_S', 'MC_B', 'MC_S',
                     'kappa', 'r_D_B', 'r_D_S')))
}



kg_dyn_state_path = function(scenario_info, year) {
  file.path(scenario_info$output_path,
            'conventional', 'supplemental',
            'kg_dynamics_state',
            paste0(year, '.rds'))
}



#-------------------------------------------------------------------------------
# Bathtub pre-pass orchestration
#-------------------------------------------------------------------------------

kg_dyn_load_bathtub_inputs = function(scenario_info, baseline_root,
                                       sample_ids, pct_sample,
                                       ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Single Tax-Data pass that builds all three baseline-side inputs the
  # bathtub needs:
  #
  #   - baseline_cells : named list of cell tibbles (G_B, R_B, r_B, m,
  #                      mG_record, mR_record) per year (ages 18-80)
  #   - baseline_tau   : named list of length-|ages| named tau vectors per
  #                      year, R-weighted on baseline static detail's
  #                      mtr_kg_lt
  #   - reform_tau     : same shape, R-weighted on reform static detail
  #
  # Bypasses static detail for the cell aggregates because the wealth
  # columns (value.*/basis.*) and q_death* live only in the source
  # Tax-Data csvs. mtr_kg_lt comes from baseline + reform static detail
  # (requires runscript registers mtr_vars = "kg_lt").
  #----------------------------------------------------------------------------

  tax_data_root = scenario_info$interface_paths$`Tax-Data`
  years         = scenario_info$years

  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'kg_lt', 'q_death1', 'q_death2',
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  baseline_cells = list()
  baseline_tau   = list()
  reform_tau     = list()

  for (t in years) {

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample) %>%
      kg_dyn_attach_record_attrs()

    baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td, ages)

    bl_mtr = file.path(baseline_root, 'baseline', 'static', 'detail',
                        paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    rf_mtr = file.path(scenario_info$output_path, 'static', 'detail',
                        paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    baseline_tau[[as.character(t)]] = td %>%
      left_join(bl_mtr, by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)

    reform_tau[[as.character(t)]] = td %>%
      left_join(rf_mtr, by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)
  }

  list(baseline_cells = baseline_cells,
       baseline_tau   = baseline_tau,
       reform_tau     = reform_tau)
}



kg_dyn_build_cell_table = function(baseline_t, year_idx,
                                    r_S_vec, lambda_I_vec, r_V_B_vec, r_V_S_vec,
                                    delta_prev,
                                    tau_B_col, tau_S_col,
                                    W_B_col, W_S_col, MC_B_col, MC_S_col,
                                    kappa_col, r_D_B_col, r_D_S_col,
                                    planned_diag = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Assembles the per-cell quantities the per-record applier needs (rate
  # factor, lock-in extra realization, deemed scaling) plus diagnostic
  # quantities used by kg_dyn_build_summary (tau_B, tau_S, W, MC, kappa,
  # channel decomposition). Persisted into the state file by
  # kg_dyn_run_bathtub_pass.
  #
  # The Bellman matrices are on the extended grid; we slice to the bathtub
  # grid [18, 80] before persisting since the per-record applier only acts
  # on those ages.
  #
  # Per-cell quantities:
  #   rate_factor   = r_S / r_B           (clamped to 1 when r_B = 0)
  #   extra_R       = r_S * dG            (lock-in stock realized at rate r_S;
  #                                        applies under all regimes)
  #   deemed_factor = (G_B + dG) / G_B    (clamped to >= 0; scales per-record
  #                                        m * G_unit so deemed revenue
  #                                        includes accumulated stock)
  #----------------------------------------------------------------------------

  ages_chr = as.character(ages_bathtub)
  diag_or = function(name, default) {
    v = planned_diag[[name]]
    if (!is.null(v)) return(v)
    if (length(default) == length(ages_chr)) {
      setNames(as.vector(default), ages_chr)
    } else {
      setNames(rep(default, length(ages_chr)), ages_chr)
    }
  }

  baseline_t %>%
    mutate(age           = as.integer(age),
           r_S           = as.numeric(r_S_vec     [as.character(age)]),
           r_S_unclipped = as.numeric(diag_or('r_S_unclipped', r_S)[as.character(age)]),
           timing_clipped = as.logical(diag_or('timing_clipped', FALSE)[as.character(age)]),
           lambda_I      = as.numeric(lambda_I_vec[as.character(age)]),
           r_V_B         = as.numeric(r_V_B_vec   [as.character(age)]),
           r_V_S         = as.numeric(r_V_S_vec   [as.character(age)]),
           r_D_B         = as.numeric(r_D_B_col   [as.character(age)]),
           r_D_S         = as.numeric(r_D_S_col   [as.character(age)]),
           r_fixed_B     = as.numeric(diag_or('r_fixed_B', lambda_I)[as.character(age)]),
           r_planned_B   = as.numeric(diag_or('r_planned_B', 0)[as.character(age)]),
           r_planned_S   = as.numeric(diag_or('r_planned_S', 0)[as.character(age)]),
           r_forced_B    = as.numeric(diag_or('r_forced_B', r_planned_B)[as.character(age)]),
           r_forced_S    = as.numeric(diag_or('r_forced_S', r_planned_S)[as.character(age)]),
           r_ordinary_B  = as.numeric(diag_or('r_ordinary_B', r_D_B)[as.character(age)]),
           r_ordinary_S  = as.numeric(diag_or('r_ordinary_S', r_D_S)[as.character(age)]),
           R_planned_B   = as.numeric(diag_or('R_planned_B', 0)[as.character(age)]),
           R_planned_S   = as.numeric(diag_or('R_planned_S', 0)[as.character(age)]),
           R_forced_B    = as.numeric(diag_or('R_forced_B', R_planned_B)[as.character(age)]),
           R_forced_S    = as.numeric(diag_or('R_forced_S', R_planned_S)[as.character(age)]),
           planned_timing_shift =
             as.numeric(diag_or('planned_timing_shift', 0)[as.character(age)]),
           forced_timing_shift =
             as.numeric(diag_or('forced_timing_shift',
                                 planned_timing_shift)[as.character(age)]),
           E_forced_B    = as.numeric(diag_or('E_forced_B', 0)[as.character(age)]),
           q_forced_B    = as.numeric(diag_or('q_forced_B', KG_DYN_FORCED_Q_B)[as.character(age)]),
           q_forced_S    = as.numeric(diag_or('q_forced_S', KG_DYN_FORCED_Q_B)[as.character(age)]),
           forced_intercept =
             as.numeric(diag_or('forced_intercept', 0)[as.character(age)]),
           forced_timing_advantage =
             as.numeric(diag_or('forced_timing_advantage', 0)[as.character(age)]),
           F0_forced_B  = as.numeric(diag_or('F0_forced_B', 0)[as.character(age)]),
           F0_forced_S  = as.numeric(diag_or('F0_forced_S', 0)[as.character(age)]),
           F1_forced_B  = as.numeric(diag_or('F1_forced_B', F0_forced_B)[as.character(age)]),
           F1_forced_S  = as.numeric(diag_or('F1_forced_S', F0_forced_S)[as.character(age)]),
           r_forced_B_model =
             as.numeric(diag_or('r_forced_B_model', r_forced_B)[as.character(age)]),
           R_forced_B_model =
             as.numeric(diag_or('R_forced_B_model', R_forced_B)[as.character(age)]),
           r_B_model    = as.numeric(diag_or('r_B_model', r_B)[as.character(age)]),
           dG            = as.numeric(delta_prev  [as.character(age)]),
           tau_B         = as.numeric(tau_B_col   [as.character(age)]),
           tau_S         = as.numeric(tau_S_col   [as.character(age)]),
           W_B           = as.numeric(W_B_col     [as.character(age)]),
           W_S           = as.numeric(W_S_col     [as.character(age)]),
           MC_B          = as.numeric(MC_B_col    [as.character(age)]),
           MC_S          = as.numeric(MC_S_col    [as.character(age)]),
           kappa         = as.numeric(kappa_col   [as.character(age)]),
           # rate_factor uses the model baseline as the denominator so that
           # baseline_check (q_S = q_B) gives r_S = r_B_model -> rate_factor = 1
           # exactly. The downstream applier multiplies per-record observed
           # kg_lt by this factor, so observed mass passes through unchanged
           # under no-policy-change runs.
           rate_factor   = if_else(r_B_model > 0, r_S / r_B_model, 1),
           extra_R       = r_S * dG,
           deemed_factor = if_else(G_B > 0,
                                   pmax(0, (G_B + dG) / G_B),
                                   1)) %>%
    select(age, G_B, R_B, r_B, r_B_model, r_S, r_S_unclipped, timing_clipped,
           lambda_I, r_V_B, r_V_S,
           r_fixed_B, r_planned_B, r_planned_S,
           r_forced_B, r_forced_B_model, r_forced_S,
           r_ordinary_B, r_ordinary_S,
           R_planned_B, R_planned_S,
           R_forced_B, R_forced_B_model, R_forced_S,
           planned_timing_shift, forced_timing_shift,
           E_forced_B, q_forced_B, q_forced_S,
           forced_intercept, forced_timing_advantage,
           F0_forced_B, F0_forced_S, F1_forced_B, F1_forced_S,
           m, mG_record, mR_record, dG,
           tau_B, tau_S, W_B, W_S, MC_B, MC_S, kappa, r_D_B, r_D_S,
           rate_factor, extra_R, deemed_factor)
}



kg_dyn_run_bathtub_pass = function(scenario_info, tax_law, baseline_cells,
                                    baseline_tau, reform_tau,
                                    psi   = KG_DYN_DEFAULT_PSI,
                                    phi_I = KG_DYN_PHI_I,
                                    planned_share = KG_DYN_SHARE_PLANNED,
                                    timing_window = KG_DYN_TIMING_WINDOW,
                                    ref_wedge     = KG_DYN_TIMING_REF_WEDGE,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  #----------------------------------------------------------------------------
  # Sequentially runs the bathtub recurrence across scenario_info$years and
  # persists one state file per year. The state file is the contract consumed
  # by the kg_dynamics behavior module's per-record applier; the module
  # computes no cell-level math itself.
  #
  # Flow:
  #   1. Build extended-grid baseline cells (bathtub ages from baseline_cells,
  #      mortality tail 81-119 from PerLifeTables).
  #   2. Pack tau matrices (baseline + reform) onto the extended grid.
  #   3. Solve Pass 1 (baseline Bellman) once; recover kappa, W_B, MC_B,
  #      r_D_B. Under current-law step-up, c_phi_B = 0.
  #   4. Resolve scenario regimes per year (may be year-varying); solve
  #      Pass 2 (scenario Bellman) once using kappa from Pass 1.
  #   5. Solve the forced-window state using baseline entrant inference and
  #      scenario q choices.
  #   6. Loop years: combine ordinary and forced-window buckets into r_S_vec,
  #      run kg_dyn_step_recurrence for dG evolution, build cell_table, persist.
  #
  # State file at kg_dynamics_state/{t}.rds:
  #   list(
  #     regime     = list(name, c_phi, delta_vanish, delta_route, delta_realize),
  #     cell_table = tibble(age, G_B, R_B, r_B, r_S, bucket diagnostics,
  #                          lambda_I, r_V_B, r_V_S, m, mG_record, mR_record, dG,
  #                          tau_B, tau_S, W_B, W_S, MC_B, MC_S, kappa,
  #                          r_D_B, r_D_S, rate_factor, extra_R, deemed_factor)
  #   )
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  if (!is.finite(psi)) {
    stop('kg_dynamics: KG_DYN_DEFAULT_PSI is not set. Run ',
         'other/kg_model_tests/calibrate.R against a full-sample baseline ',
         'and paste the calibrated psi and planned_share values into the ',
         'constants block at the top of src/sim/kg_dynamics.R.')
  }
  kg_dyn_validate_realization_buckets(fixed_share = phi_I,
                                      planned_share = planned_share,
                                      timing_window = timing_window,
                                      ref_wedge     = ref_wedge)

  years     = scenario_info$years
  state_dir = file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_state')
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  # Step 0: build per-year real-rate discount factors from Macro-Projections
  macro_root = scenario_info$interface_paths$`Macro-Projections`
  if (is.null(macro_root)) {
    stop('kg_dynamics: scenario_info$interface_paths$`Macro-Projections` is ',
         'NULL. The bathtub pre-pass needs the Macro-Projections vintage to ',
         'derive the real-rate discount factor for the Bellman.')
  }
  beta_by_year = kg_dyn_load_beta_series(macro_root, years)

  # Step 1: extended grid (mortality tail 81-119)
  life_ext = kg_dyn_load_life_table_extension(years = years)
  grid_ext = kg_dyn_build_extended_grid(baseline_cells, life_ext, years,
                                        ages_bellman = ages_bellman)
  grid_packed = kg_dyn_pack_baseline_grid(grid_ext, years,
                                          ages_bellman = ages_bellman)

  # Step 2: tau matrices
  tau_B_mat = kg_dyn_pack_tau(baseline_tau, years, ages_bellman = ages_bellman)
  tau_S_mat = kg_dyn_pack_tau(reform_tau,   years, ages_bellman = ages_bellman)

  # Step 3: baseline Bellman pass (c_phi_B = 0 under current-law step-up)
  pass1 = kg_dyn_solve_bellman_baseline(grid_packed, tau_B_mat,
                                         c_phi_B = 0,
                                         psi = psi, phi_I = phi_I,
                                         planned_share = planned_share,
                                         beta_by_year = beta_by_year)

  # Step 4: resolve year-by-year scenario regimes and run scenario Bellman
  regime_list = vector('list', length(years))
  c_phi_S     = numeric(length(years))
  for (j in seq_along(years)) {
    tlt = tax_law %>% filter(year == years[j]) %>% slice(1)
    regime_list[[j]] = kg_dyn_resolve_regime(
      regime_code = as.numeric(tlt$pref.kg_death_regime),
      theta       = as.numeric(tlt$pref.kg_bequest_motive)
    )
    c_phi_S[j] = regime_list[[j]]$c_phi
  }

  pass2 = kg_dyn_solve_bellman_scenario(grid_packed, tau_S_mat,
                                         kappa_mat = pass1$kappa,
                                         c_phi_S_by_year = c_phi_S,
                                         psi = psi, phi_I = phi_I,
                                         planned_share = planned_share,
                                         beta_by_year = beta_by_year)

  forced_state = kg_dyn_solve_forced_window_state(
    baseline_cells = baseline_cells,
    tau_S_mat      = tau_S_mat,
    years          = years,
    tau_B_mat      = tau_B_mat,
    planned_share  = planned_share,
    timing_window  = timing_window,
    ref_wedge      = ref_wedge,
    beta_by_year   = beta_by_year,
    ages_bathtub   = ages_bathtub
  )

  # Save life table for later diagnostic inspection
  saveRDS(life_ext, file.path(state_dir, 'life_table_extension.rds'))

  # Step 5: year-by-year bathtub recurrence
  A     = kg_dyn_build_aging_matrix(ages_bathtub)
  omega = kg_dyn_build_heir_matrix(ages_bathtub)

  delta = setNames(rep(0, length(ages_bathtub)), as.character(ages_bathtub))
  bathtub_ages_chr = as.character(ages_bathtub)

  for (j in seq_along(years)) {
    t  = years[j]
    bt = baseline_cells[[as.character(t)]]
    regime = regime_list[[j]]

    # Slice Bellman outputs from extended grid to bathtub grid for this year
    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t           = bt,
      r_ordinary_S         = r_D_S_bt,
      R_forced_B_col       = forced_state$R_forced_B[, j],
      R_forced_B_model_col = forced_state$R_forced_B_model[, j],
      R_forced_S_col       = forced_state$R_forced_S[, j],
      fixed_share          = phi_I
    )
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev  = delta,
      baseline_t  = bt,
      A           = A,
      omega       = omega,
      r_S_vec     = r_S_vec,
      delta_route = regime$delta_route,
      phi_I       = phi_I
    )

    r_S_named      = setNames(step$r_S,      bathtub_ages_chr)
    lambda_I_named = setNames(step$lambda_I, bathtub_ages_chr)
    r_V_B_named    = setNames(step$r_V_B,    bathtub_ages_chr)
    r_V_S_named    = setNames(step$r_V_S,    bathtub_ages_chr)

    cell_table = kg_dyn_build_cell_table(
      baseline_t   = bt,
      year_idx     = j,
      r_S_vec      = r_S_named,
      lambda_I_vec = lambda_I_named,
      r_V_B_vec    = r_V_B_named,
      r_V_S_vec    = r_V_S_named,
      delta_prev   = delta,
      tau_B_col    = tau_B_mat[bathtub_ages_chr, j],
      tau_S_col    = tau_S_mat[bathtub_ages_chr, j],
      W_B_col      = pass1$W    [bathtub_ages_chr, j],
      W_S_col      = pass2$W    [bathtub_ages_chr, j],
      MC_B_col     = pass1$MC   [bathtub_ages_chr, j],
      MC_S_col     = pass2$MC   [bathtub_ages_chr, j],
      kappa_col    = pass1$kappa[bathtub_ages_chr, j],
      r_D_B_col    = pass1$r_D  [bathtub_ages_chr, j],
      r_D_S_col    = pass2$r_D  [bathtub_ages_chr, j],
      planned_diag  = list(
        r_S_unclipped = setNames(rate_info$r_S_unclipped, bathtub_ages_chr),
        timing_clipped = setNames(rate_info$timing_clipped, bathtub_ages_chr),
        r_fixed_B = setNames(rate_info$r_fixed_B, bathtub_ages_chr),
        r_planned_B = setNames(rate_info$r_planned_B, bathtub_ages_chr),
        r_planned_S = setNames(rate_info$r_planned_S, bathtub_ages_chr),
        r_forced_B = setNames(rate_info$r_forced_B, bathtub_ages_chr),
        r_forced_B_model = setNames(rate_info$r_forced_B_model, bathtub_ages_chr),
        r_forced_S = setNames(rate_info$r_forced_S, bathtub_ages_chr),
        r_B_model = setNames(rate_info$r_B_model, bathtub_ages_chr),
        r_ordinary_B = setNames(rate_info$r_ordinary_B, bathtub_ages_chr),
        r_ordinary_S = setNames(rate_info$r_ordinary_S, bathtub_ages_chr),
        R_planned_B = forced_state$R_planned_B[, j],
        R_planned_S = forced_state$R_planned_S[, j],
        R_forced_B = forced_state$R_forced_B[, j],
        R_forced_B_model = forced_state$R_forced_B_model[, j],
        R_forced_S = forced_state$R_forced_S[, j],
        planned_timing_shift = forced_state$planned_timing_shift[, j],
        forced_timing_shift = forced_state$forced_timing_shift[, j],
        E_forced_B = forced_state$E_forced_B[, j],
        q_forced_B = forced_state$q_forced_B[, j],
        q_forced_S = forced_state$q_forced_S[, j],
        forced_intercept = forced_state$forced_intercept[, j],
        forced_timing_advantage = forced_state$forced_timing_advantage[, j],
        F0_forced_B = forced_state$F0_forced_B[, j],
        F0_forced_S = forced_state$F0_forced_S[, j],
        F1_forced_B = forced_state$F1_forced_B[, j],
        F1_forced_S = forced_state$F1_forced_S[, j]
      ),
      ages_bathtub = ages_bathtub
    )

    saveRDS(list(regime     = regime,
                 cell_table = cell_table),
            kg_dyn_state_path(scenario_info, t))

    delta = setNames(step$delta_next, bathtub_ages_chr)
  }

  invisible(NULL)
}



#-------------------------------------------------------------------------------
# Cell-MTR tau builder
#
# Each cohort uses its own gain-stock-weighted average effective MTR on
# kg_lt, pulled from the simulator's static detail. This is the only
# supported tau parameterization; flat top-rate proxies are not.
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cell_mtr = function(records_with_attrs,
                                      ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Realization-weighted cell-MTR aggregation. Records must carry G_unit,
  # age_cohort, weight, kg_lt, and mtr_kg_lt. Per cell:
  #
  #   tau(a) = sum(weight * pmax(kg_lt, 0) * mtr_kg_lt) / sum(weight * pmax(kg_lt, 0))
  #
  # The realization-weighting is the right anchor for elasticity calibration:
  # it captures the average MTR on the dollars that actually realize, which
  # is the variable the realization decision responds to. Falls back to
  # gain-stock weighting when a cell has zero positive realizations (young
  # heir cohorts under carryover); falls back to 0 when both are zero.
  #----------------------------------------------------------------------------

  agg = records_with_attrs %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    group_by(age_cohort) %>%
    summarise(num_R = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE),
              den_R = sum(weight * kg_pos,             na.rm = TRUE),
              num_G = sum(weight * G_unit * mtr_kg_lt, na.rm = TRUE),
              den_G = sum(weight * G_unit,             na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(num_R, den_R, num_G, den_G), ~ if_else(is.na(.), 0, .)),
           tau = case_when(
             den_R > 0 ~ num_R / den_R,
             den_G > 0 ~ num_G / den_G,
             TRUE      ~ 0
           )) %>%
    arrange(age) %>%
    pull(tau)

  setNames(out, as.character(ages))
}



#-------------------------------------------------------------------------------
# Post-processing: bathtub diagnostics summary
#-------------------------------------------------------------------------------

kg_dyn_build_summary = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads all per-year bathtub state files for one scenario and writes two
  # diagnostic CSVs:
  #
  #   conventional/supplemental/kg_dynamics_age_profile.csv
  #     Long format (year x age) dump of cell_table. Use this for plots
  #     of dG, r_B, r_S, m, W, MC, kappa, etc. across age and time.
  #
  #   conventional/supplemental/kg_dynamics_summary.csv
  #     Year-level rollup: regime parameters, gain-stock-weighted averages
  #     of m / r_B / r_S / tau / W / MC / kappa, channel decomposition of
  #     reform-induced realizations, decedent stock and routing, implied
  #     year-by-year aggregate semi-elasticity dlog(R)/dtau.
  #
  # No-op if the scenario has no bathtub state directory.
  #----------------------------------------------------------------------------

  state_dir = file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_state')
  if (!dir.exists(state_dir)) return(invisible(NULL))

  years = scenario_info$years
  state_files = file.path(state_dir, paste0(years, '.rds'))
  if (!all(file.exists(state_files))) return(invisible(NULL))

  states = lapply(years, function(t) readRDS(file.path(state_dir, paste0(t, '.rds'))))
  names(states) = as.character(years)

  # Long-format age profile + per-year regime metadata
  age_profile = bind_rows(lapply(seq_along(years), function(i) {
    s = states[[i]]
    s$cell_table %>%
      mutate(year   = years[i],
             regime = s$regime$name) %>%
      relocate(year, regime, age)
  }))

  age_profile %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_age_profile.csv'))

  # Year-level rollup
  regime_df = bind_rows(lapply(seq_along(years), function(i) {
    r = states[[i]]$regime
    tibble(year          = years[i],
           regime        = r$name,
           c_phi         = r$c_phi,
           delta_vanish  = r$delta_vanish,
           delta_route   = r$delta_route,
           delta_realize = r$delta_realize)
  }))

  yearly = age_profile %>%
    group_by(year) %>%
    summarise(
      G_B_total       = sum(G_B),
      R_B_total       = sum(R_B),
      dG_total        = sum(dG),
      m_avg_gw        = if_else(sum(G_B) > 0, sum(m * G_B) / sum(G_B), NA_real_),
      r_B_avg_gw      = if_else(sum(G_B) > 0, sum(r_B * G_B) / sum(G_B), 0),
      r_S_avg_gw      = if_else(sum(G_B) > 0, sum(r_S * G_B) / sum(G_B), 0),
      lambda_I_avg_gw = if_else(sum(G_B) > 0, sum(lambda_I * G_B) / sum(G_B), NA_real_),
      r_fixed_avg_gw  = if_else(sum(G_B) > 0, sum(r_fixed_B * G_B) / sum(G_B), NA_real_),
      r_planned_B_avg_gw = if_else(sum(G_B) > 0, sum(r_planned_B * G_B) / sum(G_B), NA_real_),
      r_planned_S_avg_gw = if_else(sum(G_B) > 0, sum(r_planned_S * G_B) / sum(G_B), NA_real_),
      r_forced_B_avg_gw = if_else(sum(G_B) > 0, sum(r_forced_B * G_B) / sum(G_B), NA_real_),
      r_forced_S_avg_gw = if_else(sum(G_B) > 0, sum(r_forced_S * G_B) / sum(G_B), NA_real_),
      r_ordinary_B_avg_gw = if_else(sum(G_B) > 0, sum(r_ordinary_B * G_B) / sum(G_B), NA_real_),
      r_ordinary_S_avg_gw = if_else(sum(G_B) > 0, sum(r_ordinary_S * G_B) / sum(G_B), NA_real_),
      q_forced_B_avg = if_else(sum(E_forced_B) > 0,
                               sum(q_forced_B * E_forced_B) / sum(E_forced_B),
                               NA_real_),
      q_forced_S_avg = if_else(sum(E_forced_B) > 0,
                               sum(q_forced_S * E_forced_B) / sum(E_forced_B),
                               NA_real_),
      v_share_avg_rw  = if_else(sum(R_B) > 0,
                                sum(r_V_B * G_B) / sum(r_B * G_B),
                                NA_real_),
      tau_B_avg_gw    = if_else(sum(G_B) > 0, sum(tau_B * G_B) / sum(G_B), NA_real_),
      tau_S_avg_gw    = if_else(sum(G_B) > 0, sum(tau_S * G_B) / sum(G_B), NA_real_),
      tau_B_avg_rw    = if_else(sum(R_B) > 0, sum(tau_B * R_B) / sum(R_B), NA_real_),
      tau_S_avg_rw    = if_else(sum(R_B) > 0, sum(tau_S * R_B) / sum(R_B), NA_real_),
      W_B_avg_gw      = if_else(sum(G_B) > 0, sum(W_B * G_B) / sum(G_B), NA_real_),
      W_S_avg_gw      = if_else(sum(G_B) > 0, sum(W_S * G_B) / sum(G_B), NA_real_),
      MC_B_avg_gw     = if_else(sum(G_B) > 0, sum(MC_B * G_B) / sum(G_B), NA_real_),
      MC_S_avg_gw     = if_else(sum(G_B) > 0, sum(MC_S * G_B) / sum(G_B), NA_real_),
      kappa_avg_gw    = if_else(sum(G_B) > 0, sum(kappa * G_B) / sum(G_B), NA_real_),
      rate_channel    = sum(R_B * (rate_factor - 1)),
      lockin_channel  = sum(extra_R),
      R_planned_B_total = sum(R_planned_B),
      R_planned_S_total = sum(R_planned_S),
      R_forced_B_total = sum(R_forced_B),
      R_forced_S_total = sum(R_forced_S),
      planned_timing_shift_total = sum(planned_timing_shift),
      forced_timing_shift_total = sum(forced_timing_shift),
      E_forced_B_total = sum(E_forced_B),
      timing_clipped_cells = sum(timing_clipped, na.rm = TRUE),
      decedent_stock  = sum(mG_record * deemed_factor),
      .groups = 'drop'
    ) %>%
    left_join(regime_df, by = 'year') %>%
    mutate(
      inheritance_flow   = delta_route   * decedent_stock,
      deemed_realized    = delta_realize * decedent_stock,
      R_S_total          = R_B_total + rate_channel + lockin_channel,
      dtau               = tau_S_avg_rw - tau_B_avg_rw,
      semi_elast_implied = if_else(R_B_total > 0 & R_S_total > 0 &
                                     abs(dtau) > 1e-10,
                                   log(R_S_total / R_B_total) / dtau,
                                   NA_real_)
    ) %>%
    select(year, regime, c_phi, delta_vanish, delta_route, delta_realize,
           G_B_total, R_B_total, R_S_total, dG_total,
           m_avg_gw, r_B_avg_gw, r_S_avg_gw,
           lambda_I_avg_gw, r_fixed_avg_gw,
           r_planned_B_avg_gw, r_planned_S_avg_gw,
           r_forced_B_avg_gw, r_forced_S_avg_gw,
           r_ordinary_B_avg_gw, r_ordinary_S_avg_gw,
           q_forced_B_avg, q_forced_S_avg,
           v_share_avg_rw,
           tau_B_avg_gw, tau_S_avg_gw, tau_B_avg_rw, tau_S_avg_rw,
           W_B_avg_gw, W_S_avg_gw, MC_B_avg_gw, MC_S_avg_gw, kappa_avg_gw,
           rate_channel, lockin_channel,
           R_planned_B_total, R_planned_S_total,
           R_forced_B_total, R_forced_S_total,
           planned_timing_shift_total, forced_timing_shift_total,
           E_forced_B_total, timing_clipped_cells,
           decedent_stock, inheritance_flow, deemed_realized,
           semi_elast_implied)

  yearly %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_summary.csv'))

  invisible(NULL)
}

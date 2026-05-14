#-------------------------------------------------------------------------------
# kg_dynamics.R
#
# Capital-gains dynamics behavioral module. Implements the law of motion for
# the policy-induced delta in unrealized capital gains via a representative-
# cell Bellman whose control is the discretionary realization rate r_D
# directly; see other/kg_model_tests/representative_cell_bellman_proposal.md.
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
# Bellman primitives. The representative cell maximizes per dollar of
# unrealized gain:
#   W^j(a,t) = max_{r_D in [0, 1 - r_exog_B]} {
#       kappa(a,t)*r_D - (psi/2)*r_D^2
#     - tau^j(a,t)*r_D
#     + (1 - r_exog_B - r_D) *
#         [beta*(1-m) W^j(a+1,t+1) + beta*m*F^j(a,t)]
#   }
# where r_exog_B = (fixed_share + planned_share)*r_B is the baseline
# realization share outside the ordinary Bellman bucket,
# F^j = (1 - c_phi^j)*tau^j is the death-state tax-liability forgiveness
# value (c_phi^j is the regime's holder-internalized burden share: 0 step-up,
# theta carryover, 1 deemed). Marginal cost of realization:
#   MC^j(a,t) = tau^j + beta*(1-m)*W^j(a+1,t+1) + beta*m*F^j.
# Interior FOC: r_D = (kappa - MC)/psi, clipped to [0, 1 - r_exog_B].
# kappa(a,t) is recovered from baseline so r_D^B is the ordinary bucket:
#   kappa = MC^B + psi * r_D^B   (at corner cells with r_D^B = 0, kappa = MC^B).
#
# Current implementation collapses the five tracked wealth classes into a
# single asset bucket; per-asset-class disaggregation is on the roadmap.
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

KG_DYN_AGE_MIN          = 18
KG_DYN_AGE_MAX          = 80      # bathtub topcode (matches simulator)
KG_DYN_AGE_MAX_BELLMAN  = 119     # SSA PerLifeTables hit q(x)=1 at 119

# Fallback annual discount factor for isolated solver unit tests. Production
# paths build a year-varying real-rate series via kg_dyn_load_beta_series
# (tsy_10y Fisher-deflated by year-t YoY CPI-U).
KG_DYN_BETA             = 0.978

# Realization bucket shares. phi_I is the fixed/nonresponsive share;
# planned is mechanically timeable across nearby years; the remainder is
# the ordinary Bellman-controlled share.
KG_DYN_PHI_I            = 0.4
KG_DYN_SHARE_PLANNED    = 0.3285
KG_DYN_TIMING_WINDOW    = 1L

# Fraction of planned dollars that move toward the best year in the window
# is clamp((tau_S - tau_B between source and destination) / ref_wedge, 0, 1).
# 5pp moves the full bucket; 1pp moves 20%.
KG_DYN_TIMING_REF_WEDGE = 0.05

# Static resource: dollar-weighted heir-age distribution derived from SCF
# 2022 inheritance data (filtered to non-gift transfers, weighted by
# Gale-Sabelhaus 2024 recency probabilities for the current-year flow).
# Built by other/kg_model_tests/build_heir_distribution.R; see that script
# for the filter definitions. Treated as a model constant because the
# upstream survey is not year-varying at the projection horizons we use.
KG_DYN_HEIR_DISTRIBUTION_PATH = './resources/heir_distribution_scf2022.csv'

# Calibrated jointly with KG_DYN_SHARE_PLANNED in
# other/kg_model_tests/calibrate.R against long-run dlog(R)/dtau (sim year
# 30 of a +1pp permanent shock) and short-run announcement-year response
# (year 1 of a delayed +5pp shock). Re-run calibration whenever Tax-Data
# vintage, bucket shares, ref_wedge, the discount series, or any Bellman
# primitive changes.
KG_DYN_DEFAULT_PSI      = 26.5673

# Within-cell allocation rule for policy-induced dG, controlling the
# effective cell mortality m_eff used in the death/survivor channels.
#   "G" — dG allocated proportional to G_unit; m_eff = sum(w*m*G)/sum(w*G).
#         Inheritance-flow story.
#   "R" — dG allocated proportional to positive kg_lt; m_eff = sum(w*m*R)/sum(w*R).
#         Lock-in story. Falls back to "G" when R_B = 0.
# Only affects carryover/deemed; step-up is unchanged (death channel off).
KG_DYN_DG_ALLOCATION    = 'G'

KG_DYN_ASSET_VALUE_COLS = c('value.equities', 'value.pass_throughs',
                            'value.primary_home', 'value.other_home',
                            'value.re_fund')
KG_DYN_ASSET_BASIS_COLS = c('basis.equities', 'basis.pass_throughs',
                            'basis.primary_home', 'basis.other_home',
                            'basis.re_fund')

# Trustees Report Alternative 2, 50/50 male/female blend (cohort module is
# gender-blind). Supplies the 81+ tail of the Bellman extended grid.
KG_DYN_LIFE_TABLE_M_PATH = './resources/PerLifeTables_M_Alt2_TR2024.csv'
KG_DYN_LIFE_TABLE_F_PATH = './resources/PerLifeTables_F_Alt2_TR2024.csv'


# Death-regime taxonomy. YAML pref.kg_death_regime is an integer code;
# the bequest motive theta is supplied separately and overrides c_phi for
# carryover. c_phi is the death-state burden share the holder internalizes:
# 0 step-up (full forgiveness), theta carryover, 1 deemed (no forgiveness).
# Forgiveness value F = (1 - c_phi) * tau.
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

  # Adds three derived columns the bathtub recurrence needs:
  #   G_unit       : per-record unrealized gain stock, sum_k max(0, value_k -
  #                  basis_k) across the five tracked wealth classes
  #   m_household  : q_death1 * q_death2 for joint filers; q_death1 otherwise
  #   age_cohort   : max(age1, age2) for joint, age1 otherwise; clipped to
  #                  [KG_DYN_AGE_MIN, KG_DYN_AGE_MAX]

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

  # Weight-aggregates per-record (G_unit, kg_lt, m_household) to age cells.
  # tax_units must already have G_unit, m_household, age_cohort attached.
  #
  # R_B uses positive-only sums of kg_lt so r_B >= 0 and per-record
  # allocation shares (pmax(kg_lt, 0) / R_B) sum to 1.
  #
  # Sparse-cell fallback (spec §5.1): cells with G_B > 0 but R_B = 0 inherit
  # the gain-stock-weighted aggregate r_B. Prevents young heir cohorts
  # (carryover / deemed inflows) from getting r_S = 0 forever.

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

kg_dyn_build_heir_matrix = function(heir_dist,
                                    ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Row-stochastic omega[a, h] = share of decedent-age-a gains routed to
  # heir-age h. Every row is a copy of the empirical dollar-weighted
  # heir-age distribution heir_dist, sourced from
  # kg_dyn_load_heir_distribution (which reads the static SCF-derived
  # resource at KG_DYN_HEIR_DISTRIBUTION_PATH).
  #
  # This is equivalent to assuming heir age is independent of decedent age
  # conditional on inheritance. Marginal heir flow matches the data
  # exactly; conditional dispersion is the part the marginals don't pin
  # down. Compare to a Gaussian-shift prior + IPF, which would let the
  # conditional vary at the cost of an external prior — for revenue scoring
  # under carryover the marginal-only rule is the right default.

  n = length(ages)
  if (length(heir_dist) != n) {
    stop(sprintf(
      'kg_dyn_build_heir_matrix: heir_dist length %d != length(ages) %d.',
      length(heir_dist), n))
  }
  if (any(heir_dist < 0, na.rm = TRUE) || any(is.na(heir_dist))) {
    stop('kg_dyn_build_heir_matrix: heir_dist must be nonnegative and ',
         'free of NA.')
  }
  s = sum(heir_dist)
  if (!is.finite(s) || s <= 0) {
    stop('kg_dyn_build_heir_matrix: heir_dist has nonpositive sum.')
  }
  row = as.numeric(heir_dist) / s

  W = matrix(row, nrow = n, ncol = n, byrow = TRUE)
  stopifnot(all(abs(rowSums(W) - 1) < 1e-12))
  rownames(W) = colnames(W) = ages
  W
}



kg_dyn_build_aging_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # A[a, h] = 1 if h = a + 1; A[a_max, a_max] = 1 (topcode loops). Spec §3.4.

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

  # Supplies the post-topcode mortality tail [81, 119] that the Bellman
  # needs for a true terminal condition (q(119) = 1 in the SSA tables).
  # Returns a [age, year] matrix of gender-blended q(x).

  load_one = function(path) {
    # PerLifeTables files: 4 metadata lines, then header (Year,x,q(x),...),
    # then data. Column names are odd ("q(x)", "12a(x)"); slice by position.
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

  blended = blended %>% arrange(year, x)
  out = matrix(blended$q, nrow = length(ages_ext), ncol = length(years),
               dimnames = list(as.character(ages_ext), as.character(years)))
  stopifnot(all(!is.na(out)))
  out
}



#-------------------------------------------------------------------------------
# Real-rate discount factor series (year-varying)
#-------------------------------------------------------------------------------

kg_dyn_load_beta_series = function(macro_root, years) {

  # Per-year Bellman discount built from Macro-Projections: Fisher-deflated
  # 10-year Treasury yield.
  #   infl_t   = cpiu_t / cpiu_{t-1} - 1
  #   r_real_t = (1 + tsy_10y_t / 100) / (1 + infl_t) - 1
  #   beta_t   = 1 / (1 + r_real_t)
  # Inflation cancels in the realize-now vs. hold-and-pay-on-nominally-larger-
  # gain trade-off, so the economically correct discount is real; using
  # nominal tsy_10y would double-count inflation.

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

  # Stitches the simulator's [18, 80] cell aggregates together with the
  # SSA life-table tail [81, 119] into a per-year extended grid. The
  # bathtub recurrence stays on [18, 80]; only the Bellman uses the
  # extended grid (for a true mortality-driven terminal condition).
  #
  # For ages 81+: m comes from life_ext; r_B is held flat at r_B(80, t),
  # the topcode-pool rate (otherwise the Bellman's continuation value at
  # age 80 would be purely death-driven and over-state regime-induced
  # acceleration in older cohorts under deemed). G_B/R_B stay 0 since the
  # per-dollar Bellman doesn't need cell totals.

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
# Bellman backward induction. Outer loop backward in time, inner loop
# backward in age, on the extended grid [18, 119]. Terminal condition
# W[A_max+1, .] = 0 (m(119) ~ 1, binds quickly). At year t_max we seed
# W[, t_max+1] with a stationary backward sweep using t_max primitives.
# Pass 1 solves baseline and recovers kappa; Pass 2 solves the scenario
# from that kappa via the clipped quadratic FOC.
#-------------------------------------------------------------------------------

kg_dyn_pack_tau = function(tau_list, years,
                            ages_bellman = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN) {

  # Packs per-year tau vectors (over [18, 80]) into a matrix on the
  # Bellman grid; ages 81+ inherit tau(80).

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

  # Packs per-year (m, r_B) into [age, year] matrices for the Bellman.
  #
  # m is gain-stock-weighted: sum(w*m_household*G_unit) / sum(w*G_unit).
  # The Bellman is normalized per dollar of unrealized gain, so it needs
  # the probability that the *dollar's* holder dies, not the average
  # taxpayer in the cell. (Taxpayer-weighted m is biased upward 2-3x in
  # practice since wealthier holders die less.) Falls back to taxpayer-
  # weighted m where G_B = 0 — i.e., 81+ on the extended grid, where
  # grid_ext$m is the SSA life-table mortality anyway.

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

  # One age-backward sweep through [a_min, a_max] for a single year column.
  #
  # kappa_col = NULL: Pass 1 (baseline). r_D_B = r_B after removing fixed
  # and planned buckets; kappa recovered so this is the cell's optimum.
  # kappa_col supplied: Pass 2 (scenario). r_D = clip((kappa - MC)/psi, 0,
  # 1 - r_exog_B).
  #
  # stationary = TRUE seeds the terminal year by pulling W[i+1] from the
  # same sweep (year-t_max primitives constant forward); otherwise uses
  # W_next[i+1]. At i == n_ages the continuation is 0 (W[A_max+1] = 0).

  n_ages = length(m_col)
  W     = numeric(n_ages)
  MC    = numeric(n_ages)
  r_D   = numeric(n_ages)
  kappa = numeric(n_ages)

  is_baseline_pass = is.null(kappa_col)

  # Precompute age-vector quantities used inside the loop.
  F_vec        = (1 - c_phi) * tau_col
  r_exog_B_vec = (phi_I + planned_share) * r_B_col
  r_D_cap_vec  = pmax(1 - r_exog_B_vec, 0)
  bs_vec       = beta * (1 - m_col)   # survivor discount
  bm_vec       = beta * m_col         # death-state discount

  for (i in n_ages:1) {
    tau_i = tau_col[i]
    F_i   = F_vec[i]

    W_next_i = if (i == n_ages) 0 else if (stationary) W[i + 1] else W_next[i + 1]

    death_cont = bs_vec[i] * W_next_i + bm_vec[i] * F_i
    MC_i       = tau_i + death_cont
    r_D_cap    = r_D_cap_vec[i]

    if (is_baseline_pass) {
      r_D_i   = min(max(r_B_col[i] - r_exog_B_vec[i], 0), r_D_cap)
      kappa_i = MC_i + psi * r_D_i
    } else {
      kappa_i = kappa_col[i]
      r_D_i   = min(max((kappa_i - MC_i) / psi, 0), r_D_cap)
    }

    remaining = max(1 - r_exog_B_vec[i] - r_D_i, 0)
    W[i]     = kappa_i * r_D_i - 0.5 * psi * r_D_i * r_D_i -
               tau_i * r_D_i + remaining * death_cont
    MC[i]    = MC_i
    r_D[i]   = r_D_i
    kappa[i] = kappa_i
  }

  list(W = W, MC = MC, r_D = r_D, kappa = kappa)
}



kg_dyn_solve_bellman = function(grid_packed, tau_mat, c_phi,
                                kappa_mat     = NULL,
                                psi           = KG_DYN_DEFAULT_PSI,
                                phi_I         = KG_DYN_PHI_I,
                                planned_share = KG_DYN_SHARE_PLANNED,
                                beta_by_year  = NULL) {

  #----------------------------------------------------------------------------
  # Backward induction over (age, year) cells.
  #
  # When kappa_mat = NULL: Pass 1 (baseline). Recovers kappa from the FOC
  # by forcing optimal r_D to equal the observed ordinary realization
  # bucket. c_phi is a scalar (typically 0 under current-law step-up).
  #
  # When kappa_mat is supplied: Pass 2 (scenario). Solves the clipped
  # quadratic FOC r_D = clip((kappa - MC)/psi, 0, 1 - r_exog_B). c_phi may
  # be a scalar or a length-n_years vector (e.g., a carryover regime
  # phased in mid-horizon).
  #
  # beta_by_year[j] discounts between year j and j+1; NULL falls back to a
  # constant KG_DYN_BETA vector for isolated solver unit tests.
  #
  # Returns: list(W, MC, kappa, r_D), each [age, year].
  #----------------------------------------------------------------------------

  m_mat   = grid_packed$m
  r_B_mat = grid_packed$r_B
  n_ages  = nrow(m_mat); n_years = ncol(m_mat)
  ages_chr  = rownames(m_mat); years_chr = colnames(m_mat)

  if (is.null(beta_by_year)) beta_by_year = rep(KG_DYN_BETA, n_years)
  stopifnot(length(beta_by_year) == n_years)

  c_phi_vec = if (length(c_phi) == 1) rep(c_phi, n_years) else c_phi
  stopifnot(length(c_phi_vec) == n_years)

  W     = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  MC    = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  kappa = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_D   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))

  sweep = function(j, W_next_col, stationary) {
    kg_dyn_bellman_sweep_age(
      W_next        = W_next_col,
      m_col         = m_mat  [, j],
      r_B_col       = r_B_mat[, j],
      tau_col       = tau_mat[, j],
      c_phi         = c_phi_vec[j],
      psi           = psi,
      phi_I         = phi_I,
      beta          = beta_by_year[j],
      planned_share = planned_share,
      kappa_col     = if (is.null(kappa_mat)) NULL else kappa_mat[, j],
      stationary    = stationary
    )
  }

  # Terminal year: stationary backward solve in age (W[a+1] from the same
  # sweep), treating year-n_years primitives as constant forward.
  res = sweep(n_years, W_next_col = NULL, stationary = TRUE)
  W    [, n_years] = res$W
  MC   [, n_years] = res$MC
  kappa[, n_years] = res$kappa
  r_D  [, n_years] = res$r_D

  # March backward in time.
  if (n_years >= 2) {
    for (j in (n_years - 1):1) {
      res = sweep(j, W_next_col = W[, j + 1], stationary = FALSE)
      W    [, j] = res$W
      MC   [, j] = res$MC
      kappa[, j] = res$kappa
      r_D  [, j] = res$r_D
    }
  }

  list(W = W, MC = MC, kappa = kappa, r_D = r_D)
}



#-------------------------------------------------------------------------------
# Three-bucket realization timing helpers
#-------------------------------------------------------------------------------

kg_dyn_validate_realization_buckets = function(fixed_share   = KG_DYN_PHI_I,
                                               planned_share = KG_DYN_SHARE_PLANNED,
                                               timing_window = KG_DYN_TIMING_WINDOW,
                                               ref_wedge     = KG_DYN_TIMING_REF_WEDGE) {

  if (!is.finite(fixed_share) || !is.finite(planned_share)) {
    stop('kg_dynamics: realization bucket shares must be finite.')
  }
  if (fixed_share < 0 || planned_share < 0 || fixed_share + planned_share > 1) {
    stop(sprintf(
      paste0('kg_dynamics: invalid realization bucket shares: fixed=%.4f, ',
             'planned=%.4f. Expected nonnegative shares with fixed + ',
             'planned <= 1.'),
      fixed_share, planned_share))
  }
  if (length(timing_window) != 1 || is.na(timing_window) ||
      timing_window < 0 || timing_window != as.integer(timing_window)) {
    stop('kg_dynamics: KG_DYN_TIMING_WINDOW must be a nonnegative integer.')
  }
  if (length(ref_wedge) != 1 || !is.finite(ref_wedge) || ref_wedge <= 0) {
    stop('kg_dynamics: KG_DYN_TIMING_REF_WEDGE must be a positive finite number.')
  }

  invisible(TRUE)
}



kg_dyn_build_planned_timing = function(baseline_cells, tau_S_mat, years,
                                       tau_B_mat = NULL,
                                       planned_share = KG_DYN_SHARE_PLANNED,
                                       timing_window = KG_DYN_TIMING_WINDOW,
                                       ref_wedge     = KG_DYN_TIMING_REF_WEDGE,
                                       ages_bathtub = KG_DYN_AGE_MIN:
                                                      KG_DYN_AGE_MAX,
                                       tie_tol = 1e-12) {

  # For each (age, source-year u), planned baseline dollars look at the
  # policy-induced wedge tau_S - tau_B over [u-H, u+H] and route toward
  # the best year v* (lowest wedge; ties broken by nearest, then earlier).
  # Move fraction = clamp((wedge[u] - wedge[v*]) / ref_wedge, 0, 1); the
  # complement stays at u. Using tau_S - tau_B (not just tau_S) keeps the
  # rule policy-driven so baseline-only runs don't retime dollars.

  kg_dyn_validate_realization_buckets(planned_share = planned_share,
                                      timing_window = timing_window,
                                      ref_wedge     = ref_wedge)

  ages_chr  = as.character(ages_bathtub)
  years_chr = as.character(years)
  n_ages    = length(ages_bathtub)
  n_years   = length(years)
  H         = as.integer(timing_window)

  R_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = baseline_cells[[t_chr]]
    R_B[, t_chr] = bt$R_B[match(ages_bathtub, bt$age)]
  }

  R_planned_B = planned_share * R_B
  R_planned_S = matrix(0, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  timing_tau_mat = if (is.null(tau_B_mat)) tau_S_mat else tau_S_mat - tau_B_mat
  tau_bt = timing_tau_mat[ages_chr, years_chr, drop = FALSE]

  if (planned_share == 0 || H == 0) {
    R_planned_S = R_planned_B
  } else {
    for (i in seq_len(n_ages)) {
      for (j in seq_len(n_years)) {
        amount = R_planned_B[i, j]
        if (amount == 0) next

        eligible = max(1, j - H):min(n_years, j + H)
        tau_vals = tau_bt[i, eligible]
        min_tau  = min(tau_vals, na.rm = TRUE)

        if (tau_bt[i, j] <= min_tau + tie_tol) {
          R_planned_S[i, j] = R_planned_S[i, j] + amount
        } else {
          candidates = eligible[tau_vals <= min_tau + tie_tol]
          distances  = abs(candidates - j)
          nearest    = candidates[distances == min(distances)]
          dest       = min(nearest)

          tax_saving = tau_bt[i, j] - tau_bt[i, dest]
          move_share = min(max(tax_saving / ref_wedge, 0), 1)
          moved      = amount * move_share

          R_planned_S[i, dest] = R_planned_S[i, dest] + moved
          R_planned_S[i, j]    = R_planned_S[i, j]    + (amount - moved)
        }
      }
    }
  }

  list(R_planned_B = R_planned_B,
       R_planned_S = R_planned_S,
       planned_timing_shift = R_planned_S - R_planned_B)
}



kg_dyn_build_scenario_rate = function(baseline_t, r_ordinary_S,
                                      R_planned_B_col, R_planned_S_col,
                                      fixed_share = KG_DYN_PHI_I) {

  G_B = baseline_t$G_B
  r_B = baseline_t$r_B

  r_fixed_B    = fixed_share * r_B
  r_planned_B  = ifelse(G_B > 0, R_planned_B_col / G_B, 0)
  r_planned_S  = ifelse(G_B > 0, R_planned_S_col / G_B, 0)
  r_ordinary_B = pmax(r_B - r_fixed_B - r_planned_B, 0)

  r_S_unclipped = r_fixed_B + r_ordinary_S + r_planned_S
  r_S           = pmin(pmax(r_S_unclipped, 0), 1)

  list(r_S            = r_S,
       r_S_unclipped  = r_S_unclipped,
       timing_clipped = abs(r_S - r_S_unclipped) > 1e-12,
       r_fixed_B      = r_fixed_B,
       r_planned_B    = r_planned_B,
       r_planned_S    = r_planned_S,
       r_ordinary_B   = r_ordinary_B,
       r_ordinary_S   = r_ordinary_S)
}



#-------------------------------------------------------------------------------
# Bathtub recurrence step
#-------------------------------------------------------------------------------

kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  r_S_vec, delta_route,
                                  phi_I = KG_DYN_PHI_I) {

  # One-step bathtub recurrence for delta_G on the [18, 80] grid. r_S_vec
  # combines the fixed, Bellman ordinary, and retimed planned buckets.
  #
  # Topcode caveat: the age=80 cell pools all 80+ taxpayers with a single
  # weight-averaged m_80, refreshed from each year's Tax-Data. Within-pool
  # heterogeneity (e.g., 15-year topcode residents vs. newly aged-in) is
  # smoothed out — small effect in practice but worth flagging if reforms
  # shift the topcode age mix.

  G_B       = baseline_t$G_B
  r_B       = baseline_t$r_B
  R_B       = baseline_t$R_B
  m         = baseline_t$m
  mG_record = baseline_t$mG_record
  mR_record = baseline_t$mR_record

  # Effective cell mortality m_eff = sum(w*m*X) / sum(w*X). The death
  # channel needs sum_i w_i * m_i * (G_unit_i + dG_i); the naive cell-mean
  # form m * (G_B + dG) overstates that by ~2.7x in our data due to a
  # large negative within-cell Cov(m, G_unit) (wealth-mortality gradient).
  # Allocating dG_i proportional to X_i and summing analytically gives an
  # exact per-record sum, not an approximation. Two rules via
  # KG_DYN_DG_ALLOCATION: "G" (X = G_unit) or "R" (X = pmax(kg_lt, 0),
  # falling back to "G" when R_B = 0).
  m_eff_G = if_else(G_B > 0, mG_record / G_B, m)
  m_eff_R = if_else(R_B > 0, mR_record / R_B, m_eff_G)

  m_eff = switch(KG_DYN_DG_ALLOCATION,
                 G = m_eff_G,
                 R = m_eff_R,
                 stop("Unknown KG_DYN_DG_ALLOCATION rule: ", KG_DYN_DG_ALLOCATION))
  m_eff = pmin(pmax(m_eff, 0), 1)

  # lambda_I = fixed/nonresponsive realization bucket; kept on the state
  # contract under the old name.
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

  # pref.kg_death_regime integer → canonical regime tuple (spec §3.3):
  #   0 = step_up           : c_phi = 0,     vanish = 1
  #   1 = carryover         : c_phi = theta, route = 1
  #   2 = deemed_realization: c_phi = 1,     realize = 1

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
# Per-record applier (pure allocator). Reads the precomputed cell_table from
# the bathtub state file and translates cell-level quantities into per-record
# kg_lt adjustments via three channels (spec §7.3):
#   rate     : kg_lt > 0 → kg_lt * rate_factor (= r_S/r_B, clamped to 1)
#   lock-in  : extra_R = r_S * dG, allocated by positive-kg_lt share if
#              R_B > 0, else by G_unit share, else skip
#   deemed   : delta_realize * m_household * G_unit * (G_B + dG)/G_B
# Also stamps decedent_flag = (u < m_household) using precomputed uniform
# draws from globals$random_numbers (same draw across scenarios).
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, delta_realize,
                                    decedent_random) {

  # Pull just the columns the applier consumes from cell_table via a
  # vectorized match() — avoids hash-joining the ~35-column diagnostics
  # table (with all the Bellman/timing/regime columns) onto 220k records
  # per scenario-year.
  idx           = match(tax_units$age_cohort, cell_table$age)
  rate_factor   = cell_table$rate_factor  [idx]
  extra_R       = cell_table$extra_R      [idx]
  deemed_factor = cell_table$deemed_factor[idx]
  R_B           = cell_table$R_B          [idx]
  G_B           = cell_table$G_B          [idx]

  tax_units %>%
    mutate(
      allocation = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      kg_lt = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt) +
              extra_R * allocation +
              delta_realize * m_household * G_unit * deemed_factor,
      decedent_flag = as.integer(decedent_random < m_household)
    ) %>%
    select(-allocation)
}



kg_dyn_state_dir = function(scenario_info) {
  file.path(scenario_info$output_path,
            'conventional', 'supplemental',
            'kg_dynamics_state')
}

kg_dyn_state_path = function(scenario_info, year) {
  file.path(kg_dyn_state_dir(scenario_info), paste0(year, '.rds'))
}

# Does this scenario's behavior set include any kg_dynamics module?
scenario_uses_kg_dynamics = function(scenario_info) {
  any(startsWith(scenario_info$behavior_modules %||% character(),
                 'kg_dynamics/'))
}



#-------------------------------------------------------------------------------
# Bathtub pre-pass orchestration
#-------------------------------------------------------------------------------

kg_dyn_load_heir_distribution = function(path = KG_DYN_HEIR_DISTRIBUTION_PATH,
                                          ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Reads the precomputed dollar-weighted heir-age distribution from a
  # static SCF-derived resource. Built by
  # other/kg_model_tests/build_heir_distribution.R; re-run that script
  # when the SCF vintage updates.

  if (!file.exists(path)) {
    stop('kg_dynamics: heir distribution resource missing at ', path,
         '. Regenerate via ',
         'sbatch other/kg_model_tests/build_heir_distribution.sbatch.')
  }

  raw = read_csv(path, show_col_types = FALSE)
  if (!all(c('age', 'share') %in% names(raw))) {
    stop('kg_dynamics: heir distribution resource at ', path,
         ' missing required columns (age, share).')
  }
  raw = raw %>% arrange(age)
  if (!identical(as.integer(raw$age), as.integer(ages))) {
    stop('kg_dynamics: heir distribution resource at ', path,
         ' has age range ', min(raw$age), ':', max(raw$age),
         ' but expected ', min(ages), ':', max(ages), '.')
  }
  if (any(raw$share < 0, na.rm = TRUE) || any(is.na(raw$share))) {
    stop('kg_dynamics: heir distribution resource at ', path,
         ' has negative or NA share entries.')
  }
  if (abs(sum(raw$share) - 1) > 1e-6) {
    stop('kg_dynamics: heir distribution shares at ', path,
         ' sum to ', sum(raw$share), ', expected 1.')
  }

  setNames(raw$share, as.character(raw$age))
}



kg_dyn_load_bathtub_inputs = function(scenario_info, baseline_root,
                                       sample_ids, pct_sample,
                                       ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Single Tax-Data pass producing baseline_cells (per-year G_B, R_B, r_B, m,
  # mG_record, mR_record over ages 18-80), baseline_tau, and reform_tau
  # (per-year R-weighted mtr_kg_lt vectors). Cell aggregates come straight
  # from Tax-Data csvs (the wealth value.*/basis.* and q_death* columns live
  # only there); mtr_kg_lt comes from each side's static detail.

  tax_data_root = scenario_info$interface_paths$`Tax-Data`
  years         = scenario_info$years

  heir_dist = kg_dyn_load_heir_distribution(ages = ages)

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

    # mtr aggregator only needs id/weight/kg_lt/age_cohort/G_unit; slim
    # before the joins so we don't drag the asset value.*/basis.* columns
    # through two hash joins on ~220k records.
    td_slim = td %>% select(id, weight, kg_lt, age_cohort, G_unit)

    read_mtr = function(path) {
      file.path(path, paste0(t, '.csv')) %>%
        fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
        as_tibble()
    }

    baseline_tau[[as.character(t)]] = td_slim %>%
      left_join(read_mtr(file.path(baseline_root, 'baseline', 'static',
                                   'detail')),
                by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)

    reform_tau[[as.character(t)]] = td_slim %>%
      left_join(read_mtr(file.path(scenario_info$output_path, 'static',
                                   'detail')),
                by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)
  }

  list(baseline_cells = baseline_cells,
       baseline_tau   = baseline_tau,
       reform_tau     = reform_tau,
       heir_dist      = heir_dist)
}



kg_dyn_build_cell_table = function(baseline_t, year_idx,
                                    r_S_vec, lambda_I_vec, r_V_B_vec, r_V_S_vec,
                                    delta_prev,
                                    tau_B_col, tau_S_col,
                                    W_B_col, W_S_col, MC_B_col, MC_S_col,
                                    kappa_col, r_D_B_col, r_D_S_col,
                                    planned_diag = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Assembles per-cell quantities the applier needs:
  #   rate_factor   = r_S / r_B           (clamped to 1 when r_B = 0)
  #   extra_R       = r_S * dG            (lock-in stock realized at r_S)
  #   deemed_factor = (G_B + dG) / G_B    (clamped >= 0)
  # Plus diagnostic columns used by kg_dyn_build_summary. Bellman matrices
  # are sliced from the extended grid to the bathtub grid [18, 80] before
  # persisting.

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
           r_ordinary_B  = as.numeric(diag_or('r_ordinary_B', r_D_B)[as.character(age)]),
           r_ordinary_S  = as.numeric(diag_or('r_ordinary_S', r_D_S)[as.character(age)]),
           R_planned_B   = as.numeric(diag_or('R_planned_B', 0)[as.character(age)]),
           R_planned_S   = as.numeric(diag_or('R_planned_S', 0)[as.character(age)]),
           planned_timing_shift =
             as.numeric(diag_or('planned_timing_shift', 0)[as.character(age)]),
           dG            = as.numeric(delta_prev  [as.character(age)]),
           tau_B         = as.numeric(tau_B_col   [as.character(age)]),
           tau_S         = as.numeric(tau_S_col   [as.character(age)]),
           W_B           = as.numeric(W_B_col     [as.character(age)]),
           W_S           = as.numeric(W_S_col     [as.character(age)]),
           MC_B          = as.numeric(MC_B_col    [as.character(age)]),
           MC_S          = as.numeric(MC_S_col    [as.character(age)]),
           kappa         = as.numeric(kappa_col   [as.character(age)]),
           rate_factor   = if_else(r_B > 0, r_S / r_B, 1),
           extra_R       = r_S * dG,
           deemed_factor = if_else(G_B > 0,
                                   pmax(0, (G_B + dG) / G_B),
                                   1)) %>%
    select(age, G_B, R_B, r_B, r_S, r_S_unclipped, timing_clipped,
           lambda_I, r_V_B, r_V_S,
           r_fixed_B, r_planned_B, r_planned_S, r_ordinary_B, r_ordinary_S,
           R_planned_B, R_planned_S, planned_timing_shift,
           m, mG_record, mR_record, dG,
           tau_B, tau_S, W_B, W_S, MC_B, MC_S, kappa, r_D_B, r_D_S,
           rate_factor, extra_R, deemed_factor)
}



kg_dyn_run_bathtub_pass = function(scenario_info, tax_law, baseline_cells,
                                    baseline_tau, reform_tau, heir_dist,
                                    psi   = KG_DYN_DEFAULT_PSI,
                                    phi_I = KG_DYN_PHI_I,
                                    planned_share = KG_DYN_SHARE_PLANNED,
                                    timing_window = KG_DYN_TIMING_WINDOW,
                                    ref_wedge     = KG_DYN_TIMING_REF_WEDGE,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  # Runs the bathtub recurrence across scenario_info$years and persists one
  # state file per year — the contract consumed by the kg_dynamics behavior
  # module's per-record applier. State at kg_dynamics_state/{t}.rds is
  # list(regime, cell_table).
  #
  # Flow:
  #   1. Build extended-grid baseline cells (bathtub + 81-119 SSA tail).
  #   2. Pack tau matrices (baseline + reform).
  #   3. Pass 1 Bellman (baseline): recover kappa.
  #   4. Resolve per-year scenario regime; Pass 2 Bellman using kappa.
  #   5. Build planned-timing schedule from tau_S - tau_B.
  #   6. Per year: combine buckets into r_S_vec, run kg_dyn_step_recurrence,
  #      build cell_table, persist.

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
  state_dir = kg_dyn_state_dir(scenario_info)
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

  # Step 3: baseline Bellman pass (c_phi = 0 under current-law step-up)
  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi = 0,
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

  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_mat, c_phi = c_phi_S,
                               kappa_mat = pass1$kappa,
                               psi = psi, phi_I = phi_I,
                               planned_share = planned_share,
                               beta_by_year = beta_by_year)

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = tau_S_mat,
    years          = years,
    tau_B_mat      = tau_B_mat,
    planned_share  = planned_share,
    timing_window  = timing_window,
    ref_wedge      = ref_wedge,
    ages_bathtub   = ages_bathtub
  )

  # Save life table and heir distribution for later diagnostic inspection
  saveRDS(life_ext,  file.path(state_dir, 'life_table_extension.rds'))
  saveRDS(heir_dist, file.path(state_dir, 'heir_distribution.rds'))

  # Step 5: year-by-year bathtub recurrence
  A     = kg_dyn_build_aging_matrix(ages_bathtub)
  omega = kg_dyn_build_heir_matrix(heir_dist, ages_bathtub)

  delta = setNames(rep(0, length(ages_bathtub)), as.character(ages_bathtub))
  bathtub_ages_chr = as.character(ages_bathtub)

  for (j in seq_along(years)) {
    t  = years[j]
    bt = baseline_cells[[as.character(t)]]
    regime = regime_list[[j]]

    # Slice Bellman outputs from extended grid to bathtub grid for this year
    r_D_S_bt = pass2$r_D[bathtub_ages_chr, j]
    rate_info = kg_dyn_build_scenario_rate(
      baseline_t       = bt,
      r_ordinary_S     = r_D_S_bt,
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j],
      fixed_share      = phi_I
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
        r_ordinary_B = setNames(rate_info$r_ordinary_B, bathtub_ages_chr),
        r_ordinary_S = setNames(rate_info$r_ordinary_S, bathtub_ages_chr),
        R_planned_B = planned_timing$R_planned_B[, j],
        R_planned_S = planned_timing$R_planned_S[, j],
        planned_timing_shift = planned_timing$planned_timing_shift[, j]
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

  # Realization-weighted cell-MTR aggregation: per cell
  #   tau(a) = sum(w * pmax(kg_lt, 0) * mtr_kg_lt) / sum(w * pmax(kg_lt, 0))
  # The right anchor for elasticity calibration — average MTR on the dollars
  # that realize. Falls back to gain-stock weighting when R = 0 (e.g., young
  # heir cohorts under carryover), then to 0 when both are zero.

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

  # Reads all per-year bathtub state files and writes:
  #   kg_dynamics_age_profile.csv : long (year × age) dump of cell_table
  #   kg_dynamics_summary.csv     : year-level rollup with regime, weighted
  #                                 means, channel decomposition, decedent
  #                                 stock, implied semi-elasticity.
  # No-op if the scenario has no bathtub state directory.

  state_dir = kg_dyn_state_dir(scenario_info)
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

  # Weighted means with a default when the weight column sums to zero.
  # r_B and r_S default to 0; everything else to NA.
  wmean = function(x, w, default = NA_real_) {
    s = sum(w)
    if (s > 0) sum(x * w) / s else default
  }

  yearly = age_profile %>%
    group_by(year) %>%
    summarise(
      G_B_total           = sum(G_B),
      R_B_total           = sum(R_B),
      dG_total            = sum(dG),
      m_avg_gw            = wmean(m,            G_B),
      r_B_avg_gw          = wmean(r_B,          G_B, default = 0),
      r_S_avg_gw          = wmean(r_S,          G_B, default = 0),
      lambda_I_avg_gw     = wmean(lambda_I,     G_B),
      r_fixed_avg_gw      = wmean(r_fixed_B,    G_B),
      r_planned_B_avg_gw  = wmean(r_planned_B,  G_B),
      r_planned_S_avg_gw  = wmean(r_planned_S,  G_B),
      r_ordinary_B_avg_gw = wmean(r_ordinary_B, G_B),
      r_ordinary_S_avg_gw = wmean(r_ordinary_S, G_B),
      v_share_avg_rw      = if_else(sum(R_B) > 0,
                                    sum(r_V_B * G_B) / sum(r_B * G_B),
                                    NA_real_),
      tau_B_avg_gw        = wmean(tau_B,        G_B),
      tau_S_avg_gw        = wmean(tau_S,        G_B),
      tau_B_avg_rw        = wmean(tau_B,        R_B),
      tau_S_avg_rw        = wmean(tau_S,        R_B),
      W_B_avg_gw          = wmean(W_B,          G_B),
      W_S_avg_gw          = wmean(W_S,          G_B),
      MC_B_avg_gw         = wmean(MC_B,         G_B),
      MC_S_avg_gw         = wmean(MC_S,         G_B),
      kappa_avg_gw        = wmean(kappa,        G_B),
      rate_channel    = sum(R_B * (rate_factor - 1)),
      lockin_channel  = sum(extra_R),
      R_planned_B_total = sum(R_planned_B),
      R_planned_S_total = sum(R_planned_S),
      planned_timing_shift_total = sum(planned_timing_shift),
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
           r_ordinary_B_avg_gw, r_ordinary_S_avg_gw,
           v_share_avg_rw,
           tau_B_avg_gw, tau_S_avg_gw, tau_B_avg_rw, tau_S_avg_rw,
           W_B_avg_gw, W_S_avg_gw, MC_B_avg_gw, MC_S_avg_gw, kappa_avg_gw,
           rate_channel, lockin_channel,
           R_planned_B_total, R_planned_S_total,
           planned_timing_shift_total, timing_clipped_cells,
           decedent_stock, inheritance_flow, deemed_realized,
           semi_elast_implied)

  yearly %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_summary.csv'))

  invisible(NULL)
}

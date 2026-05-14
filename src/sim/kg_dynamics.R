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
KG_DYN_SHARE_PLANNED    = 0.3041
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
KG_DYN_DEFAULT_PSI      = 26.3535

# Within-cell allocation rule for policy-induced dG, controlling the
# effective cell mortality m_eff used in the death/survivor channels.
#   "G" — dG allocated proportional to G_unit; m_eff = sum(w*m*G)/sum(w*G).
#         Inheritance-flow story.
#   "R" — dG allocated proportional to positive kg_lt; m_eff = sum(w*m*R)/sum(w*R).
#         Lock-in story. Falls back to "G" when R_B = 0.
# Only affects carryover/deemed; step-up is unchanged (death channel off).
KG_DYN_DG_ALLOCATION    = 'G'

KG_DYN_ASSET_CLASSES    = c('equities', 'pass_throughs',
                            'primary_home', 'other_home', 're_fund')
KG_DYN_ASSET_VALUE_COLS = paste0('value.', KG_DYN_ASSET_CLASSES)
KG_DYN_ASSET_BASIS_COLS = paste0('basis.', KG_DYN_ASSET_CLASSES)
KG_DYN_ASSET_GAIN_COLS  = paste0('gain.',  KG_DYN_ASSET_CLASSES)

# Trustees Report Alternative 2, 50/50 male/female blend (cohort module is
# gender-blind). Supplies the 81+ tail of the Bellman extended grid.
KG_DYN_LIFE_TABLE_M_PATH = './resources/PerLifeTables_M_Alt2_TR2024.csv'
KG_DYN_LIFE_TABLE_F_PATH = './resources/PerLifeTables_F_Alt2_TR2024.csv'


# Per-asset death-regime codes. The YAML carries one
# pref.kg_death_regime_{class} per asset class; kg_dyn_build_regime_mix
# resolves them at the cell level via gain-stock-weighted averaging.
# c_phi(a,t) (death-state burden share the holder internalizes) is the
# share of cell gain stock taxed at death given the regime mix, theta on
# carryover-routed shares, and the cell-level §121 utilization aggregate
# G_B_primary_above_cap / G_B_primary on primary_home dollars.
KG_DYN_REGIME_TRIPLET = list(
  '0' = list(vanish = 1, route = 0, realize = 0),  # step_up
  '1' = list(vanish = 0, route = 1, realize = 0),  # carryover
  '2' = list(vanish = 0, route = 0, realize = 1)   # deemed_realization
)



#-------------------------------------------------------------------------------
# Record-level helpers
#-------------------------------------------------------------------------------

kg_dyn_attach_record_attrs = function(tax_units) {

  # Adds per-record columns the bathtub recurrence and applier need:
  #   gain.{class}            : per-asset unrealized gain, max(0, value_k - basis_k)
  #   G_unit                  : sum over asset classes of gain.{class}
  #   gain.primary_home_above_cap : pmax(0, gain.primary_home -
  #                             pref.kg_sec121_excl); the §121-net primary-home
  #                             gain that would be taxable at deemed realization
  #   m_household             : q_death1 * q_death2 for joint filers; q_death1
  #                             otherwise
  #   age_cohort              : max(age1, age2) for joint, age1 otherwise;
  #                             clipped to [KG_DYN_AGE_MIN, KG_DYN_AGE_MAX]
  #
  # Requires tax_units to carry pref.kg_sec121_excl per record (filing-status
  # mapped). load_bathtub_inputs joins it in for the bathtub pass; the
  # simulator runtime already has it on tax_units from the tax_law merge.

  if (!('pref.kg_sec121_excl' %in% names(tax_units))) {
    stop('kg_dyn_attach_record_attrs: tax_units missing column ',
         '`pref.kg_sec121_excl`. Merge it in via filing_status before ',
         'calling this helper.')
  }

  values = as.matrix(tax_units[, KG_DYN_ASSET_VALUE_COLS])
  basis  = as.matrix(tax_units[, KG_DYN_ASSET_BASIS_COLS])
  diffs  = values - basis
  diffs[is.na(diffs)] = 0
  diffs[diffs < 0]    = 0
  colnames(diffs) = KG_DYN_ASSET_GAIN_COLS

  gain_primary = diffs[, 'gain.primary_home']
  sec121       = as.numeric(tax_units$`pref.kg_sec121_excl`)
  sec121[is.na(sec121)] = 0

  tax_units %>%
    bind_cols(as_tibble(diffs)) %>%
    mutate(
      G_unit                      = rowSums(diffs),
      gain.primary_home_above_cap = pmax(0, gain_primary - sec121),
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

  # Weight-aggregates per-record gain stocks, kg_lt, and m_household to age
  # cells. tax_units must already have the gain.{class} columns,
  # gain.primary_home_above_cap, G_unit, m_household, and age_cohort
  # attached by kg_dyn_attach_record_attrs.
  #
  # Returns per-cell: G_B (sum across assets), R_B, r_B, m, mG_record,
  # mR_record, per-asset G_B_{class}, and G_B_primary_above_cap (the
  # §121-net primary-home stock used in the Bellman's cell-level c_phi
  # when primary_home is in a deemed regime).
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
              G_B_equities          = sum(weight * gain.equities,          na.rm = TRUE),
              G_B_pass_throughs     = sum(weight * gain.pass_throughs,     na.rm = TRUE),
              G_B_primary_home      = sum(weight * gain.primary_home,      na.rm = TRUE),
              G_B_other_home        = sum(weight * gain.other_home,        na.rm = TRUE),
              G_B_re_fund           = sum(weight * gain.re_fund,           na.rm = TRUE),
              G_B_primary_above_cap = sum(weight * gain.primary_home_above_cap,
                                          na.rm = TRUE),
              .groups   = 'drop') %>%
    rename(age = age_cohort)

  zero_fill_cols = c('G_B', 'R_B', 'm_num', 'mG_record', 'mR_record', 'w_total',
                     'G_B_equities', 'G_B_pass_throughs', 'G_B_primary_home',
                     'G_B_other_home', 'G_B_re_fund', 'G_B_primary_above_cap')

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(all_of(zero_fill_cols), ~ if_else(is.na(.), 0, .)),
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
    select(age, G_B, R_B, r_B, m, mG_record, mR_record,
           G_B_equities, G_B_pass_throughs, G_B_primary_home,
           G_B_other_home, G_B_re_fund, G_B_primary_above_cap) %>%
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
    ext = tibble(age                   = ages_ext,
                 G_B                   = 0,
                 R_B                   = 0,
                 r_B                   = r_B_topcode,
                 m                     = as.numeric(life_ext[as.character(ages_ext), key]),
                 mG_record             = 0,
                 mR_record             = 0,
                 G_B_equities          = 0,
                 G_B_pass_throughs     = 0,
                 G_B_primary_home      = 0,
                 G_B_other_home        = 0,
                 G_B_re_fund           = 0,
                 G_B_primary_above_cap = 0)
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
                                     c_phi_col, psi, phi_I, beta,
                                     planned_share = KG_DYN_SHARE_PLANNED,
                                     kappa_col = NULL, stationary = FALSE) {

  # One age-backward sweep through [a_min, a_max] for a single year column.
  #
  # c_phi_col is a length-n_ages vector of cell-level burden shares
  # (built by kg_dyn_build_regime_mix on the bathtub grid then extended to
  # the Bellman grid in kg_dyn_run_bathtub_pass by repeating the age-80
  # value forward — same pattern as tau_col).
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
  F_vec        = (1 - c_phi_col) * tau_col
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



kg_dyn_solve_bellman = function(grid_packed, tau_mat, c_phi_mat,
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
  # bucket. c_phi_mat is typically all-zero under current-law step-up.
  #
  # When kappa_mat is supplied: Pass 2 (scenario). Solves the clipped
  # quadratic FOC r_D = clip((kappa - MC)/psi, 0, 1 - r_exog_B).
  #
  # c_phi_mat is an [n_ages, n_years] matrix of cell-level burden shares
  # produced by kg_dyn_build_regime_mix on the bathtub grid, then extended
  # to the Bellman grid by repeating the age-80 value forward (same
  # treatment as tau_mat). Scalars are accepted for unit tests and
  # broadcast to a constant matrix.
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

  if (length(c_phi_mat) == 1) {
    c_phi_mat = matrix(c_phi_mat, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(c_phi_mat), c(n_ages, n_years)))

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
      c_phi_col     = c_phi_mat[, j],
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
                                  r_S_vec, delta_route_vec,
                                  phi_I = KG_DYN_PHI_I) {

  # One-step bathtub recurrence for delta_G on the [18, 80] grid. r_S_vec
  # combines the fixed, Bellman ordinary, and retimed planned buckets.
  # delta_route_vec is a length-n_ages cell-level share of the dying stock
  # that routes to heirs (carryover); under per-asset regime mixing it's
  # produced by kg_dyn_build_regime_mix as sum_k share_k(a) * route_k.
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

  # Inheritance flow (spec §3.3.1). delta_route_vec is per-cell so a cell
  # whose regime mix has no carryover share contributes nothing to the
  # routing crossprod even when adjacent cells do.
  if (any(delta_route_vec > 0)) {
    decedent_stock = m_eff * (G_B + delta_prev)
    delta_inh      = as.numeric(crossprod(omega, delta_route_vec * decedent_stock))
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
# Cell-level regime mix (per-asset codes → per-age vanish/route/realize + c_phi)
#-------------------------------------------------------------------------------

kg_dyn_build_regime_mix = function(regime_codes, theta, baseline_t,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Aggregates per-asset regime codes into cell-level multipliers via
  # gain-stock-weighted shares:
  #   share_k(a)              = G_B_k(a) / G_B(a)
  #   share_primary_above_cap = G_B_primary_above_cap(a) / G_B(a)
  #   delta_{vanish,route,realize}(a) = sum_k share_k(a) * triplet_k$*
  #
  # c_phi(a) (share of cell gain stock taxed at death, the death-state
  # burden share the holder internalizes in the Bellman):
  #   c_phi(a) = sum_{k ≠ primary_home, deemed} share_k(a)
  #            + share_primary_above_cap(a)               (deemed + §121)
  #            + theta * sum_{k, carryover} share_k(a)    (route internalized)
  #
  # regime_codes : named list of 5 integer codes (one per asset class).
  # theta        : scalar bequest motive in [0, 1].
  # baseline_t   : per-cell tibble with G_B, G_B_{class}, G_B_primary_above_cap.
  #
  # Returns tibble keyed by age with delta_vanish, delta_route, delta_realize,
  # c_phi — each on the bathtub grid [18, 80].

  asset_classes = KG_DYN_ASSET_CLASSES

  missing = setdiff(asset_classes, names(regime_codes))
  if (length(missing) > 0) {
    stop('kg_dyn_build_regime_mix: regime_codes missing asset classes: ',
         paste(missing, collapse = ', '))
  }

  resolve_triplet = function(code) {
    t = KG_DYN_REGIME_TRIPLET[[as.character(code)]]
    if (is.null(t)) {
      stop('kg_dyn_build_regime_mix: unknown regime code ', code,
           ' (expected 0=step_up, 1=carryover, 2=deemed_realization)')
    }
    t
  }
  triplets = lapply(asset_classes, function(k) resolve_triplet(regime_codes[[k]]))
  names(triplets) = asset_classes

  G_B = baseline_t$G_B
  safe_share = function(num) if_else(G_B > 0, num / G_B, 0)

  share = lapply(asset_classes,
                 function(k) safe_share(baseline_t[[paste0('G_B_', k)]]))
  names(share) = asset_classes
  share_primary_above_cap = safe_share(baseline_t$G_B_primary_above_cap)

  n_cells       = length(G_B)
  delta_vanish  = rep(0, n_cells)
  delta_route   = rep(0, n_cells)
  delta_realize = rep(0, n_cells)
  c_phi         = rep(0, n_cells)

  for (k in asset_classes) {
    tr = triplets[[k]]
    delta_vanish  = delta_vanish  + share[[k]] * tr$vanish
    delta_route   = delta_route   + share[[k]] * tr$route
    delta_realize = delta_realize + share[[k]] * tr$realize

    # Carryover internalization: holder values theta of the routed stock
    c_phi = c_phi + theta * tr$route * share[[k]]

    # Deemed realization: full asset share for non-primary, §121-net share
    # for primary_home.
    if (k == 'primary_home') {
      c_phi = c_phi + tr$realize * share_primary_above_cap
    } else {
      c_phi = c_phi + tr$realize * share[[k]]
    }
  }

  tibble(
    age           = baseline_t$age,
    delta_vanish  = delta_vanish,
    delta_route   = delta_route,
    delta_realize = delta_realize,
    c_phi         = pmin(pmax(c_phi, 0), 1)
  )
}



#-------------------------------------------------------------------------------
# Per-record applier (pure allocator). Reads the precomputed cell_table from
# the bathtub state file and translates cell-level quantities into per-record
# kg_lt adjustments via three channels (spec §7.3):
#   rate     : kg_lt > 0 → kg_lt * rate_factor (= r_S/r_B, clamped to 1)
#   lock-in  : extra_R = r_S * dG, allocated by positive-kg_lt share if
#              R_B > 0, else by G_unit share, else skip
#   deemed   : asset-aware. For each asset class k:
#                contribution_k = realize_k * gain_k_i        (k ≠ primary)
#                contribution_primary = realize_primary *
#                                       pmax(0, gain_primary_i - sec121_i)
#              Summed then multiplied by m_household * (G_B + dG)/G_B
#              (deemed_factor). realize_k comes from regime$realize, the
#              year-level per-asset deemed indicators from the regime mix.
# Also stamps decedent_flag = (u < m_household) using precomputed uniform
# draws from globals$random_numbers (same draw across scenarios).
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, realize_by_asset,
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

  missing = setdiff(KG_DYN_ASSET_CLASSES, names(realize_by_asset))
  if (length(missing) > 0) {
    stop('kg_dyn_apply_to_records: realize_by_asset missing asset classes: ',
         paste(missing, collapse = ', '))
  }

  # Asset-aware deemed contribution per record. primary_home uses the
  # §121-net gain (precomputed in kg_dyn_attach_record_attrs); all other
  # asset classes use their full gain stock.
  deemed_per_record =
      realize_by_asset[['equities']]      * tax_units$gain.equities +
      realize_by_asset[['pass_throughs']] * tax_units$gain.pass_throughs +
      realize_by_asset[['primary_home']]  *
        tax_units$gain.primary_home_above_cap +
      realize_by_asset[['other_home']]    * tax_units$gain.other_home +
      realize_by_asset[['re_fund']]       * tax_units$gain.re_fund

  tax_units %>%
    mutate(
      decedent_flag = as.integer(decedent_random < m_household),
      allocation = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      kg_lt = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt) +
              extra_R * allocation +
              decedent_flag * deemed_factor * deemed_per_record
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



kg_dyn_load_bathtub_inputs = function(scenario_info, tax_law, baseline_root,
                                       sample_ids, pct_sample,
                                       ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Single Tax-Data pass producing baseline_cells (per-year G_B, R_B, r_B, m,
  # mG_record, mR_record, per-asset G_B_{class}, G_B_primary_above_cap over
  # ages 18-80), baseline_tau, and reform_tau (per-year R-weighted
  # mtr_kg_lt vectors). Cell aggregates come straight from Tax-Data csvs
  # (the wealth value.*/basis.* and q_death* columns live only there);
  # mtr_kg_lt comes from each side's static detail.
  #
  # tax_law is consumed only to merge the filing-status-mapped §121 cap
  # (pref.kg_sec121_excl) onto records before kg_dyn_attach_record_attrs
  # computes gain.primary_home_above_cap.

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

    sec121_t = tax_law %>%
      filter(year == t) %>%
      select(filing_status, `pref.kg_sec121_excl`) %>%
      distinct()

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample) %>%
      left_join(sec121_t, by = 'filing_status') %>%
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
                                    regime_mix,
                                    planned_diag = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Assembles per-cell quantities the applier needs:
  #   rate_factor   = r_S / r_B           (clamped to 1 when r_B = 0)
  #   extra_R       = r_S * dG            (lock-in stock realized at r_S)
  #   deemed_factor = (G_B + dG) / G_B    (clamped >= 0)
  # Plus diagnostic columns used by kg_dyn_build_summary: per-asset
  # G_B_{class}, G_B_primary_above_cap, cell-level regime-mix outputs
  # (delta_vanish/route/realize, c_phi). Bellman matrices are sliced from
  # the extended grid to the bathtub grid [18, 80] before persisting.

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

  mix_lookup = regime_mix
  mix_lookup$age = as.character(mix_lookup$age)

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
           delta_vanish  = mix_lookup$delta_vanish [match(as.character(age), mix_lookup$age)],
           delta_route   = mix_lookup$delta_route  [match(as.character(age), mix_lookup$age)],
           delta_realize = mix_lookup$delta_realize[match(as.character(age), mix_lookup$age)],
           c_phi         = mix_lookup$c_phi        [match(as.character(age), mix_lookup$age)],
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
           G_B_equities, G_B_pass_throughs, G_B_primary_home,
           G_B_other_home, G_B_re_fund, G_B_primary_above_cap,
           delta_vanish, delta_route, delta_realize, c_phi,
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

  # Step 3: baseline Bellman pass (c_phi = 0 across the whole grid under
  # current-law step-up — every asset gets step-up forgiveness)
  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               psi = psi, phi_I = phi_I,
                               planned_share = planned_share,
                               beta_by_year = beta_by_year)

  # Step 4: resolve year-by-year per-asset regime codes, build cell-level
  # regime mix (c_phi, delta_vanish/route/realize), and pack the Bellman
  # c_phi matrix on the extended grid.
  ages_bathtub_chr = as.character(ages_bathtub)
  ages_ext_chr     = as.character(setdiff(ages_bellman, ages_bathtub))
  ages_bellman_chr = as.character(ages_bellman)

  regime_list = vector('list', length(years))
  mix_list    = vector('list', length(years))
  c_phi_S_mat = matrix(0, length(ages_bellman), length(years),
                       dimnames = list(ages_bellman_chr, as.character(years)))

  for (j in seq_along(years)) {
    tlt = tax_law %>% filter(year == years[j])
    if (nrow(tlt) == 0) {
      stop('kg_dynamics: tax_law has no rows for year ', years[j])
    }
    tlt_row = tlt %>% slice(1)

    regime_codes = list(
      equities      = as.numeric(tlt_row$`pref.kg_death_regime_equities`),
      pass_throughs = as.numeric(tlt_row$`pref.kg_death_regime_pass_throughs`),
      primary_home  = as.numeric(tlt_row$`pref.kg_death_regime_primary_home`),
      other_home    = as.numeric(tlt_row$`pref.kg_death_regime_other_home`),
      re_fund       = as.numeric(tlt_row$`pref.kg_death_regime_re_fund`)
    )
    theta = as.numeric(tlt_row$`pref.kg_bequest_motive`)

    sec121_by_fs = tlt %>%
      select(filing_status, `pref.kg_sec121_excl`) %>%
      distinct()
    sec121_single = sec121_by_fs %>%
      filter(filing_status == 1) %>% pull(`pref.kg_sec121_excl`)
    sec121_married = sec121_by_fs %>%
      filter(filing_status == 2) %>% pull(`pref.kg_sec121_excl`)
    if (length(sec121_single)  == 0) sec121_single  = NA_real_
    if (length(sec121_married) == 0) sec121_married = NA_real_

    bt  = baseline_cells[[as.character(years[j])]]
    mix = kg_dyn_build_regime_mix(regime_codes, theta, bt, ages_bathtub)

    # Per-asset realize indicators (year-level scalars from the regime codes)
    realize_by_asset = lapply(KG_DYN_ASSET_CLASSES, function(k) {
      KG_DYN_REGIME_TRIPLET[[as.character(regime_codes[[k]])]]$realize
    })
    names(realize_by_asset) = KG_DYN_ASSET_CLASSES

    regime_list[[j]] = list(
      codes               = regime_codes,
      theta               = theta,
      sec121_excl_single  = sec121_single[1],
      sec121_excl_married = sec121_married[1],
      realize             = realize_by_asset
    )
    mix_list[[j]] = mix

    # Pack c_phi onto the extended Bellman grid: bathtub values from the
    # mix, age-80 value repeated forward across [81, 119] (same pattern as
    # tau_mat in kg_dyn_pack_tau).
    c_phi_bt_named = setNames(mix$c_phi, as.character(mix$age))
    c_phi_S_mat[ages_bathtub_chr, j] = c_phi_bt_named[ages_bathtub_chr]
    if (length(ages_ext_chr) > 0) {
      c_phi_S_mat[ages_ext_chr, j] =
        c_phi_bt_named[as.character(KG_DYN_AGE_MAX)]
    }
  }

  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_mat, c_phi_mat = c_phi_S_mat,
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
    mix    = mix_list[[j]]

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
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = mix$delta_route,
      phi_I           = phi_I
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
      regime_mix   = mix,
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

  # Long-format age profile: stamp the per-year regime codes onto every
  # cell row for diagnostic convenience (cell_table itself carries the
  # cell-level c_phi / delta_* mix).
  age_profile = bind_rows(lapply(seq_along(years), function(i) {
    s = states[[i]]
    codes = s$regime$codes
    s$cell_table %>%
      mutate(year                          = years[i],
             regime_equities               = codes$equities,
             regime_pass_throughs          = codes$pass_throughs,
             regime_primary_home           = codes$primary_home,
             regime_other_home             = codes$other_home,
             regime_re_fund                = codes$re_fund,
             theta                         = s$regime$theta,
             sec121_excl_single            = s$regime$sec121_excl_single,
             sec121_excl_married           = s$regime$sec121_excl_married) %>%
      relocate(year, age)
  }))

  age_profile %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_age_profile.csv'))

  # Year-level regime metadata table (per-asset codes + theta + §121 cap).
  regime_df = bind_rows(lapply(seq_along(years), function(i) {
    r = states[[i]]$regime
    tibble(year                 = years[i],
           regime_equities      = r$codes$equities,
           regime_pass_throughs = r$codes$pass_throughs,
           regime_primary_home  = r$codes$primary_home,
           regime_other_home    = r$codes$other_home,
           regime_re_fund       = r$codes$re_fund,
           theta                = r$theta,
           sec121_excl_single   = r$sec121_excl_single,
           sec121_excl_married  = r$sec121_excl_married)
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
      G_B_equities_total          = sum(G_B_equities),
      G_B_pass_throughs_total     = sum(G_B_pass_throughs),
      G_B_primary_home_total      = sum(G_B_primary_home),
      G_B_other_home_total        = sum(G_B_other_home),
      G_B_re_fund_total           = sum(G_B_re_fund),
      G_B_primary_above_cap_total = sum(G_B_primary_above_cap),
      m_avg_gw            = wmean(m,            G_B),
      r_B_avg_gw          = wmean(r_B,          G_B, default = 0),
      r_S_avg_gw          = wmean(r_S,          G_B, default = 0),
      c_phi_avg_gw        = wmean(c_phi,         G_B, default = 0),
      delta_vanish_avg_gw  = wmean(delta_vanish,  G_B, default = 0),
      delta_route_avg_gw   = wmean(delta_route,   G_B, default = 0),
      delta_realize_avg_gw = wmean(delta_realize, G_B, default = 0),
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
      decedent_stock      = sum(mG_record * deemed_factor),
      inheritance_flow    = sum(delta_route   * mG_record * deemed_factor),
      deemed_realized     = sum(delta_realize * mG_record * deemed_factor),
      .groups = 'drop'
    ) %>%
    left_join(regime_df, by = 'year') %>%
    mutate(
      R_S_total          = R_B_total + rate_channel + lockin_channel,
      dtau               = tau_S_avg_rw - tau_B_avg_rw,
      semi_elast_implied = if_else(R_B_total > 0 & R_S_total > 0 &
                                     abs(dtau) > 1e-10,
                                   log(R_S_total / R_B_total) / dtau,
                                   NA_real_)
    ) %>%
    select(year,
           regime_equities, regime_pass_throughs, regime_primary_home,
           regime_other_home, regime_re_fund,
           theta, sec121_excl_single, sec121_excl_married,
           c_phi_avg_gw,
           delta_vanish_avg_gw, delta_route_avg_gw, delta_realize_avg_gw,
           G_B_total, R_B_total, R_S_total, dG_total,
           G_B_equities_total, G_B_pass_throughs_total,
           G_B_primary_home_total, G_B_other_home_total,
           G_B_re_fund_total, G_B_primary_above_cap_total,
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

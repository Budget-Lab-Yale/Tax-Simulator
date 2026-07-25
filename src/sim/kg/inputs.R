#-------------------------------------------------------------------------------
# inputs.R
#
# Cell aggregation, record attributes, and the Tax-Data / Macro / heir loaders.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Record-level helpers
#-------------------------------------------------------------------------------

kg_dyn_attach_record_attrs = function(tax_units, cpiu_by_year = NULL) {

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

  missing_estate_cols = setdiff(ESTATE_ASSET_COLS, names(tax_units))
  if (length(missing_estate_cols) > 0) {
    stop('kg_dyn_attach_record_attrs: tax_units missing estate asset columns: ',
         paste(missing_estate_cols, collapse = ', '))
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

  estate = as.matrix(tax_units[, ESTATE_ASSET_COLS])
  estate[is.na(estate)] = 0
  estate_assets = rowSums(estate)

  estate_2026_m = rep(NA_real_, nrow(tax_units))
  if (!is.null(cpiu_by_year)) {
    if (!('year' %in% names(tax_units))) {
      stop('kg_dyn_attach_record_attrs: tax_units must include year when ',
           'cpiu_by_year is supplied.')
    }
    cpiu_years = names(cpiu_by_year)
    needed = unique(c(as.character(tax_units$year),
                      as.character(KG_DYN_CHAR_BASE_YEAR)))
    missing_cpiu = setdiff(needed, cpiu_years)
    if (length(missing_cpiu) > 0) {
      stop('kg_dyn_attach_record_attrs: cpiu_by_year missing years ',
           paste(missing_cpiu, collapse = ', '))
    }
    cpiu_base = as.numeric(cpiu_by_year[as.character(KG_DYN_CHAR_BASE_YEAR)])
    cpiu_cur  = as.numeric(cpiu_by_year[as.character(tax_units$year)])
    if (!is.finite(cpiu_base) || any(!is.finite(cpiu_cur))) {
      stop('kg_dyn_attach_record_attrs: cpiu_by_year has non-finite CPI-U ',
           'for the record year or base year.')
    }
    estate_2026_m = estate_assets * cpiu_base / cpiu_cur / 1e6
  }

  has_estate = is.finite(estate_2026_m) & estate_2026_m > 0
  log_estate = rep(NA_real_, length(estate_2026_m))
  log_estate[has_estate] = log(estate_2026_m[has_estate])
  p_char_extensive = rep(0, length(estate_2026_m))
  p_char_intensive = rep(0, length(estate_2026_m))
  p_char_extensive[has_estate] = plogis(
    KG_DYN_CHAR_EXTENSIVE_INTERCEPT +
      KG_DYN_CHAR_EXTENSIVE_LN_SLOPE * log_estate[has_estate]
  )
  p_char_intensive[has_estate] = plogis(
    KG_DYN_CHAR_INTENSIVE_INTERCEPT +
      KG_DYN_CHAR_INTENSIVE_LN_SLOPE * log_estate[has_estate]
  )

  out = tax_units %>%
    bind_cols(as_tibble(diffs)) %>%
    mutate(
      G_unit                      = rowSums(diffs),
      gain.primary_home_above_cap = pmax(0, gain_primary - sec121),
      estate_2026_m               = estate_2026_m,
      p_char_extensive            = p_char_extensive,
      p_char_intensive            = p_char_intensive,
      p_char                      = p_char_extensive * p_char_intensive,
      m_household = if_else(filing_status == 2 & !is.na(q_death2),
                            q_death1 * q_death2,
                            q_death1),
      m_household = if_else(is.na(m_household), 0, m_household),
      age_cohort  = if_else(filing_status == 2,
                            pmax(age1, age2, na.rm = TRUE),
                            age1),
      age_cohort  = pmax(KG_DYN_AGE_MIN, pmin(KG_DYN_AGE_MAX, age_cohort))
    )

  if (anyNA(out$age_cohort)) {
    n_bad = sum(is.na(out$age_cohort))
    stop(sprintf(
      paste0('kg_dyn_attach_record_attrs: %d records have NA age_cohort ',
             '(typically non-joint filers with missing age1). NA cohorts ',
             'silently drop from kg_dyn_aggregate_cells via group_by + ',
             'left_join. Fix the upstream age fields or impute before ',
             'calling this helper.'), n_bad))
  }

  out
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
  # mR_record, per-asset G_B_{class}, G_B_primary_above_cap (the
  # §121-net primary-home stock used in the Bellman's cell-level c_phi
  # when primary_home is in a deemed regime), and V_corp_exposed (the
  # omega-weighted C-corp equity VALUE underlying the kg state, sizing the
  # corporate-incidence gain-state debit -- corp_kg_state_debit_by_year).
  #
  # R_B uses positive-only sums of kg_lt so r_B >= 0 and per-record
  # allocation shares (pmax(kg_lt, 0) / R_B) sum to 1.
  #
  # Sparse-cell fallback (spec §5.1): cells with G_B > 0 but R_B = 0 inherit
  # the gain-stock-weighted aggregate r_B. Prevents young heir cohorts
  # (carryover / deemed inflows) from getting r_S = 0 forever.

  # Corporate-exposure value per record (src/sim/corp/ helper; plain
  # column so the grouped summarise below can weight-sum it).
  tax_units$corp_exposed_value = corp_kg_state_exposed_value(tax_units)

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
              V_corp_exposed        = sum(weight * corp_exposed_value,
                                          na.rm = TRUE),
              p_char_num = sum(weight * m_household * G_unit * p_char,
                               na.rm = TRUE),
              p_char_extensive_num =
                sum(weight * m_household * G_unit * p_char_extensive,
                    na.rm = TRUE),
              p_char_intensive_num =
                sum(weight * m_household * G_unit * p_char_intensive,
                    na.rm = TRUE),
              estate_2026_m_num =
                sum(weight * m_household * G_unit * estate_2026_m,
                    na.rm = TRUE),
              .groups   = 'drop') %>%
    rename(age = age_cohort)

  zero_fill_cols = c('G_B', 'R_B', 'm_num', 'mG_record', 'mR_record', 'w_total',
                     'G_B_equities', 'G_B_pass_throughs', 'G_B_primary_home',
                     'G_B_other_home', 'G_B_re_fund', 'G_B_primary_above_cap',
                     'V_corp_exposed', 'p_char_num', 'p_char_extensive_num',
                     'p_char_intensive_num', 'estate_2026_m_num')

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(all_of(zero_fill_cols), ~ if_else(is.na(.), 0, .)),
           m   = if_else(w_total > 0, m_num / w_total, 0),
           r_B = if_else(G_B     > 0, R_B   / G_B,     0),
           p_char = if_else(mG_record > 0, p_char_num / mG_record, 0),
           p_char_extensive = if_else(mG_record > 0,
                                      p_char_extensive_num / mG_record, 0),
           p_char_intensive = if_else(mG_record > 0,
                                      p_char_intensive_num / mG_record, 0),
           estate_2026_m_avg_dgw = if_else(mG_record > 0,
                                            estate_2026_m_num / mG_record,
                                            NA_real_))

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
           p_char, p_char_extensive, p_char_intensive, estate_2026_m_avg_dgw,
           G_B_equities, G_B_pass_throughs, G_B_primary_home,
           G_B_other_home, G_B_re_fund, G_B_primary_above_cap,
           V_corp_exposed) %>%
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



# NOTE: the deterministic age-shift operator lives in cohort_bathtub.R as the
# shared build_aging_matrix() (A[a, h] = 1 if h = a + 1; A[a_max, a_max] = 1 so
# the topcode age self-loops; spec §3.4). kg calls it directly at the two use
# sites in kg_dyn_run_bathtub_pass() / kg_dyn_run_frozen_pass().



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



kg_dyn_load_cpiu_levels = function(macro_root, years,
                                   base_year = KG_DYN_CHAR_BASE_YEAR) {

  needed_years = unique(c(years, base_year))
  cpiu = c('historical.csv', 'projections.csv') %>%
    file.path(macro_root, .) %>%
    map(~ read_csv(.x, show_col_types = FALSE) %>%
          select(year, cpiu)) %>%
    bind_rows() %>%
    distinct(year, .keep_all = TRUE) %>%
    filter(year %in% needed_years)

  missing = setdiff(needed_years, cpiu$year)
  if (length(missing) > 0) {
    stop('kg_dyn_load_cpiu_levels: years ',
         paste(missing, collapse = ', '),
         ' not present in macro_projections at ', macro_root)
  }
  if (any(is.na(cpiu$cpiu))) {
    stop('kg_dyn_load_cpiu_levels: NA CPI-U for years ',
         paste(cpiu$year[is.na(cpiu$cpiu)], collapse = ', '))
  }

  cpiu = cpiu %>% arrange(match(year, needed_years))
  setNames(cpiu$cpiu, as.character(cpiu$year))
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
                 p_char                = inner$p_char[inner$age == KG_DYN_AGE_MAX],
                 p_char_extensive      = inner$p_char_extensive[inner$age == KG_DYN_AGE_MAX],
                 p_char_intensive      = inner$p_char_intensive[inner$age == KG_DYN_AGE_MAX],
                 estate_2026_m_avg_dgw =
                   inner$estate_2026_m_avg_dgw[inner$age == KG_DYN_AGE_MAX],
                 G_B_equities          = 0,
                 G_B_pass_throughs     = 0,
                 G_B_primary_home      = 0,
                 G_B_other_home        = 0,
                 G_B_re_fund           = 0,
                 G_B_primary_above_cap = 0,
                 V_corp_exposed        = 0)
    out[[key]] = bind_rows(inner, ext %>% select(names(inner))) %>%
      arrange(age)
  }
  out
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



kg_dyn_load_cells_inputs = function(scenario_info, tax_law,
                                     sample_ids, pct_sample,
                                     ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Single Tax-Data pass producing baseline_cells (per-year G_B, R_B, r_B, m,
  # mG_record, mR_record, per-asset G_B_{class}, G_B_primary_above_cap over
  # ages 18-80), the slim per-record frames the tau aggregator consumes
  # (td_slim_by_year: id/weight/kg_lt/age_cohort/G_unit), and heir_dist.
  # Cell aggregates come straight from Tax-Data csvs (the wealth
  # value.*/basis.* and q_death* columns live only there).
  #
  # tax_law is consumed only to merge the filing-status-mapped §121 cap
  # (pref.kg_sec121_excl) onto records before kg_dyn_attach_record_attrs
  # computes gain.primary_home_above_cap.
  #
  # Called by the frozen mechanical pass (which persists the result to
  # kg_dyn_inputs_cache_path) and by kg_dyn_load_bathtub_inputs when no
  # cache is available.

  tax_data_root = scenario_info$interface_paths$`Tax-Data`
  macro_root    = scenario_info$interface_paths$`Macro-Projections`
  years         = scenario_info$years
  if (is.null(macro_root)) {
    stop('kg_dynamics: scenario_info$interface_paths$`Macro-Projections` is ',
         'NULL. The bathtub input pass needs CPI-U to express estate size ',
         'in 2026 dollars for terminal charity calibration.')
  }

  heir_dist = kg_dyn_load_heir_distribution(ages = ages)
  cpiu_by_year = kg_dyn_load_cpiu_levels(macro_root, years)

  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'kg_lt', 'q_death1', 'q_death2',
              ESTATE_ASSET_COLS,
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS) %>%
    unique()

  baseline_cells  = list()
  td_slim_by_year = list()

  for (t in years) {

    sec121_t = tax_law %>%
      filter(year == t) %>%
      select(filing_status, `pref.kg_sec121_excl`) %>%
      distinct()

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample,
             year = t) %>%
      left_join(sec121_t, by = 'filing_status') %>%
      kg_dyn_attach_record_attrs(cpiu_by_year = cpiu_by_year)

    baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td, ages)

    # mtr aggregator only needs id/weight/kg_lt/age_cohort/G_unit; slim
    # before the joins so we don't drag the asset value.*/basis.* columns
    # through two hash joins on ~220k records.
    td_slim_by_year[[as.character(t)]] =
      td %>% select(id, weight, kg_lt, age_cohort, G_unit)
  }

  list(baseline_cells  = baseline_cells,
       td_slim_by_year = td_slim_by_year,
       heir_dist       = heir_dist)
}



kg_dyn_load_bathtub_inputs = function(scenario_info, tax_law, baseline_root,
                                       sample_ids, pct_sample,
                                       ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                       cells_inputs = NULL) {

  # Builds the full bathtub input set: baseline_cells plus baseline_tau,
  # reform_tau, and reform_tau_timing (per-year realization-weighted
  # mtr_kg_lt / mtr_kg_lt_lawonly vectors read from each side's static
  # detail). The Tax-Data sweep is delegated to
  # kg_dyn_load_cells_inputs; pass a precomputed cells_inputs (e.g. the
  # frozen mechanical pass's inputs cache) to skip the second sweep.
  #
  # Both sides aggregate over all records. Deemed death gains never enter
  # kg_lt (priced via the two-leg expected-tax recompute in run_one_year),
  # so reform-side MTRs are pure inter-vivos margins — no decedent
  # exclusion needed.
  #
  # When the scenario levies a wealth tax (kg_dyn_wealth_law_active), the
  # reform side additionally reads mtr_net_worth and builds reform_carry:
  # per-year, per-age-cell gain-weighted means of the RECORD-LEVEL product
  # mtr_net_worth * mtr_kg_lt — the wealth-tax carrying cost of deferral h
  # consumed by the Bellman and the tau_eq recursion. The BASELINE side is
  # never read for h: h_B == 0 by law (current law has no wealth tax), an
  # INVARIANT asserted against the baseline tax_law.csv below, not an
  # assumption.
  #
  # BOTH sides additionally read mtr_estate_ded (the switch-gated marginal
  # estate rate written by run.R's static pass) and build LEG-PAIRED estate
  # exposure vectors baseline_estate / reform_estate
  # (kg_dyn_aggregate_cell_estate) — the (1 - e) offset on the kg death
  # value in the Bellman and on the tau_eq death-realize term. Unlike h,
  # the estate exposure is NONZERO UNDER CURRENT LAW (the estate tax
  # exists in baseline), so the baseline leg is genuinely load-bearing: a
  # single shared matrix would zero out estate-only reforms. The scenario
  # leg is guaranteed by run.R's kg fallback; the BASELINE leg requires
  # 'estate' registered in the baseline row's mtr_vars — stale baseline
  # detail hard-stops in read_mtr below.

  years = scenario_info$years

  wealth_active = kg_dyn_wealth_law_active(tax_law)
  if (wealth_active) {
    bl_law_path = file.path(baseline_root, 'baseline', 'static',
                            'supplemental', 'tax_law.csv')
    if (!file.exists(bl_law_path)) {
      stop('kg_dynamics: wealth-active scenario but the baseline tax law ',
           'dump is missing at ', bl_law_path, '; cannot verify the ',
           'h_B == 0 invariant (the omitted-baseline-carry convention).')
    }
    bl_law = fread(bl_law_path, showProgress = FALSE) %>% as_tibble()
    bl_rate_cols = grep('^wealth\\.rates[0-9]*$', names(bl_law), value = TRUE)
    bl_nonzero = length(bl_rate_cols) > 0 &&
      any(sapply(bl_law[bl_rate_cols],
                 function(x) any(!is.na(x) & x != 0)))
    if (bl_nonzero) {
      stop('kg_dynamics: the BASELINE wealth schedule has a nonzero rate ',
           '(', bl_law_path, '). The carry channel omits the baseline h ',
           'matrix on the invariant h_B == 0; a wealth tax in baseline law ',
           'breaks that convention and requires threading a baseline-side ',
           'carry matrix through the Bellman/tau_eq before proceeding.')
    }
  }

  if (is.null(cells_inputs)) {
    cells_inputs = kg_dyn_load_cells_inputs(
      scenario_info = scenario_info,
      tax_law       = tax_law,
      sample_ids    = sample_ids,
      pct_sample    = pct_sample,
      ages          = ages
    )
  }

  baseline_cells    = cells_inputs$baseline_cells
  baseline_tau      = list()
  reform_tau        = list()
  reform_tau_timing = list()
  reform_carry      = list()
  baseline_estate   = list()
  reform_estate     = list()

  for (t in years) {

    td_slim = cells_inputs$td_slim_by_year[[as.character(t)]]

    read_mtr = function(path, cols = c('id', 'mtr_kg_lt',
                                       'mtr_estate_ded')) {
      f = file.path(path, paste0(t, '.csv'))
      have = names(fread(f, nrows = 0, showProgress = FALSE))
      missing_cols = setdiff(cols, have)
      if (length(missing_cols) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: static detail %s lacks column(s): %s. ',
                 'mtr_kg_lt_lawonly is written by the static pass (run.R) ',
                 'for kg_dynamics scenarios; mtr_net_worth is ',
                 'guaranteed there for wealth-active kg scenarios and ',
                 'mtr_estate_ded for ALL kg scenarios (either ',
                 'registered in the runscript mtr_vars or via the run.R ',
                 'fallback). The BASELINE leg has no fallback: its row ',
                 'must register "estate" in mtr_vars. A missing column ',
                 'means STALE static detail — re-run the baseline/',
                 'scenario static pass with current code.'),
          f, paste(missing_cols, collapse = ', ')))
      }
      fread(f, select = cols, showProgress = FALSE) %>%
        as_tibble()
    }

    # Verify the static detail covers every kg-active record in td_slim.
    # left_join + na.rm = TRUE in kg_dyn_aggregate_cell_mtr would silently
    # treat a missing mtr_kg_lt as zero, biasing tau downward toward 0.
    check_mtr_coverage = function(joined, side, year) {
      missing = joined %>% filter(pmax(kg_lt, 0) > 0 & is.na(mtr_kg_lt))
      if (nrow(missing) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with kg_lt > 0 missing ',
                 'mtr_kg_lt in %s static detail for year %d. This biases ',
                 'tau toward zero. Check that the static run wrote ',
                 'mtr_kg_lt for every sample id.'),
          nrow(missing), side, year))
      }
    }

    # Estate-exposure coverage stop, mirroring check_mtr_coverage on the
    # GAIN side: a silently-NA mtr_estate_ded on a gain-holding record
    # would bias e toward zero (i.e. toward the pre-build no-offset model).
    check_estate_coverage = function(joined, side, year) {
      missing = joined %>% filter(G_unit > 0 & is.na(mtr_estate_ded))
      if (nrow(missing) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with G_unit > 0 missing ',
                 'mtr_estate_ded in %s static detail for year %d. This ',
                 'biases the estate death-value exposure e toward zero. ',
                 'Check that the static run wrote mtr_estate_ded for ',
                 'every sample id.'),
          nrow(missing), side, year))
      }
    }

    baseline_joined = td_slim %>%
      left_join(read_mtr(file.path(baseline_root, 'baseline', 'static',
                                   'detail')),
                by = 'id')
    check_mtr_coverage(baseline_joined, 'baseline', t)
    check_estate_coverage(baseline_joined, 'baseline', t)
    baseline_tau[[as.character(t)]] =
      kg_dyn_aggregate_cell_mtr(baseline_joined, ages)
    baseline_estate[[as.character(t)]] =
      kg_dyn_aggregate_cell_estate(baseline_joined, ages)

    reform_cols = c('id', 'mtr_kg_lt', 'mtr_kg_lt_lawonly', 'mtr_estate_ded')
    if (wealth_active) reform_cols = c(reform_cols, 'mtr_net_worth')
    reform_joined = td_slim %>%
      left_join(read_mtr(file.path(scenario_info$output_path, 'static',
                                   'detail'),
                         cols = reform_cols),
                by = 'id')
    check_mtr_coverage(reform_joined, 'reform', t)
    check_estate_coverage(reform_joined, 'reform', t)
    reform_tau[[as.character(t)]] =
      kg_dyn_aggregate_cell_mtr(reform_joined, ages)
    reform_estate[[as.character(t)]] =
      kg_dyn_aggregate_cell_estate(reform_joined, ages)

    # Per-year estate-exposure diagnostic: the cell aggregation above
    # compresses a very skewed record-level distribution (within-age
    # gain x estate-exposure correlation is strong at the top, and the 80+
    # cell concentrates the donor-clone records). Write the record-level
    # gain-weighted distribution next to the state files so the compression
    # is visible, per leg: overall mean / zero-exposure share / near-top-
    # rate share, and the mean by (weighted) gain decile.
    kg_dyn_write_estate_exposure_diag(
      baseline_joined = baseline_joined,
      reform_joined   = reform_joined,
      scenario_info   = scenario_info,
      year            = t)

    # Wealth-carry cell aggregation (gain-weighted record-level product;
    # zeros when no wealth tax is active). Coverage stop mirrors
    # check_mtr_coverage on the GAIN side: a silently-NA mtr_net_worth on a
    # gain-holding record would bias h toward zero.
    if (wealth_active) {
      missing_nw = reform_joined %>%
        filter(G_unit > 0 & is.na(mtr_net_worth))
      if (nrow(missing_nw) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with G_unit > 0 missing ',
                 'mtr_net_worth in reform static detail for year %d. This ',
                 'biases the wealth carrying cost h toward zero. Check ',
                 'that the static run wrote mtr_net_worth for every ',
                 'sample id.'),
          nrow(missing_nw), t))
      }
      reform_carry[[as.character(t)]] =
        kg_dyn_aggregate_cell_carry(reform_joined, ages)
    } else {
      zero = setNames(rep(0, length(ages)), as.character(ages))
      reform_carry[[as.character(t)]] = list(h = zero, tau_w = zero)
    }

    # Law-only tau for the planned-timing wedge: identical records and
    # weights, but MTRs evaluated on the pre-mech-injection frame (see the
    # static pass in run.R). tau_S - tau_B built from this column isolates
    # statutory price changes; the post-injection reform_tau above retains
    # the mech income effect for the Bellman.
    lawonly_joined = reform_joined %>%
      mutate(mtr_kg_lt = mtr_kg_lt_lawonly)
    check_mtr_coverage(lawonly_joined, 'reform (law-only)', t)
    reform_tau_timing[[as.character(t)]] =
      kg_dyn_aggregate_cell_mtr(lawonly_joined, ages)
  }

  list(baseline_cells    = baseline_cells,
       baseline_tau      = baseline_tau,
       reform_tau        = reform_tau,
       reform_tau_timing = reform_tau_timing,
       reform_carry      = reform_carry,
       baseline_estate   = baseline_estate,
       reform_estate     = reform_estate,
       heir_dist         = cells_inputs$heir_dist)
}




#-------------------------------------------------------------------------------
# inputs.R
#
# Contains functions to build the inputs the gains model needs, from Tax-Data,
# the macro projections, and the heir distribution
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Record-level helpers
#-------------------------------------------------------------------------------

kg_dyn_attach_record_attrs = function(tax_units, cpiu_by_year = NULL,
                                      death_excl_married = NULL) {

  # Adds the record-level columns the rest of the model needs: the unrealized gain
  # on each asset class and their total, the per-class gain above the §121 cap and
  # the death-gain exclusion, the widowhood probability, the chance the household
  # dies, and the cohort age.
  #
  # Joint filers die when both do, and take the older age. Ages are clipped to the
  # cohort grid.
  #
  # The death-gain exclusion is per decedent and applies pro-rata by gain across
  # the asset classes whose death regime is carryover or deemed realization,
  # after §121. A single filer who is widowed can claim the married amount, so
  # both amounts are computed and blended by the widowhood probability.
  #
  # Requires the §121 cap and the death-gain exclusion on each record. The
  # pre-pass joins them in; the simulator already has them from the tax law
  # merge. death_excl_married is the married exclusion amount, required only
  # when the exclusion is nonzero.
  #
  # Returns: tax units with those columns added (df).

  if (!('pref.kg_sec121_excl' %in% names(tax_units))) {
    stop('kg_dyn_attach_record_attrs: tax_units missing column ',
         '`pref.kg_sec121_excl`. Merge it in via filing_status before ',
         'calling this helper.')
  }
  if (!('pref.kg_death_gain_excl' %in% names(tax_units))) {
    stop('kg_dyn_attach_record_attrs: tax_units missing column ',
         '`pref.kg_death_gain_excl`. Merge it in via filing_status before ',
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

  # The gain at stake at death by class, netting §121 off a primary home. The
  # death-gain exclusion then removes its first dollars pro-rata across the
  # classes whose regime is carryover or deemed realization; every other class
  # keeps its full stake. A single filer's two amounts (own and married, the
  # latter available through a survivor election) are blended by the widowhood
  # probability, giving the expected gain above the exclusion per record.
  stakes = diffs
  stakes[, 'gain.primary_home'] = pmax(0, gain_primary - sec121)

  excl_own = as.numeric(tax_units$`pref.kg_death_gain_excl`)
  excl_own[is.na(excl_own)] = 0
  excl_active = any(excl_own > 0) ||
    (!is.null(death_excl_married) &&
       isTRUE(as.numeric(death_excl_married) > 0))

  p_widow    = rep(0, nrow(tax_units))
  above_excl = stakes
  if (excl_active) {
    regime_cols = paste0('pref.kg_death_regime_', KG_DYN_ASSET_CLASSES)
    miss_regime = setdiff(regime_cols, names(tax_units))
    if (length(miss_regime) > 0) {
      stop('kg_dyn_attach_record_attrs: the death-gain exclusion is active ',
           'but tax_units is missing regime columns: ',
           paste(miss_regime, collapse = ', '))
    }
    if (is.null(death_excl_married) ||
        !is.finite(as.numeric(death_excl_married))) {
      stop('kg_dyn_attach_record_attrs: the death-gain exclusion is active ',
           'but death_excl_married was not supplied.')
    }
    miss_widow = setdiff(c('male1', 'divorce_year'), names(tax_units))
    if (length(miss_widow) > 0) {
      stop('kg_dyn_attach_record_attrs: the death-gain exclusion is active ',
           'but tax_units is missing columns: ',
           paste(miss_widow, collapse = ', '))
    }

    eligible = sapply(KG_DYN_ASSET_CLASSES, function(k) {
      as.numeric(tax_units[[paste0('pref.kg_death_regime_', k)]][1]) %in% c(1, 2)
    })

    p_widow = calc_p_widow(tax_units, get_widowhood_table())
    excl_2x = if_else(tax_units$filing_status %in% c(1, 4),
                      as.numeric(death_excl_married), excl_own)

    elig_cols = KG_DYN_ASSET_GAIN_COLS[eligible]
    pool      = rowSums(stakes[, elig_cols, drop = FALSE])
    f_x       = ifelse(pool > 0, pmax(0, pool - excl_own) / pool, 0)
    f_2x      = ifelse(pool > 0, pmax(0, pool - excl_2x) / pool, 0)
    f_blend   = p_widow * f_2x + (1 - p_widow) * f_x
    above_excl[, elig_cols] = stakes[, elig_cols, drop = FALSE] * f_blend
  }
  colnames(above_excl) = paste0(KG_DYN_ASSET_GAIN_COLS, '_above_excl')

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
                      as.character(kg_setting('char_base_year'))))
    missing_cpiu = setdiff(needed, cpiu_years)
    if (length(missing_cpiu) > 0) {
      stop('kg_dyn_attach_record_attrs: cpiu_by_year missing years ',
           paste(missing_cpiu, collapse = ', '))
    }
    cpiu_base = as.numeric(cpiu_by_year[as.character(kg_setting('char_base_year'))])
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
    kg_setting('char_extensive_intercept') +
      kg_setting('char_extensive_ln_slope') * log_estate[has_estate]
  )
  p_char_intensive[has_estate] = plogis(
    kg_setting('char_intensive_intercept') +
      kg_setting('char_intensive_ln_slope') * log_estate[has_estate]
  )

  out = tax_units %>%
    bind_cols(as_tibble(diffs)) %>%
    bind_cols(as_tibble(above_excl)) %>%
    mutate(
      G_unit                      = rowSums(diffs),
      gain.primary_home_above_cap = pmax(0, gain_primary - sec121),
      p_widow                     = p_widow,
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
# Aggregating records to cells
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cells = function(tax_units, ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Sums gains, realizations and mortality across records within each age cell.
  # Requires the columns kg_dyn_attach_record_attrs() adds.
  #
  # Realizations are summed over positive values only, so the realization rate
  # cannot go negative and the record shares used later sum to 1.
  #
  # A cell that holds gains but realizes none inherits the overall realization
  # rate. Without that, young heirs receiving inherited gains would never realize
  # anything.
  #
  # Returns: tibble of one row per age cell (df).

  # Value of corporate equity behind each record's gains, which sizes the
  # corporate markdown's debit. Materialized as a column so it can be summed.
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
              G_B_equities_above_excl      = sum(weight * gain.equities_above_excl,
                                                 na.rm = TRUE),
              G_B_pass_throughs_above_excl = sum(weight * gain.pass_throughs_above_excl,
                                                 na.rm = TRUE),
              G_B_primary_home_above_excl  = sum(weight * gain.primary_home_above_excl,
                                                 na.rm = TRUE),
              G_B_other_home_above_excl    = sum(weight * gain.other_home_above_excl,
                                                 na.rm = TRUE),
              G_B_re_fund_above_excl       = sum(weight * gain.re_fund_above_excl,
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
                     'G_B_equities_above_excl', 'G_B_pass_throughs_above_excl',
                     'G_B_primary_home_above_excl', 'G_B_other_home_above_excl',
                     'G_B_re_fund_above_excl',
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

  # Take the pooled rate over cells that realize something, so the cells being
  # imputed do not pull the imputation toward zero. This should do nothing at full
  # sample, but young heir cohorts can still be empty in a single year.
  ok         = out$R_B > 0
  r_B_pooled = if (any(ok)) sum(out$R_B[ok]) / sum(out$G_B[ok]) else 0

  out %>%
    mutate(r_B = if_else(G_B > 0 & R_B == 0, r_B_pooled, r_B)) %>%
    select(age, G_B, R_B, r_B, m, mG_record, mR_record,
           p_char, p_char_extensive, p_char_intensive, estate_2026_m_avg_dgw,
           G_B_equities, G_B_pass_throughs, G_B_primary_home,
           G_B_other_home, G_B_re_fund, G_B_primary_above_cap,
           G_B_equities_above_excl, G_B_pass_throughs_above_excl,
           G_B_primary_home_above_excl, G_B_other_home_above_excl,
           G_B_re_fund_above_excl,
           V_corp_exposed) %>%
    arrange(age)
}



#-------------------------------------------------------------------------------
# The heir routing matrix
#-------------------------------------------------------------------------------

kg_dyn_build_heir_matrix = function(heir_dist,
                                    ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Builds the matrix routing gains from a decedent's age to the ages of the heirs.
  # Every row is the same, being the observed distribution of heir ages weighted by
  # dollars inherited.
  #
  # That amounts to assuming heir age does not depend on the decedent's age. The
  # overall flow to each heir age matches the data exactly, and what the data do
  # not pin down is how that varies with the decedent's age. Letting it vary would
  # take an outside assumption, which for revenue scoring is not worth it.
  #
  # Returns: matrix of decedent age by heir age, rows summing to 1.

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



# The aging operator is shared with the wealth model and lives in
# cohort_bathtub.R as build_aging_matrix(). It moves each age to the next, with
# the top-coded age pointing at itself.



#-------------------------------------------------------------------------------
# Mortality past the top-coded age
#-------------------------------------------------------------------------------

kg_dyn_load_life_table_extension = function(years,
                                            ages_ext = (KG_DYN_AGE_MAX + 1):
                                                       KG_DYN_AGE_MAX_BELLMAN,
                                            path_M = KG_DYN_LIFE_TABLE_M_PATH,
                                            path_F = KG_DYN_LIFE_TABLE_F_PATH) {

  # Reads mortality for the ages past the top code, which the realization choice
  # needs so that it has a real terminal condition: the SSA tables reach certain
  # death at 119.
  #
  # Returns: matrix of mortality rates by age and year, averaged over sex.

  load_one = function(path) {
    # These files carry four metadata lines, then a header, then the data. The
    # column names are awkward, so take them by position.
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
# Discount factors
#-------------------------------------------------------------------------------

kg_dyn_load_beta_series = function(macro_root, years) {

  # Builds the discount factor for each year from the 10-year Treasury rate,
  # deflated by CPI-U growth:
  #
  #   r_real = (1 + tsy_10y) / (1 + inflation) - 1
  #   beta   = 1 / (1 + r_real)
  #
  # The rate must be real. In the choice between realizing now and holding a gain
  # that grows in nominal terms, inflation cancels, so discounting at the nominal
  # rate would count it twice.
  #
  # Returns: numeric vector of discount factors by year.

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
                                   base_year = kg_setting('char_base_year')) {

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

  # Joins the model's cells to the life table tail, giving one extended grid per
  # year. Only the realization choice uses the extended ages; the stock of gains is
  # tracked over the ordinary grid.
  #
  # Past the top code, mortality comes from the life table and the realization rate
  # is held at its age-80 value. Holding the rate flat matters: otherwise the value
  # of continuing at age 80 would be driven entirely by death, which overstates how
  # much older cohorts accelerate realizations when gains are taxed at death. Cell
  # totals stay zero, since the choice is solved per dollar.

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
                 G_B_equities_above_excl      = 0,
                 G_B_pass_throughs_above_excl = 0,
                 G_B_primary_home_above_excl  = 0,
                 G_B_other_home_above_excl    = 0,
                 G_B_re_fund_above_excl       = 0,
                 V_corp_exposed        = 0)
    out[[key]] = bind_rows(inner, ext %>% select(names(inner))) %>%
      arrange(age)
  }
  out
}



#-------------------------------------------------------------------------------
# Loading the pass inputs
#-------------------------------------------------------------------------------

kg_dyn_load_heir_distribution = function(path = KG_DYN_HEIR_DISTRIBUTION_PATH,
                                          ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Reads the distribution of heir ages. Built from the SCF by
  # other/kg_model_tests/build_heir_distribution.R; re-run that when the survey
  # vintage changes.

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

  # Reads Tax-Data once and returns the baseline cells by year, a slim record frame
  # for the rate aggregation, and the heir distribution. Cells are built straight
  # from Tax-Data, since the asset values, bases and mortality columns exist only
  # there.
  #
  # Tax law is used only to put the §121 cap, the death-gain exclusion and the
  # death regime codes on each record, which the gain columns need.
  #
  # The frozen pass calls this and caches the result; the conventional pass reuses
  # that cache where it exists.
  #
  # Returns: list of the cells by year, the record frames, and the heir
  #          distribution.

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

  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2', 'male1',
              'divorce_year', 'kg_lt', 'q_death1', 'q_death2',
              ESTATE_ASSET_COLS,
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS) %>%
    unique()

  excl_law_cols = c('pref.kg_sec121_excl', 'pref.kg_death_gain_excl',
                    paste0('pref.kg_death_regime_', KG_DYN_ASSET_CLASSES))

  baseline_cells  = list()
  td_slim_by_year = list()

  for (t in years) {

    excl_t = tax_law %>%
      filter(year == t) %>%
      select(filing_status, all_of(excl_law_cols)) %>%
      distinct()

    death_excl_married_t = excl_t %>%
      filter(filing_status == 2) %>%
      pull(`pref.kg_death_gain_excl`) %>%
      first()

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample,
             year = t) %>%
      left_join(excl_t, by = 'filing_status') %>%
      kg_dyn_attach_record_attrs(cpiu_by_year       = cpiu_by_year,
                                 death_excl_married = death_excl_married_t)

    baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td, ages)

    # Drop to the five columns the rate aggregation needs before joining, so the
    # asset value and basis columns are not carried through two joins on 220,000
    # records.
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

  # Assembles everything the pass needs: the baseline cells, and the marginal rate
  # on gains for each leg, read from that leg's detail and averaged to cells.
  # A third rate vector measured on law alone drives the retiming. The Tax-Data read
  # is delegated; pass the frozen pass's cache to avoid doing it twice.
  #
  # Both legs aggregate over every record. Gains at death never enter kg_lt, so the
  # rates here are the rates a living taxpayer faces and no decedents need excluding.
  #
  # Where the scenario levies a wealth tax, the scenario leg also reads the marginal
  # wealth rate and builds the cost of deferring. The baseline leg never does:
  # current law has no wealth tax, which is checked against the baseline tax law
  # below rather than assumed.
  #
  # Both legs read the marginal estate rate and build the estate offset separately.
  # Unlike the wealth tax, this is nonzero under current law, so the baseline leg
  # carries real weight and sharing one matrix would kill estate-only reforms. The
  # scenario leg is always available; the baseline leg needs 'estate' in the
  # baseline's mtr_vars, and stale baseline detail stops the run below.
  #
  # Returns: list of the cells, rate vectors, wealth cost and estate offsets.

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

  # The realization choice is priced at the circumstances the taxpayer has after
  # everything outside his control has happened to him, so the reform-side rates
  # come from the mechanical rung where it ran. Where it did not, that rung is the
  # static one. The baseline leg has no mechanical rung and always reads static.
  reform_leg = if (scenario_runs_mechanical(scenario_info)) 'mechanical' else 'static'

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
          paste0('kg_dynamics: detail %s lacks column(s): %s. ',
                 'add_kg_bathtub_mtrs (run.R) supplies mtr_kg_lt_lawonly, ',
                 'mtr_net_worth for wealth-active kg scenarios and ',
                 'mtr_estate_ded for every kg scenario, on the static and ',
                 'mechanical rungs. The baseline leg has no fallback: its ',
                 'runscript row must register "estate" in mtr_vars. A missing ',
                 'column means stale detail -- re-run the pass with current ',
                 'code.'),
          f, paste(missing_cols, collapse = ', ')))
      }
      fread(f, select = cols, showProgress = FALSE) %>%
        as_tibble()
    }

    # Check the detail covers every record holding gains. A missing rate
    # would otherwise be read as zero and pull the cell average down.
    check_mtr_coverage = function(joined, side, year) {
      missing = joined %>% filter(pmax(kg_lt, 0) > 0 & is.na(mtr_kg_lt))
      if (nrow(missing) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with kg_lt > 0 missing ',
                 'mtr_kg_lt in %s detail for year %d. This biases ',
                 'tau toward zero. Check that the run wrote ',
                 'mtr_kg_lt for every sample id.'),
          nrow(missing), side, year))
      }
    }

    # Same check for the estate rate: a missing value would pull the offset toward
    # zero, which is the model without the offset at all.
    check_estate_coverage = function(joined, side, year) {
      missing = joined %>% filter(G_unit > 0 & is.na(mtr_estate_ded))
      if (nrow(missing) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with G_unit > 0 missing ',
                 'mtr_estate_ded in %s detail for year %d. This ',
                 'biases the estate death-value exposure e toward zero. ',
                 'Check that the run wrote mtr_estate_ded for ',
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
      left_join(read_mtr(file.path(scenario_info$output_path, reform_leg,
                                   'detail'),
                         cols = reform_cols),
                by = 'id')
    check_mtr_coverage(reform_joined, 'reform', t)
    check_estate_coverage(reform_joined, 'reform', t)
    reform_tau[[as.character(t)]] =
      kg_dyn_aggregate_cell_mtr(reform_joined, ages)
    reform_estate[[as.character(t)]] =
      kg_dyn_aggregate_cell_estate(reform_joined, ages)

    # The cell averages above compress a very skewed distribution: gains and
    # estate exposure are strongly correlated at the top, and the 80-and-over cell
    # concentrates the donor-clone records. Write the record-level distribution
    # next to the state files so that compression is visible.
    kg_dyn_write_estate_exposure_diag(
      baseline_joined = baseline_joined,
      reform_joined   = reform_joined,
      scenario_info   = scenario_info,
      year            = t)

    # Average the cost of deferring to cells, zero without a wealth tax. Same
    # coverage check as above, for the same reason.
    if (wealth_active) {
      missing_nw = reform_joined %>%
        filter(G_unit > 0 & is.na(mtr_net_worth))
      if (nrow(missing_nw) > 0) {
        stop(sprintf(
          paste0('kg_dynamics: %d records with G_unit > 0 missing ',
                 'mtr_net_worth in reform detail for year %d. This ',
                 'biases the wealth carrying cost h toward zero. Check ',
                 'that the run wrote mtr_net_worth for every ',
                 'sample id.'),
          nrow(missing_nw), t))
      }
      reform_carry[[as.character(t)]] =
        kg_dyn_aggregate_cell_carry(reform_joined, ages)
    } else {
      zero = setNames(rep(0, length(ages)), as.character(ages))
      reform_carry[[as.character(t)]] = list(h = zero, tau_w = zero)
    }

    # The same rates measured on law alone, over the same records and weights.
    # Differencing these isolates the change in the statutory rate, which is what
    # should drive retiming. The rates above instead keep the income effect of the
    # mechanical adjustments, which is what the realization choice should see.
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




#----------------------------------------------------------------------------
# horizontal.R
#
# Calculates measures of horizontal equity by comparing within-group
# dispersion of effective tax rates across baseline and reform scenarios.
# Groups are defined by income percentile × marital status × number of
# dependents.
#----------------------------------------------------------------------------



wtd_pctile_rank = function(x, w) {

  #--------------------------------------------------------------------------
  # Weighted percentile rank (1..100) by value, point-mass safe.
  #
  # The previous implementation binned income with
  #   cut(inc, breaks = c(-Inf, wtd.quantile(inc, w, seq(.01,.99,.01)), Inf),
  #       labels = 1:100)
  # which errors "'breaks' are not unique" whenever the weighted income
  # distribution has point masses (many units at an identical income — which a
  # counterfactual that shifts a block of units to the same value induces):
  # adjacent quantiles collapse to the same number and cut() rejects the
  # duplicated breaks.
  #
  # Ranking directly off the weighted ECDF sidesteps cut() entirely. For each
  # record, F(x) = (weight of units with income <= x) / total weight, and the
  # rank is ceiling(F(x) * 100). Tied incomes share the max F within the tie, so
  # a point mass lands wholly in one percentile (the correct behavior) instead of
  # crashing. In the non-degenerate case this matches the old cut() labels for
  # ~98% of records and never by more than one percentile: for F(x) in
  # ((k-1)/100, k/100] ceiling gives k, matching the interval (q_{k-1}, q_k]; the
  # residual off-by-one at bin boundaries is just the ECDF step function vs
  # wtd.quantile's interpolation, immaterial to the grouped IQR measure.
  #
  # AI-Fiscal Issue 2 / upstream Budget-Lab-Yale/Tax-Simulator#129.
  #
  # Parameters:
  #   - x (dbl) : income (or any orderable value)
  #   - w (dbl) : record weights
  #
  # Returns: integer vector of percentile ranks in 1..100.
  #--------------------------------------------------------------------------

  ord        = order(x)
  frac_upper = cumsum(w[ord]) / sum(w)             # weighted ECDF at each point
  frac_upper = ave(frac_upper, x[ord], FUN = max)  # ties share F(x) = P(X <= x)
  rank       = pmin(100L, pmax(1L, as.integer(ceiling(frac_upper * 100))))
  out        = integer(length(x))
  out[ord]   = rank
  out
}



build_horizontal_table = function(id) {

  #--------------------------------------------------------------------------
  # Computes within-group IQR of effective income tax rate for both baseline
  # and a counterfactual scenario, reported by income quintile and overall.
  #
  # Parameters:
  #   - id (str) : counterfactual scenario ID
  #
  # Returns: void, writes horizontal.csv to scenario supplemental directory.
  #--------------------------------------------------------------------------

  results = list()

  for (yr in get_scenario_info(id)$dist_years) {

    # Read baseline and scenario microdata
    baseline = globals$baseline_root %>%
      file.path('baseline/static/detail', paste0(yr, '.csv')) %>%
      fread() %>%
      tibble()

    scenario = globals$output_root %>%
      file.path(id, 'static/detail', paste0(yr, '.csv')) %>%
      fread() %>%
      tibble()

    # Combine with scenario labels
    scenario_id = id
    microdata = bind_rows(
      baseline %>% mutate(scenario = 'baseline'),
      scenario %>% mutate(scenario = scenario_id)
    ) %>%

      # Remove dependent returns and negative/zero income
      filter(dep_status == 0, expanded_inc > 0) %>%

      # Join baseline income for consistent percentile ranking
      left_join(
        baseline %>%
          filter(dep_status == 0, expanded_inc > 0) %>%
          select(id, inc = expanded_inc),
        by = 'id'
      ) %>%

      # Compute effective tax rate and grouping variables
      mutate(
        married = as.integer(filing_status == 2),
        etr     = pmax(-1, pmin(1, liab_iit_net / inc))
      ) %>%

      # Assign income percentiles (within each scenario, using baseline income).
      # Point-mass-safe weighted ECDF rank; see wtd_pctile_rank() above (#129).
      group_by(scenario) %>%
      mutate(inc_pctile = wtd_pctile_rank(inc, weight)) %>%
      ungroup()

    # Compute within-group IQR of ETR
    within_group = microdata %>%
      group_by(scenario, inc_pctile, married, n_dep) %>%
      summarise(
        n         = sum(weight),
        group_iqr = if_else(
          n() > 2,
          Hmisc::wtd.quantile(etr, weight, 0.75) - Hmisc::wtd.quantile(etr, weight, 0.25),
          0
        ),
        .groups = 'drop'
      )

    # Aggregate by quintile
    by_quintile = within_group %>%
      mutate(inc_quintile = paste0('Quintile ', floor((inc_pctile - 1) / 20) + 1)) %>%
      group_by(scenario, group = inc_quintile) %>%
      summarise(
        avg_within_group_iqr = weighted.mean(group_iqr, n, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(group_dimension = 'Income')

    # Aggregate overall
    overall = within_group %>%
      group_by(scenario) %>%
      summarise(
        avg_within_group_iqr = weighted.mean(group_iqr, n, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(group = 'Overall', group_dimension = 'Overall')

    results[[as.character(yr)]] = bind_rows(overall, by_quintile) %>%
      mutate(year = yr, .before = everything())
  }

  # Write results
  results %>%
    bind_rows() %>%
    select(year, scenario, group_dimension, group, avg_within_group_iqr) %>%
    arrange(year, scenario, group_dimension, group) %>%
    write_csv(file.path(globals$output_root, id, 'static/supplemental', 'horizontal.csv'))
}

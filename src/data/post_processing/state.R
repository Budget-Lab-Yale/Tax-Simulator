#-------------------------------------------------------------------------
# state.R
#
# Post-processing functions for state income tax results (plan §2.4)
#-------------------------------------------------------------------------


build_state_rev_est = function(id) {

  #----------------------------------------------------------------------------
  # Calculates state-level net individual fiscal estimates: deltas in tax less
  # standalone refundable transfers versus baseline, by state and year. No-op
  # when the scenario ran without state mode. Fails fast with a clear message
  # when the scenario has state totals but the baseline run does not (e.g., a
  # pre-existing baseline_vintage produced without the states column).
  #
  # Parameters:
  #   - id (str) : scenario ID
  #
  # Returns: void (writes supplemental/state_rev_est.csv per run type).
  #----------------------------------------------------------------------------

  baseline_path = file.path(globals$baseline_root,
                            'baseline/static/totals/state.csv')

  for (type in c('static', 'conventional')) {

    scenario_path = file.path(globals$output_root, id, type,
                              'totals', 'state.csv')
    if (!file.exists(scenario_path)) {
      next
    }

    if (!file.exists(baseline_path)) {
      stop("Scenario '", id, "' has state totals but the baseline run does ",
           'not. State deltas need baseline state liability: re-run the ',
           "baseline with the runscript 'states' column set (a pre-existing ",
           'baseline_vintage produced without state mode cannot be used)')
    }

    baseline = baseline_path %>%
      read_csv(show_col_types = F) %>%
      filter(variable == 'liab_st_individual_net') %>%
      select(year, state, baseline = value)

    scenario_path %>%
      read_csv(show_col_types = F) %>%
      filter(variable == 'liab_st_individual_net') %>%
      select(year, state, value) %>%
      left_join(baseline, by = c('year', 'state')) %>%
      mutate(delta = value - baseline) %>%
      select(state, year, delta) %>%
      pivot_wider(names_from  = year,
                  values_from = delta) %>%
      write_csv(file.path(globals$output_root, id, type,
                          'supplemental', 'state_rev_est.csv'))
  }
}



build_stacked_state_rev_est = function(counterfactual_ids) {

  #----------------------------------------------------------------------------
  # Calculates stacked state net individual fiscal deltas: incremental change
  # in tax less standalone refundable transfers by state and year, scenario-over-scenario in
  # runscript order (mirroring calc_stacked_rev_est()). Scenarios that ran
  # without state mode are excluded from the stack; each included scenario's
  # delta is measured against the previous INCLUDED scenario. No-op when the
  # baseline has no state totals.
  #
  # Parameters:
  #   - counterfactual_ids (str[]) : counterfactual scenario names, in
  #                                  runscript (stack) order
  #
  # Returns: void (writes stacked_state_rev_est_{static,conventional}.csv at
  #          the vintage root).
  #----------------------------------------------------------------------------

  if (length(counterfactual_ids) == 0) {
    return(invisible(NULL))
  }
  baseline_path = file.path(globals$baseline_root,
                            'baseline/static/totals/state.csv')
  if (!file.exists(baseline_path)) {
    return(invisible(NULL))
  }

  for (static in c(T, F)) {

    type = if_else(static, 'static', 'conventional')

    # Read state totals for baseline plus every counterfactual that has them,
    # preserving stack order
    stack_ids = c('baseline', counterfactual_ids)
    stacked = stack_ids %>%
      map(.f = ~ {
        path = if (.x == 'baseline') {
          baseline_path
        } else {
          file.path(globals$output_root, .x, type, 'totals', 'state.csv')
        }
        if (!file.exists(path)) {
          return(NULL)
        }
        path %>%
          read_csv(show_col_types = F) %>%
          filter(variable == 'liab_st_individual_net') %>%
          transmute(scenario_id = .x, year, state, value)
      }) %>%
      bind_rows()

    if (nrow(stacked) == 0) {
      next
    }

    # Incremental deltas in stack order within each state x year
    stacked %>%
      mutate(scenario_id = factor(scenario_id, levels = stack_ids)) %>%
      arrange(state, year, scenario_id) %>%
      group_by(state, year) %>%
      mutate(delta = value - lag(value)) %>%
      ungroup() %>%
      filter(scenario_id != 'baseline') %>%
      select(scenario_id, state, year, delta) %>%
      pivot_wider(names_from  = year,
                  values_from = delta) %>%
      arrange(scenario_id, state) %>%
      write_csv(file.path(globals$output_root,
                          paste0('stacked_state_rev_est_', type, '.csv')))
  }
}

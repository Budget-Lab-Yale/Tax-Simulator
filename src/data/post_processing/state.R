#-------------------------------------------------------------------------
# state.R
#
# Post-processing functions for state income tax results (plan §2.4)
#-------------------------------------------------------------------------


build_state_rev_est = function(id) {

  #----------------------------------------------------------------------------
  # Calculates state-level revenue estimates: deltas in state income tax
  # liability versus baseline, by state and year, for each run type. No-op
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
      filter(variable == 'liab_st_iit') %>%
      select(year, state, baseline = value)

    scenario_path %>%
      read_csv(show_col_types = F) %>%
      filter(variable == 'liab_st_iit') %>%
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

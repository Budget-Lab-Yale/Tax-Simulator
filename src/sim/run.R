#-----------
# TODO
#-----------



do_scenario = function(id, baseline_mtrs) {
  
  #----------------------------------------------------------------------------
  # Executes full simulation for a given scenario. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Get scenario info
  scenario_info = get_scenario_info(globals, id)
  
  
  #-----------------
  # Initialize data
  #-----------------
  
  # TODO other macro data
  
  # Get macro data
  indexes = generate_indexes(scenario_info)
  
  # Build tax law
  tax_law = build_tax_law(config_path = file.path(scenario_info$config_path, 'tax_law'),
                          years       = scenario_info$years,
                          indexes     = indexes)
  
  
  #----------------
  # Run simulation
  #----------------
  
  # Run static simulation
  static_mtrs = run_sim(scenario_info = scenario_info,
                        economy       = economy, 
                        tax_law       = tax_law, 
                        baseline_mtrs = NULL, 
                        static_mtrs   = NULL)
  
  # Run simulation with behavioral feedback 
  run_sim(scenario_info = scenario_info,
          economy       = economy, 
          tax_law       = tax_law, 
          baseline_mtrs = baseline_mtrs, 
          static_mtrs   = static_mtrs)
  
  # Return MTRs if running baseline
  if (id == 'baseline') {
    return(static_mtrs)
  }
  
}



run_sim = function(scenario_info, economy, tax_law, baseline_mtrs, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs simulation instance for a given scenario, either static or 
  # conventional. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Run simulation for all years
  output = scenario_info$years %>% 
    map(.f            = run_one_year,
        scenario_info = scenario_info, 
        baseline_mtrs = baseline_mtrs, 
        static_mtrs   = static_mtrs)
  
  # Calculate and write receipts
  output$totals %>% 
    bind_rows() %>% 
    calc_receipts() 
  
  # Return MTRs
  output$mtrs %>% 
    bind_rows() %>% 
    return()
}



run_one_year = function(year, scenario_info, baseline_mtrs, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Load tax unit data
  tax_units = read_puf(scenario_info, year)

  # Adjust for economic differences from baseline
  
  
  
  #---------------------------
  # Model behavioral feedback
  #---------------------------
  
  # Only simulate if we have the requisite information
  run_behavioral_feedback = !is.null(baseline_mtrs) & !is.null(static_mtrs) 
  if (run_behavioral_feedback) {
    
    # Calculate new values for specified variables
    updated_vars = tax_units %>% 
      left_join(baseline_mtrs, by = c('id', 'year')) %>%
      left_join(static_mtrs,   by = c('id', 'year')) %>% 
      do_behavioral_feedback(scenario_info$mtr_vars)
    
    # Update variables
    tax_units %<>% 
      select(-all_of(names(scenario_info$mtr_vars))) %>% 
      left_join(updated_vars, by = 'id')
  }
  
  
  #----------
  # Do taxes
  #----------
  
  # Calculate taxes
  tax_units %<>% 
    do_taxes(vars_1040    = vars_1040,
             vars_payroll = vars_payroll)
  
  # Calculate marginal tax rates
  mtrs = tax_units %>% 
    select(-all_of(c(vars_1040, vars_payroll))) %>%
    pmap(
      .f = calc_mtrs, 
      .l = list(alias = names(scenario_info$mtr_vars), 
                vars  = scenario_info$mtr_vars),
      tax_units     = (.),
      liab_baseline = tax_units$liab_pr + tax_units$liab_iit_net
    ) %>% 
    bind_cols() %>% 
    mutate(year = year) %>% 
    relocate(year)
    
  
  #-----------------
  # Post-processing
  #-----------------
  
  # Write microdata
  tax_units %>%  
    left_join(mtrs, by = 'id') %>% 
    write_csv(...)
  
  # Get totals from microdata
  totals = -1 # TODO
  
  # Return required data
  return(list(mtrs   = mtrs, 
              totals = totals))
} 

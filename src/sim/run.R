#-----------------------------------------------
# run.R
#
# Contains functions to execute full simulation
#-----------------------------------------------



do_scenario = function(id, baseline_mtrs) {
  
  #----------------------------------------------------------------------------
  # Executes full simulation for a given scenario. TODO
  # 
  # Parameters:
  #   - id (str)           : scenario ID
  #   - baseline_mtrs (df) : tibble of baseline MTRs indexed by year/tax unit 
  #                          ID; NULL if this scenario is the baseline or if 
  #                          no MTR variables were specified 
  #
  # Returns: tibble of baseline MTRs if this scenario is the baseline (df); 
  #          NULL otherwise.
  #----------------------------------------------------------------------------
  
  # Get scenario info
  scenario_info = get_scenario_info(globals, id)
  
  
  #-----------------
  # Initialize data
  #-----------------
  
  # TODO other macro data
  
  # Get macro data
  indexes = generate_indexes(scenario_info)
  
  # Build (and write) tax law
  tax_law = build_tax_law(scenario_info, indexes)
  
  
  #----------------
  # Run simulation
  #----------------
  
  # Run static simulation
  static_mtrs = run_sim(scenario_info = scenario_info,
                        economy       = economy, 
                        tax_law       = tax_law, 
                        static        = T,
                        baseline_mtrs = NULL, 
                        static_mtrs   = NULL)
  
  # Run simulation with behavioral feedback if modules are specified
  if (length(scenario_info$behavior_modules) > 0) {
    run_sim(scenario_info = scenario_info,
            economy       = economy, 
            tax_law       = tax_law, 
            static        = F,
            baseline_mtrs = baseline_mtrs, 
            static_mtrs   = static_mtrs)
  }
  
  # Return MTRs if running baseline
  if (id == 'baseline') {
    return(static_mtrs)
  }
  
}



run_sim = function(scenario_info, tax_law, static, baseline_mtrs, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs simulation instance for a given scenario, either static or 
  # conventional. TODO
  # 
  # Parameters:
  #   - scenario_info (list) : scenario info object; see get_scenario_info()
  #   - tax_law (df)         : tax law tibble; see build_tax_law()
  #   - static 
  #   - baseline_mtrs
  #   - static_mtrs
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Run simulation for all years
  output = scenario_info$years %>% 
    map(.f            = run_one_year,
        scenario_info = scenario_info, 
        static        = static,
        baseline_mtrs = baseline_mtrs, 
        static_mtrs   = static_mtrs)
  
  # Write totals files
  totals_pr = output$pr %>% 
    bind_rows() %>% 
    write_csv(file.path(scenario_info$output_path, 'totals', 'payroll.csv'))
  
  totals_1040 = output$`1040` %>% 
    bind_rows() %>% 
    write_csv(file.path(scenario_info$output_path, 'totals', '1040.csv'))
    
  # Calculate and write receipts
  totals_pr %>%  
    left_join(totals_1040, by = 'year')
    calc_receipts(scenario_info$output_path) 
  
  # Return MTRs
  output$mtrs %>% 
    bind_rows() %>% 
    return()
}



run_one_year = function(year, scenario_info, static, baseline_mtrs, static_mtrs) {
  
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

  # Adjust for economic differences from economic baseline
  # TODO
  
  
  #---------------------------
  # Model behavioral feedback
  #---------------------------
  
  # Only simulate for non-static (and by extension, non-baseline) runs
  if (!static) {
    tax_units %<>% 
      do_behavioral_feedback(scenario_info = scenario_info, 
                             baseline_mtrs = baseline_mtrs, 
                             static_mtrs   = static_mtrs)
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
      .l = list(name = names(scenario_info$mtr_vars), 
                vars = scenario_info$mtr_vars),
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
    write_csv(file.path(scenario_info$output_path, 'detail', paste0(year, '.csv')))
  
  # Get totals from microdata
  totals = list(pr     = get_pr_totals(tax_units), 
                `1040` = get_1040_totals(tax_units))
  
  # Return required data
  return(list(mtrs   = mtrs, 
              totals = totals))
} 

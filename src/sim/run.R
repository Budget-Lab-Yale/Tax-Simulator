#-----------
# TODO
#-----------



do_scenario = function(id) {
  
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
  
  
  #--------------------
  # Load economic data
  #--------------------
  
  
  # Get macro data
  indexes = generate_indexes(scenario_info)
  
  # 
  
  #---------------
  # Build tax law
  #---------------
  
  # Load tax law functions
  source('./src/misc/utils.R')
  source('./src/data/tax_law/tax_law.R')
  
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
                        static_mtrs   = NULL)
  
  # Run simulation with behavioral feedback 
  run_sim(scenario_info = scenario_info,
          economy       = economy, 
          tax_law       = tax_law, 
          static_mtrs   = static_mtrs)
  
  return(NULL)
  
}



run_sim = function(scenario_info, economy, tax_law, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs simulation instance for a given scenario, either static or 
  # conventional. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # TODO walk(scenario_info$years, run_one_year)
  
  # TODO 
  # calc_receipts(economy$totals)
  
  
}



run_one_year = function(year, scenario_info, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Load tax unit data
  # tax_units = read_puf(scenario_info, year)

  #---------------------------
  # Model behavioral feedback TODO
  #---------------------------
  
  # tax_units %<>% 
  #   do_behavioral_feedback(scenario_info, static_mtrs) 
  #     (which itself calls apply_elasticities())
  
  #----------
  # Do taxes
  #----------
  
  # Calculate taxes
  tax_units %<>%
    do_taxes(vars_1040    = vars_1040, 
             vars_payroll = vars_payroll)
  
  # Calculate marginal tax rates TODO make prettier
  tax_units %<>% 
    bind_cols(
      pmap(
        .f = calc_mtrs, 
        .l = list(alias = names(scenario_info$mtr_names), 
                  vars  = scenario_info$mtr_vars),
        tax_units = tax_units %>% 
                      select(-all_of(c(vars_1040, vars_payroll))),
        liab_baseline = tax_units$liab_pr + tax_units$liab_iit_net
      )
    )
  
  # TODO store MTRs somehow
  
  #-----------------
  # Post-processing
  #-----------------
  
  # Get totals TODO
  # totals %<>% 
  #  update_totals()
} 

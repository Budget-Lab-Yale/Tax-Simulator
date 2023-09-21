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
  
  
  #---------------
  # Build economy
  #---------------
  
  economy = list()
  
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

  # Do behavioral feedback TODO
  # economy$tax_units %<>% 
  #   do_behavioral_feedback(scenario_info) 
  #     (which itself calls apply_elasticities())
  
  # Do taxes TODO
  # economy$tax_units %<>%
  #   do_taxes()
  
  # Get totals TODO
  # economy$totals %<>% 
  #  update_totals()
} 

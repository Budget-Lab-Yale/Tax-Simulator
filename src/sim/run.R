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
  
  # Placeholder! Ugly! Reads historical inflation series for tax law generation
  economy$indexes = read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'CPIAUCNS.csv')) %>% 
    mutate(Year = year(DATE), Month = month(DATE)) %>% 
    select(Year, Month, cpi = CPIAUCNS) %>% 
    left_join(read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'SUUR0000SA0.csv')) %>% 
                mutate(Year = year(DATE), Month = month(DATE)) %>% 
                select(Year, Month, chained_cpi = SUUR0000SA0), 
              by = c('Year', 'Month')) %>% 
    mutate(FY = if_else(Month < 9, Year, Year + 1)) %>% 
    filter(FY > min(FY) + 1) %>% 
    select(-Year) %>% 
    pivot_longer(cols      = -c(FY, Month), 
                 names_to  = 'Series', 
                 values_to = 'Value') %>% 
    group_by(Series, Year = FY) %>% 
    summarise(Value = mean(Value)) %>% 
    bind_rows(read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'awi.csv'))) %>%
    group_by(Series) %>% 
    mutate(Growth = Value / lag(Value) - 1) %>% 
    select(-Value) %>% 
    filter(!is.na(Growth))
  
  
  #---------------
  # Build tax law
  #---------------
  
  # Load tax law functions
  source('./src/misc/utils.R')
  source('./src/data/tax_law/tax_law.R')
  
  # Build tax law
  tax_law = build_tax_law(config_path = file.path(scenario_info$config_path, 'tax_law'),
                          years       = scenario_info$years,
                          indexes     = economy$indexes)
  
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



run_one_year = function(scenario_info, static_mtrs) {
  
  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. TODO
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  

  # Do behavioral feedback TODO
  
  # Do taxes TODO
  
  # Get totals TODO
  
} 

#---------
# TODO
#---------


generate_indexes = function(scenario_info) {
  
  #----------------------------------------------------------------------------
  # Generates
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Placeholder! Ugly! Reads historical inflation series for tax law generation
  read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'CPIAUCNS.csv')) %>% 
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
    filter(!is.na(Growth)) %>% 
    return()
  
}


read_puf = function(scenario_info, year) { 
  
  #----------------------------------------------------------------------------
  # Loads tax microdata into memory for a given scenario-year
  # 
  # Parameters:
  #   - TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # TODO
  
}


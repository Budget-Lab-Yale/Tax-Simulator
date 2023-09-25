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
    mutate(year = year(DATE), month = month(DATE)) %>% 
    select(year, month, cpi = CPIAUCNS) %>% 
    left_join(read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'SUUR0000SA0.csv')) %>% 
                mutate(year = year(DATE), month = month(DATE)) %>% 
                select(year, month, chained_cpi = SUUR0000SA0), 
              by = c('year', 'month')) %>% 
    mutate(FY = if_else(month < 9, year, year + 1)) %>% 
    filter(FY > min(FY) + 1) %>% 
    select(-year) %>% 
    pivot_longer(cols      = -c(FY, month), 
                 names_to  = 'series', 
                 values_to = 'value') %>% 
    group_by(series, year = FY) %>% 
    summarise(value = mean(value)) %>% 
    bind_rows(read_csv(file.path(scenario_info$interface_paths$`Inflation-Data`, 'awi.csv'))) %>%
    group_by(series) %>% 
    mutate(growth = value / lag(value) - 1) %>% 
    select(-value) %>% 
    filter(!is.na(growth)) %>% 
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


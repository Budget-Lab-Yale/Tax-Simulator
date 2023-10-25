#---------
# TODO
#---------


generate_indexes = function(macro_root) {
  
  #----------------------------------------------------------------------------
  # Gets growth rates, both historical and projected for this economic 
  # scenario, for indexation variables. Currently limited to CPIU, Chained
  # CPIU, and Average Wage Index.
  # 
  # Parameters:
  #   - macro_root (str) : filepath for Macro-Projections scenario interface
  #
  # Returns: tibble of growth rates by series (df). 
  #----------------------------------------------------------------------------
  
  
  # Read and combine historical and projected macro data
  c('historical.csv', 'projections.csv') %>% 
    map(.f = ~ macro_root %>% 
          file.path(.x) %>% 
          read_csv(show_col_types = F)) %>% 
    bind_rows() %>% 
    
    # Select indexation variables and reshape long
    select(year, cpi = cpiu_irs, chained_cpi = ccpiu_irs, awi) %>% 
    pivot_longer(cols      = -year, 
                 names_to  = 'series', 
                 values_to = 'value') %>% 
    
    # Express in growth rates
    group_by(series) %>% 
    mutate(growth = value / lag(value) - 1) %>% 
    ungroup() %>% 
    select(series, year, growth) %>% 
    arrange(series, year) %>% 
    return()
}



read_microdata = function(root, year) { 
  
  #----------------------------------------------------------------------------
  # Loads tax microdata into memory for a given scenario-year
  # 
  # Parameters:
  #   - root (str) : filepath to scenario's microdata vintage
  #   - year (int) : year of microdata
  #
  # Returns: tibble of tax microdata (df).
  #----------------------------------------------------------------------------
  
  root %>% 
    file.path(paste0('tax_units_', year, '.csv')) %>% 
    read_csv(show_col_types = F) %>% 
    return()
}


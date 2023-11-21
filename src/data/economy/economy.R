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
    fread() %>%
    tibble() %>% 
    return()
}


initialize_nols = function(years) {
  
  #----------------------------------------------------------------------------
  # Initializes table to track policy-generated NOLs. 
  # 
  # Parameters:
  #   - years (int) : years for which to track generation of new NOLs.
  #
  # Returns: tibble with NOLs initialized to 0. (df).
  #----------------------------------------------------------------------------
  
  
  # Initialize table
  expand_grid(year_generated = years, 
              t              = 1:10) %>%
    mutate(year_claimed = year_generated + t) %>% 
    
    # Set distribution of claiming, 
    # from this paper https://www.journals.uchicago.edu/doi/epdf/10.17310/ntj.2018.4.04
    mutate(
      share_claimed = case_when(
        t == 1  ~ 0.13, 
        t == 2  ~ 0.08,
        t == 3  ~ 0.05, 
        t == 4  ~ 0.04, 
        t == 5  ~ 0.04,
        t == 6  ~ 0.03,
        t == 7  ~ 0.02,
        t == 8  ~ 0.02,
        t == 9  ~ 0.01, 
        t == 10 ~ 0.01
      ), 
      amount_claimed = 0
    ) %>% 
    select(-t) %>%
    return()
}


update_nols = function(nols, year, amount) {
  
  #----------------------------------------------------------------------------
  # Given an amount of new NOLs generated in a specific year, allocates NOLs
  # by year eventually claimed.
  # 
  # Parameters:
  #   - nols (df)    : NOLs table as per initialize_nols()
  #   - year (int)   : year new NOLs were generated
  #   - amount (dbl) : amount of NOLs generate in year
  #
  # Returns: NOLs table with updated values in amount_claimed column (df).
  #----------------------------------------------------------------------------
  
  
  nols %>% 
    left_join(
      tibble(year_generated = year, 
             amount         = amount), 
      by = 'year_generated') %>% 
    mutate(amount_claimed = amount_claimed + replace_na(amount, 0) * share_claimed) %>% 
    select(-amount) %>% 
    return()
}


distribute_nols = function(tax_units, nols, year) { 

  #----------------------------------------------------------------------------
  # For a given claiming year, allocates new NOLs to tax units in proportion 
  # to positive pass-through income. 
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax unit data
  #   - nols (df)      : NOLs table as per initialize_nols() and update_nols()
  #   - year (int)     : claiming year 
  #
  # Returns: updated tax units tibble (df).
  #----------------------------------------------------------------------------
  
  
  # Get total amount claimed in specified year
  amount_claimed = nols %>% 
    filter(year_claimed == year) %>%
    summarise(amount_claimed = sum(amount_claimed) * 1e9) %>% 
    get_vector('amount_claimed')
  
  # Distribution NOLs in proportion to positive pass-through income
  tax_units %>% 
    mutate(positive_pt = pmax(0, sole_prop + part_active + part_passive - 
                                 part_active_loss - part_passive_loss - 
                                 part_179 + scorp_active + scorp_passive - 
                                 scorp_active_loss - scorp_passive_loss - 
                                 scorp_179),
           new_nols    = amount_claimed * (positive_pt / sum(positive_pt * weight))) %>% 
    select(-positive_pt) %>% 
    return()
}



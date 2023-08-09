#--------------------------------------------------------
# Function to calculate taxable portion of OASI benefits
#--------------------------------------------------------


calc_taxable_ss = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates taxable OASI benefits, i.e. the amount includable in AGI 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - taxable_ss (dbl) : taxable OASI benefits
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'gross_ss',        # (dbl, self)  gross OASI benefits
    'other_inc',       # (dbl, self)  gross income less OASI benefits 
    'exempt_int',      # (dbl, self)  tax-exempt interest income
    'above_ded',       # (dbl, self)  above-the-line deductions
    'ss.magi_ss_rate', # (dbl, law)   rate at which benefits are included in the 
                       #              definition of modified AGI 
    'ss.rates[]',      # (dbl[], law) benefit inclusion rates
    'ss.brackets[]'    # (int[], law) benefit inclusion rate brackets
  )
  
  # Parse tax unit object passed as argument
  tax_unit = parse_calc_fn_input(tax_unit, req_vars) 
  
  # Determine number of inclusion rates/brackets
  n = tax_unit %>% 
    select(starts_with('ss.brackets')) %>% 
    names() %>% 
    str_sub(-1) %>% 
    as.integer() %>% 
    max()
  
  # Add a few variables required for SS benefit inclusion calculation
  tax_unit %<>%
    mutate(
      
      # (N+1)th bracket, used to calculate taxable benefits in excess of top bracket
      !!paste0('ss.brackets', n + 1) := Inf, 
  
      # Modified AGI per SS rules
      magi = pmax(0, (gross_ss * ss.magi_ss_rate) + other_inc + exempt_int - above_ded)
  )
  
  # Iterate over brackets, stored with associated output prefix
  1:n %>% 
    set_names(paste0('taxable_ss', 1:n)) %>% 
    
    # For each bracket...
    map_df(function(i) {
      
      # Limit income to next bracket
      inc = pmin(tax_unit$magi, tax_unit[[paste0('ss.brackets', i + 1)]])
      
      # Calculate taxable excess over this bracket
      excess = pmax(0, inc - tax_unit[[paste0('ss.brackets', i)]])
      
      # Limit to gross benefits
      taxable_benefits = pmin(tax_unit$gross_ss, excess)
      
      # Apply rate
      return(taxable_benefits * tax_unit[[paste0('ss.rates', i)]])
    
    }) %>% 
    
    # Calculate total, limit to highest inclusion rate, and return
    mutate(taxable_ss = rowSums(.), 
           taxable_ss = pmin(taxable_ss, tax_unit$gross_ss * tax_unit[[paste0('ss.rates', n)]])) %>% 
    select(taxable_ss) %>% 
    return()
}


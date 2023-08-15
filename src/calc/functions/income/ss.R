#--------------------------------------------------------
# Function to calculate taxable portion of OASI benefits
#--------------------------------------------------------


calc_ss = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates taxable OASI benefits, i.e. the amount includable in AGI 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - txbl_ss (dbl) : taxable OASI benefits
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    
    'gross_ss',        # (dbl)  gross OASI benefits
    'magi_ss',         # (dbl)  AGI less OASI benefits plus tax-exempt interest
    
    # Tax law attributes
    
    'ss.magi_ss_rate', # (dbl)   rate at which benefits are added to MAGI when
                       #         determining inclusion rate
    'ss.rates[]',      # (dbl[]) benefit inclusion rates
    'ss.brackets[]'    # (int[]) benefit inclusion rate brackets
  )
  
  # Parse tax unit object passed as argument
  tax_unit = parse_calc_fn_input(tax_unit, req_vars) 
  
  # Determine number of inclusion rates/brackets
  n = tax_unit %>% 
    select(starts_with('ss.brackets')) %>% 
    names() %>%
    length()
  
  # Add integer index if not specified under single-bracket case
  if (n == 1 & 'ss.brackets' %in% names(tax_unit)) {
    tax_unit %<>% 
      rename(ss.brackets1 = ss.brackets, ss.rates1 = ss.rates)
  }
  
  # Add a few variables required for SS benefit inclusion calculation
  tax_unit %<>%
    mutate(
      
      # (N+1)th bracket, used to calculate taxable benefits in excess of top bracket
      !!paste0('ss.brackets', n + 1) := Inf, 
      
      # Modified AGI plus some share of benefits
      magi_plus_ss = pmax(0, (gross_ss * ss.magi_ss_rate) + magi_ss)
    )

  # For each bracket...
  1:n %>% 
    set_names(paste0('taxable_ss', .)) %>% 
    map_df(function(i) {
      
      # Limit income to next bracket
      inc = pmin(tax_unit$magi_plus_ss, tax_unit[[paste0('ss.brackets', i + 1)]])
      
      # Calculate taxable excess over this bracket
      excess = pmax(0, inc - tax_unit[[paste0('ss.brackets', i)]])
      
      # Limit to gross benefits and apply rate
      return(pmin(excess, tax_unit$gross_ss) * tax_unit[[paste0('ss.rates', i)]])
      
    }) %>% 
    
    # Calculate total, limit to highest inclusion rate, and return
    mutate(txbl_ss = rowSums(.), 
           txbl_ss = pmin(txbl_ss, tax_unit$gross_ss * tax_unit[[paste0('ss.rates', n)]])) %>% 
    select(txbl_ss) %>% 
    return()
}


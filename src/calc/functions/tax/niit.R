#--------------------------------------------------------
# Function to calculate Net Investment Income Tax (NIIT)
#--------------------------------------------------------


calc_niit = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates Net Investment Income Tax (NIIT) liabilty.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - liab_niit (dbl) : NIIT liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: Calculate NIIT logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(liab_niit) %>% 
    return()
}

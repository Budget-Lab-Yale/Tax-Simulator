#-----------------------------------------------------------
# Function to calculate Earned Income Tax Credit (EITC)
#-----------------------------------------------------------


calc_eitc = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates Earned Income Tax Credit (EITC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - eitc (dbl) : value of EITC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: Calculate EITC logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(eitc) %>% 
    return()
}

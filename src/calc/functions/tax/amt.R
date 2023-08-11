#---------------------------------------------------------------
# Function to calculate Alternative Minimum Tax (AMT) liability
#---------------------------------------------------------------


calc_amt = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates Alternative Minimum Tax (AMT) liability.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - liab_amt (dbl) : AMT liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(liab_amt) %>% 
    return()
}

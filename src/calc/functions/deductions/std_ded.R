#------------------------------------------
# Function to calculate standard deduction
#------------------------------------------


calc_std_ded = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates standard deduction. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - std_ded (dbl) : value of standard deduction (even if itemizing)
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(std_ded) %>% 
    return()
}


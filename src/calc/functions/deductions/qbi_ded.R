#-----------------------------------------------------------
# Function to calculate Qualified Business Income deduction
#-----------------------------------------------------------


calc_qbi_ded = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates deduction for Qualified Business Income (QBI).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - qbi_ded (dbl) : value of QBI deduction
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
    select(qbi_ded) %>% 
    return()
}

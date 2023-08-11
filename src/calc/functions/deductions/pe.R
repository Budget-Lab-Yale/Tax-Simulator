#----------------------------------------------------
# Function to calculate value of personal exemptions
#----------------------------------------------------


calc_pe = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates value of personal exemptions.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - pe (dbl) : value of deduction for personal exemptions
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
    select(pe) %>% 
    return()
}

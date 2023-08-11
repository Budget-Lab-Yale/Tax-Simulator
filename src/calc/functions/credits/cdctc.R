#-------------------------------------------------------------------
# Function to calculate Child and Dependent Care Tax Credit (CDCTC)
#-------------------------------------------------------------------

calc_cdctc = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates value of Child and Dependent Care Tax Credit (CDCTC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - cdctc (dbl) : value of CDCTC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: Calculate CDCTC logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(cdctc) %>% 
    return()
}

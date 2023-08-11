#----------------------------------------
# Function to calculate recovery rebates
#----------------------------------------


calc_rebate = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates value of a fully refundable, per-person credit -- i.e. a 
  # "recovery rebate" as per 2021, a "stimulus check", a UBI, a demogrant, etc.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - rebate (dbl) : value of rebate
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: Calculate Recovery Rebate logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(rebate) %>% 
    return()
}

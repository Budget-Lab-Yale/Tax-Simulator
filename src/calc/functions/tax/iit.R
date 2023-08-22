#---------------------------------------------------------------------
# Function to calculate individual income tax liability after credits 
#---------------------------------------------------------------------


calc_liab_iit = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates final individual income tax liability by allocating credit 
  # values. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - liab_iit (dbl) : individual income tax liability
  #          - refund   (dbl) : extent to which refundable credits exceed 
  #                             individual income tax liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # TODO: form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(liab_iit, refund) %>% 
    return()
}

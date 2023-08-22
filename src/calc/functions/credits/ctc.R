#----------------------------------------------
# Function to calculate Child Tax Credit (CTC)
#----------------------------------------------


calc_ctc = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Child Tax Credit (CTC), both the nonrefundable and the 
  # refundable portion (Additional Child Tax Credit, or ACTC), including the
  # credit for other dependents.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - ctc (dbl)  : value of CTC including credit for other dependents
  #          - actc (dbl) : ACTC, i.e. refundable component of the CTC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # TODO: Calculate CTC logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(ctc) %>% 
    return()
}

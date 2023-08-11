#--------------------------------------
# Function to calculate Saver's Credit 
#--------------------------------------

calc_savers_cred = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates value of the Retirement Savings Contributions Credit, or the 
  # "Saver's Credit".
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - savers_cred (dbl) : value of Savers Credit
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO: Calculate Savers Credit logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(savers_cred) %>% 
    return()
}

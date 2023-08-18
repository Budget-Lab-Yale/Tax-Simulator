#--------------------------------------
# Function to calculate Saver's Credit 
#--------------------------------------

calc_savers_cred = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of the Retirement Savings Contributions Credit, or the 
  # "Saver's Credit".
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - savers_cred (dbl) : value of Savers Credit
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # TODO: Calculate Savers Credit logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(savers_cred) %>% 
    return()
}

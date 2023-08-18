#-----------------------------------------------------------
# Function to calculate education credits
#-----------------------------------------


calc_ed_credit = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of the American Opportunity Credit (AOC) and the Lifetime 
  # Learning Credit (LLC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - aoc (dbl) : value of AOC
  #          - llc (dbl) : value of LLC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO: List required variables here
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # TODO: Calculate Education Credits logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(aoc, llc) %>% 
    return()
}

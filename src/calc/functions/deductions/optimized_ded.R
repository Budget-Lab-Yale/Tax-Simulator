#-----------------------------------------------------------------------
# Function to determine whether to take standard or itemized deductions 
#-----------------------------------------------------------------------


calc_optimized_ded = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates whether filers tax standard or itemized deductions, accounting
  # for any nonitemizer charitable contribution deduction available.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - itemizing (bool) : whether filer itemizes deductions
  #          - ded       (dbl)  : standard deduction or itemized deduction if 
  #                               itemizing   
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'TODO',        # (dbl, self)  TODO
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # TODO form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(itemizing, ded) %>% 
    return()
}

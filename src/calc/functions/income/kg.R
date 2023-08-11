#-------------------------------------------------------
# Function to calculate capital gains includable in AGI
#-------------------------------------------------------


calc_kg = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates capital gains includable in AGI, i.e. implements Schedule D
  # form logic. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - txbl_kg (dbl) : net capital gain includable in AGI
  #----------------------------------------------------------------------------
  
  req_vars = c(
    '...',               # TODO
    'agi.kg_loss_limit', # (int, law) maximum deductible capital loss
    'agi.kg_excl_rate'   # (dbl, law) share of capital gains excluded from AGI 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(txbl_kg) %>% 
    return()
}


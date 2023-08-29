#-------------------------------------------------------
# Function to calculate capital gains includable in AGI
#-------------------------------------------------------


calc_kg = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates capital gains includable in AGI, i.e. implements Schedule D
  # form logic. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables 
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - txbl_kg (dbl) : net capital gain includable in AGI
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'kg_st',       # (dbl) net short term capital gain ex carryover
    'kg_lt',       # (dbl) net long term capital gain ex carryover
    'kg_st_carry', # (dbl) short term capital loss carryover (positive number)
    'kg_lt_carry', # (dbl) long term capital loss carryover (positive number)
    
    # Tax law attributes
    'agi.kg_loss_limit', # (int) maximum deductible capital loss (positive number)
    'agi.kg_excl_rate'   # (dbl) share of capital gains excluded from AGI 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate taxable capital gain, limiting to maximum deductible loss
      txbl_kg = pmax(kg_st + kg_lt - kg_st_carry - kg_lt_carry, -agi.kg_loss_limit),
      
      # Exclude a policy-supplied fraction of capital gains from AGI 
      txbl_kg = txbl_kg * (1 - agi.kg_excl_rate)
      
    ) %>% 
    
    # Keep variables to return
    select(txbl_kg) %>% 
    return()
}


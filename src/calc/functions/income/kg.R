#-------------------------------------------------------
# Function to calculate capital gains includable in AGI
#-------------------------------------------------------

# Set return variables for function
return_vars$calc_kg = c('kg_pref', 'txbl_kg')


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
  #   - kg_pref (dbl) : preferred-rate capital gains ("net capital gain" in the 
  #                     internal revenue code)
  #   - txbl_kg (dbl) : net capital gain includable in AGI
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'kg_st',       # (dbl) net short term capital gain ex carryover
    'kg_lt',       # (dbl) net long term capital gain ex carryover
    
    # Tax law attributes
    'agi.kg_loss_limit', # (int) maximum deductible capital loss (positive number)
    'agi.kg_excl_rate'   # (dbl) share of capital gains excluded from AGI 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate preferred-rate capital gain ("net capital gain" in the code, 
      # i.e. the non-negative smaller of line 15 or 16 on Sch. D)
      kg_pref = pmax(0, pmin(kg_lt, kg_st + kg_lt)),
      
      # Calculate taxable capital gain, limiting to maximum deductible loss
      txbl_kg = pmax(kg_st + kg_lt, -agi.kg_loss_limit),
      
      # Exclude a policy-supplied fraction of capital gains from AGI 
      txbl_kg = if_else(txbl_kg > 0, txbl_kg * (1 - agi.kg_excl_rate), txbl_kg)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_kg)) %>% 
    return()
}


#----------------------------------
# Function to calculate AGI Surtax
#----------------------------------

# Set return variables for function
return_vars$calc_agi_surtax = c('liab_surtax')

calc_agi_surtax = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates a flat AGI surtax above a single threshold.
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of the following variable:
  #  - liab_surtax (dbl) : total AGI surtax liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi', # (dbl) Adjusted Gross Income
    
    # Tax law attributes - 3 brackets
    'surtax.threshold1', # (dbl) AGI threshold for first bracket
    'surtax.rate1',      # (dbl) surtax rate for first bracket
    'surtax.threshold2', # (dbl) AGI threshold for second bracket  
    'surtax.rate2',      # (dbl) surtax rate for second bracket
    'surtax.threshold3', # (dbl) AGI threshold for third bracket
    'surtax.rate3'       # (dbl) surtax rate for third bracket
  )
  
  tax_unit %>%
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(
      
      # Calculate AGI surtax liability for each bracket
      # First bracket: from threshold1 to threshold2
      bracket1_income = pmax(0, pmin(agi, surtax.threshold2) - surtax.threshold1),
      bracket1_tax    = bracket1_income * surtax.rate1,
      
      # Second bracket: from threshold2 to threshold3
      bracket2_income = pmax(0, pmin(agi, surtax.threshold3) - surtax.threshold2),
      bracket2_tax    = bracket2_income * surtax.rate2,
      
      # Third bracket: above threshold3
      bracket3_income = pmax(0, agi - surtax.threshold3),
      bracket3_tax    = bracket3_income * surtax.rate3,
      
      # Total surtax liability
      liab_surtax = bracket1_tax + bracket2_tax + bracket3_tax
      
    ) %>%
    
    # Keep variables to return
    select(all_of(return_vars$calc_agi_surtax)) %>%
    return()
}

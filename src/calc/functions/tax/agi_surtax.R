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
    
    # Tax law attributes
    'surtax.threshold', # (dbl) AGI threshold for surtax
    'surtax.rate'       # (dbl) surtax rate
  )
  
  tax_unit %>%
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(
      
      # Calculate AGI surtax liability
      liab_surtax = pmax(0, agi - surtax.threshold) * surtax.rate
      
    ) %>%
    
    # Keep variables to return
    select(all_of(return_vars$calc_agi_surtax)) %>%
    return()
}

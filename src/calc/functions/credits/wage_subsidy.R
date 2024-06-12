#------------------------------------------------------------
# Function to calculate hypothetical individual wage subsidy
#------------------------------------------------------------

# Set return variables for function
return_vars$calc_wage_subsidy = c('wage_subsidy1', 'wage_subsidy2')


calc_wage_subsidy = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates hypothetical wage subsidy applied to individual earnings (in
  # contrast to EITC which is applied to the tax unit level). Initially used
  # to model the work credit in Bill Gale's 2024 Brookings paper on tax
  # simplification.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - wage_subsidy1 (dbl) : value of wage subsidy for primary earner
  #   - wage_subsidy2 (dbl) : value of wage subsidy for secondary earner
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'ei1', # (dbl)  earned income of primary filer
    'ei2', # (dbl)  earned income of secondary filer
    
    # Tax law attributes
    'wagesub.pi_rate',  # (dbl) phase-in rate
    'wagesub.pi_end',   # (dbl) earnings amount at which phase-in ends 
    'wagesub.po_rate',  # (dbl) phase out rate 
    'wagesub.po_thresh' # (dbl) earnings amount at which credit begins to phase out
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate credit value for primary earner
      max_value1    = pmin(ei1, wagesub.pi_end) * wagesub.pi_rate,
      wage_subsidy1 = pmax(0, max_value1 - pmax(0, ei1 - wagesub.po_thresh) * wagesub.po_rate),
      
      # Calculate credit value for secondary earner
      max_value2    = pmin(ei2, wagesub.pi_end) * wagesub.pi_rate,
      wage_subsidy2 = pmax(0, max_value2 - pmax(0, ei2 - wagesub.po_thresh) * wagesub.po_rate)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_wage_subsidy)) %>% 
    return()
}

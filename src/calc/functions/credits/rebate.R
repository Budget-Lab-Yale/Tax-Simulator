#----------------------------------------
# Function to calculate recovery rebates
#----------------------------------------

# Set return variables for function
return_vars$calc_rebate = c('rebate')


calc_rebate = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of a fully refundable, per-person credit -- i.e. a 
  # "recovery rebate" as per 2021, a "stimulus check", a UBI, a demogrant, etc.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - rebate (dbl) : value of rebate
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_status',    # (bool) whether tax filer is a dependent
    'filing_status', # (int)  filing status (1 = single, 2 = married filing jointly, 3 = married filing separately, 4 = head of household)
    'dep_age1',      # (int)  age of youngest dependent (NA for tax units without a dependent)
    'dep_age2',      # (int)  age of second youngest dependent (NA for tax units without a second dependent)
    'dep_age3',      # (int)  age of oldest dependent (NA for tax units without a third dependent)
    'dep_ssn1',      # (bool) whether youngest dependent has a Social Security number (NA for tax units without a dependent)
    'dep_ssn2',      # (bool) whether second youngest dependent has a Social Security number (NA for tax units without a second dependent)
    'dep_ssn3',      # (bool) whether oldest dependent has a Social Security number (NA for tax units without a third dependent)
    'agi',           # (dbl)  Adjusted Gross Income
    
    # Tax law attributes
    'rebate.dep_age_limit',  # (int) maximum qualifying dependent age
    'rebate.dep_ssn_req',    # (int) whether dependents are required to have an SSN to qualify
    'rebate.value',          # (int) credit value per adult
    'rebate.value_dep',      # (int) credit value per qualifying dependent
    'rebate.po_thresh',      # (int) AGI phaseout threshold
    'rebate.po_type',        # (int) whether phaseout type is a rate (0 means range)
    'rebate.po_range',       # (int) phaseout range
    'rebate.po_rate'         # (dbl) phaseout rate
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Determine number of qualifying dependents based on age and SSN status
      across(.cols  = c(dep_age1, dep_age2, dep_age3), 
             .fns   = ~ if_else(!is.na(.), . <= rebate.dep_age_limit, F),
             .names = 'age_qual{str_sub(col, -1)}'),
      across(.cols = c(dep_ssn1, dep_ssn2, dep_ssn3), 
             .fns  = ~ !is.na(.) & .),
      
      n_dep = (age_qual1 & (dep_ssn1 | rebate.dep_ssn_req == 0)) + 
              (age_qual2 & (dep_ssn2 | rebate.dep_ssn_req == 0)) + 
              (age_qual3 & (dep_ssn3 | rebate.dep_ssn_req == 0)),
      
      # Calculate credit value before phaseout
      n_adults = 1 + (filing_status == 2),
      rebate = (rebate.value * n_adults) + (rebate.value_dep * n_dep),
      
      # Apply phaseout
      po_range = if_else(rebate.po_type == 1, 
                         if_else(rebate == 0,  # Rate-based phaseout
                                 Inf, 
                                 rebate / rebate.po_rate),  
                         rebate.po_range),     # Range-based phaseout
      po_share = pmin(1, pmax(0, agi - rebate.po_thresh) / po_range),
      rebate   = rebate * (1 - po_share),
      
      # Apply dependent restriction
      rebate = rebate * !dep_status

    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_rebate)) %>% 
    return()
}

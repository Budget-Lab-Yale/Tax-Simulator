#----------------------------------------------------
# Function to calculate value of personal exemptions
#----------------------------------------------------


# Set return variables for function
return_vars$calc_pe_ded = c('pe_ded')


calc_pe_ded = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates personal exemptions.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe with the following variables:
  #   - pe_ded (dbl) : value of personal exemptions and deductions
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'filing_status',  # (int) filing status (1 = single, 2 = married filing jointly, 3 = married filing separately, 4 = head of household)
    'n_dep',          # (int) number of dependents
    'agi',            # (dbl) Adjusted Gross Income
    
    # Tax law attributes
    'pe.value',            # (int) personal exemption value
    'pe.po_thresh',        # (int) phaseout threshold 
    'pe.po_range',         # (int) range over which phaseout occurs
    'pe.po_discrete',      # (int) whether phaseout is discretized, as per pre-TCJA law
    'pe.po_discrete_step', # (dbl) rounding step for discretized phaseout
    'pe.dep_qualify',      # (int) whether dependents qualify for exemptions
    'pe.nondep_qualify'    # (int) whether non-dependents qualify for exemptions
  )
  
  tax_unit %>%
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(
      
      # Calculate value of personal exemptions
      n_pe_nondep = (pe.nondep_qualify == 1) * (1 + (filing_status == 2)), 
      n_pe_dep    = (pe.dep_qualify == 1)    * n_dep,
      pe_ded      = (n_pe_nondep + n_pe_dep) * pe.value,
      
      # Calculate extent to which deduction is phased out 
      po_share = pmin(1, pmax(0, agi - pe.po_thresh) / pe.po_range),
      
      # Round if phaseout is discretized, as per pre-TCJA law. Rounds up. Add 
      # new parameters if we want to round down or to closest.
      po_share = if_else(pe.po_discrete == 1, 
                         ceiling(po_share / pe.po_discrete_step) * pe.po_discrete_step, 
                         po_share),
      
      # Apply phaseout
      pe_ded = pe_ded * (1 - po_share)
      
    ) %>%
    
    # Keep variables to return
    select(all_of(return_vars$calc_pe_ded)) %>%
    return()
}


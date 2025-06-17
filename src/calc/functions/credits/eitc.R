#-----------------------------------------------------------
# Function to calculate Earned Income Tax Credit (EITC)
#-----------------------------------------------------------

# Set return variables for function
return_vars$calc_eitc = c('eitc')


calc_eitc = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Earned Income Tax Credit (EITC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - eitc (dbl) : value of EITC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_status',    # (bool) whether tax filer is a dependent
    'filing_status', # (int)  filing status (1 = single, 2 = married filing jointly, 3 = married filing separately, 4 = head of household)
    'n_dep_eitc',    # (int)  number of EITC-qualifying children
    'age1',          # (int)  age of primary filer
    'age2',          # (int)  age of secondary filer
    'ei1',           # (dbl)  earned income of primary filer
    'ei2',           # (dbl)  earned income of secondary filer
    'txbl_int',      # (dbl) taxable interest income 
    'exempt_int',    # (dbl) tax-exempt interest income
    'div_ord',       # (dbl) non-qualified dividend income
    'div_pref',      # (dbl) qualified dividend income
    'txbl_kg',       # (dbl) net capital gain included in AGI
    'sch_e',         # (dbl) Schedule E net income
    'part_scorp',    # (dbl) net partnership and S corporation income
    
    # Tax law attributes
    'eitc.pi_rate_0',     # (dbl) credit phase-in rate for filers with no qualifying children
    'eitc.pi_rate_1',     # (dbl) credit phase-in rate for filers with 1 child
    'eitc.pi_rate_2',     # (dbl) credit phase-in rate for filers with 2 children
    'eitc.pi_rate_3',     # (dbl) credit phase-in rate for filers with 3+ children
    'eitc.po_rate_0',     # (dbl) credit phase-out rate for filers with no children
    'eitc.po_rate_1',     # (dbl) credit phase-out rate for filers with 1 child
    'eitc.po_rate_2',     # (dbl) credit phase-out rate for filers with 2 children
    'eitc.po_rate_3',     # (dbl) credit phase-out rate for filers with 3+ children
    'eitc.pi_end_0',      # (int) earned income threshold at which credit is maximized for filers with no children
    'eitc.pi_end_1',      # (int) earned income threshold at which credit is maximized for filers with 1 child
    'eitc.pi_end_2',      # (int) earned income threshold at which credit is maximized for filers with 2 children
    'eitc.pi_end_3',      # (int) earned income threshold at which credit is maximized for filers with 3+ children
    'eitc.po_thresh_0',   # (int) AGI threshold at which credit begins phasing out for filers with no children
    'eitc.po_thresh_1',   # (int) AGI threshold at which credit begins phasing out for filers with 1 child
    'eitc.po_thresh_2',   # (int) AGI threshold at which credit begins phasing out for filers with 2 children
    'eitc.po_thresh_3',   # (int) AGI threshold at which credit begins phasing out for filers with 3 children
    'eitc.inv_inc_limit', # (int) maximum allowable investment income for credit eligibility
    'eitc.min_age',       # (int) minimum age for credit eligibility for filers with 0 children
    'eitc.max_age',       # (int) maximum age for credit eligibility for filers with 0 children
    'eitc.mfs_eligible',  # (int) whether credit is available for married filing separately returns   
    'eitc.parent_precert' # (int) whether parents are required to pre-certify their eligibility 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Determine qualifying earned income based on age
      qual1 = (n_dep_eitc > 0) | (age1 >= eitc.min_age & age1 <= eitc.max_age),
      qual2 = if_else(
        !is.na(age2),
        (n_dep_eitc > 0) | (age2 >= eitc.min_age & age2 <= eitc.max_age),
        F
      ),
      ei = (ei1 * qual1) + if_else(filing_status == 2, ei2 * qual2, 0),

      # Potentially deny eligibility based on dependent and filing status
      ei = ei * (!dep_status & (eitc.mfs_eligible == 1 | filing_status != 3)),
      
      # Potentially deny eligibility based on investment income
      inv_inc = txbl_int + 
                exempt_int + 
                div_ord + 
                div_pref + 
                pmax(0, txbl_kg) + 
                pmax(0, (sch_e - part_scorp)),
      ei = ei * (inv_inc <= eitc.inv_inc_limit),
            
      # Assign credit parameters based on number of children
      pi_rate = case_when(
        n_dep_eitc == 0 ~ eitc.pi_rate_0,
        n_dep_eitc == 1 ~ eitc.pi_rate_1,
        n_dep_eitc == 2 ~ eitc.pi_rate_2,
        T               ~ eitc.pi_rate_3
      ), 
      
      po_rate = case_when(
        n_dep_eitc == 0 ~ eitc.po_rate_0,
        n_dep_eitc == 1 ~ eitc.po_rate_1,
        n_dep_eitc == 2 ~ eitc.po_rate_2,
        T               ~ eitc.po_rate_3
      ), 
      
      pi_end = case_when(
        n_dep_eitc == 0 ~ eitc.pi_end_0,
        n_dep_eitc == 1 ~ eitc.pi_end_1,
        n_dep_eitc == 2 ~ eitc.pi_end_2,
        T               ~ eitc.pi_end_3
      ), 
      
      po_thresh = case_when(
        n_dep_eitc == 0 ~ eitc.po_thresh_0,
        n_dep_eitc == 1 ~ eitc.po_thresh_1,
        n_dep_eitc == 2 ~ eitc.po_thresh_2,
        T               ~ eitc.po_thresh_3
      ),
      
      # Calculate credit value
      max_eitc = pmin(ei, pi_end) * pi_rate,
      eitc     = pmax(0, max_eitc - pmax(0, pmax(ei, agi) - po_thresh) * po_rate),
      
      # Adjust for pre-certification changes
      eitc     = if_else(eitc.parent_precert & n_dep_eitc > 0,
                         if_else(globals$random_numbers$r.eitc_precert < 0.031, 0, eitc), 
                         eitc)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_eitc)) %>% 
    return()
}

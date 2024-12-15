#-------------------------------------------------------------------
# Function to calculate Child and Dependent Care Tax Credit (CDCTC)
#-------------------------------------------------------------------

# Set return variables for function
return_vars$calc_cdctc = c('cdctc_nonref', 'cdctc_ref')


calc_cdctc = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of Child and Dependent Care Tax Credit (CDCTC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables 
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #  - cdctc_nonref (dbl) : nonrefundable component of CDCTC; if law specifies
  #                         the CDCTC is refundable, this variable is 0 for all
  #  - cdctc_ref (dbl)    : refundable component of CDCTC; if law specifies the
  #                         CDCTC is nonrefundable, this variable is 0 for all
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'n_dep',         # (dbl) number of dependents
    'care_exp',      # (dbl) eligible dependent care expenses 
    'filing_status', # (int) filing status of tax unit
    'ei1',           # (dbl) earned income of primary filer
    'ei2',           # (dbl) earned income of secondary filer (NA for non-joint returns)
    'agi',           # (dbl) Adjusted Gross Income
    'liab_bc',       # (dbl) income tax liability before credits, including AMT
    'ftc',           # (dbl) value of foreign tax credits
    
    # Tax law attributes 
    'cdctc.exp_limit',     # (int) maximum credit-eligible expenses per qualifying dependent
    'cdctc.n_dep_limit',   # (int) maximum number of qualifying dependents 
    'cdctc.rate1',         # (dbl) credit rate for base credit (share of eligible expenses)
    'cdctc.rate2',         # (dbl) credit rate for additional credit (share of eligible expenses)
    'cdctc.po_thresh1',    # (dbl) AGI threshold above which credit rate 1 begins phasing out
    'cdctc.po_thresh2',    # (dbl) AGI threshold above which credit rate 2 begins phasing out
    'cdctc.po_rate1',      # (dbl) rate at which credit rate 1 phases out with AGI
    'cdctc.po_rate2',      # (dbl) rate at which credit rate 2 phases out with AGI
    'cdctc.discrete_step', # (int) rounding step for descretized phaseout function
    'cdctc.refundable'     # (int) whether credit is refundable
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Limit number of qualifying dependents to maximum
      n_qual_dep = pmin(n_dep, cdctc.n_dep_limit),
      
      # Limit credit-eligible expenses to per-qualifying-dependent maximum
      qual_exp = pmin(care_exp, n_qual_dep * cdctc.exp_limit),
      
      # Limit credit-eligible expenses to earned income
      qual_exp = pmin(qual_exp, pmax(0, if_else(filing_status == 2, pmin(ei1, ei2), ei1))),
      
      # Calculate AGI in excess of phaseout rates and adjust for discrete rounding steps 
      excess1 = pmax(0, agi - cdctc.po_thresh1),
      excess2 = pmax(0, agi - cdctc.po_thresh2),
      
      excess1 = ceiling(excess1 / cdctc.discrete_step) * cdctc.discrete_step,
      excess2 = ceiling(excess2 / cdctc.discrete_step) * cdctc.discrete_step,
      
      # Calculate credit rate after phaseouts 
      rate1 = pmax(0, pmax(0, cdctc.rate1 - excess1 * cdctc.po_rate1)),
      rate2 = pmax(0, pmax(0, cdctc.rate2 - excess2 * cdctc.po_rate2)),
      rate  = rate1 + rate2,
      
      # Calculate credit and allocate to refundable and nonrefundable components
      cdctc        = qual_exp * rate,
      liab         = pmax(0, liab_bc - ftc),
      
      cdctc_nonref = if_else(cdctc.refundable == 1, 0, pmin(cdctc, liab)),
      cdctc_ref    = if_else(cdctc.refundable == 1, cdctc, 0)
    
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_cdctc)) %>% 
    return()
}

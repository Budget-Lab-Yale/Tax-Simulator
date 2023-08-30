#-------------------------------------------------------------------
# Function to calculate Child and Dependent Care Tax Credit (CDCTC)
#-------------------------------------------------------------------


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
    'n_dep',    # (dbl) number of dependents
    'care_exp', # (dbl) eligible dependent care expenses 
    'ei1',      # (dbl) earned income of primary filer
    'ei2',      # (dbl) earned income of secondary filer (NA for non-joint returns)
    'agi',      # (dbl) Adjusted Gross Income
    'liab_bc',  # (dbl) income tax liability before credits, including AMT
    'ftc',      # (dbl) value of foreign tax credits
    
    # Tax law attributes 
    'cdctc.exp_limit',     # (int) maximum credit-eligible expenses per qualifying dependent
    'cdctc.n_dep_limit',   # (int) maximum number of qualifying dependents 
    'cdctc.ei_limit',      # (int) whether to limit eligible expenses to adult-minimum earned income
    'cdctc.min_rate',      # (dbl) minimum credit rate (share of eligible expenses)
    'cdctc.max_rate',      # (dbl) maximum credit rate (share of eligible expenses)
    'cdctc.po_thresh',     # (dbl) AGI threshold above which credit rate begins phasing out from max to min
    'cdctc.po_rate',       # (dbl) rate at which credit rate phases out with AGI
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
      
      # Limit credit-eligible expenses to adult-minimum earned income, if law requires
      min_ei   = pmin(ei1, replace_na(as.numeric(ei2), Inf)),
      qual_exp = pmin(qual_exp, if_else(cdctc.ei_limit == 1, min_ei, Inf)),
      
      # Calculate credit rate: a descretized linear negative function of AGI, 
      # akin to a phaseout, with a floor and ceiling 
      excess = pmax(0, agi - cdctc.po_thresh),
      excess = ceiling(excess / cdctc.discrete_step) * cdctc.discrete_step, 
      rate   = pmax(cdctc.min_rate, pmax(0, cdctc.max_rate - excess * cdctc.po_rate)),
      
      # Calculate credit and allocate to refundable and nonrefundable components
      cdctc        = qual_exp * rate,
      liab         = pmax(0, liab_bc - ftc),
      
      cdctc_nonref = if_else(cdctc.refundable == 1, 0, pmin(cdctc, liab)),
      cdctc_ref    = if_else(cdctc.refundable == 1, cdctc, 0)
    
    ) %>% 
    
    # Keep variables to return
    select(cdctc_nonref, cdctc_ref) %>% 
    return()
}

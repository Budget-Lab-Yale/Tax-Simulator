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
    'dep_age1',       # (int) age of youngest dependent (NA for tax units without a dependent)
    'dep_age2',       # (int) age of second youngest dependent (NA for tax units without a second dependent)
    'dep_age3',       # (int) age of oldest dependent (NA for tax units without a third dependent)
    'care_exp',       # (dbl) eligible dependent care expenses 
    'filing_status',  # (int) filing status of tax unit
    'ei1',            # (dbl) earned income of primary filer
    'ei2',            # (dbl) earned income of secondary filer (NA for non-joint returns)
    'agi',            # (dbl) Adjusted Gross Income
    'liab_bc',        # (dbl) income tax liability before credits, including AMT
    'ftc',            # (dbl) value of foreign tax credits
    'r.cdctc_takeup', # (dbl) random number for determining takeup
    
    # Tax law attributes 
    'cdctc.exp_limit',         # (int) maximum credit-eligible expenses per qualifying dependent
    'cdctc.n_dep_limit',       # (int) maximum number of qualifying dependents
    'cdctc.young_age_limit',   # (int) maximum age to be considered a young qualifying dependent
    'cdctc.old_age_limit',     # (int) maximum age to be considered an old qualifying dependent
    'cdctc.young_rate1',       # (dbl) young dependents: credit rate for base credit (share of eligible expenses)
    'cdctc.young_rate2',       # (dbl) young dependents: credit rate for first additional credit (share of eligible expenses)
    'cdctc.young_rate3',       # (dbl) young dependents: credit rate for second additional credit (share of eligible expenses)
    'cdctc.young_po_thresh1',  # (dbl) young dependents: AGI threshold above which credit rate 1 begins phasing out
    'cdctc.young_po_thresh2',  # (dbl) young dependents: AGI threshold above which credit rate 2 begins phasing out
    'cdctc.young_po_thresh3',  # (dbl) young dependents: AGI threshold above which credit rate 3 begins phasing out
    'cdctc.young_po_rate1',    # (dbl) young dependents: rate at which credit rate 1 phases out with AGI
    'cdctc.young_po_rate2',    # (dbl) young dependents: rate at which credit rate 2 phases out with AGI
    'cdctc.young_po_rate3',    # (dbl) young dependents: rate at which credit rate 3 phases out with AGI
    'cdctc.old_rate1',         # (dbl) old dependents: credit rate for base credit (share of eligible expenses)
    'cdctc.old_rate2',         # (dbl) old dependents: credit rate for first additional credit (share of eligible expenses)
    'cdctc.old_rate3',         # (dbl) old dependents: credit rate for second additional credit (share of eligible expenses)
    'cdctc.old_po_thresh1',    # (dbl) old dependents: AGI threshold above which credit rate 1 begins phasing out
    'cdctc.old_po_thresh2',    # (dbl) old dependents: AGI threshold above which credit rate 2 begins phasing out
    'cdctc.old_po_thresh3',    # (dbl) old dependents: AGI threshold above which credit rate 3 begins phasing out
    'cdctc.old_po_rate1',      # (dbl) old dependents: rate at which credit rate 1 phases out with AGI
    'cdctc.old_po_rate2',      # (dbl) old dependents: rate at which credit rate 2 phases out with AGI
    'cdctc.old_po_rate3',      # (dbl) old dependents: rate at which credit rate 3 phases out with AGI
    'cdctc.discrete_step',     # (int) rounding step for descretized phaseout function
    'cdctc.refundable'         # (int) whether credit is refundable
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate number of dependents by age category
      across(.cols  = c(cdctc.young_age_limit, cdctc.old_age_limit), 
             .fns   = list('1' = ~ !is.na(dep_age1) & (dep_age1 <= .), 
                           '2' = ~ !is.na(dep_age2) & (dep_age2 <= .), 
                           '3' = ~ !is.na(dep_age3) & (dep_age3 <= .)),
             .names = '{str_sub(col, 7, 7)}{fn}'),
      n_young = y1 + y2 + y3,
      n_old   = o1 + o2 + o3 - n_young,
      
      # Calculate number of eligible dependents. Young children stack first
      # (assumes young child policy is at least as generous as older child policy)
      n_young = pmin(n_young, cdctc.n_dep_limit),
      n_old   = pmin(n_old,   cdctc.n_dep_limit - n_young),
      
      # Calculate earned income limitation for eligible expenses
      ei_limit = pmax(0, if_else(filing_status == 2, pmin(ei1, ei2), ei1)),
      
      
      #------------------
      # Young dependents
      #------------------
      
      # Limit credit-eligible expenses to per-qualifying-dependent maximum
      young_qual_exp = pmin(care_exp, n_young * cdctc.exp_limit),
      
      # Limit credit-eligible expenses to earned income
      young_qual_exp = pmin(young_qual_exp, ei_limit),
      
      # Calculate AGI in excess of phaseout rates and adjust for discrete rounding steps 
      young_excess1 = pmax(0, agi - cdctc.young_po_thresh1),
      young_excess2 = pmax(0, agi - cdctc.young_po_thresh2),
      young_excess3 = pmax(0, agi - cdctc.young_po_thresh3),
      
      young_excess1 = ceiling(young_excess1 / cdctc.discrete_step) * cdctc.discrete_step,
      young_excess2 = ceiling(young_excess2 / cdctc.discrete_step) * cdctc.discrete_step,
      young_excess3 = ceiling(young_excess3 / cdctc.discrete_step) * cdctc.discrete_step,
      
      # Calculate credit rate after phaseouts 
      young_rate1 = pmax(0, cdctc.young_rate1 - young_excess1 * cdctc.young_po_rate1),
      young_rate2 = pmax(0, cdctc.young_rate2 - young_excess2 * cdctc.young_po_rate2),
      young_rate3 = pmax(0, cdctc.young_rate3 - young_excess2 * cdctc.young_po_rate3),
      young_rate  = young_rate1 + young_rate2 + young_rate3,
      
      # Calculate credit value
      young_cdctc = young_qual_exp * young_rate,
      
      
      #----------------
      # Old dependents
      #----------------
      
      # Limit credit-eligible expenses to per-qualifying-dependent maximum
      old_qual_exp = pmin(care_exp - young_qual_exp, n_old * cdctc.exp_limit),
      
      # Limit credit-eligible expenses to earned income
      old_qual_exp = pmin(old_qual_exp, ei_limit),
      
      # Calculate AGI in excess of phaseout rates and adjust for discrete rounding steps 
      old_excess1 = pmax(0, agi - cdctc.old_po_thresh1),
      old_excess2 = pmax(0, agi - cdctc.old_po_thresh2),
      old_excess3 = pmax(0, agi - cdctc.old_po_thresh3),
      
      old_excess1 = ceiling(old_excess1 / cdctc.discrete_step) * cdctc.discrete_step,
      old_excess2 = ceiling(old_excess2 / cdctc.discrete_step) * cdctc.discrete_step,
      old_excess3 = ceiling(old_excess3 / cdctc.discrete_step) * cdctc.discrete_step,
      
      # Calculate credit rate after phaseouts 
      old_rate1 = pmax(0, pmax(0, cdctc.old_rate1 - old_excess1 * cdctc.old_po_rate1)),
      old_rate2 = pmax(0, pmax(0, cdctc.old_rate2 - old_excess2 * cdctc.old_po_rate2)),
      old_rate3 = pmax(0, pmax(0, cdctc.old_rate3 - old_excess3 * cdctc.old_po_rate3)),
      old_rate  = old_rate1 + old_rate2 + old_rate3,
      
      # Calculate credit value
      old_cdctc = old_qual_exp * old_rate,
      
      
      #-------------------
      # Credit allocation
      #-------------------
      
      # Allocate foreign tax credit, which stacks before all other credits
      liab = pmax(0, liab_bc - ftc),
      
      # Allocate to refundable and nonrefundable components
      cdctc_nonref = if_else(cdctc.refundable == 1, 0, pmin(young_cdctc + old_cdctc, liab)),
      cdctc_ref    = if_else(cdctc.refundable == 0, 0, young_cdctc + old_cdctc),
      
      # Model take-up: 90% calibrated to target 2019 actual CDCTC
      cdctc_nonref = cdctc_nonref * (r.cdctc_takeup < 0.9),
      cdctc_ref    = cdctc_ref    * (r.cdctc_takeup < 0.9)
    
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_cdctc)) %>% 
    return()
}

#----------------------------------------------
# Function to calculate Child Tax Credit (CTC)
#----------------------------------------------

# Set return variables for function
return_vars$calc_ctc = c('ctc_nonref', 'ctc_ref')


calc_ctc = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Child Tax Credit (CTC), both the nonrefundable and the 
  # refundable portion (Additional Child Tax Credit, or ACTC), including the
  # credit for other dependents.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #  - ctc_nonref (dbl) : value of CTC including credit for other dependents
  #  - ctc_ref (dbl)    : ACTC, i.e. refundable component of the CTC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_age1',      # (int) age of youngest dependent (NA for tax units without a dependent)
    'dep_age2',      # (int) age of second youngest dependent (NA for tax units without a second dependent)
    'dep_age3',      # (int) age of oldest dependent (NA for tax units without a third dependent)
    'dep_ssn1',      # (bool) whether youngest dependent has a Social Security number (NA for tax units without a dependent)
    'dep_ssn2',      # (bool) whether second youngest dependent has a Social Security number (NA for tax units without a second dependent)
    'dep_ssn3',      # (bool) whether oldest dependent has a Social Security number (NA for tax units without a third dependent)
    'dep_ctc1',      # (bool) whether youngest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a dependent)
    'dep_ctc2',      # (bool) whether second youngest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a second dependent)
    'dep_ctc3',      # (bool) whether oldest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a third dependent)
    'n_dep',         # (int) number of dependents
    'agi',           # (dbl) Adjusted Gross Income
    'liab_bc',       # (dbl) liability before credits, including AMT   
    'ftc',           # (dbl) value of foreign tax credit 
    'cdctc_nonref',  # (dbl) value of nonrefundable Child and Dependent Care Credit 
    'ed_nonref',     # (dbl) value of nonrefundable education credit
    'savers_nonref', # (dbl) value of nonrefundable Saver's Credit
    'old_cred',      # (dbl) value of Elderly and Disabled Credit
    'ei',            # (dbl) earned income
    
    # Tax law attributes
    'ctc.young_age_limit',  # (int) maximum age to qualify as "young" child
    'ctc.old_age_limit',    # (int) maximum age to qualify as "old" child
    'ctc.need_ssn',         # (int) whether SSN is required to qualify for CTC
    'ctc.value_young1',     # (int) maximum credit value per "young" child corresponding to phaseout threshold 1 
    'ctc.value_young2',     # (int) maximum credit value per "young" child corresponding to phaseout threshold 2
    'ctc.value_old1',       # (int) maximum credit value per "old" child corresponding to phaseout threshold 1
    'ctc.value_old2',       # (int) maximum credit value per "old" child corresponding to phaseout threshold 2
    'ctc.value_other',      # (int) maximum (nonrefundable) credit value per nonqualifying dependent (assumed to phase out with po_thresh1)
    'ctc.po_thresh1',       # (int) AGI threshold above which value 1 phases out
    'ctc.po_thresh2',       # (int) AGI threshold above which value 2 phases out
    'ctc.po_rate1',         # (dbl) phaseout rate for value 1
    'ctc.po_rate2',         # (dbl) phaseout rate for value 2 
    'ctc.po_discrete',      # (int) whether phaseout is discretized, as in current-law form
    'ctc.po_discrete_step', # (int) rounding step for discretized phaseout
    'ctc.max_refund',       # (int) maximum refundable CTC per qualifying child
    'ctc.pi_thresh',        # (int) earned income threshold above which ACTC phases in
    'ctc.pi_rate',          # (dbl) ACTC phase-in rate
    'ctc.fully_refundable'  # (int) whether CTC (exlcluding Credit for Other Dependent) is fully refundable
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      #------------------
      # Child tax credit
      #------------------
      
      # Determine number of qualifying dependents by age, checking for SSNs
      need_ssn = ctc.need_ssn == 1,
      across(.cols = starts_with('dep_age'), 
             .fns  = ~ replace_na(as.numeric(.), Inf)),
      across(.cols = c(starts_with('dep_ssn'), starts_with('dep_ctc')), 
             .fns  = ~ !is.na(.) & .),
      across(.cols  = c(ctc.young_age_limit, ctc.old_age_limit), 
             .fns   = list('1' = ~ dep_ctc1 & (dep_age1 <= .) & (dep_ssn1 | !need_ssn), 
                           '2' = ~ dep_ctc2 & (dep_age2 <= .) & (dep_ssn2 | !need_ssn), 
                           '3' = ~ dep_ctc3 & (dep_age3 <= .) & (dep_ssn3 | !need_ssn)), 
             .names = '{str_sub(col, 5, 5)}{fn}'),
      
      n_young = y1 + y2 + y3,
      n_old   = o1 + o2 + o3 - n_young,
      n_other = n_dep - n_young - n_old,
      
      # Calculate value before phase-in/out, including nonrefundable credit
      # for other dependents
      max_value1      = (ctc.value_young1 * n_young) + (ctc.value_old1 * n_old),
      max_value2      = (ctc.value_young2 * n_young) + (ctc.value_old2 * n_old),
      max_value_other = ctc.value_other * n_other,
      
      # Determine and apply phaseouts
      excess1 = agi - ctc.po_thresh1,
      excess2 = agi - ctc.po_thresh2,
      across(.cols = contains('excess'),
             .fns  = ~ if_else(ctc.po_discrete == 1, 
                               ceiling(pmax(0, .) / ctc.po_discrete_step) * ctc.po_discrete_step, 
                               pmax(0, .))),
      
      value1      = pmax(0, max_value1      - excess1 * ctc.po_rate1),
      value2      = pmax(0, max_value2      - excess2 * ctc.po_rate2),
      value_other = pmax(0, max_value_other - excess1 * ctc.po_rate1),
      
      # Allocate against liability after select nonrefundable credits
      nonref     = ftc - cdctc_nonref - ed_nonref - savers_nonref - old_cred,
      liab       = pmax(0, liab_bc - nonref),
      ctc_nonref = pmin(liab, value1 + value2 + value_other),
      
      
      #-----------------------------
      # Additional Child Tax Credit
      #-----------------------------
      
      # Calculate unused CTC
      remaining_ctc = value1 + value2 + value_other - ctc_nonref, 
      
      # Limit to max per-child refundable credit value
      max_refund = if_else(is.infinite(ctc.max_refund), Inf, (n_young + n_old) * ctc.max_refund),
      ctc_ref    = pmin(remaining_ctc, max_refund),
      
      # Phase in with earned income
      ctc_ref = pmin(pmax(0, ei - ctc.pi_thresh) * ctc.pi_rate, ctc_ref),
      
      
      #-------------------------------------
      # Allocation for fully refundable CTC
      #-------------------------------------
      
      # Ignore all the previous restrictions if CTC (excluding credit for 
      # other dependents) is fully refundable
      ctc_nonref = if_else(ctc.fully_refundable == 1, 
                           pmin(liab, value_other),
                           ctc_nonref),
      ctc_ref    = if_else(ctc.fully_refundable == 1, 
                           value1 + value2,
                           ctc_ref)
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_ctc)) %>% 
    return()
}

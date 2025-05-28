#----------------------------------------------
# Function to calculate Child Tax Credit (CTC)
#----------------------------------------------

# Set return variables for function
return_vars$calc_ctc = c('ctc_nonref', 'ctc_ref', 'become_filer_ctc')


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
  #  - ctc_nonref       (dbl) : value of CTC including other dependent credit
  #  - ctc_ref          (dbl) : ACTC, i.e. refundable component of the CTC
  #  - become_filer_ctc (int) : for nonfilers, whether induced to file 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_age1',              # (int) age of youngest dependent (NA for tax units without a dependent)
    'dep_age2',              # (int) age of second youngest dependent (NA for tax units without a second dependent)
    'dep_age3',              # (int) age of oldest dependent (NA for tax units without a third dependent)
    'dep_ssn1',              # (bool) whether youngest dependent has a Social Security number (NA for tax units without a dependent)
    'dep_ssn2',              # (bool) whether second youngest dependent has a Social Security number (NA for tax units without a second dependent)
    'dep_ssn3',              # (bool) whether oldest dependent has a Social Security number (NA for tax units without a third dependent)
    'dep_ctc1',              # (bool) whether youngest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a dependent)
    'dep_ctc2',              # (bool) whether second youngest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a second dependent)
    'dep_ctc3',              # (bool) whether oldest dependent qualifies for CTC status based on non-age and non-SSN criteria (NA for tax units without a third dependent)
    'n_dep',                 # (int) number of dependents
    'agi',                   # (dbl) Adjusted Gross Income
    'filing_status',         # (int) filing status of tax unit (1 = single, 2 = joint, 3 = MFS, 4 = HoH)
    'liab_bc',               # (dbl) liability before credits, including AMT   
    'ftc',                   # (dbl) value of foreign tax credit 
    'cdctc_nonref',          # (dbl) value of nonrefundable Child and Dependent Care Credit 
    'ed_nonref',             # (dbl) value of nonrefundable education credit
    'savers_nonref',         # (dbl) value of nonrefundable Saver's Credit
    'old_cred',              # (dbl) value of Elderly and Disabled Credit
    'caregiver_cred_nonref', # (dbl) value of nonrefundable caregiver credit
    'ei',                    # (dbl) earned income
    'ei_prior_yr',           # (dbl) earned income last year       
    'filer',                 # (int) whether tax unit files a tax return
    
    # Tax law attributes
    'ctc.young_age_limit',     # (int) maximum age to qualify as "young" child
    'ctc.old_age_limit',       # (int) maximum age to qualify as "old" child
    'ctc.need_ssn',            # (int) whether SSN is required to qualify for CTC
    'ctc.young_level',         # (int) whether to express value for young children in levels (in which case the calculator uses value_young) or in terms of difference from old (in which case the calculator adds bonus_young to value_old) 
    'ctc.value_young1',        # (int) maximum credit value per "young" child corresponding to phaseout threshold 1 
    'ctc.value_young2',        # (int) maximum credit value per "young" child corresponding to phaseout threshold 2
    'ctc.bonus_young1',        # (int) maximum additional value per "young" child corresponding to phaseout threshold 1 
    'ctc.bonus_young2',        # (int) maximum additional value per "young" child corresponding to phaseout threshold 2
    'ctc.value_old1',          # (int) maximum credit value per "old" child corresponding to phaseout threshold 1
    'ctc.value_old2',          # (int) maximum credit value per "old" child corresponding to phaseout threshold 2
    'ctc.value_other',         # (int) maximum (nonrefundable) credit value per nonqualifying dependent (assumed to phase out with po_thresh1)
    'ctc.po_thresh1',          # (int) AGI threshold above which value 1 (and nonqualifying dependent credit) phases out
    'ctc.po_thresh2',          # (int) AGI threshold above which value 2 phases out
    'ctc.po_type',             # (int) whether phaseout type is a rate (0 means range)
    'ctc.po_rate1',            # (dbl) phaseout rate for value 1
    'ctc.po_rate2',            # (dbl) phaseout rate for value 2 
    'ctc.po_range1',           # (dbl) phaseout range for value 2
    'ctc.po_range2',           # (dbl) phaseout range for value 1
    'ctc.po_range_other',      # (dbl) non-child dependent credit phaseout range for married returns
    'ctc.po_discrete',         # (int) whether phaseout is discretized, as in current-law form
    'ctc.po_discrete_step',    # (int) rounding step for discretized phaseout
    'ctc.mfs_eligible',        # (int) whether married filing separate returns are eligible
    'ctc.min_refund_level',    # (int) whether to express minimum refundable CTC in dollar terms (in which case the calculator uses min_refund) or share of maximum value (in which case it uses min_refund_share)
    'ctc.min_refund_young',    # (int) minimum refundable CTC per young qualifying child
    'ctc.min_refund_old',      # (int) minimum refundable CTC per old qualifying child
    'ctc.min_refund_share',    # (int) minimum refundable CTC as a share of total maximum credit
    'ctc.max_refund_young',    # (int) maximum refundable CTC per young qualifying child
    'ctc.max_refund_old',      # (int) maximum refundable CTC per old qualifying child
    'ctc.ei_prior_yr',         # (int) whether prior year earned income qualifies for phase-in
    'ctc.pi_thresh',           # (int) earned income threshold above which CTC phases in
    'ctc.pi_type',             # (int) whether phase-in type is a rate (0 means range)
    'ctc.pi_rate',             # (dbl) phase-in rate
    'ctc.pi_range',            # (dbl) phase-in range for total CTC (excluding nonqualifying dependent credit) 
    'ctc.baby_bonus',          # (dbl) refundable credit for newborn child
    'ctc.baby_bonus_pi_rate',  # (dbl) phase-in rate for baby bonus
    'ctc.baby_bonus_po_rate',  # (dbl) phase-out rate for baby bonus
    'ctc.baby_bonus_po_thresh' # (dbl) phase-out threshold for baby bonus (AGI)
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
      z1 = dep_ctc1 & (dep_age1 == 0) & (dep_ssn1 | !need_ssn),
      z2 = dep_ctc2 & (dep_age2 == 0) & (dep_ssn2 | !need_ssn), 
      z3 = dep_ctc3 & (dep_age3 == 0) & (dep_ssn3 | !need_ssn),
      
      
      n_young = y1 + y2 + y3,
      n_old   = o1 + o2 + o3 - n_young,
      n_0     = z1 + z2 + z3,
      n_other = n_dep - n_young - n_old,
      
      # Determine value for young children
      value_young1 = if_else(ctc.young_level == 1, ctc.value_young1, ctc.value_old1 + ctc.bonus_young1),
      value_young2 = if_else(ctc.young_level == 1, ctc.value_young2, ctc.value_old2 + ctc.bonus_young2),
      
      # Calculate value before phase-in/out, including nonrefundable credit
      # for other dependents
      max_value1      = (value_young1 * n_young) + (ctc.value_old1 * n_old),
      max_value2      = (value_young2 * n_young) + (ctc.value_old2 * n_old),
      max_value_other = ctc.value_other * n_other,
      max_value_baby  = n_0 * ctc.baby_bonus,
      
      # Determine amount by which income exceeds phaseout thresholds
      excess1 = agi - ctc.po_thresh1,
      excess2 = agi - ctc.po_thresh2, 
      across(.cols = contains('excess'),
             .fns  = ~ if_else(ctc.po_discrete == 1, 
                               ceiling(pmax(0, .) / ctc.po_discrete_step) * ctc.po_discrete_step, 
                               pmax(0, .))),
      
      # Convert range phaseout structure to
      po_rate1      = if_else(ctc.po_type == 1, ctc.po_rate1, max_value1 / ctc.po_range1),
      po_rate2      = if_else(ctc.po_type == 1, ctc.po_rate1, max_value2 / ctc.po_range2),
      po_rate_other = if_else(ctc.po_type == 1, 
                              ctc.po_rate_other, 
                              max_value_other / ctc.po_range_other),
      
      # Apply phaseouts
      value1      = pmax(0, max_value1      - excess1 * po_rate1),
      value2      = pmax(0, max_value2      - excess2 * po_rate2),
      value_other = pmax(0, max_value_other - excess1 * po_rate_other),
      
      # Apply filing status limitations
      across(.cols = c(value1, value2, value_other), 
             .fns = ~ . * (ctc.mfs_eligible != 3 | ctc.mfs_eligible == 1)),
      
      # Allocate against liability after select nonrefundable credits
      nonref     = ftc + cdctc_nonref + ed_nonref + savers_nonref + old_cred + caregiver_cred_nonref,
      liab       = pmax(0, liab_bc - nonref),
      ctc_nonref = pmin(liab, value1 + value2 + value_other),
      
      
      #-----------------------------
      # Additional Child Tax Credit
      #-----------------------------
      
      # Calculate unused CTC
      remaining_ctc = value1 + value2 + value_other - ctc_nonref, 
      
      # Determine minimum refundable credit value
      min_refund = if_else(ctc.min_refund_level == 1, 
                           (n_young * ctc.min_refund_young) + (n_old * ctc.min_refund_old),
                           ctc.min_refund_share * (max_value1 + max_value2)),
      
      # Determine maximum refundable credit value
      max_refund_young = if_else(is.infinite(ctc.max_refund_young), 
                                 Inf, 
                                 n_young * ctc.max_refund_young),
      max_refund_old   = if_else(is.infinite(ctc.max_refund_old), 
                                 Inf, 
                                 n_old * ctc.max_refund_old),
      
      # If allowed, use larger of this year's and last year's earned income
      # for purposes of phase-in
      qual_ei = if_else(ctc.ei_prior_yr == 1, 
                        pmax(ei, ei_prior_yr), 
                        ei),
      
      # Determine phase-in rate 
      pi_rate = if_else(ctc.pi_type == 1, 
                        ctc.pi_rate, 
                        (max_value1 + max_value2) / ctc.pi_range), 
      
      # Phase in with earned income above minimum refund value
      ctc_ref = pmin(min_refund + (pmax(0, qual_ei - ctc.pi_thresh) * pi_rate), 
                     pmin(remaining_ctc, max_refund_young + max_refund_old)),
      
      # Calculate refundable baby bonus and add to refundable CTC
      baby_bonus = pmin(max_value_baby, pmax(0, qual_ei) * ctc.baby_bonus_pi_rate),
      baby_bonus = pmax(0, baby_bonus - pmax(0, agi - ctc.baby_bonus_po_thresh) * ctc.baby_bonus_po_rate),
      
      # Phase in baby bonus
      ctc_ref = ctc_ref + baby_bonus,
      
      # For $0-earning nonfilers, switch filing status if policy offers a refund
      become_filer_ctc = as.integer(filer == 0 & qual_ei == 0 & ctc_ref > 0)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_ctc)) %>% 
    return()
}

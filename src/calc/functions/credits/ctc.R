#----------------------------------------------
# Function to calculate Child Tax Credit (CTC)
#----------------------------------------------


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
  #          - ctc (dbl)  : value of CTC including credit for other dependents
  #          - actc (dbl) : ACTC, i.e. refundable component of the CTC
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'dep_age1', # (int) age of youngest dependent (NA for tax units without a dependent)
    'dep_age2', # (int) age of second youngest dependent (NA for tax units without a second dependent)
    'dep_age3', # (int) age of oldest dependent (NA for tax units without a third dependent)
    'n_dep',    # (int) number of dependents
    'agi',      # (int) Adjusted Gross Income
    
    # Tax law attributes
    'ctc.young_age_limit', # (int) maximum age to qualify as "young" child
    'ctc.old_age_limit',   # (int) maximum age to qualify as "old" child
    'ctc.value_young1',    # (int) maximum credit value per "young" child corresponding to phaseout threshold 1 
    'ctc.value_young2',    # (int) maximum credit value per "young" child corresponding to phaseout threshold 2
    'ctc.value_old1',      # (int) maximum credit value per "old" child corresponding to phaseout threshold 1
    'ctc.value_old2',      # (int) maximum credit value per "old" child corresponding to phaseout threshold 2
    'ctc.value_other',     # (int) maximum (nonrefundable) credit value per nonqualifying dependent (assumed to phase out with po_thresh1)
    'ctc.po_thresh1',      # (int) AGI threshold above which value 1 phases out
    'ctc.po_thresh2',      # (int) AGI threshold above which value 2 phases out
    'ctc.po_rate1',        # (dbl) phaseout rate for value 1
    'ctc.po_rate2',        # (dbl) phaseout rate for value 2 
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      #------------------
      # Child tax credit
      #------------------
      
      # Determine number of qualifying dependents by age
      across(.cols = starts_with('dep_age'), 
             .fns  = ~ replace_na(., 0)),
      across(.cols  = c(ctc.young_age_limit, ctc.old_age_limit), 
             .fns   = list('1' = ~ as.integer(dep_age1 <= .), 
                           '2' = ~ as.integer(dep_age2 <= .), 
                           '3' = ~ as.integer(dep_age3 <= .)), 
             .names = '{str_sub(col, 5, 5)}{fn}'),
  
      n_young = y1 + y2 + y3,
      n_old   = o1 + o2 + o3,
      n_other = n_dep - n_young - n_old,
      
      # Calculate value before phase-in/out, including nonrefundable credit
      # for other dependents
      value1      = (ctc.value_young1 * n_young) + (ctc.value_old1 * n_old),
      value2      = (ctc.value_young2 * n_young) + (ctc.value_old2 * n_old),
      value_other = ctc.value_other * n_other,
      
      # Determine and apply phaseouts
      reduction1 = pmax(0, agi - ctc.po_thresh1) * ctc.po_rate1,
      reduction2 = pmax(0, agi - ctc.po_thresh2) * ctc.po_rate2,
      
      value1      = pmax(0, value1      - reduction1),
      value2      = pmax(0, value2      - reduction2),
      value_other = pmax(0, value_other - reduction1),
      
      
      #-----------------------------
      # Additional Child Tax Credit
      #-----------------------------
      
      
    ) %>% 
    
    # Keep variables to return
    select(ctc) %>% 
    return()
}

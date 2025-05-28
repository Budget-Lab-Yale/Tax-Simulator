#-----------------------------------------------------------
# Function to calculate Qualified Business Income deduction
#-----------------------------------------------------------

# Set return variables for function
return_vars$calc_qbi_ded = c('qbi_ded')


calc_qbi_ded = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates deduction for Qualified Business Income (QBI).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - qbi_ded (dbl) : value of QBI deduction
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'id',                 # (int) tax unit identifier
    'sole_prop',          # (dbl) sole proprietor's net income (Sch. C)
    'part',               # (dbl) net partnership income
    'scorp',              # (dbl) net S corporation income
    'farm',               # (dbl) net farm income (Sch. F)
    'wagebill_sole_prop', # (dbl) W2 wages paid in sole proprietorship
    'wagebill_part',      # (dbl) owner's share of W2 wages paid in partnership
    'wagebill_scorp',     # (dbl) owner's share of W2 wages paid in S corporation
    'wagebill_farm',      # (dbl) owner's share of W2 wages paid in farm business
    'sstb_sole_prop',     # (int) whether sole proprietor's net income is derived from an SSTB 
    'sstb_part',          # (int) whether net partnership income is derived from an SSTB 
    'sstb_scorp',         # (int) whether net S corporation income is derived from an SSTB 
    'sstb_farm',          # (int) whether net farm income is derived from an SSTB 
    'div_pref',           # (dbl) qualified dividends
    'kg_pref',            # (dbl) net capital gain eligible for preferred rates
    'agi',                # (dbl) Adjusted Gross Income
    'std_ded',            # (dbl) value of standard deduction (even if eventually itemizing)
    'item_ded',           # (dbl) value of itemized deductions
    'pe_ded',             # (dbl) value of deduction for personal exemptions 
    
    # Tax law attributes
    'qbi.rate',                    # (dbl) deduction rate
    'qbi.po_thresh_sstb',          # (int) taxable income phaseout threshold for SSTBs 
    'qbi.po_thresh_non_sstb',      # (int) taxable income phaseout threshold for non-SSTBs
    'qbi.po_range_sstb',           # (int) taxable income range for SSTBs 
    'qbi.po_range_non_sstb',       # (int) taxable income range for non-SSTBs
    'qbi.wage_exception_sstb',     # (int) for SSTBs, whether paying wages excepts taxpayer from taxable income phaseout 
    'qbi.wage_exception_non_sstb', # (int) for non-SSTBs, whether paying wages excepts taxpayer from taxable income phaseout 
    'qbi.wage_limit',              # (dbl) share of W2 wage bill required for full deduction after phase-out 
    'qbi.txbl_inc_limit',          # (dbl) share of ordinary taxable income limit
    'qbi.po_type'                  # (int) phaseout type: 0 = TCJA design, 1 = May 2025 House-passed OBBB design
  )
  
  
  tax_unit %<>% 
    
    # Parse tax unit object passed as argument 
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    
    # Derive taxable income without regard to QBI deduction 
    mutate(txbl_inc = pmax(0, agi - pmax(std_ded, item_ded) - pe_ded))
  
  # Calculate QBI deduction for each business 
  qbi_ded = tax_unit %>% 
    
    # Replace NAs with 0s 
    mutate(across(.cols = c(starts_with('wagebill_'), starts_with('sstb_')), 
                  .fns  = ~ replace_na(., 0))) %>% 
    
    # Reshape long in business type (data limitations require that business
    # aggregation happens at the business type level) such that we have three 
    # QBI-related variables per business type: income, wage bill, and SSTB status
    select(all_of(req_vars), txbl_inc) %>% 
    rename_with(.cols = c(starts_with('wagebill_'), starts_with('sstb_')), 
                .fn   = ~ str_replace(., '_', '.')) %>% 
    rename_with(.cols = c(sole_prop, part, scorp, farm), 
                .fn   = ~ paste0('inc.', .)) %>%
    pivot_longer(cols      = c(starts_with('inc.'), 
                               starts_with('wagebill.'), 
                               starts_with('sstb.')), 
                 names_to  = c('series', 'business_type'), 
                 names_sep = '[.]', 
                 values_to = 'value') %>%
    pivot_wider(names_from  = series,
                values_from = value) %>%
    
    # Now, tax calculation by business type
    mutate(
      
      #-------------------------
      # TCJA-style calculation
      #------------------------
      
      # Calculate tentative deduction
      qbi_ded_tent = inc * qbi.rate,
      
      # Determine and apply limitations based on taxable income, SSTB status, 
      # and wages paid. We calibrate the imputation of wages paid such that 
      # it also reflects the other 25% + 2.5% asset limitation. First, 
      # determine the extent to which the taxable income phaseout applies:
      po_thresh = if_else(sstb == 1, qbi.po_thresh_sstb, qbi.po_thresh_non_sstb),
      po_range  = if_else(sstb == 1, qbi.po_range_sstb, qbi.po_range_non_sstb),
      po_share  = pmin(1, pmax(0, txbl_inc - po_thresh) / po_range),
      
      # Determine whether taxpayer is excepted from taxable income phaseout 
      # conditional on paying sufficient wages
      wage_exception = if_else(sstb == 1, 
                               qbi.wage_exception_sstb,
                               qbi.wage_exception_non_sstb),
      
      # Calculate reduction in deduction as a function of whether wage exception 
      # applies and, if so, the degree to which taxpayer has sufficient wages. 
      # (Note that if taxpayer does not have a wage exception per law, wage credit 
      # evaluates to 0 and phaseout fully applies) 
      wage_credit = wagebill * qbi.wage_limit * wage_exception,
      reduction   = po_share * pmax(0, qbi_ded_tent - wage_credit),
      qbi_ded     = pmax(0, qbi_ded_tent - reduction),
      
      #------------------------
      # OBBB-style calculation
      #------------------------
      
      # Step 1: Non-SSTB QBI deduction limited to usual wage + asset test  
      obbb_step1 = pmin(inc * qbi.rate * (sstb == 0), wagebill * qbi.wage_limit), 
      
    ) %>% 
      
    # Reshape wide again
    select(id, business_type, inc, qbi_ded, obbb_step1) %>% 
    pivot_wider(names_from  = business_type, 
                names_sep   = '.',
                values_from = c(inc, qbi_ded, obbb_step1))
  
  
  # Finally, join QBI deduction data back into main tax unit data
  tax_unit %>% 
    left_join(qbi_ded, by = 'id') %>%
    mutate(
      
      # Calculate total QBI deduction across businesses
      qbi_ded = sole_prop.qbi_ded + part.qbi_ded + scorp.qbi_ded + farm.qbi_ded,
      
      #-----------------------------------
      # OBBB-style calculation, continued
      #-----------------------------------
      
      # Aggregate step 1 calculations
      obbb_step1 = sole_prop.obbb_step1 + part.obbb_step1 + scorp.obbb_step1 + farm.obbb_step1,
      
      # Step 2: Phase out based on taxable income for all income, including SSTB
      inc = sole_prop.inc + part.inc + scorp.inc + farm.inc,
      obbb_step2 = pmax(0, (inc * qbi.rate) - (0.75 * pmax(0, txbl_inc - po_thresh))),
      
      # Deduction is larger of step 1 or step 2
      obbb_qbi_ded = pmax(obbb_step1, obbb_step2),
      
      # Only use OBBB calculation if specified
      qbi_ded = if_else(qbi.po_type == 0, qbi_ded, obbb_qbi_ded),
      
      #------------------
      # Final limitation
      #------------------
      
      # Limit deduction to a share of ordinary taxable income
      qbi_ded = pmin(qbi_ded, pmax(0, txbl_inc - div_pref - kg_pref) * qbi.txbl_inc_limit)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_qbi_ded)) %>% 
    return()
}





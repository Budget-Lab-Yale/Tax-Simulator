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
    'sstb_sole_prop',     # (dbl) whether sole proprietor's net income is derived from an SSTB 
    'sstb_part',          # (dbl) whether net partnership income is derived from an SSTB 
    'sstb_scorp',         # (dbl) whether net S corporation income is derived from an SSTB 
    'sstb_farm',          # (dbl) whether net farm income is derived from an SSTB 
    'div_pref',           # (dbl) qualified dividends
    'kg_pref',            # (dbl) net capital gain eligible for preferred rates
    'agi',                # (dbl) Adjusted Gross Income
    'std_ded',            # (dbl) value of standard deduction (even if eventually itemizing)
    'item_ded',           # (dbl) value of itemized deductions
    'pe_ded',             # (dbl) value of deduction for personal exemptions 
    
    # Tax law attributes
    'qbi.rate',       # (dbl) deduction rate
    'qbi.po_thresh',  # (int) taxable income phaseout threshold
    'qbi.po_range',   # (int) taxable income phaseout range
    'qbi.wage_rate'   # (dbl) share of W2 wage bill required for full deduction after phase-out 
    
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
      
      # Calculate tentative deduction
      qbi_ded = inc * qbi.rate,
      
      # Determine and apply limitations based on taxable income, SSTB status, 
      # and wages paid. We calibrate the imputation of wages paid such that 
      # it also reflects the other 25% + 2.5% asset limitation. First, 
      # determine the extent to which the taxable income phaseout applies:
      po_share = pmin(1, pmax(0, txbl_inc - qbi.po_thresh) / qbi.po_range),
      
      # Reduce deduction by the amount by which the QBI deduction exceeds wage
      # credit, scaling by the extent to which the taxable income limitation 
      # applies. Note that for SSTBs, this expression evaluates to 0, effectively 
      # applying the phaseout without regard to wages paid. 
      wage_credit = wagebill * qbi.wage_rate * sstb,
      reduction   = pmin(qbi_ded, po_share * pmax(0, qbi_ded - wage_credit)),
      qbi_ded     = qbi_ded - reduction,
      
    ) %>% 
      
    # Reshape wide again
    select(id, qbi_ded, business_type) %>% 
    pivot_wider(names_from  = business_type, 
                names_glue  = 'qbi_ded_{business_type}',
                values_from = qbi_ded)
  
  
  # Finally, join QBI deduction data back into main tax unit data
  tax_unit %>% 
    left_join(qbi_ded, by = 'id') %>%
    mutate(
      
      # Calculate total QBI deduction across businesses
      qbi_ded = qbi_ded_sole_prop + 
                qbi_ded_part +
                qbi_ded_scorp + 
                qbi_ded_farm,
      
      # Limit deduction to a share of ordinary taxable income
      qbi_ded = pmin(qbi_ded, pmax(0, txbl_inc - div_pref - kg_pref) * qbi.rate)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_qbi_ded)) %>% 
    return()
}





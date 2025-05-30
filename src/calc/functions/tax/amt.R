#---------------------------------------------------------------
# Function to calculate Alternative Minimum Tax (AMT) liability
#---------------------------------------------------------------

# Set return variables for function
return_vars$calc_amt = c('amt_gross_inc', 'amt_txbl_inc', 'liab_amt', 'liab_bc')


calc_amt = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Alternative Minimum Tax (AMT) liability. Also calculates 
  # income liability before credits. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - amt_gross_inc (dbl) : what 6251 calls AMT taxable income
  #   - amt_txbl_inc (dbl)  : AMT taxable income after exemption
  #   - liab_amt (dbl)      : AMT liability
  #   - liab_bc (dbl)       : normal income tax liability (including repayment  
  #                           of excess Premium Tax Credit) plus AMT liability 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',                # (dbl)  Adjusted Gross Income
    'pe_ded',             # (dbl)  deduction for personal exemptions
    'ded',                # (dbl)  itemized deductions if itemizing, else standard deduction
    'std_ded',            # (dbl)  value of standard deduction 
    'qbi_ded',            # (dbl)  value of deduction for Qualified Business Income
    'tip_ded',            # (dbl) value of below-the-line tip deduction
    'ot_ded',             # (dbl) value of below-the-line overtime deduction
    'senior_ded',         # (dbl) value of below-the-line extra senior deduction
    'itemizing',          # (bool) whether filer itemizes deductions
    'salt_item_ded',      # (dbl)  itemized deduction for state and local taxes
    'misc_item_ded',      # (dbl)  miscellaneous itemized deductions
    'item_ded_ex_limits', # (dbl) itemized deductions before overall limitations
    'item_ded',           # (dbl)  total itemized deductions
    'state_ref',          # (dbl)  taxable refunds/credits/offsets of SALT
    'amt_nols',           # (dbl)  NOLs includable in AMT income
    'amt_ftc',            # (dbl)  foreign tax credit for AMT purposes
    'txbl_inc',           # (dbl)  taxable income
    'div_pref',           # (dbl)  qualified dividend income
    'kg_pref',            # (dbl)  preferred-rate capital gains ("net capital gain" in the internal revenue code)  
    'kg_1250',            # (dbl)  section 1250 unrecaptured gain
    'kg_collect',         # (dbl)  collectibles gain
    'liab',               # (dbl)  normal income tax liability
    'excess_ptc',         # (dbl)  repayment of excess advance Premium Tax Credit
    
    # Tax law attributes
    'amt.pe_pref',            # (int)   whether exemptions are a preference item (i.e. added back from taxable income)
    'amt.exempt',             # (int)   AMT exemption
    'amt.exempt_po_thresh',   # (int)   AMT taxable income threshold above which exemption phases out
    'amt.exempt_po_rate',     # (dbl)   exemption phaseout rate
    'amt.rates[]',            # (dbl[]) AMT rate schedule
    'amt.brackets[]',         # (int[]) AMT bracket schedule
    'pref.rates[]',           # (dbl[]) preferred tax rate schedule
    'pref.brackets[]',        # (int[]) brackets for preferred-rate income
    'pref.unrecapture_rate',  # (dbl)   tax rate on Section 1250 unrecaptured gain 
    'pref.collectibles_rate'  # (dbl)   tax rate on collectibles gain
  )
  
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    
    #------------------------------
    # Calculate AMT taxable income
    #------------------------------
    
    mutate(
      
      # Calculate AMT gross income (what the form calls "taxable income") 
      amt_gross_inc = agi -
                      ded -
                      (pe_ded * (amt.pe_pref == 0)) - 
                      qbi_ded - 
                      tip_ded - 
                      ot_ded - 
                      senior_ded + 
                      if_else(itemizing, 
                              salt_item_ded + 
                              misc_item_ded - 
                              (item_ded_ex_limits - item_ded), 
                              std_ded) - 
                      state_ref + 
                      amt_nols,
      
      # Calculate AMT exemption after phaseout
      excess = pmax(0, amt_gross_inc - amt.exempt_po_thresh),
      exempt = pmax(0, amt.exempt - (excess * amt.exempt_po_rate)), 
        
      # Calculate taxable AMT income 
      amt_txbl_inc = pmax(0, amt_gross_inc - exempt)
    
    ) %>% 
    
    #-------------------------
    # Calculate AMT liability
    #-------------------------
    
    # Because we need to allow the preferred-rates component of AMT taxable
    # income to be taxed at preferred rates, we call calc_tax() but using AMT  
    # concepts in place ordinary rates/brackets
    select(-starts_with('ord.rates'), -starts_with('ord.brackets'), -txbl_inc) %>% 
    rename_with(.cols = c(starts_with('amt.rates'), starts_with('amt.brackets')),
                .fn   = ~ paste0('ord', str_sub(., 4))) %>% 
    rename(txbl_inc = amt_txbl_inc) %>%
    
    # Calculate tax on taxable income 
    bind_cols(
      (.) %>% 
        select(-all_of(return_vars$calc_tax)) %>% 
        calc_tax() %>% 
        select(liab_amt_gross = liab)
    ) %>% 
    
    # Apply FTC and determine excess over normal liability
    mutate(liab_amt = pmax(0, liab_amt_gross - amt_ftc - liab), 
           liab_bc  = liab + liab_amt + excess_ptc) %>% 
    
    # Keep variables to return
    rename(amt_txbl_inc = txbl_inc) %>%
    select(all_of(return_vars$calc_amt)) %>% 
    return()
}

#---------------------------------------------------------------
# Function to calculate Alternative Minimum Tax (AMT) liability
#---------------------------------------------------------------


calc_amt = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Alternative Minimum Tax (AMT) liability.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - liab_amt (dbl) : AMT liability
  #   - liab_bc (dbl)  : normal income liability tax plus AMT liability 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',            # (dbl)  Adjusted Gross Income
    'ded',            # (dbl)  itemized deductions if itemizing, else standard deduction
    'std_ded',        # (dbl)  value of standard deduction 
    'qbi_ded',        # (dbl)  value of deduction for Qualified Business Income
    'itemizing',      # (bool) whether filer itemizes deductions
    'salt_item_ded',  # (dbl)  itemized deduction for state and local taxes
    'state_ref',      # (dbl)  taxable refunds/credits/offsets of SALT
    'amt_nols',       # (dbl)  NOLs includible in AMT income
    'amt_other_adj',  # (dbl)  other adjustments to AMT income
    'amt_ftc',        # (dbl)  foreign tax credit for AMT purposes
    'liab',           # (dbl)  normal income tax liability
    
    # Tax law attributes
    'amt.exempt',            # (int)   AMT exemption
    'amt.exempt_po_thresh',  # (int)   AMT taxable income threshold above which exemption phases out
    'amt.exempt_po_rate',    # (dbl)   exemption phaseout rate
    'amt.rates[]',           # (dbl[]) AMT rate schedule
    'amt.brackets[]'         # (int[]) AMT bracket schedule
  )
  
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate AMT gross income (what the form calls "taxable income") 
      amt_gross_inc = agi -
                      ded - 
                      qbi_ded + 
                      if_else(itemizing, salt_item_ded, std_ded) + 
                      state_refunds + 
                      amt_nols + 
                      amt_other_adj,
      
      # Calculate AMT exemption after phaseout
      excess = pmax(0, amt_gross_inc - amt.exempt_po_thresh),
      exempt = pmax(0, amt.exempt - (excess * amt.exempt_po_rate)), 
        
      # Calculate taxable AMT income 
      amt_txbl_inc = pmax(0, amt_gross_inc - exempt),
      
      # TODO calculate tax part...
      liab_amt_gross = -1, 
      
      # Apply FTC and determine excess over normal liability
      liab_amt = pmax(0, liab_amt_gross - liab_ftc - liab),
      
    ) %>% 
    
    # Keep variables to return
    select(liab_amt, liab_bc) %>% 
    return()
}

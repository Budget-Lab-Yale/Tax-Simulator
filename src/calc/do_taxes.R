#----------------------------------------------------------------
# Contains functions to calculate tax liability for all tax units
#----------------------------------------------------------------


do_taxes = function(tax_units) { 

  #----------------------------------------------------------------------------
  # Calculates payroll and individual income taxes for all tax units. Form-
  # behavior optimization in which the filer needs to calculate taxes more
  # than once is performed here (rather than inside of a calc function.) 
  # Currently supports charitable contribution reporting optimization when
  # a deduction is available both above the line and on Schedule A.
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units
  #
  # Returns: tibble of tax units with new columns for calculated tax variables
  #          (df).  
  #----------------------------------------------------------------------------
      
  # Set 1040 return variables
  vars_1040 = c('') # TODO during reporting
  
  # Derive useful policy-independent variables
  tax_units %<>% 
    derive_vars()
  
  
  #---------------
  # Payroll taxes
  #---------------
    
  # Do payroll taxes
  tax_units %<>%
    bind_cols(do_payroll_taxes(.)) 
    
  
  #-------------------------
  # Individual income taxes
  #-------------------------

  # Check whether individual income taxes need to be calculated more than once.
  # If both an above-the-line and an itemized charitable deduction are available,
  # the taxpayer chooses between the two by calculating their taxes twice
  if (any(tax_units$char.above_limit > 0 & tax_units$char.item_limit > 0)) { 
    
    # Force filers to take the above-the-line deduction
    above = tax_units %>% 
      do_1040('liab_iit_net', force_char = T, char_above = T) %>% 
      mutate(char_ded_type = 'above')
    
    # Force filers to take the itemized deduction
    item = tax_units %>% 
      do_1040('liab_iit_net', force_char = T, char_above = F) %>% 
      mutate(char_ded_type = 'item')
    
    # Determine which is better
    opt = above %>% 
      select(ID, above = liab_iit_net) %>% 
      bind_cols(
        item %>% 
          select(ID, item = liab_iit_net)
      ) %>% 
      mutate(char_ded_type = if_else(above <= item), 'above', 'item') %>% 
      select(char_ded_type)
    
    # Select optimized answer
    tax_units %<>%
      bind_cols(opt) %>%
      left_join(bind_rows(above, item), by = c('ID', 'char_ded_type'))
  
    
  # Standard case: just calculate the 1040 once  
  } else {
    tax_units %<>% 
      bind_cols(do_1040(., return_vars))
  }
  
  
  return(tax_units)
}



do_payroll_taxes = function(tax_units) {
  
  #----------------------------------------------------------------------------
  # Calculates payroll taxes for all tax units. Currently just a wrapper for 
  # calc_pr(), but written this way for consistency with do_1040().
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units
  #
  # Returns: tibble of tax units with return variables from calc_pr().
  #----------------------------------------------------------------------------
  
  tax_units %>% 
    calc_pr() %>%
    return()
}



do_1040 = function(tax_units, return_vars, force_char = F, char_above = F) {
  
  #----------------------------------------------------------------------------
  # Calculates individual income taxes for all tax units. 
  # 
  # Parameters:
  #   - tax_units (df)      : tibble of tax unita
  #   - return_vars (str[]) : vector of (calculated) names of variables to 
  #                           return  
  #   - force_char (bool)   : whether the calculator forces filers to report 
  #                           their charitable contribution in a specific,
  #                           mutally exclusive place on the 1040
  #   - char_above (bool)   : if forcing charitable contribution reporting, 
  #                           whether to report as an above-the-line deduction 
  #                           (F indicates reporting as itemized deduction)
  #
  # Returns: tibble of tax units with
  #----------------------------------------------------------------------------
  
  
  # Create tibble-length vectors for form-behavior booleans 
  force_char = rep(force_char, nrow(tax_unit))
  char_above = rep(char_above, nrow(tax_unit))
  
  
  tax_units %>% 
    
    # Create duplicates for variables affected by form-behavior optimization
    mutate(across(.cols  = c(char_cash, char_noncash), 
                  .fns   = ~ ., 
                  .names = '{col}_')) %>%  
    
    #-----------------------
    # Adjusted gross income
    #-----------------------
    
    # Net capital gain includable in AGI
    bind_cols(calc_kg(.)) %>% 
    
    # AGI, including taxable OASI benefits
    mutate(across(.cols = c(char_cash, char_noncash), 
                  .fns  = ~ if_else(force_char & !char_above, 0, .))) %>% 
    bind_cols(calc_agi(.)) %>% 
    mutate(char_cash    = care_cash_, 
           char_noncash = care_noncash_) %>% 
    
    
    #----------------
    # Taxable income 
    #----------------
    
    # Standard deduction
    bind_cols(calc_std_ded(.)) %>% 
    
    # Itemized deductions
    mutate(across(.cols = c(char_cash, char_noncash), 
                  .fns  = ~ if_else(force_char & char_above, 0, .))) %>%
    bind_cols(calc_item_ded(.)) %>% 
    mutate(char_cash    = care_cash_, 
           char_noncash = care_noncash_) %>% 
    
    # Personal exemptions
    bind_cols(calc_pe_ded(.)) %>% 
    
    # QBI deduction
    bind_cols(calc_qbi_ded(.)) %>% 
    
    # Taxable income
    bind_cols(calc_txbl_inc(.)) %>% 
    
    
    #--------------------------
    # Liability before credits
    #--------------------------
  
    # Liability
    bind_cols(calc_tax(.)) %>%
    
    # Alternative minimum tax
    bind_cols(calc_amt(.)) %>% 
    
    
    #---------
    # Credits
    #---------
    
    # CDCTC
    bind_cols(calc_cdctc(.)) %>% 
    
    # Education credits
    bind_cols(calc_ed_cred(.)) %>% 
    
    # Saver's credit
    bind_cols(calc_savers_cred(.)) %>% 
    
    # CTC
    bind_cols(calc_ctc(.)) %>% 
    
    # EITC
    bind_cols(calc_eitc(.)) %>% 
    
    # Rebates / UBI
    bind_cols(calc_rebate(.)) %>%
    
    #----------------------
    # Liability allocation
    #----------------------
    
    # NIIT
    bind_cols(calc_niit(.)) %>% 
    
    # Liability
    bind_cols(calc_liab(.)) %>% 
    
    # Select variables and return
    select(all_of(return_vars)) %>%
    return()

} 






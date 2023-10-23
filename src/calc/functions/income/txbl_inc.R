#--------------------------------------
# Function to determine taxable income
#--------------------------------------

# Set return variables for function
return_vars$calc_txbl_inc = c('itemizing', 'ded', 'txbl_inc')


calc_txbl_inc = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Determines whether filer takes standard or itemized deductions, and 
  # calculates taxable income. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #          - itemizing (bool) : whether filer itemizes deductions
  #          - ded       (dbl)  : standard deduction or itemized deduction if 
  #                               itemizing
  #          - txbl_inc (dbl)   : taxable income 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',      # (dbl) Adjusted Gross Income
    'std_ded',  # (dbl) value of standard deduction 
    'item_ded', # (dbl) value of itemized deductions
    'pe_ded',   # (dbl) value of deduction for personal exemptions 
    'qbi_ded'   # (dbl) value of deduction for Qualified Business Income
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Determine itemizing status
      itemizing = item_ded > std_ded,
      ded       = pmax(std_ded, item_ded), 
      
      # Calculate taxable income
      txbl_inc = pmax(0, agi - ded - pe_ded - qbi_ded)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_txbl_inc)) %>% 
    return()
}

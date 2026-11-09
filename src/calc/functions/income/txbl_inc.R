#--------------------------------------
# Function to determine taxable income
#--------------------------------------

# Set return variables for function
return_vars$calc_txbl_inc = c('item_ded_limited', 'itemizing', 'char_nonitem_ded', 
                               'ded', 'txbl_inc')


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
  #          - char_nonitem_ded (dbl) : charitable deduction of a filer who
  #                                     does not itemize
  #          - ded       (dbl)  : standard deduction or itemized deduction if 
  #                               itemizing
  #          - txbl_inc (dbl)   : taxable income 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',           # (dbl) Adjusted Gross Income
    'std_ded',       # (dbl) value of standard deduction 
    'item_ded',      # (dbl) value of itemized deductions
    'salt_item_ded', # (dbl) value of SALT deduction (post-limitation)
    'pe_ded',        # (dbl) value of deduction for personal exemptions 
    'qbi_ded',       # (dbl) value of deduction for Qualified Business Income
    'tip_ded',       # (dbl) value of below-the-line tip deduction
    'ot_ded',        # (dbl) value of below-the-line overtime deduction
    'senior_ded',    # (dbl) value of below-the-line extra senior deduction
    'auto_int_ded',  # (dbl) value of below-the-line auto loan interest deduction
    'char_cash',     # (dbl) cash charitable contributions
    'char_above_ded', # (dbl) above-the-line charitable deduction
    
    # Tax law attributes
    'item.limit_tax_value_thresh',     # (dbl) tax value limitation reduction threshold
    'item.salt_limit_tax_value_rate',  # (dbl) tax value limitation reduction rate
    'item.limit_tax_value_rate',       # (dbl) tax value limitation reduction rate, non-SALT deductions
    'char.nonitem_limit'               # (dbl) maximum charitable deduction for a filer who does not itemize
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Claw back itemized deductions based on tax value
      txbl_inc_ex_item_ded = agi - pe_ded - qbi_ded,
      salt_excess      = pmin(salt_item_ded, pmax(0, txbl_inc_ex_item_ded - item.limit_tax_value_thresh)),
      other_excess     = pmin(pmax(0, item_ded - salt_item_ded), pmax(0, txbl_inc_ex_item_ded - salt_item_ded - item.limit_tax_value_thresh)),
      salt_reduction   = salt_excess  * item.salt_limit_tax_value_rate,
      other_reduction  = other_excess * item.limit_tax_value_rate,
      item_ded_limited = item_ded - salt_reduction - other_reduction,
      
      # Calculate charitable deduction for a filer who does not itemize (IRC
      # 170(p)), net of any gift already deducted above the line
      char_nonitem = pmin(pmax(0, char_cash - char_above_ded), char.nonitem_limit),
      
      # Determine itemizing status. The filer gives up the charitable
      # deduction by itemizing
      itemizing        = item_ded_limited > std_ded + char_nonitem,
      ded              = pmax(std_ded + char_nonitem, item_ded_limited),
      char_nonitem_ded = if_else(itemizing, 0, char_nonitem),
      
      # Allocate senior deduction depending on itemizing status
      std_ded    = std_ded + if_else(!itemizing, senior_ded, 0), 
      ded        = ded     + if_else(!itemizing, senior_ded, 0), 
      senior_ded = itemizing * senior_ded,
      
      # Calculate taxable income
      txbl_inc = pmax(0, agi - ded - pe_ded - qbi_ded - tip_ded - ot_ded - senior_ded -
                         auto_int_ded)
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_txbl_inc)) %>% 
    return()
}

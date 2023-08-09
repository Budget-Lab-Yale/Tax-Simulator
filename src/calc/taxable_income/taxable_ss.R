#--------------------------------------------------------
# Function to calculate taxable portion of OASI benefits
#--------------------------------------------------------


calc_taxable_ss = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # TODO
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  req_vars = c(
    'gross_ss',        # (numeric, tax unit) TODO
    'other_inc',     # (numeric, tax unit) TODO
    'exempt_int',    # 
    'above_ded',     # 
    'ss.rates',     # (numeric, tax law)
    'ss.brackets'   # 
  )
  
  tax_unit %>% 
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      
      # modified AGI
      magi = pmax(0, (gross_ss * ss.rates2) + other_inc + exempt_int - above_ded),
      
      
      # magi in excess of 0% bracket 
      line9 = pmax(0, magi - ss.brackets2),
      
      line10 = ss.brackets3 - ss.brackets2,
      
      # magi in excess of 85% bracket
      line11 = pmax(0, line9 - line10),
      
      line12 = pmin(line9, line10),
      
      # benefits included at 50% rate 
      line13 = ss.rates2 * line12, 
      
      # 50%-rate benefits -- which could be based on MAGI -- are limited to 50% of ss benefits 
      line14 = pmin(gross_ss * ss.rates2, line13),
      
      # benefits included at 85% rate
      line15 = line11 * ss.rates3,
      
      # limit taxable benefits to top rate applied to raw gross SS
      taxable_ss = pmin(line14 + line15, gross_ss * ss.rate3)
      
      # .maybe just keep this one explicit. it's kinda weird with the brackets. or copy integrate_...logic !
      
    ) 
}

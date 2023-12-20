#--------------------------------------------------------
# Function to calculate Net Investment Income Tax (NIIT)
#--------------------------------------------------------

# Set return variables for function
return_vars$calc_niit = c('liab_niit')


calc_niit = function(tax_unit, fill_missings = FALSE) {
  
  #----------------------------------------------------------------------------
  # Calculates Net Investment Income Tax (NIIT).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe with the following variables:
  #   - liab_niit (dbl) : amount of Net Investment Income Tax liability
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'txbl_int',          # (dbl) taxable interest income
    'div_ord',           # (dbl) non-qualified dividend income
    'div_pref',          # (dbl) qualified dividend income
    'txbl_kg',           # (dbl) net capital gain included in AGI
    'sch_e',             # (dbl) Schedule E net income
    'scorp_active',      # (dbl) active S corp income
    'scorp_active_loss', # (dbl) active S corp loss 
    'scorp_179',         # (dbl) S corp section 179 deduction
    'part_active',       # (dbl) active partnership income 
    'part_active_loss',  # (dbl) active partnership loss 
    'part_179',          # (dbl) partnership section 179 deduction
    'inv_int_item_ded',  # (dbl) itemized deduction for investment interest expense
    'agi',               # (dbl) Adjusted Gross Income
    
    # Tax law attributes
    'niit.include_active', # (int)   whether active earnings of pass-through 
                           #         businesses are subject to the NIIT
    'niit.rates[]',        # (dbl[]) NIIT rate schedule
    'niit.brackets[]'      # (int[]) NIIT brackets
  )
  
  tax_unit %>%
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%
    
    mutate(
      
      # Floor AGI at 0
      agi = pmax(0, agi),
      
      # Calculate net active earnings of pass-through income included on
      # Schedule E, which are netted out under current law (line 4b of Form 8960)
      net_active_bus = scorp_active - scorp_active_loss - scorp_179 +
                       part_active  - part_active_loss  - part_179,
      
      # Determine whether active business earnings are deductible
      net_active_ded = if_else(niit.include_active == 1, 0, net_active_bus),
      
      # Calculate net investment income
      nii = pmax(0, txbl_int + div_ord + div_pref + txbl_kg + sch_e - 
                    net_active_ded - inv_int_item_ded),
      
      # TEMP: scale down by a factor to match actuals. This is a rough
      # calibration factor to reflect gaps in the tax base not easily extractable 
      # from PUF data. A future version will model these differences more fundamentally.
      nii = nii * 0.7
    
    ) %>%
    
    # Calculate NIIT liability
    bind_cols(
      integrate_conditional_rates_brackets(
        df              = .,
        n_brackets      = NULL, 
        prefix_brackets = 'niit.brackets', 
        prefix_rates    = 'niit.rates', 
        y               = 'agi', 
        x               = 'nii', 
        inclusive       = T,
        output_name     = 'liab_niit', 
        by_bracket      = F
      )
    ) %>%
    
    # Keep variables to return
    select(all_of(return_vars$calc_niit)) %>%
    return()
}

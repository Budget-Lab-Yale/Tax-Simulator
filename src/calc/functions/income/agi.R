#---------------------------------------------------------
# Function to AGI and its derived intermediate components
#---------------------------------------------------------


calc_agi = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates Adjusted Gross Income (AGI). 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - TODO
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    
    'wages',          # (dbl) W2 wages after pre-tax deductions
    'txbl_int',       # (dbl) taxable interest income 
    'exempt_int',     # (dbl) tax-exempt interest income
    'div',            # (dbl) total dividend income
    'txbl_ira_dist',  # (dbl) taxable IRA distributions
    'txbl_pens_dist', # (dbl) taxable DB and DC pension distributions plus
                      #       annuity payments
    'gross_ss',       # (dbl) gross OASI benefits
    'txbl_kg',        # (dbl) net capital gain includable in AGI
    'state_ref',      # (dbl) taxable refunds/credits/offsets of SALT
    'alimony',        # (dbl) taxable alimony income
    'divorce_year',   # (int) year of divorce if applicable
    'sole_prop',      # (dbl) sole proprietor's net income (Sch. C)
    'sch_e',          # (dbl) net partnership, S corp, rental, royalty, estate,
                      #       and trust income (Sch E.)
    'farm',           # (dbl) net farm income (Sch. F)
    'ui',             # (dbl) gross unemployment benefits
    'nols',           # (dbl) net operating losses
    'other',          # (dbl) all other income sources: gambling, debt 
                      #       cancellation, etc. See Sch. 1
    
    # Tax law attributes
    
    'agi.alimony_repeal_year' # (int)
  )
  
  tax_unit %>% 
   
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      
      # Determine whether alimony is includable TODO
      txbl_alimony = alimony * (divorce_year < agi.alimony_repeal_year),
      
      # Calculate gross income excluding OASI benefits
      inc_ex_ss = wages + txbl_int + div + txbl_ira_dist + txbl_pens_dist + 
                  txbl_kg + txbl_alimony + sole_prop + sch_e + farm + ui - nols + 
                  other,
      
      # Calculate above-the-line deductions
      alimony_ded = alimony_paid * (divorce_year < agi.alimony_repeal_year),
      above_ded = ...
      
    ) %>% 
    
    # Calculate taxable social security benefits 
    bind_cols(calc_taxable_ss(.)) %>% 
    mutate(
      
      # Calculate gross income and AGI
      gross_inc = inc_ex_ss + taxable_ss, 
      agi       = gross_inc - above_ded
      
    ) %>%
    
    # Keep variables to return
    select(txbl_ss, txbl_alimony, gross_inc, alimony_ded, above_ded, agi) %>% 
    return()
}


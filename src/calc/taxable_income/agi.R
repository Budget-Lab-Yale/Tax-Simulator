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
    'wages',              # (dbl, self) W2 wages after pre-tax deductions
    'txbl_int',           # (dbl, self) taxable interest income 
    'exempt_int',         # (dbl, self) tax-exempt interest income
    'div',                # (dbl, self) total dividend income
    'txbl_ira_dist',      # (dbl, self) taxable IRA distributions
    'txbl_pens_dist',     # (dbl, self) taxable DB and DC pension distributions plus annuity payments
    'gross_ss',           # (dbl, self) gross OASI benefits
    'txbl_kg',            # (dbl, self) net capital gain includable in AGI
    'state_ref',          # (dbl, self) taxable refunds/credits/offsets of SALT
    'alimony',            # (dbl, self) taxable alimony income
    'divorce_year',       # (int, self) year of divorce if applicable
    'sole_prop',          # (dbl, self) sole proprietor's net income (Sch. C)
    'part_active',        # (dbl, self) active partnership net income
    'part_passive',       # (dbl, self) passive partnership net income
    'part_active_loss',   # (dbl, self) active partnership net loss
    'part_passive_loss',  # (dbl, self) passive partnership net loss
    'part_179',           # (dbl, self) partnership section 179 deduction
    'scorp_active',       # (dbl, self) active S corporation net income
    'scorp_passive',      # (dbl, self) passive S corporation net income
    'scorp_active_loss',  # (dbl, self) active S corporation net loss
    'scorp_passive_loss', # (dbl, self) passive S corporation net loss
    'scorp_179',          # (dbl, self) S corporation section 179 deduction
    'rent',               # (dbl, self) rent and royalty net income
    'rent_loss',          # (dbl, self) rent and royalty net loss
    'estate',             # (dbl, self) estate and trust net income
    'estate_loss',        # (dbl, self) estate and trust net loss
    'farm',               # (dbl, self) net farm income (Sch. F)
    'ui',                 # (dbl, self) gross unemployment benefits
    'nols',               # (dbl, self) net operating losses
    'other',              # (dbl, self) all other income sources: gambling, debt cancellation, etc. See Sch. 1
    'agi.alimony_repeal_year' # (int, year)
  )
  
  tax_unit %>% 
   
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # Calculate several convenient derived variables
      part       = part_active + part_passive - part_active_loss - 
                   part_passive_loss - part_179,
      scorp      = scorp_active + scorp_passive - scorp_active_loss - 
                   scorp_passive_loss - scorp_179,
      part_scorp = part + scorp,
      pt         = part + scorp + sole_prop,
      sch_e      = part_scorp + rent + rent_loss + estate + estate_loss,
      
      # Determine whether alimony is includable TODO
      txbl_alimony = alimony * (divorce_year < agi.alimony_repeal_year),
      
      # Calculate gross income excluding OASI benefits
      inc_ex_ss = wages + txbl_int + div + txbl_ira_dist + txbl_pens_dist + 
                  txbl_kg + txbl_alimony + sole_prop + sch_e + ui - nols + other,
      
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
    select(part, scorp, part_scorp, pt, sch_e, txbl_ss, txbl_alimony, gross_inc, 
           alimony_ded, above_ded, agi) %>% 
    return()
}


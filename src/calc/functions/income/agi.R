#---------------------------------------------------------
# Function to AGI and its derived intermediate components
#---------------------------------------------------------

# Set return variables for function
return_vars$calc_agi = c('txbl_ss', 'excess_bus_loss', 'char_above_ded', 
                         'auto_int_ded', 'gross_inc', 'above_ded', 'agi')


calc_agi = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates Adjusted Gross Income (AGI). 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - txbl_ss        (dbl) Social Security benefits included in AGI
  #   - char_above_ded (dbl) above-the-line charitable deduction
  #   - gross_inc      (dbl) gross income
  #   - above_ded      (dbl) above-the-line deductions
  #   - agi            (dbl) Adjusted Gross Income
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'wages',           # (dbl) W2 wages after pre-tax deductions
    'txbl_int',        # (dbl) taxable interest income 
    'exempt_int',      # (dbl) tax-exempt interest income
    'div_ord',         # (dbl) non-qualified dividend income
    'div_pref',        # (dbl) qualified dividend income
    'txbl_ira_dist',   # (dbl) taxable IRA distributions
    'txbl_pens_dist',  # (dbl) taxable DB and DC pension distributions plus annuity payments
    'gross_ss',        # (dbl) gross OASI benefits
    'txbl_kg',         # (dbl) net capital gain includable in AGI
    'other_gains',     # (dbl) capital gain distributions
    'state_ref',       # (dbl) taxable refunds/credits/offsets of SALT
    'alimony',         # (dbl) alimony income
    'divorce_year',    # (int) year of divorce if applicable
    'sole_prop',       # (dbl) sole proprietor's net income (Sch. C)
    'sch_e',           # (dbl) net partnership, S corp, rental, royalty, estate, and trust income (Sch E.)
    'pt',              # (dbl) pass-through net income
    'farm',            # (dbl) net farm income (Sch. F)
    'ui',              # (dbl) gross unemployment benefits
    'other_inc',       # (dbl) all other income sources: NOLs, gambling, debt cancellation, etc. See Sch. 1
    'ed_exp',          # (dbl) educator expenses
    'hsa_contr',       # (dbl) pretax contributions to an HSA
    'liab_seca_er',    # (dbl) "employer"-side SECA liability
    'trad_contr_ira',  # (dbl) pretax contributions to an IRA
    'sl_int_ded',      # (dbl) student loan interest deduction
    'keogh_contr',     # (dbl) contributions to SEP plans and KOEGH accounts
    'se_health',       # (dbl) self-employed health insurance premiums paid
    'early_penalty',   # (dbl) penalty on early withdrawal from retirement account
    'alimony_exp',     # (dbl) alimony paid
    'tuition_ded',     # (dbl) deductible tuition and fees (pre-2021 definition)
    'dpad',            # (dbl) domestic production activities deduction
    'char_cash',       # (dbl) charitable contributions made in cash
    'char_noncash',    # (dbl) noncash charitable contributions 
    'other_above_ded', # (dbl) other deductions per Schedule 1 line 24
    'auto_int_exp',    # (dbl) auto loan interest expense
    'r.bus_loss',      # (dbl) random number for determining excess business loss limitation eligibility
    'r.new_car',       # (dbl) random number for determining whether auto loan interest is attributable to a new car
    
    # Tax law attributes
    'agi.alimony_repeal_year',    # (int) year during and after which a divorce does not generate taxable/deductible alimony
    'agi.bus_loss_limit',         # (int) maximum deductible business loss
    'agi.sl_limit',               # (int) maximum deductible student loan interest
    'agi.sl_po_thresh',           # (int) MAGI phaseout threshold for student loan interest deduction
    'agi.sl_po_range',            # (int) MAGI phaseout range for student loan interest deduction
    'agi.tuition_ded_limit',      # (int) limit on tuition and feeds deduction 
    'agi.dpad_limit',             # (int) limit on domestic production activities deduction
    'char.above_limit',           # (int) maximum deductible above-the-line charitable contributions
    'agi.auto_int_ded_limit',     # (int) maximum amount of auto loan interest deductible from gross income
    'agi.auto_int_ded_po_thresh', # (int) threshold above which auto loan interest deduction begins to phase out
    'agi.auto_int_ded_po_rate',   # (int) phaseout rate for auto loan interest deduction
    'agi.auto_int_ded_new'        # (int) whether auto interest is only deductible for new cars
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Calculate gross income excluding OASI benefits
      alimony_qualifies = !is.na(divorce_year) & (divorce_year < agi.alimony_repeal_year),
      inc_ex_ss = wages + 
                  txbl_int + 
                  div_ord +
                  div_pref + 
                  state_ref + 
                  txbl_ira_dist + 
                  txbl_pens_dist + 
                  txbl_kg + 
                  other_gains + 
                  alimony * alimony_qualifies + 
                  sole_prop + 
                  sch_e + 
                  farm +
                  ui +
                  other_inc,
      
      # Calcualte excess business losses. (10% is calibrated to JCT's score.)
      # (Excess losses are NOT denied in this model. Rather, we calculate excess
      # losses for use as input into off-model estimate.) 
      excess_bus_loss = (r.bus_loss < 0.1) * pmax(0, -pt - agi.bus_loss_limit),

      # Calculate above-the-line deductions, excluding student loan interest deduction 
      char_above_ded  = pmin(char.above_limit, char_cash + char_noncash),
      above_ded_ex_sl = ed_exp + 
                        hsa_contr + 
                        liab_seca_er + 
                        keogh_contr + 
                        se_health + 
                        early_penalty + 
                        alimony_exp * alimony_qualifies + 
                        trad_contr_ira +
                        pmin(tuition_ded, agi.tuition_ded_limit) + 
                        pmin(dpad, agi.dpad_limit),

      # Calculate auto loan interest deduction
      # (source for new car share of financing: Experian's State of the Automotive Finance Market, 2024)
      magi         = inc_ex_ss + gross_ss - above_ded_ex_sl,
      auto_int_ded = pmin(agi.auto_int_ded_limit, auto_int_exp),
      auto_int_ded = pmax(0, auto_int_ded - pmax(0, magi - agi.auto_int_ded_po_thresh) * agi.auto_int_ded_po_rate),
      auto_int_ded = auto_int_ded * as.integer((agi.auto_int_ded_new == 0) | (r.new_car < 0.58)),
      
      above_ded_ex_sl = above_ded_ex_sl + auto_int_ded,
                      
      # Calculate MAGI for taxable Social Security benefits calculation
      magi_ss = inc_ex_ss - above_ded_ex_sl
      
    ) %>% 
    
    # Calculate taxable social security benefits 
    bind_cols(calc_ss(.)) %>% 
    
    # Put all the pieces together
    mutate(gross_inc = inc_ex_ss + txbl_ss,
           above_ded = above_ded_ex_sl + sl_int_ded,
           agi       = gross_inc - above_ded) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_agi)) %>% 
    return()
}



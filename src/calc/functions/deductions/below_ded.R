#-----------------------------------------------
# Function to other "below the line" deductions
#-----------------------------------------------

# Set return variables for function
return_vars$calc_below_ded = c('tip_ded', 'ot_ded', 'senior_ded', 'auto_int_ded')


calc_below_ded = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates "below the line" deductions -- those which are deducted from 
  # AGI to get to taxable income -- other than the standard deduction, 
  # itemized deductions, and the QBI deduction. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - std_ded (dbl) : value of standard deduction (even if itemizing)
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'tips1',     # (dbl) tipped income included in wages, secondary earner
    'tips2',     # (dbl) tipped income included in wages, primary earner
    'wages1',    # (dbl) wages of primary earner
    'wages2',    # (dbl) wages of secondary earner (0 for non-joint returns)
    'tips_lh1',  # (int) whether tips1 is earned in a leisure and hospitality business
    'tips_lh2',  # (int) whether tips2 is earned in a leisure and hospitality business
    'agi',       # (dbl) Adjusted Gross Income
    'ot1',       # (dbl) FLSA-eligible overtime income included in wages, primary earner
    'ot2',       # (dbl) FLSA-eligible overtime income included in wages, secondary earner
    'age1',      # (int) age of primary filer
    'age2',      # (int) age of secondary filer
    'auto_int_exp', # (dbl) auto loan interest expense
    'r.new_car',    # (dbl) random number for determining whether auto loan interest is attributable to a new car
    
    # Tax law attributes
    'below.tip_ded_wage_limit',   # (int) wage earnings level above which tip deduction is denied
    'below.tip_ded_lh',           # (int) whether tips deduction is limited to leisure and hospitality workers only
    'below.tip_ded_limit',        # (int) maximum deductible OT, non-joint returns
    'below.tip_ded_po_thresh',    # (int) AGI threshold for OT deduction phaseout, non-joint returns
    'below.tip_ded_po_type',      # (int) whether OT phaseout type is a rate (0 means range)
    'below.tip_ded_po_rate',      # (int) phaseout rate for OT deduction
    'below.tip_ded_po_range',     # (int) phaseout range for OT deduction
    'below.ot_ded_wage_limit',    # (int) wage earnings level above which OT deduction is denied
    'below.ot_ded_half',          # (int) whether OT deduction applies only to the "half" of "time and a half"
    'below.ot_ded_limit',         # (int) maximum deductible OT, non-joint returns
    'below.ot_ded_po_thresh',     # (int) AGI threshold for OT deduction phaseout, non-joint returns
    'below.ot_ded_po_type',       # (int) whether OT phaseout type is a rate (0 means range)
    'below.ot_ded_po_rate',       # (int) phaseout rate for OT deduction
    'below.ot_ded_po_range',      # (int) phaseout range for OT deduction
    'below.senior_ded_value',     # (int) value of senior bonus deduction
    'below.senior_ded_po_thresh', # (int) AGI threshold for phaseout of senior bonus deduction
    'below.senior_ded_po_rate',   # (int) phaseout rate for senior bonus deduction
    'agi.auto_int_ded_limit',     # (int) maximum deductible auto loan interest
    'agi.auto_int_ded_po_thresh', # (int) MAGI threshold above which the auto loan interest deduction phases out
    'agi.auto_int_ded_po_rate',   # (int) phaseout rate for auto loan interest deduction
    'agi.auto_int_ded_po_step',   # (int) MAGI step the phaseout rounds up to
    'agi.auto_int_ded_new'        # (int) whether auto interest is only deductible for new cars
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      #---------------
      # Tip deduction
      #---------------
      
      # Limit based on wages ("highly compensated employee" rules)
      tip_ded1 = if_else(wages1 > below.tip_ded_wage_limit, 0, tips1),
      tip_ded2 = if_else(wages2 > below.tip_ded_wage_limit, 0, tips2),
      
      # Limit to leisure and hospitality industry tips if applicable 
      tips_lh = tip_ded1 * tips_lh1 + tip_ded2 * tips_lh2,
      tip_ded = if_else(below.tip_ded_lh == 1, tips_lh, tip_ded1 + tip_ded2),
      
      # Limit to maximum value
      tip_ded = pmin(tip_ded, below.tip_ded_limit),
      
      # Limit based on AGI phaseout
      po_rate = if_else(below.tip_ded_po_type == 1, below.tip_ded_po_rate, tip_ded / below.tip_ded_po_range),
      tip_ded = pmax(0, tip_ded - pmax(0, agi - below.tip_ded_po_thresh) * po_rate),
      
      
      #--------------------
      # Overtime deduction
      #--------------------
      
      # Limit based on wages ("highly compensated employee" rules)
      ot_ded1 = if_else(wages1 > below.ot_ded_wage_limit, 0, ot1),
      ot_ded2 = if_else(wages2 > below.ot_ded_wage_limit, 0, ot2),
      ot_ded  = ot_ded1 + ot_ded2,
      
      # Limit to the "half" portion of "time and a half" if applicable
      ot_ded = ot_ded / (1 + (2 * below.ot_ded_half)),
      
      # Limit to maximum value
      ot_ded = pmin(ot_ded, below.ot_ded_limit),
      
      # Limit based on AGI phaseout
      po_rate = if_else(below.ot_ded_po_type == 1, below.ot_ded_po_rate, ot_ded / below.ot_ded_po_range),
      ot_ded  = pmax(0, ot_ded - pmax(0, agi - below.ot_ded_po_thresh) * po_rate),

      
      #------------------
      # Senior deduction
      #------------------
      
      # Count number of seniors
      n_seniors = as.integer(age1 >= 65) + as.integer(!is.na(age2) & (age2 >= 65)),
      
      # Limit to maximum value
      senior_ded = below.senior_ded_value * n_seniors,
      
      # Limit based on AGI phaseout
      po_rate    = n_seniors * below.senior_ded_po_rate,
      senior_ded = pmax(0, senior_ded - pmax(0, agi - below.senior_ded_po_thresh) * po_rate),
      
      
      #-----------------------------------
      # Qualified passenger vehicle loan
      # interest deduction (IRC 163(h)(4))
      #-----------------------------------
      
      # A below-the-line deduction (Schedule 1-A line 30): it reduces taxable
      # income for itemizers and non-itemizers alike but is NOT part of AGI.
      # The phaseout tests MAGI, which is AGI plus the foreign earned income
      # and possession exclusions the model does not carry, so AGI here. The
      # parameters keep their historical agi.* names, which reforms and the
      # auto/hanson behavioral module reference
      # (source for new car share of financing: Experian's State of the Automotive Finance Market, 2024)
      auto_int_ded    = pmin(agi.auto_int_ded_limit, auto_int_exp),
      auto_int_excess = pmax(0, agi - agi.auto_int_ded_po_thresh),
      auto_int_excess = ceiling(auto_int_excess / agi.auto_int_ded_po_step) * agi.auto_int_ded_po_step,
      auto_int_ded    = pmax(0, auto_int_ded - auto_int_excess * agi.auto_int_ded_po_rate),
      auto_int_ded = auto_int_ded * as.integer((agi.auto_int_ded_new == 0) | (r.new_car < 0.58))
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_below_ded)) %>% 
    return()
}


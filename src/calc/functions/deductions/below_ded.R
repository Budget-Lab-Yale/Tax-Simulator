#-----------------------------------------------
# Function to other "below the line" deductions
#-----------------------------------------------

# Set return variables for function
return_vars$calc_below_ded = c('tip_ded', 'ot_ded', 'senior_ded')


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
    'tips',      # (dbl) tipped income included in wages
    'tips1',     # (dbl) tipped income included in wages, secondary earner
    'tips2',     # (dbl) tipped income included in wages, primary earner
    'tips_lh1',  # (int) whether tips1 is earned in a leisure and hospitality business
    'tips_lh2',  # (int) whether tips2 is earned in a leisure and hospitality business
    'ot',        # (dbl) FLSA-eligible overtime income included in wages
    'agi',       # (dbl) Adjusted Gross Income

    # Tax law attributes
    'below.tip_ded_lh',         # (int) whether tips deduction is limited to leisure and hospitality workers only
    'below.tip_ded_limit',      # (int) maximum deductible OT, non-joint returns
    'below.tip_ded_po_thresh',  # (int) AGI threshold for OT deduction phaseout, non-joint returns
    'below.tip_ded_po_type',    # (int) whether OT phaseout type is a rate (0 means range)
    'below.tip_ded_po_rate',    # (int) phaseout rate for OT deduction
    'below.tip_ded_po_range',   # (int) phaseout range for OT deduction
    'below.ot_ded_half',        # (int) whether OT deduction applies only to the "half" of "time and a half"
    'below.ot_ded_limit',       # (int) maximum deductible OT, non-joint returns
    'below.ot_ded_po_thresh',   # (int) AGI threshold for OT deduction phaseout, non-joint returns
    'below.ot_ded_po_type',     # (int) whether OT phaseout type is a rate (0 means range)
    'below.ot_ded_po_rate',     # (int) phaseout rate for OT deduction
    'below.ot_ded_po_range'     # (int) phaseout range for OT deduction
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      #---------------
      # Tip deduction
      #---------------
      
      # Limit to leisure and hospitality industry tips if applicable 
      tips_lh = tips1 * tips_lh1 + tips2 * tips_lh2, 
      tip_ded = if_else(below.tip_ded_lh == 1, tips_lh, tips),
      
      # Limit to maximum value
      tip_ded = pmin(tip_ded, below.tip_ded_limit),
      
      # Limit base on AGI phaseout
      po_rate = if_else(below.tip_ded_po_type == 1, below.tip_ded_po_rate, tip_ded / below.tip_ded_po_range),
      tip_ded = pmax(0, tip_ded - pmax(0, agi - below.tip_ded_po_thresh) * po_rate),
      
      
      #--------------------
      # Overtime deduction
      #--------------------
      
      # Limit to the "half" portion of "time and a half" if applicable
      ot_ded = ot / (1 + (2 * below.ot_ded_half)),
      
      # Limit to maximum value
      ot_ded = pmin(ot_ded, below.ot_ded_limit),
      
      # Limit base on AGI phaseout
      po_rate = if_else(below.ot_ded_po_type == 1, below.ot_ded_po_rate, ot_ded / below.ot_ded_po_range),
      ot_ded  = pmax(0, ot_ded - pmax(0, agi - below.ot_ded_po_thresh) * po_rate),
      
      

      # TODO for josh -- move senior deduction here. 
      # 
      # the legislative wording is a bit strange. it says that for nonitemizers, 
      # the 4K senior deduction is part of the standard deduction, but for 
      # itemizers it's a separate below-the-line deduction. this means what we 
      # have now, where it's included in itemized deductions for itemizers, 
      # isn't technically correct, since the total amount itemized deductions are  
      # an input into other tax calc functions like the AMT. 
      # 
      # this is tricky to implement because the choice between standard and 
      # itemized deductions is not decided at this point in the execution. that 
      # happens below in the function that determines txbl_inc. 
      # 
      # I think the easiest way to do this is to:
      # 1) remove the existing code for the senior deduction and move its params
      #    from std_ded.yaml to below_ded.yaml
      # 2) do the calculation here and return a variable senior_ded
      # 3) in calc_txbl_inc(), after itemizing status has been determined, 
      #    add the amount of senior_below_ded to std_ded for nonitemizers and leave
      #    it be for itemizers
      
      senior_ded = 0, # placeholder 
      
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_below_ded)) %>% 
    return()
}


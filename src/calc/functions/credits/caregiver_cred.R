#------------------------------------------------------
# Function to calculate credit for caregiving expenses
#------------------------------------------------------

# Set return variables for function
return_vars$calc_caregiver_cred = c('caregiver_cred_nonref', 'caregiver_cred_ref')


calc_caregiver_cred = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of a credit for qualified informal caregiving expenses.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables 
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #  - caregiver_cred_nonref (dbl) : nonrefundable component of the credit; if 
  #                                  law specifies a refundable credit, this 
  #                                  variable is 0 for all
  #  - caregiver_cred_ref (dbl)    : refundable component of the credit; if 
  #                                  law specifies a nonrefundable credit,  
  #                                  this variable is 0 for all
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'caregiver_exp1', # (dbl) eligible dependent care expenses, primary earner  
    'caregiver_exp2', # (dbl) eligible dependent care expenses, secondary earner
    'ei1',            # (dbl) earned income of primary filer
    'ei2',            # (dbl) earned income of secondary filer (NA for non-joint returns)
    'filing_status',  # (int) filing status of tax unit
    'agi',            # (dbl) Adjusted Gross Income
    'liab_bc',        # (dbl) income tax liability before credits, including AMT
    'ftc',            # (dbl) value of foreign tax credits
    'cdctc_nonref',   # (dbl) value of nonrefundable Child and Dependent Care Credit 
    'ed_nonref',      # (dbl) value of nonrefundable education credit
    'old_cred',       # (dbl) value of Elderly and Disabled Credit
    
    # Tax law attributes 
    'caregiver.exp_floor',     # (dbl) minimum amount of caregiving expenses above which credit phases in
    'caregiver.min_ei',        # (dbl) minimum amount of earned income to qualify
    'caregiver.max_credit',    # (dbl) maximum credit value
    'caregiver.credit_rate',   # (dbl) credit rate (share of eligible expenses)
    'caregiver.po_thresh',     # (dbl) AGI threshold above which credit begins phasing out
    'caregiver.po_rate',       # (dbl) rate at which credit phases out with AGI
    'caregiver.refundable'     # (int) whether credit is refundable
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(

      # Calculate eligible expenses
      exp1 = pmax(0, caregiver_exp1 - caregiver.exp_floor),
      exp2 = pmax(0, caregiver_exp2 - caregiver.exp_floor),
      
      # Apply earned income limitation
      exp1 = exp1 * (ei1 > caregiver.min_ei),
      exp2 = if_else(filing_status == 2, exp2 * (ei2 > caregiver.min_ei), 0),
      
      # Calculate tenative credit value 
      caregiver_credit = pmin((exp1 + exp2) * caregiver.credit_rate),
      
      # Apply maximum value limitation
      caregiver_credit = pmin(caregiver.max_credit, caregiver_credit),
      
      # Apply phaseout
      reduction        = pmax(0, agi - caregiver.po_thresh) * caregiver.po_rate,
      caregiver_credit = pmax(0, caregiver_credit - reduction),
      
      # Allocate credits to liability stacked ahead of this one
      liab = pmax(0, liab_bc - ftc - cdctc_nonref - ed_nonref - old_cred),
      
      # Allocate to refundable and nonrefundable components
      caregiver_cred_nonref = if_else(caregiver.refundable == 1, 0, pmin(caregiver_credit, liab)),
      caregiver_cred_ref    = if_else(caregiver.refundable == 0, 0, caregiver_credit)
    
    ) %>% 
    
    # Keep variables to return
    select(all_of(return_vars$calc_caregiver_cred)) %>% 
    return()
}

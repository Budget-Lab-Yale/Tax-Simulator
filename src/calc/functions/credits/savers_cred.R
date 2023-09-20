#--------------------------------------
# Function to calculate Saver's Credit 
#--------------------------------------

calc_savers_cred = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of the Retirement Savings Contributions Credit, or the 
  # "Saver's Credit". Currently, this function is a simplified calculation:
  # it just takes the precalculated value from the PUF and applies the 
  # current-law rules on whether it's refundable or not. If this credit 
  # becomes of interest in future policy discussions, we'll build out an 
  # imputation of lower-income retirement savings contributions, take-up rates, 
  # etc. 
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - savers_nonref (dbl) : nonrefundable component of Saver's Credit; if law 
  #                           specifies it's refundable, this is 0 for all
  #   - savers_ref (dbl)    : nonrefundable component of Saver's Credit; if law 
  #                           specifies it's nonrefundable, this is 0 for all
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'liab_bc',      # (dbl) income tax liability before credits, including AMT
    'ftc',          # (dbl) value of foreign tax credit
    'cdctc_nonref', # (dbl) value of Child and Dependent Care Credit
    'ed_nonref',    # (dbl) value of nonrefundable education credits
    'savers_cred',  # (dbl) PUF-abstracted value of Saver's Credit
    
    # Tax law attributes
    'savers.refundable'  # (int) whether Saver's Credit is refundable
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    mutate(
      
      # Limit to liability if nonrefundable
      liab          = pmax(0, liab_bc - ftc - cdctc_nonref - ed_nonref),
      savers_nonref = if_else(savers.refundable == 0, 
                              pmin(liab, savers_cred),
                              0),
      
      # Generate refundable variable value
      savers_ref = if_else(savers.refundable == 1, 
                           savers_cred, 
                           0)
    ) %>% 
    
    # Keep variables to return
    select(savers_nonref, savers_ref) %>% 
    return()
}

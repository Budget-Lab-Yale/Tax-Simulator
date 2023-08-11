#-------------------------------------
# Function to calculate payroll taxes
#-------------------------------------


calc_pr = function(tax_unit) {
  
  #----------------------------------------------------------------------------
  # Calculates employment taxes for all individuals in the tax unit.
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #
  # Returns: dataframe of following variables:
  #          - liab_fica1   (dbl) : FICA taxes for primary earner
  #          - liab_fica2   (dbl) : FICA taxes for secondary earner
  #          - liab_fica    (dbl) : FICA taxes for tax unit
  #          - liab_seca1   (dbl) : SECA taxes for primary earner
  #          - liab_seca2   (dbl) : SECA taxes for secondary earner
  #          - liab_seca    (dbl) : SECA taxes for tax unit
  #          - liab_oasdi1  (dbl) : OASDI taxes for primary earner
  #          - liab_oasdi2  (dbl) : OASDI taxes for secondary earner
  #          - liab_oasdi   (dbl) : OASDI taxes for tax unit
  #          - liab_hi1     (dbl) : Medicare taxes for primary earner
  #          - liab_hi2     (dbl) : Medicare taxes for secondary earner
  #          - liab_hi      (dbl) : Medicare taxes for tax unit
  #          - liab_add_med (dbl) : Additional Medicare Tax for tax unit 
  #          - liab_pr1     (dbl) : payroll taxes for primary earner
  #          - liab_pr2     (dbl) : payroll taxes for secondary earner
  #          - liab_pr_ee   (dbl) : employee's share of tax unit's payroll tax
  #          - liab_pr_er   (dbl) : employer's share of tax unit's payroll tax
  #          - liab_pr      (dbl) : payroll taxes for tax unit 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    '...',      # TODO
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars) %>% 
    mutate(
      
      # TODO form logic
      
      
    ) %>% 
    
    # Keep variables to return
    select(liab_fica1, liab_fica2, liab_fica, liab_seca1, liab_seca2, liab_seca, 
           liab_oasdi1, liab_oasdi2, liab_oasdi, liab_hi1, liab_hi2, liab_hi, 
           liab_add_med, liab_pr1, liab_pr2, liab_pr_ee, liab_pr_er, liab_pr) %>% 
    return()
}


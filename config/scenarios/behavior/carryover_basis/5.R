do_carryover_basis = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Models carryover basis as an increase in taxable capital gains, reflecting
  # (a) mechanical effect of nontaxable realizations becoming taxable and (b)
  # the lower elasticity of capital gains, as applied to the baseline level
  # of realizations. Currently, we simply calibrate the increase (modeled 
  # entirely as an intensive-margin effect) to roughly match CBO's revenue 
  # estimate; in the future, when we have a model of unrealized gains, we can
  # model this from the ground up. 
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax units with calculated variables
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  tax_units %>% 
    mutate(
      kg_lt = kg_lt * if_else(pref.carryover_basis == 1 & kg_lt > 0, 
                              1.05, 
                              1)
    ) %>%  
    return()
}

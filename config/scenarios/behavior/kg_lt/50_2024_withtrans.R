adjust_kg_lt = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts capital gains at the intensive margin using an elasticity of 
  # -0.7, converted to log-lin form after evaluation at the current-law 
  # rate of 0.238.  
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of post-adjustment kg values. 
  #----------------------------------------------------------------------------
  
  # Set elasticities
  e_transitory = -1 / 0.238
  e_permanent  = -0.5 / 0.238
  
  
  tax_units %>% 
    mutate(e_kg_lt = if_else(year == 2024, 
                             e_transitory, 
                             e_permanent), 
           e_kg_lt_type = 'semi') %>% 
    apply_mtr_elasticity('kg_lt', baseline_mtrs, static_mtrs, 1) %>% 
    return()
}

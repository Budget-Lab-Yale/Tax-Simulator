do_kg = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts capital gains at the intensive margin using an elasticity of 
  # -0.62, converted to log-lin form after evaluation at the current-law 
  # rate of 0.238.  
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment kg values. 
  #----------------------------------------------------------------------------
  
  # Set elasticities
  e_permanent  = -0.62 / 0.238
  
  # Apply elasticities and calculate new values
  new_values = tax_units %>% 
    mutate(e_kg_lt = e_permanent, 
           e_kg_lt_type = 'semi') %>% 
    apply_mtr_elasticity('kg_lt', baseline_mtrs, static_mtrs, 1)

  # Replace old values with new and return
  tax_units %>% 
    select(-kg_lt) %>% 
    bind_cols(new_values) %>% 
    return()
}

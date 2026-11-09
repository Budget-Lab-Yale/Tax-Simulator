do_charity = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts cash charitable contributions along the intensive margin with a 
  # tax price elasticity of -0.5.
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment cash charitable 
  #          contribution values. 
  #----------------------------------------------------------------------------
  
  # Set elasticity
  e = -0.5
  
  # Apply elasticities and calculate new values
  new_values = tax_units %>% 
    mutate(e_char_cash      = e, 
           e_char_cash_type = 'taxprice') %>% 
    apply_mtr_elasticity('char_cash', baseline_mtrs, static_mtrs, 1)

  # Replace old values with new and return
  tax_units %>% 
    select(-char_cash) %>% 
    bind_cols(new_values) %>% 
    return()
}

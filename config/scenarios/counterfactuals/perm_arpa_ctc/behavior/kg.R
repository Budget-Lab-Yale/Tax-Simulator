adjust_kg = function(tax_units) { 
  
  #----------------------------------------------------------------------------
  # TODO
  # 
  # Parameters: TODO
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Set elasticity
  e = -0.5 / 0.238
  
  tax_units %>% 
    mutate(e_kg      = e, 
           e_kg_type = 'semi') %>% 
    apply_mtr_elasticity('kg', 1) %>% 
    return()
}

do_auto = function(tax_units, ...) {
  
  #----------------------------------------------------------------------------
  # Adjusts auto interest expenditures to account for a behavioral change in 
  # response to change in the auto interest deductions for itemizers. Uses the
  # higher end estimation for the elasticity of the mortgage interest deduction
  # as evaluated in Hanson (2020), reasoning that the lower price and different
  # utility of a personal vehicle would induce a greater response than housing.
  # https://www.sciencedirect.com/science/article/pii/S0094119020300279
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment auto interest values.
  #----------------------------------------------------------------------------
  
  first_year   = 2025
  e_intensive  = -0.3
  
  current_year = unique(tax_units$year)
  
  if (current_year < first_year | all(tax_units$agi.auto_int_ded_limit == 0)) {
    return(tax_units)
  } else {
    
    new_data = tax_units %>% 
      left_join(
        baseline_mtrs %>% 
          rename_with(.cols = -c(id, year), .fn   = ~ paste0(., '_baseline')), 
        by = c('id', 'year')
      ) %>%
      left_join(static_mtrs, by = c('id', 'year')) %>%
      mutate(
        mtr_delta    = mtr_auto_int_exp - mtr_auto_int_exp_baseline,
        auto_int_exp = auto_int_exp * (1 + e_intensive * mtr_delta)
      ) %>%
      select(id, auto_int_exp)
    
    tax_units %>%
      select(!auto_int_exp) %>%
      left_join(new_data, by = 'id') %>%
      return()
    
  }
}
do_ot = function(tax_units, ...) {
  
  #----------------------------------------------------------------------------
  # Adjusts overtime income to account for behavioral responses to a tax exemption
  # for overtime pay. Models intensive margin response (existing overtime workers 
  # reporting more overtime). Based on a French study showing overtime hours rise
  # but working hours don't) in response to an OT exemption:
  #  The Detaxation of Overtime Hours: Lessons from the French Experiment
  #  Pierre Cahuc and Stéphane Carcillo
  #  Journal of Labor Economics , Vol. 32, No. 2 (April 2014), pp. 361-400
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment overtime values.
  #----------------------------------------------------------------------------
  

  
  # Study params and elasticity derivation
  mean_ot     = 0.46 # Average OT per week (Table D4)
  delta_ot    = 0.2  # Net change in OT reported (Table 2 column 6 row 3)
  france_mtr  = (0.317 + 0.275) / 2 # From OECD's Taxing Wages 2007-2008, Table I7 single/married at average wages
  e_intensive = (delta_ot / mean_ot) / -france_mtr
  
  # Reform timing
  first_year = 2025
  phase_in_years = 3  # full effect realized after 3 years
  
  # Get current year from the data
  current_year = unique(tax_units$year)
  years_since_reform = max(0, current_year - first_year)
  
  # Calculate phase-in factor (0 to 1 over phase_in_years)
  phase_in_factor = min(1, years_since_reform / phase_in_years)
  
  # Skip if no income tax exemption for overtime this year
  if (current_year < first_year | all(tax_units$below.ot_ded_limit == 0)) {
    return(tax_units)
  } else {
    
    # Join MTRs onto data
    new_data = tax_units %>% 
      left_join(
        baseline_mtrs %>% 
          rename_with(.cols = -c(id, year), .fn = ~ paste0(., '_baseline')), 
        by = c('id', 'year')
      ) %>%
      left_join(static_mtrs, by = c('id', 'year'))
    
    new_data = new_data %>%
      mutate(
        
        # Calculate marginal tax rate change (negative for a tax exemption)
        mtr_delta1 = mtr_ot1 - mtr_ot1_baseline,
        mtr_delta2 = mtr_ot2 - mtr_ot2_baseline,
        
        # Calculate response (only for those who already have overtime)
        # Semi-elasticity: each percentage point decrease in MTR increases OT by e_intensive percent
        ot_response1 = (ot1 > 0) * e_intensive * mtr_delta1 * phase_in_factor,
        ot_response2 = (ot2 > 0) * e_intensive * mtr_delta2 * phase_in_factor,
        
        # Apply response to overtime amounts
        ot1 = ot1 * (1 + ot_response1),
        ot2 = ot2 * (1 + ot_response2),
      )
    
    # Keep adjusted variables and return
    tax_units %>% 
      mutate(
        ot1 = new_data$ot1,
        ot2 = new_data$ot2,
        ot  = ot1 + ot2,
      ) %>% 
      return()
  }
}
do_entity_shifting = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # TODO 
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax units with calculated variables
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  
  # Set semi elasticity, starting with Pearce and Prisinzano's Table IV.B preferred 
  # results, evaluated at pass-through's share of business income 
  e = -0.3788 / 0.6
  
  # Set other parameters -- assuming 45% of distributions are paid as 
  # dividends and the ETR on gains, reflecting the "benefit of deferral", is 
  # 25% of the actual rate as per the paper. A future version of this module
  # should more realistically model the micro-level behavior that gives rise
  # to the 25% benefit-of-deferral value 
  alpha = 0.45
  beta  = 0.25
  
  # Read baseline tax law and extract corporate rate
  corp.rate_baseline = globals$baseline_root %>% 
    file.path('baseline/static/supplemental/tax_law.csv') %>% 
    read_csv() %>% 
    select(year, corp.rate_baseline = corp.rate_baseline)
  

  new_values = tax_units %>% 
    
    # Join MTRs
    left_join(baseline_mtrs %>% 
                rename_with(.cols = -c(id, year), 
                            .fn   = ~ paste0(., '_baseline')), 
              by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>% 
    
    # Join baseline corporate rate
    left_join(corp.rate_baseline, by = 'year') %>% 
    
    mutate(
      
      # Calculate change in tax differential, "tau-tau". First, the tax rate 
      # on corporate distributions. For computational convenience we assume the 
      # tax rate faced by dividends and capital gains is the same; this 
      # formulation is not robust to a pre-Bush-style rate differential 
      tau_dist_policy   = (alpha * mtr_kg_lt)          + ((1 - alpha) * beta * mtr_kg_lt),
      tau_dist_baseline = (alpha * mtr_kg_lt_baseline) + ((1 - alpha) * beta * mtr_kg_lt_baseline),
      
      # Next, the net corporate rate
      tau_corp_policy   = corp.rate          + (1 - corp.rate) * tau_dist_policy, 
      tau_corp_baseline = corp.rate_baseline + (1 - corp.rate_baseline) * tau_dist_baseline,
      
      # Finally, the pass-through rate. Assumed to be equal for all types of 
      # pass-through income, another computationally minded assumption
      tau_pass_policy   = mtr_part_active,
      tau_pass_baseline = mtr_part_active_baseline,
      
      # Calculate change in tax differential
      delta_tau_tau = (tau_corp_policy - tau_pass_policy) - (tau_corp_baseline - tau_pass_baseline),
      
      # Calculate implied shifting of business income into pass-through
      amount_shifted = part_active * e * delta_tau_tau,
      
      # Adjust pass-through income
      part_active = part_active + amount_shifted,
      
      # Adjust corporate distributions. Assume any reduction operates through 
      # capital gain, to prevent dividends from being negative. A situation where
      # the revenue effect is correct but the micro-level output cannot be 
      # interpreted literally, not unlike standard capital gains elasticity modeling
      kg_lt = kg_lt - (amount_shifted * (alpha + (1 - alpha) * beta)),
      
      # Calculate implied change in corporate tax revenue
      corp_tax_change = -amount_shifted * (corp.rate - corp.rate_baseline)
      
    ) %>%
    
    select(part_active, kg_lt, corp_tax_change)
  
  # Replace old values with new and return
  tax_units %>% 
    select(-part_active, -kg_lt) %>% 
    bind_cols(new_values) %>% 
    return()
}

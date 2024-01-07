do_employment = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts wage earnings at the extensive margin, per Bastian (2023). TODO 
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under the baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under the static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with post-adjustment wage earnings values.
  #----------------------------------------------------------------------------
  
  # Set elasticities
  e_mothers_poor  = 0.4
  e_mothers_other = 0.2
  e_else          = 0.05
  
  # Set random seed 
  set.seed(76)
  

  tax_units %>% 
    
    # Join MTRs
    left_join(baseline_mtrs %>% 
                rename_with(.cols = -c(id, year), 
                            .fn   = ~ paste0(., '_baseline')), 
              by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>% 
    
    mutate(

      # Calculate tax unit-level income (roughly AGI)
      income = wages + txbl_int + div_ord + div_pref + state_ref + 
               txbl_ira_dist + txbl_pens_dist + txbl_kg + other_gains + 
               sole_prop + sch_e + farm + ui + gross_ss + other_inc,
      
      #------------------
      # Set elasticities
      #------------------
      
      # First earner
      e1 = case_when(
        
        # Low-income single mothers
        (gender1 == 1) & (n_dep_ctc > 0) & (wages1 < eitc.po_thresh_1) & (filing_status != 2) ~ e_mothers_poor, 
        
        # All other mothers with family income below $80,000 
        (gender1 == 1) & (n_dep_ctc > 0) & (income < 80000) ~ e_mothers_other, 
        
        # Others below $80,000
        (income < 80000 & n_dep_ctc > 0) ~ e_else
        
        # Everyone else
        TRUE ~ 0
      ),
      
      # Second earner
      e2 = case_when(
        
        # Low-income single mothers
        (gender2 == 1) & (n_dep_ctc > 0) & (wages1 < eitc.po_thresh_1) & (filing_status != 2) ~ e_mothers_poor, 
        
        # All other mothers with family income below $80,000 
        (gender2 == 1) & (n_dep_ctc > 0) & (income < 80000) ~ e_mothers_other, 
        
        # Others below $80,000
        (income < 80000 & n_dep_ctc > 0) ~ e_else
        
        # Everyone else
        TRUE ~ 0
      ),
      
      
      #---------------------------
      # Simulate labor force exit
      #---------------------------
      
      # Calculate percent change in return-to-work
      delta_rtw1 = ((1 - mtr_wages1) - (1 - mtr_wages1_baseline)) / (1 - mtr_wages1_baseline),
      delta_rtw2 = ((1 - mtr_wages2) - (1 - mtr_wages2_baseline)) / (1 - mtr_wages2_baseline),
      
      # Calculate probability of remaining employed defined as 1 plus the 
      # implied percent change in employment 
      pr_emp1 = 1 + (e1 * delta_rtw1),
      pr_emp2 = 1 + (e2 * delta_rtw2),
      
      # Simulate outcomes
      emp1 = runif(nrow(.)) < pr_emp1,
      emp2 = runif(nrow(.)) < pr_emp2,
      
      # Adjust wages
      wages1 = if_else(wages1 == 0, 0, wages1 * emp1),
      wages2 = if_else(wages2 == 0, 0, wages2 * emp2),
      wages  = wages1 + wages2 
    
    ) %>% 
    return()
}

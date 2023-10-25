#-------------------------------------------------------------------------
# revenue.R
# 
# Post-processing functions to produce revenue estimates for fiscal years
#-------------------------------------------------------------------------



calc_receipts = function(totals, scenario_root) {
  
  #----------------------------------------------------------------------------
  # Calculates a scenario's receipts 
  # 
  # Parameters:
  #   - totals (df) : dataframe containing columns for calendar year totals of
  #        - pmt_iit_nonwithheld (dbl)    : income tax paid at time of filing
  #        - pmt_iit_withheld (dbl)       : income tax withheld or paid 
  #                                         quarterly
  #        - pmt_refund_nonwithheld (dbl) : payments for refundable credits 
  #                                         paid during filing season
  #        - pmt_refund_withheld (dbl)    : advance credits paid throughout 
  #                                         year
  #        - pmt_pr_nonwithheld (dbl)     : payroll tax paid at time of filing
  #        - pmt_pr_withheld (dbl)        : payroll tax withheld (FICA) or paid 
  #                                         quarterly (SECA)  
  #   - scenario_root (str) : directory where scenario's data is written
  #
  # Returns:  void, writes a dataframe for the scenario containing values for:
  #   - Fiscal Year
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #----------------------------------------------------------------------------
  
  totals %>%
    mutate(
      
      # FY receipts: nonwithheld tax plus 75% of current CY withheld tax plus 
      # 25% of previous CY withheld 
      outlays_tax_credits = 0.75 * pmt_refund_withheld + 
                            0.25 * lag(pmt_refund_withheld) + 
                            pmt_refund_nonwithheld,
      
      revenues_income_tax = 0.75 * pmt_iit_withheld + 
                            0.25 * lag(pmt_iit_withheld) + 
                            pmt_iit_nonwithheld,
      
      revenues_payroll_tax = 0.75 * pmt_pr_withheld + 
                             0.25 * lag(pmt_pr_withheld) + 
                             pmt_pr_nonwithheld
    
    ) %>%
    
    # Drop incomplete year
    filter(year != min(year)) %>%
    
    # Write CSV
    select(year, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits) %>%
    write_csv(file.path(scenario_root, 'supplemental', 'receipts.csv'))

}



calc_rev_est = function(counterfactual_ids, global_root, static) {
  
  #----------------------------------------------------------------------------
  # Calculates all scenario revenue estimate deltas when compared to the 
  # baseline.
  # 
  # Parameters: 
  #   - counterfactual_ids (str[]) : list of scenario names for counterfactual
  #                                  scenarios
  #   - global_root (str)          : version-level output root 
  #   - static (bool)              : whether scenario run is static
  #
  # Returns: Void, writes dataframes containing, for each scenario, fiscal year 
  # deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #----------------------------------------------------------------------------
  
  # Read in baseline receipts
  baseline = file.path(output_root, 
                       'baseline', 
                       'static', 
                       'supplemental',
                       'receipts.csv') %>%
    read_csv(show_col_types = F) %>%
    
    # Create and rename variables with b for baseline
    mutate(total = revenues_payroll_tax + 
                   revenues_income_tax - 
                   outlays_tax_credits) %>%
    rename_with(.cols = -year, 
                .fn   = ~ paste0(., '_b')) 
  
  
  for (scenario in counterfactual_ids) {
    
    # Get scenario-specific supplemental filepath
    scenario_path = file.path(global_root, 
                              scenario, 
                              if_else(static, 'static', 'conventional'),
                              'supplemental')
    
    # Read receipts
    file.path(scenario_path, 'receipts.csv') %>% 
      read_csv(show_col_types = F) %>% 
      
      # Calculate difference from baseline 
      calc_rev_delta(baseline) %>%
      
      # Write revenue estimates file
      write_csv(file.path(scenario_path, 'revenue_estimates.csv'))
  }
}



calc_rev_delta = function(sim, baseline) {
  
  #----------------------------------------------------------------------------
  # Calculates a single scenario's revenue estimate delta when compared to the 
  # baseline.
  # 
  # Parameters: 
  #   - sim      (df) : tibble containing one scenario's receipts
  #   - baseline (df) : tibble containing baseline receipts
  #
  # Returns: dataframe containing, for each fiscal year, deltas for
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  # Create and rename variables with s for simulation
  sim = sim %>% 
    mutate(total = revenues_payroll_tax + 
                   revenues_income_tax - 
                   outlays_tax_credits) %>%
    rename_with(.cols = -year, 
                .fn   = ~ paste0(., '_s')) 
  
  # Merge and calculate deltas
  baseline %>%
    left_join(sim, by = "year") %>% 
    mutate(
      total                = total_s                - total_b,
      revenues_payroll_tax = revenues_payroll_tax_s - revenues_payroll_tax_b,
      revenues_income_tax  = revenues_income_tax_s  - revenues_income_tax_b,
      outlays_tax_credits  = outlays_tax_credits_s  - outlays_tax_credits_b
    ) %>%
    
    select(year, total, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits) %>%
    return()
}



calc_stacked = function(counterfactual_ids, global_root, static) {
  
  #----------------------------------------------------------------------------
  # Calculates stacked revenue deltas. Usable if scenarios build off of one 
  # another.
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : list of scenario names for counterfactual
  #                                  scenarios
  #   - global_root (str)          : version-level output root 
  #   - static (bool)              : whether scenario run is stacked 
  #
  # Returns: Void, writes dataframe with fiscal year columns stacking 
  #          scenario revenue deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  
  c('baseline', counterfactual_ids) %>% 
    
    # Read scenario receipts file and store 
    map(.f = ~ file.path(global_root, 
                         scenario, 
                         if_else(static | .x == 'baseline', 'static', 'conventional'),
                         'supplemental', 
                         'receipts.csv') %>% 
                 read_csv(show_col_types = F) %>% 
                 pivot_longer(cols      = -year, 
                              names_to  = 'series', 
                              values_to = 'receipts') %>%
                 mutate(scenario = .x)) %>% 
    
    # Join into single dataframe and group by year-series so as to leave 
    # scenarios ungrouped
    bind_rows() %>% 
    group_by(year, series) %>%
    
    # Calculate stacked difference
    mutate(delta = receipts - lag(receipts)) %>%
    select(year, scenario, series, delta) %>%
    pivot_wider(names_from  = year,
                values_from = delta) %>%
    
    # Drop baseline full of zeros or NA
    filter(scenario != 'baseline') %>% 

    # Write to supplemental folder for final scenario in stacking order 
    write_csv(file.path(global_root, 
                        counterfactual_ids[length(counterfactual_ids)],
                        if_else(static, 'static', 'conventional'),
                        'supplemental',
                        'stacked_revenue_estimates.csv'))
}



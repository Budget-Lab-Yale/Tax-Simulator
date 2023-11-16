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
    write_csv(file.path(scenario_root, 'totals', 'receipts.csv'))

}



calc_rev_est = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Calculates all scenario revenue estimate deltas when compared to the 
  # baseline.
  # 
  # Parameters: 
  #   - counterfactual_ids (str[]) : list of scenario names for counterfactual
  #                                  scenarios
  #
  # Returns: Void, writes dataframes containing, for each scenario, fiscal year 
  # deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #----------------------------------------------------------------------------
  
  # Read in baseline receipts
  baseline = file.path(globals$baseline_root, 
                       'baseline', 
                       'static', 
                       'totals',
                       'receipts.csv') %>%
    read_csv(show_col_types = F) %>%
    
    # Pivot long in variable type
    mutate(total = revenues_payroll_tax + revenues_income_tax - outlays_tax_credits) %>% 
    pivot_longer(cols      = -year, 
                 names_to  = 'series', 
                 values_to = 'baseline')
  
  
  
  for (static in c(T, F)) {
    
    for (scenario_id in counterfactual_ids) {
      
      # Read in counterfactual scenario receipts 
      scenario = file.path(globals$output_root, 
                           scenario_id, 
                           if_else(static, 'static', 'conventional'),
                           'totals', 
                           'receipts.csv') %>%
        read_csv(show_col_types = F) %>%
        
        # Pivot long in variable type
        mutate(total = revenues_payroll_tax + revenues_income_tax - outlays_tax_credits) %>% 
        pivot_longer(cols      = -year, 
                     names_to  = 'series', 
                     values_to = 'counterfactual')
      
      # Join together
      rev_est = baseline %>% 
        left_join(scenario, by = c('year', 'series')) %>% 
        mutate(delta = counterfactual - baseline) %>% 
        select(year, Series = series, delta) %>% 
        pivot_wider(names_from  = `year`, 
                    values_from = delta) %>% 
        mutate(Series = case_when(
          Series == 'total' ~ 'Total budget effect', 
          Series == 'revenues_payroll_tax' ~ '  Revenues, payroll tax', 
          Series == 'revenues_income_tax' ~ '  Revenues, individual income tax', 
          Series == 'outlays_tax_credits' ~ '  Outlays, refundable tax credits'
        )) %>%
        arrange(desc(Series))
      
      # Create workbook
      wb = createWorkbook()
      addWorksheet(wb, scenario_id)
      
      # Add titles and notes 
      writeData(wb = wb, sheet = scenario_id, x = rev_est, startRow = 2)
      writeData(wb = wb, sheet = scenario_id, startRow = 1, 
                x = 'Budget effects of policy change, fiscal year receipts')
      
      # Format numbers and cells 
      addStyle(wb         = wb, 
               sheet      = scenario_id, 
               rows       = 2:6, 
               cols       = 2:ncol(rev_est), 
               gridExpand = T, 
               style      = createStyle(numFmt = 'COMMA'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = scenario_id, 
               rows       = c(1, 2, 6), 
               cols       = 1:ncol(rev_est), 
               gridExpand = T, 
               style      = createStyle(border = 'bottom'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = scenario_id, 
               rows       = 2, 
               cols       = 1:ncol(rev_est), 
               gridExpand = T, 
               style      = createStyle(textDecoration = 'bold'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = scenario_id, 
               rows       = 2:6, 
               cols       = 2:ncol(rev_est), 
               gridExpand = T, 
               style      = createStyle(halign = 'center'), 
               stack      = T)
      setColWidths(wb     = wb,
                   sheet  = scenario_id,
                   cols   = 1:ncol(rev_est),
                   widths = c(29, rep(6, ncol(rev_est) - 1)))
        

      # Write revenue estimates file
      saveWorkbook(wb   = wb, 
                   file = file.path(globals$output_root, 
                                    scenario_id, 
                                    if_else(static, 'static', 'conventional'),
                                    'supplemental', 
                                    'revenue_estimates.xlsx'), 
                   overwrite = T)
    }
  }
}




calc_stacked = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Calculates stacked revenue deltas. Usable if scenarios build off of one 
  # another.
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : list of scenario names for counterfactual
  #                                  scenarios
  #
  # Returns: Void, writes dataframe with fiscal year columns stacking 
  #          scenario revenue deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  for (static in c(T, F)) {
    
    c('baseline', counterfactual_ids) %>% 
      
      # Read scenario receipts file and store 
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root),
                           .x, 
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
      write_csv(file.path(globals$output_root, 
                          counterfactual_ids[length(counterfactual_ids)],
                          if_else(static, 'static', 'conventional'),
                          'supplemental',
                          'stacked_revenue_estimates.csv'))
    
  }
  
}



#-------------------------------------------------------------------------
# revenue.R
# 
# Post-processing functions to produce revenue estimates for fiscal years
#-------------------------------------------------------------------------



calc_receipts = function(totals, scenario_root, corp_tax_root, estate_tax_root, 
                         off_model_root) {
  
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
  #   - corp_tax_root (str) : directory where corporate tax revenue for this 
  #                           scenario is stored
  #   - estate_tax_root (str) : directory where estate tax revenue for this 
  #                             scenario is stored 
  #   - off_model_root (str) : directory where off-model estimates for this
  #                            scenario are stored
  #
  # Returns:  void, writes a dataframe for the scenario containing values for:
  #   - Fiscal Year
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   - Corporate Income tax
  #   - Estate and Gift Taxes
  #----------------------------------------------------------------------------
  
  # Read corporate tax receipts
  revenues_corp_tax = corp_tax_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, revenues_corp_tax = receipts_fy)
  
  # Read estate tax receipts
  revenues_estate_tax = estate_tax_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, revenues_estate_tax = receipts_fy)
  
  # Read other off-model receipts 
  off_model = off_model_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F)
  
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
                             pmt_pr_nonwithheld,
      
      delta_revenues_corp_tax = 0.75 * corp_tax_change +
                                0.25 * lag(corp_tax_change)
    ) %>%
    
    # Join corporate tax levels and net out changes owing to behavior
    left_join(revenues_corp_tax, by = 'year') %>% 
    mutate(revenues_corp_tax = revenues_corp_tax + delta_revenues_corp_tax) %>%
    
    # Join estate tax levels 
    left_join(revenues_estate_tax, by = 'year') %>% 
    
    # Join off-model estimates
    left_join(off_model, by = 'year') %>% 
    mutate(revenues_income_tax = revenues_income_tax + individual,  
           revenues_corp_tax   = revenues_corp_tax + corporate) %>% 
    select(-individual, -corporate) %>% 
    
    # Drop incomplete year
    filter(year != min(year)) %>%
    
    # Write CSV
    select(year, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits, 
           revenues_corp_tax, revenues_estate_tax) %>%
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
  #   - Corporate Income Tax
  #----------------------------------------------------------------------------

  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  
  # Read in baseline receipts
  baseline = file.path(globals$baseline_root, 
                       'baseline', 
                       'static', 
                       'totals',
                       'receipts.csv') %>%
    read_csv(show_col_types = F) %>%
    
    # Pivot long in variable type
    mutate(total = revenues_payroll_tax + 
                   revenues_income_tax - 
                   outlays_tax_credits + 
                   revenues_corp_tax + 
                   revenues_estate_tax) %>% 
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
        mutate(total = revenues_payroll_tax + 
                       revenues_income_tax - 
                       outlays_tax_credits + 
                       revenues_corp_tax + 
                       revenues_estate_tax) %>% 
        pivot_longer(cols      = -year, 
                     names_to  = 'series', 
                     values_to = 'counterfactual')
      
      # Read GDP 
      gdp = globals$interface_paths %>% 
        filter(ID == globals$interface_path$ID[1], interface == 'Macro-Projections') %>% 
        get_vector('path') %>% 
        file.path(c('historical.csv', 'projections.csv')) %>% 
        map(~ read_csv(.x, show_col_types = F)) %>% 
        bind_rows() %>% 
        select(year, gdp_fy)
      
      # Join together and calculate estimates, both nominal and share-of-GDP
      rev_est = baseline %>% 
        left_join(scenario, by = c('year', 'series')) %>% 
        mutate(Dollars = counterfactual - baseline) %>% 
        select(year, Series = series, Dollars) %>%
        left_join(gdp, by = 'year') %>% 
        mutate(`Share of GDP` = Dollars / gdp_fy) %>%
        select(-gdp_fy) %>% 
        pivot_longer(cols      = c(Dollars, `Share of GDP`), 
                     names_to  = 'Measure', 
                     values_to = 'delta') %>% 
        pivot_wider(names_from  = `year`, 
                    values_from = delta) %>% 
        mutate(Series = case_when(
          Series == 'total'                ~ 'Total budget effect', 
          Series == 'revenues_payroll_tax' ~ '  Revenues, payroll tax', 
          Series == 'revenues_income_tax'  ~ '  Revenues, individual income tax', 
          Series == 'outlays_tax_credits'  ~ '  Outlays, refundable tax credits',
          Series == 'revenues_corp_tax'    ~ '  Revenues, corporate income tax',
          Series == 'revenues_estate_tax'  ~ '  Revenues, estate tax'
        )) %>%
        arrange(Measure, desc(Series)) 
      
      # Convert to measure-indexed list
      rev_est = c('Dollars', 'Share of GDP') %>% 
        map(.f = ~ rev_est %>% 
              filter(Measure == .x) %>% 
              select(-Measure)) %>% 
        set_names(c('Dollars', 'Share of GDP'))
      
      # Write machine-readable version
      rev_est$Dollars %>% 
        filter(Series == 'Total budget effect') %>% 
        select(-Series) %>% 
        pivot_longer(cols      = everything(), 
                     names_to  = 'year',
                     values_to = 'total') %>% 
        mutate(year = as.integer(year)) %>% 
        write_csv(file.path(globals$output_root, 
                            scenario_id, 
                            if_else(static, 'static', 'conventional'),
                            'supplemental', 
                            'revenue_estimates.csv'))

      # Create workbook
      wb = createWorkbook()
      addWorksheet(wb, as.character(scenario_id))
      
      # Write data
      writeData(wb = wb, sheet = as.character(scenario_id), x = rev_est$Dollars, startRow = 2)
      writeData(wb = wb, sheet = as.character(scenario_id), startRow = 1, 
                x = 'FY budget effects of policy change, nominal dollars')
      
      writeData(wb = wb, sheet = as.character(scenario_id), x = rev_est$`Share of GDP`, startRow = 11)
      writeData(wb = wb, sheet = as.character(scenario_id), startRow = 10, 
                x = 'FY budget effects of policy change, share of GDP')
      
      # Format numbers and cells 
      addStyle(wb         = wb, 
               sheet      = as.character(scenario_id), 
               rows       = 2:8, 
               cols       = 2:ncol(rev_est$Dollars), 
               gridExpand = T, 
               style      = createStyle(numFmt = 'COMMA'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = as.character(scenario_id),
               rows       = 12:17, 
               cols       = 2:ncol(rev_est$Dollars), 
               gridExpand = T, 
               style      = createStyle(numFmt = 'PERCENTAGE'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = as.character(scenario_id), 
               rows       = c(1, 2, 8, 10, 11, 17), 
               cols       = 1:ncol(rev_est$Dollars), 
               gridExpand = T, 
               style      = createStyle(border = 'bottom'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = as.character(scenario_id), 
               rows       = c(2, 11), 
               cols       = 1:ncol(rev_est$Dollars), 
               gridExpand = T, 
               style      = createStyle(textDecoration = 'bold'), 
               stack      = T)
      addStyle(wb         = wb, 
               sheet      = as.character(scenario_id), 
               rows       = 2:17,
               cols       = 2:ncol(rev_est$Dollars), 
               gridExpand = T, 
               style      = createStyle(halign = 'center'), 
               stack      = T)
      setColWidths(wb     = wb,
                   sheet  = as.character(scenario_id),
                   cols   = 1:ncol(rev_est$Dollars),
                   widths = c(29, rep(6, ncol(rev_est$Dollars) - 1)))
        

      # Write revenue estimates file
      saveWorkbook(wb   = wb, 
                   file = file.path(globals$output_root, 
                                    as.character(scenario_id), 
                                    if_else(static, 'static', 'conventional'),
                                    'supplemental', 
                                    'revenue_estimates.xlsx'), 
                   overwrite = T)
    }
  }
}




calc_stacked_rev_est = function(counterfactual_ids) {
  
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
  #   - Corporate Income Tax
  #----------------------------------------------------------------------------
  
  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  for (static in c(T, F)) {
    
    stacked_rev_est = c('baseline', counterfactual_ids) %>% 
      
      # Read scenario receipts file and store 
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root),
                           .x, 
                           if_else(static | .x == 'baseline', 'static', 'conventional'),
                           'totals', 
                           'receipts.csv') %>% 
            read_csv(show_col_types = F) %>% 
            mutate(scenario_id = .x,
                   total = revenues_payroll_tax + 
                           revenues_income_tax - 
                           outlays_tax_credits + 
                           revenues_corp_tax + 
                           revenues_estate_tax) %>% 
            select(scenario_id, year, total)) %>% 
      
      # Join into single dataframe and group by year-series so as to leave 
      # scenarios ungrouped
      bind_rows() %>% 
      group_by(year) %>%
      
      # Calculate stacked difference
      mutate(Dollars = total - lag(total)) %>%
      
      # Calculate share-of-GDP metric
      left_join(
        globals$interface_paths %>% 
          filter(ID == globals$interface_path$ID[1], interface == 'Macro-Projections') %>% 
          get_vector('path') %>% 
          file.path(c('historical.csv', 'projections.csv')) %>% 
          map(~ read_csv(.x, show_col_types = F)) %>% 
          bind_rows() %>% 
          select(year, gdp_fy), 
        by = 'year'
      ) %>% 
      mutate(`Share of GDP` = Dollars / gdp_fy) %>% 
      select(-gdp_fy, -total) %>% 
      
      # Reshape wide in year
      pivot_longer(cols      = c(Dollars, `Share of GDP`), 
                   names_to  = 'Measure', 
                   values_to = 'value') %>% 
      pivot_wider(names_from  = year,
                  values_from = value) %>%
      
      # Drop baseline full of zeros or NA
      filter(scenario_id != 'baseline') 
    
    # Convert to measure-indexed list, adding totals row in the process
    stacked_rev_est = c('Dollars', 'Share of GDP') %>% 
      map(.f = ~ stacked_rev_est %>% 
            filter(Measure == .x) %>% 
            select(-Measure) %>% 
            bind_rows(
              (.) %>%
                summarise(across(.cols = -scenario_id, 
                                 .fns  = sum)) %>% 
                mutate(scenario_id = 'Total')
            ) %>% 
            rename(Scenario = scenario_id)) %>% 
      set_names(c('Dollars', 'Share of GDP'))
    
    
    # Create workbook
    wb = createWorkbook()
    addWorksheet(wb, 'Stacked revenue estimates')
    
    # Add data and titles
    writeData(wb = wb, sheet = 'Stacked revenue estimates', 
              x = stacked_rev_est$Dollars, startRow = 2)
    writeData(wb = wb, sheet = 'Stacked revenue estimates', startRow = 1, 
              x = 'Stacked FY budget effects of policy changes, nominal dollars')
    
    writeData(wb = wb, sheet = 'Stacked revenue estimates', 
              x = stacked_rev_est$`Share of GDP`, startRow = nrow(stacked_rev_est$Dollars) + 5)
    writeData(wb = wb, sheet = 'Stacked revenue estimates', startRow = nrow(stacked_rev_est$Dollars) + 4, 
              x = 'Stacked FY budget effects of policy changes, share of GDP')
    
    
    # Format numbers and cells 
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = 2:(nrow(stacked_rev_est$Dollars) + 2), 
             cols       = 2:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'COMMA'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = (nrow(stacked_rev_est$Dollars) + 6):(2 * nrow(stacked_rev_est$Dollars) + 6), 
             cols       = 2:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'PERCENTAGE'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = c(1, 2, nrow(stacked_rev_est$Dollars) + c(1, 2)) %>% 
                             c((.) + nrow(stacked_rev_est$Dollars) + 3), 
             cols       = 1:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(border = 'bottom'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = c(2, nrow(stacked_rev_est$Dollars) + 5), 
             cols       = 1:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(textDecoration = 'bold'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = 2:(2 * (ncol(stacked_rev_est$Dollars) + 2)), 
             cols       = 2:(ncol(stacked_rev_est$Dollars)), 
             gridExpand = T, 
             style      = createStyle(halign = 'center'), 
             stack      = T)
    setColWidths(wb     = wb,
                 sheet  = 'Stacked revenue estimates',
                 cols   = 1:ncol(stacked_rev_est$Dollars),
                 widths = c(29, rep(6, ncol(stacked_rev_est$Dollars) - 1)))
    
    
    # Write revenue estimates file
    saveWorkbook(wb   = wb, 
                 file = file.path(globals$output_root, 
                                  counterfactual_ids[length(counterfactual_ids)], 
                                  if_else(static, 'static', 'conventional'),
                                  'supplemental', 
                                  'stacked_revenue_estimates.xlsx'), 
                 overwrite = T)
  }
}



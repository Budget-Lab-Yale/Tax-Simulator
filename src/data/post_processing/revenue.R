#-------------------------------------------------------------------------
# revenue.R
# 
# Post-processing functions to produce revenue estimates for fiscal years
#-------------------------------------------------------------------------



calc_receipts = function(totals, scenario_root, corp_tax_root, estate_tax_root, 
                         vat_root, other_root, off_model_root) {
  
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
  #   - scenario_root (str)   : directory where scenario's data is written
  #   - corp_tax_root (str)   : directory where corporate tax revenue for this 
  #                             scenario is stored
  #   - estate_tax_root (str) : directory where estate tax revenue for this 
  #                             scenario is stored 
  #   - vat_root (str)        : directory where value added tax revenue for
  #                             this scenario is stored
  #   - other_root (str)      : Macro-Projections root (for other taxes)
  #   - off_model_root (str)  : directory where off-model estimates for this
  #                             scenario are stored
  #
  # Returns:  void, writes a dataframe for the scenario containing values for:
  #   - Fiscal year
  #   - Payroll tax revenues
  #   - Individual income tax revenues
  #   - Refundable credit outlays
  #   - Corporate income tax revenues
  #   - Estate and gift tax revenues
  #   - Value added tax revenues
  #----------------------------------------------------------------------------
  
  # Read corporate tax receipts
  revenues_corp_tax = corp_tax_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    rename(
      revenues_corp_rate  = rate,
      revenues_corp_other = other,
    )
  
  # Read estate tax receipts
  revenues_estate_tax = estate_tax_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, revenues_estate_tax = receipts_fy)
  
  # Read VAT receipts
  revenues_vat = vat_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, revenues_vat = receipts_fy)
  
  # Read other receipts 
  revenues_other = c('historical.csv', 'projections.csv') %>% 
    map(.f = ~ other_root %>% 
          file.path(.x) %>%
          read_csv(show_col_types = F) %>%
          mutate(revenues_other = rev_excise + rev_customs + rev_misc) %>% 
          select(year, revenues_other)
    ) %>% 
    bind_rows()
  
  # Read off-model receipts 
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
    
    # Join estate tax levels 
    left_join(revenues_estate_tax, by = 'year') %>% 
    
    # Join VAT levels 
    left_join(revenues_vat, by = 'year') %>% 
    
    # Join other revenues levels
    left_join(revenues_other, by = 'year') %>% 
    
    # Join off-model estimates
    left_join(off_model, by = 'year') %>% 
    mutate(revenues_income_tax = revenues_income_tax + individual,  
           revenues_corp_tax   = revenues_corp_other + corporate, 
           revenues_vat        = revenues_vat + vat) %>% 
    select(-individual, -corporate, -vat) %>% 
    
    # Drop incomplete year
    filter(year != min(year)) %>%
    
    # Write CSV
    select(year, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits, 
           revenues_corp_tax, revenues_corp_rate, revenues_estate_tax, 
           revenues_vat, revenues_other) %>%
    write_csv(file.path(scenario_root, 'totals', 'receipts.csv'))
}



calc_rev_est = function(id) {
  
  #----------------------------------------------------------------------------
  # Calculates revenue deltas against baseline for each counterfactual 
  # scenario.
  # 
  # Parameters: 
  #   - id : scenario ID
  #
  # Returns: Void, writes dataframe containing fiscal year deltas for:
  #   - Total revenues
  #   - Payroll tax revenues
  #   - Individual income tax revenues
  #   - Refundable credit outlays
  #   - Corporate income tax revenues
  #   - Estate and gift tax revenues
  #   - Value added tax revenues
  #   - Other receipts
  #----------------------------------------------------------------------------

  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  # Read baseline receipts
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
                   revenues_corp_rate +
                   revenues_corp_tax + 
                   revenues_estate_tax + 
                   revenues_vat + 
                   revenues_other) %>% 
    pivot_longer(cols      = -year, 
                 names_to  = 'series', 
                 values_to = 'baseline')
  
  for (static in c(T, F)) {
  
    # Read VAT price offset for baseline dollars calculation
    vat_price_offset = globals$output_root %>% 
      file.path(id, 'static/supplemental/vat_price_offset.csv') %>% 
      read_csv(show_col_types = F)
    
    # Read in counterfactual scenario receipts 
    scenario = file.path(globals$output_root, 
                         id, 
                         if_else(static, 'static', 'conventional'),
                         'totals', 
                         'receipts.csv') %>%
      read_csv(show_col_types = F) %>%
      
      # Pivot long in variable type
      mutate(total = revenues_payroll_tax + 
                     revenues_income_tax - 
                     outlays_tax_credits + 
                     revenues_corp_rate +
                     revenues_corp_tax + 
                     revenues_estate_tax + 
                     revenues_vat +
                     revenues_other) %>% 
      pivot_longer(cols      = -year, 
                   names_to  = 'series', 
                   values_to = 'counterfactual')
    
    # Read GDP and adjust for VAT (i.e. price level rises)
    gdp = globals$interface_paths %>% 
      filter(ID == globals$interface_path$ID[1], interface == 'Macro-Projections') %>% 
      get_vector('path') %>% 
      file.path(c('historical.csv', 'projections.csv')) %>% 
      map(~ read_csv(.x, show_col_types = F)) %>% 
      bind_rows() %>% 
      left_join(scenario %>% 
                  filter(series == 'revenues_vat') %>% 
                  select(year, vat = counterfactual), 
                by = 'year') %>% 
      mutate(gdp_counterfactual = gdp_fy + vat) %>% 
      select(year, gdp_baseline = gdp_fy, gdp_counterfactual)
    
    # Join together and calculate estimates: nominal, baseline dollars (for
    # scenarios with a VAT), and share-of-GDP
    rev_est = baseline %>% 
      left_join(scenario, by = c('year', 'series')) %>% 
      left_join(gdp, by = 'year') %>% 
      left_join(vat_price_offset, by = 'year') %>%
      mutate(
        Dollars            = counterfactual - baseline, 
        `Baseline dollars` = (counterfactual / gdp_deflator_factor) - baseline,
        `Share of GDP`     = (counterfactual / gdp_counterfactual) - (baseline / gdp_baseline)
      ) %>% 
      select(year, Series = series, Dollars, `Baseline dollars`, `Share of GDP`) %>%
      pivot_longer(cols      = c(Dollars, `Baseline dollars`, `Share of GDP`), 
                   names_to  = 'Measure', 
                   values_to = 'delta') %>% 
      pivot_wider(names_from  = `year`, 
                  values_from = delta) %>% 
      arrange(Measure, 
              match(Series, c('total', 'revenues_income_tax', 
                              'revenues_payroll_tax', 'revenues_corp_rate', 'revenues_corp_tax', 
                              'revenues_estate_tax', 'revenues_other',
                              'revenues_vat', 'outlays_tax_credits'))
      ) %>% 
      mutate(Series = case_when(
        Series == 'total'                ~ 'Total budget effect', 
        Series == 'revenues_payroll_tax' ~ '  Revenues, payroll tax', 
        Series == 'revenues_income_tax'  ~ '  Revenues, individual income tax', 
        Series == 'outlays_tax_credits'  ~ '  Outlays, refundable tax credits',
        Series == 'revenues_corp_rate'   ~ '  Revenues, corporate income tax (rate change)',
        Series == 'revenues_corp_tax'    ~ '  Revenues, corporate income tax',
        Series == 'revenues_estate_tax'  ~ '  Revenues, estate tax',
        Series == 'revenues_vat'         ~ '  Revenues, value added tax',
        Series == 'revenues_other'       ~ '  Revenues, other'
      )) 
    
    # Convert to measure-indexed list
    rev_est = c('Dollars', 'Baseline dollars', 'Share of GDP') %>% 
      map(.f = ~ rev_est %>% 
            filter(Measure == .x) %>% 
            select(-Measure)) %>% 
      set_names(c('Dollars', 'Baseline dollars', 'Share of GDP'))
    
    # Write machine-readable version
    rev_est$Dollars %>% 
      filter(Series == 'Total budget effect') %>% 
      select(-Series) %>% 
      pivot_longer(cols      = everything(), 
                   names_to  = 'year',
                   values_to = 'total') %>% 
      mutate(year = as.integer(year)) %>% 
      write_csv(file.path(globals$output_root, 
                          id, 
                          if_else(static, 'static', 'conventional'),
                          'supplemental', 
                          'revenue_estimates.csv'))

    # Create workbook
    wb = createWorkbook()
    addWorksheet(wb, as.character(id))
    
    # Write data
    writeData(wb = wb, sheet = as.character(id), x = rev_est$Dollars, startRow = 2)
    writeData(wb = wb, sheet = as.character(id), startRow = 1, 
              x = 'FY budget effects of policy change, nominal dollars')
    
    writeData(wb = wb, sheet = as.character(id), x = rev_est$`Baseline dollars`, startRow = 13)
    writeData(wb = wb, sheet = as.character(id), startRow = 12, 
              x = 'FY budget effects of policy change, baseline dollars')
    
    writeData(wb = wb, sheet = as.character(id), x = rev_est$`Share of GDP`, startRow = 24)
    writeData(wb = wb, sheet = as.character(id), startRow = 23, 
              x = 'FY budget effects of policy change, share of GDP')
    
    # Format numbers and cells 
    addStyle(wb         = wb, 
             sheet      = as.character(id), 
             rows       = c(2:10, 14:21), 
             cols       = 2:ncol(rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'COMMA'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(id),
             rows       = 25:31, 
             cols       = 2:ncol(rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'PERCENTAGE'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(id), 
             rows       = c(1, 2, 10, 12, 13, 21, 23, 24, 32), 
             cols       = 1:ncol(rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(border = 'bottom'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(id), 
             rows       = c(2, 13, 24), 
             cols       = 1:ncol(rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(textDecoration = 'bold'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(id), 
             rows       = 2:31,
             cols       = 2:ncol(rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(halign = 'center'), 
             stack      = T)
    setColWidths(wb     = wb,
                 sheet  = as.character(id),
                 cols   = 1:ncol(rev_est$Dollars),
                 widths = c(29, rep(6, ncol(rev_est$Dollars) - 1)))
      

    # Write revenue estimates file
    saveWorkbook(wb   = wb, 
                 file = file.path(globals$output_root, 
                                  as.character(id), 
                                  if_else(static, 'static', 'conventional'),
                                  'supplemental', 
                                  'revenue_estimates.xlsx'), 
                 overwrite = T)
  }
}




calc_stacked_rev_est = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Calculates stacked revenue deltas.
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : list of scenario names for counterfactual
  #                                  scenarios
  #
  # Returns: Void, writes dataframe with fiscal year columns stacking 
  #          scenario revenue deltas for total receipts. 
  #----------------------------------------------------------------------------
  
  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  for (static in c(T, F)) {
    
    # Read VAT price offset for baseline dollars calculation
    vat_price_offsets = counterfactual_ids %>% 
      map(.f = ~ globals$output_root %>% 
            file.path(.x, 'static/supplemental/vat_price_offset.csv') %>% 
            read_csv(show_col_types = F) %>% 
            mutate(scenario_id = .x) 
      ) %>% 
      bind_rows()
    
    
    stacked_rev_est = c('baseline', counterfactual_ids) %>% 
      
      # Read scenario receipts file and store 
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root),
                           .x, 
                           if_else(static | .x == 'baseline', 'static', 'conventional'),
                           'totals', 
                           'receipts.csv') %>% 
            read_csv(show_col_types = F) %>% 
            mutate(scenario_id = .x,
                   Dollars = revenues_payroll_tax + 
                             revenues_income_tax - 
                             outlays_tax_credits +
                             revenues_corp_rate +
                             revenues_corp_tax + 
                             revenues_estate_tax + 
                             revenues_vat + 
                             revenues_other) %>% 
            select(scenario_id, year, Dollars, vat = revenues_vat)) %>% 
      bind_rows() %>% 
      
      # Calculate share-of-GDP metric, accounting  for introduction of a VAT
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
      mutate(`Share of GDP` = Dollars / (gdp_fy + vat))  %>% 
      select(-gdp_fy, -vat) %>%
    
      # Calculate revenues in baseline dollars
      left_join(vat_price_offsets, by = c('year', 'scenario_id')) %>%
      mutate(
        gdp_deflator_factor = if_else(scenario_id == 'baseline', 1, gdp_deflator_factor),
        `Baseline dollars`  = Dollars / gdp_deflator_factor
      ) %>%
        
      # Pivot long in metric and calculate stacked deltas
      select(scenario_id, year, Dollars, `Baseline dollars`, `Share of GDP`) %>% 
      pivot_longer(cols      = c(Dollars, `Baseline dollars`, `Share of GDP`), 
                   names_to  = 'Measure', 
                   values_to = 'value') %>% 
      group_by(Measure, year) %>%
      mutate(value = value - lag(value)) %>%
      ungroup() %>% 
      
      # Reshape wide in year
      pivot_wider(names_from  = year,
                  values_from = value) %>%
      
      # Drop baseline full of zeros or NA
      filter(scenario_id != 'baseline') 
    
    
    # Convert to measure-indexed list, adding totals row in the process
    stacked_rev_est = c('Dollars', 'Baseline dollars', 'Share of GDP') %>% 
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
      set_names(c('Dollars', 'Baseline dollars', 'Share of GDP'))
    
    
    # Create workbook
    wb = createWorkbook()
    addWorksheet(wb, 'Stacked revenue estimates')
    
    # Calculate alignment positions
    start_real = nrow(stacked_rev_est$Dollars) + 5
    start_gdp  = start_real + nrow(stacked_rev_est$`Baseline dollars`) + 3
    
    # Add data and titles
    writeData(wb = wb, sheet = 'Stacked revenue estimates', 
              x = stacked_rev_est$Dollars, startRow = 2)
    writeData(wb = wb, sheet = 'Stacked revenue estimates', startRow = 1, 
              x = 'Stacked FY budget effects of policy changes, nominal dollars')
    
    writeData(wb = wb, sheet = 'Stacked revenue estimates', 
              x = stacked_rev_est$`Baseline dollars`, startRow = start_real)
    writeData(wb = wb, sheet = 'Stacked revenue estimates', startRow = start_real - 1, 
              x = 'Stacked FY budget effects of policy changes, baseline dollars')
    
    writeData(wb = wb, sheet = 'Stacked revenue estimates', 
              x = stacked_rev_est$`Share of GDP`, startRow = start_gdp)
    writeData(wb = wb, sheet = 'Stacked revenue estimates', startRow = start_gdp - 1, 
              x = 'Stacked FY budget effects of policy changes, share of GDP')
    
    
    # Format numbers and cells 
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = c(2:(nrow(stacked_rev_est$Dollars) + 2),
                            1 + start_real:(start_real + nrow(stacked_rev_est$`Baseline dollars`))),
             cols       = 2:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'COMMA'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = 1 + start_gdp:(start_gdp + nrow(stacked_rev_est$`Share of GDP`)), 
             cols       = 2:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'PERCENTAGE'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = c(1, 2, nrow(stacked_rev_est$Dollars) + c(1, 2), 
                            start_real + c(-1, 0, nrow(stacked_rev_est$`Baseline dollars`) + c(-1, 0)),
                            start_gdp  + c(-1, 0, nrow(stacked_rev_est$`Share of GDP`) + c(-1, 0))
                            ),
             cols       = 1:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(border = 'bottom'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = c(2, start_real, start_gdp), 
             cols       = 1:ncol(stacked_rev_est$Dollars), 
             gridExpand = T, 
             style      = createStyle(textDecoration = 'bold'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = 'Stacked revenue estimates', 
             rows       = 2:(1 + start_gdp + nrow(stacked_rev_est$`Share of GDP`)), 
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



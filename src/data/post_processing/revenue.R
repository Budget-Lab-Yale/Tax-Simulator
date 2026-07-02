#-------------------------------------------------------------------------
# revenue.R
#
# Post-processing functions to produce revenue estimates for fiscal years
#-------------------------------------------------------------------------



add_receipts_total = function(receipts, name) {

  #----------------------------------------------------------------------------
  # Appends the total budget-effect column to a wide receipts dataframe:
  # the sum of all revenue components less refundable credit outlays.
  #
  # Parameters:
  #   - receipts (df) : wide receipts dataframe (receipts_full.csv layout)
  #   - name (str)    : name for the new total column
  #
  # Returns: receipts with the total column appended (df).
  #----------------------------------------------------------------------------

  receipts[[name]] = with(receipts,
    revenues_payroll_tax + revenues_income_tax - outlays_tax_credits +
    revenues_corp_tax + revenues_estate_tax + revenues_wealth_tax +
    revenues_vat + revenues_other
  )
  return(receipts)
}



read_receipts_long = function(path, values_to) {

  #----------------------------------------------------------------------------
  # Reads a receipts_full.csv, adds the total budget-effect column, and pivots
  # long in series.
  #
  # Parameters:
  #   - path (str)      : path to a receipts_full.csv
  #   - values_to (str) : name for the value column in the long output
  #
  # Returns: tibble long in (year, series) (df).
  #----------------------------------------------------------------------------

  path %>%
    read_csv(show_col_types = F) %>%
    add_receipts_total('total') %>%
    pivot_longer(cols      = -year,
                 names_to  = 'series',
                 values_to = values_to) %>%
    return()
}



split_by_measure = function(rev_est) {

  #----------------------------------------------------------------------------
  # Splits a revenue table long in Measure into a measure-indexed list.
  #
  # Parameters:
  #   - rev_est (df) : revenue estimates with a Measure column
  #
  # Returns: list of dataframes named by measure (lst).
  #----------------------------------------------------------------------------

  measures = c('Dollars', 'Baseline dollars', 'Share of GDP')
  measures %>%
    map(.f = ~ rev_est %>%
          filter(Measure == .x) %>%
          select(-Measure)) %>%
    set_names(measures) %>%
    return()
}



style_rev_sheet = function(wb, sheet, n_cols, comma_rows, pct_rows,
                           border_rows, bold_rows, center_rows) {

  #----------------------------------------------------------------------------
  # Applies the shared revenue-workbook styling (number formats, borders, bold
  # headers, centering, column widths) to a sheet. Callers supply row
  # positions, which differ between the fixed-layout single-scenario report
  # and the nrow-dependent stacked report.
  #
  # Parameters:
  #   - wb (Workbook)     : openxlsx workbook object (modified in place)
  #   - sheet (str)       : sheet name
  #   - n_cols (int)      : number of columns in the written tables
  #   - comma_rows (int[]): rows formatted with comma separators
  #   - pct_rows (int[])  : rows formatted as percentages
  #   - border_rows (int[]) : rows given a bottom border
  #   - bold_rows (int[]) : rows in bold
  #   - center_rows (int[]) : rows center-aligned
  #
  # Returns: void (modifies wb in place).
  #----------------------------------------------------------------------------

  addStyle(wb = wb, sheet = sheet, rows = comma_rows, cols = 2:n_cols,
           gridExpand = T, style = createStyle(numFmt = 'COMMA'), stack = T)
  addStyle(wb = wb, sheet = sheet, rows = pct_rows, cols = 2:n_cols,
           gridExpand = T, style = createStyle(numFmt = 'PERCENTAGE'), stack = T)
  addStyle(wb = wb, sheet = sheet, rows = border_rows, cols = 1:n_cols,
           gridExpand = T, style = createStyle(border = 'bottom'), stack = T)
  addStyle(wb = wb, sheet = sheet, rows = bold_rows, cols = 1:n_cols,
           gridExpand = T, style = createStyle(textDecoration = 'bold'), stack = T)
  addStyle(wb = wb, sheet = sheet, rows = center_rows, cols = 2:n_cols,
           gridExpand = T, style = createStyle(halign = 'center'), stack = T)
  setColWidths(wb = wb, sheet = sheet, cols = 1:n_cols,
               widths = c(29, rep(6, n_cols - 1)))
}



calc_receipts = function(totals, scenario_root, vat_root, other_root,
                         cost_recovery_root, off_model_root, excess_growth_all_rev) {
  
  #----------------------------------------------------------------------------
  # Calculates a scenario's receipts 
  # 
  # Parameters:
  #   - totals (df) : dataframe containing columns for calendar year totals of
  #        - pmt_iit_nonwithheld    (dbl) : income tax paid at time of filing
  #        - pmt_iit_withheld       (dbl) : income tax withheld or paid quarterly
  #        - pmt_refund_nonwithheld (dbl) : payments for refundable credits paid during filing season
  #        - pmt_refund_withheld    (dbl) : advance credits paid throughout year
  #        - pmt_pr_nonwithheld     (dbl) : payroll tax paid at time of filing
  #        - pmt_pr_withheld        (dbl) : payroll tax withheld (FICA) or paid quarterly (SECA)
  #        - est_tax_exp            (dbl) : expected estate tax liability, $B (CY of death)
  #        - wealth_tax             (dbl) : annual wealth tax liability, $B (CY)
  #   - scenario_root         (str) : directory where scenario's data is written
  #   - vat_root              (str) : directory for VAT revenue for this scenario
  #   - other_root            (str) : Macro-Projections root (for other taxes)
  #   - cost_recovery_root    (str) : Cost-Recovery-Simulator director for this scenario
  #   - off_model_root        (str) : directory for miscellaneous off-model deltas for this scenario
  #   - excess_growth_all_rev (int) : whether to apply excess growth to all revenue or just IIT/payroll/corporate taxes
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
  
  # Read VAT receipts
  revenues_vat = vat_root %>%
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, revenues_vat = receipts_fy)
  
  # Read other receipts (CBO projections)
  revenues_other = read_macro_spliced(other_root) %>%
    mutate(revenues_other = rev_excise + rev_customs + rev_misc) %>%
    select(year, revenues_corp_tax = rev_corp, revenues_estate_tax = rev_estate, revenues_other)
  
  # Read deltas attributable to cost recovery changes
  deltas_cost_recovery = cost_recovery_root %>% 
    file.path('deltas/revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    rename(delta_revenues_cost_recovery = delta)
  
  # Read off-model deltas
  deltas_off_model = off_model_root %>%
    file.path('revenues.csv') %>%
    read_csv(show_col_types = F)

  # Read the model-baseline estate tax series. Estate revenue is presented as
  # the CBO LEVEL plus an on-model DELTA (scenario minus model-baseline), so
  # baseline receipts stay CBO-anchored and the model contributes only reform
  # deltas — the quantity it is validated on (JCT sunset delta). The baseline
  # leg is always the baseline's STATIC totals (baseline has no conventional).
  baseline_estate_path = globals$baseline_root %>%
    file.path('baseline/static/totals/estate.csv')
  if (file.exists(baseline_estate_path)) {
    baseline_estate = baseline_estate_path %>%
      read_csv(show_col_types = F) %>%
      select(year, est_tax_exp_baseline = est_tax_exp)
  } else {

    # SLURM Phase 3a runs scenarios as a parallel array, so the baseline's
    # totals may not be written yet when a counterfactual's receipts job
    # runs; rebuild the baseline series from its detail files, which Phase 2
    # guarantees exist (counterfactual years read baseline detail)
    baseline_estate = get_estate_totals_from_detail(
      detail_root = file.path(globals$baseline_root, 'baseline/static/detail'),
      years       = totals$year
    )
    if (!is.null(baseline_estate)) {
      baseline_estate %<>% select(year, est_tax_exp_baseline = est_tax_exp)
    } else {
      warning('estate: no baseline totals/estate.csv at ',
              baseline_estate_path, ' and baseline detail lacks estate ',
              'columns (baseline run predates the on-model estate tax?). ',
              'On-model estate deltas are set to 0 — estate reform effects ',
              'will NOT appear in receipts. Re-run the baseline.')
    }
  }
  
  # Read excess growth offset (on CY basis) and convert to FY basis
  excess_growth_offset_cy = scenario_root %>% 
    file.path('/supplemental/excess_growth_offset.csv') %>% 
    read_csv(show_col_types = F)
  
  # Convert CY excess growth offset to FY basis using 75%/25% weighting
  # FY factor = 75% of current CY factor + 25% of previous CY factor
  excess_growth_offset_fy = excess_growth_offset_cy %>%
    mutate(
      income_factor_fy = 0.75 * income_factor + 0.25 * lag(income_factor, default = 1)
    ) %>%
    select(year, income_factor_fy)
  
  totals %>%

    # Join the model-baseline estate series (delta 0 when unavailable)
    { if (!is.null(baseline_estate)) left_join(., baseline_estate, by = 'year')
      else mutate(., est_tax_exp_baseline = est_tax_exp) } %>%
    mutate(

      # FY receipts: nonwithheld tax plus 75% of current CY withheld tax plus
      # 25% of previous CY withheld. lag default=0 zero-imputes the missing
      # prior CY when the sim starts at year t with no t-1 lead-in. For
      # deltas, this is unbiased whenever ΔCY (t-1) = 0 (i.e., a reform whose
      # effect starts at CY t). The partial-FY first-year row is written
      # only to the internal receipts_full.csv used by calc_rev_est; the
      # public receipts.csv still drops it.
      outlays_tax_credits = 0.75 * pmt_refund_withheld +
        0.25 * lag(pmt_refund_withheld, default = 0) +
        pmt_refund_nonwithheld,

      revenues_income_tax = 0.75 * pmt_iit_withheld +
        0.25 * lag(pmt_iit_withheld, default = 0) +
        pmt_iit_nonwithheld,

      revenues_payroll_tax = 0.75 * pmt_pr_withheld +
        0.25 * lag(pmt_pr_withheld, default = 0) +
        pmt_pr_nonwithheld,

      delta_revenues_corp_tax = 0.75 * corp_tax_change +
        0.25 * lag(corp_tax_change, default = 0),

      # On-model estate tax delta on an FY basis: CY t decedents' liability is
      # due 9 months after death (Form 706), so it books entirely in FY t + 1.
      # lag(default = 0) zero-imputes the missing prior CY in the first sim
      # year — same lead-in convention as the withheld splits above.
      delta_revenues_estate_tax = lag(
        coalesce(est_tax_exp - est_tax_exp_baseline, 0), default = 0),

      # On-model annual wealth tax: a pure LEVEL (no CBO anchor, unlike estate's
      # level+delta — there is no current-law wealth tax to anchor to). Like any
      # NON-WITHHELD tax it is paid at filing in the spring AFTER the assessment
      # year (CY t net worth → return filed Jan–Apr CY t+1 → FY t+1), so it books
      # entirely in FY t+1: revenues_wealth_tax[FY t] = wealth_tax[CY t-1]. This
      # matches the estate t+1 lag mechanically (lag with zero-imputed prior CY),
      # NOT the in-year booking the model currently uses for ordinary nonwithheld
      # income tax (a known simplification in remit_taxes/calc_receipts). It is
      # on-model (computed on already-adjusted data), so it does NOT take the
      # excess-growth `all_rev` factor below.
      revenues_wealth_tax = lag(coalesce(wealth_tax, 0), default = 0)
    ) %>%
    
    
    # Join VAT levels 
    left_join(revenues_vat, by = 'year') %>% 
    
    # Join other revenue levels
    left_join(revenues_other, by = 'year') %>% 
    mutate(revenues_corp_tax = revenues_corp_tax + delta_revenues_corp_tax) %>% 
    
    # Join deltas from cost recovery changes
    left_join(deltas_cost_recovery, by = 'year') %>% 
    mutate(revenues_corp_tax = revenues_corp_tax + delta_revenues_cost_recovery) %>% 
    
    # Join off-model estimates (deltas) and aggregate, applying excess growth offset where necessary
    left_join(deltas_off_model, by = 'year') %>% 
    left_join(excess_growth_offset_fy, by = 'year') %>% 
    mutate(revenues_payroll_tax = revenues_payroll_tax + (payroll * income_factor_fy),
           revenues_income_tax = revenues_income_tax + (individual * income_factor_fy),
           revenues_corp_tax   = (revenues_corp_tax + corporate) * income_factor_fy,

           # CBO level plus the on-model delta. The off-model `estate` column
           # is superseded by the on-model estate tax and no longer applied.
           revenues_estate_tax = revenues_estate_tax + delta_revenues_estate_tax,
           revenues_vat        = revenues_vat + vat) %>%
    
    # Apply excess growth to non-IIT/payroll/corporate revenues if applicable 
    mutate(
      all_rev = excess_growth_all_rev,
      across(
        .cols = c(revenues_estate_tax, revenues_vat, revenues_other), 
        .fns  = ~ . * if_else(all_rev == 1, income_factor_fy, 1)
      )
    ) %>% 
    
    # Final column selection
    select(year, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits,
           revenues_corp_tax, revenues_estate_tax, revenues_wealth_tax,
           revenues_vat, revenues_other) ->
    fy_receipts

  # Internal sidecar: keeps the partial first sim year so calc_rev_est can
  # produce an FY t delta when sim starts at the policy effective year t.
  fy_receipts %>%
    write_csv(file.path(scenario_root, 'totals', 'receipts_full.csv'))

  # Public file: drops the partial first year (preserves existing format
  # for external readers / analysis scripts).
  fy_receipts %>%
    filter(year != min(year)) %>%
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
  
  # Read baseline receipts (sidecar: includes partial first sim year)
  baseline = file.path(globals$baseline_root,
                       'baseline',
                       'static',
                       'totals',
                       'receipts_full.csv') %>%
    read_receipts_long(values_to = 'baseline')

  # Read VAT price offset for baseline dollars calculation
  vat_price_offset = read_vat_offset(id)

  for (static in c(T, F)) {

    # Read in counterfactual scenario receipts (sidecar: includes partial
    # first sim year)
    scenario = file.path(globals$output_root,
                         id,
                         if_else(static, 'static', 'conventional'),
                         'totals',
                         'receipts_full.csv') %>%
      read_receipts_long(values_to = 'counterfactual')

    # Read GDP and adjust for VAT (i.e. price level rises)
    gdp = interface_root('Macro-Projections') %>%
      read_macro_spliced() %>%
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
                              'revenues_payroll_tax', 'revenues_corp_tax',
                              'revenues_estate_tax', 'revenues_wealth_tax', 'revenues_other',
                              'revenues_vat', 'outlays_tax_credits'))
      ) %>%
      mutate(Series = case_when(
        Series == 'total'                ~ 'Total budget effect',
        Series == 'revenues_payroll_tax' ~ '  Revenues, payroll tax',
        Series == 'revenues_income_tax'  ~ '  Revenues, individual income tax',
        Series == 'outlays_tax_credits'  ~ '  Outlays, refundable tax credits',
        Series == 'revenues_corp_tax'    ~ '  Revenues, corporate income tax',
        Series == 'revenues_estate_tax'  ~ '  Revenues, estate tax',
        Series == 'revenues_wealth_tax'  ~ '  Revenues, wealth tax',
        Series == 'revenues_vat'         ~ '  Revenues, value added tax',
        Series == 'revenues_other'       ~ '  Revenues, other'
      ))
    
    # Convert to measure-indexed list
    rev_est = split_by_measure(rev_est)
    
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
    
    # Format numbers and cells (fixed layout: 9 series rows per block)
    style_rev_sheet(
      wb          = wb,
      sheet       = as.character(id),
      n_cols      = ncol(rev_est$Dollars),
      comma_rows  = c(2:10, 14:21),
      pct_rows    = 25:31,
      border_rows = c(1, 2, 10, 12, 13, 21, 23, 24, 32),
      bold_rows   = c(2, 13, 24),
      center_rows = 2:31
    )

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
  
  # Read VAT price offset for baseline dollars calculation
  vat_price_offsets = counterfactual_ids %>%
    map(.f = ~ read_vat_offset(.x) %>%
          mutate(scenario_id = .x)
    ) %>%
    bind_rows()

  for (static in c(T, F)) {

    stacked_rev_est = c('baseline', counterfactual_ids) %>%

      # Read scenario receipts file and store (sidecar: includes partial
      # first sim year)
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root),
                           .x,
                           if_else(static | .x == 'baseline', 'static', 'conventional'),
                           'totals',
                           'receipts_full.csv') %>%
            read_csv(show_col_types = F) %>%
            add_receipts_total('Dollars') %>%
            mutate(scenario_id = .x) %>%
            select(scenario_id, year, Dollars, vat = revenues_vat)) %>%
      bind_rows() %>%

      # Calculate share-of-GDP metric, accounting  for introduction of a VAT
      left_join(
        interface_root('Macro-Projections') %>%
          read_macro_spliced() %>%
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
    stacked_rev_est = split_by_measure(stacked_rev_est) %>%
      map(.f = ~ .x %>%
            bind_rows(
              .x %>%
                summarise(across(.cols = -scenario_id,
                                 .fns  = sum)) %>%
                mutate(scenario_id = 'Total')
            ) %>%
            rename(Scenario = scenario_id))
    
    
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
    
    
    # Format numbers and cells (nrow-dependent layout)
    style_rev_sheet(
      wb          = wb,
      sheet       = 'Stacked revenue estimates',
      n_cols      = ncol(stacked_rev_est$Dollars),
      comma_rows  = c(2:(nrow(stacked_rev_est$Dollars) + 2),
                      1 + start_real:(start_real + nrow(stacked_rev_est$`Baseline dollars`))),
      pct_rows    = 1 + start_gdp:(start_gdp + nrow(stacked_rev_est$`Share of GDP`)),
      border_rows = c(1, 2, nrow(stacked_rev_est$Dollars) + c(1, 2),
                      start_real + c(-1, 0, nrow(stacked_rev_est$`Baseline dollars`) + c(-1, 0)),
                      start_gdp  + c(-1, 0, nrow(stacked_rev_est$`Share of GDP`) + c(-1, 0))),
      bold_rows   = c(2, start_real, start_gdp),
      center_rows = 2:(1 + start_gdp + nrow(stacked_rev_est$`Share of GDP`))
    )


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



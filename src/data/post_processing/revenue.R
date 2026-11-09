#-------------------------------------------------------------------------
# revenue.R
#
# Post-processing functions to produce revenue estimates for fiscal years
#-------------------------------------------------------------------------



# The reportable rungs, in ladder order. Every counterfactual carries all three:
# a rung that cannot differ from the one below reports that rung's totals.
REPORTING_LEGS = c('static', 'mechanical', 'conventional')



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



ome_corp_col = function(ome_df, static) {

  #----------------------------------------------------------------------------
  # Resolves the pass-appropriate corporate Off-Model-Estimates revenue series.
  # From interface version 5 the corporate off-model estimate carries two streams.
  # The conventional leg reads the corporate column, which holds the modeled
  # corporate behavioral response gross of the on-model individual tax offset. The
  # static leg reads corporate_static, the mechanical revenue change, which the
  # static receipts booking and the distribution smear consume. The record
  # incidence channel stays on the conventional stream.
  #
  # A static-leg read of a revenues.csv without the static column stops the run.
  # Scenarios with no corporate provision read the baseline off-model estimate,
  # whose static column is zero.
  #
  # Parameters:
  #   - ome_df (df)   : an Off-Model-Estimates revenues.csv, read as-is
  #   - static (lgl)  : TRUE for the static leg, FALSE for conventional
  #
  # Returns: a two-column tibble (year, corporate) holding the resolved series.
  #----------------------------------------------------------------------------

  if (static) {
    if (!'corporate_static' %in% names(ome_df)) {
      stop('Off-Model-Estimates revenues.csv lacks a `corporate_static` column: ',
           'static-leg corporate receipts and the distribution smear require the ',
           'two-stream structure of interface version 5 or later. Point the ',
           "economy leg's interfaces.yaml at a later vintage, or regenerate the ",
           'vintage with a corporate_static stream.', call. = FALSE)
    }
    return(ome_df %>% transmute(year, corporate = corporate_static))
  }

  ome_df %>% transmute(year, corporate = corporate)
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
  # Applies the revenue workbook's number formats, borders, bold headers,
  # centering, and column widths to a sheet. Callers supply the row positions,
  # which differ between the single-scenario report and the stacked one.
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
                         cost_recovery_root, off_model_root,
                         leg = c('static', 'mechanical', 'conventional')) {
  
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
  #   - leg                   (str) : 'static', 'mechanical' or 'conventional'.
  #        Selects the corporate off-model stream and the on-model rate delta: the
  #        mechanical corporate change on the static and mechanical legs, the
  #        behavioral one on the conventional leg. There is no corporate-level
  #        avoidance on the mechanical rung, which carries the individual-side
  #        channels alone.
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
  
  leg = match.arg(leg)

  # The corporate streams come in two forms, mechanical and behavioral, and the
  # mechanical leg reads the same one the static leg does.
  corp_mechanical = leg %in% c('static', 'mechanical')

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

  # Resolve the pass's corporate stream and put it in the corporate column, so
  # that the booking below reads one name either way. Both raw columns are dropped
  # first so the rejoin cannot collide.
  corp_series = ome_corp_col(deltas_off_model, static = corp_mechanical)
  deltas_off_model = deltas_off_model %>%
    select(-any_of(c('corporate', 'corporate_static'))) %>%
    left_join(corp_series, by = 'year')

  # Calculate the on-model corporate statutory rate delta, booked on top of the
  # CBO corporate receipts level. The static and mechanical passes take the
  # mechanical change and the conventional pass the base-eroded one; see
  # src/sim/corp_rate.R. The
  # scenario's rate comes from this pass's tax law file. Receipts are already on a
  # fiscal year basis, so this books directly beside cost recovery below with no
  # calendar year smear.
  deltas_corp_rate = corp_rate_delta(
    rate_series = corp_rate_read_series(
      file.path(scenario_root, 'supplemental', 'tax_law.csv')),
    rev_corp    = revenues_other %>% select(year, rev_corp = revenues_corp_tax),
    static      = corp_mechanical
  ) %>%
    rename(delta_revenues_corp_rate = delta)

  # Read the model baseline estate tax series. Estate revenue is reported as the
  # CBO level plus the on-model delta of scenario against model baseline, so that
  # baseline receipts stay anchored to CBO and the model supplies only reform
  # deltas, which is the quantity it is validated on. The baseline leg is always
  # the baseline's static totals.
  baseline_estate_path = globals$baseline_root %>%
    file.path('baseline/static/totals/estate.csv')
  baseline_estate = NULL
  if (file.exists(baseline_estate_path)) {
    baseline_estate = baseline_estate_path %>%
      read_csv(show_col_types = F) %>%
      select(year, est_tax_exp_baseline = est_tax_exp)

    # A totals file covering only part of this scenario's years, from a baseline
    # run over a shorter window, would zero the estate delta for the uncovered
    # years in the coalesce below. Rebuild from detail instead
    if (!all(totals$year %in% baseline_estate$year)) {
      baseline_estate = NULL
    }
  }
  if (is.null(baseline_estate)) {

    # Phase 3a of the SLURM pipeline runs scenarios as a parallel array, so the
    # baseline totals may not be written when a counterfactual's receipts job
    # runs. Rebuild the series from the baseline detail files, which Phase 2
    # guarantees exist
    baseline_estate = get_estate_totals_from_detail(
      detail_root = file.path(globals$baseline_root, 'baseline/static/detail'),
      years       = totals$year
    )
    if (!is.null(baseline_estate)) {
      baseline_estate %<>% select(year, est_tax_exp_baseline = est_tax_exp)
    } else {
      warning('estate: baseline totals/estate.csv at ', baseline_estate_path,
              ' is missing or does not cover years ',
              min(totals$year), ':', max(totals$year), ', and the baseline ',
              'detail files cannot supply them (missing, or predating the ',
              'on-model estate tax?). On-model estate deltas are set to 0 — ',
              'estate reform effects will NOT appear in receipts. Re-run ',
              'the baseline.')
    }
  }
  
  totals %>%

    # Join the model baseline estate series, taking a zero delta when unavailable
    { if (!is.null(baseline_estate)) left_join(., baseline_estate, by = 'year')
      else mutate(., est_tax_exp_baseline = est_tax_exp) } %>%
    mutate(

      # Fiscal year receipts are nonwithheld tax plus 75% of the current calendar
      # year's withheld tax plus 25% of the prior year's. The lag default imputes
      # zero for the missing prior year when the simulation starts at the year the
      # policy takes effect, which is unbiased for a delta whenever that prior
      # year's change is zero. The resulting partial first-year row is written to
      # receipts_full.csv and dropped from receipts.csv.
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

      # Book the estate tax delta a year late. Liability on a decedent dying in
      # calendar year t is due nine months after death on Form 706, so it falls
      # entirely in fiscal year t + 1. The lag imputes zero for the first
      # simulation year, as the withheld splits above do.
      delta_revenues_estate_tax = lag(
        coalesce(est_tax_exp - est_tax_exp_baseline, 0), default = 0),

      # Book the annual wealth tax a year late, as a level. There is no current-law
      # wealth tax to anchor to, so unlike the estate tax it is not a delta against
      # a CBO series. Being nonwithheld, it is paid at filing in the spring after
      # the assessment year, so net worth in calendar year t is remitted in fiscal
      # year t + 1. The model books ordinary nonwithheld income tax in year, which
      # is a simplification in calc_receipts.
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

    # Join the on-model statutory rate delta onto the corporate line, where the
    # off-model rate wedge it replaces was booked
    left_join(deltas_corp_rate, by = 'year') %>%
    mutate(revenues_corp_tax = revenues_corp_tax +
             coalesce(delta_revenues_corp_rate, 0)) %>%
    
    # Join off-model estimates (deltas) and aggregate
    left_join(deltas_off_model, by = 'year') %>%
    mutate(revenues_payroll_tax = revenues_payroll_tax + payroll,
           revenues_income_tax  = revenues_income_tax + individual,
           revenues_corp_tax    = revenues_corp_tax + corporate,
           revenues_vat         = revenues_vat + vat,

           # CBO level plus the on-model delta. The off-model estate column is
           # superseded by the on-model estate tax
           revenues_estate_tax  = revenues_estate_tax + delta_revenues_estate_tax) %>%
    
    # Final column selection
    select(year, revenues_payroll_tax, revenues_income_tax, outlays_tax_credits,
           revenues_corp_tax, revenues_estate_tax, revenues_wealth_tax,
           revenues_vat, revenues_other) ->
    fy_receipts

  # Write the internal file, keeping the partial first year so that calc_rev_est
  # can produce a delta for it when the simulation starts at the effective year
  fy_receipts %>%
    write_csv(file.path(scenario_root, 'totals', 'receipts_full.csv'))

  # Write the public file, dropping the partial first year
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
  
  # Read baseline receipts, including the partial first year
  baseline = file.path(globals$baseline_root,
                       'baseline',
                       'static',
                       'totals',
                       'receipts_full.csv') %>%
    read_receipts_long(values_to = 'baseline')

  # Read VAT price offset for baseline dollars calculation
  vat_price_offset = read_vat_offset(id)

  # One estimate per reportable rung. The mechanical rung's totals are written for
  # every counterfactual, from the static ones where the rung was skipped, so the
  # loop needs no special case.
  for (leg in REPORTING_LEGS) {

    # Read in counterfactual scenario receipts, including the partial first year
    scenario = file.path(globals$output_root,
                         id,
                         leg,
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
                          leg,
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
    
    # Format numbers and cells, nine series rows per block
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
                                  leg,
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

  for (leg in REPORTING_LEGS) {

    stacked_rev_est = c('baseline', counterfactual_ids) %>%

      # Read scenario receipts file, including the partial first year. Baseline has
      # one rung only
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root),
                           .x,
                           if_else(.x == 'baseline', 'static', leg),
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
    
    
    # Format numbers and cells, at row positions that depend on the table size
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
                                  leg,
                                  'supplemental', 
                                  'stacked_revenue_estimates.xlsx'), 
                 overwrite = T)
  }
}



#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------



process_for_distribution = function(id, baseline_id, year, financing = 'none', 
                                    corp_delta, labor_share, vat_price_offset) {
  
  #----------------------------------------------------------------------------
  # Reads and cleans input data for a given scenario and a given "baseline", 
  # calculating tax change variables at the record level.
  # 
  # Parameters:
  #   - id          (str)     : scenario ID
  #   - baseline_id (str)     : ID of scenario against which metrics are 
  #                             calculated. For regular tables, this is the 
  #                             actual baseline; for stacked tables, this is 
  #                             the precedeing scenario
  #   - year        (int)     : year to calculate metrics for
  #   - financing   (str)     : assumption for how any deficit effect is 
  #                             financed. "none" for none, "head" for 
  #                             proportional tax, "income" for flat income tax, 
  #                             "liability" for current-law income tax 
  #   - corp_delta  (dbl)     : corporate tax change under scenario, billions
  #   - labor_share (dbl)     : labor's share of the burden of corporate tax
  #   - vat_price_offset (df) : CPI change attributable to introduction of VAT
  # 
  # Returns: microdata with all record-level variables required to calculate
  #          aggregate distributional metrics (df).
  #----------------------------------------------------------------------------
  
  
  #-----------------------
  # Read microdata output
  #-----------------------
  
  if (baseline_id == 'baseline') {
    baseline_root = file.path(globals$baseline_root, 'baseline')
  } else {
    baseline_root = file.path(globals$output_root, baseline_id)
  }
  
  baseline = file.path(baseline_root, 'static/detail', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble()
  
  reform = file.path(globals$output_root, id, 'static/detail', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble()
  
  
  #------------------------------------
  # Calculate record-level tax changes
  #------------------------------------
  
  microdata = baseline %>% 
    
    # Remove dependent returns
    filter(dep_status == 0) %>% 
    
    # Join counterfactual scenario liability, expressed in baseline dollars.
    # Calculate helper information for VAT breakdown supplemental file.  
    mutate(liab_baseline = liab_iit_net + liab_pr) %>% 
    left_join(reform %>% 
                mutate(
                  liab       = (liab_iit_net + liab_pr)                 / vat_price_offset, 
                  ss_new     = gross_ss                                 / vat_price_offset, 
                  debt_new   = (txbl_int + exempt_int - first_mort_int) / vat_price_offset, 
                  equity_new = (div_ord + div_pref + kg_st + kg_lt)     / vat_price_offset,
                  mixed_new  = (sole_prop + part_scorp + farm)          / vat_price_offset,
                  inc_new    = expanded_inc                             / vat_price_offset, 
                  other_new  = inc_new - ss_new - debt_new - equity_new - mixed_new) %>% 
                select(id, liab, ends_with('_new')), 
              by = 'id') %>%
    mutate(
      
      # Create adult-level weight
      weight_person = weight * (1 + (filing_status == 2)),
      
      
      #--------------------------
      # VAT burden determination
      #--------------------------
      
      # VAT burden is the loss of real income from higher prices. Some components 
      # of expanded income will rise with prices (e.g. OASDI or capital income), 
      # others won't; compositional differences determine distributional impact 
      vat_burden = expanded_inc - inc_new, 
      

      #--------------------------
      # Corporate tax allocation 
      #--------------------------
      
      # Calculate factor incomes
      labor   = pmax(0, wages + (sole_prop + part_scorp + farm) * 0.8), 
      capital = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int + 
                        exempt_int + div_ord + div_pref + kg_st + kg_lt),
      
      # Then allocate corporate tax change in accordance with assumed labor incidence  
      corp_tax_labor   = corp_delta * 1e9 * labor_share       * (labor / sum(labor * weight)),
      corp_tax_capital = corp_delta * 1e9 * (1 - labor_share) * (capital / sum(capital * weight)),
      
  
      other_taxes = corp_tax_labor + corp_tax_capital + vat_burden, 
      
      
      #--------------------
      # Deficit allocation
      #--------------------
      
      financing_cost = sum((liab - liab_baseline + other_taxes) * weight) * case_when(
        financing == 'none'      ~ 0,
        financing == 'head'      ~ (1 + (filing_status == 2)) / sum(weight_person),
        financing == 'income'    ~ pmax(0, expanded_inc) / sum(pmax(0, expanded_inc) * weight),
        financing == 'liability' ~ pmax(0, liab_baseline) / sum(pmax(0, liab_baseline) * weight)
      ),
      
      
      #----------------
      # Overall change
      #----------------
      
      # Calculate total change from baseline
      delta = (liab - liab_baseline) + other_taxes - financing_cost,
      
      # Binary dummies for if a tax unit received a meaningful raise or cut
      cut   = delta <= -5,
      raise = delta >= 5,
      
    ) 
  
  #------------------------
  # Add grouping variables
  #------------------------
  
  # Calculate income thresholds
  income_groups = wtd.quantile(
    x       = microdata %>% 
                filter(expanded_inc >= 0) %>%
                get_vector('expanded_inc'), 
    probs   = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999),
    weights = microdata %>% 
                filter(expanded_inc >= 0) %>%
                get_vector('weight_person')
  )
  
  
  microdata %>%  
    mutate(
      
      # Assign income groups
      income_group = cut(
        x              = expanded_inc, 
        breaks         = c(-Inf, 0, income_groups, Inf), 
        labels         = c('Negative income', 'Bottom quintile', 'Second quintile', 
                           'Middle quintile', 'Fourth quintile', '80% - 90%', 
                           '90% - 99%', '99% - 99.9%', 'Top 0.1%'), 
        right          = F, 
        include.lowest = T
      ),
      
      # Assign age groups
      oldest_adult = if_else(filing_status == 2, pmax(age1, age2), age1),
      age_group = case_when(
        oldest_adult < 25 ~ '24 and under', 
        oldest_adult < 30 ~ '25 - 29', 
        oldest_adult < 40 ~ '30 - 39', 
        oldest_adult < 50 ~ '40 - 49',
        oldest_adult < 65 ~ '50 - 64', 
        T                 ~ '65+'
      ) 
    ) %>%
    return()
}



calc_dist_metrics = function(microdata, group_var) {
  
  #----------------------------------------------------------------------------
  # Aggregates record-level tax change microdata into summary stats, grouped
  # either by income or age. 
  # 
  # Parameters: 
  #  - microdata (df)  : tax unit data, ouput by process_for_distribution()
  #  - group_var (str) : variable over which to calculate group summary stats, 
  #                      either "income_group" or "age_group"
  # 
  # Returns: tibble of distributional metrics grouped by group_var (df).
  #----------------------------------------------------------------------------
  
  # Calculate metrics by specified group
  microdata %>% 
    group_by(!!sym(group_var)) %>%
    summarise(
      
      # Group-metric-specific summary stats
      income_cutoff = round(min(expanded_inc) / 5) * 5,
      n_tax_units   = sum(weight), 
      
      # Income group's total dollar amount tax change
      group_delta = sum(round(delta) * weight),
      
      # Unconditional and conditional averages
      avg       = round(weighted.mean(delta, weight) / 5) * 5,
      avg_cut   = round(weighted.mean(delta, (weight * cut)) / 5) * 5,
      avg_raise = round(weighted.mean(delta, (weight * raise)) / 5) * 5,
      
      # Counts
      share_cut   = sum(weight * cut) / sum(weight),
      share_raise = sum(weight * raise) / sum(weight),
      
      # Relative changes
      pct_chg_ati = sum((expanded_inc - liab_baseline - delta) * weight) /
                    sum((expanded_inc - liab_baseline) * weight) - 1
    ) %>%
    
    # Group's share of total change
    mutate(share_total     = group_delta / sum(group_delta), 
           share_tax_units = n_tax_units / sum(n_tax_units)) %>% 
    return()
}



calc_pledge_metrics = function(microdata) {
  
  #----------------------------------------------------------------------------
  # Calculates share of tax units below a series of AGI thresholds which 
  # experience a tax hike of at least $X.
  # 
  # Parameters: 
  #  - microdata (df)  : tax unit data, output by process_for_distribution()
  # 
  # Returns: tibble of pledge metrics. 
  #----------------------------------------------------------------------------
  
  seq(0, 1e6, 50e3) %>% 
    map(.f = ~ microdata %>%
          filter(agi < .x) %>% 
          summarise(
            threshold = .x,
            across(
              .cols  = delta,  
              .fns   = list('5'   = ~ round(sum(((. > 5)   * weight) / sum(weight)), 4), 
                            '50'  = ~ round(sum(((. > 50)  * weight) / sum(weight)), 4), 
                            '100' = ~ round(sum(((. > 100) * weight) / sum(weight)), 4), 
                            '500' = ~ round(sum(((. > 500) * weight) / sum(weight)), 4)), 
              .names = '{fn}'
            ) 
          )
    ) %>% 
    bind_rows() %>% 
    return()
}




build_distribution_tables = function(id, baseline_id) {
  
  #----------------------------------------------------------------------------
  # Generates distribution tables by year and financing assumption for a
  # given scenario, both by income and age.
  # 
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  # 
  # Returns: void. 
  #----------------------------------------------------------------------------
  
  
  #------------------------------------------
  # Read and process non-individual tax data
  #------------------------------------------
  
  # Read VAT price offset for deflating other taxes
  vat_price_offset = globals$output_root %>% 
    file.path(id, '/static/supplemental/vat_price_offset.csv') %>% 
    read_csv(show_col_types = F)
  
  # Read baseline off-model revenues 
  rev_baseline = globals$baseline_root %>% 
    file.path('baseline/static/totals/receipts.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, baseline = revenues_corp_tax)
  
  # Read counterfactual scenario off-model revenues 
  rev_reform = globals$output_root %>% 
    file.path(id, '/static/totals/receipts.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, reform = revenues_corp_tax) %>% 

    # Express corporate tax in baseline (consumer) dollars
    left_join(vat_price_offset, by = 'year') %>%
    mutate(reform = reform / cpi_factor) %>% 
    select(-ends_with('_factor'))
  
  # Calculate change in corporate tax liability by year
  corp_delta = rev_reform %>% 
    left_join(rev_baseline, by = 'year') %>% 
    mutate(delta = reform - baseline) %>%  
  
    # Determine first year of policy reform, if any, and allocate labor
    # share of changed corporate burden over time
    mutate(first_year = ifelse(sum(delta) != 0, 
                               min(year[cumsum(delta) != 0 & lag(delta, default = 0) == 0]), 
                               Inf), 
           labor_share = 0.2 * pmax(0, pmin(1, (year - first_year) / 10))) %>% 
    select(year, delta, labor_share) 

  
  #---------------------------------------------------------------
  # Loop over year X grouping X financing X incidence assumptions
  #---------------------------------------------------------------
  
  # Initialize lists of unformatted tables
  results = list('income' = list(), 
                 'age'    = list())
  
  pledge_results = list()
    
  # Create new Excel workbook
  wb = createWorkbook()

  # Year loop 
  first_year = get_scenario_info(id)$years[1]
  for (yr in get_scenario_info(id)$dist_years) {
  
    # Skip year if it's the first year in the run -- we wont have corporate 
    # receipts data for this year 
    if (yr == first_year) {
      next
    }
    
    # Get corporate tax info for this year
    this_corp_delta = corp_delta %>% 
      filter(year == yr) %>% 
      get_vector('delta')
    this_corp_labor_share = corp_delta %>% 
      filter(year == yr) %>% 
      get_vector('labor_share')
    
    # Get vat info for this year
    this_vat_price_offset = vat_price_offset %>% 
      filter(year == yr) %>% 
      get_vector('cpi_factor')
    
    # Grouping variable loop
    for (group_var in c('income', 'age')) {
      
      # Financing assumption loop
      for (financing in c('none', 'liability')) {
        
        # Corporate tax and VAT inclusion loop
        for (include_other in c(F, T)) {
        
          # Process microdata
          microdata = process_for_distribution(
            id               = id, 
            baseline_id      = 'baseline', 
            year             = yr, 
            financing        = financing, 
            corp_delta       = if_else(include_other, this_corp_delta, 0), 
            labor_share      = this_corp_labor_share, 
            vat_price_offset = if_else(include_other, this_vat_price_offset, 0)
          )
          
          # Calculate standard metrics and add to results 
          dist_metrics = calc_dist_metrics(microdata, paste0(group_var, '_group'))
          
          # Add to results 
          results[[group_var]][[length(results[[group_var]]) + 1]] = dist_metrics %>% 
            mutate(year = yr, .before = everything()) %>%
            mutate(includes_other = include_other, financing = financing, .after = year)
          
          # Add to Excel workbook and format
          format_table(dist_metrics, wb, yr, paste0(group_var, '_group'), financing, include_other)
          
          # Calculate pledge metrics (separate product from metrics above)
          pledge_results[[length(pledge_results) + 1]] = microdata %>% 
            calc_pledge_metrics() %>%
            mutate(year = yr, .before = everything()) %>%
            mutate(includes_other = include_other, financing = financing, .after = year)
        }
      }
    }
  }
  
  # Write CSVs
  c('income', 'age') %>% 
    walk(.f = ~ write_csv(
      x    = bind_rows(results[[.x]]), 
      file = file.path(globals$output_root, 
                       id,
                       'static',
                       'supplemental', 
                       paste0('distribution_', .x, '.csv'))
      
    ))
  
  # Write workbook 
  saveWorkbook(wb   = wb, 
               file = file.path(globals$output_root, 
                                id,
                                'static',
                                'supplemental', 
                                'distribution.xlsx'), 
               overwrite = T)
  
  
  # Write pledge output 
  pledge_results %>% 
    bind_rows() %>%
    write_csv(file.path(globals$output_root, id, 'static/supplemental/pledge_metrics.csv'))
}



format_table = function(dist_metrics, wb, year, group_var, financing, other) {

  
  #----------------------------------------------------------------------------
  # Given a tibble of distributional metrics calculated either by income or
  # age, places the output in a WorkBook object and formats the sheet.
  # 
  # Parameters:
  #   - dist_metrics (df)   : tibble of aggregated distributional metrics
  #   - wb           (wb)   : destination WorkBook object for output
  #   - year         (int)  : year of distributional metrics
  #   - group_var    (str)  : either "income_group" or "age_group", representing
  #                           the variable by which dist_metrics are grouped
  #   - financing    (str)  : financing assumption
  #   - other        (bool) : whether estimates includes burden of corporate 
  #                           tax and VAT
  # 
  # Returns: void.
  #----------------------------------------------------------------------------
  
  #---------------------------------
  # Formatting for by-income tables
  #---------------------------------
  
  # Set worksheet name
  sheet_name = paste0(
    year, ', ',
    if_else(other, 'all', 'direct only'), ', ',
    if_else(financing != 'none', 'financing', 'no financing')
  )
  
  # Write out financing description
  financing_description = case_when(
    financing == 'none'      ~ 'no financing',
    financing == 'head'      ~ 'per-person financing',
    financing == 'income'    ~ 'financing proportional to income',
    financing == 'liability' ~ 'financing proportional to income taxes'
  )
  
  # Write out other tax incidence assumption
  incidence_description = if_else(other,
    'including incidence of corporate tax and VAT changes',
    'individual income taxes and payroll taxes only'
  )
  
  if (group_var == 'income_group') {
    
    dist_table = dist_metrics %>% 
      
      # Clean up -- deal with missings, divide-by-zeros, things of that nature
      mutate(
        income_cutoff = if_else(row_number() == 1, NA, income_cutoff), 
        pct_chg_ati   = if_else(row_number() == 1, NA, pct_chg_ati), 
        avg_cut       = if_else(is.nan(avg_cut) | round(share_cut, 4) == 0, NA, avg_cut),
        avg_raise     = if_else(is.nan(avg_raise) | round(avg_raise, 4) == 0, NA, avg_raise),
        share_total   = if_else(is.nan(share_total) | financing != 'none', NA, share_total)
      ) %>% 
      
      # Format names
      select(`Income group`                       = income_group, 
             `Income cutoff`                      = income_cutoff, 
             `Average tax change`                 = avg, 
             `Share with tax cut`                 = share_cut, 
             `Average tax cut`                    = avg_cut, 
             `Share with tax increase`            = share_raise, 
             `Average tax increase`               = avg_raise, 
             `Percent change in after-tax income` = pct_chg_ati, 
             `Share of total tax change`          = share_total)
    
    # Add worksheet and table to workbook
    addWorksheet(wb, sheet_name)
    writeData(wb = wb, sheet = sheet_name, x = dist_table, startRow = 2)
    
    # Add titles and notes 
    title = paste0('Distributional impact by income group in ', year, ', ', 
                   incidence_description, ', ', financing_description)
    writeData(wb = wb, sheet = sheet_name, startRow = 1, 
              x = title)
    writeData(wb = wb, sheet = sheet_name, startRow = 12, 
              x = paste0('Estimate universe is nondependent tax units, including nonfilers.', 
                         '"Income" is measured as AGI plus: above-the-line deductions, ', 
                         'nontaxable interest, nontaxable pension income (including OASI ',
                         'benefits), and employer-side payroll taxes. Income percentile ', 
                         'thresholds are calculated with respect to positive income only ', 
                         'and are adult-weighted.' 
              )
    )
    
    # Format numbers and cells 
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = 3:11, 
             cols       = c(4, 6, 8, 9), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'PERCENTAGE'))
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = 3:11, 
             cols       = c(2, 3, 5, 7), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'COMMA'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = c(1, 2, 11), 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(border = 'bottom'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = 2, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(textDecoration = 'bold', 
                                      wrapText       = T), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = 2:11, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(halign = 'center'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = sheet_name, 
             rows       = 12, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(fontSize       = 8, 
                                      textDecoration = 'italic',
                                      valign         = 'center',
                                      wrapText       = T), 
             stack      = T)
    mergeCells(wb    = wb, 
               sheet = sheet_name, 
               rows  = 12:13, 
               cols  = 1:9)
    setColWidths(wb     = wb, 
                 sheet  = sheet_name, 
                 cols   = 1:9, 
                 widths = c(15, 8, 11, 11, 11, 11, 11, 15, 12))
  
  
  #---------------------------------
  # Formatting for by-age tables
  #---------------------------------
    
  } else {

    dist_table = dist_metrics %>%

      # Clean up -- deal with missings, divide-by-zeros, things of that nature
      mutate(
        avg_cut     = if_else(is.nan(avg_cut) | round(share_cut, 4) == 0, NA, avg_cut),
        avg_raise   = if_else(is.nan(avg_raise) | round(avg_raise, 4) == 0, NA, avg_raise),
        share_total = if_else(is.nan(share_total) | financing != 'none', NA, share_total)
      ) %>%

      # Format names
      select(`Age group`                          = age_group,
             `Share of tax units`                 = share_tax_units,
             `Average tax change`                 = avg,
             `Share with tax cut`                 = share_cut,
             `Average tax cut`                    = avg_cut,
             `Share with tax increase`            = share_raise,
             `Average tax increase`               = avg_raise,
             `Percent change in after-tax income` = pct_chg_ati,
             `Share of total tax change`          = share_total)

    # Add worksheet and table to workbook
    writeData(wb = wb, sheet = sheet_name, x = dist_table, startRow = 16)

    # Add titles and notes
    title = paste0('Distributional impact by age group in ', year, ', ', 
                   incidence_description, ', ', financing_description)
    writeData(wb = wb, sheet = sheet_name, startRow = 15,
              x = title)
    writeData(wb = wb, sheet = sheet_name, startRow = 23,
              x = paste0('Estimate universe is nondependent tax units, including nonfilers.',
                         '"Income" is measured as AGI plus: above-the-line deductions, ',
                         'nontaxable interest, nontaxable pension income (including OASI ',
                         'benefits), and employer-side payroll taxes. Income percentile ',
                         'thresholds are calculated with respect to positive income only ',
                         'and are adult-weighted.'
              )
    )

    # Format numbers and cells
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = 17:22,
             cols       = c(2, 4, 6, 8, 9),
             gridExpand = T,
             style      = createStyle(numFmt = 'PERCENTAGE'))
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = 17:22,
             cols       = c(3, 5, 7),
             gridExpand = T,
             style      = createStyle(numFmt = 'COMMA'),
             stack      = T)
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = c(15, 16, 22),
             cols       = 1:9,
             gridExpand = T,
             style      = createStyle(border = 'bottom'),
             stack      = T)
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = 16,
             cols       = 1:9,
             gridExpand = T,
             style      = createStyle(textDecoration = 'bold',
                                      wrapText       = T),
             stack      = T)
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = 16:22,
             cols       = 1:9,
             gridExpand = T,
             style      = createStyle(halign = 'center'),
             stack      = T)
    addStyle(wb         = wb,
             sheet      = sheet_name,
             rows       = 23,
             cols       = 1:9,
             gridExpand = T,
             style      = createStyle(fontSize       = 8,
                                      textDecoration = 'italic',
                                      valign         = 'center',
                                      wrapText       = T),
             stack      = T)
    mergeCells(wb    = wb,
               sheet = sheet_name,
               rows  = 23:24,
               cols  = 1:9)
    setColWidths(wb    = wb,
                 sheet  = sheet_name,
                 cols   = 1:9,
                 widths = c(15, 8, 11, 11, 11, 11, 11, 15, 12))
  }
}


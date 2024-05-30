#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------



process_for_distribution = function(id, baseline_id, year, financing = 'none', 
                                    corp_delta = 0, labor_share = 0) {
  
  #----------------------------------------------------------------------------
  # Reads and cleans input data for a given scenario and a given "baseline", 
  # calculating tax change variables at the record level.
  # 
  # Parameters:
  #   - id          (str) : scenario ID
  #   - baseline_id (str) : ID of scenario against which metrics are calculated. 
  #                         For regular tables, this is the actual baseline; 
  #                         for stacked tables, this is the precedeing scenario
  #   - year        (int) : year to calculate metrics for
  #   - financing   (str) : assumption for how any deficit effect is financed. 
  #                         'none' for none, "head" for proportional tax, 
  #                         "income" for flat income tax, "liability" for 
  #                         income tax 
  #   - corp_delta  (dbl) : corporate tax change under scenario, billions
  #   - labor_share (dbl) : labor's share of the burden of the corporate tax
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
  
  scenario = file.path(globals$output_root, id, 'static/detail', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble()
  
  
  #------------------------------------
  # Calculate record-level tax changes
  #------------------------------------
  
  microdata = baseline %>% 
    
    # Remove dependent returns
    filter(dep_status == 0) %>% 
    
    # Join scenario liability 
    mutate(liab_baseline = liab_iit_net + liab_pr) %>% 
    left_join(scenario %>% 
                mutate(liab = liab_iit_net + liab_pr) %>% 
                select(id, liab), 
              by = 'id') %>%
    mutate(
      
      # Create adult-level weight
      weight_person = weight * (1 + (filing_status == 2)),
      
      # Adjust for corporate tax changes. Calculate factor incomes
      labor   = pmax(0, wages + (sole_prop + part_scorp + farm) * 0.8), 
      capital = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int + 
                        exempt_int + div_ord + div_pref + kg_st + kg_lt),
      
      # The allocate corporate tax change in accordance with assumed labor incidence  
      corp_tax_labor   = corp_delta * 1e9 * labor_share       * (labor / sum(labor * weight)),
      corp_tax_capital = corp_delta * 1e9 * (1 - labor_share) * (capital / sum(capital * weight)),
      
      # Adjust for financing effects i.e. distributing deficit
      financing_cost = sum((liab - liab_baseline + corp_tax_labor + corp_tax_capital) * weight) * case_when(
        financing == 'none'      ~ 0,
        financing == 'head'      ~ (1 + (filing_status == 2)) / sum(weight_person),
        financing == 'income'    ~ pmax(0, expanded_inc) / sum(pmax(0, expanded_inc) * weight),
        financing == 'liability' ~ pmax(0, liab_baseline) / sum(pmax(0, liab_baseline) * weight)
      ),
      
      # Calculate change from baseline
      delta = (liab - liab_baseline) + corp_tax_labor + corp_tax_capital - financing_cost,
      
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




build_distribution_tables = function(id, baseline_id, file_name) {
  
  #----------------------------------------------------------------------------
  # Generates distribution tables by year and financing assumption for a
  # given scenario, both by income and age.
  # 
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  #   - file_name (str)   : name of file to prepend .xlsx and .csv
  # 
  # Returns: void. 
  #----------------------------------------------------------------------------
  
  
  #-------------------------------------
  # Read and process corporate tax data
  #-------------------------------------
  
  # Read baseline revenues 
  corp_baseline = globals$baseline_root %>% 
    file.path('baseline/static/totals/receipts.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, baseline = revenues_corp_tax)
  
  # Read counterfactual scenario revenues 
  corp_scenario = globals$output_root %>% 
    file.path(id, '/static/totals/receipts.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, scenario = revenues_corp_tax)
  
  # Calculate change in liability by year
  corp_delta = corp_scenario %>% 
    left_join(corp_baseline, by = 'year') %>% 
    mutate(corp_delta = scenario - baseline) %>% 
  
    # Determine first year of policy reform, if any, and allocate labor
    # share of new corporate burden over time
    mutate(first_year = ifelse(sum(corp_delta) > 0, 
                               min(year[cumsum(corp_delta) > 0 & lag(corp_delta) == 0]), 
                               Inf), 
           labor_share = 0.2 * pmax(0, pmin(1, (year - first_year) / 10))) %>% 
    select(year, corp_delta, labor_share) 

  #-----------------------------------
  # Loop over year-grouping-financing
  #-----------------------------------
  
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
      get_vector('corp_delta')
    this_corp_labor_share = corp_delta %>% 
      filter(year == yr) %>% 
      get_vector('labor_share')
    
    # Skip historical years, under the assumption we're scoring policy for the future...
    if (yr < year(Sys.time())) {
      next
    }
    
    # Grouping variable loop
    for (group_var in c('income', 'age')) {
      
      # Financing assumption loop
      for (financing in c('none', 'liability')) {
        
        # Corporate tax assumption loop
        for (include_corp in c(F, T)) {
        
          # Process microdata
          microdata = process_for_distribution(
            id          = id, 
            baseline_id = 'baseline', 
            year        = yr, 
            financing   = financing, 
            corp_delta  = if_else(include_corp, this_corp_delta, 0), 
            labor_share = this_corp_labor_share
          )
          
          # Calculate standard metrics and add to results 
          dist_metrics = calc_dist_metrics(microdata, paste0(group_var, '_group'))
          
          # Add to results 
          results[[group_var]][[length(results[[group_var]]) + 1]] = dist_metrics %>% 
            mutate(year = yr, .before = everything()) %>%
            mutate(includes_corp = include_corp, financing = financing, .after = year)
          
          # Add to Excel workbook and format
          format_table(dist_metrics, wb, yr, paste0(group_var, '_group'), financing, include_corp)
          
          # Calculate pledge metrics (separate product from metrics above)
          pledge_results[[length(pledge_results) + 1]] = microdata %>% 
            calc_pledge_metrics() %>%
            mutate(year = yr, .before = everything()) %>%
            mutate(includes_corp = include_corp, financing = financing, .after = year)
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
                       paste0(file_name, '_', .x, '.csv'))
      
    ))
  
  # Write workbook 
  saveWorkbook(wb   = wb, 
               file = file.path(globals$output_root, 
                                id,
                                'static',
                                'supplemental', 
                                paste0(file_name, '.xlsx')), 
               overwrite = T)
  
  
  # Write pledge output 
  if (file_name != 'stacked_distribution') {
    pledge_results %>% 
      bind_rows() %>%
      write_csv(file.path(globals$output_root, id, 'static/supplemental/pledge_metrics.csv'))
  }
}



build_all_distribution_tables = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # For all non-baseline scenarios, generates distribution tables.
  # 
  # Parameters:
  #   - counterfactual_ids : (str) list of non-baseline scenario IDs
  # 
  # Returns: void.
  #----------------------------------------------------------------------------
  
  for (id in counterfactual_ids) { 
    build_distribution_tables(id          = id, 
                              baseline_id = 'baseline', 
                              file_name   = 'distribution')
  }
}


build_all_stacked_distribution_tables = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # For all non-baseline scenarios, generates distribution tables relative to
  # prior scenario in stacking order. 
  # 
  # Parameters:
  #   - counterfactual_ids : (str) list of non-baseline scenario IDs
  # 
  # Returns: void.
  #----------------------------------------------------------------------------
  
  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  scenario_ids = c('baseline', counterfactual_ids)
  for (i in 2:length(scenario_ids)) { 
    build_distribution_tables(id          = scenario_ids[i], 
                              baseline_id = scenario_ids[i - 1], 
                              file_name   = 'stacked_distribution')
  }
}


format_table = function(dist_metrics, wb, year, group_var, financing, corp) {
  
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
  #   - corp         (bool) : whether estimates include corporate taxes
  # 
  # Returns: void.
  #----------------------------------------------------------------------------
  
  #---------------------------------
  # Formatting for by-income tables
  #---------------------------------
  
  # Set worksheet name
  sheet_name = paste0(
    year, ', ',
    if_else(corp, 'with corp', 'no corp'), ', ',
    if_else(financing != 'none', 'with financing', 'no financing')
  )
  
  # Write out financing description
  financing_description = case_when(
    financing == 'none'      ~ 'no financing',
    financing == 'head'      ~ 'per-person financing',
    financing == 'income'    ~ 'financing proportional to income',
    financing == 'liability' ~ 'financing proportional to income taxes'
  )
  
  # Write out corporate tax assumption
  corporate_tax_description = if_else(corp,
    'including incidence of corporate tax changes',
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
                   corporate_tax_description, ', ', financing_description)
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
                   corporate_tax_description, ', ', financing_description)
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


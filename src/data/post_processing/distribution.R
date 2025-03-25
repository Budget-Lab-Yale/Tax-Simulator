#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------



build_distribution_tables = function(id, baseline_id) {
  
  #----------------------------------------------------------------------------
  # Generates distribution tables by year and for a given scenario,
  # both by income and age.
  #
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  #
  # Returns: void.
  #----------------------------------------------------------------------------
  
  
  # Get info on VAT, corporate rate, and cost recovery changes
  other_taxes = get_other_taxes(id, baseline_id)

  # Loop over years 
  dist_tables = list()
  for (yr in get_scenario_info(id)$dist_years) {
    
    # Process microdata 
    microdata = process_for_distribution(id, baseline_id, yr, other_taxes)
    
    # Calculate overall averages
    dist_tables[[as.character(yr)]] = microdata %>% 
      group_by(taxes_included, group = 'Overall') %>% 
      calc_dist_metrics() %>% 
      mutate(group_dimension = 'Overall') %>% 
      
      # Add age cuts 
      bind_rows(
        microdata %>% 
          group_by(taxes_included, group = age_group) %>% 
          calc_dist_metrics() %>% 
          mutate(group_dimension = 'Age') 
      ) %>%
      
      # Add income quintile cuts  
      bind_rows(
        microdata %>% 
          group_by(taxes_included, group = replace_na(quintile, 'Negative income')) %>%
          calc_dist_metrics() %>%
          mutate(group_dimension = 'Income')
      ) %>% 
      
      # Add top income cuts
      bind_rows(
        microdata %>% group_by(taxes_included, group = top_10) %>% calc_dist_metrics() %>% filter(!is.na(group)) %>% mutate(group_dimension = 'Income'), 
        microdata %>% group_by(taxes_included, group = top_5)  %>% calc_dist_metrics() %>% filter(!is.na(group)) %>% mutate(group_dimension = 'Income'), 
        microdata %>% group_by(taxes_included, group = top_1)  %>% calc_dist_metrics() %>% filter(!is.na(group)) %>% mutate(group_dimension = 'Income'), 
        microdata %>% group_by(taxes_included, group = top_01) %>% calc_dist_metrics() %>% filter(!is.na(group)) %>% mutate(group_dimension = 'Income')
      ) %>% 
      
      # Add year indicator
      mutate(year = yr, .before = everything())
  }

  
  # Combine and write results
  dist_tables %>% 
    bind_rows() %>% 
    arrange(year, taxes_included) %>% 
    select(year, taxes_included, group_dimension, everything()) %>% 
    write_csv(file.path(globals$output_root, id, 'static/supplemental', paste0('distribution.csv')))
}
  


process_for_distribution = function(id, baseline_id, yr, other_taxes) {
  
  #----------------------------------------------------------------------------
  # Reads and cleans input data for a given scenario and a given "baseline",
  # calculating tax change variables at the record level.
  #
  # Parameters:
  #   - id          (str) : scenario ID
  #   - baseline_id (str) : ID of scenario against which metrics are calculated. 
  #                         For regular tables, this is the actual baseline; for 
  #                         stacked tables, this is the precedeing scenario
  #   - yr          (int) : year to calculate metrics for
  #   - other_taxes (df)  : tibble of metrics for CIT and VAT (see 
  #                         get_other_taxes())
  #
  # Returns: microdata with all record-level variables required to calculate
  #          aggregate distributional metrics (df).
  #----------------------------------------------------------------------------
  

  # Get file path for baseline 
  if (baseline_id == 'baseline') {
    baseline_root = file.path(globals$baseline_root, 'baseline')
  } else {
    baseline_root = file.path(globals$output_root, baseline_id)
  }
  
  # Read baseline microdata
  file.path(baseline_root, 'static/detail', paste0(yr, '.csv')) %>%
    fread() %>%
    tibble() %>% 
    
    # Remove dependent returns and extraneous variables for distribution calculation
    filter(dep_status == 0) %>%
    mutate(
      year          = yr,
      weight_person = weight * (1 + (filing_status == 2)),
      age           = if_else(filing_status == 2, pmax(age1, age2), age1),
      labor         = pmax(0, wages + (sole_prop + part_scorp + farm) * 0.8),
      capital       = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int + exempt_int + div_ord + div_pref + kg_st + kg_lt),
      liab_iit_pr   = liab_iit_net + liab_pr
    ) %>% 
    select(year, id, weight, weight_person, filing_status, age, labor, capital, income = expanded_inc, liab_iit_pr) %>% 
    
    # Make 3 copies for tax-type inclusion assumptions 
    expand_grid(taxes_included = c('iit_pr', 'iit_pr_estate', 'iit_pr_estate_cit_vat')) %>% 
    
    # Join counterfactual reform scenario tax microdata
    left_join(
      file.path(globals$output_root, id, 'static/detail', paste0(yr, '.csv')) %>%
        fread() %>%
        tibble() %>% 
        mutate(liab_iit_pr_reform  = liab_iit_net + liab_pr) %>%
        select(id, income_reform = expanded_inc, liab_iit_pr_reform),
      by = 'id'
    ) %>% 
    
    # Join estate tax data
    left_join(
      globals$interface_paths %>% 
        filter(ID == globals$interface_path$ID[1], interface == 'Estate-Tax-Distribution') %>%
        pull(path) %>% 
        file.path(paste0('estate_tax_detail_', yr, '.csv')) %>% 
        fread() %>% 
        tibble() %>% 
        rename(liab_estate = estate_tax_liability), 
      by = 'id'
    ) %>% 
    left_join(
      get_scenario_info(id)$interface_paths$`Estate-Tax-Distribution` %>%
        file.path(paste0('estate_tax_detail_', yr, '.csv')) %>% 
        fread() %>% 
        tibble() %>% 
        select(id, inheritance_reform = inheritance, liab_estate_reform = estate_tax_liability), 
      by = 'id'
    ) %>% 
    
    # Split records based on probability of inheritance
    expand_grid(copy_id = 1:2) %>% 
    mutate(
      weight = weight * if_else(copy_id == 1, p_inheritance, 1 - p_inheritance), 
      across(
        .cols = c(starts_with('inheritance'), starts_with('liab_estate')), 
        .fns  = ~ . * (copy_id == 1)
      ) 
    ) %>% 
    filter(weight > 0) %>% 
    select(-p_inheritance) %>% 
    
    # Join other taxes 
    left_join(other_taxes, by = 'year') %>%
    group_by(taxes_included) %>% 
    mutate(
      
      # Express counterfactual reform variables in baseline dollars to account for VAT
      across(.cols = ends_with('_reform'), .fns  = ~ . / vat_price_offset),
      
      # Add inheritance to income for estate tax-inclusive assumption scenarios
      income        = income        + inheritance        * (taxes_included %in% c('iit_pr_estate', 'iit_pr_estate_cit_vat')),
      income_reform = income_reform + inheritance_reform * (taxes_included %in% c('iit_pr_estate', 'iit_pr_estate_cit_vat')),
      
      # VAT burden is the loss of real income from higher prices. Some components
      # of expanded income will rise with prices (e.g. OASDI or capital income),
      # others won't; compositional differences determine distributional impact
      liab_vat = income - income_reform,
    
      # Allocate corporate tax changes in accordance with assumed labor incidence
      liab_other_corp_labor      = other_corp_delta    * 1e9 * other_corp_labor_share       * (labor / sum(labor * weight)),
      liab_other_corp_capital    = other_corp_delta    * 1e9 * (1 - other_corp_labor_share) * (capital / sum(capital * weight)),
      liab_cost_recovery_labor   = cost_recovery_delta * 1e9 * 0.5                          * (labor / sum(labor * weight)),
      liab_cost_recovery_capital = cost_recovery_delta * 1e9 * 0.5                          * (capital / sum(capital * weight)),
      liab_corp                  = liab_other_corp_labor + liab_other_corp_capital + liab_cost_recovery_labor + liab_cost_recovery_capital, 
      
      # Calculate liability under each scenario
      liab = case_when(
        taxes_included == 'iit_pr'                ~ liab_iit_pr,
        taxes_included == 'iit_pr_estate'         ~ liab_iit_pr + liab_estate,
        taxes_included == 'iit_pr_estate_cit_vat' ~ liab_iit_pr + liab_estate
      ), 
      liab_reform = case_when(
        taxes_included == 'iit_pr'                ~ liab_iit_pr_reform,
        taxes_included == 'iit_pr_estate'         ~ liab_iit_pr_reform + liab_estate_reform,
        taxes_included == 'iit_pr_estate_cit_vat' ~ liab_iit_pr_reform + liab_estate_reform + liab_corp + liab_vat
      ), 
      
      # Calculate change in tax liability
      liab_delta = liab_reform - liab, 
      
      # Calculate after-tax income in both scenarios
      ati        = income        - liab,
      ati_reform = income_reform - liab_reform, 
      
    ) %>% 
    
    # Add grouping variables
    arrange(income, .by_group = T) %>% 
    mutate(
      
      # Income percentile
      income_pctile = cumsum(weight * (income >= 0)) / sum(weight * (income >= 0)), 
      income_pctile = if_else(income < 0, NA, income_pctile), 
      
      # Quintiles and top shares
      quintile = case_when(
        income_pctile <= 0.2 ~ 'Quintile 1',
        income_pctile <= 0.4 ~ 'Quintile 2',
        income_pctile <= 0.6 ~ 'Quintile 3',
        income_pctile <= 0.8 ~ 'Quintile 4',
        income_pctile <= 1   ~ 'Quintile 5',
      ), 
      top_10 = if_else(income_pctile > 0.9,   'Top 10%',   NA), 
      top_5  = if_else(income_pctile > 0.95,  'Top 5%',    NA), 
      top_1  = if_else(income_pctile > 0.99,  'Top 1%',    NA), 
      top_01 = if_else(income_pctile > 0.999, 'Top 0.1%',  NA),
      
      # Age group
      age_group = case_when(
        age < 25 ~ '24 and under',
        age < 30 ~ '25 - 29',
        age < 40 ~ '30 - 39',
        age < 50 ~ '40 - 49',
        age < 65 ~ '50 - 64',
        T        ~ '65+'
      )
    ) %>% 
    ungroup() %>% 
    return()
}



calc_dist_metrics = function(grouped_microdata) {
  
  #----------------------------------------------------------------------------
  # Aggregates record-level tax change microdata into summary stats, grouped
  # by tax inclusion scenario and either income or age.
  #
  # Parameters:
  #  - grouped_microdata (df) : output of  process_for_distribution()
  #
  # Returns: tibble of distributional metrics
  #----------------------------------------------------------------------------
  
  # Calculate metrics by specified group
  grouped_microdata %>%
    summarise(
      
      # Group-metric-specific summary stats
      income_cutoff = round(min(income) / 5) * 5,
      n_tax_units   = sum(weight),
      
      # Unconditional and conditional averages
      avg       = round(weighted.mean(liab_delta, weight) / 5) * 5,
      avg_cut   = round(weighted.mean(liab_delta, (weight * (liab_delta <= -5))) / 5) * 5,
      avg_raise = round(weighted.mean(liab_delta, (weight * (liab_delta >= 5)))  / 5) * 5,
      
      # Counts
      share_cut   = sum(weight * (liab_delta <= -5)) / sum(weight),
      share_raise = sum(weight * (liab_delta >= 5))  / sum(weight),
      
      # Relative changes
      pct_chg_ati = sum(ati_reform * weight) / sum(ati * weight) - 1, 
      
      # Income group's total dollar amount tax change
      net_change = sum(round(liab_delta) * weight) / 1e9,
      
      .groups = 'drop_last'
    ) %>%
    
    # Group's share of total change
    mutate(share_net_change = net_change / sum(net_change)) %>%
    ungroup() %>%
    return()
}



get_other_taxes = function(id, baseline_id) {
  
  #----------------------------------------------------------------------------
  # Gets time series of aggregate effects for VAT changes, corporate tax rate
  # changes, and changes to cost recovery rules.
  #
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  #
  # Returns: list of three dataframes: VAT price effecvt, corporate rate 
  #.         delta, and cost recovery delta (lst).
  #----------------------------------------------------------------------------
  
  # Get scenario info for counterfactual scenario
  scenario_info = get_scenario_info(id)
  first_year    = min(scenario_info$years)
  last_year     = max(scenario_info$years)
  
  
  #-----------------
  # Value added tax
  #-----------------
  
  # Read VAT price offset for deflating other taxes
  vat_price_offset = globals$output_root %>%
    file.path(id, '/static/supplemental/vat_price_offset.csv') %>%
    read_csv(show_col_types = F) %>% 
    select(year, vat_price_offset = cpi_factor)
  
  
  #-----------------------
  # Cost recovery changes
  #-----------------------
  
  # Get corporate tax rate by year
  corp_rate = file.path(globals$output_root, id, 'static/supplemental/tax_law.csv') %>%
    read_csv(show_col_types = F) %>%
    distinct(year, corp.rate)
  
  # Read recovery ratios by legal form
  cost_recovery_delta = globals$interface_paths %>%
    filter(ID == globals$interface_path$ID[1], interface == 'Cost-Recovery-Simulator') %>%
    pull(path) %>% 
    file.path('totals/recovery_ratios_form.csv') %>%
    read_csv(show_col_types = F) %>%
    mutate(policy = 'baseline') %>%
    bind_rows(
      scenario_info$interface_paths$`Cost-Recovery-Simulator` %>% 
        file.path('totals/recovery_ratios_form.csv') %>%
        read_csv(show_col_types = F) %>%
        mutate(policy = 'scenario')
    ) %>%
    filter(year >= first_year, year <= last_year) %>% 
    pivot_wider(
      names_from  = c(policy, form), 
      values_from = c(investment, real, pv)
    ) %>% 
    
    # Calculate implied long-run revenue loss
    left_join(corp_rate, by = 'year') %>% 
    mutate(
      cost_recovery_delta = investment_baseline_ccorp * (pv_scenario_ccorp - pv_baseline_ccorp) * -corp.rate
    ) %>% 
    select(year, cost_recovery_delta)
  
  
  #-----------------------------
  # Other corporate tax changes
  #-----------------------------

  # Read baseline off-model revenue deltas (0 if actual baseline)
  other_corp_delta = globals$interface_paths %>%
    filter(ID == baseline_id, interface == 'Off-Model-Estimates') %>%
    pull(path) %>% 
    file.path('revenues.csv') %>%
    read_csv(show_col_types = F) %>%
    select(year, baseline = corporate) %>% 
    filter(year >= first_year, year <= last_year) %>% 
    
    # Read counterfactual scenario off-model revenues
    left_join(
      scenario_info$interface_paths$`Off-Model-Estimates` %>% 
        file.path('revenues.csv') %>%
        read_csv(show_col_types = F) %>%
        select(year, reform = corporate), 
      by = 'year'
    ) %>% 
    
    # Express corporate tax in baseline (consumer) dollars
    left_join(vat_price_offset, by = 'year') %>%
      mutate(
        reform           = reform / vat_price_offset, 
        other_corp_delta = reform - baseline
      ) %>%
      select(-ends_with('_factor')) %>% 
      
      # Determine first year of policy reform, if any, and allocate labor
      # share of changed corporate burden over time
      mutate(
        first_year = ifelse(
          sum(other_corp_delta) != 0,
          min(year[cumsum(other_corp_delta) != 0 & lag(other_corp_delta, default = 0) == 0]),
          Inf
        ),
        other_corp_labor_share =  0.2 * pmax(0, pmin(1, (year - first_year) / 10))) %>%
      select(year, other_corp_delta, other_corp_labor_share)
    
  # Combine and return
  vat_price_offset %>% 
    left_join(cost_recovery_delta, by = 'year') %>% 
    left_join(other_corp_delta,    by = 'year') %>% 
    return()
}


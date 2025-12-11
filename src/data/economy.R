#---------------------------------------------------------
# economy.R 
# 
# Contains helper functions which either read and process  
# economic data or perform economic modeling operations
#---------------------------------------------------------


generate_indexes = function(macro_root, vat_price_offset, excess_growth_offset) {
  
  #----------------------------------------------------------------------------
  # Gets growth rates, both historical and projected for this economic 
  # scenario, for indexation variables. Currently limited to CPIU, Chained
  # CPIU, and Average Wage Index.
  # 
  # Parameters:
  #   - macro_root (str)          : path for Macro-Projections scenario 
  #                                 interface
  #   - vat_price_offset (df)     : series of price level adjustment factors to 
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess 
  #                                 real GDP growth scenario
  #
  # Returns: tibble of growth rates by series (df). 
  #----------------------------------------------------------------------------
  
  
  # Read and combine historical and projected macro data
  c('historical.csv', 'projections.csv') %>% 
    map(.f = ~ macro_root %>% 
          file.path(.x) %>% 
          read_csv(show_col_types = F)) %>% 
    bind_rows() %>% 
    
    # Select indexation variables and reshape long
    select(year, cpi = cpiu_irs, chained_cpi = ccpiu_irs, awi) %>% 
    pivot_longer(cols      = -year, 
                 names_to  = 'series', 
                 values_to = 'value') %>% 
    
    # Adjust price level for VAT/excess growth
    left_join(vat_price_offset, by = 'year') %>% 
    mutate(value = if_else(series %in% c('cpi', 'chained_cpi'), 
                           value * replace_na(cpi_factor, 1), 
                           value)) %>% 
    left_join(excess_growth_offset, by = 'year') %>% 
    mutate(value = if_else(series == 'awi', 
                           value * income_factor, 
                           value)) %>%   
    select(-cpi_factor, -gdp_deflator_factor, -income_factor) %>% 
    
    # Express in growth rates
    group_by(series) %>% 
    mutate(growth = value / lag(value) - 1) %>% 
    ungroup() %>% 
    select(series, year, growth) %>% 
    arrange(series, year) %>% 
    return()
}



get_vat_price_offset = function(macro_root, vat_root, years) {

  #----------------------------------------------------------------------------
  # Calculates the amount by which prices rise in response to the intro-
  # duction of a VAT. 
  # 
  # Parameters:
  #   - macro_root (str) : path for Macro-Projections scenario interface
  #   - vat_root (str)   : path for Value-Added-Tax-Model scenario interface
  #   - years (int[])    : years for which to run simulation
  #
  # Returns: tibble of price level adjustment factors over time (df). 
  #----------------------------------------------------------------------------
  
  # Read projected macro aggregates
  macro = macro_root %>% 
    file.path('projections.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, gdp, c = gdp_c)
  
  # Read VAT revenues under counterfactual scenario. If a VAT ever becomes law 
  # (lol) or if we add other indirect taxes to the model then this bit needs 
  # to be expressed as difference from baseline
  vat = vat_root %>% 
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, vat = receipts_fy)
  
  # Calculate price level multiplier as revenue over baseline consumption
  tibble(year = years) %>% 
    left_join(macro, by = 'year') %>% 
    left_join(vat, by = 'year') %>% 
    mutate(cpi_factor          = replace_na(1 + vat / c, 1), 
           gdp_deflator_factor = replace_na(1 + vat / gdp, 1)) %>% 
    select(year, cpi_factor, gdp_deflator_factor) %>% 
    return()
}



do_ss_cola = function(tax_units, yr, vat_price_offset) {
  
  #----------------------------------------------------------------------------
  # Adjusts Social Security benefits for price increased caused by the 
  # introduction of a VAT. (Note: the imputations here should at some point be
  # folded into Tax-Data.)
  # 
  # Parameters:
  #   - tax_units (df)        : tibble of tax units
  #   - yr (int)              : simulation year 
  #   - vat_price_offset (df) : series of price level adjustment factors to 
  #                             reflect introduction of a VAT
  # 
  # Returns: tax units tibble with updated values for gross_ss (df). 
  #----------------------------------------------------------------------------

  # Get relevant Social Security information
  ss = tax_units %>% 
    filter(gross_ss > 0) %>% 
    mutate(
      
      # Young ages indicate disability
      di   = if_else(age1 < 62 & (is.na(age2) | age2 < 62), gross_ss, 0),
      oasi = gross_ss - di, 
      
      # For married couples, perform retirement year calculation based on 
      # older earner. Ages are top-coded at 80 so impute above that using 
      # exponential distribution. Assume retirement at age 62. Pretty rough. 
      age = pmax(age1, replace_na(age2, -1)),
      age = if_else(age == 80, 80 + r.oasdi_exp, age),  
      claiming_year = yr - (age - 62),
      
      # For DI recipients, impute claiming year as exponential distribution
      claiming_year = if_else(di > 0, yr - r.oasdi_exp - 1, claiming_year)
      
    ) %>% 
    select(id, claiming_year, di, oasi)
  
  
  # Determine cumulative inflation in excess of baseline by retirement year
  colas = ss %>% 
    filter(claiming_year < yr) %>% 
    distinct(claiming_year) %>% 
    arrange(claiming_year) %>% 
    
    # Create time series by benefit claiming year
    expand_grid(year = min(ss$claiming_year):(yr - 1)) %>% 
    filter(year >= claiming_year) %>% 
    
    # Add VAT and calculate cumulative excess inflation since retirement 
    left_join(vat_price_offset %>% 
                mutate(excess_inflation = cpi_factor / lag(cpi_factor, default = 1) - 1) %>% 
                select(year, excess_inflation), 
              by = 'year') %>% 
    group_by(claiming_year) %>% 
    mutate(cola = cumprod(1 + replace_na(excess_inflation, 0))) %>% 
    ungroup() %>% 
    filter(year == yr - 1) %>% 
    select(claiming_year, cola)
  
  
  # Join and overwrite COLA-adjusted benefits 
  tax_units %>% 
    left_join(
      ss %>%  
        left_join(colas, by = 'claiming_year') %>% 
        mutate(new_gross_ss = (di + oasi) * cola) %>%
        select(id, new_gross_ss), 
      by = 'id'
    ) %>% 
    mutate(gross_ss = if_else(is.na(new_gross_ss), gross_ss, new_gross_ss)) %>% 
    select(-new_gross_ss) %>% 
    return()
} 



do_capital_adjustment = function(tax_units, yr, vat_price_offset) {
  
  #----------------------------------------------------------------------------
  # Adjusts capital income to reflect the introduction of a VAT. The basic 
  # idea is that a VAT burdens returns to pre-enactment ("old") capital while
  # exempting the normal return to post-enactment ("new") capital. Here, 
  # because we assume prices rise in response to a VAT, we implement this
  # logic by 1) crudely imputing the share of returns that are attributable to
  # new capital 2) scaling up the normal share of those returns (assumed to be
  # 80%).
  #
  # (Note: the imputations here should at some point be folded into Tax-Data.)
  # 
  # Parameters:
  #   - tax_units (df)        : tibble of tax units
  #   - yr (int)              : simulation year 
  #   - vat_price_offset (df) : series of price level adjustment factors to 
  #                             reflect introduction of a VAT
  # 
  # Returns: tax units tibble with updated values for gross_ss (df). 
  #----------------------------------------------------------------------------
  
  # Read info on distribution of debt maturities and calculate cumulative share
  # of debt matured after (tenor) years
  new_debt = read_csv('./resources/debt_maturities.csv', show_col_types = F) %>% 
    mutate(share_new_debt = cumsum(share)) %>% 
    select(-share)
  
  # Assuming an economic depreciation rate of 5.7% (NIPA average over 2015-2022
  # for private fixed assets), construct series tracking the share of returns 
  # attributable to new capital (i.e. one minus cumulative depreciation)
  new_capital = tibble(year = 1:100) %>% 
    mutate(share_new_capital = 1 - round((1 - 0.057) ^ year, 2)) 
  
  # Determine years when VAT changed, requiring that vintages be tracked
  vat_change_years = vat_price_offset %>% 
    mutate(excess_inflation = cpi_factor / lag(cpi_factor, default = 1) - 1) %>% 
    select(source_year = year, excess_inflation) %>% 
    filter(excess_inflation != 0, source_year < yr)
  
  # Skip if no VAT-driven changes in prices
  if (nrow(vat_change_years) == 0) {
    return(tax_units)
  }
  
  
  # Calculate capital income adjustment factors
  adjustment_factors = vat_change_years %>% 
    
    # Create vintaging series for each source year of price changes
    expand_grid(year = min(vat_change_years$source_year):yr) %>% 
    mutate(t = year - source_year) %>% 
    filter(t > 0) %>% 
    
    # Join schedules for new debt and capital
    left_join(new_debt, by = c('t' = 'tenor')) %>% 
    mutate(share_new_debt = replace_na(share_new_debt, 1)) %>% 
    left_join(new_capital, by = c('t' = 'year')) %>% 
    
    # Calculate vintage-specific adjustment factors (i.e. share of income to 
    # be scaled up to reflect renegotiated returns after VAT, or in other words
    # the share of returns attributable to post-enactment investment) by source 
    # year and year, scaling by normal share of total return in the case of capital
    # (50% assumption is from Auerbach via Toder)
    mutate(debt_factor    = 1 + excess_inflation * share_new_debt, 
           capital_factor = 1 + excess_inflation * share_new_capital * 0.5) %>% 
    
    # Aggregate effects by year
    group_by(year) %>% 
    summarise(debt_factor    = prod(debt_factor), 
              capital_factor = prod(capital_factor)) %>% 
    
    # Subset to this specific year
    filter(year == yr) 
    
  
  # Apply adjustment and return
  tax_units %>% 
    mutate(
      
      # Debt
      across(
        .cols = c(txbl_int, exempt_int, first_mort_int, second_mort_int, inv_int_exp), 
        .fns  = ~ . * adjustment_factors$debt_factor
      ), 
      
      # Equity 
      across(
        .cols = c(div_ord, div_pref, kg_st, kg_lt, kg_1250, kg_collect),  
        .fns  = ~ . * adjustment_factors$capital_factor
      ), 
      
      # Mixed income (assumes 20% of pass-through business is the return to capital)
      across(
        .cols = c(sole_prop, part_active, part_passive, part_active_loss, 
                  part_passive_loss, part_179, scorp_active, scorp_passive, 
                  scorp_active_loss, scorp_passive_loss, scorp_179, farm),
        .fns  = ~ . * (1 + (adjustment_factors$capital_factor - 1) * 0.2) 
      )
    ) %>% 
    return()
}



get_excess_growth_offset = function(excess_growth, start_year, years) {
  
  #----------------------------------------------------------------------------
  # Calculates the amount by which the level of real GDP exceeds its 
  # baseline value in relative terms, given an excess growth rate and a start
  # year. 
  # 
  # Parameters:
  #   - excess_growth (dbl) : annual real growth rate in excess of baseline 
  #   - start_year (int)    : year when excess growth starts
  #   - years (int[])       : years for which to run simulation
  #
  # Returns: tibble of income adjustment factors over time (df). 
  #----------------------------------------------------------------------------
  
  tibble(year = years) %>% 
    mutate(income_factor = cumprod(1 + if_else(year >= start_year, excess_growth, 0))) %>% 
    return()
}



do_excess_growth = function(tax_units, scenario_info, excess_growth_offset) {
  
  #----------------------------------------------------------------------------
  # Adjusts intensive-margin variables for excess real GDP growth
  # 
  # Parameters:
  #   - tax_units (df)             : tibble of tax units
  #   - scenario_info (list)       : scenario info object; see get_scenario_info()
  #   - excess_growth_offset (dbl) : income adjustment factors reflecting  
  #                                  excess real GDP growth scenario
  # 
  # Returns: tax units tibble with updated values for intensive-margin 
  #          variables (df). 
  #----------------------------------------------------------------------------
  
  # Read info on variables and the growth factors used in projecting Tax-Data
  variable_guide = scenario_info$interface_paths$`Tax-Data` %>% 
    file.path('./variable_guide.csv') %>% 
    read_csv(show_col_types = F)
  
  gdp_vars = variable_guide %>% 
    filter(
      (variable %in% colnames(tax_units)) & 
        !is.na(grow_with) & 
        grow_with != 'ss' & 
        grow_with != 'pensions'
    ) %>%
    select(variable) %>% 
    deframe()
  
  oasdi_vars = variable_guide %>% 
    filter(
      (variable %in% colnames(tax_units)) & 
        (grow_with == 'ss')
    ) %>%
    select(variable) %>% 
    deframe()
  
  pension_vars = variable_guide %>% 
    filter(
      (variable %in% colnames(tax_units)) & 
        (grow_with == 'pensions')
    ) %>%
    select(variable) %>% 
    deframe()
  
  # Adjust variables
  tax_units %>%
    left_join(excess_growth_offset, by = 'year') %>% 
    mutate(
      
      # For Social Security and pensions, getting cumulative income factor as of
      # age 60 for the record (mimicking AIME computation) for all records 60 and
      # over as of current year. If less than 60: 
      # -- For Social Security, we assume this is SSDI income and that the claiming
      #    year was 3 years prior (currently a placeholder for average vintage of current
      #    SSDI claims).
      # -- For pensions, we make no adjustments.
      age_avg = ceil(ifelse(!is.na(age2),((age1 + age2)/2), age1)), 
      wedge_factor_oasdi = case_when(
        age_avg < 60  ~ (1 + scenario_info$excess_growth)^(-pmin(year - scenario_info$excess_growth_start_year, 3)),
        age_avg >= 60 ~ (1 + scenario_info$excess_growth)^(60 - age_avg),
        TRUE ~ 1
      ),
      wedge_factor_pension = case_when(
        age_avg < 60  ~ 1,
        age_avg >= 60 ~ (1 + scenario_info$excess_growth)^(60 - age_avg),
        TRUE ~ 1
      ),      
      
      # For OASDI and pension growth variables, multiply by wedge factor and adjustment factor
      across(
        .cols = all_of(oasdi_vars), 
        .fns  = ~ . * pmax(wedge_factor_oasdi * income_factor,1)
      ),   
      across(
        .cols = all_of(pension_vars), 
        .fns  = ~ . * pmax(wedge_factor_pension * income_factor,1)
      ),        
      
      # For GDP growth variables, multiply by income adjustment factor
      across(
        .cols = all_of(gdp_vars), 
        .fns  = ~ . * income_factor
      )
      
    ) %>%
    select(-age_avg, -wedge_factor_oasdi, -wedge_factor_pension, -income_factor) %>%
    return()
}



read_microdata = function(root, year) {

  #----------------------------------------------------------------------------
  # Loads tax microdata into memory for a given scenario-year
  #
  # Parameters:
  #   - root (str) : filepath to scenario's microdata vintage
  #   - year (int) : year of microdata
  #
  # Returns: tibble of tax microdata (df).
  #----------------------------------------------------------------------------

  root %>%
    file.path(paste0('tax_units_', year, '.csv')) %>%
    fread() %>%
    tibble() %>%
    return()
}



duplicate_with_wage_percentiles = function(tax_units) {

  #----------------------------------------------------------------------------
  # Duplicates each tax unit record with wage percentile values for wages1.
  # Creates 111 versions of each record: 1 original + 110 percentile copies.
  #
  # Percentile values are:
  #   - $0 (literal zero)
  #   - p1 through p99 (calculated from positive wages1 only)
  #   - p99.1 through p99.9 (top percentile detail)
  #   - p100 (maximum wage)
  #
  # Parameters:
  #   - tax_units (df) : tibble of tax units with wages1, wages2, wages columns
  #
 # Returns: tibble with 111x the original number of rows. New columns:
  #   - pctl_label (dbl)      : percentile label (NA for original, 0-100 for copies)
  #   - wages1_original (dbl) : original wages1 value before replacement
  #   - id (chr)              : modified to include percentile suffix for copies
  #----------------------------------------------------------------------------

  # Get wages1 values for positive earners only (for percentile calculation)
  positive_wages = tax_units %>%
    filter(wages1 > 0) %>%
    pull(wages1)

  # Define percentile points: 0, 1-99, 99.1-99.9, 100
  percentile_points = c(
    0,                     # $0 literal
    1:99,                  # p1 through p99
    seq(99.1, 99.9, 0.1),  # p99.1 through p99.9
    100                    # p100
  )

  # Calculate percentile values (using positive wages only for p1-p100)
  percentile_values = c(
    0,  # $0 literal
    quantile(positive_wages, probs = (1:99) / 100, names = FALSE),
    quantile(positive_wages, probs = seq(99.1, 99.9, 0.1) / 100, names = FALSE),
    quantile(positive_wages, probs = 1, names = FALSE)  # p100
  )

  # Create lookup tibble for percentile values
  pctl_lookup = tibble(
    pctl_idx   = 1:110,
    pctl_label = percentile_points,
    pctl_value = percentile_values
  )

  # Store original wages1 for reference
  tax_units = tax_units %>%
    mutate(wages1_original = wages1)

  # Create 110 copies per record with percentile wages1 values
  expanded = tax_units %>%
    crossing(pctl_idx = 1:110) %>%
    left_join(pctl_lookup, by = 'pctl_idx') %>%
    mutate(
      wages1 = pctl_value,
      wages  = wages1 + wages2,  # Update total wages
      id     = paste0(id, '_p', pctl_label)  # Make IDs unique
    ) %>%
    select(-pctl_idx, -pctl_value)

  # Mark original records with NA percentile label (already have wages1_original)
  originals = tax_units %>%
    mutate(pctl_label = NA_real_)

  # Combine original records with expanded copies
  bind_rows(originals, expanded) %>%
    return()
}


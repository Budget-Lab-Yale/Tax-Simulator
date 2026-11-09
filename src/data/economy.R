#---------------------------------------------------------
# economy.R 
# 
# Contains helper functions which either read and process  
# economic data or perform economic modeling operations
#---------------------------------------------------------


generate_indexes = function(macro_root, vat_price_offset) {
  
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
  #
  # Returns: tibble of growth rates by series (df). 
  #----------------------------------------------------------------------------
  
  
  # Read pre-1970 CPI-U annual averages (BLS, 1982-84=100) and compute growth
  pre1970_cpi = read_csv('./resources/cpiu_historical.csv', show_col_types = F) %>%
    arrange(year) %>%
    mutate(growth = cpiu / lag(cpiu) - 1) %>%
    filter(!is.na(growth)) %>%
    select(year, growth) %>%
    mutate(series = 'cpi')

  # Read and combine historical and projected macro data
  macro = read_macro_spliced(macro_root) %>%

    # Select indexation variables and reshape long
    select(year, cpi = cpiu_irs, chained_cpi = ccpiu_irs, awi) %>%
    pivot_longer(cols      = -year,
                 names_to  = 'series',
                 values_to = 'value') %>% 
    
    # Adjust price level for VAT
    left_join(vat_price_offset, by = 'year') %>%
    mutate(value = if_else(series %in% c('cpi', 'chained_cpi'),
                           value * replace_na(cpi_factor, 1),
                           value)) %>%
    select(-cpi_factor, -gdp_deflator_factor) %>%
    
    # Express in growth rates
    group_by(series) %>% 
    mutate(growth = value / lag(value) - 1) %>% 
    ungroup() %>% 
    select(series, year, growth) %>% 
    arrange(series, year)

  # Prepend historical CPI growth rates, used for capital gains basis indexation.
  # The historical file also supplies the growth rate at the splice: the macro
  # series' first year, 1970, has no prior-year level to chain from and so no
  # growth rate, and without the historical row that year's 5.7% inflation drops
  # out of any chain spanning the splice. Keep a historical row for every year the
  # macro series lacks a valid growth rate, and drop the macro rows they replace so
  # that series and year stay unique.
  cpi_splice = pre1970_cpi %>%
    anti_join(macro %>% filter(series == 'cpi', !is.na(growth)), by = 'year')
  macro %>%
    filter(!(series == 'cpi' & year %in% cpi_splice$year)) %>%
    bind_rows(cpi_splice) %>%
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



vat_excess_inflation = function(vat_price_offset) {

  #----------------------------------------------------------------------------
  # Computes year-over-year price growth attributable to the VAT price offset.
  #
  # Parameters:
  #   - vat_price_offset (df) : series of price level adjustment factors; see
  #                             get_vat_price_offset()
  #
  # Returns: tibble of excess inflation rates by year (df).
  #----------------------------------------------------------------------------

  vat_price_offset %>%
    mutate(excess_inflation = cpi_factor / lag(cpi_factor, default = 1) - 1) %>%
    select(year, excess_inflation) %>%
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
    left_join(vat_excess_inflation(vat_price_offset), by = 'year') %>%
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
  vat_change_years = vat_excess_inflation(vat_price_offset) %>%
    rename(source_year = year) %>%
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



calc_kg_cpi_ratio = function(tax_units, indexes, year) {

  #----------------------------------------------------------------------------
  # Computes the CPI ratio between current year and purchase year for each tax
  # unit's long-term capital gains. Uses blended CPI series (CPI-U pre-2017,
  # chained CPI-U 2017+), consistent with tax parameter indexation convention.
  # Pure data plumbing -- no policy gating.
  #
  # Parameters:
  #   - tax_units (df) : tibble of tax units
  #   - indexes (df)   : tibble of growth rates; see generate_indexes()
  #   - year (int)     : simulation year
  #
  # Returns: tax units tibble with new column kg_lt_cpi_ratio (df).
  #----------------------------------------------------------------------------

  # Validate required columns
  if (!('kg_lt_years_held' %in% colnames(tax_units))) {
    stop('calc_kg_cpi_ratio: kg_lt_years_held column missing from tax_units')
  }
  if (!('kg_lt_basis' %in% colnames(tax_units))) {
    stop('calc_kg_cpi_ratio: kg_lt_basis column missing from tax_units')
  }

  # Check data integrity: nonzero kg_lt must have non-NA years held
  if (any(tax_units$kg_lt != 0 & is.na(tax_units$kg_lt_years_held), na.rm = TRUE)) {
    stop('calc_kg_cpi_ratio: kg_lt != 0 but kg_lt_years_held is NA for some records')
  }

  # Build blended CPI level series (CPI growth pre-2017, chained CPI growth 2017+)
  cpi_levels = bind_rows(
    indexes %>% filter(series == 'cpi', year < 2017),
    indexes %>% filter(series == 'chained_cpi', year >= 2017)
  ) %>%
    arrange(year) %>%
    mutate(level = cumprod(1 + replace_na(growth, 0))) %>%
    select(year, level)

  # Validate current year exists in CPI data
  if (!(year %in% cpi_levels$year)) {
    stop(paste0('calc_kg_cpi_ratio: year ', year, ' not found in CPI data'))
  }

  # Compute ratio for each tax unit, clamping purchase years to CPI range
  min_cpi_year  = min(cpi_levels$year)
  current_level = cpi_levels$level[cpi_levels$year == year]

  tax_units %>%
    mutate(.purchase_year = pmax(as.integer(year - round(kg_lt_years_held)), min_cpi_year)) %>%
    left_join(cpi_levels %>% rename(.purchase_level = level),
              by = c('.purchase_year' = 'year')) %>%
    mutate(
      kg_lt_cpi_ratio = if_else(
        kg_lt == 0 | is.na(kg_lt_years_held) | kg_lt_years_held == 0,
        1,
        current_level / .purchase_level
      )
    ) %>%
    select(-.purchase_year, -.purchase_level) %>%
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


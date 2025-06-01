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
  #   - macro_root (str)      : path for Macro-Projections scenario interface
  #   - vat_price_offset (df) : series of price level adjustment factors to 
  #                             reflect introduction of a VAT
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
    mutate(value = if_else(series == 'awi', 
                           value *  (1 + excess_growth), 
                           value)) %>%   
    select(-cpi_factor, -gdp_deflator_factor) %>% 
    
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
  #   - yr (str)              : simulation year 
  #   - vat_price_offset (df) : series of price level adjustment factors to 
  #                             reflect introduction of a VAT
  # 
  # Returns: tax units tibble with updated values for gross_ss (df). 
  #----------------------------------------------------------------------------

  # Set random seed
  set.seed(globals$random_seed)
  
  
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
      age = if_else(age == 80, 80 + round(rexp(nrow(.), 1 / 4)), age),  
      claiming_year = yr - (age - 62),
      
      # For DI recipients, impute claiming year as exponential distribution
      claiming_year = if_else(di > 0, yr - round(rexp(nrow(.), 1 / 4)) - 1, claiming_year)
      
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
  #   - yr (str)              : simulation year 
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


do_excess_growth = function(tax_units, yr, excess_growth) {
  
  #----------------------------------------------------------------------------
  # Adjusts intensive-margin variables for excess real GDP growth
  # 
  # Parameters:
  #   - tax_units (df)        : tibble of tax units
  #   - yr (str)              : simulation year 
  #   - excess_growth (float) : factor for increased GDP growth for macroeconomic
  #                             scenarios
  # 
  # Returns: tax units tibble with updated values for intensive-margin variables (df). 
  #----------------------------------------------------------------------------
  
  # Read info on variables and the growth factors used in projecting Tax-Data
  variable_guide = read_csv('./config/variable_guide/baseline.csv')
  
  # Get list of variables that grow by GDP (or one of its components) and which grow by OASDI outlays
  gdp_vars = variable_guide %>% 
              filter(-is.na(grow_with) & grow_with!='ss' & grow_with!='pensions') %>%
                select(variable) %>% 
                  deframe() 
  oasdi_vars = variable_guide %>% 
                filter(grow_with=='ss' | grow_with=='pensions') %>%
                  select(variable) %>% 
                    deframe()

  # For pensions/Social Security, need to get average age of tax unit to compute wedge factor
  tax_units %>% 
    mutate(age_avg = ceil(ifelse(!is.na(age2),((age1 + age2)/2), age1))) %>%
      mutate(wedge_factor = case_when(
                              avg_age < 62 ~ 1,
                              avg_age >= 62 ~ FINISH THIS
      ) )
  
  # Adjust variables
  tax_units %>%
    mutate(
      # For GDP growth variables, multiply by 1 + excess growth
      across(
        .cols = gdp_vars, 
        .fns  = ~ . * (1 + excess_growth)
      ),
      # For OASDI growth variables, multiply by wedge factor and 1 + excess_growth
      across(
        .cols = oasdi_vars, 
        .fns  = ~ . * wedge_factor * (1 + excess_growth)
      ),     
      
    ) %>%
      select(-age) %>%
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



initialize_nols = function(years) {
  
  #----------------------------------------------------------------------------
  # Initializes table to track policy-generated NOLs. 
  # 
  # Parameters:
  #   - years (int) : years for which to track generation of new NOLs.
  #
  # Returns: tibble with NOLs initialized to 0. (df).
  #----------------------------------------------------------------------------
  
  
  # Initialize table
  expand_grid(year_generated = years, 
              t              = 1:10) %>%
    mutate(year_claimed = year_generated + t) %>% 
    
    # Set distribution of claiming, 
    # from this paper https://www.journals.uchicago.edu/doi/epdf/10.17310/ntj.2018.4.04
    mutate(
      share_claimed = case_when(
        t == 1  ~ 0.26, 
        t == 2  ~ 0.16,
        t == 3  ~ 0.10, 
        t == 4  ~ 0.08, 
        t == 5  ~ 0.08,
        t == 6  ~ 0.06,
        t == 7  ~ 0.04,
        t == 8  ~ 0.04,
        t == 9  ~ 0.02, 
        t == 10 ~ 0.02
      ), 
      amount_claimed = 0
    ) %>% 
    select(-t) %>%
    return()
}



update_nols = function(nols, year, amount) {
  
  #----------------------------------------------------------------------------
  # Given an amount of new NOLs generated in a specific year, allocates NOLs
  # by year eventually claimed.
  # 
  # Parameters:
  #   - nols (df)    : NOLs table as per initialize_nols()
  #   - year (int)   : year new NOLs were generated
  #   - amount (dbl) : amount of NOLs generate in year
  #
  # Returns: NOLs table with updated values in amount_claimed column (df).
  #----------------------------------------------------------------------------
  
  
  nols %>% 
    left_join(
      tibble(year_generated = year, 
             amount         = amount), 
      by = 'year_generated') %>% 
    mutate(amount_claimed = amount_claimed + replace_na(amount, 0) * share_claimed) %>% 
    select(-amount) %>% 
    return()
}



distribute_nols = function(tax_units, nols, year) { 

  #----------------------------------------------------------------------------
  # For a given claiming year, allocates new NOLs to tax units in proportion 
  # to positive pass-through income. 
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax unit data
  #   - nols (df)      : NOLs table as per initialize_nols() and update_nols()
  #   - year (int)     : claiming year 
  #
  # Returns: updated tax units tibble (df).
  #----------------------------------------------------------------------------
  
  
  # Get total amount claimed in specified year
  amount_claimed = nols %>% 
    filter(year_claimed == year) %>%
    summarise(amount_claimed = sum(amount_claimed) * 1e9) %>% 
    get_vector('amount_claimed')
  
  # Distribution NOLs in proportion to positive pass-through income
  tax_units %>% 
    mutate(positive_pt = pmax(0, sole_prop + part_active + part_passive - 
                                 part_active_loss - part_passive_loss - 
                                 part_179 + scorp_active + scorp_passive - 
                                 scorp_active_loss - scorp_passive_loss - 
                                 scorp_179),
           new_nols    = amount_claimed * (positive_pt / sum(positive_pt * weight))) %>% 
    select(-positive_pt) %>% 
    return()
}



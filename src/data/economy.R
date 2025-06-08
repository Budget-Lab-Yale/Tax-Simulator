#---------------------------------------------------------
# economy.R 
# 
# Contains helper functions which either read and process  
# economic data or perform economic modeling operations
#---------------------------------------------------------


generate_indexes = function(macro_root, macro_offsets) {
  
  #----------------------------------------------------------------------------
  # Gets growth rates, both historical and projected for this economic 
  # scenario, for indexation variables. Currently limited to CPIU, Chained
  # CPIU, and Average Wage Index.
  # 
  # Parameters:
  #   - macro_root    (str) : path for Macro-Projections scenario interface
  #   - macro_offsets (lst) : macro_offsets object, see get_macro_offsets()
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
    
    # Adjust price level for VAT and tariffs
    left_join(
      macro_offsets$vat %>% 
        select(year, vat_factor = cpi_factor), 
      by = 'year'
    ) %>%
    left_join(
      macro_offsets$tariffs %>% 
        select(year, tariff_factor = overall), 
      by = 'year'
    ) %>% 
    mutate(value = if_else(series %in% c('cpi', 'chained_cpi'), 
                           value * replace_na(vat_factor * tariff_factor, 1), 
                           value)) %>% 
    
    # Adjust wage index for excess growth
    left_join(macro_offsets$excess_growth, by = 'year') %>% 
    mutate(value = if_else(series == 'awi', 
                           value * income_factor, 
                           value)) %>%   
    select(-vat_factor, -tariff_factor, -income_factor) %>% 
    
    # Express in growth rates
    group_by(series) %>% 
    mutate(growth = value / lag(value) - 1) %>% 
    ungroup() %>% 
    select(series, year, growth) %>% 
    arrange(series, year) %>% 
    return()
}



get_macro_offsets = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Calculates and compiles different factors used to adjust the baseline
  # macroeconomic forecast, reflecting either policy changes (VATs or tariffs, 
  # which we asssume affects the price level through) or alternative economic
  # scenarios (additional above-trend productivity growth)
  # 
  # Parameters:
  #   - scenario_info (list) : scenario_info object; see get_scenario_info()
  #
  # Returns: list of three dataframes:
  #   - vat           (df) : VAT adjustment factors by year
  #   - tariffs       (df) : tariff adjustment factors by year
  #   - excess_growth (df) : excess growth adjustment factors by year
  #----------------------------------------------------------------------------

  #-----
  # VAT 
  #-----
  
  # Read projected macro aggregates
  macro = scenario_info$interface_paths$`Macro-Projections`  %>% 
    file.path('projections.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, gdp, c = gdp_c)
  
  # Read VAT revenues under counterfactual scenario. If a VAT ever becomes law 
  # (lol) or if we add other indirect taxes to the model then this bit needs 
  # to be expressed as difference from baseline
  vat = scenario_info$interface_paths$`Value-Added-Tax-Model` %>% 
    file.path('revenues.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, vat = receipts_fy)
  
  # Calculate price level multiplier as revenue over baseline consumption
  vat = tibble(year = scenario_info$years) %>% 
    left_join(macro, by = 'year') %>% 
    left_join(vat, by = 'year') %>% 
    mutate(cpi_factor          = replace_na(1 + vat / c, 1), 
           gdp_deflator_factor = replace_na(1 + vat / gdp, 1)) %>% 
    select(year, cpi_factor, gdp_deflator_factor)
  
  
  #---------
  # Tariffs
  #---------
  
  # Read price level adjustment factors and limit to simulation years
  tariffs = scenario_info$interface_paths$`Tariff-Model` %>% 
    file.path('price_effects.csv') %>% 
    read_csv(show_col_types = F) %>% 
    filter(year %in% scenario_info$years)
  
  
  #---------------
  # Excess growth
  #---------------
  
  # Calculate amount by which the level of real GDP exceeds its baseline value in relative terms
  excess_growth = tibble(year = scenario_info$years) %>% 
    mutate(income_factor = cumprod(1 + if_else(year >= scenario_info$excess_growth_start_year, 
                                               scenario_info$excess_growth, 
                                               0)))
  
  # Store as list and return
  return(
    list(
      vat           = vat, 
      tariffs       = tariffs, 
      excess_growth = excess_growth  
    )
  )
  
}





do_ss_cola = function(tax_units, yr, macro_offsets) {
  
  #----------------------------------------------------------------------------
  # Adjusts Social Security benefits for price increased caused by the 
  # introduction of a VAT or new tariffs. (Note: the imputations here should 
  # at some point be folded into Tax-Data.)
  # 
  # Parameters:
  #   - tax_units (df)      : tibble of tax units
  #   - yr (int)            : simulation year 
  #   - macro_offsets (lst) : macro_offsets object; see get_macro_offsets()
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
    left_join(
      macro_offsets$vat %>% 
        select(year, vat_factor = cpi_factor) %>% 
        left_join(
          macro_offsets$tariffs %>% 
            select(year, tariff_factor = overall), 
          by = 'year'
        ) %>% 
        mutate(
          price_level_factor = vat_factor * tariff_factor, 
          excess_inflation   = price_level_factor / lag(price_level_factor, default = 1) - 1
        ) %>% 
        select(year, excess_inflation),
      by = 'year'
    ) %>% 
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



do_capital_adjustment = function(tax_units, yr, macro_offsets) {
  
  #----------------------------------------------------------------------------
  # Adjusts capital income to reflect new indirect taxes. The basic idea for
  # a VAT is that it burdens returns to pre-enactment ("old") capital while
  # exempting the normal return to post-enactment ("new") capital. Here, 
  # because we assume prices rise in response to a VAT, we implement this
  # logic by 1) crudely imputing the share of returns that are attributable to
  # new capital 2) scaling up the normal share of those returns (assumed to be
  # 80%).
  # 
  # Tariffs are a bit different because they apply to capital goods as well. 
  # Therefore the normal return is not exempting for the component of tariff 
  # revenue attributable to imports of capital goods. We assume that 30% of 
  # the tariff base is capital goods, based on:  
  # https://x.com/ernietedeschi/status/1931373635551285657
  #
  # (Note: the imputations here should at some point be folded into Tax-Data.)
  # 
  # Parameters:
  #   - tax_units (df)      : tibble of tax units
  #   - yr (int)            : simulation year 
  #   - macro_offsets (lst) : macro_offsets object; see get_macro_offsets()
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
  vat_change_years = macro_offsets$vat %>% 
    mutate(excess_inflation = cpi_factor / lag(cpi_factor, default = 1) - 1) %>% 
    select(source_year = year, excess_inflation) %>% 
    filter(excess_inflation != 0, source_year < yr)
  
  # Same for tariffs
  tariff_change_years = macro_offsets$tariffs %>% 
    mutate(excess_inflation = overall / lag(overall, default = 1) - 1) %>% 
    select(source_year = year, excess_inflation) %>% 
    filter(excess_inflation != 0, source_year < yr)
  
  # Calculate VAT adjustment factors 
  if (nrow(vat_change_years) > 0) {
    
    # Calculate capital income adjustment factors
    vat_factors = vat_change_years %>% 
      
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
  } else {
    vat_factors = tibble(year = yr, debt_factor = 1, capital_factor = 1)
  }
  
  # Calculate tariff adjustment factors 
  if (nrow(tariff_change_years) > 0) {
    
    # Calculate capital income adjustment factors
    tariff_factors = tariff_change_years %>% 
      
      # Create vintaging series for each source year of price changes
      expand_grid(year = min(tariff_change_years$source_year):yr) %>% 
      mutate(t = year - source_year) %>% 
      filter(t > 0) %>% 
      
      # Join schedules for new debt and capital
      left_join(new_debt, by = c('t' = 'tenor')) %>% 
      mutate(share_new_debt = replace_na(share_new_debt, 1)) %>% 
      left_join(new_capital, by = c('t' = 'year')) %>% 
      
      # Calculate vintage-specific adjustment factors. See the VAT section 
      # above for an explanation. This differs from the VAT case because
      # 30% of imports are capital goods and thus the logic doesn't apply 
      mutate(debt_factor    = 1 + excess_inflation * (1 - 0.3) * share_new_debt, 
             capital_factor = 1 + excess_inflation * (1 - 0.3) * share_new_capital * 0.5) %>% 
      
      # Aggregate effects by year
      group_by(year) %>% 
      summarise(debt_factor    = prod(debt_factor), 
                capital_factor = prod(capital_factor)) %>% 
      
      # Subset to this specific year
      filter(year == yr) 
  } else {
    tariff_factors = tibble(year = yr, debt_factor = 1, capital_factor = 1)
  }
  
  
  # Apply adjustments and return
  tax_units %>% 
    mutate(
      
      # Debt
      across(
        .cols = c(txbl_int, exempt_int, first_mort_int, second_mort_int, inv_int_exp), 
        .fns  = ~ . * vat_factors$debt_factor * tariff_factors$debt_factor
      ), 
      
      # Equity 
      across(
        .cols = c(div_ord, div_pref, kg_st, kg_lt, kg_1250, kg_collect),  
        .fns  = ~ . * vat_factors$capital_factor * tariff_factors$capital_factor
      ), 
      
      # Mixed income (assumes 20% of pass-through business is the return to capital)
      across(
        .cols = c(sole_prop, part_active, part_passive, part_active_loss, 
                  part_passive_loss, part_179, scorp_active, scorp_passive, 
                  scorp_active_loss, scorp_passive_loss, scorp_179, farm),
        .fns  = ~ . * (1 + (vat_factors$capital_factor * tariff_factors$capital_factor - 1) * 0.2) 
      )
    ) %>% 
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
        .fns  = ~ . * pmax(wedge_factor_oasdi * income_factor, 1)
      ),   
      across(
        .cols = all_of(pension_vars), 
        .fns  = ~ . * pmax(wedge_factor_pension * income_factor, 1)
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


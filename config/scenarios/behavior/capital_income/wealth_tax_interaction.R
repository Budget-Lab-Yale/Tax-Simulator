do_capital_income = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts capital income variables to account for an off-model wealth tax
  # and the associated reduction in high-end net worth due mechanical 
  # decumulation (which reduces capital income one-for-one) and avoidance/
  # evasion (only a fraction of which flows through to lower reported 
  # capital income).
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #
  # Returns: tibble of tax units with post-adjustment capital income values. 
  #----------------------------------------------------------------------------
  
  #----------------
  # Set parameters
  #----------------
  
  # Wealth tax scenario ID
  wealth_tax_scenario_id = 'nickel_dime'
  
  # Set wealth tax model output root 
  wealth_tax_root = '/gpfs/gibbs/project/sarin/shared/model_data/Wealth-Tax-Simulator/v1/2025032510'
  
  # List capital income variables potentially affected by a wealth tax 
  cap_inc_vars = c('txbl_int', 'exempt_int', "div_ord", "div_pref", "kg_st", 
                   "kg_lt", "sole_prop", "farm",
                   
                   "part_active", "part_passive", "part_active_loss", 
                   "part_passive_loss", "part_179", 
                   
                   "scorp_active", "scorp_passive", "scorp_active_loss", 
                   "scorp_passive_loss", "scorp_179",
                   
                   "rent", "rent_loss",
                   
                   "estate", "estate_loss"
                   )
  
  # Income groups
  cutoffs       = c(-Inf, 1e6, 2e6, 3e6, 4e6, 5e6, 1e7, Inf) 
  cutoff_labels = c(-1, 1, 2, 3, 4, 5, 10)
  
  # Baseline income tax underreporting rate 
  baseline_evasion = 0.15 
  
  # Share of new wealth avoidance passing through to income tax
  # 13% = capital income tax underreporting rate per IRS
  # 50% = estate tax underreporting per PWBM
  # 50% = share of new wealth avoidance attributable to underrporting (and not other means)
  avoidance_pass_through = (0.13 / 0.5) * 0.5
  
  # Wealth tax policy first year
  start_year = 2026
  
  # Determine the current simulation year 
  current_year = unique(tax_units$year)

  # Skip if prior to policy start year
  if (current_year < start_year) {
    return(tax_units)
  } else {
    
    # Calculate change in reported wealth due to wealth tax
    deltas = file.path(wealth_tax_root, wealth_tax_scenario_id, 'detail', paste0(current_year, '.csv')) %>%
      
      # Read counterfactual scenario
      read_csv(show_col_types = F) %>% 
      
      # Assign income groups
      mutate(
        income_group = cut(income.conventional, cutoffs, labels = cutoff_labels, include.lowest = T)
      ) %>%
      
      # Calculate total net worth, both static (includes mechanical decumulation) and conventional (avoidance)
      group_by(income_group) %>%
      reframe(
        static = sum(weight * net_worth.static),
        conventional = sum(weight * net_worth.conventional)
      ) %>%
      
      # Do the same for baseline
      left_join(
        file.path(wealth_tax_root, 'baseline/detail', paste0(current_year, '.csv')) %>%
          read_csv(show_col_types = F) %>% 
          mutate(
            income_group = cut(income.conventional, cutoffs, labels = cutoff_labels, include.lowest = T)
          ) %>%
          group_by(income_group) %>%
          reframe(
            baseline = sum(weight * net_worth.static)
          ), by = "income_group") %>%
      
      # Calculate income group-specific reduction in reported capital income
      group_by(income_group) %>%
      mutate(
        delta_capital_income = min(0, ((avoidance_pass_through * (conventional - static) + (static - baseline)) / baseline) + baseline_evasion)
      ) 
    
    # Write deltas as supplemental output
    if(current_year == 2026) {
      deltas %>%
        select(income_group, delta_capital_income) %>%
        mutate(year = current_year) %>%
        pivot_wider(
          names_from = income_group, 
          values_from = delta_capital_income, 
          names_prefix = "Income_Group_") %>%
        write_csv(
          file.path(
            globals$output_root, 
            scenario_info$ID,
            'conventional/supplemental/capital_income_deltas.csv'))
    } else {
      read_csv(file.path(globals$output_root, scenario_info$ID,
                         'conventional/supplemental/capital_income_deltas.csv')) %>%
        bind_rows(deltas  %>%
                    select(income_group, delta_capital_income) %>%
                    mutate(year = current_year) %>%
                    pivot_wider(names_from = income_group,
                                values_from = delta_capital_income,
                                names_prefix = "Income_Group_"))  %>%
        write_csv(
          file.path(
            globals$output_root,
            scenario_info$ID,
            'conventional/supplemental/capital_income_deltas.csv')
        )
    }
    
    # Apply capital income deltas to tax unit data
    tax_units %>%
      mutate(
        agi_temp = wages + txbl_int + div_ord + div_pref + state_ref +
          txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains +
          sole_prop + part_active + part_passive - part_active_loss -
          part_passive_loss - part_179 + scorp_active + scorp_passive -
          scorp_active_loss - scorp_passive_loss - scorp_179 + rent -
          rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
        income_group = cut(agi_temp, cutoffs, labels = cutoff_labels, include.lowest = T)
      ) %>%
      left_join(deltas, by = c('income_group')) %>%
      mutate(
        across(
          .cols = cap_inc_vars, 
          .fns  = ~ .x * (1 + delta_capital_income)
        )
      ) %>%
      
      # Remove intermediate calculations and return
      select(!c(income_group, agi_temp, delta_capital_income)) %>%
      return()
    
  }

}

do_wealth = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # Adjusts capital income variables to account for an off model wealth tax
  # and the associated avoidance
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #
  # Returns: tibble of tax units with post-adjustment capital income values. 
  #----------------------------------------------------------------------------
  
  cap_inc_vars = c('txbl_int', 'exempt_int', "div_ord", "div_pref", "kg_st", 
                   "kg_lt", "sole_prop", "farm",
                   
                   # Components of Schedule E
                   "part_active", "part_passive", "part_active_loss", 
                   "part_passive_loss", "part_179", 
                   
                   "scorp_active", "scorp_passive", "scorp_active_loss", 
                   "scorp_passive_loss", "scorp_179",
                   
                   "rent", "rent_loss",
                   
                   "estate", "estate_loss"
                   )
  
  current_year = unique(tax_units$year)

  deltas = read_csv(file.path('/gpfs/gibbs/project/sarin/shared/model_data/Wealth-Tax-Simulator/v1/2025011312', scenario_id, 'detail', paste0(current_year, '.csv'))) %>%
    mutate(
      income_group = cut(income.conventional, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
    ) %>%
    group_by(income_group) %>%
    reframe(
      static = sum(weight * net_worth.static),
      conventional = sum(weight * net_worth.conventional)
    ) %>%
    left_join(.,
              read_csv(file.path('/gpfs/gibbs/project/sarin/shared/model_data/Wealth-Tax-Simulator/v1/2025011312/baseline', 'detail', paste0(current_year, '.csv'))) %>%
                mutate(
                  income_group = cut(income.conventional, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
                ) %>%
                group_by(income_group) %>%
                reframe(
                  baseline = sum(weight * net_worth.static)
                ), by = "income_group") %>%
    group_by(income_group) %>%
    mutate(
      x = .13,
      delta_net_worth = min(0, ((x * (conventional - static) + (static - baseline)) / baseline))
    ) 
  
  if(current_year == 2026) {
    deltas %>%
      select(income_group, delta_net_worth) %>%
      mutate(year = current_year) %>%
      pivot_wider(
        names_from = income_group, 
        values_from = delta_net_worth, 
        names_prefix = "Income_Group_") %>%
      write_csv(
        file.path(
          globals$output_root, 
          scenario_info$ID,
          'conventional/supplemental/net_worth_deltas.csv'))

    } else {
    
      read_csv(file.path(globals$output_root, scenario_info$ID,
                       'conventional/supplemental/net_worth_deltas.csv')) %>%
        bind_rows(deltas  %>%
                    select(income_group, delta_net_worth) %>%
                    mutate(year = current_year) %>%
                    pivot_wider(names_from = income_group,
                                values_from = delta_net_worth,
                                names_prefix = "Income_Group_"))  %>%
        write_csv(
          file.path(
            globals$output_root,
            scenario_info$ID,
            'conventional/supplemental/net_worth_deltas.csv')
        )
  }
  
  
  tax_units %>%
    mutate(
      agi_temp = wages + txbl_int + div_ord + div_pref + state_ref +
        txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains +
        sole_prop + part_active + part_passive - part_active_loss -
        part_passive_loss - part_179 + scorp_active + scorp_passive -
        scorp_active_loss - scorp_passive_loss - scorp_179 + rent -
        rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
      
      income_group = cut(agi_temp, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
    ) %>%
    left_join(., deltas) %>%
    mutate(across(cap_inc_vars, ~ .x * (1 + delta_net_worth))) %>%
    select(!c(income_group, agi_temp, delta_net_worth)) %>%
    return()
}

# read_csv(file.path('/gpfs/gibbs/project/sarin/shared/model_data/Wealth-Tax-Simulator/v1/2025011312/nickel_dime', 'detail', paste0(current_year, '.csv'))) %>%
#   mutate(
#     income_group = cut(income.conventional, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
#   ) %>%
#   group_by(income_group) %>%
#   reframe(
#     static = sum(weight * net_worth.static),
#     conventional = sum(weight * net_worth.conventional)
#   ) %>%
#   mutate(
#     gap = conventional - static
#   )
# 
# 
# read_csv(file.path('/gpfs/gibbs/project/sarin/shared/model_data/Wealth-Tax-Simulator/v1/2025011312/baseline', 'detail', paste0(current_year, '.csv'))) %>%
#   mutate(
#     income_group = cut(income.conventional, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
#   ) %>%
#   group_by(income_group) %>%
#   reframe(
#     baseline = sum(weight * net_worth.static)
#   )
# 
#  toad = tax_units %>%
#   mutate(
#     agi_temp = wages + txbl_int + div_ord + div_pref + state_ref +
#       txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains +
#       sole_prop + part_active + part_passive - part_active_loss -
#       part_passive_loss - part_179 + scorp_active + scorp_passive -
#       scorp_active_loss - scorp_passive_loss - scorp_179 + rent -
#       rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
# 
#     income_group = cut(agi_temp, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
#   ) %>%
#   left_join(.,
#             read_csv(file.path(scenario_info$interface_paths$`Wealth-Tax-Simulator`, 'detail', paste0(current_year, '.csv'))) %>%
#               mutate(
#                 income_group = cut(income.conventional, c(-Inf,1e6,2e6,3e6,4e6,5e6,1e7,Inf), labels = c(-1,1,2,3,4,5,10), include.lowest = T)
#               ) %>%
#               group_by(income_group) %>%
#               reframe(
#                 delta_net_worth = sum(weight * net_worth.conventional) / sum(weight * net_worth.static)
#               )
#   ) %>%
#   mutate(across(cap_inc_vars, ~ .x * delta_net_worth)) %>%
#   select(!c(income_group, agi_temp, delta_net_worth))


do_child_earnings = function(tax_units, ...) { 
  
  #----------------------------------------------------------------------------
  # For each age between 0-18:
  #  Filter to parents of 1+ children of this age 
  #  Calculate baseline parental income percentiles
  #  Assign baseline parental income percentiles to each record (parent_rank_baseline)
  #  Assign policy parental income percentile, according to baseline thresholds (parent_rank_policy)
  #  ??? Calculate baseline adult outcome based on transition matrix
  # 
  # For each age 18+ 
  #  Go back to year minus age and load parental income percentile change microdata
  #  Assign baseline child rank based on reverse matrix bayes 
  #  Assgin policy "..."
  #  Scale up labor earnings (???) by X% times expected increase in wages, plus whatever other conditions (e.g. only do this below $80K)
  #
  # Do all the work in year...like read-in future tax units and adjust
  # 
  # Scale effect size by exposure...a phase in of sorts...
  # 
  # Parameters: 
  #   - tax_units (df) : tibble of tax units with calculated variables
  #
  # Returns: TODO
  #----------------------------------------------------------------------------
  

  
  #----------------------------------------------------
  # Calculate future impacts from current-year changes
  #----------------------------------------------------
  
  # Read intergenerational mobility parameters
  mobility_matrix = read_csv('resources/mobility_matrix.csv') %>% 
    pivot_longer(cols            = -child_rank, 
                 names_to        = 'parent_rank', 
                 names_transform = as.integer,
                 values_to       = 'pdf') %>%
    relocate(parent_rank, .before = everything()) %>% 
    arrange(parent_rank, child_rank) 
  
  # Add income variable
  tax_units %<>% 
    mutate(income = wages + trad_contr_er1 + trad_contr_er2 + txbl_int + 
                    exempt_int + div_ord + div_pref + state_ref + txbl_ira_dist + 
                    gross_pens_dist + kg_st + kg_lt + other_gains + alimony + 
                    sole_prop + part_active + part_passive - part_active_loss - 
                    part_passive_loss - part_179 + scorp_active + scorp_passive - 
                    scorp_active_loss - scorp_passive_loss - scorp_179 + farm + 
                    gross_ss + ui + other_inc + salt_workaround_part + salt_workaround_scorp)
  
  # Calculate parent income percentiles for each child age group 
  get_parent_income_thresholds = function(age) {
    
    # Filter to parents of this age group
    parents = tax_units %>% 
      filter(dep_age1 == age | dep_age2 == age | dep_age3 == age)
    
    # Determine thresholds
    thresholds = wtd.quantile(
      x       = parents$income, 
      weights = parents$weight, 
      p       = seq(0, 1, 0.01)
    )
    return(thresholds)
  }
  
  parent_income_thresholds = 0:17 %>% 
    map(get_parent_income_thresholds) %>% 
    bind_rows() %>%
    mutate(child_age = 0:17) %>% 
    pivot_longer(cols            = -child_age, 
                 names_to        = 'parent_rank', 
                 names_transform = ~ as.integer(str_extract(., "[0-9]+")), 
                 values_to       = 'parent_threshold') %>% 
    expand_grid(child_rank = 1:100) %>% 
    relocate(child_rank, .after = parent_rank) %>% 
    left_join(mobility_matrix, by = c('parent_rank', 'child_rank'))
  

  
  #------------------------------------------------------------------------
  # Apply changes to current year owing to policy changes from prior years 
  #------------------------------------------------------------------------
  
  
  # Set parameter: first year of policy
  start_year = 2025
  
  
  # Get size of policy effect
  policy_effect = globals$output_root %>% 
    file.path('arpa_ctc/static/supplemental/distribution_income.csv') %>% 
    read_csv() %>% 
    filter(!includes_corp, financing == 'none') %>%
    group_by(year) %>%
    mutate(delta_income = pct_chg_ati / share_cut, 
           direct_cost = sum(group_delta) / 1e9) %>%
    ungroup() %>% 
    filter(income_group %in% c('Bottom quintile', 'Second quintile')) %>% 
    select(year, income_group, direct_cost, delta_income)
  
  
  get_effect = function(year) {
    tax_units = globals$output_root %>% 
      file.path('arpa_ctc/static/detail/', paste0(year, '.csv')) %>% 
      fread() %>% 
      tibble() 
    
    thresholds = wtd.quantile(
      x       = tax_units$expanded_inc, 
      weights = tax_units$weight, 
      p       = seq(0, 1, 0.1)
    )

    tax_units %>% 
      mutate(child_rank = cut(
        x      = expanded_inc, 
        breaks = c(thresholds, 1e99), 
        labels = names(thresholds))
      ) %>% 
      mutate(child_rank = as.integer(str_extract(child_rank, "[0-9]+"))) %>% 
      left_join(
        mobility_matrix %>% 
          mutate(across(c(parent_rank, child_rank), ~floor(. / 10) * 10)) %>% 
          group_by(child_rank) %>% 
          summarise(`Bottom quintile` = sum(pdf * (parent_rank < 20)) / 10, 
                    `Second quintile` = sum(pdf * (parent_rank %in% c(20, 30))) / 10) %>% 
          mutate(other = 1 - `Bottom quintile` - `Second quintile`) %>% 
          pivot_longer(cols      = -child_rank,
                       names_to  = 'income_group', 
                       values_to = 'p'), 
        by = 'child_rank', 
        relationship = 'many-to-many'
      ) %>%
      mutate(income_group = replace_na(income_group, 'other'), 
             p            = replace_na(p, 0)) %>% 
      
      # Calculate exposure size adjusted for years as a kid under the policy
      mutate(year = year) %>% 
      left_join(
        policy_effect %>% 
          rename(policy_year = year) %>% 
          expand_grid(year_offset = 1:20) %>% 
          mutate(future_year = policy_year + year_offset) %>% 
          select(-year_offset) %>% 
          filter(future_year <= 2053) %>% 
          expand_grid(age = 18:80) %>%
          mutate(birth_year           = future_year - age, 
                 child_in_policy_year = as.integer((birth_year + 17) >= policy_year)) %>% 
          group_by(year = future_year, income_group, age) %>% 
          summarise(exposure = sum(delta_income * child_in_policy_year) / 17,
                    .groups = 'drop'), 
        by = c('year', 'income_group', 'age1' = 'age')
      ) %>% 
      mutate(exposure = replace_na(exposure, 0)) %>% 
      expand_grid(causal_share = c(0.1, 0.2, 1),
                  ige          = c(0.2, 0.5, 0.7)) %>%
      group_by(ige, causal_share) %>%
      summarise(year         = mean(year),
                delta_tax_20 = sum(wages * weight * p * exposure * ige * causal_share * mtr_wages * (income_group == 'Bottom quintile')) / 1e9,
                delta_tax_40 = sum(wages * weight * p * exposure * ige * causal_share * mtr_wages) / 1e9,
                .groups = 'drop') %>%
      pivot_longer(cols            = starts_with('delta_'),
                   names_to        = 'income_group',
                   names_prefix    = 'delta_tax_',
                   names_transform = ~ paste0('Nonzero effect for bottom ', ., '%'),
                   values_to       = 'delta_tax') %>%
      return()
  }
  
  
  # Estimate aggregate effect
  estimate = 2026:2053 %>% 
    map(get_effect) %>% 
    bind_rows()

  
  estimate %>% 
    left_join(policy_effect %>% 
                distinct(year, direct_cost), 
              by = 'year')  %>% 
    mutate(net_cost = direct_cost + delta_tax) %>%
    mutate(causal_ige = ige * causal_share) %>% 
    filter(causal_ige %in% c(0.05, 0.1, 0.2, 0.7), 
           income_group == 'Nonzero effect for bottom 20%') %>% 
    pivot_longer(c(direct_cost, delta_tax), names_to = 'source') %>% 
    mutate(source = if_else(source == 'delta_tax', 'Revenue from higher earnings in adulthood', 'Direct costs')) %>% 
    ggplot(aes(x = year, y = value, fill = source)) +
    geom_col() +
    geom_line(aes(y = net_cost), linewidth = 2) + 
    geom_hline(yintercept = 0) +
    facet_wrap(~causal_ige, nrow = 1) + 
    scale_fill_manual(values = c('#ed392f', '#32a852')) + 
    theme_bw() +
    scale_x_continuous(breaks = seq(2025, 2053, 4)) + 
    scale_y_continuous(breaks = seq(-180, 20, 20)) + 
    labs(x = 'Year', y = 'Billions of dollars', fill = '') + 
    theme(legend.position = 'top') + 
    ggtitle('Net budget cost of permanent ARPA CTC',
             subtitle = 'By causal IGE for the bottom 20%')
  
  
  
  estimate %>% 
    left_join(policy_effect %>% 
                distinct(year, direct_cost), 
              by = 'year') %>% 
    mutate(offset_size = -delta_tax / direct_cost) %>% 
    mutate(causal_ige = ige * causal_share) %>% 
    filter(causal_ige %in% c(0.05, 0.1, 0.2, 0.7)) %>% 
    ggplot(aes(x = year, y = offset_size, 
               colour = as.factor(causal_ige), 
               linetype = income_group)) + 
    geom_point() + 
    geom_line() + 
    theme_bw() + 
    labs(x = 'Year', y = 'Share of direct cost', )
    ggtitle('Partial dynamic offset share of direct cost')
  
  # Assumptions
  # just have to have been exposed in one year (conservative)
  

}







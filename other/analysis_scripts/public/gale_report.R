#---------------------------------------------------------
# Produces supplementary calculations used in Gale report
#---------------------------------------------------------


library(tidyverse)
library(data.table)



#---------------------
# Distribution charts
#---------------------

stacked_dist_scenarios = list(
  `Base broadeners common to all plans` = list(
    vintage = '202406071423', 
    provisions = c(
      '1) Repeal itemized deductions' = '01-item',
      '2) Repeal preferential tax rates' = '03-pref',
      '3) Repeal all personal credits' = '04-credits', 
      '4) Repeal HoH status' = '05-hoh',
      '5) Repeal the AMT' = '06-amt'
    )
  ), 
  `Simple` = list(
    vintage = '202406071513', 
    provisions = c(
      '1) Base broadeners' = '06-amt',
      '2) Introduce a $1,000 UBI' = 'simple-09-ubi',
      '3) Introduce a new wage subsidy' = 'simple-10-wagesub'
    )
  ), 
  `Modified Simple` = list(
    vintage = '202406071557', 
    provisions = c(
      '1) Base broadeners' = '06-amt',
      '2) Introduce a $2,000 UBI with phaseout' = 'mod_simple-09-ubi',
      '3) Introduce a new wage subsidy' = 'mod_simple-10-wagesub'
    )
  ), 
  `Back to the Future` = list(
    vintage = '202406071640', 
    provisions = c(
      '1) Base broadeners' = '06-amt',
      '2) Increase the standard deduction' = '07-std',
      '3) Increase tax rates to at least 25%' = '08-rates',
      '4) Introduce a $2,800 UBI' = 'back_future-09-ubi',
      '5) Introduce a new wage subsidy' = 'back_future-10-wagesub', 
      '6) Impose a 10% broad-based VAT' = 'back_future-vat'
    )
  ), 
  `UBI` = list(
    vintage = '202406071731', 
    provisions = c(
      '1) Base broadeners' = '06-amt',
      '2) Introduce a $3,900 UBI' = 'ubi-09-ubi',
      '3) Introduce a new wage subsidy' = 'ubi-10-wagesub', 
      '4) Impose a 10% broad-based VAT' = 'ubi-vat'
    )
  )
)


get_stacked_chart_data = function(type, scenario) { 
  
  chart_data = stacked_dist_scenarios[[scenario]]$provisions %>%
    names() %>% 
    map(
      ~ file.path(
        '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1',
        stacked_dist_scenarios[[scenario]]$vintage,
        stacked_dist_scenarios[[scenario]]$provisions[.x],
        'static/supplemental', 
        paste0('distribution_', type, '.csv') 
      ) %>% 
        read_csv() %>% 
        mutate(provision = .x)
    ) %>% 
    bind_rows() %>% 
    
    # Rename group variable
    rename_with(.cols = any_of(c('income_group', 'age_group')), 
                .fn   = ~ 'group') %>% 
    
    # Remove extraneous rows and columns
    filter(group != 'Negative income', 
           financing == 'none', 
           includes_other, 
           year %in% c(2026, 2054)) %>% 
    select(year, provision, group, pct_chg_ati) %>% 
    
    # Get contributions
    group_by(year, group) %>% 
    mutate(contribution = 100 * (pct_chg_ati - lag(pct_chg_ati, default = 0))) %>% 
    ungroup() %>% 
    
    # Reshape wide in group
    select(-pct_chg_ati) %>% 
    pivot_wider(names_from  = group, 
                values_from = contribution) %>%
    
    # Add totals series
    bind_rows(
      (.) %>% 
        group_by(year) %>%
        summarise(across(.cols = -provision, .fns = sum)) %>% 
        mutate(provision = 'total')
    ) %>%
    arrange(year) %>%
    
    # Write 
    write_csv(
      file.path(
        './other/analysis_scripts/', 
        paste0('stacked_dist_', type, '_', scenario, '.csv')
      )
    )
  
  return(chart_data)
}

contribution_chart_data = stacked_dist_scenarios %>% 
  names() %>% 
  map(~ get_stacked_chart_data('income', .x)) %>% 
  set_names(names(stacked_dist_scenarios)) 


contribution_plots = contribution_chart_data %>% 
  names() %>% 
  map(.f = ~ contribution_chart_data[[.x]] %>% 
        filter(year == 2026) %>%
        select(-year) %>% 
        pivot_longer(cols     = -provision, 
                     names_to = 'rank') %>% 
        left_join((.) %>% 
                    filter(provision == 'total') %>% 
                    select(rank, total = value), 
                  by = 'rank') %>% 
        filter(provision != 'total') %>% 
        mutate(rank = fct_inorder(factor(rank))) %>% 
        ggplot(aes(x = rank, y = value, fill = provision)) + 
        geom_col() +
        geom_point(aes(y = total), size = 4) +
        geom_hline(yintercept = 0) + 
        theme_bw() + 
        labs(x = 'Income group', y = 'Percent', fill = 'Provision') + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
        ggtitle(.x, subtitle = 'Contribution to overall percent change in after-tax income, 2026')
  ) 
  

contribution_chart_data %>% 
  names() %>% 
  map(.f = ~contribution_chart_data[[.x]] %>% 
        mutate(scenario = .x)
  ) %>% 
  bind_rows() %>% 
  filter(scenario != 'Provisions common to all plans',
         provision == 'total', 
         year == 2026) %>% 
  select(-year) %>% 
  pivot_longer(cols     = -c(scenario, provision), 
               names_to = 'rank') %>% 
  mutate(rank = fct_inorder(factor(rank))) %>% 
  ggplot(aes(x = rank, y = value, colour = scenario)) + 
  geom_point(size = 4) +
  geom_line(aes(x = as.integer(rank))) + 
  geom_hline(yintercept = 0) + 
  theme_bw() +   
  labs(x = 'Income group', y = 'Percent', colour = 'Scenario') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle('Percent change in after-tax income, 2026')



#-----------
# Tax rates 
#-----------

mtrs = c('baseline', paste0('partial-', c('simple', 'mod_simple', 'back_future', 'ubi'))) %>% 
  map(
    ~ file.path(
      '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202406071306',
      .x,
      'static/detail/2026.csv'  
    ) %>% 
      fread() %>% 
      tibble() %>%
      mutate(
        scenario = case_when(
          .x == 'baseline'            ~ 'Current law',
          .x == 'partial-simple'      ~ 'Simple',
          .x == 'partial-mod_simple'  ~ 'Modified simple',
          .x == 'partial-back_future' ~ 'Back to the future',
          .x == 'partial-ubi'         ~ 'UBI'
        ), 
        parent = if_else(n_dep_ctc > 0, 'Parent', 'Non-parent')
      ) %>% 
      select(scenario, id, weight, parent, filing_status, 
             wages.1 = wages1, wages.2 = wages2, mtr_wages.1 = mtr_wages1, mtr_wages.2 = mtr_wages2)
  ) %>% 
  bind_rows() %>%
  
  # Convert to person level data 
  pivot_longer(cols      = contains('.'), 
               names_sep = '[.]', 
               names_to  = c('name', 'index')) %>%
  filter(filing_status == 2 | index == 1) %>% 
  pivot_wider() %>% 
  
  # Add bins and calculate average MTRs
  mutate(wage_bin = 5000 * (cut(wages, seq(0, 80000, 5000), labels = F) - 1)) %>% 
  group_by(scenario, parent, wage_bin) %>% 
  summarise(mtr_wages = weighted.mean(mtr_wages, weight), 
            .groups = 'drop') %>% 
  
  # Clean up and reshape
  filter(!is.na(wage_bin)) %>% 
  pivot_wider(names_from = scenario, values_from = mtr_wages) %>% 
  
  # Write
  write_csv('./other/analysis_scripts/mtrs.csv')


mtrs %>% 
  pivot_longer(cols     = -c(parent, wage_bin, `Current law`), 
               names_to = 'Scenario') %>% 
  ggplot(aes(x = wage_bin)) + 
  geom_line(aes(y = value, colour = Scenario)) +
  geom_point(aes(y = value, colour = Scenario)) + 
  geom_line(aes(y = `Current law`, linetype = 'Current law')) + 
  geom_point(aes(y = `Current law`)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~parent) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(-0.2, 0.5, 0.1)) + 
  labs(x = 'Wages', y = 'Effective marginal tax rate', 
       Scenario = 'Reform scenario',  linetype = '') + 
  ggtitle('Average Effective Marginal Tax Rates on Wage Earnings, 2026')
  



#------------------------
# Child earnings impacts
#------------------------

outcomes =  paste0('partial-', c('simple', 'mod_simple', 'back_future', 'ubi')) %>% 
  map(
    ~ '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202406071314' %>% 
      file.path(.x, 'static/supplemental/child_earnings/outcomes_2054.csv') %>% 
      read_csv() %>% 
      mutate(scenario = case_when(
        .x == 'partial-simple'      ~ '1) Simple',
        .x == 'partial-mod_simple'  ~ '2) Modified Simple',
        .x == 'partial-back_future' ~ '3) Back to the Future',
        .x == 'partial-ubi'         ~ '4) UBI'
      ))
  ) %>% 
  bind_rows() %>%
  mutate(parent_quintile = floor((parent_rank - 1) / 20) + 1) %>% 
  group_by(scenario, parent_quintile) %>% 
  summarise('Percent change' = weighted.mean(pct_change, n), 
            'Average change' = weighted.mean(pct_change * wages / n, n) / (1.022 ^ 30)) %>%
  write_csv('./other/analysis_scripts/child_earnings.csv')



outcomes %>% 
  ggplot(aes(x = parent_quintile, y = `Percent change` * 100, colour = scenario)) +
  geom_point(size = 4) +
  geom_line() + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  labs(x = 'Parent income quintile', y = 'Percentage points', colour = 'Reform scenario') + 
  ggtitle('Estimated Impact on Later-Life Earnings by Parent Income Quintile, 2054', 
          subtitle = 'Change in Wages Relative to Baseline') 
  

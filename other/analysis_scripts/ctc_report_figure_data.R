#-----------------------------------------------------------
# Custom post-processing script to generate data for tables   
# and charts for TCJA launch product website content
#-----------------------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)


output_root = 'C:/Users/jar335/Downloads/ctc_runs' # '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1'

#-----------------------------
# Stacked distribution charts
#-----------------------------

stacked_dist_scenarios = list(
  `Current policy` = list(
    vintage = '202403122228', 
    provisions = c(
      '1) Impose SSN requirement' = '01-eligibility',
      '2) Increase maximum value' = '02-value',
      '3) Limit refund' = '03-phasein',
      '4) Increase the phase-out threshold' = '04-phaseout'
    )
  ),
  `2021 law` = list(
    vintage = '202403122306', 
    provisions = c(
      '1) Increase age limit' = '01-eligibility',
      '2) Increase maximum value' = '02-value',
      '3) Eliminate phase-in' = '03-phasein',
      '4) Increase phase-out threshold' = '04-phaseout'
    )
  ), 
  `Edelberg-Kearney` = list(
    vintage = '202403122344', 
    provisions = c(
      '1) Increase age limit' = '01-eligibility',
      '2) Increase maximum value' = '02-value',
      '3) Allow half of credit at $0 of earnings and increase phase-in rate' = '03-phasein',
      '4) Reduce phase-out rate' = '04-phaseout'
    )
  ), 
  `FSA` = list(
    vintage = '202403130022', 
    provisions = c(
      '1) Raise age limit' = 'ctc-01-eligibility',
      '2) Increase maximum value' = 'ctc-02-value',
      '3) Increase phase-in rate' = 'ctc-03-phasein',
      '4) Increase phase-out threshold' = 'ctc-04-phaseout', 
      '5) Reform the EITC' = 'eitc',
      '6) Eliminate Head Of Household filing status' = 'hoh',
      '7) Eliminate Child and Dependent Care Tax Credut' = 'cdctc',
      '8) Eliminate deduction for state and local taxes' = 'salt'
    )
  )
)

get_stacked_chart_data = function(type, scenario) { 
  
  stacked_dist_scenarios[[scenario]]$provisions %>%
    names() %>% 
    map(
      ~ file.path(
        output_root,
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
           !includes_corp) %>% 
    select(provision, group, pct_chg_ati) %>% 
    
    # Get contributions
    group_by(group) %>% 
    mutate(contribution = 100 * (pct_chg_ati - lag(pct_chg_ati, default = 0))) %>% 
    ungroup() %>% 
    
    # Reshape wide in group
    select(-pct_chg_ati) %>% 
    pivot_wider(names_from  = group, 
                values_from = contribution) %>%
    
    # Add totals series
    bind_rows(
      (.) %>% 
        summarise(across(.cols = -provision, .fns = sum)) %>% 
        mutate(provision = 'total')
    ) %>% 
    
    # Write 
    write_csv(
      file.path(
        './other/analysis_scripts/', 
        paste0('stacked_dist_', type, '_', scenario, '.csv')
      )
    )
}

for (type in c('age', 'income')) {
  stacked_dist_scenarios %>% 
    names() %>% 
    walk(~get_stacked_chart_data(type, .x))
}




#-----------------------
# ATI comparison charts
#-----------------------

ati_scenarios = list(
  `Current policy` = list(
    vintage    = '202403122228', 
    provisions = '04-phaseout'
  ),
  `2021 law` = list(
    vintage   = '202403122306', 
    provision = '04-phaseout'
  ), 
  `Edelberg-Kearney` = list(
    vintage   = '202403122344', 
    provision = '04-phaseout'
  ), 
  `Family Security Act 2.0 CTC` = list(
    vintage   = '202403130022', 
    provision = 'ctc-04-phaseout'
  ), 
  `Family Security Act 2.0` = list(
    vintage   = '202403130022', 
    provision = 'salt'
  )
)

get_ati_data = function(type) {
  
  # Read data
  ati_scenarios %>% 
    names() %>% 
    map(
      ~ file.path(
        output_root,
        ati_scenarios[[.x]]$vintage,
        ati_scenarios[[.x]]$provision,
        'static/supplemental', 
        paste0('distribution_', type, '.csv') 
      ) %>% 
        read_csv() %>% 
        mutate(scenario = .x)
    ) %>% 
    bind_rows() %>% 
    
    # Rename group variable
    rename_with(.cols = any_of(c('income_group', 'age_group')), 
                .fn   = ~ 'group') %>% 
    
    # Remove extraneous rows and columns
    filter(group != 'Negative income', 
           financing == 'none', 
           !includes_corp) %>% 
    select(scenario, group, pct_chg_ati) %>% 
    
    # Express in percentage points
    mutate(pct_chg_ati = pct_chg_ati * 100) %>%
    
    # Reshape wide in scenario
    pivot_wider(names_from  = scenario, 
                names_sep   = '', 
                values_from = 'pct_chg_ati') %>%
    # Write 
    write_csv(
      file.path(
        './other/analysis_scripts/', 
        paste0('ati_', type, '.csv')
      )
    )
} 


c('age', 'income') %>% 
  walk(get_ati_data)


#--------------------------
# Winners and losers chart
#--------------------------

winners_losers_scenarios = list(
  `Current policy` = list(
    vintage    = '202403122228', 
    provisions = '04-phaseout'
  ),
  `2021 law` = list(
    vintage   = '202403122306', 
    provision = '04-phaseout'
  ), 
  `Edelberg-Kearney` = list(
    vintage   = '202403122344', 
    provision = '04-phaseout'
  ), 
  `Family Security Act 2.0 CTC` = list(
    vintage   = '202403130022', 
    provision = 'ctc-04-phaseout'
  ), 
  `Family Security Act 2.0` = list(
    vintage   = '202403130022', 
    provision = 'salt'
  )
)


get_winners_losers_data = function(type) {
  
  # Read data
  winners_losers_scenarios %>% 
    names() %>% 
    map(
      ~ file.path(
        output_root,
        winners_losers_scenarios[[.x]]$vintage,
        winners_losers_scenarios[[.x]]$provision,
        'static/supplemental', 
        paste0('distribution_', type, '.csv') 
      ) %>% 
        read_csv() %>% 
        mutate(scenario = .x)
    ) %>% 
    bind_rows() %>% 
    
    # Rename group variable
    rename_with(.cols = any_of(c('income_group', 'age_group')), 
                .fn   = ~ 'group') %>% 
    
    # Remove extraneous rows and columns
    filter(group != 'Negative income', 
           financing == 'none', 
           !includes_corp) %>% 
    select(scenario, group, `Tax cut` = share_cut, `Tax increase` = share_raise) %>% 
    
    # Format
    mutate(`No change` = 1 - `Tax cut` - `Tax increase`, 
           across(.cols = contains(' '), .fns = ~ . * 100)) %>%
    relocate(`No change`, .after = `Tax cut`) %>% 
    
    # Write 
    write_csv(
      file.path(
        './other/analysis_scripts/', 
        paste0('winners_losers_', type, '.csv')
      )
    )
} 

c('age', 'income') %>% 
  walk(get_winners_losers_data)


#-----------
# MTR chart
#-----------


# Read data
c('baseline', 'perm_tcja_ctc', 'perm_arpa_ctc', 
  'edelberg_kearney', 'fsa_ctc', 'fsa') %>% 
  map(
    ~ file.path(
      output_root,
      'mtrs',
      .x,
      'static/detail/2026.csv'  
    ) %>% 
      fread() %>% 
      tibble() %>%
      mutate(scenario = .x, parent = if_else(n_dep_ctc > 0, 'Parent', 'Non-parent')) %>% 
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
  mutate(wage_bin = 5000 * (cut(wages, seq(0, 60000, 5000), labels = F) - 1)) %>% 
  group_by(scenario, parent, wage_bin) %>% 
  summarise(mtr_wages = weighted.mean(mtr_wages, weight), 
            .groups = 'drop') %>% 
  
  # Clean up and reshape
  filter(!is.na(wage_bin)) %>% 
  mutate(scenario = case_when(
    scenario == 'baseline'         ~ 'Current law', 
    scenario == 'perm_tcja_ctc'    ~ 'Current policy', 
    scenario == 'perm_arpa_ctc'    ~ '2021 law', 
    scenario == 'edelberg_kearney' ~ 'Edelberg-Kearney', 
    scenario == 'fsa_ctc'          ~ 'Family Security Act 2.0 CTC', 
    scenario == 'fsa'              ~ 'Family Security Act 2.0 full proposal'
  )) %>%
  pivot_wider(names_from = scenario, values_from = mtr_wages) %>% 
  
  # Write
  write_csv('./other/analysis_scripts/mtrs.csv')


#----------------
# Child earnings
#----------------


child_earnings_scenarios = list(
  `Current policy` = list(
    vintage    = '202403122228', 
    provisions = 'partially_dynamic'
  ),
  `2021 law` = list(
    vintage   = '202403122306', 
    provision = 'partially_dynamic'
  ), 
  `Edelberg-Kearney` = list(
    vintage   = '202403122344', 
    provision = 'partially_dynamic'
  ), 
  `Family Security Act 2.0 CTC` = list(
    vintage   = '202403130022', 
    provision = 'partially_dynamic_ctc_only'
  ), 
  `Family Security Act 2.0` = list(
    vintage   = '202403130022', 
    provision = 'partially_dynamic'
  )
)


# Read data
child_earnings_scenarios %>% 
  names() %>% 
  map(
    ~ file.path(
      output_root,
      child_earnings_scenarios[[.x]]$vintage,
      child_earnings_scenarios[[.x]]$provision,
      '/static/supplemental/child_earnings/outcomes_2050.csv') %>% 
      read_csv() %>% 
      mutate(scenario = .x)  
    ) %>% 
  bind_rows() %>% 
  
  # Get averages by quintile
  mutate(across(.cols = c(child_rank, parent_rank), 
                .fns  = ~ floor((. - 1) / 20) + 1)) %>% 
  group_by(scenario, parent_rank, child_rank) %>% 
  summarise(pct_change = weighted.mean(pct_change, n), 
            .groups = 'drop') %>% 
  
  # Clean, reshape, and write
  filter(!is.na(child_rank), child_rank %in% c(1, 5)) %>% 
  arrange(parent_rank) %>%
  mutate(across(.cols = c(parent_rank, child_rank), 
                .fns  = ~ case_when(
                            . == 1 ~ 'Bottom quintile',
                            . == 2 ~ 'Second quintile',
                            . == 3 ~ 'Middle quintile',
                            . == 4 ~ 'Fourth quintile',
                            . == 5 ~ 'Top quintile')
                )) %>% 
  pivot_wider(names_from = scenario, values_from = pct_change) %>% 
  arrange(child_rank) %>% 
  write_csv('./other/analysis_scripts/child_earnings.csv')



#------------------
# Child income CDF
#------------------

# Read data
microdata = c('baseline', 'perm_tcja_ctc', 'perm_arpa_ctc', 
              'edelberg_kearney', 'fsa_ctc', 'fsa') %>%
  map(
    ~ file.path(
      output_root,
      'mtrs',
      .x,
      '/static/detail/2026.csv') %>% 
      read_csv() %>% 
      mutate(scenario = .x, 
             ati      = expanded_inc - liab_iit_net - liab_pr_ee) %>%
      select(scenario, id, n_dep_ctc, weight, expanded_inc, ati)
  ) %>% 
  bind_rows() %>%
  left_join(
    (.) %>%
      filter(scenario == 'baseline') %>%
      select(id, baseline_ati = ati), 
    by = 'id'
  )

child_income_distribution = seq(0, 500e3, 5e3) %>%
  map(~ microdata %>% 
        mutate(change = ati - baseline_ati) %>%
        group_by(scenario) %>% 
        summarise(threshold = .x, 
                  share_kids = sum((expanded_inc < .x) * n_dep_ctc * weight) / sum(n_dep_ctc * weight),
                  cdf_change = sum((expanded_inc < .x) * change * weight) / sum(change * weight))
  ) %>% 
  bind_rows() %>%
  group_by(scenario) %>%
  mutate(pdf_change = cdf_change - lag(cdf_change, default = 0), 
         ratio = cdf_change / share_kids) %>% 
  ungroup()


child_income_distribution %>% 
  filter(scenario != 'baseline', 
         scenario != 'fsa') %>% 
  ggplot(aes(x = share_kids, y = ratio, colour = scenario)) + 
  geom_line(size = 0.8) +
  #geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0) + 
  theme_bw() 


share_change = microdata %>% 
  mutate(change = ati - baseline_ati) %>%
  mutate(pctile = cut(
    x = expanded_inc, 
    breaks = wtd.quantile(
      x = expanded_inc, 
      weights = weight * n_dep_ctc, 
      probs = seq(0, 1, 0.1)
    ),
    labels = F
    )
  ) %>% 
  group_by(pctile, scenario) %>% 
  summarise(change = sum(change * weight), 
            .groups = 'drop') %>% 
  group_by(scenario) %>% 
  mutate(ratio = change * 10 / sum(change), 
         excess = ratio - 1) %>% 
  ungroup()

share_change %>% 
  filter(scenario != 'baseline', 
         scenario != 'fsa') %>% 
  ggplot(aes(x = pctile, y = ratio, colour = scenario)) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = 1:10) + 
  labs(x = 'Decile') + 
  theme_bw()

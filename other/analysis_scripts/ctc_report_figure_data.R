#-----------------------------------------------------------
# Custom post-processing script to generate data for tables   
# and charts for TCJA launch product website content
#-----------------------------------------------------------

library(tidyverse)
library(data.table)


#-----------------------------
# Stacked distribution charts
#-----------------------------

stacked_dist_scenarios = list(
  arpa_ctc = list(
    vintage = '202403112354', 
    provisions = c(
      '1) Increase age limit and remove SSN requirement' = '01-eligibility',
      '2) Eliminate the phase-in' = '02-phasein',
      '3) Increase the phase-out threshold' = '03-phaseout',
      '4) Increase the maximum value' = '04-value'
    )
  ), 
  edelberg_kearney = list(
    vintage = '202403120033', 
    provisions = c(
      '1) Increase age limit and remove SSN requirement' = '01-eligibility',
      '2) Allow partial refundability and increse phase-in rate' = '02-phasein',
      '3) Reduce the phase-out rate' = '03-phaseout',
      '4) Increase the maximum value' = '04-value'
    )
  ), 
  romney = list(
    vintage = '202403120112', 
    provisions = c(
      '1) Raise the age limit' = 'ctc-01-eligibility',
      '2) Increase the phase-in rate' = 'ctc-02-phasein',
      '3) Increase the phase-out threshold' = 'ctc-03-phaseout', 
      '4) Increase the maximum value' = 'ctc-04-value', 
      '5) EITC payfor' = 'eitc',
      '6) HOH payfor' = 'hoh',
      '7) CDCTC payfor' = 'cdctc',
      '8) SALT payfor' = 'salt'
    )
  )
)

get_stacked_chart_data = function(type, scenario) { 
  
  stacked_dist_scenarios[[scenario]]$provisions %>%
    names() %>% 
    map(
      ~ file.path(
        '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1',
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






get_stacked_chart_data = function(type, scenario) { 
  
  stacked_dist_scenarios[[scenario]]$provisions %>%
    names() %>% 
    map(
      ~ file.path(
        '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1',
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
    ungroup()  
    
}


for (type in c('age', 'income')) {
  stacked_dist_scenarios %>% 
    names() %>% 
    map(~get_stacked_chart_data(type, .x) %>% 
          ggplot(aes(x = group, y = contribution, fill = provision)) + 
          geom_col())
}


#-----------------------
# ATI comparison charts
#-----------------------

ati_scenarios = list(
  `Full Extension` = list(
    vintage   = '202403072232', 
    provision = '07-qbi'
  ), 
  `Partial Extension` = list(
    vintage   = '202403072336', 
    provision = 'qbi'
  ), 
  `Clausing-Sarin` = list(
    vintage   = '202403080033',
    provision = '14-ctc'
  )
)

get_ati_data = function(type) {
  
  # Read data
  ati_scenarios %>% 
    names() %>% 
    map(
      ~ file.path(
        '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1',
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
           financing == 'none') %>% 
    select(scenario, includes_corp, group, pct_chg_ati) %>% 
    
    # Express in percentage points
    mutate(pct_chg_ati = pct_chg_ati * 100) %>%
    
    # Reshape wide in scenario-corp 
    mutate(includes_corp = if_else(includes_corp, ', including corporate taxes', '')) %>%
    pivot_wider(names_from   = c(scenario, includes_corp), 
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
  `Full Extension` = list(
    vintage   = '202403072232', 
    provision = '07-qbi'
  ), 
  `Partial Extension` = list(
    vintage   = '202403072336', 
    provision = 'qbi'
  ), 
  `Clausing-Sarin` = list(
    vintage   = '202403080033',
    provision = '14-ctc'
  )
)

get_winners_losers_data = function(type) {
  
  # Read data
  winners_losers_scenarios %>% 
    names() %>% 
    map(
      ~ file.path(
        '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1',
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
c('baseline', 'full_extension', 'sarin_clausing') %>% 
  map(
    ~ file.path(
      '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403101543/',
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
    scenario == 'baseline'       ~ 'Current law', 
    scenario == 'full_extension' ~ 'Full and Partial Extension', 
    scenario == 'sarin_clausing' ~ 'Clausing-Sarin'
  )) %>%
  pivot_wider(names_from = scenario, values_from = mtr_wages) %>% 
  
  # Write
  write_csv('./other/analysis_scripts/mtrs.csv')


#----------------
# Child earnings
#----------------

# Read data
c('full_extension', 'partial_extension', 'sarin_clausing') %>% 
  map(
    ~ file.path(
      '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403102353',
      .x,
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
  mutate(scenario = case_when(
    scenario == 'full_extension'    ~ 'Full Extension', 
    scenario == 'partial_extension' ~ 'Partial Extension', 
    scenario == 'sarin_clausing'    ~ 'Clausing-Sarin'
  )) %>% 
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
  

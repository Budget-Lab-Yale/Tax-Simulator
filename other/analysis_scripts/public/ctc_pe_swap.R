#------------------------------------------------------
# Code used to produce the calculations and figures in 
# the TBL April 2025 blog about CTC-exemption trade  
#------------------------------------------------------


library(tidyverse)
library(data.table)
library(Hmisc)



#-----------
# Read data
#-----------

# Read microdata 
results = bind_rows(
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081630/baseline/static/detail/2018.csv') %>% 
    tibble() %>% 
    mutate(year = 2018, scenario = 'tcja'), 
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081630/ctc/static/detail/2018.csv') %>% 
    tibble(year = 2018, ) %>% 
    mutate(scenario = 'ctc'),
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081630/pe_dep/static/detail/2018.csv') %>% 
    tibble() %>% 
    mutate(year = 2018, scenario = 'pe_dep'), 
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081635/ctc/static/detail/2026.csv') %>% 
    tibble() %>% 
    mutate(year = 2026, scenario = 'tcja'),
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081635/pe_dep/static/detail/2026.csv') %>% 
    tibble() %>% 
    mutate(year = 2026, scenario = 'ctc'),
  fread('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202504081635/pe_nondep/static/detail/2026.csv') %>% 
    tibble() %>% 
    mutate(year = 2026, scenario = 'pe_dep')
)

# Read inflation projections
inflation = bind_rows(
  read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2025021319/baseline/historical.csv'), 
  read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2025021319/baseline/projections.csv')
) %>% 
  select(year, ccpiu_irs)


#-------------------
# Calculate metrics
#-------------------

# Calculate deltas by provision and year
deltas = results %>% 
  select(year, scenario, id, liab_iit_net) %>% 
  pivot_wider(names_from  = scenario, 
              values_from = liab_iit_net) %>% 
  mutate(delta.ctc    = ctc - tcja, 
         delta.pe_dep = pe_dep - ctc,
         delta.net    = delta.ctc + delta.pe_dep) %>%
  select(-tcja, -ctc, -pe_dep) %>% 
  arrange(id, year)

# Add other variables and adjust for inflation
deltas = results %>% 
  mutate(n_kids = (!is.na(dep_age1) & dep_age1 < 18) +
                  (!is.na(dep_age2) & dep_age2 < 18) + 
                  (!is.na(dep_age3) & dep_age3 < 18), 
         married = as.integer(filing_status == 2)) %>%
  filter(scenario == 'tcja', dep_status == 0, n_kids > 0) %>% 
  select(year, scenario, id, weight, married, n_kids, agi, wages, expanded_inc) %>% 
  left_join(deltas, by = c('year', 'id')) %>% 
  mutate(
    inflation_factor = if_else(year == 2018, 
                               inflation %>% 
                                 summarise(ccpiu_irs[year == 2026] / ccpiu_irs[year == 2018]) %>% 
                                 deframe(), 
                               1), 
    across(.cols = c(starts_with('delta'), agi, wages, expanded_inc), 
           .fns  = ~ . * inflation_factor)
  )


# Calculate and assign deciles
deltas %<>% 
  filter(agi > 0) %>% 
  group_by(year) %>% 
  mutate(
    pctile = cut(
      x      = agi, 
      breaks = wtd.quantile(agi, weight, seq(0, 1, 0.01)), 
      labels = F
    ), 
    pctile_weighted = cut(
      x      = agi, 
      breaks = wtd.quantile(agi, weight * (1 + as.integer(married == 1)), seq(0, 1, 0.01)), 
      labels = F
    ),
    agi_cat = cut(
      x      = agi, 
      breaks = seq(0, 600000, 10000), 
      labels = seq(0, 599000, 10000)
    ) %>% 
      as.character() %>% 
      as.integer()
  ) %>% 
  ungroup() %>% 
  
  # Re-attach negative income tax units 
  bind_rows(
    deltas %>% 
      filter(agi <= 0)
  ) 
  

# Calculate summary metrics
get_summary_metrics = function(...) {
  
  deltas %>% 
    group_by(...) %>% 
    summarise(
      
      avg_inc     = weighted.mean(agi, weight),
      avg_nom_inc = weighted.mean(agi / inflation_factor, weight),
      min_inc     = min(agi),
      avg_wages   = weighted.mean(wages, weight),
      
      # Averages
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean(., weight), 
             .names = 'avg_{col}'),
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean(. / n_kids, weight), 
             .names = 'perkid_avg_{col}'),
      
      # Nominal averages
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean(. / inflation_factor, weight), 
             .names = 'nom_avg_{col}'),
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean((. / n_kids) / inflation_factor, weight), 
             .names = 'nom_perkid_avg_{col}'),
      
      # Percent change
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean(. / agi, weight * agi), 
             .names = 'etr_{col}'),
      across(.cols  = starts_with('delta'), 
             .fns   = ~ weighted.mean((. / n_kids) / agi, weight * agi), 
             .names = 'perkid_etr_{col}'),
      
      # Winners/losers
      share_winners = sum((delta.net > 5) * weight) / sum(weight),
      
      .groups = 'drop'
    )
}


#----------------
# Generate plots
#----------------

get_summary_metrics(year)

get_summary_metrics(pctile_weighted, year) %>% 
  rename(`Expand CTC`                  = nom_perkid_avg_delta.ctc, 
         `Repeal dependent\nexemptions` = nom_perkid_avg_delta.pe_dep) %>% 
  pivot_longer(c(`Expand CTC`, `Repeal dependent\nexemptions`)) %>% 
  filter(year == 2018) %>% 
  ggplot(aes(x = pctile_weighted, y = value, fill = name)) + 
  geom_col(width = 1) + 
  geom_point(aes(y = nom_perkid_avg_delta.net)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = seq(-1500, 2000, 500)) +
  theme_bw() + 
  scale_fill_manual(values = c('#ffe7b1', '#009e73')) +
  labs(x = 'AGI percentile', y = element_blank(), fill = 'Provision', 
       caption = 'Source: The Budget Lab calculations.') + 
  theme(plot.caption = element_text(hjust = 0, size = 8)) +
  ggtitle('Figure 1. Average Per-Child Benefit from the TCJA’s Exemptions-For-CTC Trade, 2018', 
          subtitle = 'Contribution to Net Benefit')
  
  

get_summary_metrics(pctile_weighted, year) %>% 
  ggplot(aes(x = pctile_weighted, y = perkid_avg_delta.net, colour = as.factor(year))) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +  
  scale_y_continuous(labels = scales::dollar_format(), breaks = seq(-500, 1250, 250)) +
  theme_bw() + 
  labs(x = 'Income percentile', y = '2026 dollars', colour = 'Year', 
       caption = 'Source: The Budget Lab calculations.') + 
  theme(plot.caption = element_text(hjust = 0, size = 8)) +
  ggtitle('Figure 2. Average Per-Child Benefit from the TCJA’s Exemptions-For-CTC Trade, 2018 vs 2026', 
          subtitle = '2026 Dollars')





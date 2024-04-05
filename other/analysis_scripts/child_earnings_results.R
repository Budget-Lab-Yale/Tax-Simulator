#-----------------------------------------------------------
# Custom post-processing script to generate data for tables   
# and charts for TCJA launch product website content
#-----------------------------------------------------------

library(tidyverse)

# Set params
output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403261058/perm_arpa_ctc/'
years = 2026:2054

#-----------
# Read data
#-----------

# Exposure size
exposure = years %>% 
  map(~ output_root %>%  
          file.path(paste0('/static/supplemental/child_earnings/exposure_', .x, '.csv')) %>% 
          read_csv(show_col_types = F) %>% 
          mutate(year = .x, .before = everything())
      ) %>%
  bind_rows()

# Effect size
outcomes = years %>%
  `[`(2:length(years)) %>% 
  map(~ output_root %>%  
        file.path(paste0('/static/supplemental/child_earnings/outcomes_', .x, '.csv')) %>% 
        read_csv(show_col_types = F) %>% 
        mutate(year = .x, .before = everything())
  ) %>%
  bind_rows()

# Revenues
revenue_estimates = bind_rows(
  file.path(output_root, 'static/supplemental/revenue_estimates.csv') %>% 
    read_csv() %>% 
    mutate(type = 'direct'), 
  file.path(output_root, 'conventional/supplemental/revenue_estimates.csv') %>% 
    read_csv() %>% 
    mutate(type = 'indirect')
) %>% 
  pivot_wider(names_from = type, values_from = total) %>%
  mutate(indirect = indirect - direct) 

# Read indexed data
revenue_estimates_indexed = bind_rows(
  file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403211024/perm_arpa_ctc_indexed/static/supplemental/revenue_estimates.csv') %>% 
    read_csv() %>% 
    mutate(type = 'direct'), 
  file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403211024/perm_arpa_ctc_indexed/conventional/supplemental/revenue_estimates.csv') %>% 
    read_csv() %>% 
    mutate(type = 'indirect')
  ) %>% 
  pivot_wider(names_from = type, values_from = total) %>%
  mutate(indirect = indirect - direct) 

# Macro projections
projections = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2024021509/baseline/projections.csv')


#---------------------
# Exposure size chart
#---------------------

exposure_size_figure_data = exposure %>% 
  filter(year == 2026) %>%
  mutate(decile  = floor((parent_rank - 1) / 10) + 1,
         percent = delta_income / expanded_inc) %>%
  select(decile, income = expanded_inc, 
         `Dollars` = delta_income, `Percent of income` = percent, `Rank units` = delta_rank) %>% 
   pivot_longer(cols     = -c(decile, income), 
               names_to = 'metric') %>%
  group_by(metric, decile) %>% 
  summarise(value = weighted.mean(value, income), 
            .groups = 'drop') %>% 
  pivot_wider(names_from = 'metric')

exposure_size_figure_data %>% 
  pivot_longer(cols     = -decile, 
               names_to = 'metric') %>% 
  ggplot(aes(x = decile, y = value)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ metric, scales = 'free') + 
  scale_x_continuous(breaks = 1:10) + 
  theme_bw() + 
  labs(x = 'Parent income decile', y = element_blank())

#-----------------
# Earnings matrix
#-----------------

outcomes %>% 
  filter(year == 2050) %>% 
  group_by(parent_quintile = floor((parent_rank - 1) / 20) + 1, 
           child_quintile  = floor((child_rank - 1) / 20) + 1) %>% 
  summarise(pct_change = 100 * weighted.mean(pct_change, n), 
            .groups = 'drop') %>% 
  mutate(label = round(pct_change, 2)) %>% 
  ggplot(aes(x = parent_quintile, y = child_quintile, fill = pct_change)) +
  geom_tile(color    = 'black',
            lwd      = 0.2,
            linetype = 1) +
  coord_fixed() + 
  scale_fill_gradient(low = 'white', high = '#76D17F') + 
  geom_text(aes(label = label), color = "black", size = 3) + 
  theme(legend.position = 'none', panel.background = element_blank()) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = 1:5) + 
  labs(x = 'Parent quintile', y = 'Child quintile')

#-----------------------------
# Revenue contributions chart
#-----------------------------

revenue_estimates %>% 
  mutate(version = ' Unindexed') %>% 
  bind_rows(
    revenue_estimates_indexed %>% 
      mutate(version = 'Indexed')
  ) %>% 
  pivot_longer(cols            = -c(year, version), 
               names_transform = str_to_title) %>% 
  ggplot(aes(x = year, y = value, fill = name)) + 
  geom_col() + 
  theme_bw() + 
  geom_hline(yintercept = 0) +
  facet_wrap(~version) + 
  scale_x_continuous(breaks = seq(2025, 2055, 5)) + 
  labs(x = 'Fiscal year', y = 'Billions of dollars', fill = 'Source')


#----------------------
# Estimate comparisons
#----------------------

outcomes %>% 
  filter(year == 2050) %>% 
  summarise(below_50  = weighted.mean(pct_change, n * (parent_rank <= 38)), 
            below_100 = weighted.mean(pct_change, n * (parent_rank <= 62)),
            all       = weighted.mean(pct_change, n), 
            werner    = weighted.mean(pct_change, n * (parent_rank <= 44)))


test %>% mutate(across(.cols = c(child_rank, parent_rank), 
                         .fns  = ~ floor((. - 1) / 20) + 1)) %>% 
  group_by(Scenario = scenario, child_rank) %>% 
  summarise(pct_change = weighted.mean(pct_change, n), 
            .groups = 'drop') %>% bind_rows(test %>% mutate(across(.cols = c(child_rank, parent_rank), 
                                                                   .fns  = ~ floor((. - 1) / 20) + 1)) %>% 
                                              group_by(Scenario = scenario,) %>% 
                                              summarise(pct_change = weighted.mean(pct_change, n), 
                                                        .groups = 'drop') %>% mutate(child_rank = 0)) %>%
  filter(!is.na(child_rank)) %>% 
  mutate(child_rank = case_when(
    child_rank == 0 ~ ' Overall',
    child_rank == 1 ~ '1) Among current-law bottom-\nquintile earners in adulthood',
    child_rank == 2 ~ '2) Among current-law second-\nquintile earners in adulthood',
    child_rank == 3 ~ '3) Among current-law middle-\nquintile earners in adulthood',
    child_rank == 4 ~ '4) Among current-law fourth-\nquintile earners in adulthood',
    child_rank == 5 ~ '5) Among current-law top-\nquintile earners in adulthood'
  )) %>%
  ggplot(aes(x = child_rank, y = pct_change, fill = Scenario)) + 
  geom_col(position = 'dodge') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  


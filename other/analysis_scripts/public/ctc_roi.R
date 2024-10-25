#-------------------------------------------------------
# Script to calculate cohort-level return on investment
# for the 2021 CTC
#-------------------------------------------------------

library(tidyverse)
library(data.table)

output_root    = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202403261909'
first_year     = 2026
years          = first_year:2097
discount_rates = 0:5/100


#-------------------
# Calculate outlays
#-------------------

# Define function to get direct outlays for a year
get_outlays = function(yr) {
  c('baseline', 'perm_arpa_ctc') %>% 
    map(.f = ~ output_root %>% 
          
          # Read data
          file.path(.x, 'static/detail/', paste0(yr, '.csv')) %>% 
          fread() %>% 
          tibble() %>%
          mutate(scenario = .x) %>% 
          
          # Filter to families with kids in this age cohort
          mutate(n_age = (!is.na(dep_age1) & (dep_age1 == (yr - first_year))) + 
                         (!is.na(dep_age2) & (dep_age2 == (yr - first_year))) + 
                         (!is.na(dep_age3) & (dep_age3 == (yr - first_year)))) %>% 
          filter(n_age > 0) %>% 
          select(scenario, id, weight, n_dep, n_age, liab_iit_net)
    ) %>% 
    bind_rows() %>% 
    
    # Reshape wide in scenario and calculate size of reform
    pivot_wider(names_from = scenario, values_from = liab_iit_net) %>% 
    mutate(diff = perm_arpa_ctc - baseline) %>% 
    select(-baseline, -perm_arpa_ctc) %>% 
    
    # Adjust to reflect age cohort only (note this is technical average, not marginal...)
    mutate(diff = diff * (n_age / n_dep)) %>% 
    
    # Calculate direct outlay
    summarise(year        = yr, 
              direct_cost = sum(-diff * weight) / 1e9) 
}

# Get outlays 
outlays = (first_year + 0:17) %>% 
  map(get_outlays) %>% 
  bind_rows() 


#--------------------
# Calculate revenues
#--------------------
  
# Define function to calculate returns for a specific year
get_returns = function(year) {
  c('static', 'conventional') %>% 
    map(.f = ~ output_root %>% 
          
          # Read data
          file.path('perm_arpa_ctc', .x, 'detail', paste0(year, '.csv')) %>% 
          fread() %>% 
          tibble() %>%
          mutate(type = .x) %>% 
          
          # Filter to tax units of cohort age
          filter(age1 == year - first_year) %>% 
          
          # Select payroll tax liability and income tax liability
          mutate(revenue = liab_iit_net + liab_pr) %>% 
          select(type, id, weight, wages, revenue)
    ) %>% 
    bind_rows() %>% 
    
    # Reshape long in variable 
    pivot_longer(cols     = c(wages, revenue), 
                 names_to = 'metric') %>% 
    
    # Reshape wide in scenario and calculate effect size
    pivot_wider(names_from = type) %>% 
    mutate(diff = conventional - static) %>% 
    select(-conventional, -static) %>% 
    
    # Calculate total revenue impact for both wages and liability
    group_by(metric) %>% 
    summarise(year    = year, 
              returns = sum(diff * weight) / 1e9)
}

# Get returns
returns = expand_grid(metric = c('wages', 'revenue'), 
                       year   = years) %>% 
  left_join(
     (first_year + 18:70) %>% 
       map(get_returns) %>% 
       bind_rows(), 
     by = c('metric', 'year')
  )



#---------------
# Calculate NPV
#---------------



# Construct interest rate scenarios
expand_grid(
  year            = direct_year:max(years),
  rate_assumption = discount_rates 
) %>% 
  
  # Calculate interest costs
  arrange(rate_assumption) %>% 
  group_by(rate_assumption) %>% 
  mutate(debt          = direct_cost * cumprod(1 + rate_assumption), 
         interest_cost = if_else(year == yr, 0, debt - lag(debt)), 
         direct_cost   = if_else(year == yr, direct_cost, 0)) %>% 
  ungroup() %>% 
  select(rate_assumption, year, direct_cost, interest_cost)

primary_surplus = tibble(year = years) %>% 
  
  # Join everything together
  left_join(outlays, by = 'year') %>% 
  left_join(returns %>% 
              filter(metric == 'revenue') %>% 
              select(-metric), 
            by = 'year') %>% 

  # Calculate primary deficit
  mutate(returns         = replace_na(returns, 0),
         direct_cost     = replace_na(direct_cost, 0),
         primary_surplus = returns - direct_cost) 

# Define function to dynamically calculate debt
calculate_debt = function(df, rate) {
  debt = vector(mode = 'numeric', length = length(years))
  for (t in seq_along(years)) {
    if (t == 1) {
      debt[t] = -df$primary_surplus[t]
    } else {
      debt[t] = debt[t - 1] * (1 + rate) - df$primary_surplus[t]
    }
  }
  return(debt)
}

# Calculate debt and under different rates
npv = discount_rates %>% 
  map(.f = ~ primary_surplus %>% 
        mutate(
          rate_assumption = .x, 
          debt            = calculate_debt(., .x), 
          interest_cost   = debt - lag(debt, default = 0) + primary_surplus)
      ) %>%
  bind_rows() %>% 
  select(-debt, -primary_surplus) %>% 
  relocate(rate_assumption) %>% 
  
  # Add return metric dimension 
  expand_grid(metric = c('wages', 'revenue')) %>% 
  left_join(returns %>% 
              mutate(new_returns = replace_na(returns, 0)) %>% 
              select(-returns), 
            by = c('metric', 'year')) %>% 
  mutate(returns = if_else(metric == 'wages', new_returns, returns)) %>%
  select(-new_returns) %>% 
  relocate(metric, .after = rate_assumption) %>% 
  arrange(rate_assumption, metric) %>% 

  # Discount cash flows
  group_by(rate_assumption, metric) %>% 
  mutate(across(.cols = c(direct_cost, interest_cost, returns), 
                .fns  = ~ . / cumprod(1 + lag(rate_assumption, default = 0)))) %>% 
  ungroup() %>% 
  
  # Get aggregates 
  mutate(direct_cost = -direct_cost, interest_cost = -interest_cost) %>%
  pivot_longer(cols     = c(direct_cost, interest_cost, returns), 
               names_to = 'series') %>% 
  group_by(rate_assumption, metric, series) %>% 
  summarise(total = sum(value), 
            .groups = 'drop') %>%
  
  # Calculate offsets and net values 
  pivot_wider(names_from  = series, 
              values_from = total) %>% 
  mutate(net_with       = direct_cost + interest_cost + returns, 
         net_without    = direct_cost + returns, 
         offset_with    = -returns / (direct_cost + interest_cost), 
         offset_without = -returns / direct_cost) 


npv %>% 
  select(rate_assumption, metric, 
         `Direct costs`   = direct_cost, 
         `Interest costs` = interest_cost, 
         `Returns`        = returns) %>%
  pivot_longer(cols = -c(rate_assumption, metric)) %>% 
  group_by(rate_assumption, metric) %>% 
  mutate(total = sum(value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = rate_assumption, y = value, fill = name)) + 
  geom_col() +
  geom_hline(yintercept = 0) + 
  geom_point(aes(y = total), size = 3) + 
  facet_wrap(~metric) + 
  scale_x_continuous(breaks = 0:5/100) + 
  scale_fill_manual(values = c('#fcba03', '#e35d5d', '#8dd9a1')) + 
  theme_bw() + 
  labs(x = 'Interest rate', y = 'Billions of PV dollars', fill = 'Source') +
  ggtitle('NPV of ARPA CTC given to 2026 newborn cohort', 
          subtitle = 'By measure of return: revenues (public) vs wages (social)')

  
  

npv %>% 
  select(rate_assumption, metric, offset_yes = offset_with, offset_no = offset_without) %>% 
  pivot_longer(cols = -c(rate_assumption, metric), 
               names_prefix = 'offset_', 
               names_transform = str_to_title) %>% 
  ggplot(aes(x = rate_assumption, y = value, colour = name)) + 
  geom_point(size = 3) +
  geom_line() + 
  geom_hline(yintercept = c(0, 1)) + 
  facet_wrap(~metric) +
  theme_bw() +
  scale_y_continuous(breaks = 0:16/4) + 
  labs(x = 'Interest rate', y = 'Share of costs offset by returns', colour = 'Include interest costs?') + 
  ggtitle('Offset ratio of ARPA CTC given to 2026 newborn cohort', 
          subtitle = 'By measure of return: revenues (public) vs wages (social)')

  
  
  

#-----------------------------------------------
# TODO
#-----------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

# Set roots
output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202409241523'



#-----------
# Load data
#-----------

# Define function to read simulator output
get_data = function(scenario, year) {
  output_root %>% 
    file.path(scenario, 'static/detail/', paste0(year, '.csv')) %>% 
    fread() %>% 
    tibble() %>%
    filter(dep_status == 0) %>% 
    mutate(scenario = scenario, year = year) %>% 
    return()
} 


# Read Tax-Simulator data 
microdata = bind_rows(
  get_data('baseline', 2025), 
  get_data('tips', 2025), 
  get_data('tips_ot', 2025), 
  get_data('baseline', 2026), 
  get_data('tips', 2026), 
  get_data('tips_ot', 2026)
)


test = microdata %>% 
  filter(expanded_inc > 0, wages > 0) %>% 
  mutate(
    eq_inc   = expanded_inc / sqrt(1 + (filing_status == 2) + n_dep),  #((1 + (filing_status == 2) + 0.7 * n_dep) ^ 0.7),
    eq_wages = wages        / sqrt(1 + (filing_status == 2) + n_dep)   #((1 + (filing_status == 2) + 0.7 * n_dep) ^ 0.7)
  ) %>% 
  group_by(year, scenario) %>% 
  mutate(
    inc_pctile = cut(
      x      = expanded_inc, 
      breaks = wtd.quantile(expanded_inc, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.integer(), 
    eq_inc_pctile = cut(
      x      = eq_inc, 
      breaks = wtd.quantile(eq_inc, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.integer(), 
    wages_pctile = cut(
      x      = wages, 
      breaks = wtd.quantile(wages, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.integer(), 
    eq_wages_pctile = cut(
      x      = eq_wages, 
      breaks = wtd.quantile(eq_wages, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.integer()
  ) %>% 
  ungroup()



test %>%  
  filter(year == 2026) %>% 
  group_by(eq_inc_pctile, scenario) %>% 
  reframe(
    name  = c(0.5, 2.5, 5, 10, 25, 75, 90, 95, 97.5, 99.5),
    value = wtd.quantile(mtr_wages, weight, c(0.005, 0.025, 0.05, 0.10, 0.25, 0.75, 0.90, 0.95, 0.975, 0.995)) 
  ) %>% 
  pivot_wider() %>% 
  mutate(
    `Middle 50%` = `75` - `25`,
    `Middle 80%` = `90` - `10`,
    `Middle 90%` = `95` - `5`, 
    `Middle 95%` = `97.5` - `2.5`
  ) %>% 
  select(eq_inc_pctile, scenario, starts_with('Middle')) %>% 
  pivot_longer(cols = starts_with('Middle')) %>% 
  pivot_wider(names_from = scenario) %>% 
  mutate(tips = tips - baseline, tips_ot = tips_ot - baseline) %>% 
  pivot_longer(cols = c(tips, tips_ot), names_to = 'scenario') %>% 
  ggplot(aes(x = eq_inc_pctile, y = value, colour = scenario)) + 
  geom_point(alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(se = F) + 
  facet_wrap(~name, ncol = 4)




test %>%  
  filter(year == 2026) %>% 
  group_by(
    eq_inc_pctile = floor((eq_inc_pctile - 1) / 10) + 1, 
    scenario
  ) %>% 
  reframe(
    name  = c(0.5, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99.5),
    value = wtd.quantile(mtr_wages, weight, c(0.005, 0.025, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.975, 0.995)) 
  ) %>% 
  pivot_wider() %>% 
  mutate(
    `low.Middle 50%` = `25`,  `high.Middle 50%` = `75`,   `mid.Middle 50%` = `50`,
    `low.Middle 90%` = `5`,   `high.Middle 90%` = `95`,   `mid.Middle 90%` = `50`
  ) %>% 
  select(eq_inc_pctile, scenario, starts_with('low.'), starts_with('high.'), starts_with('mid.')) %>% 
  pivot_longer(
    cols      = c(starts_with('low.'), starts_with('high.'), starts_with('mid.')), 
    names_sep = '[.]', 
    names_to  = c('bound', 'metric')
  ) %>%  
  mutate(mid = if_else(bound == 'mid', 'circle', NA)) %>% 
  filter(scenario != 'tips') %>% 
  ggplot(aes(x = value, y = eq_inc_pctile + as.numeric(as.factor(scenario)) / 4 - 3/8, colour = scenario)) +  
  geom_line(aes(group = eq_inc_pctile + as.numeric(as.factor(scenario)) / 4 - 3/8)) + 
  geom_point(aes(shape = mid), show.legend = F) + 
  geom_vline(xintercept = 0) +
  facet_wrap(~metric, ncol = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(-0.5, 0.4, 0.1), labels = scales::percent_format()) + 
  scale_y_continuous(breaks = seq(1:10)) + 
  labs(
    x      = 'Average effective tax rate on wages', 
    y      = 'Equalized income decile', 
    colour = 'Scenario'
  ) + 
  ggtitle('Variation in tax rates on wages by income decile')




test %>%  
  filter(year == 2025) %>% 
  group_by(eq_inc_pctile, scenario) %>% 
  summarise(
    sd_dev = sqrt(wtd.var(mtr_wages, weight)), 
    .groups = 'drop'
  ) %>%  
  ggplot(aes(x = eq_inc_pctile, y = sd_dev, colour = scenario)) + 
  geom_point(alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(se = F)




# TODO
#  - more controls -- get at tax law sources of deviation not income

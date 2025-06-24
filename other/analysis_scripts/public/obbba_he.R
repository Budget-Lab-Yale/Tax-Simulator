#----------------------------------------------
# Script to produce horizontal equity metrics 
# for commentary piece on tips, OT exemptions
#----------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

# Set root
output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Simulator/v1/202506231509'


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
    get_data('baseline', 2026), 
    get_data('tcja',  2026),
    get_data('house', 2026), 
    get_data('senate',  2026)
  ) %>% 
  
  # Calculate tax rates and income percentiles
  filter(expanded_inc > 0) %>% 
  rename(inc = expanded_inc) %>% 
  mutate(
    married          = as.integer(filing_status == 2),
    eq.inc           = inc / (1 + (filing_status == 2) + 0.7 * n_dep) ^ 0.7, 
    etr_inc_net      = pmax(-1, pmin(1, liab_iit_net / inc)), 
    etr_inc          = pmax(-1, pmin(1, liab_iit     / inc)), 
    eq.etr_inc_net   = pmax(-1, pmin(1, liab_iit_net / eq.inc)), 
    eq.etr_inc       = pmax(-1, pmin(1, liab_iit     / eq.inc)), 
  ) %>% 
  group_by(year, scenario) %>% 
  mutate(
    inc_pctile = cut(
      x      = inc, 
      breaks = wtd.quantile(inc, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.numeric(), 
    eq.inc_pctile = cut(
      x      = eq.inc, 
      breaks = wtd.quantile(eq.inc, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.numeric(), 
  ) %>% 
  ungroup() %>% 
  filter(!is.na(inc_pctile))


#--------------------
# Dispersion metrics
#--------------------

# Calculate average within-group disperion by group
var_by_group = microdata %>% 
  group_by(scenario, married, n_dep, inc_pctile) %>%
  summarise(
    n = sum(weight),
    across(
      .cols  = c(etr_inc_net, etr_inc), 
      .fns   = list(
        sd    = ~ wtd.var(., weight) ^ 0.5,
        mid50 = ~ wtd.quantile(., weight, 0.75) - wtd.quantile(., weight, 0.25)
      ),
      .names = '{fn}.{col}' 
    ), 
    .groups = 'drop'
  ) %>% 
  pivot_longer(
    cols      = contains('.'), 
    names_sep = '[.]', 
    names_to  = c('metric', 'rate_type')
  ) 


# Calculate average within-group dispersion by income percentile
var_by_income = var_by_group %>% 
  group_by(scenario, inc_pctile, metric, rate_type) %>% 
  summarise(
    value = weighted.mean(value, n),
    n     = sum(n),
    .groups = 'drop'
  )


# Calculate overall ETRs
var_overall = var_by_group %>% 
  group_by(metric, rate_type, scenario) %>% 
  summarise(value = weighted.mean(value, n), .groups = 'drop')


# Figure 1
var_by_income %>% 
  filter(rate_type == 'etr_inc_net', metric == 'mid50') %>% 
  mutate(
    scenario = case_when(
      scenario == 'baseline' ~ 'TCJA expiration', 
      scenario == 'tcja'     ~ 'TCJA extension', 
      scenario == 'house'    ~ 'House version', 
      scenario == 'senate'   ~ 'Senate version', 
    ), 
    metric = case_when(
      metric == 'mid50' ~ 'Interquartile range', 
      metric == 'sd'    ~ ' Standard deviation'
    ), 
    value = value * 100
  ) %>% 
  ggplot(aes(x = inc_pctile, y = value, colour = scenario)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = F, method = 'loess', formula = 'y ~ x', span = 0.4) + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  labs(
    x        = 'Income percentile',
    y        = '<- Lower dispersion                Percentage points                Higher dispersion ->',
    colour   = element_blank()
  ) +
  theme(legend.position = 'top', text = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  coord_flip() 

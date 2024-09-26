#-----------------------------------------------
# TODO
#-----------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)
library(quantregForest)

# Set roots
output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202409251552'



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
    get_data('tips_ot', 2025),
    get_data('baseline', 2026), 
    get_data('tips_ot', 2026)
  ) %>% 
  
  # Calculate tax rates and income percentiles
  filter(expanded_inc > 0) %>% 
  mutate(
    married       = as.integer(filing_status == 2),
    etr_inc_net   = pmax(-1, pmin(1, liab_iit_net / expanded_inc)), 
    etr_inc       = pmax(-1, pmin(1, liab_iit     / expanded_inc)), 
    etr_wages_net = pmax(-1, pmin(1, mtr_wages))
  ) %>% 
  group_by(year, scenario) %>% 
  mutate(
    inc_pctile = cut(
      x      = expanded_inc, 
      breaks = wtd.quantile(expanded_inc, weight, seq(0, 1, 0.01)), 
      labels = 1:100
    ) %>% as.character() %>% as.numeric()
  ) %>% 
  ungroup() %>% 
  filter(!is.na(inc_pctile))


#---------------------
# Estimate dispersion
#---------------------











train = microdata %>% 
  filter(year == 2025) %>% 
  select(scenario, weight, etr_inc_net, married, n_dep, expanded_inc) %>% 
  sample_n(5000)

qrf = quantregForest(
  x        = train[c('scenario', 'married', 'n_dep', 'expanded_inc')], 
  y        = train$etr_inc_net,
  weights  = train$weight,
  nthreads = 8,
  mtry     = 4
)



train %>% 
  mutate(
    sd = predict(
      object  = qrf, 
      newdata = train, 
      what    = function(x) sqrt(var(x))
    )
  ) %>%
  group_by(scenario) %>% 
  summarise(
    sd = weighted.mean(sd, weight)
  )






manual = microdata %>% 
  group_by(scenario, year, married = filing_status == 2, n_dep, inc_pctile) %>%
  summarise(
    n              = sum(weight),
    etr_inc_net.sd = wtd.var(etr_inc_net, weight) ^ 0.5, 
    etr_inc_net.mid90  = wtd.quantile(etr_inc_net, weight, 0.95) - wtd.quantile(etr_inc_net, weight, 0.05), 
    groups = 'drop'
  ) %>% 
  pivot_longer(cols = starts_with('etr_'))

manual %>% 
  group_by(name, scenario, year, inc_pctile) %>% 
  summarise(value = weighted.mean(value, n)) %>% 
  ggplot(aes(x = inc_pctile, y = value, colour = paste(year, scenario))) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~name) + 
  ylim(0, 0.1) + 
  coord_flip()



demo_adjusted = test %>%  
  filter(!is.na(inc_pctile)) %>% 
  group_by(inc_pctile), %>% 
  summarise(
    n             = sum(weight),
    mtr.mid50     = wtd.quantile(mtr_wages, weight, 0.75) - wtd.quantile(mtr_wages, weight, 0.25), 
    mtr.mid90     = wtd.quantile(mtr_wages, weight, 0.95) - wtd.quantile(mtr_wages, weight, 0.05), 
    mtr.sd        = wtd.var(mtr_wages, weight) ^ 0.5,
    etr_net.mid50 = wtd.quantile(etr_net, weight, 0.75) - wtd.quantile(etr_net, weight, 0.25), 
    etr_net.mid90 = wtd.quantile(etr_net, weight, 0.95) - wtd.quantile(etr_net, weight, 0.05), 
    etr_net.sd    = wtd.var(etr_net, weight) ^ 0.5,
    etr.mid50     = wtd.quantile(etr, weight, 0.75) - wtd.quantile(etr, weight, 0.25), 
    etr.mid90     = wtd.quantile(etr, weight, 0.95) - wtd.quantile(etr, weight, 0.05), 
    etr.sd        = wtd.var(etr, weight) ^ 0.5,
    .groups = 'drop'
  ) %>% 
  pivot_longer(
    cols      = c(starts_with('mtr'), starts_with('etr')), 
    names_sep = '[.]', 
    names_to  = c('tax_rate_metric', 'variance_metric')
  ) %>% 
  group_by(inc_pctile, tax_rate_metric, variance_metric, scenario, year) %>% 
  summarise(
    value = weighted.mean(value, n), 
    n     = sum(n),
    .groups = 'drop'
  )


demo_adjusted %>% 
  group_by(tax_rate_metric, variance_metric, scenario, year) %>% 
  summarise(value = weighted.mean(value, n), .groups = 'drop') %>% 
  filter(variance_metric == 'sd') %>% 
  ggplot(aes(x = paste0(year, ': ', scenario), y = value, fill = as.factor(year))) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~tax_rate_metric) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(x = element_blank(), y = element_blank(), fill = 'Year')




demo_adjusted_sums = test %>%  
  filter(!is.na(inc_pctile)) %>% 
  group_by(inc_pctile, married = filing_status == 2, n_dep, scenario, year) %>% 
  summarise(
    n            = sum(weight),
    mtr.mean     = weighted.mean(mtr_wages, weight), 
    mtr.sd       = wtd.var(mtr_wages, weight) ^ 0.5,
    etr_net.mean = weighted.mean(etr_net, weight), 
    etr_net.sd   = wtd.var(etr_net, weight) ^ 0.5,
    etr.mean     = weighted.mean(etr, weight), 
    etr.sd       = wtd.var(etr, weight) ^ 0.5,
    .groups = 'drop'
  ) %>% 
  pivot_longer(
    cols      = c(starts_with('mtr'), starts_with('etr')), 
    names_sep = '[.]', 
    names_to  = c('tax_rate_metric', 'metric')
  ) %>% 
  group_by(tax_rate_metric, metric, scenario, year) %>% 
  summarise(
    value = weighted.mean(value, n, na.rm = T), 
    n     = sum(n),
    .groups = 'drop'
  )



demo_ranges_groups = test %>%  
  filter(!is.na(inc_pctile)) %>% 
  group_by(inc_pctile, married = filing_status == 2, n_dep, scenario, year) %>% 
  reframe(
    n       = sum(weight),
    p       = seq(0, 100, 1), 
    etr     = wtd.quantile(etr, weight, seq(0, 1, 0.01)), 
    etr_net = wtd.quantile(etr_net, weight, seq(0, 1, 0.01)), 
    mtr     = wtd.quantile(mtr_wages, weight, seq(0, 1, 0.01))
  )

demo_ranges = seq(2, 100, 2) %>% 
  map(
    .f = ~ demo_ranges %>% 
      filter(p %in% c(50 - .x / 2, 50 + .x / 2)) %>% 
      mutate(p = if_else(p == 50 - .x / 2, 'low', 'high')) %>% 
      pivot_longer(cols = c(etr, etr_net, mtr), names_to = 'metric') %>% 
      pivot_wider(names_from = p) %>%
      mutate(range = high - low, middle = .x) %>% 
      group_by(metric, scenario, year) %>% 
      summarise(
        middle = mean(middle),
        range  = weighted.mean(range, n),
        n      = sum(n), 
        .groups = 'drop'
      )
  ) %>% 
  bind_rows()


demo_ranges %>% 
  filter(middle < 100) %>% 
  ggplot(aes(x = middle, y = range, colour = paste(year, scenario))) + 
  geom_line() + 
  facet_wrap(~metric) + 
  labs(y = 'Range of tax rates', x = 'Middle X% of tax rates')




demo_adjusted %>%  
  mutate(
    scenario = case_when(
      scenario == 'baseline'     ~ 'Pre-TCJA', 
      scenario == 'tcja'         ~ 'TCJA', 
      scenario == 'tcja_tips_ot' ~ 'TCJA + tips + OT'
    ), 
    tax_rate_metric = case_when(
      tax_rate_metric == 'etr'     ~ 'Average ETR on all income (bound at 0%)',
      tax_rate_metric == 'etr_net' ~ 'Average ETR on all income (including refunds)', 
      tax_rate_metric == 'mtr'     ~ 'Average ETR on wages (including refunds)'
    ), 
  ) %>%
  filter(variance_metric == 'sd') %>% 
  ggplot(aes(x = inc_pctile, y = value, colour = scenario)) + 
  geom_point(alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(se = F) + 
  facet_wrap(~tax_rate_metric) + 
  theme_bw() + 
  ylim(0, 0.08) + 
  labs(x = 'Income percentile', y = '<- less dispersion        more dispersion -> ') + 
  ggtitle('Within-family-type tax rate dispersion: Standard deviation') + 
  coord_flip()



demo_adjusted %>%  
  pivot_wider(names_from = scenario) %>% 
  mutate(delta_tcja         = tcja - baseline, 
         delta_tcja_tips_ot = tcja_tips_ot - tcja + delta_tcja) %>% 
  select(-tcja, -baseline, -tcja_tips_ot) %>% 
  pivot_longer(cols = starts_with('delta'), names_prefix = 'delta_', names_to = 'scenario') %>% 
  mutate(
    scenario = case_when(
      scenario == 'baseline'     ~ 'Pre-TCJA', 
      scenario == 'tcja'         ~ 'TCJA', 
      scenario == 'tcja_tips_ot' ~ 'TCJA + tips + OT'
    ), 
    tax_rate_metric = case_when(
      tax_rate_metric == 'etr'     ~ 'Average ETR on all income (bound at 0%)',
      tax_rate_metric == 'etr_net' ~ 'Average ETR on all income (including refunds)', 
      tax_rate_metric == 'mtr'     ~ 'Average ETR on wages (including refunds)'
    ), 
  ) %>%
  filter(variance_metric == 'sd') %>% 
  ggplot(aes(x = inc_pctile, y = value, colour = scenario)) + 
  geom_point(alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(se = F) + 
  facet_wrap(~tax_rate_metric) + 
  theme_bw() + 
  ylim(-0.015, 0) + 
  labs(x = 'Income percentile', y = 'Change in dispersion') + 
  ggtitle('Change in within-family-type tax rate dispersion: Standard deviation') + 
  coord_flip()




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


test %>%  
  filter(year == 2025, eq_inc_pctile %in% 70:79, scenario != 'tips') %>%  
  ggplot(aes(x = mtr_wages, colour = scenario)) + 
  geom_density(bw = 0.01)


test %>% 
  filter(year == 2025, scenario != 'tips') %>% 
  mutate(decile = floor((eq_inc_pctile - 1) / 10) + 1) %>% 
  ggplot(aes(x = mtr_wages, y = as.factor(decile), colour = scenario, fill = NULL)) + 
  geom_density_ridges(alpha = 0.1, scale = 1, aes(weight = weight)) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.5)) + 
  theme_bw() + 
  labs(y = 'Equalized income decile', x = 'Average effective tax rate on wages') + 
  ggtitle('Distribution of ETRs on wages by income')
  


# TODO
#  - must adjust for level of tax rates. 
#     - total amount of variance 
#  - are you controlling away variance merely correlated with differences in family size, not caused by? 
#     - ex 50th pctile income. married nonmarried, 50-50 split. 
#        - all unmarried people pay 10%. 
#        - 50% of married people own a home. they pay 5%. the rest pay 10%.
#        ...idk
# - qrf
#   - what does it add? rollmean? 
#   - how the fuck is it actually measuring var based on predictions
#     - robustness...
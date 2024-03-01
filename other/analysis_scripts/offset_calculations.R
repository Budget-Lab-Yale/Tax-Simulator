
ids = c('arpa_ctc', 'romney_ctc', 'clausing_sarin', 'partial')

revenue_estimates = ids %>% 
  map(~read_csv(file.path(globals$output_root, .x, 'conventional/totals/receipts.csv')) %>% 
        mutate(scenario = .x)) %>% 
  bind_rows() %>% 
  mutate(type = 'partially_dynamic') %>% 
  bind_rows(
    ids %>% 
      map(~read_csv(file.path(globals$output_root, .x, 'static/totals/receipts.csv')) %>% 
            mutate(scenario = .x)) %>% 
      bind_rows() %>% 
      mutate(type = 'static')
  ) %>% 
  pivot_longer(cols = -c(year, type, scenario)) %>% 
  left_join(
    read_csv(file.path(globals$output_root, 'baseline/static/totals/receipts.csv')) %>% 
      pivot_longer(cols = -year, values_to = 'baseline'), 
    by = c('year', 'name')
  ) %>% 
  mutate(delta = value - baseline) %>% 
  select(-baseline, -value) %>% 
  pivot_wider(names_from = type, values_from = delta) %>% 
  mutate(offset = partially_dynamic - static) %>% 
  select(-partially_dynamic) %>% 
  pivot_longer(cols = c(static, offset), 
               names_to = 'source') %>% 
  mutate(value = replace_na(value, 0))


revenue_estimates %>% 
  group_by(scenario, year, source) %>% 
  summarise(value = sum(if_else(name == 'outlays_tax_credits', -value, value)), 
            .groups = 'drop') %>% 
  filter(year > 2026) %>% 
  group_by(scenario, year) %>% 
  mutate(net = sum(value)) %>%
  ggplot(aes(x = year, y = value, fill = source)) + 
  geom_col() +
  geom_point(aes(y = net)) +
  theme_bw() + 
  facet_wrap(~scenario, scales = 'free') + 
  ggtitle('Annual direct cost and offset')

revenue_estimates %>% 
  group_by(scenario, year, source) %>% 
  summarise(value = sum(if_else(name == 'outlays_tax_credits', -value, value)), 
            .groups = 'drop') %>%
  filter(year > 2025) %>% 
  group_by(scenario, source) %>% 
  mutate(value = cumsum(value)) %>%
  pivot_wider(names_from = source) %>% 
  mutate(offset_share = -offset / static) %>% 
  ggplot(aes(x = year, y = offset_share, colour = scenario)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_point() +
  theme_bw() + 
  ggtitle('Offset share of cumulative direct cost')
  

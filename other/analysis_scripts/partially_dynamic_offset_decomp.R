



output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403202038'


# Overall
test = c('perm_arpa_ctc', 'edelberg_kearney') %>% 
  map(
    .f = ~ bind_rows(
      file.path(output_root, paste0(.x, '_both'), 'static/totals/receipts.csv') %>% 
        read_csv() %>% 
        mutate(type = 'static'), 
      file.path(output_root, paste0(.x, '_employment'), 'conventional/totals/receipts.csv') %>% 
        read_csv() %>% 
        mutate(type = 'employment'),
      file.path(output_root, paste0(.x, '_both'), 'conventional/totals/receipts.csv') %>% 
        read_csv() %>% 
        mutate(type = 'both')
    ) %>% 
      mutate(scenario = .x, .before = everything())
  ) %>% 
  bind_rows() %>% 
  pivot_longer(cols = -c(scenario, year, type))  %>% 
  mutate(value = if_else(name == 'outlays_tax_credits', -value, value)) %>% 
  pivot_wider(names_from  = type) %>% 
  mutate(employment_delta     = employment - static, 
         child_earnings_delta = both - employment) %>% 
  group_by(scenario, name, year) %>% 
  summarise(employment_delta     = sum(employment_delta), 
            child_earnings_delta = sum(child_earnings_delta))


test %>% 
  pivot_longer(cols = contains('_'),
               names_to = 'behavior') %>% 
  group_by(scenario, year, behavior) %>% 
  mutate(total = sum(value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = value, fill = name)) + 
  geom_col() +
  facet_wrap(~scenario + behavior) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(y = total))


# Why do credits fall under EK?


# tax credits
credits = c('perm_arpa_ctc', 'edelberg_kearney') %>% 
  map(
    .f = ~ bind_rows(
      file.path(output_root, paste0(.x, '_both'), 'static/totals/1040.csv') %>% 
        read_csv() %>% 
        mutate(type = 'static'), 
      file.path(output_root, paste0(.x, '_employment'), 'conventional/totals/1040.csv') %>% 
        read_csv() %>% 
        mutate(type = 'employment'),
      file.path(output_root, paste0(.x, '_both'), 'conventional/totals/1040.csv') %>% 
        read_csv() %>% 
        mutate(type = 'both')
    ) %>% 
      mutate(scenario = .x, .before = everything())
  ) %>% 
  bind_rows() %>% 
  mutate(other = ref - ctc_ref - eitc) %>% 
  select(scenario, type, year, ctc_ref, ctc_nonref, eitc) %>% 
  pivot_longer(cols = -c(scenario, year, type)) %>%
  pivot_wider(names_from  = type) %>%
  mutate(employment_delta     = employment - static, 
         child_earnings_delta = both - employment) %>% 
  group_by(scenario, name, year) %>% 
  summarise(employment_delta     = sum(employment_delta), 
            child_earnings_delta = sum(child_earnings_delta))

credits %>% 
  group_by(scenario, year) %>% 
  mutate(total = sum(employment_delta)) %>% 
  ungroup() %>%
  ggplot(aes(x = year, y = employment_delta, fill = name)) +
  geom_col() + 
  geom_point(aes(y = total)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~scenario)



# Who is getting less EITC under EK?
static   = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403201907/edelberg_kearney/static/detail/2026.csv')
behavior = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403201907/edelberg_kearney/conventional/detail/2026.csv')

static %>% 
  select(id, filing_status, n_dep, sole_prop, wages_pre = wages, eitc_pre = eitc) %>% 
  bind_cols(behavior %>% 
              select(wages_post = wages, eitc_post = eitc)) %>% 
  filter(eitc_pre != eitc_post)

# Filers with sole prop....duh! 
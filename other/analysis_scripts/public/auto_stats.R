
detail = bind_rows(
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061106/baseline/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_law', scenario = 'baseline'), 
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061106/above/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_law', scenario = 'above'), 
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061106/item/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_law', scenario = 'item'), 
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061107/tcja_ext/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_policy', scenario = 'baseline'), 
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061107/above/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_policy', scenario = 'above'), 
  read_csv('/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202505061107/item/static/detail/2026.csv') %>% 
    mutate(baseline = 'current_policy', scenario = 'item')
)

detail %>% 
  select(baseline_definition = baseline, name = scenario, id, weight, value = liab_iit_net) %>% 
  pivot_wider() %>% 
  mutate(
    item = item - baseline, above = above - baseline
  ) %>% 
  group_by(baseline_definition) %>%
  summarise(
    across(
      .cols = c(above, item), 
      .fns  = ~ weighted.mean(. < 0, weight)
    )
  )


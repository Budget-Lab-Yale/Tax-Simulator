#------------------------------------
# ctc_age_experiment.R
#
# TODO
#------------------------------------


library(tidyverse)
library(data.table)
library(Hmisc)



output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202503291057'



# Read tax unit detail files
detail = c('no_refund_limit', '0', '5', '16') %>% 
  map(
    .f = ~ file.path(output_root, .x, 'static/detail/2026.csv') %>% 
      fread() %>%
      tibble() %>% 
      mutate(scenario = .x, .before = everything())
  ) %>% 
  bind_rows() %>% 
  filter(dep_status == 0, n_dep_ctc > 0)


# Put data on child basis
child_detail = detail %>% 
  select(scenario, id, weight, dep_age1, dep_age2, dep_age3, parent_age1 = age1, parent_age2 = age2, agi) %>% 
  pivot_longer(
    cols            = starts_with('dep_age'), 
    names_prefix    = 'dep_age', 
    names_transform = as.integer, 
    names_to        = 'child_index', 
    values_to       = 'child_age'
  ) %>% 
  filter(!is.na(child_age), child_age < 17)


# Plot median AGI by age
child_detail %>%
  filter(scenario == 'no_refund_limit') %>% 
  group_by(child_age) %>%
  summarise(
    n_children      = sum(weight),
    median_agi      = wtd.quantile(agi, weight, 0.5),
    share_below_50k = sum(weight * (agi < 50e3)) / sum(weight),
    .groups = 'drop'
  ) %>%
  arrange(child_age) %>% 
  ggplot(aes(x = child_age, y = share_below_50k)) + 
  geom_point()



detail %>% 
  select(name = scenario, id, weight, expanded_inc, value = liab_iit_net) %>% 
  pivot_wider() %>% 
  mutate(
    across(
      .cols = c(`0`, `5`, `16`), 
      .fns  = ~ . - no_refund_limit
    )
  ) %>% 
  filter(expanded_inc > 0) %>% 
  arrange(expanded_inc) %>%
  group_by(
    income_group = pmin(5, floor(1 + (cumsum(weight) / sum(weight)) * 5))
  ) %>% 
  summarise(
    across(
      .cols = c(`0`, `5`, `16`),
      .fns  = list(
        share = ~ sum(. * weight) / 1e9,
        avg   = ~ weighted.mean(., weight), 
        etr   = ~ weighted.mean(. / expanded_inc, weight)
      ), 
      .names = '{fn}_{col}'
    ), 
    .group = 'drop'
  ) %>% 
  mutate(
    share_0  = share_0  / sum(share_0),
    share_5  = share_5  / sum(share_5),
    share_16 = share_16 / sum(share_16) 
  )


# Different framing...
# the lack of refundability is a tax on YOUNGER parents 
# show parent and child impacts of lifting refundability
# you'll never show what you want because refundability is actually the issue
# in the way
# thesis:
# - CTC is fundamentally a social insurance program
#   - social insurance helps smooth income across life events which impact income
#   - one of those events is having a kid
# - lack of refundability + phasein means limited benefits for those under $X
# - poorer parents are younger on average due to lifecyle effects and labor market disruptions associated with raising a kid
#    - cite statistics on disrupted earnings for moms 
#    - show how many poor parents of young kids will be not veyr poor next year using ASEC
# - therefore lack of refundability + phasein means that CTC's social insurance function is being limited
# - this contrasts with the standard/static view of just some people being poor forever
# - we know that income matters most at a young age for kids too for outcomes


output_root = '/vast/palmer/scratch/sarin/jar335/model_data/Tax-Simulator/v1/202503291241'



# Read tax unit detail files
detail = c('tcja_ext', 'refundability') %>% 
  map(
    .f = ~ file.path(output_root, .x, 'static/detail/2026.csv') %>% 
      fread() %>%
      tibble() %>% 
      mutate(scenario = .x, .before = everything())
  ) %>% 
  bind_rows() %>% 
  filter(dep_status == 0, n_dep_ctc > 0)


effects = detail %>% 
  mutate(age     = if_else(is.na(age2), age1, (age1 + age2) / 2)) %>% 
  select(id, name = scenario, age, starts_with('dep_age'), weight, expanded_inc, value = liab_iit_net) %>% 
  pivot_wider() %>% 
  mutate(benefit = tcja_ext - refundability) 

effects %>% 
  filter(age <= 65) %>% 
  group_by(age) %>% 
  summarise(
    avg = weighted.mean(benefit, weight), 
    etr = sum(benefit * weight) / sum(expanded_inc * weight), 
  ) %>% 
  ggplot(aes(x = age, y = avg)) +
  geom_point()

effects %>% 
  pivot_longer(
    cols            = starts_with('dep_age'), 
    names_prefix    = 'dep_age', 
    names_transform = as.integer, 
    names_to        = 'child_index', 
    values_to       = 'child_age'
  ) %>% 
  filter(!is.na(child_age), child_age < 17) %>%
  group_by(
    child_age 
  ) %>% 
  summarise(
    avg = weighted.mean(benefit, weight), 
    etr = sum(benefit * weight) / sum(expanded_inc * weight),
    share_benefiting = weighted.mean(benefit > 0, weight)
  ) %>% 
  ggplot(aes(x = child_age, y = share_benefiting)) +
  geom_point()
  
  effects %>% 
    summarise(sum(weight * benefit) / 1e9)





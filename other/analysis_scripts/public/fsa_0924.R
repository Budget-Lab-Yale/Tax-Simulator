library(tidyverse)
library(data.table)
library(Hmisc)

current_law_output_root    = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202409201536'
current_policy_output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202409201541'
tax_data_output_root       = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024091614'
  

#-----------------------
# Read and process data
#-----------------------

# Read input tax data
tax_data = tax_data_output_root %>% 
  file.path('/baseline/tax_units_2026.csv') %>% 
  fread() %>%
  tibble() %>% 
  mutate(year = 2026)


# Define function to calculate deltas from specified baseline
get_tax_change_microdata = function(year, baseline_id, baseline_name, output_root) {
  
  baseline = output_root %>% 
    file.path(baseline_id, paste0('/static/detail/', year, '.csv')) %>% 
    fread() %>% 
    tibble() %>% 
    mutate(year = year)
  
  new = output_root %>% 
    file.path(paste0('/salt/static/detail/', year, '.csv')) %>% 
    fread() %>% 
    tibble() %>% 
    mutate(eitc_fsa = eitc, ctc_fsa = ctc_nonref + ctc_ref) %>% 
    select(New = liab_iit_net, ctc_fsa, eitc_fsa)  
  
  old = output_root %>% 
    file.path(paste0('/old/static/detail/', year, '.csv')) %>% 
    fread() %>% 
    tibble() %>% 
    select(Old = liab_iit_net)
  
  alt = bind_cols(new, old)

  baseline %>%
    left_join(
      tax_data %>% 
        select(year, id, starts_with('dep_ssn')), 
      by = c('year', 'id')
    ) %>% 
    mutate(
      baseline = baseline_name,
      ctc_baseline = ctc_nonref + ctc_ref,
      n_kids = as.integer(!is.na(dep_age1) & dep_age1 < 18) + 
               as.integer(!is.na(dep_age2) & dep_age2 < 18) + 
               as.integer(!is.na(dep_age3) & dep_age3 < 18), 
      n_kids_ssn = as.integer(!is.na(dep_ssn1) & dep_ssn1) + 
                   as.integer(!is.na(dep_ssn2) & dep_ssn2) + 
                   as.integer(!is.na(dep_ssn3) & dep_ssn3)
    ) %>%
    select(year, baseline, id, dep_status, weight, filer, filing_status, n_kids, 
           n_kids_ssn, n_dep_ctc, wages, agi, ctc_baseline, eitc_baseline = eitc, liab_baseline = liab_iit_net) %>% 
    bind_cols(alt) %>% 
    filter(dep_status == 0) %>%
    pivot_longer(
      cols      = c(New, Old), 
      names_to  = 'reform', 
      values_to = 'liab'
    ) %>% 
    mutate(
      delta     = liab - liab_baseline, 
      tax_cut   = delta < -100, 
      tax_hike  = delta > 100, 
      no_change = !tax_cut & !tax_hike 
    ) %>% 
    return() 
}
  
tax_change_microdata = bind_rows(
  get_tax_change_microdata(2026, 'baseline', 'Current law',    current_law_output_root), 
  get_tax_change_microdata(2026, 'tcja',     'Current policy', current_policy_output_root),
)



#-------------------------
# Calculate summary stats
#-------------------------
  
# Table 2
tax_change_microdata %>%
  filter(n_kids > 0) %>% 
  group_by(baseline, reform) %>% 
  summarise(
    avg = round(weighted.mean(delta, weight) / 25) * 25,
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight) ,
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ round(weighted.mean(delta * ., weight) / 25) * 25
      )
    ), 
    .groups = 'drop'
  ) %>% 
  write.csv()

# Figure 1
tax_change_microdata %>%
  filter(n_kids > 0, agi < 100000) %>%
  mutate(Version = if_else(reform == 'New', 'September version', 'July version')) %>% 
  ggplot(aes(x = delta, fill = Version, colour = Version)) + 
  geom_density(alpha = 0.4, bw = 500) + 
  facet_wrap(~baseline) + 
  geom_vline(xintercept = 0) + 
  theme_bw() +
  scale_x_continuous(labels = scales::dollar_format(), limits = c(-7500, 5000)) + 
  labs(y = element_blank(), x = 'Tax change') + 
  theme(axis.text.y = element_blank(), ) 


# Figure 2
tax_change_microdata %>%
  filter(n_kids > 0, filing_status == 2) %>% 
  mutate(Version = if_else(reform == 'New', 'September version', 'July version')) %>% 
  group_by(
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    baseline, 
    Version
  ) %>% 
  summarise(
    avg = round(weighted.mean(delta, weight) / 25) * 25,
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight) ,
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ round(weighted.mean(delta * ., weight) / 25) * 25
      )
    ), 
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, colour = Version)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~baseline) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', colour = 'Version') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + 
  ggtitle('Married')


tax_change_microdata %>%
  filter(n_kids > 0, filing_status != 2) %>% 
  mutate(Version = if_else(reform == 'New', 'September version', 'July version')) %>% 
  group_by(
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    baseline, 
    Version
  ) %>% 
  summarise(
    avg = round(weighted.mean(delta, weight) / 25) * 25,
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight) ,
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ round(weighted.mean(delta * ., weight) / 25) * 25
      )
    ), 
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, colour = Version)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~baseline) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', colour = 'Version') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + 
  ggtitle('Unmarried')


tax_change_microdata %>%
  filter(n_kids > 0) %>% 
  mutate(Version = if_else(reform == 'New', 'September version', 'July version')) %>% 
  group_by(
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    baseline, 
    Version
  ) %>% 
  summarise(
    avg = round(weighted.mean(delta, weight) / 25) * 25,
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = avg, colour = Version)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~baseline) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Average tax change', colour = 'Version') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) + 
  scale_y_continuous(labels = scales::label_dollar()) +
  ggtitle('Parents')

tax_change_microdata %>%
  filter(n_kids == 0) %>% 
  mutate(Version = if_else(reform == 'New', 'September version', 'July version')) %>% 
  group_by(
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    baseline, 
    Version
  ) %>% 
  summarise(
    avg = round(weighted.mean(delta, weight) / 25) * 25,
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = avg, colour = Version)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~baseline) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Average tax change', colour = 'Version') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) +
  scale_y_continuous(breaks = seq(0, 750, 250), limits = c(-250, 750), labels = scales::label_dollar()) +
  ggtitle('Non-parents')






current_law_output_root    = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202407221816'
current_policy_output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202407221819'
current_law_23_output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202407221818'

current_law_options_output_root    = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202407231129'
current_policy_options_output_root = '/vast/palmer/scratch/sarin/jar335/Tax-Simulator/v1/202407231131'

tax_data_output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024070817'
  

#-----------------------
# Read and process data
#-----------------------

# Read input tax data
tax_data = bind_rows(
  tax_data_output_root %>% 
    file.path('/baseline/tax_units_2023.csv') %>% 
    fread() %>%
    tibble() %>% 
    mutate(year = 2023), 
  tax_data_output_root %>% 
    file.path('/baseline/tax_units_2026.csv') %>% 
    fread() %>%
    tibble() %>% 
    mutate(year = 2026)
)


# Define function to calculate deltas from specified baseline
get_tax_change_microdata = function(year, options, baseline_id, baseline_name, output_root) {
  
  baseline = output_root %>% 
    file.path(baseline_id, paste0('/static/detail/', year, '.csv')) %>% 
    fread() %>% 
    tibble() %>% 
    mutate(year = year)
  
  if (!options) { 
    ctc = output_root %>% 
      file.path(paste0('/ctc/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>%
      select(`1) CTC` = liab_iit_net)
    eitc = output_root %>% 
      file.path(paste0('/eitc/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>% 
      select(`2) EITC` = liab_iit_net)
    hoh = output_root %>% 
      file.path(paste0('/hoh/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>% 
      select(`3) Head of Household` = liab_iit_net)
    other = output_root %>% 
      file.path(paste0('/salt/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>% 
      mutate(eitc_fsa = eitc, ctc_fsa = ctc_nonref + ctc_ref) %>% 
      select(`4) Other` = liab_iit_net, ctc_fsa, eitc_fsa)  
    alt = bind_cols(ctc, eitc, hoh, other)
  } else {
    as_written = output_root %>% 
      file.path(paste0('/as_written/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>%
      select(`As written` = liab_iit_net)
    option25 = output_root %>% 
      file.path(paste0('/option25/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>%
      select(`25% increase` = liab_iit_net)
    option50 = output_root %>% 
      file.path(paste0('/option50/static/detail/', year, '.csv')) %>% 
      fread() %>% 
      tibble() %>%
      select(`50% increase` = liab_iit_net)
    alt = bind_cols(as_written, option25, option50)
  }
  
  
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
      cols      = contains(' '), 
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
  get_tax_change_microdata(2026, F, 'baseline', 'Current law, 2026',    current_law_output_root), 
  get_tax_change_microdata(2026, F, 'tcja',     'Current policy, 2026', current_policy_output_root),
  get_tax_change_microdata(2023, F, 'baseline', 'Current policy, 2023', current_law_23_output_root)
)

tax_change_microdata_options = bind_rows(
  get_tax_change_microdata(2026, T, 'baseline', 'Current law, 2026',    current_law_options_output_root), 
  get_tax_change_microdata(2026, T, 'tcja',     'Current policy, 2026', current_policy_options_output_root),
)


#-------------------------
# Calculate summary stats
#-------------------------
  
# Table 1 
tax_change_microdata %>%
  filter(reform == '4) Other', n_kids > 0) %>% 
  group_by(baseline, ) %>% 
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


tax_change_microdata %>% 
  filter(reform == '4) Other', n_kids > 0) %>% 
  group_by(baseline) %>% 
  summarise(
    across(
      .cols = delta, 
      .fns  = list(
        `10`   = ~ weighted.mean(delta > 10, weight),
        `100`  = ~ weighted.mean(delta > 100, weight),
        `250`  = ~ weighted.mean(delta > 250, weight),
        `500`  = ~ weighted.mean(delta > 500, weight),
        `1000` = ~ weighted.mean(delta > 1000, weight)
      ), 
      .names = '{fn}'
    )
  ) %>% 
  pivot_longer(cols = -baseline, names_transform = as.integer) %>% 
  ggplot(aes(x = name, y = value, colour = baseline)) + 
  geom_point(size = 4) + 
  geom_line() + 
  geom_label(aes(label = round(value, 2)), show.legend = F) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +  
  scale_x_continuous(breaks = c(10, 100, 250, 500, 1000), labels = scales::label_dollar()) + 
  labs(x = 'Tax hike threshold', y = 'Share of families', colour = 'Baseline') + 
  ggtitle('Figure 1. Sensitivity to definition of tax hike')


# Decision tree stats 
get_share_with_hike = function(b, ...) {
  tax_change_microdata %>% 
    filter(
      baseline == b, 
      reform == '4) Other', 
      n_kids > 0
    ) %>% 
    group_by(...) %>% 
    summarise(
      across(
        .cols = c(tax_cut, tax_hike, no_change), 
        .fns  = list(
          p      = ~ weighted.mean(., weight),
          n_kids = ~ sum(. * n_kids * weight) / 1e6,
          avg    = ~ weighted.mean(delta * ., weight)
        )
      ), 
      .groups = 'drop'
    )
}

get_share_with_hike('Current policy, 2026')
get_share_with_hike('Current policy, 2026', married = filing_status == 2)
get_share_with_hike('Current policy, 2026', married = filing_status == 2, agi < 50000)
get_share_with_hike('Current policy, 2026', married = filing_status == 2, agi < 50000, agi < 25000)

# Contribution charts
stacked_stats = tax_change_microdata %>% 
  filter(
    n_kids > 0, 
    agi >= 0
  ) %>% 
  group_by(
    baseline, 
    married = if_else(filing_status == 2, 'Married', 'Unmarried'), 
    income  = cut(agi, seq(0, 200000, 10000), labels = seq(0, 190000, 10000)) %>% as.character() %>% as.numeric(), 
    reform
  ) %>% 
  summarise(
    avg = weighted.mean(delta, weight),
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight),
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ weighted.mean(delta * ., weight)
      )
    ), 
    .groups = 'drop'
  ) %>% 
  group_by(baseline, married, income) %>% 
  mutate(
    across(
      .cols = -reform, 
      .fns  = ~ if_else(row_number() > 1, . - lag(.), .)
    )
  ) %>% 
  ungroup()

stacked_stats %>% 
  filter(baseline == 'Current law, 2026') %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, fill = reform)) + 
  geom_col() +
  geom_hline(yintercept = 0) + 
  facet_wrap(~married) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', fill = 'Provision') + 
  scale_x_continuous(breaks = seq(0, 200, 25), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) + 
  ggtitle('Figure 2. Contribution to Share of Families with Tax Hike, 2026 Current Law Baseline')

stacked_stats %>% 
  filter(baseline == 'Current policy, 2026') %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, fill = reform)) + 
  geom_col() +
  geom_hline(yintercept = 0) + 
  facet_wrap(~married) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', fill = 'Provision') + 
  scale_x_continuous(breaks = seq(0, 200, 25), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) + 
  ggtitle('Figure 3. Contribution to Share of Families with Tax Hike, 2026 Current Policy Baseline')


#----------------------
# Options calculations
#----------------------

# Budget cost
tax_change_microdata_options %>% 
  group_by(baseline, reform) %>% 
  summarise(value = sum(delta * weight) / 1e9) %>% 
  pivot_wider(names_from = reform) %>% 
  mutate(across(.cols = contains('%'), 
                .fns  = ~ . - `As written`))

# Basic stats
tax_change_microdata_options %>% 
  filter(n_kids > 0) %>% 
  group_by(baseline, reform) %>% 
  summarise(
    across(
      .cols = tax_hike, 
      .fns  = list(
        p      = ~ weighted.mean(., weight),
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ weighted.mean(delta * ., weight)
      )
    )
  )

tax_change_microdata_options %>% 
  filter(n_kids > 0) %>% 
  group_by(baseline, reform, agi < 50000) %>% 
  summarise(
    across(
      .cols = tax_hike, 
      .fns  = list(
        p      = ~ weighted.mean(., weight),
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ weighted.mean(delta * ., weight)
      )
    )
  ) %>% 
  View()

  
  
# Income charts
tax_change_microdata_options %>% 
  filter(
    baseline == 'Current law, 2026',
    n_kids > 0, 
    agi >= 0
  ) %>% 
  group_by(
    baseline, 
    married = if_else(filing_status == 2, 'Married', 'Unmarried'), 
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    reform
  ) %>% 
  summarise(
    avg = weighted.mean(delta, weight),
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight),
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ weighted.mean(delta * ., weight)
      )
    ), 
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, colour = reform)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~married) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', colour = 'EITC design') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + 
  scale_colour_manual(values = c('#83c5be', '#e29578', '#343a40')) + 
  ggtitle('Figure 5. Share of Families with Tax Hike under EITC Options, 2026 Current Law Baseline')

tax_change_microdata_options %>% 
  filter(
    baseline == 'Current policy, 2026',
    n_kids > 0, 
    agi >= 0
  ) %>% 
  group_by(
    baseline, 
    married = if_else(filing_status == 2, 'Married', 'Unmarried'), 
    income  = cut(agi, seq(0, 100000, 5000), labels = seq(0, 95000, 5000)) %>% as.character() %>% as.numeric(), 
    reform
  ) %>% 
  summarise(
    avg = weighted.mean(delta, weight),
    across(
      .cols = c(tax_cut, tax_hike, no_change), 
      .fns  = list(
        p      = ~ weighted.mean(., weight),
        n_fam  = ~ sum(. * weight)          / 1e6,
        n_kids = ~ sum(. * n_kids * weight) / 1e6,
        avg    = ~ weighted.mean(delta * ., weight)
      )
    ), 
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = income / 1000, y = tax_hike_p, colour = reform)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~married) +
  theme_bw() + 
  labs(x = 'AGI bin (thousands of dollars)', y = 'Share of families', colour = 'EITC design') + 
  scale_x_continuous(breaks = seq(0, 100, 10), labels = scales::label_dollar()) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) + 
  scale_colour_manual(values = c('#83c5be', '#e29578', '#343a40')) + 
  ggtitle('Figure 6. Share of Families with Tax Hike under EITC Options, 2026 Current Policy Baseline')


################################################################################
# Profile the bottom-quintile (expanded income) tax units that face a tax HIKE
# (>$100) under the Clausing-Sarin hybrid. Replicates the distribution.R income
# measure / quintile assignment, isolates Q1 hike units, decomposes the source
# of the increase, and prints concrete example records.
################################################################################

library(tidyverse)
library(data.table)

vintage  = Sys.getenv('CLAUSING_VINTAGE',  'clausing_2039_surprise')
year     = as.integer(Sys.getenv('CLAUSING_YEAR', '2030'))
scenario = Sys.getenv('CLAUSING_SCENARIO', '06_niit_reform')
root     = file.path('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1', vintage)

keep = c('id','weight','dep_status','filing_status','age1','age2','n_dep',
         'wages','txbl_int','exempt_int','div_ord','div_pref','kg_st','kg_lt',
         'sole_prop','sch_e','farm','part_scorp','gross_ss','txbl_ss',
         'agi','expanded_inc','txbl_inc','qbi_ded',
         'liab_ord','liab_pref','liab_niit','liab_iit_net','liab_pr')

base = fread(file.path(root, 'baseline', 'static/detail', paste0(year, '.csv')),
             select = keep) %>% tibble() %>% filter(dep_status == 0)
ref  = fread(file.path(root, scenario, 'static/detail', paste0(year, '.csv')),
             select = c('id','liab_ord','liab_pref','liab_niit','liab_iit_net','liab_pr')) %>%
       tibble() %>% rename_with(~ paste0(.x, '_r'), -id)

d = base %>%
  left_join(ref, by = 'id') %>%
  mutate(
    income      = expanded_inc,
    liab_iit_pr = liab_iit_net + liab_pr,
    liab_iit_pr_r = liab_iit_net_r + liab_pr_r,
    liab_delta  = liab_iit_pr_r - liab_iit_pr,
    d_ord  = liab_ord_r  - liab_ord,
    d_pref = liab_pref_r - liab_pref,
    d_niit = liab_niit_r - liab_niit,
    d_pr   = liab_pr_r   - liab_pr,
    # residual captures credit / QBI / AMT / netting effects flowing through liab_iit_net
    d_other = liab_delta - (d_ord + d_pref + d_niit + d_pr)
  ) %>%
  # Expanded-income quintile, exactly as distribution.R (income >= 0 only)
  arrange(income) %>%
  mutate(
    pctile = cumsum(weight * (income >= 0)) / sum(weight * (income >= 0)),
    pctile = if_else(income < 0, NA_real_, pctile),
    quintile = case_when(
      pctile <= 0.2 ~ 'Q1', pctile <= 0.4 ~ 'Q2', pctile <= 0.6 ~ 'Q3',
      pctile <= 0.8 ~ 'Q4', pctile <= 1   ~ 'Q5')
  )

q1     = d %>% filter(quintile == 'Q1')
q1_top = q1 %>% summarise(cut = round(max(income))) %>% pull(cut)
hike   = q1 %>% filter(liab_delta >= 100)

cat('\n==============================================================\n')
cat('Clausing hybrid -', scenario, '| year', year, '| vintage', vintage, '\n')
cat('==============================================================\n')
cat('Q1 = bottom expanded-income quintile. Top of Q1 income: $', q1_top, '\n', sep='')
cat('Q1 total tax units (wtd):   ', format(round(sum(q1$weight)), big.mark=','), '\n')
cat('Q1 with hike >$100 (wtd):   ', format(round(sum(hike$weight)), big.mark=','),
    '  (', round(100*sum(hike$weight)/sum(q1$weight), 2), '% of Q1)\n', sep='')

cat('\n--- Average hike decomposition among Q1 hike units ($/unit, wtd) ---\n')
hike %>% summarise(
  avg_total = weighted.mean(liab_delta, weight),
  pref      = weighted.mean(d_pref, weight),
  niit      = weighted.mean(d_niit, weight),
  ord       = weighted.mean(d_ord,  weight),
  payroll   = weighted.mean(d_pr,   weight),
  other_qbi_cred = weighted.mean(d_other, weight)
) %>% mutate(across(everything(), ~round(.x))) %>% print(width = Inf)

cat('\n--- Share of Q1 hike units whose increase is PRIMARILY from each source ---\n')
hike %>%
  mutate(driver = case_when(
    d_pref  >= pmax(d_niit, d_ord, d_pr, d_other) ~ 'pref rate (0->5% / +5pp)',
    d_niit  >= pmax(d_pref, d_ord, d_pr, d_other) ~ 'NIIT on active',
    d_ord   >= pmax(d_pref, d_niit, d_pr, d_other) ~ 'ordinary rates',
    d_other >= pmax(d_pref, d_niit, d_ord, d_pr)  ~ 'QBI repeal / credits',
    TRUE ~ 'payroll/other')) %>%
  group_by(driver) %>% summarise(wtd = sum(weight), .groups='drop') %>%
  mutate(share = round(100*wtd/sum(wtd), 1)) %>% arrange(desc(wtd)) %>% print(width = Inf)

cat('\n--- Income composition: Q1 hike units vs all of Q1 (wtd means, $) ---\n')
comp = function(df, lbl) df %>% summarise(
  group=lbl, n_wtd=round(sum(weight)),
  expanded_inc=round(weighted.mean(expanded_inc, weight)),
  agi=round(weighted.mean(agi, weight)),
  wages=round(weighted.mean(wages, weight)),
  kg_lt=round(weighted.mean(kg_lt, weight)),
  div_pref=round(weighted.mean(div_pref, weight)),
  gross_ss=round(weighted.mean(gross_ss, weight)),
  part_scorp=round(weighted.mean(part_scorp, weight)),
  sole_prop=round(weighted.mean(sole_prop, weight)))
bind_rows(comp(hike,'Q1 hike'), comp(q1,'all Q1')) %>% print(width = Inf)

cat('\n--- Pct of Q1 hike units with positive amounts of each component ---\n')
hike %>% summarise(
  has_kg_lt   = round(100*sum(weight*(kg_lt>0))/sum(weight),1),
  has_div_pref= round(100*sum(weight*(div_pref>0))/sum(weight),1),
  has_part_scorp= round(100*sum(weight*(part_scorp>0))/sum(weight),1),
  has_ss      = round(100*sum(weight*(gross_ss>0))/sum(weight),1),
  has_wages   = round(100*sum(weight*(wages>0))/sum(weight),1)
) %>% print(width = Inf)

cat('\n--- 12 representative Q1 hike records (largest-weight) ---\n')
hike %>% arrange(desc(weight)) %>% head(12) %>%
  transmute(
    fs = filing_status, age1, n_dep,
    exp_inc = round(expanded_inc), agi = round(agi),
    wages = round(wages), kg_lt = round(kg_lt), div_pref = round(div_pref),
    ss = round(gross_ss), part_scorp = round(part_scorp),
    hike = round(liab_delta),
    via_pref = round(d_pref), via_niit = round(d_niit), via_other = round(d_other)
  ) %>% print(n = Inf, width = Inf)

cat('\nDone.\n')

#-------------------------------------------------------------------------
# Compares cumulative-fix baseline runs to attribute revenue impact to each
# 2026-07-01 calc-layer bug fix, plus pre/post reform-score comparison.
#
# Variants (cumulative commits):
#   c0 = pre-fix, c2 = +#4(pr_ee)+#6(magi_ss), c3 = +#5(1250/collect),
#   c4 = +#3(cdctc), c5 = +#8(dep std ded) = post-all
#-------------------------------------------------------------------------

library(tidyverse)

root     = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
variants = c('c0', 'c2', 'c3', 'c4', 'c5')
pairs    = tribble(
  ~from, ~to,  ~fix,
  'c0',  'c2', '#6 magi_ss exempt_int (+#4 pr_ee, revenue-neutral)',
  'c2',  'c3', '#5 1250/collectibles stacking',
  'c3',  'c4', '#3 CDCTC shared earned-income cap',
  'c4',  'c5', '#8 dependent std ded bonus'
)

read_totals = function(v, file) {
  read_csv(file.path(root, paste0('calcfix_', v), 'baseline/static/totals', file),
           show_col_types = FALSE) %>%
    mutate(variant = v)
}

#--------------------------------------------------
# 1. Baseline aggregate deltas between consecutive variants
#--------------------------------------------------

t1040 = map_df(variants, read_totals, '1040.csv')
tpr   = map_df(variants, read_totals, 'payroll.csv')
trec  = map_df(variants, read_totals, 'receipts.csv')

key_1040 = c('txbl_ss', 'n_txbl_ss', 'std_ded', 'txbl_inc', 'liab_1250',
             'liab_collect', 'liab_pref', 'liab_ord', 'cdctc_nonref',
             'n_cdctc_nonref', 'cdctc_ref', 'eitc', 'refund',
             'liab_iit', 'liab_iit_net')
key_pr   = c('liab_pr', 'liab_pr_ee', 'liab_pr_er', 'liab_add_med')
key_rec  = c('revenues_income_tax', 'revenues_payroll_tax', 'outlays_tax_credits')

wide = t1040 %>% select(variant, year, all_of(key_1040)) %>%
  left_join(tpr  %>% select(variant, year, all_of(key_pr)),  by = c('variant', 'year')) %>%
  left_join(trec %>% select(variant, year, all_of(key_rec)), by = c('variant', 'year'))

cat('==================== LEVELS BY VARIANT (baseline, $B) ====================\n')
wide %>% arrange(year, match(variant, variants)) %>% print(n = Inf, width = Inf)

cat('\n==================== MARGINAL DELTA PER FIX (to - from, $B) ====================\n')
deltas = pairs %>%
  pmap_df(function(from, to, fix) {
    inner_join(
      wide %>% filter(variant == from) %>% select(-variant),
      wide %>% filter(variant == to)   %>% select(-variant),
      by = 'year', suffix = c('_a', '_b')
    ) %>%
      transmute(
        fix, year,
        across(.cols = ends_with('_b'),
               .fns  = ~ . - get(str_replace(cur_column(), '_b$', '_a')),
               .names = '{str_remove(.col, "_b$")}')
      )
  })
deltas %>% print(n = Inf, width = Inf)

write_csv(wide,   'other/calc_fixes/levels_by_variant.csv')
write_csv(deltas, 'other/calc_fixes/marginal_deltas.csv')

#--------------------------------------------------
# 2. Detail-level affected-record counts (2026, per consecutive pair)
#--------------------------------------------------

cat('\n==================== AFFECTED TAX UNITS, 2026 ====================\n')
read_detail = function(v) {
  read_csv(file.path(root, paste0('calcfix_', v), 'baseline/static/detail/2026.csv'),
           col_select = c(id, weight, liab_iit_net, txbl_ss, std_ded,
                          cdctc_nonref, cdctc_ref, liab_pr_ee),
           show_col_types = FALSE)
}
details = set_names(variants) %>% map(read_detail)

pairs %>%
  pmap_df(function(from, to, fix) {
    d = inner_join(details[[from]], details[[to]], by = 'id', suffix = c('_a', '_b'))
    stopifnot(nrow(d) == nrow(details[[from]]))
    d %>%
      mutate(chg = abs(liab_iit_net_b - liab_iit_net_a) > 0.005) %>%
      summarise(
        fix                 = fix,
        n_affected_millions = sum(weight_a * chg) / 1e6,
        delta_iit_net_B     = sum(weight_a * (liab_iit_net_b - liab_iit_net_a)) / 1e9,
        mean_delta_affected = sum(weight_a * (liab_iit_net_b - liab_iit_net_a)) /
                              pmax(1, sum(weight_a * chg)),
        delta_pr_ee_B       = sum(weight_a * (liab_pr_ee_b - liab_pr_ee_a)) / 1e9
      )
  }) %>%
  print(width = Inf)

#--------------------------------------------------
# 3. Reform score comparison (c0 vs c5)
#--------------------------------------------------

cat('\n==================== REFORM REVENUE ESTIMATES, PRE VS POST ($B) ====================\n')
for (s in c('cdctc_test', 'kg_top_5pp', 'sd_bump_10k')) {
  for (rt in c('static', 'conventional')) {
    f0 = file.path(root, 'calcfix_c0', s, rt, 'supplemental/revenue_estimates.csv')
    f5 = file.path(root, 'calcfix_c5', s, rt, 'supplemental/revenue_estimates.csv')
    if (!file.exists(f0) | !file.exists(f5)) next
    cmp = full_join(
      read_csv(f0, show_col_types = FALSE) %>% rename(pre_fix = total),
      read_csv(f5, show_col_types = FALSE) %>% rename(post_fix = total),
      by = 'year'
    ) %>%
      mutate(scenario = s, run_type = rt, delta = post_fix - pre_fix,
             pct_chg = delta / abs(pre_fix) * 100)
    print(cmp, width = Inf)
    write_csv(cmp, sprintf('other/calc_fixes/reform_cmp_%s_%s.csv', s, rt))
  }
}

cat('\nCOMPARE DONE\n')

# NJ TAXSIM 2019 (2026-10-01): misses where OUR NJ income is below TAXSIM's -- which federal income is missing from ours?
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
r = fread('research/state_tax/cross_model/cache_sw/nj_run/raw/taxsim_2019.csv')
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & abs_diff > 100 & st_agi < v32_state_agi - 100]
f = as.data.table(readRDS('research/state_tax/cross_model/cache/fed_calc_2019.rds')$tax_units)[, .(id, net_estate, estate, estate_loss, part, scorp, part_scorp, sole_prop, farm, net_rent, other_gains, txbl_ss, ui, txbl_pens_dist, txbl_ira_dist, wages, txbl_int, div_ord, div_pref, kg_st, kg_lt, alimony, other_inc)]
d = merge(r[, .(id, agi, st_agi, v32_state_agi, st_retirement_excl, st_subtractions)], f, by = 'id')
d[, short := v32_state_agi - st_agi]
cat(sprintf('%d misses with ours below TAXSIM; median shortfall %.0f\n', nrow(d), median(d$short)))
for (v in c('net_estate', 'part', 'scorp', 'sole_prop', 'farm', 'net_rent', 'other_gains', 'txbl_pens_dist', 'txbl_ira_dist'))
  cat(sprintf('  %-15s positive on %.2f; shortfall within 5%% of it on %.2f of those\n', v, mean(d[[v]] > 1), mean(abs(d$short[d[[v]] > 1] - d[[v]][d[[v]] > 1]) <= 0.05 * d[[v]][d[[v]] > 1])))
cat('  shortfall within 5% of net_estate + positive other pieces? estate share of shortfall (median, estate>0):', round(median((d$net_estate / d$short)[d$net_estate > 1]), 3), '\n')

# NJ TAXSIM 2018 (2026-10-01): which positive income is in our NJ AGI but not TAXSIM's, on NOL-flagged misses?
suppressPackageStartupMessages({ library(data.table) }); options(width = 230)
r = fread('research/state_tax/cross_model/cache_sw/nj_run/raw/taxsim_2018.csv')
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & other_inc < -2000 & abs_diff > 100]
f = as.data.table(readRDS('research/state_tax/cross_model/cache/fed_calc_2018.rds')$tax_units)[, .(id, part, scorp, sole_prop, farm, net_rent, net_estate, other_gains, kg_st, kg_lt, txbl_int, div_ord, div_pref, alimony, txbl_pens_dist, txbl_ira_dist, part_se, sstb_part, sstb_scorp)]
d = merge(r[, .(id, agi, st_agi, v32 = v32_state_agi, other_inc, wages = ei1 + ei2)], f, by = 'id')
d[, g := st_agi - v32]
pos = function(x) pmax(0, x)
cands = list(part = pos(d$part), scorp = pos(d$scorp), sole_prop = pos(d$sole_prop), net_rent = pos(d$net_rent), net_estate = pos(d$net_estate),
             gains = pos(d$kg_st + d$kg_lt + d$other_gains), dividends = d$div_ord + d$div_pref, interest = d$txbl_int, pensions = d$txbl_pens_dist + d$txbl_ira_dist)
cat(sprintf('%d NOL-flagged misses; median gap (ours - v32) %.0f\n', nrow(d), median(d$g)))
for (k in names(cands)) { x = cands[[k]]; h = x > 1
  cat(sprintf('  %-10s positive on %.2f; gap within 2%% of it on %.2f of those\n', k, mean(h), if (any(h)) mean(abs(d$g[h] - x[h]) <= 0.02 * x[h]) else NaN)) }

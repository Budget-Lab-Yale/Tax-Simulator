# NJ TAXSIM (2026-10-01, after the net-loss and no-tax fixes): what is in (our NJ AGI - TAXSIM v32) on the misses?
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
yr = 2019
r = fread(sprintf('research/state_tax/cross_model/cache_sw/nj_run/raw/taxsim_%d.csv', yr))
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & !(other_inc < -2000)]
f = as.data.table(readRDS(sprintf('research/state_tax/cross_model/cache/fed_calc_%d.rds', yr))$tax_units)
f = f[, .(id, trad_contr_ira, keogh_contr, se_health, hsa_contr, sl_int_ded, other_above_ded, liab_seca, alimony_exp, txbl_kg, kg_st, kg_lt,
          other_gains, part, scorp, sole_prop, farm, net_rent, net_estate, estate, gross_ss, txbl_ss, ui, txbl_pens_dist, txbl_ira_dist,
          exempt_int, wages, gross_wages, state_ref)]
d = merge(r, f, by = 'id', suffixes = c('', '.f'))
d[, `:=`(g = st_agi - v32_state_agi, miss = abs_diff > 100)]
m = d[miss == TRUE]
cands = list(adj_all = m$trad_contr_ira + m$keogh_contr + m$se_health + m$hsa_contr + m$sl_int_ded + m$other_above_ded + m$liab_seca / 2,
             half_seca = m$liab_seca / 2, ira = m$trad_contr_ira, keogh = m$keogh_contr, se_health = m$se_health, hsa = m$hsa_contr,
             sl_int = m$sl_int_ded, other_above = m$other_above_ded, kg_floor = pmax(0, -m$txbl_kg), estate = m$net_estate,
             state_ref = m$state_ref, gross_minus_wages = m$gross_wages - m$wages)
cat(sprintf('NJ %d non-NOL misses: %d; (ours - v32) median %.0f, positive on %.2f\n', yr, nrow(m), median(m$g), mean(m$g > 100)))
for (k in names(cands)) { x = cands[[k]]
  cat(sprintf('  %-18s present on %.2f of misses; gap within $50 of it on %.2f of those; corr with gap %.2f\n', k, mean(abs(x) > 1),
      mean(abs(m$g[abs(x) > 1] - x[abs(x) > 1]) <= 50), suppressWarnings(cor(m$g, x)))) }
cat('  most common gaps:', head(names(sort(table(round(m$g)), decreasing = TRUE)), 10), '\n')
cat('  gap / wages, median on misses with wages > 0:', round(median((m$g / m$wages)[m$wages > 0]), 4), '\n')

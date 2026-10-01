# NJ TAXSIM 2019 (2026-10-01): implied TAXSIM deduction vs ours on the misses
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
r = fread('research/state_tax/cross_model/results/raw/taxsim_2019.csv',
          select = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','itemizing','st_agi','st_exempt','st_ded','st_txbl_inc',
                     'v32_state_agi','v33_state_exemption_amount','v36_state_taxable_income','other_inc','filing_status','n_dep','age1'))
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & !(other_inc < -2000)]
f = as.data.table(readRDS('research/state_tax/cross_model/cache/fed_calc_2019.rds')$tax_units)[, .(id, salt_prop, med_item_ded_potential, alimony_exp)]
d = merge(r, f, by = 'id')
d[, `:=`(tx_ded = v32_state_agi - v33_state_exemption_amount - v36_state_taxable_income, miss = abs_diff > 100)]
cat(sprintf('salt_prop > 0: federal itemizers %.2f, non-itemizers %.2f\n', mean(d$salt_prop[d$itemizing] > 0), mean(d$salt_prop[!d$itemizing] > 0)))
m = d[miss == TRUE]
cat(sprintf('misses %d: TAXSIM implied deduction median %.0f (nonzero %.2f) | ours median %.0f (nonzero %.2f)\n', nrow(m), median(m$tx_ded), mean(m$tx_ded > 1), median(m$st_ded), mean(m$st_ded > 1)))
cat(sprintf('  implied - ours: median %.0f; within $100 on %.2f\n', median(m$tx_ded - m$st_ded), mean(abs(m$tx_ded - m$st_ded) <= 100)))
cat('  most common (TAXSIM implied - ours):', head(names(sort(table(round(m$tx_ded - m$st_ded)), decreasing = TRUE)), 8), '\n')
cat(sprintf('  exemption gap (ours - v33) nonzero %.2f; most common: %s\n', mean(abs(m$st_exempt - m$v33_state_exemption_amount) > 1),
    paste(head(names(sort(table(round(m$st_exempt - m$v33_state_exemption_amount)), decreasing = TRUE)), 6), collapse = ' ')))
cat(sprintf('  misses with medical potential > 0: %.2f; alimony paid > 0: %.3f\n', mean(m$med_item_ded_potential > 0), mean(m$alimony_exp > 0)))

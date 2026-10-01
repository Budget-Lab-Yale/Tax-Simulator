# NJ TAXSIM 2019 (2026-10-01): example misses where TAXSIM deducts much more than we do
suppressPackageStartupMessages({ library(data.table) }); options(width = 250)
r = fread('research/state_tax/cross_model/results/raw/taxsim_2019.csv',
          select = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','itemizing','st_ded','st_exempt','st_txbl_inc',
                     'v32_state_agi','v33_state_exemption_amount','v36_state_taxable_income','other_inc','filing_status','n_dep','age1','age2','care_exp'))
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & !(other_inc < -2000) & abs_diff > 100]
f = as.data.table(readRDS('research/state_tax/cross_model/cache/fed_calc_2019.rds')$tax_units)[, .(id, salt_prop, med_item_ded_potential, mort_int_item_ded_potential, char_item_ded_potential, item_ded, gross_ss, txbl_pens_dist)]
d = merge(r, f, by = 'id')
d[, tx_ded := v32_state_agi - v33_state_exemption_amount - v36_state_taxable_income]
x = d[tx_ded - st_ded > 5000]
cat(sprintf('%d misses where TAXSIM deducts > $5,000 more; salt_prop > 0 on %.2f; itemizers %.2f; age1 65+ %.2f\n', nrow(x), mean(x$salt_prop > 0), mean(x$itemizing), mean(x$age1 >= 65)))
cat('  (TAXSIM deduction) vs (fed AGI - TAXSIM AGI): correlation', round(cor(x$tx_ded, x$agi - x$v32_state_agi), 3), '\n')
print(head(x[order(-tx_ded), .(fs = filing_status, n_dep, age1, agi = round(agi), v32 = round(v32_state_agi), tx_ded = round(tx_ded), ours_ded = round(st_ded),
        v33 = v33_state_exemption_amount, our_ex = st_exempt, salt_prop = round(salt_prop), item_ded = round(item_ded), care = round(care_exp), diff = round(diff))], 15))

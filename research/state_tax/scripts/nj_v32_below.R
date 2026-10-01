# NJ TAXSIM 2019 (2026-10-01): misses where our NJ AGI ~ federal AGI but TAXSIM's v32 is below it
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
r = fread('research/state_tax/cross_model/results/raw/taxsim_2019.csv',
          select = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','st_agi','v32_state_agi','v33_state_exemption_amount','st_exempt',
                     'st_txbl_inc','v36_state_taxable_income','st_ded','age1','age2','filing_status','n_dep','gross_ss','txbl_pens_dist','txbl_ira_dist','ui','other_inc'))
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & !(other_inc < -2000)]
x = r[abs_diff > 100 & abs(st_agi - agi) <= 100 & v32_state_agi < agi - 100]
x[, below := agi - v32_state_agi]
cat(sprintf('misses with ours ~ fed AGI and TAXSIM below: %d of %d misses\n', nrow(x), sum(r$abs_diff > 100)))
cat('  most common (fed AGI - v32):', head(names(sort(table(round(x$below)), decreasing = TRUE)), 8), '\n')
cat(sprintf('  share 62+: %.2f | with pension/IRA dist: %.2f | with SS: %.2f | with UI: %.2f | median AGI %.0f\n',
    mean(x$age1 >= 62), mean(x$txbl_pens_dist + x$txbl_ira_dist > 0), mean(x$gross_ss > 0), mean(x$ui > 0), median(x$agi)))
cat(sprintf('  below == txbl_pens + txbl_ira (within $5): %.2f | below == pens+IRA+SS-type? see ratios\n', mean(abs(x$below - (x$txbl_pens_dist + x$txbl_ira_dist)) <= 5)))
cat('  our taxable inc vs TAXSIM on these: median', median(x$st_txbl_inc - x$v36_state_taxable_income), '| our NJ deduction median', median(x$st_ded), '\n')
cat('  by age group:\n'); print(x[, .(n = .N, med_below = median(below), med_pens = median(txbl_pens_dist + txbl_ira_dist)), by = .(age62 = age1 >= 62)])

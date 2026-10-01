# MN TAXSIM 2019: which stage carries the dependent-family misses? (2026-09-30)
suppressPackageStartupMessages(library(data.table)); options(width = 220)
cols = c('id','state','fed_aligned','excluded','abs_diff','diff','n_dep','agi','ei1','ei2','filing_status','care_exp',
         'st_agi','st_exempt','st_txbl_inc','st_tax_pre_credit','st_earned_credit','st_cdctc','st_marriage_credit',
         'st_credits_nonref','st_credits_ref','liab_st_iit',
         'v32_state_agi','v33_state_exemption_amount','v36_state_taxable_income','v38_state_child_care_credit',
         'v39_state_eitc','v40_state_total_credits')
yr = as.integer(commandArgs(trailingOnly = TRUE)[1])
r = fread(sprintf('research/state_tax/cross_model/results/raw/taxsim_%d.csv', yr), select = cols)
r = r[state == 'MN' & fed_aligned == TRUE & excluded == FALSE & n_dep > 0]
r[, miss := abs_diff > 100]
r[, `:=`(d_agi = st_agi - v32_state_agi, d_exempt = st_exempt - v33_state_exemption_amount,
         d_txbl = st_txbl_inc - v36_state_taxable_income, d_wfc = st_earned_credit - v39_state_eitc,
         d_care = st_cdctc - v38_state_child_care_credit,
         d_cred = (st_credits_nonref + st_credits_ref) - v40_state_total_credits)]
cat(sprintf('MN %d with dependents: n %d, misses %d (%.3f match)\n', yr, nrow(r), sum(r$miss), mean(!r$miss)))
st = function(x) sprintf('nonzero %.2f | median %.0f', mean(abs(x) > 1), median(x))
m = r[miss == TRUE]
for (v in c('d_agi','d_exempt','d_txbl','d_wfc','d_care','d_cred')) cat(sprintf('  misses %-9s %s   (matches: %s)\n', v, st(m[[v]]), st(r[miss == FALSE][[v]])))
cat('\nby number of dependents:\n'); print(r[, .(n = .N, match = round(mean(!miss), 3), med_d_exempt = median(d_exempt), med_d_wfc = median(d_wfc), med_d_care = median(d_care)), by = .(n_dep = pmin(n_dep, 3))][order(n_dep)])
cat('\nmost common exemption gaps among misses:', head(names(sort(table(round(m$d_exempt)), decreasing = TRUE)), 6), '\n')
cat('most common WFC gaps among misses:', head(names(sort(table(round(m$d_wfc)), decreasing = TRUE)), 6), '\n')
cat('most common total-credit gaps among misses:', head(names(sort(table(round(m$d_cred)), decreasing = TRUE)), 6), '\n')

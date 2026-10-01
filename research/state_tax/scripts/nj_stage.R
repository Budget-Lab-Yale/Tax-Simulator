# NJ TAXSIM 2019 (2026-10-01): which stage do the non-NOL misses diverge at?
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
cols = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','other_inc','itemizing','filing_status','n_dep','age1',
         'st_agi','v32_state_agi','st_exempt','v33_state_exemption_amount','st_ded','st_item_ded','v34_state_std_deduction_amount',
         'v35_state_itemized_deduction','st_txbl_inc','v36_state_taxable_income','st_tax_pre_credit','st_credits_nonref','st_credits_ref',
         'v37_state_property_tax_credit','v38_state_child_care_credit','v39_state_eitc','v40_state_total_credits','st_eitc','st_hh_credit')
r = fread('research/state_tax/cross_model/results/raw/taxsim_2019.csv', select = cols)
r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE & !(other_inc < -2000)]
m = r[abs_diff > 100]
cat(sprintf('NJ 2019 non-NOL: n %d, match %.3f, misses %d, ours higher %.2f\n', nrow(r), mean(r$abs_diff <= 100), nrow(m), mean(m$diff > 0)))
st = function(lab, x) cat(sprintf('  %-26s misses nonzero %.2f median %9.0f | matches nonzero %.2f\n', lab, mean(abs(x[r$abs_diff > 100]) > 100), median(x[r$abs_diff > 100]), mean(abs(x[r$abs_diff <= 100]) > 100)))
st('state AGI (ours - v32)', r$st_agi - r$v32_state_agi)
st('exemptions (ours - v33)', r$st_exempt - r$v33_state_exemption_amount)
st('deductions (ours - v35)', r$st_ded - r$v35_state_itemized_deduction)
st('taxable inc (ours - v36)', r$st_txbl_inc - r$v36_state_taxable_income)
st('credits (ours - v40)', r$st_credits_nonref + r$st_credits_ref - r$v40_state_total_credits)
cat('  our NJ deduction (st_ded) on misses: share zero', round(mean(m$st_ded == 0), 2), '| TAXSIM v35 on misses: share zero', round(mean(m$v35_state_itemized_deduction == 0), 2), ', median when > 0', median(m$v35_state_itemized_deduction[m$v35_state_itemized_deduction > 0]), '\n')
cat('  TAXSIM property tax credit v37 > 0 on misses:', round(mean(m$v37_state_property_tax_credit > 0), 2), '\n')
cat('  federal itemizers among misses:', round(mean(m$itemizing), 2), '| among all:', round(mean(r$itemizing), 2), '\n')

# NJ cross-model residual after the 2026-10-01 fixes: where do the misses sit?
suppressPackageStartupMessages({ library(data.table) }); options(width = 220)
dir = 'research/state_tax/cross_model/cache_sw/nj_run/raw'
prof = function(model, yr) {
  r = fread(sprintf('%s/%s_%d.csv', dir, model, yr))[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE]
  r[, miss := abs_diff > 100]
  r[, band := cut(agi, c(-Inf, 0, 10000, 20000, 40000, 75000, 150000, 300000, Inf), dig.lab = 7)]
  cat(sprintf('\n==== NJ %s %d: n %d, match %.3f, misses %d, ours higher %.2f, NOL share of misses %.2f\n', model, yr, nrow(r),
      mean(!r$miss), sum(r$miss), mean(r$diff[r$miss] > 0), mean(r$other_inc[r$miss] < -2000)))
  print(r[, .(n = .N, match = round(mean(!miss), 3), share_of_misses = round(sum(miss) / sum(r$miss), 3), med_diff_miss = round(median(diff[miss]))), by = band][order(band)])
  m = r[miss == TRUE & !(other_inc < -2000)]
  cat('  most common diffs (non-NOL misses):', head(names(sort(table(round(m$diff)), decreasing = TRUE)), 10), '\n')
  if (model == 'taxsim') {
    g = function(lab, x) cat(sprintf('  %-26s nonzero(>$100) on misses %.2f median %8.0f | on matches %.2f\n', lab, mean(abs(x[r$miss & !(r$other_inc < -2000)]) > 100), median(x[r$miss & !(r$other_inc < -2000)]), mean(abs(x[!r$miss]) > 100)))
    g('state AGI (ours - v32)', r$st_agi - r$v32_state_agi)
    g('exemptions (ours - v33)', r$st_exempt - r$v33_state_exemption_amount)
    g('taxable (ours - v36)', r$st_txbl_inc - r$v36_state_taxable_income)
    g('credits (ours - v40)', r$st_credits_nonref + r$st_credits_ref - r$v40_state_total_credits)
  }
}
prof('taxsim', 2019); prof('taxsim', 2020); prof('policyengine', 2021); prof('policyengine', 2024)

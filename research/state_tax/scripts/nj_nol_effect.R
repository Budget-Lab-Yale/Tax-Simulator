# NJ (2026-10-01): effect of excluding records with a federal NOL (negative other income)
suppressPackageStartupMessages({ library(data.table) })
for (m in c('taxsim', 'policyengine')) for (yr in if (m == 'taxsim') 2017:2020 else 2021:2025) {
  r = fread(sprintf('research/state_tax/cross_model/results/raw/%s_%d.csv', m, yr), select = c('id','state','fed_aligned','excluded','abs_diff','diff','agi','other_inc'))
  r = r[state == 'NJ' & fed_aligned == TRUE & excluded == FALSE]
  hit = r$other_inc < -2000
  cat(sprintf('%-12s %d: cell %.3f | NOL hits %4d (%.1f%%, match %.3f, ours higher %.2f) | excluding %.3f | misses left: ours-higher share %.2f\n',
      m, yr, mean(r$abs_diff <= 100), sum(hit), 100 * mean(hit), mean(r$abs_diff[hit] <= 100), mean(r$diff[hit & r$abs_diff > 100] > 0),
      mean(r$abs_diff[!hit] <= 100), mean(r$diff[!hit & r$abs_diff > 100] > 0)))
}

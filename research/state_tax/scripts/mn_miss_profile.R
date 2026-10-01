# Minnesota cross-model miss profile (2026-09-30): where do the clean-subset
# mismatches (> $100) concentrate, per model-year?
suppressPackageStartupMessages(library(data.table)); options(width = 220)
prof = function(model, y) {
  cols = c('state','fed_aligned','excluded','abs_diff','diff','st_itemizing','itemizing','n_dep','age1','filing_status','agi','id')
  r = fread(sprintf('research/state_tax/cross_model/results/raw/%s_%d.csv', model, y), select = cols)
  r = r[state == 'MN' & fed_aligned == TRUE & excluded == FALSE]
  r[, miss := abs_diff > 100]
  g = function(lab, cond) { cond[is.na(cond)] = FALSE
    data.table(group = lab, share_of_misses = round(mean(cond[r$miss]), 3),
               share_of_all = round(mean(cond), 3), match_in_group = round(mean(!r$miss[cond]), 3)) }
  out = rbind(g('state itemizer', r$st_itemizing == TRUE), g('federal itemizer', r$itemizing == TRUE),
              g('has dependents', r$n_dep > 0), g('age1 >= 65', r$age1 >= 65),
              g('MFJ', r$filing_status == 2), g('MFS', r$filing_status == 3),
              g('agi < 40k', r$agi < 40000), g('agi >= 200k', r$agi >= 200000))
  cat(sprintf('\n== %s %d: n %d, match %.3f, misses %d, ours-higher %.2f, top diff modes: %s\n',
              model, y, nrow(r), mean(!r$miss), sum(r$miss), mean(r$diff[r$miss] > 0),
              paste(head(names(sort(table(round(r$diff[r$miss])), decreasing = TRUE)), 6), collapse = ' ')))
  print(out)
}
args = commandArgs(trailingOnly = TRUE)
for (a in args) { p = strsplit(a, ':')[[1]]; prof(p[1], as.integer(p[2])) }

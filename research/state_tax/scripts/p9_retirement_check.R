# Evidence for retiring the P9 (dependent aged 18+) known-difference rows after
# the 2026-09-30 driver fix that supplies is_tax_unit_dependent. For each P9 row
# and year: clean-subset match@100 for records the row's predicate hits vs the
# rest of the state, with every OTHER exclude row applied.
suppressPackageStartupMessages(library(data.table)); options(width = 200)
kd = fread('src/tests/state/cross_model/known_differences.csv'); kd[, line := .I + 1]
p9 = kd[(grepl('dependents aged 18', description) | line == 88) & model == 'policyengine']
others = kd[action == 'exclude' & model %in% c('policyengine', 'both') & !(line %in% p9$line)]
out = rbindlist(lapply(2021:2024, function(y) {
  r = fread(sprintf('research/state_tax/cross_model/results/raw/policyengine_%d.csv', y))
  r = r[fed_aligned == TRUE]
  ex = rep(FALSE, nrow(r))
  for (i in seq_len(nrow(others))) { k = others[i]
    if (y < k$year_min || y > k$year_max) next
    h = (k$state == 'ALL' | r$state == k$state)
    if (!is.na(k$predicate) && nzchar(k$predicate)) { p = r[, eval(parse(text = k$predicate))]; p[is.na(p)] = FALSE; h = h & p }
    ex = ex | h }
  r = r[!ex]
  rbindlist(lapply(seq_len(nrow(p9)), function(i) { k = p9[i]; d = r[state == k$state]
    hit = d[, eval(parse(text = k$predicate))]; hit[is.na(hit)] = FALSE
    data.table(year = y, line = k$line, state = k$state, action = k$action, n_hit = sum(hit),
               hit = mean(d$abs_diff[hit] <= 100), rest = mean(d$abs_diff[!hit] <= 100)) }))
}))
out[, gap := round(hit - rest, 3)]
print(dcast(out, line + state + action ~ year, value.var = 'gap'))
cat('\nrecords hit per row, 2021-24 pooled:\n'); print(out[, .(n_hit = sum(n_hit), mean_hit = round(weighted.mean(hit, n_hit), 3), mean_rest = round(mean(rest), 3)), by = .(line, state)])

#-------------------------------------------------------------------------------
# Worker-level wage cells: average wages, worker counts, and average income-tax
# MTR on wages, for 2027-2036.
#
# Source: baseline / static detail files from vintage top_tax_dials_30y_v3
#         (full sample; mtr_wages1 and mtr_wages2 are income-tax-only --
#         run_mtr_block calls calc_mtrs with pr = FALSE).
#
# Unit of observation is a WORKER, not a tax unit: each record contributes a
# primary worker (wages1 > 0) and, if present, a secondary worker (wages2 > 0),
# each carrying the tax unit's weight.
#-------------------------------------------------------------------------------

library(data.table)

vintage_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/baseline/static/detail'
out_dir     = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wage_mtr_cells'
years       = 2027:2036

# Cell edges: nonzero to 500k in 20k steps, 500k to 1M in 50k steps, then 1M+
edges = c(0, seq(20e3, 500e3, by = 20e3), seq(550e3, 1e6, by = 50e3), Inf)
lo    = head(edges, -1)
hi    = edges[-1]
labels = ifelse(is.infinite(hi),
                sprintf('$%dk+', as.integer(lo / 1000)),
                sprintf('$%dk-$%dk', as.integer(lo / 1000),
                                     as.integer(pmin(hi, 1e9) / 1000)))
labels[length(labels)] = '$1M+'

bucketize = function(w) cut(w, breaks = edges, labels = labels,
                            right = FALSE, include.lowest = FALSE)

res = rbindlist(lapply(years, function(yr) {

  f = file.path(vintage_dir, paste0(yr, '.csv'))
  d = fread(f, select = c('weight', 'wages1', 'wages2',
                          'mtr_wages1', 'mtr_wages2'))

  # Reshape to worker level
  workers = rbindlist(list(
    d[wages1 > 0, .(weight, earner = 'primary',   wages = wages1, mtr = mtr_wages1)],
    d[wages2 > 0, .(weight, earner = 'secondary', wages = wages2, mtr = mtr_wages2)]
  ))

  workers[, cell := as.character(bucketize(wages))]

  out = workers[, .(
    n_workers        = sum(weight),
    n_workers_mtr    = sum(weight * !is.na(mtr)),
    avg_wages        = sum(weight * wages) / sum(weight),
    total_wages      = sum(weight * wages),
    avg_mtr_iit      = sum(weight * mtr, na.rm = TRUE) / sum(weight * !is.na(mtr)),
    avg_mtr_iit_wtd  = sum(weight * wages * mtr, na.rm = TRUE) /
                         sum(weight * wages * !is.na(mtr))
  ), keyby = cell]

  # Keep every cell in the ladder even if empty, in ladder order
  full = data.table(cell = labels, wage_lower = lo, wage_upper = hi)
  out  = out[full, on = 'cell']
  out[, year := yr]

  # All-workers row
  allrow = workers[, .(
    cell             = 'All workers',
    n_workers        = sum(weight),
    n_workers_mtr    = sum(weight * !is.na(mtr)),
    avg_wages        = sum(weight * wages) / sum(weight),
    total_wages      = sum(weight * wages),
    avg_mtr_iit      = sum(weight * mtr, na.rm = TRUE) / sum(weight * !is.na(mtr)),
    avg_mtr_iit_wtd  = sum(weight * wages * mtr, na.rm = TRUE) /
                         sum(weight * wages * !is.na(mtr)),
    year             = yr,
    wage_lower       = 0,
    wage_upper       = Inf
  )]

  cat('year', yr, ': ', nrow(workers), ' worker records, ',
      round(sum(workers$weight) / 1e6, 1), 'M weighted workers\n', sep = '')

  rbind(out[, .(year, cell = as.character(cell), wage_lower, wage_upper,
                n_workers, n_workers_mtr, avg_wages, total_wages,
                avg_mtr_iit, avg_mtr_iit_wtd)],
        allrow[, .(year, cell, wage_lower, wage_upper, n_workers,
                   n_workers_mtr, avg_wages, total_wages,
                   avg_mtr_iit, avg_mtr_iit_wtd)])
}))

fwrite(res, file.path(out_dir, 'wage_mtr_cells_2027_2036.csv'))
cat('\nwrote', file.path(out_dir, 'wage_mtr_cells_2027_2036.csv'), '\n')
cat('rows:', nrow(res), '\n')
print(res[year == 2027])
cat('BUILD_CELLS_DONE\n')

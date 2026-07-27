#-------------------------------------------------------------------------------
# Diagnostic: how much wage income sits above the OASDI taxable maximum in the
# baseline, worker level, 2027. Two very different quantities:
#
#   (A) total wages OF workers whose wages exceed the cap
#   (B) the EXCESS above the cap -- sum of max(0, w - cap)
#
# SSA's "share of covered earnings above the taxable maximum" is (B)-like.
# Also reports the tax-unit `wages` control total and the wages1+wages2
# residual, and the Forbes-clone (age1 top-code) contribution to the top.
#-------------------------------------------------------------------------------

library(data.table)

vintage_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/baseline/static/detail'

for (yr in c(2027, 2036)) {

  d = fread(file.path(vintage_dir, paste0(yr, '.csv')),
            select = c('id', 'weight', 'filing_status', 'age1', 'wages1',
                       'wages2', 'wages', 'liab_fica_er1', 'liab_fica_er2'))

  # Recover the year's OASDI taxable max from employer-side FICA:
  #   liab = 0.062 * min(gw, cap) + 0.0145 * gw
  # so for capped earners cap = (liab - 0.0145*gw) / 0.062. Use the modal value
  # among high earners (gw well above any plausible cap).
  cap_est = d[wages1 > 3e5, (liab_fica_er1 - 0.0145 * wages1) / 0.062]
  cap     = as.numeric(names(sort(table(round(cap_est, -2)), decreasing = TRUE))[1])
  cat('\n==== ', yr, ' ====\n', sep = '')
  cat('implied OASDI taxable max: ', format(cap, big.mark = ','), '\n', sep = '')

  workers = rbindlist(list(
    d[wages1 > 0, .(weight, wages = wages1, clone = age1 >= 80)],
    d[wages2 > 0, .(weight, wages = wages2, clone = age1 >= 80)]
  ))

  tot   = workers[, sum(weight * wages)]
  above = workers[wages > cap, sum(weight * wages)]
  excess = workers[, sum(weight * pmax(0, wages - cap))]
  n_above = workers[wages > cap, sum(weight)]

  cat('worker-level total wages         : $', round(tot / 1e12, 3), 'T\n', sep = '')
  cat('tax-unit `wages` control total   : $',
      round(d[, sum(weight * wages)] / 1e12, 3), 'T\n', sep = '')
  cat('  (wages1+wages2 residual        : $',
      round((d[, sum(weight * wages)] - tot) / 1e9, 1), 'B)\n', sep = '')
  cat('workers above cap                : ', round(n_above / 1e6, 1), 'M (',
      round(100 * n_above / workers[, sum(weight)], 1), '% of workers)\n', sep = '')
  cat('(A) TOTAL wages of above-cap wkrs: $', round(above / 1e12, 3), 'T = ',
      round(100 * above / tot, 1), '% of all wages\n', sep = '')
  cat('(B) EXCESS above the cap         : $', round(excess / 1e12, 3), 'T = ',
      round(100 * excess / tot, 1), '% of all wages\n', sep = '')

  # Forbes-clone / top-code contribution to the excess
  cat('    of which age1 >= 80 records  : $',
      round(workers[clone == TRUE, sum(weight * pmax(0, wages - cap))] / 1e9, 1),
      'B\n', sep = '')

  # Where the excess sits
  brk = workers[wages > cap][, grp := cut(wages, c(cap, 2.5e5, 5e5, 1e6, Inf),
                                          labels = c('cap-250k', '250-500k',
                                                     '500k-1M', '1M+'))][
    , .(n = sum(weight) / 1e6,
        excess_B = sum(weight * (wages - cap)) / 1e9), keyby = grp]
  print(brk)
}
cat('\nCAP_CHECK_DONE\n')

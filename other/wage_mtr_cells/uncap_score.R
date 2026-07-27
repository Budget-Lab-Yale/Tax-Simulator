#-------------------------------------------------------------------------------
# Mechanical uncap score implied by the wage tabulation: 12.4% OASDI applied to
# the earnings ABOVE the taxable maximum, 2027-2036, less the income-tax offset
# on the deductible employer half. No behavioral response, no benefit credits.
#
# Two bases:
#   wages only  -- what the wage-cell tabulation covers
#   wages + SE  -- adds self-employment earnings, which stack on top of wages
#                  against the same per-person cap. `se` is a tax-unit column,
#                  so it is attributed to the primary earner (approximation).
#-------------------------------------------------------------------------------

library(data.table)

vintage_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/baseline/static/detail'

OASDI    = 0.124    # combined ee + er
ER_SHARE = 0.062
SECA_TXBL = 0.9235  # pr.seca_taxable_rate

res = rbindlist(lapply(2027:2036, function(yr) {

  d = fread(file.path(vintage_dir, paste0(yr, '.csv')),
            select = c('weight', 'wages1', 'wages2', 'se', 'liab_fica_er1',
                       'mtr_wages1', 'mtr_wages2'))

  cap_est = d[wages1 > 4e5, (liab_fica_er1 - 0.0145 * wages1) / 0.062]
  cap     = as.numeric(names(sort(table(round(cap_est, -2)), decreasing = TRUE))[1])

  # Wage excess, per earner
  ex_w = d[, sum(weight * (pmax(0, wages1 - cap) + pmax(0, wages2 - cap)))]

  # SE excess: SE stacks above wages against the same cap (primary earner)
  d[, se_txbl := pmax(0, se) * SECA_TXBL]
  ex_se = d[, sum(weight * (pmax(0, wages1 + se_txbl - cap) -
                            pmax(0, wages1 - cap)))]

  # Weighted average income-tax MTR among above-cap wage dollars, for the
  # employer-side deductibility offset
  w = rbindlist(list(d[wages1 > cap, .(weight, ex = wages1 - cap, mtr = mtr_wages1)],
                     d[wages2 > cap, .(weight, ex = wages2 - cap, mtr = mtr_wages2)]))
  mtr_bar = w[, sum(weight * ex * mtr) / sum(weight * ex)]

  data.table(year = yr, cap = cap,
             excess_wages_T = ex_w / 1e12,
             excess_se_T    = ex_se / 1e12,
             mtr_bar        = mtr_bar,
             gross_wages_B  = OASDI * ex_w / 1e9,
             gross_both_B   = OASDI * (ex_w + ex_se) / 1e9,
             offset_both_B  = ER_SHARE * (ex_w + ex_se) * mtr_bar / 1e9)
}))

res[, net_both_B := gross_both_B - offset_both_B]
res[, net_wages_B := gross_wages_B * (1 - ER_SHARE * mtr_bar / OASDI)]

print(res[, .(year, cap, excess_wages_T = round(excess_wages_T, 3),
              excess_se_T = round(excess_se_T, 3), mtr_bar = round(mtr_bar, 3),
              gross_both_B = round(gross_both_B), offset_both_B = round(offset_both_B),
              net_both_B = round(net_both_B))])

cat('\n---- 10-year totals, 2027-2036 (CY, $B) ----\n')
cat('wage excess base            : $', round(res[, sum(excess_wages_T)], 2), 'T\n', sep = '')
cat('  + SE excess base          : $', round(res[, sum(excess_se_T)], 2), 'T\n', sep = '')
cat('gross OASDI, wages only     : $', round(res[, sum(gross_wages_B)] / 1000, 2), 'T\n', sep = '')
cat('gross OASDI, wages + SE     : $', round(res[, sum(gross_both_B)] / 1000, 2), 'T\n', sep = '')
cat('income-tax offset (er half) : $', round(res[, sum(offset_both_B)] / 1000, 2), 'T\n', sep = '')
cat('NET, wages only             : $', round(res[, sum(net_wages_B)] / 1000, 2), 'T\n', sep = '')
cat('NET, wages + SE             : $', round(res[, sum(net_both_B)] / 1000, 2), 'T\n', sep = '')
fwrite(res, '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wage_mtr_cells/uncap_score.csv')
cat('UNCAP_SCORE_DONE\n')

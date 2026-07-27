#-------------------------------------------------------------------------------
# Adds the SECOND-ROUND offsets to the mechanical uncap arithmetic.
#
# do_taxes() (src/calc/do_taxes.R L56-85) holds total labor compensation fixed:
# when employer-side payroll rises, taxable wages fall, with 85% of the marginal
# non-payroll compensation assumed to land in nontaxable fringe. Payroll taxes
# are then recomputed ON THE REDUCED WAGES. So the wage cut erodes:
#   - income tax           at the worker's income-tax MTR
#   - employee OASDI       6.2%  (uncapped, so it bites on these dollars)
#   - employee HI          1.45% + 0.9% additional Medicare above the threshold
#   - employer HI          1.45%
#   - employer OASDI       6.2%  (uncapped)
#
# Approximation of the model's own mechanics, not a substitute for running the
# reform: the wage cut is first-order (no iteration), and the additional-Medicare
# threshold test uses baseline wages.
#-------------------------------------------------------------------------------

library(data.table)

vintage_dir = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/baseline/static/detail'

OASDI_EE = 0.062; OASDI_ER = 0.062; HI_EE = 0.0145; HI_ER = 0.0145
ADD_MED  = 0.009; FRINGE = 0.85; SECA_TXBL = 0.9235

res = rbindlist(lapply(2027:2036, function(yr) {

  d = fread(file.path(vintage_dir, paste0(yr, '.csv')),
            select = c('weight', 'filing_status', 'wages1', 'wages2', 'wages',
                       'se', 'liab_fica_er1', 'mtr_wages1', 'mtr_wages2'))

  cap = as.numeric(names(sort(table(round(
    d[wages1 > 4e5, (liab_fica_er1 - 0.0145 * wages1) / 0.062], -2)),
    decreasing = TRUE))[1])

  # Additional-Medicare threshold (0.9% on combined wages above it)
  d[, add_med_thresh := fifelse(filing_status == 2, 250e3,
                        fifelse(filing_status == 3, 125e3, 200e3))]
  d[, over_add_med := wages > add_med_thresh]

  w = rbindlist(list(
    d[wages1 > cap, .(weight, w = wages1, ex = wages1 - cap,
                      mtr = mtr_wages1, over_add_med)],
    d[wages2 > cap, .(weight, w = wages2, ex = wages2 - cap,
                      mtr = mtr_wages2, over_add_med)]
  ))

  # New employer-side OASDI on the uncapped excess, and the wage cut it induces
  w[, d_er   := OASDI_ER * ex]
  w[, d_wage := FRINGE * d_er * w / (w + d_er)]

  # Marginal erosion rate on the lost wage dollars
  w[, erosion := mtr + OASDI_EE + HI_EE + HI_ER + OASDI_ER +
                 fifelse(over_add_med, ADD_MED, 0)]

  gross_wage_leg = w[, sum(weight * ex)] * (OASDI_EE + OASDI_ER)
  offset_total   = w[, sum(weight * d_wage * erosion)]
  offset_iit     = w[, sum(weight * d_wage * mtr)]

  # SE leg: no employer half, so no compensation-shift offset. SECA is fully
  # deductible above the line at half, already in the baseline structure;
  # keep the simple gross figure and flag it.
  d[, se_txbl := pmax(0, se) * SECA_TXBL]
  ex_se = d[, sum(weight * (pmax(0, wages1 + se_txbl - cap) - pmax(0, wages1 - cap)))]

  data.table(year = yr, cap = cap,
             excess_T       = w[, sum(weight * ex)] / 1e12,
             wage_cut_B     = w[, sum(weight * d_wage)] / 1e9,
             erosion_bar    = w[, sum(weight * d_wage * erosion) / sum(weight * d_wage)],
             gross_wage_B   = gross_wage_leg / 1e9,
             offset_iit_B   = offset_iit / 1e9,
             offset_pr_B    = (offset_total - offset_iit) / 1e9,
             net_wage_B     = (gross_wage_leg - offset_total) / 1e9,
             se_gross_B     = ex_se * (OASDI_EE + OASDI_ER) / 1e9)
}))

res[, net_incl_se_B := net_wage_B + se_gross_B]
print(res[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

s = res[, lapply(.SD, sum), .SDcols = c('gross_wage_B', 'offset_iit_B',
                                        'offset_pr_B', 'net_wage_B',
                                        'se_gross_B', 'net_incl_se_B',
                                        'wage_cut_B')]
cat('\n---- 10-year, 2027-2036, wage leg ($T) ----\n')
cat('gross OASDI on excess      : ', round(s$gross_wage_B / 1000, 2), '\n')
cat('induced taxable wage cut   : ', round(s$wage_cut_B / 1000, 2), '\n')
cat('  income-tax offset        : ', round(s$offset_iit_B / 1000, 2), '\n')
cat('  PAYROLL offset (new)     : ', round(s$offset_pr_B / 1000, 2), '\n')
cat('net, wages only            : ', round(s$net_wage_B / 1000, 2), '\n')
cat('net, incl SE leg           : ', round(s$net_incl_se_B / 1000, 2), '\n')
cat('effective net rate on base : ',
    round(100 * s$net_wage_B / (res[, sum(excess_T)] * 1000), 2), '% of excess\n')
fwrite(res, '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wage_mtr_cells/uncap_offsets.csv')
cat('UNCAP_OFFSETS_DONE\n')

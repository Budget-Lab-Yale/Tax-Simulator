#-------------------------------------------------------------------------------
# compute_top_eti.R
#
# Computes the top-subset ordinary-income ETI (the exhibit's own-ordinary
# cell: NOT-form, taxable income excl. net gains, top-bracket membership
# fixed at baseline) for one scenario vs a baseline — used by the sigma
# central calibration (leave-one-out and confirmation runs).
#
# Usage: Rscript compute_top_eti.R <scenario_root> <scenario_id> \
#          <baseline_root> <threshold_source_state_dir> <first_year> <last_year>
# threshold_source_state_dir: a kg_dynamics_state dir whose sigma trackers
# carry the gate thresholds for this reform (the no-sigma leg has none of
# its own).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr)
})

args = commandArgs(trailingOnly = TRUE)
scen_root = args[1]; scen = args[2]; base_root = args[3]
th_dir = args[4]
years = as.integer(args[5]):as.integer(args[6])

res = rbindlist(lapply(years, function(t) {
  b = fread(file.path(base_root, 'baseline/static/detail', paste0(t, '.csv')),
            select = c('id', 'weight', 'filing_status', 'txbl_inc', 'txbl_kg',
                       'mtr_wages1', 'wages'), showProgress = FALSE)
  s = fread(file.path(scen_root, scen, 'static/detail', paste0(t, '.csv')),
            select = c('id', 'mtr_wages1'), showProgress = FALSE)
  cv = fread(file.path(scen_root, scen, 'conventional/detail', paste0(t, '.csv')),
             select = c('id', 'weight', 'txbl_inc', 'txbl_kg'),
             showProgress = FALSE)
  th = readRDS(file.path(th_dir, paste0(t, '.rds')))$sigma$thresholds

  b = merge(b, as.data.table(th), by = 'filing_status', all.x = TRUE)
  setkey(b, id); setkey(s, id); setkey(cv, id)
  top = !is.na(b$sigma_thresh) & b$txbl_inc >= b$sigma_thresh

  O = function(d, sel) d[sel, sum(weight * pmax(txbl_inc - pmax(txbl_kg, 0), 0))]
  O_b = O(b, top); O_c = O(cv[b[, .(id)], on = 'id'], top)

  j = merge(b[, .(id, weight, wages, mtr_wages1)],
            s[, .(id, mtr_s = mtr_wages1)], by = 'id')
  w = pmax(j$wages, 0)
  ok = !is.na(j$mtr_s - j$mtr_wages1)
  dtau  = j[ok, sum(pmax(wages,0) * (mtr_s - mtr_wages1))] / sum(w[ok])
  tau_b = j[ok, sum(pmax(wages,0) * mtr_wages1)] / sum(w[ok])

  dlog_ntr = log((1 - (tau_b + dtau)) / (1 - tau_b))
  data.table(year = t, O_base = O_b, O_conv = O_c,
             dlogO = log(O_c / O_b), dlog_ntr = dlog_ntr,
             eti = ifelse(abs(dlog_ntr) > 1e-12, log(O_c / O_b) / dlog_ntr, NA))
}))

print(res)
cat(sprintf('\nMean ETI %d-%d (lead-in dropped): %.4f\n',
            min(years) + 1, max(years),
            res[year > min(years), mean(eti, na.rm = TRUE)]))

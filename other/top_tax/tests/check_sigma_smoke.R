#-------------------------------------------------------------------------------
# check_sigma_smoke.R
#
# Post-hoc checks on a sigma_smoke pipeline run (plan Verification item 3):
#  (a) per-record WAGE deltas in conventional detail equal the sigma dump's
#      conversions exactly (wages are touched by no other module in the
#      stack; PT legs compose with entity shifting + evasion, so they are
#      checked at the conservation level instead);
#  (b) sum(record conv) == sum(cell conv_inflow) per year (tracker vs dump);
#  (c) tau_eq columns present on the cell table, within bounds, and the
#      dtau_eq the conversions used matches tau_eq_S - tau_eq_B;
#  (d) the recurrence actually carries the injected inflow: sum over the
#      cell table's conv_inflow equals the tracker's total.
#
# Usage: Rscript other/top_tax/tests/check_sigma_smoke.R <output_root> \
#          <scenario_id> <first_year> <last_year>
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tibble)
})

args = commandArgs(trailingOnly = TRUE)
root = args[1]; scen = args[2]
years = as.integer(args[3]):as.integer(args[4])

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}

for (t in years) {
  st = fread(file.path(root, scen, 'static', 'detail', paste0(t, '.csv')),
             select = c('id', 'weight', 'wages1', 'wages2'), showProgress = FALSE)
  cv = fread(file.path(root, scen, 'conventional', 'detail', paste0(t, '.csv')),
             select = c('id', 'weight', 'wages1', 'wages2'), showProgress = FALSE)
  stopifnot(identical(st$id, cv$id))

  dump = fread(file.path(root, scen, 'conventional', 'supplemental',
                         'sigma_conversion_dump', paste0(t, '.csv')),
               showProgress = FALSE)
  state = readRDS(file.path(root, scen, 'conventional', 'supplemental',
                            'kg_dynamics_state', paste0(t, '.rds')))
  tr = state$sigma
  ct = state$cell_table

  # (a) per-record wage-leg equality: conventional - static == -conv, up to
  # the model's employer-FICA incidence convention in do_taxes (85% of the
  # employer payroll change on the moved wages is passed back into wages, so
  # the observed delta is smaller than conv by at most 0.85 * 7.65% of it).
  # Records with zero conversion must match EXACTLY (no other module in the
  # stack touches wages).
  d = merge(data.table(id = st$id, dw1 = cv$wages1 - st$wages1,
                       dw2 = cv$wages2 - st$wages2),
            dump[, .(id, conv_w1, conv_w2)], by = 'id', all.x = TRUE)
  d[is.na(conv_w1), `:=`(conv_w1 = 0, conv_w2 = 0)]
  err_zero = with(d[conv_w1 == 0 & conv_w2 == 0],
                  max(abs(dw1), abs(dw2), 0))
  check(err_zero < 1e-6,
        sprintf('year %d: unconverted records have zero wage delta (max err %.2e)', t, err_zero))
  r1 = with(d[conv_w1 != 0], abs(dw1 + conv_w1) - 0.85 * 0.0765 * abs(conv_w1))
  r2 = with(d[conv_w2 != 0], abs(dw2 + conv_w2) - 0.85 * 0.0765 * abs(conv_w2))
  err1 = max(c(r1, r2, -Inf))
  check(err1 < 1e-6,
        sprintf('year %d: converted-record wage deltas == -conv within the er-FICA incidence band (max excess %.2e)', t, err1))

  # (b) conservation: dump records vs tracker cell inflow
  dump_total = dump[, sum(weight * conv_total)]
  err2 = abs(dump_total - tr$conv_total) / max(abs(tr$conv_total), 1)
  check(err2 < 1e-9,
        sprintf('year %d: dump total ($%.3fB) == tracker cell inflow ($%.3fB)',
                t, dump_total / 1e9, tr$conv_total / 1e9))

  # (c) tau_eq columns present, bounded, and consistent with the dump's dtau_eq
  check(all(c('tau_eq_B', 'tau_eq_S', 'conv_inflow') %in% names(ct)),
        sprintf('year %d: cell table carries tau_eq_B / tau_eq_S / conv_inflow', t))
  check(all(ct$tau_eq_B >= 0) && all(ct$tau_eq_S >= 0) &&
        max(ct$tau_eq_B, ct$tau_eq_S) < 0.5,
        sprintf('year %d: tau_eq in bounds (B: [%.4f, %.4f]; S: [%.4f, %.4f])',
                t, min(ct$tau_eq_B), max(ct$tau_eq_B),
                min(ct$tau_eq_S), max(ct$tau_eq_S)))
  te = setNames(ct$tau_eq_S - ct$tau_eq_B, as.character(ct$age))
  gd = dump[!is.na(dtau_eq)]
  err3 = max(abs(gd$dtau_eq - te[as.character(gd$age_cohort)]))
  check(err3 < 1e-12,
        sprintf('year %d: dump dtau_eq == cell tau_eq_S - tau_eq_B (max err %.2e)', t, err3))

  # (d) cell table conv_inflow ties to the tracker
  err4 = abs(sum(ct$conv_inflow) - tr$conv_total) / max(abs(tr$conv_total), 1)
  check(err4 < 1e-9,
        sprintf('year %d: cell-table conv_inflow total == tracker ($%.3fB)',
                t, tr$conv_total / 1e9))

  cat(sprintf(
    '        year %d diagnostics: pool $%.1fB (%.0f records, %.2fM weighted); mean dW %.4f; mean dtau_eq %.5f; inflow $%.3fB (wages $%.3fB / PT $%.3fB)\n',
    t, tr$pool_dollars / 1e9, tr$pool_records, tr$pool_weighted / 1e6,
    tr$mean_dW_pooled, tr$mean_dtau_eq, tr$conv_total / 1e9,
    tr$conv_dollars_wages / 1e9, tr$conv_dollars_pt / 1e9))
}

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL CHECKS PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

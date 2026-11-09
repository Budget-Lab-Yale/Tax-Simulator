#-------------------------------------------------------------------------------
# Integration verification for the on-model corporate statutory-rate module.
# Ties the booked corporate-receipts DELTA (scenario - baseline) to the Form A
# delta computed on the TRUE CBO rev_corp base (NOT baseline receipts, which
# also carry a large year-varying non-rate corporate component -- cost recovery
# etc. -- that cancels in the delta). Baseline has only a static pass, so BOTH
# the scenario static and conventional deltas are measured against baseline
# static. Also checks incidence activation + distribution presence.
# module load R/4.4.1-foss-2022b; single core.
#-------------------------------------------------------------------------------

suppressMessages({library(dplyr); library(tidyr); library(readr); library(purrr);
                  library(stringr); library(magrittr)})

staging = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/corp_rate_smoke/_slurm_staging'
source('src/slurm/common.R')
reconstitute_environment(staging)

V    = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/corp_rate_smoke'
e    = 0.367; t0 = 0.21; tN = 0.28
fail = character(0)
note = function(...) cat(sprintf(...), '\n')

# TRUE CBO rev_corp base (the object the module reprices)
rev_corp = read_macro_spliced(
  get_scenario_info('corp_rate_28')$interface_paths$`Macro-Projections`) %>%
  distinct(year, .keep_all = TRUE) %>%
  transmute(year, R0 = rev_corp)

corp = function(id, pass) {
  f = file.path(V, id, pass, 'totals', 'receipts_full.csv')
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) %>% select(year, rc = revenues_corp_tax)
}

b = corp('baseline', 'static')       # baseline has only a static pass
note('== (1) revenue tie-out (delta vs Form A on true rev_corp) ==')
for (pass in c('static', 'conventional')) {
  s = corp('corp_rate_28', pass)
  if (is.null(s)) { fail = c(fail, paste0('missing scenario ', pass)); next }
  cmp = b %>% rename(rc_b = rc) %>%
    inner_join(s %>% rename(rc_s = rc), by = 'year') %>%
    inner_join(rev_corp, by = 'year') %>%
    mutate(
      t   = if_else(year >= 2027, tN, t0),
      obs = rc_s - rc_b,
      exp = if (pass == 'static') R0 * (t / t0 - 1)
            else                  R0 * ((t / t0) * ((1 - t) / (1 - t0))^e - 1),
      diff = obs - exp)
  note('  --- pass = %s (vs baseline static) ---', pass)
  for (i in seq_len(nrow(cmp))) with(cmp[i, ],
    note('   %d  R0=%7.1f  obs=%9.3f  exp=%9.3f  diff=%8.4f', year, R0, obs, exp, diff))
  worst = max(abs(cmp$diff), na.rm = TRUE)
  note('   max |obs-exp| = %.5f  %s', worst, if (worst <= 0.05) 'PASS' else 'FAIL')
  if (worst > 0.05) fail = c(fail, sprintf('%s tie-out off by %.4f', pass, worst))
}

note('\n== (2) incidence activation ==')
diag = list.files(file.path(V, 'corp_rate_28', 'conventional', 'supplemental'),
                  pattern = 'corp_conservation_diag_.*\\.csv', full.names = TRUE)
note('  conservation diag files: %d  %s', length(diag), if (length(diag) > 0) 'PASS' else 'FAIL')
if (length(diag) == 0) fail = c(fail, 'no corp conservation diagnostic')
det = file.path(V, 'corp_rate_28', 'conventional', 'detail', '2027.csv')
if (file.exists(det)) {
  d = read_csv(det, show_col_types = FALSE)
  tot = if ('corp_dY_exog' %in% names(d)) sum(abs(d$corp_dY_exog), na.rm = TRUE) else NA
  note('  corp_dY_exog sum|.| (2027) = %.3g  %s', tot, if (isTRUE(tot > 0)) 'PASS' else 'FAIL')
  if (!isTRUE(tot > 0)) fail = c(fail, 'corp_dY_exog missing/zero')
} else { fail = c(fail, 'conventional detail 2027 missing') }

note('\n== (3) distribution smear present ==')
distf = file.path(V, 'corp_rate_28', 'static', 'supplemental', 'distribution.csv')
note('  distribution.csv: %s', if (file.exists(distf)) 'present PASS' else 'MISSING FAIL')
if (!file.exists(distf)) fail = c(fail, 'distribution.csv missing')

note('\n=================')
if (length(fail) == 0) note('ALL INTEGRATION CHECKS PASS') else {
  note('FAILURES:'); for (f in fail) note('  - %s', f); quit(status = 1)
}

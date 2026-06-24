#-------------------------------------------------------------------------------
# compare_wts.R — cross-model comparison of the integrated Tax-Simulator wealth
# tax vs the standalone Wealth-Tax-Simulator (nickel_dime, the shared scenario).
# Also summarizes the integrated model's Warren scenario and distribution.
#
# Usage: Rscript compare_wts.R <ts_run_root> <wts_root_or_AUTO> <out_csv_dir>
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

args     = commandArgs(trailingOnly = TRUE)
ts_root  = args[1]
wts_arg  = ifelse(length(args) >= 2, args[2], 'AUTO')
out_dir  = ifelse(length(args) >= 3, args[3],
                  '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_tax')

rd = function(p) if (file.exists(p)) as_tibble(fread(p)) else NULL

# --- Resolve WTS output root (latest timestamp under wts_compare) ------------
if (wts_arg == 'AUTO') {
  base = '/nfs/roberts/scratch/pi_nrs36/jar335/wts_compare'
  ts   = list.dirs(base, recursive = FALSE)
  wts_root = if (length(ts) > 0) ts[order(basename(ts))][length(ts)] else NA
} else wts_root = wts_arg
cat('TS  root:', ts_root, '\n')
cat('WTS root:', wts_root, '\n\n')

# --- Integrated Tax-Simulator wealth tax levels ($B, CY) ---------------------
ts_levels = function(scn) {
  s = rd(file.path(ts_root, scn, 'static/totals/wealth.csv'))
  c = rd(file.path(ts_root, scn, 'conventional/totals/wealth.csv'))
  if (is.null(s)) return(NULL)
  s %>% select(year, ts_static = wealth_tax, ts_returns = wealth_returns) %>%
    left_join(c %>% select(year, ts_conv = wealth_tax), by = 'year') %>%
    mutate(scenario = scn)
}
ts_nd  = ts_levels('wealth_tax_nickel_dime')
ts_war = ts_levels('wealth_tax_warren')

cat('=== Integrated Tax-Simulator: annual wealth tax level ($B, CY) ===\n')
print(bind_rows(ts_nd, ts_war) %>%
        select(scenario, year, ts_static, ts_conv, ts_returns) %>%
        mutate(across(c(ts_static, ts_conv), ~ round(., 1)),
               ts_returns = round(ts_returns)))

# --- Standalone WTS nickel_dime levels (aggregate detail) --------------------
wts_levels = function(scn) {
  dd = file.path(wts_root, scn, 'detail')
  if (is.na(wts_root) || !dir.exists(dd)) return(NULL)
  fs = list.files(dd, pattern = '\\.csv$', full.names = TRUE)
  map_dfr(fs, function(f) {
    yr = as.integer(str_extract(basename(f), '[0-9]+'))
    d  = as_tibble(fread(f))
    sc = if ('liability.static' %in% names(d)) d$liability.static else NA
    cv = if ('liability.conventional' %in% names(d)) d$liability.conventional else NA
    tibble(year = yr,
           wts_static = sum(sc * d$weight, na.rm = TRUE) / 1e9,
           wts_conv   = sum(cv * d$weight, na.rm = TRUE) / 1e9,
           wts_returns = sum((sc > 0) * d$weight, na.rm = TRUE))
  }) %>% arrange(year)
}
wts_nd = wts_levels('nickel_dime')

cat('\n=== Standalone WTS: nickel_dime wealth tax level ($B, CY) ===\n')
if (!is.null(wts_nd)) {
  print(wts_nd %>% mutate(across(c(wts_static, wts_conv), ~ round(., 1)),
                          wts_returns = round(wts_returns)))
} else {
  cat('  (no WTS output found)\n')
}

# --- Side-by-side comparison (nickel_dime) -----------------------------------
cat('\n=== CROSS-MODEL COMPARISON: nickel_dime ($B wealth tax, CY) ===\n')
if (!is.null(ts_nd) && !is.null(wts_nd)) {
  cmp = ts_nd %>% select(year, ts_static, ts_conv, ts_returns) %>%
    inner_join(wts_nd, by = 'year') %>%
    mutate(static_ratio = ts_static / wts_static,
           conv_ratio   = ts_conv   / wts_conv) %>%
    select(year, ts_static, wts_static, static_ratio,
           ts_conv, wts_conv, conv_ratio, ts_returns, wts_returns)
  print(cmp %>% mutate(across(c(ts_static, wts_static, ts_conv, wts_conv), ~ round(., 1)),
                       across(c(static_ratio, conv_ratio), ~ round(., 2)),
                       across(c(ts_returns, wts_returns), ~ round(.))))
  write_csv(cmp, file.path(out_dir, 'comparison_nickel_dime.csv'))
  cat('\nwrote', file.path(out_dir, 'comparison_nickel_dime.csv'), '\n')
} else cat('  (cannot compare — missing one side)\n')

# Persist the integrated levels too
write_csv(bind_rows(ts_nd, ts_war), file.path(out_dir, 'ts_wealth_levels.csv'))
cat('wrote', file.path(out_dir, 'ts_wealth_levels.csv'), '\n')

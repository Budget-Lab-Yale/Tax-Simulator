#-------------------------------------------------------------------------------
# validate_wealth.R
#
# Post-run verification of the on-model wealth tax. Reads a completed run's
# output and checks the Part 6 verification items from the plan. Prints a PASS/
# FAIL report. Usage:
#   Rscript validate_wealth.R <run_root> [reform_id] [exemption] [year]
# where run_root is the vintage folder (…/<vintage>) containing baseline/ and
# the reform scenarios.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

args       = commandArgs(trailingOnly = TRUE)
run_root   = args[1]
reform_id  = ifelse(length(args) >= 2, args[2], 'wealth_tax_nickel_dime')
exemption  = ifelse(length(args) >= 3, as.numeric(args[3]), 50e6)
year       = ifelse(length(args) >= 4, as.integer(args[4]), 2026L)

cat('=================================================================\n')
cat('WEALTH TAX VALIDATION\n')
cat('run_root :', run_root, '\n')
cat('reform   :', reform_id, '\n')
cat('exemption:', format(exemption, big.mark = ','), '\n')
cat('year     :', year, '\n')
cat('=================================================================\n\n')

pass = function(msg) cat('  [PASS]', msg, '\n')
fail = function(msg) cat('  [FAIL]', msg, '\n')
chk  = function(cond, ok, no) if (isTRUE(cond)) pass(ok) else fail(no)

rd = function(...) file.path(run_root, ...) %>% fread() %>% as_tibble()

# --- 1. Dormancy: baseline liab_wealth identically 0; net_worth present ------
cat('1. DORMANCY (baseline)\n')
base_det = rd('baseline/static/detail', paste0(year, '.csv'))
chk('liab_wealth' %in% names(base_det), 'liab_wealth column present in detail',
    'liab_wealth column MISSING')
chk('net_worth' %in% names(base_det), 'net_worth column present in detail',
    'net_worth column MISSING')
chk(max(abs(base_det$liab_wealth)) == 0,
    sprintf('baseline liab_wealth identically 0 (max abs = %g)', max(abs(base_det$liab_wealth))),
    sprintf('baseline liab_wealth NONZERO (max abs = %g)', max(abs(base_det$liab_wealth))))
cat(sprintf('     net_worth: min=%s  median=%s  max=%s\n',
            format(round(min(base_det$net_worth)), big.mark = ','),
            format(round(median(base_det$net_worth)), big.mark = ','),
            format(round(max(base_det$net_worth)), big.mark = ',')))

# --- 2. Static score: reform populates liab_wealth + totals + receipts -------
cat('\n2. STATIC SCORE (reform)\n')
ref_det = rd(reform_id, 'static/detail', paste0(year, '.csv'))
tot_w   = rd(reform_id, 'static/totals/wealth.csv')
wsum_detail = sum(ref_det$liab_wealth * ref_det$weight) / 1e9
wsum_totals = tot_w$wealth_tax[tot_w$year == year]
chk(any(ref_det$liab_wealth > 0), 'reform liab_wealth populated (some > 0)',
    'reform liab_wealth all zero')
cat(sprintf('     wealth tax %d: detail=$%.2fB  totals/wealth.csv=$%.2fB\n',
            year, wsum_detail, wsum_totals))
chk(abs(wsum_detail - wsum_totals) < 1e-6 * max(1, wsum_totals),
    'totals/wealth.csv reconciles with detail aggregate',
    'totals/wealth.csv does NOT match detail aggregate')

# receipts: revenues_wealth_tax appears and feeds the total
rec = rd(reform_id, 'static/totals/receipts.csv')
chk('revenues_wealth_tax' %in% names(rec),
    'revenues_wealth_tax column present in receipts.csv',
    'revenues_wealth_tax column MISSING from receipts.csv')
if ('revenues_wealth_tax' %in% names(rec)) {
  cat(sprintf('     receipts revenues_wealth_tax (FY=CY): %s\n',
              paste(sprintf('%d=$%.1fB', rec$year, rec$revenues_wealth_tax), collapse = '  ')))
}

# revenue_estimates: the total budget effect should be net-negative for govt
# (i.e. positive revenue), and wealth tax should be in the delta. Read the
# machine-readable revenue_estimates.csv (total) for sanity.
re_path = file.path(run_root, reform_id, 'static/supplemental/revenue_estimates.csv')
if (file.exists(re_path)) {
  re = fread(re_path) %>% as_tibble()
  cat(sprintf('     revenue_estimates total budget effect: %s\n',
              paste(sprintf('%d=$%.1fB', re$year, re$total), collapse = '  ')))
  chk(any(re$total > 0), 'revenue estimate total is positive (revenue raised)',
      'revenue estimate total not positive — wealth tax may not reach headline')
}

# --- 3. MTR spot check: mtr_net_worth == statutory marginal rate -------------
cat('\n3. MTR SPOT CHECK (reform static)\n')
if ('mtr_net_worth' %in% names(ref_det)) {
  below = ref_det %>% filter(net_worth < exemption * 0.9)
  above = ref_det %>% filter(net_worth > exemption * 1.1, net_worth < exemption + 100e6)
  chk(nrow(below) == 0 || max(abs(below$mtr_net_worth)) < 1e-9,
      'mtr_net_worth == 0 below the exemption',
      sprintf('mtr_net_worth nonzero below exemption (max = %g)',
              ifelse(nrow(below) > 0, max(abs(below$mtr_net_worth)), NA)))
  if (nrow(above) > 0) {
    cat(sprintf('     mtr_net_worth just above exemption: min=%.4f median=%.4f max=%.4f (n=%d)\n',
                min(above$mtr_net_worth), median(above$mtr_net_worth),
                max(above$mtr_net_worth), nrow(above)))
  }
  cat('     distinct mtr_net_worth values (rounded): ',
      paste(sort(unique(round(ref_det$mtr_net_worth, 4))), collapse = ' '), '\n')
} else {
  fail('mtr_net_worth column missing from reform static detail')
}

# --- 4. Conventional avoidance + estate isolation ----------------------------
cat('\n4. CONVENTIONAL AVOIDANCE + ISOLATION\n')
conv_det  = rd(reform_id, 'conventional/detail', paste0(year, '.csv'))
w_static  = sum(ref_det$liab_wealth  * ref_det$weight)  / 1e9
w_conv    = sum(conv_det$liab_wealth * conv_det$weight) / 1e9
chk(w_conv < w_static,
    sprintf('conventional wealth tax < static (avoidance): $%.2fB < $%.2fB', w_conv, w_static),
    sprintf('conventional wealth tax NOT below static: $%.2fB vs $%.2fB', w_conv, w_static))
cat(sprintf('     avoidance share: %.1f%%\n', 100 * (1 - w_conv / max(w_static, 1e-12))))

# estate totals must be identical static vs conventional (value.* untouched)
es <- rd(reform_id, 'static/totals/estate.csv')
ec <- rd(reform_id, 'conventional/totals/estate.csv')
es_y = es$est_tax_exp[es$year == year]; ec_y = ec$est_tax_exp[ec$year == year]
chk(abs(es_y - ec_y) < 1e-6 * max(1, abs(es_y)),
    sprintf('estate tax unchanged static vs conventional ($%.4fB both) — isolation holds', es_y),
    sprintf('estate tax DIFFERS static ($%.4fB) vs conventional ($%.4fB) — isolation BROKEN', es_y, ec_y))

# net_worth should differ static vs conv for top records (avoidance shrinks it)
nw_join = ref_det %>% select(id, nw_static = net_worth) %>%
  inner_join(conv_det %>% select(id, nw_conv = net_worth), by = 'id') %>%
  filter(nw_static > exemption)
if (nrow(nw_join) > 0) {
  chk(mean(nw_join$nw_conv <= nw_join$nw_static + 1) == 1 & any(nw_join$nw_conv < nw_join$nw_static),
      'net_worth shrinks (or holds) under avoidance for above-exemption records',
      'net_worth did NOT shrink under avoidance')
}

# --- 5. Distribution: wealth tax concentrated at the top ---------------------
cat('\n5. DISTRIBUTION CONCENTRATION\n')
dist_files = list.files(file.path(run_root, reform_id, 'static/supplemental'),
                        pattern = 'distribution.*\\.csv$', full.names = TRUE)
cat('     distribution files:', length(dist_files), '\n')
if (length(dist_files) > 0) for (f in head(dist_files, 3)) cat('       ', basename(f), '\n')

cat('\n=================================================================\n')
cat('VALIDATION COMPLETE\n')
cat('=================================================================\n')

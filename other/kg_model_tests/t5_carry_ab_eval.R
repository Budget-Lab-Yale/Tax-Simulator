#-------------------------------------------------------------------------------
# t5_carry_ab_eval.R
#
# T5 evaluation for the wealth-carry channel: full-sample A/B between
#   PRE  = commit 4e95d0904 (entity-shifting tau_eq, no carry)  -> carry_ab_pre
#   POST = commit 47a31b1d2 (wealth-carry h)                    -> carry_ab_post
# runscript config/runscripts/top_tax/carry_ab.csv (2027:2037, pct_sample 1,
# SIGMA_TAU_EQ_FDCHECK=1 on both sides).
#
# Checks (plan T5, window-level expectations — review fix):
#   1. baseline: byte-identical everywhere (xlsx excluded — docProps stamp)
#   2. pc_ordr50_cgr30 (no-wealth kg control): revenue/detail/totals/
#      distribution CSVs byte-identical; kg diagnostics differ ONLY by the
#      all-zero carry_h/tau_w columns
#   3. carry_nomtr (guarantee path, net_worth NOT in mtr_vars): POST static
#      detail carries mtr_net_worth (the run.R fallback fired); static
#      totals byte-identical to PRE
#   4. pc_cgr30_wealthr3t500 (wealth dial): carry_h_avg_gw > 0 and matches
#      an independent gain-weighted recomputation from static detail +
#      inputs cache; R_S_total UP over the window; gain-weighted mean
#      tau_eq_S UP (sigma conversion down); log(r_D) response ~ monotone in
#      cell h; young-cell MC wedge ~ -h*bs/(1-bs) magnitude sanity
#
# Sbatch-only: sbatch other/kg_model_tests/t5_carry_ab_eval.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
PRE  = file.path(ROOT, 'carry_ab_pre')
POST = file.path(ROOT, 'carry_ab_post')
YEARS = 2027:2037
WINDOW = 2027:2036

fails = character()
note  = function(...) cat(sprintf(...), '\n')
check = function(cond, what) {
  if (isTRUE(cond)) { note('PASS  %s', what) } else {
    fails <<- c(fails, what); note('FAIL  %s', what)
  }
}

# Byte-compare every regular file under a scenario subtree, minus exclusions.
tree_diff = function(rel, exclude_regex = '\\.xlsx$') {
  a = file.path(PRE, rel); b = file.path(POST, rel)
  fa = list.files(a, recursive = TRUE, full.names = FALSE)
  fb = list.files(b, recursive = TRUE, full.names = FALSE)
  fa = fa[!grepl(exclude_regex, fa)]; fb = fb[!grepl(exclude_regex, fb)]
  out = list(only_pre = setdiff(fa, fb), only_post = setdiff(fb, fa))
  common = intersect(fa, fb)
  differing = character()
  for (f in common) {
    same = tryCatch(
      identical(readBin(file.path(a, f), 'raw', file.size(file.path(a, f))),
                readBin(file.path(b, f), 'raw', file.size(file.path(b, f)))),
      error = function(e) FALSE)
    if (!same) differing = c(differing, f)
  }
  out$differing = differing
  out
}

#===============================================================================
# 1. Baseline byte-identical
#===============================================================================

d = tree_diff('baseline')
check(length(d$differing) == 0 && length(d$only_pre) == 0 &&
        length(d$only_post) == 0,
      'baseline: all files byte-identical (xlsx excluded)')
if (length(d$differing) > 0) note('  differing: %s',
                                  paste(head(d$differing, 10), collapse = ', '))

#===============================================================================
# 2. No-wealth kg control byte-identical (carry diagnostics excluded, then
#    verified all-zero)
#===============================================================================

kg_diag = 'kg_dynamics_(age_profile|summary)\\.csv$|kg_dynamics_state/|kg_dynamics_mech_state/'
d = tree_diff('pc_ordr50_cgr30',
              exclude_regex = paste0('\\.xlsx$|', kg_diag))
check(length(d$differing) == 0 && length(d$only_pre) == 0 &&
        length(d$only_post) == 0,
      'pc_ordr50_cgr30: revenue/detail/totals/distribution byte-identical')
if (length(d$differing) > 0) note('  differing: %s',
                                  paste(head(d$differing, 10), collapse = ', '))

ap_post = read_csv(file.path(POST, 'pc_ordr50_cgr30/conventional/supplemental',
                             'kg_dynamics_age_profile.csv'),
                   show_col_types = FALSE)
ap_pre  = read_csv(file.path(PRE, 'pc_ordr50_cgr30/conventional/supplemental',
                             'kg_dynamics_age_profile.csv'),
                   show_col_types = FALSE)
check(all(c('carry_h', 'tau_w') %in% names(ap_post)) &&
        all(ap_post$carry_h == 0) && all(ap_post$tau_w == 0),
      'pc_ordr50_cgr30: POST carry columns exist and are all-zero')
shared = intersect(names(ap_post), names(ap_pre))
check(isTRUE(all.equal(ap_post[shared], ap_pre[shared], tolerance = 0)),
      'pc_ordr50_cgr30: kg age profile identical on shared columns')

#===============================================================================
# 3. Guarantee path (carry_nomtr): fallback wrote mtr_net_worth; static
#    totals identical
#===============================================================================

hdr = names(fread(file.path(POST, 'carry_nomtr/static/detail/2036.csv'),
                  nrows = 0))
check('mtr_net_worth' %in% hdr,
      'carry_nomtr: POST static detail has mtr_net_worth (run.R fallback)')
hdr_pre = names(fread(file.path(PRE, 'carry_nomtr/static/detail/2036.csv'),
                      nrows = 0))
note('  (PRE has mtr_net_worth: %s — expected FALSE)',
     'mtr_net_worth' %in% hdr_pre)

# Totals aggregate every mtr_* column, so POST's 1040 totals legitimately
# gain ONE column (mtr_net_worth) — the same schema the registered-mtr path
# produces. Check: extra columns are exactly {mtr_net_worth}, and every
# shared column is identical.
for (tf in c('1040.csv', '1040_by_agi.csv', 'estate.csv', 'payroll.csv',
             'receipts.csv', 'receipts_full.csv', 'wealth.csv')) {
  ta = read_csv(file.path(PRE,  'carry_nomtr/static/totals', tf),
                show_col_types = FALSE)
  tb = read_csv(file.path(POST, 'carry_nomtr/static/totals', tf),
                show_col_types = FALSE)
  extra  = setdiff(names(tb), names(ta))
  shared = intersect(names(ta), names(tb))
  check(all(extra %in% 'mtr_net_worth') && length(setdiff(names(ta), names(tb))) == 0 &&
          isTRUE(all.equal(ta[shared], tb[shared], tolerance = 0)),
        sprintf('carry_nomtr: static totals %s identical on shared cols (extra = mtr_net_worth only)', tf))
}

#===============================================================================
# 4. Wealth dial: carry live and correctly sized
#===============================================================================

sm_post = read_csv(file.path(POST,
                             'pc_cgr30_wealthr3t500/conventional/supplemental',
                             'kg_dynamics_summary.csv'), show_col_types = FALSE)
sm_pre  = read_csv(file.path(PRE,
                             'pc_cgr30_wealthr3t500/conventional/supplemental',
                             'kg_dynamics_summary.csv'), show_col_types = FALSE)

check(all(sm_post$carry_h_avg_gw[sm_post$year >= 2027] > 0),
      'wealth dial: carry_h_avg_gw > 0 every year')

# Independent recomputation of carry_h_avg_gw from static detail + cache:
# overall gain-weighted mean of mtr_net_worth * mtr_kg_lt equals the
# G_B-weighted mean over cells of the gain-weighted cell h.
cache = readRDS(file.path(POST, 'pc_cgr30_wealthr3t500/static/supplemental',
                          'kg_dynamics_mech_state/inputs_cache.rds'))
yr_chk = 2032
td = cache$td_slim_by_year[[as.character(yr_chk)]]
det = fread(file.path(POST, 'pc_cgr30_wealthr3t500/static/detail',
                      paste0(yr_chk, '.csv')),
            select = c('id', 'mtr_net_worth', 'mtr_kg_lt'),
            showProgress = FALSE) %>% as_tibble()
h_indep = td %>% left_join(det, by = 'id') %>%
  summarise(h = sum(weight * G_unit * coalesce(mtr_net_worth, 0) *
                      coalesce(mtr_kg_lt, 0)) / sum(weight * G_unit)) %>%
  pull(h)
h_summary = sm_post$carry_h_avg_gw[sm_post$year == yr_chk]
note('  carry_h_avg_gw %d: summary %.6f vs independent %.6f', yr_chk,
     h_summary, h_indep)
check(abs(h_summary - h_indep) < 1e-6,
      'wealth dial: carry_h_avg_gw matches independent recomputation')

# R_S_total UP over the window (window-level, not every-year)
rs_post = sum(sm_post$R_S_total[sm_post$year %in% WINDOW])
rs_pre  = sum(sm_pre $R_S_total[sm_pre $year %in% WINDOW])
note('  R_S_total window sum: pre %.4e post %.4e (delta %+.3f%%)',
     rs_pre, rs_post, 100 * (rs_post / rs_pre - 1))
check(rs_post > rs_pre,
      'wealth dial: R_S_total UP over 2027-2036 window (carry unlocks gains)')

# tau_eq_S up gain-weighted (sigma conversion down)
apw_post = read_csv(file.path(POST,
                              'pc_cgr30_wealthr3t500/conventional/supplemental',
                              'kg_dynamics_age_profile.csv'),
                    show_col_types = FALSE)
apw_pre  = read_csv(file.path(PRE,
                              'pc_cgr30_wealthr3t500/conventional/supplemental',
                              'kg_dynamics_age_profile.csv'),
                    show_col_types = FALSE)
te = function(df) df %>% filter(year %in% WINDOW) %>%
  summarise(v = sum(tau_eq_S * G_B) / sum(G_B)) %>% pull(v)
note('  gain-weighted tau_eq_S: pre %.5f post %.5f', te(apw_pre), te(apw_post))
check(te(apw_post) > te(apw_pre),
      'wealth dial: mean tau_eq_S UP (sigma conversion falls)')

# FOC identity across runs: both sides share Pass-1 kappa and r_D_B (their
# baselines are byte-identical), so on unclipped cells
#   log(r_D_S_post / r_D_S_pre) == -eta * (MC_S_post - MC_S_pre)
# exactly. (Current-year cell h is NOT the right regressor — dlog responds
# to the PV of the whole future h path, and young cells inherit the large
# old-age h through W_next — so a monotonicity-in-current-h check is
# misconceived; the identity below is the controlled-primitives content of
# the plan's monotonicity expectation, and T1/T3b cover the rest.)
ETA_LIVE = as.numeric(Sys.getenv('KG_ETA', '2.3992'))
mono = apw_post %>%
  select(year, age, carry_h, r_D_S_post = r_D_S, MC_S_post = MC_S) %>%
  inner_join(apw_pre %>% select(year, age, r_D_S_pre = r_D_S,
                                MC_S_pre = MC_S),
             by = c('year', 'age')) %>%
  filter(r_D_S_pre > 1e-12, r_D_S_pre < 1 - 1e-9, r_D_S_post < 1 - 1e-9) %>%
  mutate(dlog = log(r_D_S_post / r_D_S_pre),
         foc  = -ETA_LIVE * (MC_S_post - MC_S_pre))
foc_err = max(abs(mono$dlog - mono$foc))
note('  FOC identity: max |dlog - (-eta*dMC)| = %.3e over %d unclipped cells',
     foc_err, nrow(mono))
check(foc_err < 1e-8,
      'wealth dial: cross-run FOC identity dlog r_D_S = -eta*dMC_S (unclipped)')
check(all(mono$dlog >= -1e-12), 'wealth dial: r_D_S never falls under carry')

# Young-cell MC wedge magnitude sanity: dMC ~ -h * bs/(1-bs), bs = beta(1-m)
BETA_APPROX = 0.978
young = apw_post %>% filter(year == 2032, age <= 30) %>%
  select(age, carry_h, m, MC_S_post = MC_S) %>%
  inner_join(apw_pre %>% filter(year == 2032) %>%
               select(age, MC_S_pre = MC_S), by = 'age') %>%
  mutate(bs    = BETA_APPROX * (1 - m),
         dMC   = MC_S_post - MC_S_pre,
         approx = -carry_h * bs / (1 - bs))
note('  young-cell MC wedge (2032, ages<=30): mean dMC %.5f vs stationary approx %.5f',
     mean(young$dMC), mean(young$approx))
check(sign(sum(young$dMC)) <= 0,
      'wealth dial: young-cell MC wedge is negative (deferral penalized)')

# Conventional revenue direction note (not a gate: composition effects ride
# along) — total conventional revenue-estimate delta, post vs pre ($B)
rev_path = 'pc_cgr30_wealthr3t500/conventional/supplemental/revenue_estimates.csv'
if (file.exists(file.path(PRE, rev_path))) {
  rv_pre  = read_csv(file.path(PRE, rev_path), show_col_types = FALSE)
  rv_post = read_csv(file.path(POST, rev_path), show_col_types = FALSE)
  tot_pre  = sum(rv_pre $total[rv_pre $year %in% WINDOW])
  tot_post = sum(rv_post$total[rv_post$year %in% WINDOW])
  note('  conventional revenue estimate, 2027-2036 sum: pre %.2f post %.2f (delta %+.2f $B)',
       tot_pre, tot_post, tot_post - tot_pre)
}

#===============================================================================

if (length(fails) > 0) {
  stop('T5 A/B FAILED checks:\n  ', paste(fails, collapse = '\n  '))
}
cat('\nT5 A/B: ALL CHECKS PASSED\n')

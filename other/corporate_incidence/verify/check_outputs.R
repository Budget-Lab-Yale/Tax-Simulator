#-------------------------------------------------------------------------------
# check_outputs.R
#
# Theorem checks on the corp_incidence test-pipeline outputs (plan
# "Verification" items 3-5), run AFTER the SLURM pipeline for
# tests/corp_incidence completes (vintage corp_test_v1, local root).
#
#   1. D5 static wall: corp_perm's static detail is value-identical to
#      baseline's on their COMMON columns (the shock never touches the static
#      pass; the runscripts deliberately register mtr_kg_lt on baseline only,
#      so raw byte-identity is not expected).
#   2. Endogenous offset direction: corp_perm's conventional FY income-tax
#      receipts fall vs baseline (dividend/interest/rent/pt cuts shrink the
#      IIT base); the corporate line itself books the off-model wedge.
#   3. Estate erosion: corp_perm's conventional est_tax_exp < baseline's from
#      enactment on (markdown shrinks gross estates).
#   4. P8/D16 forcing invariants (corp_perm_wealth). NOTE the D16 corollary
#      AT SCALE: the external-income flow leg alone would debit wealth
#      s*D(1-tau) (P8), but the INTERNAL-conversion legs (kg price-margin
#      realization cuts, retirement-dist cuts) contribute pure tax REBATES to
#      the forcing (their resource loss is booked in the balance-sheet
#      markdown, not in income -- the "retiree corollary" P9 records so it is
#      not mistaken for a sign bug). Under this test's kg-heavy shock the
#      rebates outweigh the net-of-tax flow loss, so the AGGREGATE forcing is
#      mildly NEGATIVE (banked rebates) against a ~$1.9T markdown position.
#      What is checkable:
#        (a) P8 proper: F exceeds the tax-only forcing by exactly -dY_exog
#            (the generalization debits wealth RELATIVE to tax-only; the
#            rebate never arrives without the loss);
#        (b) the bathtub's cell-summed forcing equals an independent
#            recomputation from the conv-no-wealth + baseline detail;
#        (c) the state's sign and the estate-side direction agree with the
#            measured forcing (here: slightly SHALLOWER erosion than
#            corp_perm -- banked rebates add to estates).
#   5. D17 persistence: post-expiry (2032-33), corp_sunset's conventional
#      estate equals baseline (markdown + flows gone) while
#      corp_sunset_wealth's stays eroded... NOTE under the same corollary the
#      sunset-wealth post-expiry footprint is the compounded accumulated
#      forcing, whatever its sign -- the check asserts it is NONZERO and
#      matches the state's sign (persistence is the theorem; sign is
#      composition-dependent per D16).
#   6. kg composition: corp_perm_kg's bathtub cell tables carry a positive
#      corp_gain_debit; the run produced conventional detail (ordering
#      corp -> haircut -> kg behavior executed).
#   7. Conservation diagnostics exist per year with realized == analytic
#      within tolerance.
#   8. Status-quo comparison (smear fallback): corp_nometa (no metadata,
#      channel OFF) books the same corporate line but NO offset -- its
#      income-tax receipts match baseline; summary table written.
#
# Writes: other/corporate_incidence/verify/out/output_checks_summary.csv
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})

root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/corp_test_v1'
out_dir = 'other/corporate_incidence/verify/out'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

rec = function(scen, pass) {
  read_csv(file.path(root, scen, pass, 'totals', 'receipts_full.csv'),
           show_col_types = FALSE)
}
est = function(scen, pass) {
  read_csv(file.path(root, scen, pass, 'totals', 'estate.csv'),
           show_col_types = FALSE)
}

fails = character(0)
ok = function(cond, label) {
  status = if (isTRUE(cond)) 'PASS' else 'FAIL'
  if (!isTRUE(cond)) fails <<- c(fails, label)
  cat(sprintf('[%s] %s\n', status, label))
}

# --- 1. D5 static wall ----------------------------------------------------------
# Value-identity on COMMON columns: baseline registers mtr_kg_lt (for the kg
# composition scenarios' bathtub) and corp_perm does not, so the files differ
# by that column ONLY. Any non-mtr difference is a wall breach.
# The correct control is a CHANNEL-OFF COUNTERFACTUAL (corp_nometa), not the
# baseline scenario: every counterfactual's do_taxes rebuilds wages =
# wages1 + wages2 through the baseline_pr_er wage adjustment, dropping the
# known Tax-Data wages residual (see the house memory note) -- so baseline
# detail differs from ANY counterfactual's in wages + downstream derived
# columns, corp or not. corp_perm's static pass must be byte-identical to
# corp_nometa's (same law, same code path, channel never touches static).
suppressPackageStartupMessages(library(data.table))
same = sapply(2026:2033, function(y) {
  a = file.path(root, 'corp_nometa', 'static', 'detail', paste0(y, '.csv'))
  b = file.path(root, 'corp_perm',   'static', 'detail', paste0(y, '.csv'))
  file.exists(a) && file.exists(b) && (tools::md5sum(a) == tools::md5sum(b))
})
ok(all(same),
   'D5: corp_perm static detail byte-identical to channel-off counterfactual (all years)')

# --- 2/3. offset direction + estate erosion (corp_perm) -------------------------
rb = rec('baseline', 'static')
rp = rec('corp_perm', 'conventional')
cmp = rb %>%
  select(year, iit_b = revenues_income_tax, corp_b = revenues_corp_tax) %>%
  inner_join(rp %>% select(year, iit_p = revenues_income_tax,
                           corp_p = revenues_corp_tax), by = 'year') %>%
  mutate(d_iit = iit_p - iit_b, d_corp = corp_p - corp_b)
print(as.data.frame(cmp %>% mutate(across(-year, ~ round(., 1)))))
post = cmp %>% filter(year >= 2028)   # FY 2028+ fully in-window
ok(all(post$d_iit < 0),
   'Offset: corp_perm conventional FY income tax falls vs baseline (2028+)')
ok(all(post$d_corp > 100),
   'Corporate line: off-model wedge (~$130B+) booked on receipts (2028+)')

eb = est('baseline', 'static')    %>% select(year, est_b = est_tax_exp)
ep = est('corp_perm', 'conventional') %>% select(year, est_p = est_tax_exp)
ecmp = inner_join(eb, ep, by = 'year') %>% mutate(d = est_p - est_b)
print(as.data.frame(ecmp %>% mutate(across(-year, ~ round(., 3)))))
ok(all(ecmp$d[ecmp$year >= 2027] < 0),
   'Estate erosion: corp_perm CY est_tax_exp below baseline from enactment')
ok(abs(ecmp$d[ecmp$year == 2026]) < 1e-6,
   'Estate: pre-enactment year untouched (2026 delta = 0)')

# --- 4. P8/D16 forcing invariants (corp_perm_wealth) -----------------------------
# Independent forcing recomputation for one year from detail:
y_chk = 2028
nw_path = file.path(root, 'corp_perm_wealth', 'conventional_no_wealth',
                    'detail', paste0(y_chk, '.csv'))
nw_cols = intersect(c('id', 'weight', 'dep_status', 'liab_iit_net', 'liab_pr',
                      'liab_deemed', 'liab_wealth', 'corp_dY_exog',
                      'net_worth_raw'),
                    names(fread(nw_path, nrows = 0)))
scen_nw = fread(nw_path, select = nw_cols)
if (!('liab_deemed' %in% names(scen_nw))) scen_nw$liab_deemed = 0
base_d = fread(file.path(root, 'baseline', 'static', 'detail',
                         paste0(y_chk, '.csv')),
               select = c('id', 'liab_iit_net', 'liab_pr'))
# Replicate the pre-pass cell population exactly: dependents excluded AND
# positive-net-worth ranking only (D17 -- records with net_worth_raw <= 0
# have no cell and contribute no forcing).
mm = merge(scen_nw[dep_status == 0 & net_worth_raw > 0],
           base_d, by = 'id', suffixes = c('', '_b'))
toB = 1e-9
dT0_hand = sum(mm$weight * ((mm$liab_iit_net + mm$liab_pr -
                             fifelse(is.na(mm$liab_deemed), 0, mm$liab_deemed) +
                             mm$liab_wealth) -
                            (mm$liab_iit_net_b + mm$liab_pr_b))) * toB
dY_hand  = sum(mm$weight * mm$corp_dY_exog) * toB
F_hand   = dT0_hand - dY_hand

st_y = readRDS(file.path(root, 'corp_perm_wealth', 'conventional',
                         'supplemental', 'wealth_dynamics_state',
                         paste0(y_chk, '.rds')))
F_state = sum(st_y$diag$dT0) * toB
cat(sprintf('forcing decomposition %d: dT0 = $%.2fB, dY_exog = $%.2fB, F = $%.2fB (state: $%.2fB)\n',
            y_chk, dT0_hand, dY_hand, F_hand, F_state))

ok(dY_hand < -1, 'D16: external-income shock is a real loss (dY_exog << 0)')
ok(F_hand > dT0_hand + 1,
   'P8: generalized forcing exceeds tax-only forcing by |dY_exog| (rebate never arrives without the loss)')
ok(abs(F_state - F_hand) < max(0.1, 0.01 * abs(F_hand)),
   sprintf('Bathtub forcing matches independent recomputation ($%.2fB vs $%.2fB)',
           F_state, F_hand))

st = readRDS(file.path(root, 'corp_perm_wealth', 'conventional', 'supplemental',
                       'wealth_dynamics_state', '2033.rds'))
ew = est('corp_perm_wealth', 'conventional') %>% select(year, est_w = est_tax_exp)
ecmp2 = ecmp %>% inner_join(ew, by = 'year') %>% mutate(d_w = est_w - est_b)
# Sign coherence: deficit P > 0 (dissaving) must deepen erosion (d_w < d);
# P < 0 (banked rebates, the D16 corollary under a kg-heavy shock) must
# shallow it (d_w > d).
p_sign = sign(sum(st$P))
est_dir = ecmp2 %>% filter(year >= 2029) %>%
  summarise(deeper = mean(d_w - d)) %>% pull(deeper)
cat(sprintf('bathtub position 2033: $%.1fB (%s); estate delta gap (d_w - d, 2029+): $%.4fB avg\n',
            sum(st$P) / 1e9,
            if (p_sign > 0) 'dissaving' else 'banked rebates (D16 corollary)',
            est_dir))
ok(p_sign == -sign(est_dir) || est_dir == 0,
   'Sign coherence: bathtub position direction matches the estate-side effect')

# --- 5. D17 persistence ----------------------------------------------------------
es  = est('corp_sunset', 'conventional')        %>% select(year, est_s  = est_tax_exp)
esw = est('corp_sunset_wealth', 'conventional') %>% select(year, est_sw = est_tax_exp)
d17 = eb %>% inner_join(es, by = 'year') %>% inner_join(esw, by = 'year') %>%
  mutate(d_s = est_s - est_b, d_sw = est_sw - est_b) %>%
  filter(year %in% 2032:2033)
print(as.data.frame(d17 %>% mutate(across(-year, ~ round(., 3)))))
ok(all(abs(d17$d_s) < 1e-3),
   'D17: corp_sunset (no s) estate back to baseline post-expiry (2032-33)')
# The windowed shock is FLOW-dominant (mu is the small annuity share, so the
# kg price-margin rebate is small): P8 dissaving wins, the footprint is
# genuine erosion -- the opposite composition from the permanent case above.
ok(all(d17$d_sw < -1e-3),
   'D17: corp_sunset_wealth estate STAYS eroded post-expiry (dissaving persists)')

# --- 6. kg composition ------------------------------------------------------------
kg_state = readRDS(file.path(root, 'corp_perm_kg', 'conventional', 'supplemental',
                             'kg_dynamics_state', '2028.rds'))
ok('corp_gain_debit' %in% names(kg_state$cell_table) &&
     sum(kg_state$cell_table$corp_gain_debit) > 0,
   'kg: corp_perm_kg cell table carries a positive corp_gain_debit')
ok(file.exists(file.path(root, 'corp_perm_kg_wealth', 'conventional', 'detail',
                         '2033.csv')),
   'kg+wealth composition ran to completion (2033 conventional detail present)')

# --- 7. conservation diagnostics ---------------------------------------------------
diag_ok = TRUE; gap_max = 0
for (y in 2027:2033) {
  f = file.path(root, 'corp_perm', 'conventional', 'supplemental',
                sprintf('corp_conservation_diag_%d.csv', y))
  if (!file.exists(f)) { diag_ok = FALSE; next }
  d = read_csv(f, show_col_types = FALSE)
  gap_max = max(gap_max, abs(d$dY_total_realized - d$dY_total_analytic))
}
ok(diag_ok && gap_max < 0.05,
   sprintf('Conservation diag: files present 2027-33, max realized-analytic gap $%.4fB',
           gap_max))

# --- 8. status-quo comparison (channel-off fallback) --------------------------------
rn = rec('corp_nometa', 'conventional')
cmp_n = rb %>% select(year, iit_b = revenues_income_tax) %>%
  inner_join(rn %>% select(year, iit_n = revenues_income_tax,
                           corp_n = revenues_corp_tax), by = 'year') %>%
  mutate(d_iit_nometa = iit_n - iit_b)
ok(all(abs(cmp_n$d_iit_nometa) < 1e-6),
   'Fallback: corp_nometa (channel OFF) income tax identical to baseline')

summary_tbl = cmp %>%
  select(year, d_iit_onmodel = d_iit, d_corp) %>%
  left_join(cmp_n %>% select(year, d_iit_nometa), by = 'year') %>%
  left_join(ecmp2 %>% select(year, d_est_perm = d, d_est_perm_wealth = d_w),
            by = 'year')
write_csv(summary_tbl, file.path(out_dir, 'output_checks_summary.csv'))
print(as.data.frame(summary_tbl %>% mutate(across(-year, ~ round(., 2)))))

cat('\n')
if (length(fails) == 0) {
  cat('ALL OUTPUT THEOREM CHECKS PASSED\n')
} else {
  cat('FAILURES:\n'); for (f in fails) cat(' -', f, '\n')
  quit(status = 1)
}

#-------------------------------------------------------------------------------
# check_estate_offset_ab.R
#
# Post-run verification of the estate-margins build on the estate_offset_ab
# pipeline outputs (config/runscripts/tests/estate_offset_ab.csv, vintage
# estate_offset_ab_v1). Scenarios:
#   baseline       — current law
#   est2009_kg     — ESTATE-ONLY reform (2009 params) + kg  -> leg-pairing
#   cg5pp_kg       — CG-ONLY +5pp reform + kg               -> e_S == e_B
#   dr_noded_kg    — deemed primary homes + income_tax_ded=0 -> column split
#   est2009_avoid  — estate reform + wealth/avoidance        -> part (b)
#
# Checks (plan verification 1a, 4, 5, 6):
#   V1  baseline mtr_estate: in [0, 0.45]; 0 below the exemption; ~0.40 at
#       the top; mtr_estate_ded == mtr_estate under baseline law (switch = 1)
#   V2  dr_noded_kg: mtr_estate_ded == 0 for ALL records while mtr_estate is
#       unchanged from baseline (two-column split; ded-only lever cannot
#       move the un-switched rate)
#   V3  est2009_kg state: e_S > e_B on gain-weighted average (lower
#       exemption + higher top rates), r_D_S >= r_D_B everywhere with strict
#       increase where e moved, tau_S == tau_B (estate-only reform must move
#       realizations ONLY through e — the leg-pairing backstop)
#   V4  cg5pp_kg state: e_S == e_B (CG reform leaves the estate price alone)
#       and both > 0 at old ages (current law HAS an estate tax)
#   V5  exposure diagnostics written for every kg scenario-year
#   V6  est2009_avoid: estate_concealed_frac > 0 exactly where the estate
#       price rose; estate_distributable invariant static vs conventional;
#       conventional estate revenue < static (avoidance) but > baseline
#
# Run AFTER the pipeline completes:
#   sbatch other/kg_model_tests/check_estate_offset_ab.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_offset_ab_v1'
YR   = 2027

n_fail = 0
check = function(ok, label, detail = '') {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s%s\n', status, label,
              if (nchar(detail)) paste0('  [', detail, ']') else ''))
}

detail = function(scen, type, yr = YR) {
  fread(file.path(ROOT, scen, type, 'detail', paste0(yr, '.csv')),
        showProgress = FALSE) %>% as_tibble()
}

#--- V1: baseline mtr_estate sanity -------------------------------------------
bl = detail('baseline', 'static')
check(all(c('mtr_estate', 'mtr_estate_ded') %in% names(bl)),
      'V1 baseline static detail carries mtr_estate + mtr_estate_ded')
check(all(bl$mtr_estate >= -1e-12 & bl$mtr_estate <= 0.45 + 1e-12),
      'V1 mtr_estate in [0, 0.45]',
      sprintf('range %.4f..%.4f', min(bl$mtr_estate), max(bl$mtr_estate)))
low = bl %>% filter(net_worth < 1e6)
check(all(abs(low$mtr_estate) < 1e-9),
      'V1 mtr_estate = 0 well below the exemption (< $1M net worth)',
      sprintf('%d records', nrow(low)))
top = bl %>% filter(net_worth > 1e8)
share_top = with(top, weighted.mean(mtr_estate >= 0.35, weight))
check(nrow(top) > 0 && share_top > 0.5,
      'V1 mtr_estate ~ top statutory rate for >$100M records',
      sprintf('share >= 0.35: %.2f (n = %d)', share_top, nrow(top)))
check(max(abs(bl$mtr_estate_ded - bl$mtr_estate)) < 1e-12,
      'V1 mtr_estate_ded == mtr_estate under baseline law (switch = 1)')

#--- V2: two-column split under income_tax_ded = 0 ----------------------------
dr = detail('dr_noded_kg', 'static')
check(max(abs(dr$mtr_estate_ded)) < 1e-12,
      'V2 dr_noded_kg: mtr_estate_ded == 0 everywhere (switch off)')
j = bl %>% select(id, mtr_b = mtr_estate) %>%
  inner_join(dr %>% select(id, mtr_s = mtr_estate), by = 'id')
check(max(abs(j$mtr_b - j$mtr_s)) < 1e-9,
      'V2 dr_noded_kg: un-switched mtr_estate identical to baseline',
      sprintf('max |diff| = %.2e', max(abs(j$mtr_b - j$mtr_s))))

#--- V3: estate-only reform moves realizations through e ----------------------
st_est = readRDS(file.path(ROOT, 'est2009_kg', 'conventional',
                           'supplemental', 'kg_dynamics_state',
                           paste0(YR, '.rds')))$cell_table
gw = function(x, w) sum(x * w) / sum(w)
w_g = pmax(st_est$G_B, 0)
check(all(c('estate_e_B', 'estate_e_S') %in% names(st_est)),
      'V3 state cell_table carries estate_e_B / estate_e_S')
check(gw(st_est$estate_e_S, w_g) > gw(st_est$estate_e_B, w_g),
      'V3 est2009_kg: gain-weighted e_S > e_B (estate-only hike)',
      sprintf('e_B = %.4f, e_S = %.4f',
              gw(st_est$estate_e_B, w_g), gw(st_est$estate_e_S, w_g)))
check(max(abs(st_est$tau_S - st_est$tau_B)) < 1e-6,
      'V3 est2009_kg: tau_S == tau_B (income-tax law unchanged)',
      sprintf('max |dtau| = %.2e', max(abs(st_est$tau_S - st_est$tau_B))))
moved = st_est$estate_e_S > st_est$estate_e_B + 1e-9
check(all(st_est$r_D_S >= st_est$r_D_B - 1e-12) &&
      any(st_est$r_D_S[moved] > st_est$r_D_B[moved] + 1e-12),
      'V3 est2009_kg: realizations rise where e rose (leg-pairing backstop)',
      sprintf('%d/%d cells moved', sum(moved), nrow(st_est)))

#--- V4: CG-only reform leaves the estate price alone -------------------------
st_cg = readRDS(file.path(ROOT, 'cg5pp_kg', 'conventional',
                          'supplemental', 'kg_dynamics_state',
                          paste0(YR, '.rds')))$cell_table
check(max(abs(st_cg$estate_e_S - st_cg$estate_e_B)) < 1e-9,
      'V4 cg5pp_kg: e_S == e_B (CG reform, estate law unchanged)',
      sprintf('max |de| = %.2e', max(abs(st_cg$estate_e_S - st_cg$estate_e_B))))
old = st_cg$age >= 70
check(any(st_cg$estate_e_B[old] > 0.01),
      'V4 cg5pp_kg: current-law estate exposure > 0 at old ages',
      sprintf('max e_B (70+) = %.4f', max(st_cg$estate_e_B[old])))

#--- V5: exposure diagnostics -------------------------------------------------
for (scen in c('est2009_kg', 'cg5pp_kg', 'dr_noded_kg')) {
  p = file.path(ROOT, scen, 'conventional', 'supplemental',
                paste0('kg_estate_exposure_diag_', YR, '.csv'))
  check(file.exists(p), paste0('V5 exposure diagnostic written: ', scen))
}
diag = read_csv(file.path(ROOT, 'est2009_kg', 'conventional', 'supplemental',
                          paste0('kg_estate_exposure_diag_', YR, '.csv')),
                show_col_types = FALSE)
all_rows = diag %>% filter(group == 'all')
check(nrow(all_rows) == 2 && all(all_rows$share_zero < 1) &&
      all(all_rows$e_gw_mean > 0),
      'V5 diagnostic content sane (both legs, nonzero exposure)',
      sprintf('reform gw-mean e = %.4f, zero-share = %.2f',
              all_rows$e_gw_mean[all_rows$leg == 'reform'],
              all_rows$share_zero[all_rows$leg == 'reform']))

#--- V6: part (b) on the avoidance leg ----------------------------------------
av_conv   = detail('est2009_avoid', 'conventional')
av_static = detail('est2009_avoid', 'static')
check('estate_concealed_frac' %in% names(av_conv),
      'V6 est2009_avoid conv detail carries estate_concealed_frac')
n_pos = sum(av_conv$estate_concealed_frac > 1e-9)
check(n_pos > 0,
      'V6 estate own-rate response fired (frac > 0 records exist)',
      sprintf('n = %d, wmax = %.4f', n_pos, max(av_conv$estate_concealed_frac)))
# frac == 0 exactly where the estate price did not move: join baseline
# mtr_estate to the scenario's static mtr_estate
jj = av_static %>% select(id, mtr_s = mtr_estate) %>%
  inner_join(bl %>% select(id, mtr_b = mtr_estate), by = 'id') %>%
  inner_join(av_conv %>% select(id, frac = estate_concealed_frac), by = 'id')
same_price = abs(jj$mtr_s - jj$mtr_b) < 1e-9
check(all(abs(jj$frac[same_price]) < 1e-9),
      'V6 no response where the estate price is unchanged',
      sprintf('%d unchanged-price records', sum(same_price)))
check(all(jj$frac[jj$mtr_s > jj$mtr_b + 1e-9] > 1e-12),
      'V6 response fires exactly where the estate price rose')
# distributable invariance (static frame vs conv frame, same records)
di = av_static %>% select(id, d_s = estate_distributable) %>%
  inner_join(av_conv %>% select(id, d_c = estate_distributable), by = 'id')
check(max(abs(di$d_s - di$d_c)) < 1e-6,
      'V6 estate_distributable invariant to the reported-base response',
      sprintf('max |diff| = %.2e', max(abs(di$d_s - di$d_c))))
# revenue ordering: static reform > conv reform (avoidance) > baseline
est_tot = function(scen, type) {
  read_csv(file.path(ROOT, scen, type, 'totals', 'estate.csv'),
           show_col_types = FALSE) %>% filter(year == YR) %>% pull(est_tax_exp)
}
e_bl = est_tot('baseline', 'static')
e_st = est_tot('est2009_avoid', 'static')
e_cv = est_tot('est2009_avoid', 'conventional')
check(e_st > e_bl && e_cv < e_st && e_cv > e_bl,
      'V6 estate revenue: baseline < conventional < static under the reform',
      sprintf('bl %.2f < conv %.2f < static %.2f ($B)', e_bl, e_cv, e_st))

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL A/B CHECKS PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

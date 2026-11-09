#-------------------------------------------------------------------------------
# check_estate_offset_ab2.R
#
# SINGLE-CHANNEL verification of the estate-margins build on the PURE
# estate-only 2009 reform (tests/estate2009_only: warren_estate2009's estate
# parameters WITHOUT its wealth tax — the first A/B's est2009 legs conflated
# the estate offset with the wealth-carry h and wealth-concealment channels).
# Vintage estate_offset_ab2_v1, baseline reused from estate_offset_ab_v1.
#
#   W1  est2009_pure_kg: NO wealth tax => carry_h == 0 in state; e_S > e_B;
#       tau_S == tau_B; realizations rise where e rose — the effect is
#       attributable to the estate offset ALONE (leg-pairing backstop, pure)
#   W2  est2009_pure_avoid: mtr_net_worth = 0 everywhere => wealth
#       concealment OFF; estate_concealed_frac = the KS response ALONE;
#       revenue ordering baseline < conv < static with the wedge = pure (b)
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

BASE = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_offset_ab_v1'
ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_offset_ab2_v1'
YR   = 2027

n_fail = 0
check = function(ok, label, detail = '') {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s%s\n', status, label,
              if (nchar(detail)) paste0('  [', detail, ']') else ''))
}

#--- W1: pure estate-only reform through kg ------------------------------------
st = readRDS(file.path(ROOT, 'est2009_pure_kg', 'conventional',
                       'supplemental', 'kg_dynamics_state',
                       paste0(YR, '.rds')))$cell_table
gw = function(x, w) sum(x * w) / sum(w)
w_g = pmax(st$G_B, 0)
check(max(abs(st$carry_h)) == 0,
      'W1 carry_h == 0 (no wealth tax => h channel silent)')
check(gw(st$estate_e_S, w_g) > gw(st$estate_e_B, w_g),
      'W1 gain-weighted e_S > e_B under the pure estate hike',
      sprintf('e_B = %.4f, e_S = %.4f',
              gw(st$estate_e_B, w_g), gw(st$estate_e_S, w_g)))
check(max(abs(st$tau_S - st$tau_B)) < 1e-6,
      'W1 tau_S == tau_B (income-tax law untouched)',
      sprintf('max |dtau| = %.2e', max(abs(st$tau_S - st$tau_B))))
moved = st$estate_e_S > st$estate_e_B + 1e-9
check(all(st$r_D_S >= st$r_D_B - 1e-12) &&
      any(st$r_D_S[moved] > st$r_D_B[moved] + 1e-12),
      'W1 realizations rise where e rose — estate offset ALONE moves them',
      sprintf('%d/%d cells moved; max dr_D = %.5f',
              sum(moved), nrow(st), max(st$r_D_S - st$r_D_B)))
# Aggregate realization response for the report
dR = sum(w_g * (st$r_D_S - st$r_D_B)) / sum(w_g * st$r_D_B)
cat(sprintf('        gain-weighted realization-rate response: %+.3f%%\n',
            100 * dR))

#--- W2: pure (b) response ------------------------------------------------------
av_conv   = fread(file.path(ROOT, 'est2009_pure_avoid', 'conventional',
                            'detail', paste0(YR, '.csv')),
                  showProgress = FALSE) %>% as_tibble()
av_static = fread(file.path(ROOT, 'est2009_pure_avoid', 'static',
                            'detail', paste0(YR, '.csv')),
                  showProgress = FALSE) %>% as_tibble()
check(max(abs(av_static$mtr_net_worth)) < 1e-12,
      'W2 mtr_net_worth == 0 (no wealth tax => wealth concealment OFF)')
bl = fread(file.path(BASE, 'baseline', 'static', 'detail',
                     paste0(YR, '.csv')),
           select = c('id', 'mtr_estate'), showProgress = FALSE)
jj = av_static %>% select(id, mtr_s = mtr_estate) %>%
  inner_join(bl %>% rename(mtr_b = mtr_estate), by = 'id') %>%
  inner_join(av_conv %>% select(id, frac = estate_concealed_frac), by = 'id')
same = abs(jj$mtr_s - jj$mtr_b) < 1e-9
check(all(abs(jj$frac[same]) < 1e-9) &&
      all(jj$frac[jj$mtr_s > jj$mtr_b + 1e-9] > 1e-12),
      'W2 estate_concealed_frac is the KS response alone (fires iff price rose)',
      sprintf('%d responding records, wmax = %.4f',
              sum(!same), max(jj$frac)))
# Exact-form spot check on the largest responder
eps = as.numeric(Sys.getenv('ESTATE_REPORT_EPS', unset = '0.16'))
i = which.max(jj$frac)
f_hand = 1 - ((1 - pmin(jj$mtr_s[i], 1 - 1e-6)) /
              (1 - pmin(jj$mtr_b[i], 1 - 1e-6)))^eps
check(abs(jj$frac[i] - f_hand) < 1e-9,
      'W2 power form exact at the record level (top responder)',
      sprintf('tau %.3f -> %.3f, f = %.5f', jj$mtr_b[i], jj$mtr_s[i], f_hand))
est_tot = function(root, scen, type) {
  read_csv(file.path(root, scen, type, 'totals', 'estate.csv'),
           show_col_types = FALSE) %>% filter(year == YR) %>% pull(est_tax_exp)
}
e_bl = est_tot(BASE, 'baseline', 'static')
e_st = est_tot(ROOT, 'est2009_pure_avoid', 'static')
e_cv = est_tot(ROOT, 'est2009_pure_avoid', 'conventional')
check(e_st > e_bl && e_cv < e_st && e_cv > e_bl,
      'W2 estate revenue: baseline < conventional < static (pure KS wedge)',
      sprintf('bl %.2f < conv %.2f < static %.2f ($B); KS giveback = %.1f%% of the static delta',
              e_bl, e_cv, e_st, 100 * (e_st - e_cv) / (e_st - e_bl)))

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL PURE-CHANNEL CHECKS PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

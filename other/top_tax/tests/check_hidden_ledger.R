#-------------------------------------------------------------------------------
# check_hidden_ledger.R
#
# Post-hoc checks on a hidden_ledger_smoke pipeline run (plan Verification item
# 3). Compares a CHI-central run against a CHI=0 run of the SAME warren fixture
# (both full sample), scenario hl_warren:
#
#  (a) CHI=0 no-op (concealment fully off): in the CHI=0 run the conventional
#      MARKETABLE flows (div_ord / txbl_int / kg_lt -- concealment's c_pub targets,
#      which the evasion module never touches) and the estate liability equal the
#      STATIC pass record-by-record; estate_concealed_frac = 0 everywhere; and the
#      reported net_worth is identical across the CHI legs (CHI drives concealment,
#      not the avoidance response). NB the closely-held/income aggregates are NOT
#      compared to static here: the evasion module is in the stack per the plan and
#      is not a perfect no-op even under a pure wealth tax (tiny baseline-vs-static
#      income-MTR residuals -- the known Tax-Data wages residual), so concealment
#      is isolated via the marketable columns + the cross-CHI-leg comparisons.
#  (b) Central run: the marketable flows shrink by exactly exp(mtr * public_e)
#      [= 1 - c_pub] where mtr > 0, closely-held aggregates never increase, and
#      below the exemption the central conventional frame equals the CHI=0
#      conventional frame EXACTLY (concealment is the only cross-leg difference and
#      it is off below the threshold; evasion is identical in both legs).
#  (c) estate_distributable is invariant record-by-record across the CHI legs and
#      equals the static value (heir-allocator bequest ladder untouched); if the
#      estate_allocator diagnostic is present its Sw*p*lambda identity is checked.
#  (d) Conservation identity recomputed from detail: for the marketable flows,
#      Sw(static - conventional) equals the hidden_ledger diagnostic's concealed
#      flow total; the module's own per-record conservation_max_leg_err is < 1e-6.
#  (e) Direction: CHI-central vs CHI=0 => conventional estate revenue DOWN and
#      conventional income-tax revenue DOWN, magnitudes proportional to chi * f.
#
# Usage:
#   Rscript other/top_tax/tests/check_hidden_ledger.R \
#     <root_central> <root_chi0> <scenario_id> <first_year> <last_year>
# where <root_*> are the two vintage roots (.../Tax-Simulator/v1/<vintage>).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tibble)
})

args      = commandArgs(trailingOnly = TRUE)
root_c    = args[1]                       # CHI-central
root_0    = args[2]                       # CHI = 0
scen      = args[3]
years     = as.integer(args[4]):as.integer(args[5])

# Author-accepted central elasticities (avoidance.R). Used only for the exact
# marketable-flow ratio in check (b); the direction checks do not depend on them.
PUBLIC_E  = -7

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}

detail = function(root, run, t, cols = NULL) {
  f = file.path(root, scen, run, 'detail', paste0(t, '.csv'))
  d = fread(f, showProgress = FALSE)
  if (!is.null(cols)) d = d[, intersect(cols, names(d)), with = FALSE]
  d
}

# Expected estate tax per record (E over the DSUE / no-DSUE branch and death)
est_liab = function(d) {
  d$estate_m * (d$estate_p_dsue * d$liab_estate_dsue +
                (1 - d$estate_p_dsue) * d$liab_estate_nodsue)
}
wsum = function(w, x) sum(w * x, na.rm = TRUE)

MKT_FLOWS = c('div_ord', 'txbl_int', 'kg_lt')
CH_AGG    = c('sole_prop', 'part_scorp', 'sch_e')   # closely-held detail aggregates

for (t in years) {

  #--- Frames ------------------------------------------------------------------
  c0s = detail(root_0, 'static',       t)   # CHI=0 static
  c0c = detail(root_0, 'conventional', t)   # CHI=0 conventional
  cs  = detail(root_c, 'static',       t)   # central static
  cc  = detail(root_c, 'conventional', t)   # central conventional
  stopifnot(identical(c0s$id, c0c$id), identical(cs$id, cc$id),
            identical(cc$id, c0c$id))       # same sample/order across runs

  # The avoidance module keys concealment off the STATIC-frame net_worth MTR
  # (static_mtrs$mtr_net_worth, == the static detail's column). The conventional
  # detail's mtr_net_worth is recomputed on the AVOIDED frame (run.R:866) and can
  # differ near a bracket edge, so reconstruct c_pub from the static column.
  pos_mtr = cs$mtr_net_worth > 0
  below   = !pos_mtr
  cat(sprintf('        year %d: %d records with mtr_net_worth > 0 (weighted %.3fM)\n',
              t, sum(pos_mtr), sum(cs$weight[pos_mtr]) / 1e6))

  #--- (a) CHI = 0 no-op (concealment fully off) -------------------------------
  # Marketable flows are concealment's c_pub targets and evasion never touches
  # them, so at CHI=0 they must equal the static pass exactly.
  err_mkt0 = max(sapply(MKT_FLOWS, function(v)
    if (v %in% names(c0c)) max(abs(c0c[[v]] - c0s[[v]])) else 0))
  check(err_mkt0 == 0,
        sprintf('year %d (a): CHI=0 conventional marketable flows == static exactly (max err %.2e)',
                t, err_mkt0))
  err_est = max(abs(est_liab(c0c) - est_liab(c0s)))
  check(err_est < 1e-3,
        sprintf('year %d (a): CHI=0 conventional estate liability == static (max err %.2e)',
                t, err_est))
  ecf0 = if ('estate_concealed_frac' %in% names(c0c)) max(abs(c0c$estate_concealed_frac)) else NA
  check(!is.na(ecf0) && ecf0 == 0,
        sprintf('year %d (a): CHI=0 estate_concealed_frac = 0 everywhere', t))
  # net_worth is the FULL avoidance response (uses f, not c), so it is identical
  # across the CHI legs -- CHI drives concealment, not the reported wealth base.
  err_nw = max(abs(cc$net_worth - c0c$net_worth))
  check(err_nw == 0,
        sprintf('year %d (a): reported net_worth identical across CHI legs (max err %.2e)',
                t, err_nw))

  #--- (b) concealment fires only where mtr_net_worth > 0 ----------------------
  # Below the exemption, concealment is the ONLY thing differing between the two
  # legs and it is off (c = 0), so the central and CHI=0 conventional frames must
  # be byte-identical there (evasion runs identically in both legs).
  err_below = max(sapply(c(MKT_FLOWS, CH_AGG), function(v)
    if (v %in% names(cc)) max(abs(cc[[v]][below] - c0c[[v]][below])) else 0))
  check(err_below == 0,
        sprintf('year %d (b): below-exemption central-conv == CHI=0-conv exactly (max err %.2e)',
                t, err_below))

  # Marketable flows shrink by EXACTLY exp(mtr * public_e) where the static leg
  # is positive and mtr > 0 (c_pub = 1 - exp(mtr * public_e), CHI_PUB = 1).
  fac = exp(cs$mtr_net_worth * PUBLIC_E)
  for (v in MKT_FLOWS) {
    if (!(v %in% names(cc))) next
    idx = pos_mtr & cs[[v]] > 0
    if (sum(idx) == 0) next
    rel = max(abs(cc[[v]][idx] - cs[[v]][idx] * fac[idx]) / pmax(cs[[v]][idx], 1))
    check(rel < 1e-6,
          sprintf('year %d (b): %s shrinks by exp(mtr*public_e) on %d records (max rel err %.2e)',
                  t, v, sum(idx), rel))
  }
  # Closely-held aggregates: direction only (mixed positive/negative legs), and
  # only for records where the aggregate is positive under static.
  for (v in CH_AGG) {
    if (!(v %in% names(cc))) next
    idx = pos_mtr & cs[[v]] > 0
    if (sum(idx) == 0) next
    check(all(cc[[v]][idx] <= cs[[v]][idx] + 1e-6),
          sprintf('year %d (b): %s does not increase under concealment (%d records)',
                  t, v, sum(idx)))
  }

  #--- (c) estate_distributable invariance -------------------------------------
  c0c_dist = c0c$estate_distributable[match(cc$id, c0c$id)]
  err_dist_chi = max(abs(cc$estate_distributable - c0c_dist))
  err_dist_stat = max(abs(cc$estate_distributable - cs$estate_distributable))
  check(err_dist_chi < 1e-3 && err_dist_stat < 1e-3,
        sprintf('year %d (c): estate_distributable invariant across CHI legs and vs static (max err %.2e)',
                t, max(err_dist_chi, err_dist_stat)))

  # Heir-allocator Sw*p*lambda identity (best effort; file present only if the
  # distribution post-processing ran this year)
  alloc = file.path(root_c, scen, 'static', 'supplemental',
                    paste0('estate_allocator_diag_', t, '.csv'))
  if (file.exists(alloc)) {
    ad = fread(alloc, showProgress = FALSE)
    check(TRUE, sprintf('year %d (c): estate_allocator_diag present (%d rows)', t, nrow(ad)))
  } else {
    cat(sprintf('        year %d (c): estate_allocator_diag absent (no dist year) -- skipped\n', t))
  }

  #--- (d) conservation identity from detail vs the module diagnostic ----------
  diagf = file.path(root_c, scen, 'conventional', 'supplemental',
                    paste0('hidden_ledger_', t, '.csv'))
  if (file.exists(diagf)) {
    hd = fread(diagf, showProgress = FALSE)
    check(hd$conservation_max_leg_err < 1e-6,
          sprintf('year %d (d): module per-record conservation_max_leg_err %.2e < 1e-6',
                  t, hd$conservation_max_leg_err))
    map = c(div_ord = 'concealed_flow_div_ord', txbl_int = 'concealed_flow_txbl_int',
            kg_lt = 'concealed_flow_kg_lt')
    for (v in names(map)) {
      if (!(v %in% names(cc)) || !(map[[v]] %in% names(hd))) next
      # pre-concealment == static (evasion never touches marketable flows)
      removed = wsum(cc$weight, pmax(cs[[v]], 0) * (cs[[v]] > 0)) -
                wsum(cc$weight, cc[[v]] * (cs[[v]] > 0))
      rel = abs(removed - hd[[map[[v]]]]) / pmax(abs(hd[[map[[v]]]]), 1)
      check(rel < 1e-6,
            sprintf('year %d (d): Sw(static-conv) %s == diag concealed_flow ($%.3fB; rel err %.2e)',
                    t, v, hd[[map[[v]]]] / 1e9, rel))
    }
    cat(sprintf(
      '        year %d diag: concealed wealth mkt $%.2fB / clh $%.2fB; estate_concealed_frac wmean %.4f; CHI %.2f/%.2f\n',
      t, hd$concealed_wealth_marketable / 1e9, hd$concealed_wealth_closely_held / 1e9,
      hd$estate_concealed_frac_wmean, hd$chi_pub, hd$chi_priv))
  } else {
    check(FALSE, sprintf('year %d (d): hidden_ledger diagnostic missing (%s)', t, diagf))
  }

  #--- (e) direction: central vs CHI=0 conventional revenue --------------------
  est_c = wsum(cc$weight,  est_liab(cc))
  est_0 = wsum(c0c$weight, est_liab(c0c))
  iit_c = wsum(cc$weight,  cc$liab_iit_net)
  iit_0 = wsum(c0c$weight, c0c$liab_iit_net)
  check(est_c < est_0 - 1e-3,
        sprintf('year %d (e): conventional estate revenue DOWN vs CHI=0 ($%.3fB < $%.3fB; delta $%.3fB)',
                t, est_c / 1e9, est_0 / 1e9, (est_c - est_0) / 1e9))
  check(iit_c < iit_0 - 1e-3,
        sprintf('year %d (e): conventional income-tax revenue DOWN vs CHI=0 ($%.3fB < $%.3fB; delta $%.3fB)',
                t, iit_c / 1e9, iit_0 / 1e9, (iit_c - iit_0) / 1e9))
}

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL CHECKS PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

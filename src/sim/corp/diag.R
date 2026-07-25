#-------------------------------------------------------------------------------
# diag.R
#
# Conservation diagnostic and the analytic path self-check.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Conservation diagnostic (FORMAL_MODEL section 4, as amended by the external
# review: the three-way identity w = B_flow + B_accr + B_res is a REPORT --
# B_res is residually defined -- while the TESTABLE content is the per-line
# analytic-intended vs record-realized reconciliation. WARN-level; promote to
# hard-error only after the permanent + windowed test scenarios pin sign
# behavior and tolerances. Invariant 2 (markdown telescoping) is already a
# hard assert inside corp_resolve_paths.)
#-------------------------------------------------------------------------------

corp_write_conservation_diag = function(pre, post, paths, year, conv_root) {

  #----------------------------------------------------------------------------
  # Writes conventional/supplemental/corp_conservation_diag_{t}.csv (estate-
  # allocator-diag precedent) for one CY year, from the pre- and post-applier
  # frames (row-aligned; final conventional pass only).
  #
  # Columns:
  #   - inputs/paths: w, phi, mu, eta, roll, sigma_n/kappa/theta_res knobs
  #   - per-line reconciliation ($B): dY_{div,int,rent,pt}_realized (measured
  #     by differencing the frames -- INDEPENDENT of the applier's analytic
  #     accumulation) vs their sum dY_total_analytic (= sum w*corp_dY_exog,
  #     the applier's accumulated column). A gap flags a weights bug, a
  #     missed/overwritten line, or clamping. WARN beyond tolerance.
  #   - B_flow_hh = -sum(w * corp_dY_exog): the household external-income
  #     burden flow (positive under a hike);
  #   - markdown_position_hh = sum(w * per-record markdown dollars): the
  #     household PV markdown POSITION at t (a stock; its year-over-year
  #     movement is the B_accr flow -- difference the per-year files, or see
  #     the bathtub state's corp_gain_debit for the kg slice);
  #   - B_res_theta = theta_res * w (D3/D10 foreign/nonprofit/DB slice) and
  #     drho_int (the named delta-rho revaluation line: undelivered unrolled-
  #     interest compression, D15/P14);
  #   - residual_unallocated = w - B_flow_hh - B_res_theta - drho_int: the
  #     honest unallocated remainder REPORT (no gross-up forces this to zero;
  #     it also absorbs the accrual flow not measured here).
  #
  # Returns: invisibly the one-row diag tibble.
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) return(invisible(NULL))
  p = paths$sim[i, ]

  w8   = pre$weight
  toB  = 1e-9
  line = function(cols_pos, cols_neg = character(0)) {
    d = rep(0, nrow(pre))
    for (cc in intersect(cols_pos, names(pre))) {
      d = d + (replace_na(post[[cc]], 0) - replace_na(pre[[cc]], 0))
    }
    for (cc in intersect(cols_neg, names(pre))) {
      d = d - (replace_na(post[[cc]], 0) - replace_na(pre[[cc]], 0))
    }
    sum(w8 * d) * toB
  }

  pt_cols_pos = c('sole_prop', 'part_active', 'part_passive',
                  'scorp_active', 'scorp_passive', 'farm')
  pt_cols_neg = c('part_active_loss', 'part_passive_loss', 'part_179',
                  'scorp_active_loss', 'scorp_passive_loss', 'scorp_179')

  dY_div_realized  = line(CORP_FLOWS_DIV)
  dY_int_realized  = line(CORP_FLOWS_INT)
  dY_rent_realized = line('rent', 'rent_loss')
  dY_pt_realized   = line(pt_cols_pos, pt_cols_neg)
  dY_total_realized = dY_div_realized + dY_int_realized +
                      dY_rent_realized + dY_pt_realized

  dY_total_analytic = sum(w8 * replace_na(post$corp_dY_exog, 0)) * toB

  # Household markdown position, re-measured from the exposed value.* deltas
  # (independent of the applier's internal markdown_amt).
  md = rep(0, nrow(pre))
  for (a in intersect(names(corp_asset_exposure()), names(pre))) {
    md = md + (replace_na(pre[[a]], 0) - replace_na(post[[a]], 0))
  }
  markdown_position_hh = sum(w8 * md) * toB

  knobs = paths$knobs
  diag = tibble(
    year  = year,
    w     = p$w,
    phi   = p$phi,
    mu    = p$mu,
    eta   = p$eta,
    roll  = p$roll,
    sigma_n   = knobs$sigma_n,
    kappa     = knobs$kappa,
    theta_res = assumption('corp', 'theta_res'),
    dY_div_realized   = dY_div_realized,
    dY_int_realized   = dY_int_realized,
    dY_rent_realized  = dY_rent_realized,
    dY_pt_realized    = dY_pt_realized,
    dY_total_realized = dY_total_realized,
    dY_total_analytic = dY_total_analytic,
    B_flow_hh            = -dY_total_analytic,
    markdown_position_hh = markdown_position_hh,
    B_res_theta          = assumption('corp', 'theta_res') * p$w,
    drho_int             = p$drho_int,
    residual_unallocated = p$w + dY_total_analytic -
                           assumption('corp', 'theta_res') * p$w - p$drho_int
  )

  # The testable content: analytic accumulation vs frame-measured realization.
  gap = abs(dY_total_realized - dY_total_analytic)
  if (gap > max(0.05, 0.005 * abs(dY_total_analytic))) {
    warning(sprintf(paste0(
      'corp_incidence conservation diag, year %d: record-realized external-',
      'income delta ($%.2fB) differs from the analytic corp_dY_exog ',
      'accumulation ($%.2fB) by $%.2fB. A weights bug, a missed/overwritten ',
      'line, or clamping is likely.'),
      year, dY_total_realized, dY_total_analytic, gap), call. = FALSE)
  }

  dir.create(file.path(conv_root, 'supplemental'), recursive = TRUE,
             showWarnings = FALSE)
  write_csv(diag, file.path(conv_root, 'supplemental',
                            sprintf('corp_conservation_diag_%d.csv', year)))
  invisible(diag)
}



#-------------------------------------------------------------------------------
# Self-checks on synthetic inputs (callable from the test harness / sbatch
# verification; NOT run at source time)
#-------------------------------------------------------------------------------

corp_selfcheck_paths = function() {

  #----------------------------------------------------------------------------
  # Drives corp_build_paths_core with synthetic series and asserts the plan's
  # unit properties:
  #   1. PERMANENT, sigma_n = 0 (rent-only corner): mu_t constant across the
  #      live window and equal to theta * (w/pi share) -- the Delta-tau/(1-tau)
  #      equivalent (P1); dividend factor mirrors it via omega_div.
  #   2. PERMANENT, sigma_n > 0: mu_t decays MONOTONICALLY toward the
  #      rent-share floor sigma-split (P14/D14): late-horizon mu ->
  #      (1 - sigma_n) * (w/pi constant share).
  #   3. WINDOWED (zero beyond horizon): M_t = 0 (and mu_t = 0) for all years
  #      at/after expiry (P3); under priced_as_permanent the same input keeps
  #      mu > 0 through the window's end.
  #   4. Telescoping + pre-enactment inertness via corp_assert_paths.
  #
  # Returns: TRUE invisibly; stops with a message on any violation.
  #----------------------------------------------------------------------------

  years_all = 2024:2200
  pi0 = 4000; g = 0.035
  macro = tibble(
    year = years_all,
    pi_at = pi0 * (1 + g)^(years_all - min(years_all)),
    tsy_10y = 4.2,
    gdp_interest = 2000 * (1 + g)^(years_all - min(years_all)),
    gdp_rent     = 1100 * (1 + g)^(years_all - min(years_all)),
    gdp_proprietors = 2200 * (1 + g)^(years_all - min(years_all))
  )
  roll_fn = function(t_since) pmin(pmax(t_since, 0) / 10, 1)
  sim_years = 2025:2036
  w_share = 0.05   # wedge = 5% of after-tax profits, permanent

  perm_wedge = tibble(year = 2024:2040,
                      w = if_else(year >= 2026,
                                  w_share * macro$pi_at[match(year, macro$year)],
                                  0))

  # --- 1. permanent, rent-only ------------------------------------------------
  p1 = corp_build_paths_core(perm_wedge, macro, sim_years, 'extend',
                             sigma_n = 0, kappa = 0.4, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p1)
  live = p1$sim %>% filter(year >= p1$t0)
  if ((max(live$mu) - min(live$mu)) > 1e-6) {
    stop('corp_selfcheck: permanent rent-only mu is not constant (range ',
         min(live$mu), ' .. ', max(live$mu), ').')
  }
  if (abs(mean(live$mu) - w_share * assumption('corp', 'theta')) > 1e-3) {
    stop('corp_selfcheck: permanent rent-only mu = ', mean(live$mu),
         ' differs from the flow share ', w_share,
         ' (the Delta-tau/(1-tau) equivalent).')
  }

  # --- 2. permanent, migrating ------------------------------------------------
  sig = 0.375; kap = 0.4
  p2 = corp_build_paths_core(perm_wedge, macro, sim_years, 'extend',
                             sigma_n = sig, kappa = kap, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p2)
  live2 = p2$sim %>% filter(year >= p2$t0)
  if (any(diff(live2$mu) > 1e-9)) {
    stop('corp_selfcheck: permanent migrating mu is not weakly decaying.')
  }
  floor_share = (1 - sig) * w_share
  # far tail: eta ~ 1 -> mu -> rent floor (+ small residual from the ramp)
  tail_mu = p2$by_year %>% filter(year == max(year)) %>% pull(mu)
  if (tail_mu < floor_share - 1e-3 || tail_mu > floor_share + 0.3 * sig * w_share) {
    stop('corp_selfcheck: far-tail mu = ', tail_mu,
         ' is not at the rent-share floor ', floor_share, '.')
  }

  # --- 3. windowed ------------------------------------------------------------
  win_wedge = tibble(year = 2024:2040,
                     w = if_else(year >= 2026 & year <= 2031,
                                 w_share * macro$pi_at[match(year, macro$year)],
                                 0))
  p3 = corp_build_paths_core(win_wedge, macro, sim_years, 'zero',
                             sigma_n = 0, kappa = kap, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p3)
  post = p3$by_year %>% filter(year >= 2031)   # M_t = PV of hits AFTER t
  if (any(abs(post$M) > 1e-6) || any(abs(post$mu) > 1e-9)) {
    stop('corp_selfcheck: windowed markdown does not vanish at expiry ',
         '(max |M| after window = ', max(abs(post$M)), ').')
  }
  p3b = corp_build_paths_core(win_wedge, macro, sim_years, 'zero',
                              sigma_n = 0, kappa = kap, roll_fn = roll_fn,
                              pt_weight = 0.2, priced_as_permanent = TRUE)
  mu_at_2030 = p3b$by_year %>% filter(year == 2030) %>% pull(mu)
  if (mu_at_2030 < 0.5 * w_share) {
    stop('corp_selfcheck: priced-as-permanent corner did not keep the ',
         'markdown alive near the window end (mu(2030) = ', mu_at_2030, ').')
  }

  message('corp_selfcheck_paths: all path-property checks passed ',
          '(permanent-constant mu, rent-share floor decay, windowed expiry, ',
          'priced-as-permanent corner, telescoping).')
  invisible(TRUE)
}

#-------------------------------------------------------------------------------
# diag.R
#
# Contains functions to reconcile the corporate channel and to check its paths
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Reconciliation
#
# The corporate revenue change ought to equal what households lose in flows, plus
# what they lose in asset value, plus what falls on foreign owners, nonprofits and
# pension sponsors. That is a report rather than a test: the last piece is defined
# as whatever is left over.
#
# What can actually be tested is each line separately: what the paths said would
# be applied against what the records show was applied. That runs at warning level.
# Promote it to an error once the permanent and windowed test scenarios have pinned
# down the signs and tolerances. The one hard check, that the markdown is
# consistent with itself, already runs inside corp_resolve_paths().
#-------------------------------------------------------------------------------

corp_write_conservation_diag = function(pre, post, paths, year, conv_root) {

  #----------------------------------------------------------------------------
  # Writes one reconciliation file per year, from the record frames before and
  # after the corporate step. Runs on the final conventional pass only.
  #
  # The file carries the year's inputs and paths, then the reconciliation itself:
  # what was applied to each income line, measured by differencing the two frames,
  # against the total the step accumulated as it went. A gap between them points to
  # a weighting error, a line that was missed or overwritten, or something being
  # clamped.
  #
  # It then carries the household loss in flows, the household markdown position,
  # the share falling on foreign owners, nonprofits and pension sponsors, the
  # revaluation of debt that has not yet rolled over, and finally whatever is
  # unaccounted for. Nothing is grossed up to force that last figure to zero. It
  # also absorbs the change in asset values, which is not measured here: the
  # markdown position is a stock, so difference it across years to get the flow.
  #
  # Returns: invisibly the one-row table.
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

  # The household markdown, measured again from the change in asset values rather
  # than taken from the step that applied it.
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
    theta_res = economy_param('corp', 'theta_res'),
    dY_div_realized   = dY_div_realized,
    dY_int_realized   = dY_int_realized,
    dY_rent_realized  = dY_rent_realized,
    dY_pt_realized    = dY_pt_realized,
    dY_total_realized = dY_total_realized,
    dY_total_analytic = dY_total_analytic,
    B_flow_hh            = -dY_total_analytic,
    markdown_position_hh = markdown_position_hh,
    B_res_theta          = economy_param('corp', 'theta_res') * p$w,
    drho_int             = p$drho_int,
    residual_unallocated = p$w + dY_total_analytic -
                           economy_param('corp', 'theta_res') * p$w - p$drho_int
  )

  # The part that is actually a test: what the paths said against what happened.
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
# Checks on made-up inputs, called by the test harness rather than at source time
#-------------------------------------------------------------------------------

corp_selfcheck_paths = function() {

  #----------------------------------------------------------------------------
  # Drives the path builder with made-up series, where the right answer is known,
  # and checks four things.
  #
  # Under a permanent change falling entirely on above-normal returns, the markdown
  # is constant over the window and equals the share of profit lost. The cut to
  # dividends mirrors it.
  #
  # Under a permanent change with some of it on normal returns, the markdown falls
  # steadily as that capital migrates away, toward the share falling on
  # above-normal returns alone.
  #
  # Under a change that expires, the markdown is zero from the year of expiry
  # onward. If markets are assumed not to believe the expiry, it stays positive
  # through the window instead.
  #
  # And the markdown is consistent with itself, and nothing moves before enactment.
  #
  # Returns: invisibly TRUE; stops with a message on any violation.
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

  # Permanent, entirely on above-normal returns
  p1 = corp_build_paths_core(perm_wedge, macro, sim_years, 'extend',
                             sigma_n = 0, kappa = 0.4, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p1)
  live = p1$sim %>% filter(year >= p1$t0)
  if ((max(live$mu) - min(live$mu)) > 1e-6) {
    stop('corp_selfcheck: permanent rent-only mu is not constant (range ',
         min(live$mu), ' .. ', max(live$mu), ').')
  }
  if (abs(mean(live$mu) - w_share * economy_param('corp', 'theta')) > 1e-3) {
    stop('corp_selfcheck: permanent rent-only mu = ', mean(live$mu),
         ' differs from the flow share ', w_share,
         ' (the Delta-tau/(1-tau) equivalent).')
  }

  # Permanent, with capital migrating away
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
  # Once capital has fully turned over, only the above-normal share is left
  tail_mu = p2$by_year %>% filter(year == max(year)) %>% pull(mu)
  if (tail_mu < floor_share - 1e-3 || tail_mu > floor_share + 0.3 * sig * w_share) {
    stop('corp_selfcheck: far-tail mu = ', tail_mu,
         ' is not at the rent-share floor ', floor_share, '.')
  }

  # Expiring
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

#-------------------------------------------------------------------------------
# bellman.R
#
# Contains functions to solve the realization choice and to assemble its inputs
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# The problem is solved by working backward, over years in the outer loop and
# over ages in the inner one, on an age grid extended to 119. Value past the last
# age is zero, which binds quickly since mortality there is close to 1. The last
# year is seeded by sweeping backward through ages while holding that year's
# inputs constant forward.
#
# The baseline is solved first, which recovers the benefit of realizing. The
# scenario is then solved from that benefit in closed form.
#-------------------------------------------------------------------------------

kg_dyn_pack_tau = function(tau_list, years,
                            ages_bellman = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX_BELLMAN) {

  # Packs the yearly tax rate vectors into one matrix over the extended age
  # grid. Ages past 80 take the age-80 rate.

  out = matrix(0, length(ages_bellman), length(years),
               dimnames = list(as.character(ages_bellman), as.character(years)))
  for (t in years) {
    key = as.character(t)
    v = tau_list[[key]]
    out[as.character(KG_DYN_AGE_MIN:KG_DYN_AGE_MAX), key] = as.numeric(v)
    out[as.character((KG_DYN_AGE_MAX + 1):KG_DYN_AGE_MAX_BELLMAN), key] =
      as.numeric(v[as.character(KG_DYN_AGE_MAX)])
  }
  out
}



kg_dyn_pack_baseline_grid = function(grid_ext, years,
                                     ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  # Packs mortality and the baseline realization rate into matrices of age by
  # year.
  #
  # Mortality is weighted by the stock of gains held rather than by taxpayer,
  # because the problem is written per dollar of unrealized gain and so needs the
  # chance that the dollar's holder dies. Weighting by taxpayer instead
  # overstates it by a factor of two or three, since wealthier holders die less.
  # Where a cell holds no gains, which past age 80 is every cell, this falls back
  # to the life table rate.

  n_ages  = length(ages_bellman)
  n_years = length(years)
  ages_chr = as.character(ages_bellman)
  years_chr = as.character(years)

  m   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_B = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  p_char = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  for (t_chr in years_chr) {
    bt = grid_ext[[t_chr]]
    m_gw = ifelse(bt$G_B > 0, bt$mG_record / bt$G_B, bt$m)
    m  [, t_chr] = pmin(pmax(m_gw, 0), 1)
    r_B[, t_chr] = bt$r_B
    p_char[, t_chr] = pmin(pmax(bt$p_char, 0), 1)
  }
  list(m = m, r_B = r_B, p_char = p_char)
}



kg_dyn_bellman_sweep_age = function(W_next, m_col, r_B_col, tau_col,
                                     c_phi_col, p_char_col, eta, beta,
                                     kappa_col = NULL, stationary = FALSE,
                                     h_col = NULL, e_col = NULL,
                                     form = kg_dyn_response_form()) {

  # Sweeps backward through ages for one year.
  #
  # c_phi_col is the share of the cell's gains taxed at death, which the holder
  # takes into account when deciding whether to realize during life. It comes
  # from kg_dyn_build_regime_mix(), extended past age 80 by repeating the age-80
  # value, as the tax rate is.
  #
  # h_col is the wealth tax cost of deferring one more year: the gain-weighted
  # average of each record's wealth rate times its gains rate. It is taken as the
  # average of the product, not the product of the averages, since the two rates
  # are positively correlated at the top. The wealth tax is assessed on holdings
  # at year end, so anyone who realizes or dies during the year escapes it, and
  # only continued deferral is charged. Passing NULL means no wealth tax, which
  # covers the baseline and every scenario without one.
  #
  # e_col is the share of a gain taxed at death that the estate tax takes back,
  # as the gain-weighted average marginal estate rate. Capital gains tax paid at
  # death is deductible from the estate, so realizing during life shrinks the
  # future estate while dying with the gain either gives up that deduction, under
  # deemed realization, or keeps it, under step-up. The value of dying with a gain
  # is therefore scaled by (1 - e). It does nothing under deemed realization,
  # bites hardest under step-up, and scales in between under carryover. Cells
  # below the exemption have e = 0.
  #
  # Note that e differs from h in being paired to its own leg: the baseline pass
  # gets the baseline estate rate and the scenario pass the reform rate. Sharing
  # one matrix would silently kill estate-only reforms, where the gains rate does
  # not move and so nothing else would distinguish the two passes. That is the
  # margin this term exists to price.
  #
  # form selects the cost of realizing away from the baseline rate. Both forms
  # have zero derivative at the baseline rate, so the baseline pass is the same
  # under either.
  #
  #   'levels' C(r) = (1/eta) * [r*ln(r/r_B) - r + r_B]
  #            giving r = r_B * exp(-eta * (MC - MC_B))
  #   'logs'   C(r) = (1 - MC_B) * [(eta/(eta+1))*r_B*(r/r_B)^((eta+1)/eta)
  #                                 - r + r_B/(eta+1)]
  #            giving r = r_B * ((1 - MC) / (1 - MC_B)) ^ eta
  #
  # The logs form stops the run if any cell's marginal cost reaches the cap,
  # since the base of the power would go non-positive.
  #
  # Leaving kappa_col NULL solves the baseline, where the realization rate is
  # held at its observed value and the benefit of realizing is recovered from it.
  # Supplying kappa_col solves a scenario against that benefit.
  #
  # Setting stationary seeds the last year by taking next year's value from this
  # same sweep, holding the year's inputs constant forward. Past the last age the
  # continuation is zero.
  #
  # Returns: list of value, marginal cost, realization rate and benefit, each a
  #          vector over ages.

  n_ages = length(m_col)
  W     = numeric(n_ages)
  MC    = numeric(n_ages)
  r_D   = numeric(n_ages)
  kappa = numeric(n_ages)

  is_baseline_pass = is.null(kappa_col)
  if (!form %in% c('levels', 'logs'))
    stop(sprintf("kg_dynamics: form must be 'levels' or 'logs'; got '%s'.", form))

  # Quantities that do not change inside the loop. Charity is assumed to come
  # proportionally out of the mix of death treatments rather than preferentially
  # out of the taxed share, the same assumption kg_dyn_step_recurrence() makes.
  c_phi_eff    = c_phi_col * (1 - pmin(pmax(p_char_col, 0), 1))
  if (is.null(e_col)) e_col = numeric(n_ages)
  # Value of dying with the gain, net of the estate tax offset. The estate rate is
  # capped at 1 upstream, so this cannot change sign.
  F_vec        = (1 - c_phi_eff) * tau_col * (1 - e_col)
  # The whole baseline rate can respond, so realization is capped at 1 and the
  # cost is anchored at the baseline rate. Both passes recompute the anchor.
  r_D_cap_vec  = rep(1, length(r_B_col))
  r_D_B_vec    = pmin(pmax(r_B_col, 0), 1)
  bs_vec       = beta * (1 - m_col)   # discount if the holder survives
  bm_vec       = beta * m_col         # discount if the holder dies
  if (is.null(h_col)) h_col = numeric(n_ages)

  for (i in n_ages:1) {
    tau_i   = tau_col[i]
    F_i     = F_vec[i]
    r_D_B_i = r_D_B_vec[i]

    W_next_i = if (i == n_ages) 0 else if (stationary) W[i + 1] else W_next[i + 1]

    # The wealth tax cost is charged only if the holder survives. Both the value
    # and the marginal cost pick it up through the continuation, so a wealth tax
    # lowers the marginal cost of realizing and raises the realization rate.
    death_cont = bs_vec[i] * (W_next_i - h_col[i]) + bm_vec[i] * F_i
    MC_i       = tau_i + death_cont
    r_D_cap    = r_D_cap_vec[i]

    # The net-of-tax form is undefined as the marginal cost approaches 1. Stop
    # rather than clamp. The levels form has no such limit, so skip the check.
    if (form == 'logs') {
      mc_max = if (is_baseline_pass) MC_i else max(MC_i, kappa_col[i])
      if (mc_max >= KG_DYN_LOGS_MC_CAP) {
        stop(sprintf(paste0('kg_dynamics (logs form): marginal cost %.4f >= cap ',
                     '%.2f at age index %d (MC = %.4f, MC_B = %.4f). The ',
                     'net-of-tax response is undefined near MC = 1 and will ',
                     'not be clamped. Check the discount series and the death ',
                     'continuation.'),
                     mc_max, KG_DYN_LOGS_MC_CAP, i, MC_i,
                     if (is_baseline_pass) MC_i else kappa_col[i]))
      }
    }

    if (is_baseline_pass) {
      # Under either cost form the derivative is zero at the baseline rate, so
      # the benefit of realizing equals the marginal cost exactly.
      r_D_i   = r_D_B_i
      kappa_i = MC_i
    } else {
      # Scenario. Only the upper bound can bind, and cells that realized nothing
      # in the baseline stay at zero.
      kappa_i = kappa_col[i]
      if (form == 'levels') {
        r_D_i = if (r_D_B_i > 0)
                  min(r_D_B_i * exp(-eta * (MC_i - kappa_i)), r_D_cap)
                else 0
      } else {
        r_D_i = if (r_D_B_i > 0)
                  min(r_D_B_i * ((1 - MC_i) / (1 - kappa_i))^eta, r_D_cap)
                else 0
      }
    }

    if (form == 'levels') {
      # The separate branch for a baseline rate of zero avoids taking log(0). A
      # realization rate clipped to zero needs no special handling, since the
      # r*log(r) term goes to zero with it.
      if (r_D_B_i > 0) {
        xlogx = if (r_D_i > 0) r_D_i * log(r_D_i / r_D_B_i) else 0
        C_i   = (xlogx - r_D_i + r_D_B_i) / eta
      } else {
        C_i = 0
      }
    } else {
      # The power cost, which is zero and flat at the baseline rate and convex
      # away from it. Again the zero branch avoids dividing by the baseline rate.
      if (r_D_B_i > 0) {
        C_i = (1 - kappa_i) *
              ((eta / (eta + 1)) * r_D_B_i * (r_D_i / r_D_B_i)^((eta + 1) / eta) -
               r_D_i + r_D_B_i / (eta + 1))
      } else {
        C_i = 0
      }
    }

    remaining = max(1 - r_D_i, 0)
    W[i]     = kappa_i * r_D_i - C_i - tau_i * r_D_i + remaining * death_cont
    MC[i]    = MC_i
    r_D[i]   = r_D_i
    kappa[i] = kappa_i
  }

  list(W = W, MC = MC, r_D = r_D, kappa = kappa)
}



kg_dyn_solve_bellman = function(grid_packed, tau_mat, c_phi_mat,
                                kappa_mat     = NULL,
                                eta           = kg_dyn_active_eta(),
                                beta_by_year  = NULL,
                                c_phi         = NULL,
                                h_mat         = NULL,
                                e_mat         = NULL,
                                form          = kg_dyn_response_form()) {

  #----------------------------------------------------------------------------
  # Solves the realization choice backward over age and year.
  #
  # Leaving kappa_mat NULL solves the baseline, recovering the benefit of
  # realizing by requiring the chosen rate to match the observed one. Supplying
  # kappa_mat solves a scenario against that benefit, in the closed form the
  # active response form gives.
  #
  # Parameters:
  #   - grid_packed (list) : mortality, baseline realization rate and charity
  #                          probability, each a matrix of age by year
  #   - tau_mat (matrix)   : marginal tax rate on gains, by age and year
  #   - c_phi_mat (matrix) : share of gains taxed at death. A scalar is accepted
  #                          and repeated, for unit tests
  #   - kappa_mat (matrix) : benefit of realizing, from the baseline solve; NULL
  #                          to solve the baseline instead
  #   - eta (dbl)          : the active form's elasticity parameter
  #   - beta_by_year (dbl[]) : discount factor from each year to the next; NULL
  #                          uses the fallback, for unit tests
  #   - h_mat (matrix)     : wealth tax cost of deferring; NULL or 0 for none
  #   - e_mat (matrix)     : estate tax offset on the value of dying with the
  #                          gain, paired to the leg being solved; NULL or 0 for
  #                          none
  #   - form (str)         : 'levels' or 'logs'
  #
  # Returns: list of value, marginal cost, benefit of realizing and realization
  #          rate, each a matrix of age by year.
  #----------------------------------------------------------------------------

  m_mat   = grid_packed$m
  r_B_mat = grid_packed$r_B
  p_char_mat = grid_packed$p_char
  n_ages  = nrow(m_mat); n_years = ncol(m_mat)
  ages_chr  = rownames(m_mat); years_chr = colnames(m_mat)

  if (is.null(beta_by_year)) beta_by_year = rep(kg_setting('beta_fallback'), n_years)
  stopifnot(length(beta_by_year) == n_years)

  if (missing(c_phi_mat) || is.null(c_phi_mat)) {
    c_phi_mat = c_phi %||% 0
  }

  if (length(c_phi_mat) == 1) {
    c_phi_mat = matrix(c_phi_mat, n_ages, n_years,
                       dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(c_phi_mat), c(n_ages, n_years)))

  if (is.null(p_char_mat)) {
    p_char_mat = matrix(0, n_ages, n_years,
                        dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(p_char_mat), c(n_ages, n_years)))

  if (is.null(h_mat)) h_mat = 0
  if (length(h_mat) == 1) {
    h_mat = matrix(h_mat, n_ages, n_years,
                   dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(h_mat), c(n_ages, n_years)))

  if (is.null(e_mat)) e_mat = 0
  if (length(e_mat) == 1) {
    e_mat = matrix(e_mat, n_ages, n_years,
                   dimnames = list(ages_chr, years_chr))
  }
  stopifnot(identical(dim(e_mat), c(n_ages, n_years)))

  W     = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  MC    = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  kappa = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))
  r_D   = matrix(0, n_ages, n_years, dimnames = list(ages_chr, years_chr))

  sweep = function(j, W_next_col, stationary) {
    kg_dyn_bellman_sweep_age(
      W_next        = W_next_col,
      m_col         = m_mat  [, j],
      r_B_col       = r_B_mat[, j],
      tau_col       = tau_mat[, j],
      c_phi_col     = c_phi_mat[, j],
      p_char_col    = p_char_mat[, j],
      eta           = eta,
      beta          = beta_by_year[j],
      kappa_col     = if (is.null(kappa_mat)) NULL else kappa_mat[, j],
      stationary    = stationary,
      h_col         = h_mat[, j],
      e_col         = e_mat[, j],
      form          = form
    )
  }

  # Last year: sweep backward through ages taking next year's value from this
  # same sweep, which treats the last year's inputs as constant forward.
  res = sweep(n_years, W_next_col = NULL, stationary = TRUE)
  W    [, n_years] = res$W
  MC   [, n_years] = res$MC
  kappa[, n_years] = res$kappa
  r_D  [, n_years] = res$r_D

  # Then work backward year by year.
  if (n_years >= 2) {
    for (j in (n_years - 1):1) {
      res = sweep(j, W_next_col = W[, j + 1], stationary = FALSE)
      W    [, j] = res$W
      MC   [, j] = res$MC
      kappa[, j] = res$kappa
      r_D  [, j] = res$r_D
    }
  }

  list(W = W, MC = MC, kappa = kappa, r_D = r_D)
}




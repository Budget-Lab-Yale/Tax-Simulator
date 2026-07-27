#-------------------------------------------------------------------------------
# tau_eq.R
#
# Contains functions to price the tax on a dollar of unrealized gain
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# How much tax does a dollar of unrealized gain eventually pay? The answer is
# used by the income conversion module, which needs to compare taking income as
# wages against taking it as equity.
#
# A dollar entering a cell's stock of gains at the end of a year pays tax in
# three ways:
#
#   - each year it is held, the realization rate times the marginal rate;
#   - at death, the share deemed realized is taxed, less the estate tax
#     deduction that tax earns;
#   - under carryover, the routed share moves to the heir's cell and is taxed
#     there in later years, at the heir's full rate.
#
# Gains that get a step-up in basis, or that go to charity, are never taxed.
#
# The value is computed by a backward recursion,
#
#   T(a,j)      = c(a,j) + beta * [K T(.,j+1)](a)
#   tau_eq(a,j) = beta * T(a,j+1)
#
# where c is the year's tax flow and K carries the dollar forward, aging the
# surviving unrealized share and routing the inherited share to heirs. The last
# year is solved as a stationary system, which converges because the discount
# factor is below 1.
#
# kg_dyn_tau_eq_finite_diff() computes the same quantity the slow way, by
# following a test dollar forward and adding up the tax it pays. Unit tests check
# the recursion against it cell by cell.
#
# Two approximations are worth naming. Tax is priced at the cell's average
# marginal rate, since this is an input to a wedge and not a revenue estimate.
# And the record-level adjustments to deemed realizations, the avoidance haircut
# and the §121 netting, are left out here, as they are in the realization choice.
#-------------------------------------------------------------------------------

kg_dyn_tau_eq_primitives = function(baseline_cells, years, r_S_by_year,
                                    tau_bt_mat, mix_list, A, omega,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    h_bt_mat = NULL, e_bt_mat = NULL) {

  # Assembles the inputs shared by the recursion and the finite-difference check,
  # so that the two cannot drift apart.
  #
  # Parameters:
  #   - baseline_cells (list) : the year's cells, one tibble per year
  #   - years (int[])         : simulation years
  #   - r_S_by_year (list)    : realization rate by age, one vector per year
  #   - tau_bt_mat (matrix)   : marginal rate on gains, by age and year
  #   - mix_list (list|NULL)  : treatment of gains at death, one tibble per
  #                             year; NULL means step-up everywhere, which is
  #                             the baseline assumption
  #   - A, omega (matrix)     : the aging and heir-routing operators
  #   - h_bt_mat (matrix|NULL): wealth tax cost of deferring; NULL for none,
  #                             which covers the baseline and every scenario
  #                             without a wealth tax
  #   - e_bt_mat (matrix|NULL): estate tax offset on tax paid at death, paired
  #                             to the leg being priced; NULL for none
  #
  # Returns: list of the matrices above, plus the operators, ages and years.

  ages_chr  = as.character(ages_bathtub)
  years_chr = as.character(years)
  n_ages    = length(ages_bathtub)
  n_years   = length(years)

  blank = function() matrix(0, n_ages, n_years,
                            dimnames = list(ages_chr, years_chr))
  m_eff   = blank(); p_char = blank(); r_S = blank()
  route   = blank(); realize = blank()

  for (j in seq_len(n_years)) {
    bt = baseline_cells[[years_chr[j]]]
    stopifnot(identical(as.integer(bt$age), as.integer(ages_bathtub)))
    m_eff [, j] = kg_dyn_cell_m_eff(bt)
    p_char[, j] = pmin(pmax(bt$p_char, 0), 1)
    r_S   [, j] = pmin(pmax(as.numeric(r_S_by_year[[j]]), 0), 1)
    if (!is.null(mix_list)) {
      mix = mix_list[[j]]
      idx = match(ages_bathtub, mix$age)
      route  [, j] = mix$delta_route  [idx]
      realize[, j] = mix$delta_realize[idx]
    }
  }

  tau = tau_bt_mat[ages_chr, years_chr, drop = FALSE]

  h = if (is.null(h_bt_mat)) blank() else {
    stopifnot(identical(dim(h_bt_mat), c(n_ages, n_years)))
    h_bt_mat[ages_chr, years_chr, drop = FALSE]
  }

  e = if (is.null(e_bt_mat)) blank() else {
    stopifnot(identical(dim(e_bt_mat), c(n_ages, n_years)))
    e_bt_mat[ages_chr, years_chr, drop = FALSE]
  }

  list(m_eff = m_eff, p_char = p_char, r_S = r_S, tau = tau,
       route = route, realize = realize, h = h, e = e, A = A, omega = omega,
       ages = ages_bathtub, years = years)
}



kg_dyn_tau_eq_flow = function(prims, j) {

  # One year's tax on a dollar of unrealized gain. Used by both the recursion and
  # the finite-difference check, so that the two stay in step. Three terms:
  #
  #   realized during life  the realization rate times the marginal rate
  #   realized at death     the deemed share, taxed and net of the estate offset
  #   wealth tax            the cost of carrying the deferred liability
  #
  # The wealth tax applies only to the share that survives the year unrealized.
  # It is assessed on holdings at year end, so anyone who realizes or dies during
  # the year escapes it, the same convention the realization choice uses.
  #
  # The estate offset applies only to tax paid at death. That tax is deductible
  # against the estate, so a dollar of it collects only (1 - e). It deliberately
  # does not apply to the other two terms. Carryover routes basis to the heir
  # rather than collecting anything at death, so there is no deduction to price.
  # And capital gains tax paid during life does shrink the future estate, but the
  # wealth bathtub already carries that: during-life tax payments erode wealth and
  # so reduce the estate at death. Applying the offset here as well would count it
  # twice.
  #
  # On why holding is costly under a wealth tax: net worth is assets less debts,
  # with no deduction for the deferred capital gains liability. So on a dollar of
  # gain, realizing now costs the gains tax today and leaves the remainder in the
  # wealth base, while holding leaves the whole dollar in the wealth base. The
  # difference is the wealth rate times the gains rate, per year of deferral.

  # Unit tests build these by hand and may omit the estate offset, which then
  # defaults to none.
  e_j = if (is.null(prims$e)) 0 else prims$e[, j]

  prims$r_S[, j] * prims$tau[, j] +
    prims$m_eff[, j] * (1 - prims$p_char[, j]) *
      prims$realize[, j] * prims$tau[, j] * (1 - e_j) +
    (1 - prims$m_eff[, j]) * (1 - prims$r_S[, j]) * prims$h[, j]
}



kg_dyn_compute_tau_eq = function(prims, beta_by_year) {

  # Runs the backward recursion described above. The discount series is the same
  # one the realization choice uses.
  #
  # Returns: list of the priced tax and the two intermediate quantities, by age
  #          and year.

  n_ages  = length(prims$ages)
  n_years = length(prims$years)
  stopifnot(length(beta_by_year) == n_years)

  dimnm = list(as.character(prims$ages), as.character(prims$years))

  # The year's tax flow, and the operator that carries the dollar forward by
  # aging survivors and routing inheritances. The heir's own wealth tax cost is
  # priced through that routing, since the routed dollar re-enters the flow in
  # the heir's cell and pays it again while deferred. That is the same asymmetry
  # as pricing the heir's full income tax here while the holder internalizes only
  # part of it.
  c_mat = matrix(0, n_ages, n_years, dimnames = dimnm)
  for (j in seq_len(n_years)) c_mat[, j] = kg_dyn_tau_eq_flow(prims, j)
  K_of = function(j) {
    surv_w  = (1 - prims$m_eff[, j]) * (1 - prims$r_S[, j])
    route_w = prims$m_eff[, j] * (1 - prims$p_char[, j]) * prims$route[, j]
    surv_w * prims$A + route_w * prims$omega   # vector * matrix scales rows
  }

  # Past the last year, solve the stationary system under that year's inputs.
  T_stat = solve(diag(n_ages) - beta_by_year[n_years] * K_of(n_years),
                 c_mat[, n_years])

  T_mat  = matrix(0, n_ages, n_years, dimnames = dimnm)
  tau_eq = matrix(0, n_ages, n_years, dimnames = dimnm)

  T_next = T_stat
  for (j in n_years:1) {
    tau_eq[, j] = beta_by_year[j] * T_next
    T_mat [, j] = c_mat[, j] +
                  beta_by_year[j] * as.numeric(K_of(j) %*% T_next)
    T_next      = T_mat[, j]
  }

  list(tau_eq = tau_eq, T = T_mat, T_stationary = T_stat)
}



kg_dyn_tau_eq_finite_diff = function(prims, beta_by_year, j0,
                                     horizon = 500, tol = 1e-14) {

  # Computes the same quantity the slow way, as a check on the recursion. Follows
  # a test dollar in every age cell forward through the exact dynamics of
  # kg_dyn_step_recurrence() and adds up the discounted tax it pays. Inputs are
  # held at their last-year values past the horizon, matching the recursion.
  #
  # Returns: numeric vector of the priced tax by age.

  n_ages  = length(prims$ages)
  n_years = length(prims$years)

  delta  = diag(n_ages)                # rows are current age, columns the start
  pv     = numeric(n_ages)
  disc   = beta_by_year[j0]            # discounts next year's tax back to j0
  tA     = t(prims$A)
  tOmega = t(prims$omega)

  for (step in seq_len(horizon)) {
    ju  = min(j0 + step, n_years)      # hold the last year's inputs past it
    c_u = kg_dyn_tau_eq_flow(prims, ju)

    pv = pv + disc * as.numeric(crossprod(delta, c_u))

    # Carry the dollar forward: the surviving unrealized share ages, and the
    # routed share of the inherited stock moves to the heirs' cells.
    surv_w  = (1 - prims$m_eff[, ju]) * (1 - prims$r_S[, ju])
    route_w = prims$m_eff[, ju] * (1 - prims$p_char[, ju]) * prims$route[, ju]
    delta   = tA %*% (surv_w * delta) + tOmega %*% (route_w * delta)

    disc = disc * beta_by_year[ju]
    if (max(colSums(abs(delta))) < tol) break
  }

  setNames(pv, as.character(prims$ages))
}



kg_dyn_check_tau_eq = function(tau_eq_mat, tau_bt_mat, side,
                               carry_slack = 0) {

  # Checks that the priced tax is plausible. It is a discounted tax on one dollar
  # taxed roughly once, so it cannot be negative and cannot much exceed the
  # highest marginal rate in any cell. The 5% of slack covers the model's own
  # overlap between realization and death, since a year charges the realization
  # rate on the whole stock while deaths also take from it.
  #
  # With a wealth tax the dollar also pays the carrying cost every year it is
  # held, so it can legitimately exceed that cap. Callers pass the bound from
  # kg_dyn_carry_slack(), which is zero without a wealth tax.

  max_tau = max(tau_bt_mat, na.rm = TRUE)
  if (any(!is.finite(tau_eq_mat))) {
    stop('kg_dynamics: non-finite tau_eq_', side, ' values.')
  }
  if (any(tau_eq_mat < -1e-9)) {
    stop('kg_dynamics: negative tau_eq_', side, ' values (min = ',
         format(min(tau_eq_mat)), ').')
  }
  if (any(tau_eq_mat > max_tau * 1.05 + carry_slack + 1e-9)) {
    stop('kg_dynamics: tau_eq_', side, ' exceeds 1.05 * max cell tau + ',
         'carry slack (max = ', format(max(tau_eq_mat)), ' vs tau cap ',
         format(max_tau), ', carry_slack ', format(carry_slack), ').')
  }
  invisible(TRUE)
}



kg_dyn_carry_slack = function(prims, beta_by_year) {

  # Bounds how much the wealth tax can add to the priced tax. Each year held adds
  # at most the largest carrying cost, discounted and survived at some rate below
  # 1, so the total is that cost times rho / (1 - rho). Using the actual survival
  # and realization weights rather than the discount factor alone keeps the bound
  # tight enough to catch a real error.

  h_max = max(prims$h)
  if (h_max <= 0) return(0)
  surv = (1 - prims$m_eff) * (1 - prims$r_S)
  rho  = max(sweep(surv, 2, beta_by_year, `*`))
  stopifnot(is.finite(rho), rho >= 0, rho < 1)
  h_max * rho / (1 - rho)
}




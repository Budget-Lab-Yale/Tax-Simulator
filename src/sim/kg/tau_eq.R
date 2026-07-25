#-------------------------------------------------------------------------------
# tau_eq.R
#
# tau_eq: expected PV tax per dollar entering the gain state.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# tau_eq: expected PV tax per dollar entering the gain state
#
# The equity leg of the sigma income-conversion wedge (top-tax exercise; see
# other/top_tax/DESIGN_LOCK.md rulings 1 and 6). tau_eq(a, t) prices a dollar
# injected into cell (age a, year t)'s deviation stock at END of year t (the
# inheritance-inflow convention, same as the conversion inflow itself), as the
# present value of tax the kg machinery actually collects on it:
#
#   - realization tax r_S * tau on the FULL stock each year (matching
#     extra_R = r_S * dG in kg_dyn_build_cell_table — r_S is the full-pool
#     scenario rate incl. the retimed short-run timing overlay);
#   - deaths take the full stock (decedents do not realize in-year, matching
#     the recurrence event order): the taxable share (1 - p_char) * m_eff
#     splits by regime mix — delta_realize taxed at tau at death (deemed),
#     delta_route routed to heir cells via omega and taxed there in later
#     years (FULL heir taxes, no theta -- tau_eq is a tax-price, not the
#     holder-internalized Bellman burden), delta_vanish forgiven (step-up).
#
# Ground truth is the finite-difference harness (kg_dyn_tau_eq_finite_diff):
# forward-simulate the EXACT kg_dyn_step_recurrence marginal dynamics for a
# test dollar and accumulate discounted taxes. The production path is the
# linear backward recursion (kg_dyn_compute_tau_eq), which unit tests (and
# the SIGMA_TAU_EQ_FDCHECK=1 in-pass check) verify cell-by-cell against the
# finite difference:
#
#   T(a, j)      = c(a, j) + beta_j * [K_j T(., j+1)](a)
#   c(a, j)      = r_S*tau + m_eff*(1 - p_char)*delta_realize*tau
#   K_j          = diag((1-m_eff)(1-r_S)) A  +  diag(m_eff (1-p_char) route) omega
#   tau_eq(a, t_j) = beta_j * T(a, j+1)     (end-of-year entry: first events
#                                            in t+1, discounted back to t)
#
# Terminal condition mirrors the Bellman's stationary assumption: T at
# t_max + 1 solves the year-t_max stationary system (I - beta K) T = c, a
# contraction since row sums of beta*K are <= beta < 1.
#
# Deliberate approximations (documented, Bellman-consistent):
#   - taxes are priced at the cell-aggregate MTR tau (the record applier
#     allocates to records and taxes through the calculator; tau_eq is a
#     wedge input, not a revenue booking);
#   - the record-level deemed refinements (avoidance haircut, sec 121
#     netting) do NOT enter, mirroring their exclusion from c_phi/Bellman.
#-------------------------------------------------------------------------------

kg_dyn_tau_eq_primitives = function(baseline_cells, years, r_S_by_year,
                                    tau_bt_mat, mix_list, A, omega,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    h_bt_mat = NULL, e_bt_mat = NULL) {

  # Assembles the [age, year] primitive matrices on the bathtub grid that
  # both the tau_eq recursion and the finite-difference harness consume, so
  # the two code paths share inputs by construction.
  #
  # Parameters:
  #   - baseline_cells (list) : per-year cell tibbles (G_B, m, mG_record, ...)
  #   - years (int[])         : simulation years
  #   - r_S_by_year (list)    : per-year realization-rate vectors on the
  #                             bathtub grid (scenario: rate_info$r_S incl.
  #                             the retimed planned bucket; baseline: r_B)
  #   - tau_bt_mat (mat)      : [age, year] cell MTRs, bathtub slice
  #   - mix_list (list|NULL)  : per-year regime-mix tibbles (delta_route,
  #                             delta_realize); NULL = step-up everywhere
  #                             (the baseline-side assumption, mirroring
  #                             Pass 1's c_phi = 0)
  #   - A, omega (mat)        : aging and heir-routing operators
  #   - h_bt_mat (mat|NULL)   : [age, year] wealth-tax carrying cost
  #                             (bathtub slice of the packed h matrix,
  #                             post-kg.wealth_carry_scale); NULL = zeros
  #                             (baseline side — h_B == 0 by law, asserted
  #                             in kg_dyn_load_bathtub_inputs; and every
  #                             non-wealth scenario)
  #   - e_bt_mat (mat|NULL)   : [age, year] estate exposure of the death
  #                             value (bathtub slice of the packed e matrix;
  #                             gain-weighted cell mtr_estate_ded, clamped
  #                             to [0, 1]). LEG-PAIRED, unlike h: prims_B
  #                             must receive the baseline-law e_B (current
  #                             law HAS an estate tax) and prims_S the
  #                             reform-law e_S. NULL = zeros (isolated unit
  #                             tests only)
  #
  # Returns: list of [n_ages, n_years] matrices (m_eff, p_char, r_S, tau,
  #          route, realize, h, e) plus A, omega, ages, years.

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

  # Per-dollar year-j tax flow on one dollar of unrealized gain — the SINGLE
  # source of the c vector for BOTH the tau_eq backward recursion
  # (kg_dyn_compute_tau_eq) and the finite-difference verifier
  # (kg_dyn_tau_eq_finite_diff), so recursion and FD stay in lockstep by
  # construction. Three terms:
  #   realization:   r_S * tau
  #   death events:  m_eff * (1 - p_char) * realize * tau * (1 - e)
  #   wealth carry:  (1 - m_eff) * (1 - r_S) * h   — the SURVIVING-unrealized
  #                  share pays the wealth-tax carrying cost h = tau_w*tau_cg
  #                  on the deferred liability it keeps in the wealth base.
  # Timing convention (matches the Bellman sweep's survivor gate): wealth tax
  # assessed on END-OF-YEAR holdings, so in-year realizers and decedents pay
  # no carrying cost for that year. h == 0 leaves the flow untouched (x + 0 is
  # exact for finite x), so a non-wealth scenario is dormant here.
  #
  # Estate offset on the DEATH-REALIZE term only (leg-paired e = cell
  # mtr_estate_ded, clamped [0,1]): the death-triggered CG tax is deductible
  # against the taxable estate (Sec. 2053-style, gated by the
  # estate.income_tax_ded law switch baked into mtr_estate_ded), so a dollar
  # of deemed-realization tax collects only (1 - e) net of the estate-tax
  # offset. Deliberately NOT discounted:
  #   - the route term (carryover-to-heir): a DEFERRAL of basis, not a
  #     collection — no death-time income tax is stamped there, so there is
  #     no Sec. 2053 deduction to price.
  #   - the during-life r_S * tau term: CG tax paid during life also shrinks
  #     the future estate, but that channel is carried MECHANICALLY by the
  #     wealth bathtub (during-life tax payments enter F = dT0 - dY_exog and
  #     drain into the estate base at death); discounting here would
  #     double-count it. Demonstrated end-to-end by the one-record
  #     accounting test (other/kg_model_tests/test_estate_offset.R).
  #   - the wealth-carry h term: a during-life wealth-tax flow, same
  #     bathtub-carried logic.
  # e == 0 leaves the flow untouched (x * (1 - 0) is exact).
  #
  # Balance-sheet basis note: net_worth = sum(value.*) - debts with NO
  # netting of the contingent CG liability — that non-deductibility is
  # exactly why the carrying cost exists. Two-path check on $1 of gain,
  # tau_w = w, tau_cg = tau:
  #   realize now : pay tau today; the (1 - tau) proceeds stay in the wealth
  #                 base and pay w*(1 - tau) per year.
  #   hold        : pay nothing today; the FULL dollar stays in the wealth
  #                 base and pays w*1 per year.
  #   difference  : holding costs w*tau = h per year of continued deferral.

  # Missing e (hand-built prims in unit tests) defaults to 0 — the exact
  # pre-offset flow; kg_dyn_tau_eq_primitives always materializes it.
  e_j = if (is.null(prims$e)) 0 else prims$e[, j]

  prims$r_S[, j] * prims$tau[, j] +
    prims$m_eff[, j] * (1 - prims$p_char[, j]) *
      prims$realize[, j] * prims$tau[, j] * (1 - e_j) +
    (1 - prims$m_eff[, j]) * (1 - prims$r_S[, j]) * prims$h[, j]
}



kg_dyn_compute_tau_eq = function(prims, beta_by_year) {

  # Linear backward recursion for tau_eq on the bathtub grid (see the block
  # comment above for the model and conventions). beta_by_year is the same
  # per-year real-rate discount series the Bellman uses.
  #
  # Returns: list(tau_eq, T, T_stationary), tau_eq and T both [age, year].

  n_ages  = length(prims$ages)
  n_years = length(prims$years)
  stopifnot(length(beta_by_year) == n_years)

  dimnm = list(as.character(prims$ages), as.character(prims$years))

  # Per-year tax flow per dollar of start-of-year stock (shared with the FD
  # verifier via kg_dyn_tau_eq_flow — incl. the wealth-tax carrying cost on
  # the surviving-unrealized share), and the two continuation operators
  # (survivor aging + heir routing). Deemed composition note: h rides the
  # survivor branch only; HEIR carrying is priced through tau_eq's routing
  # continuation (the routed dollar re-enters the flow at the heir cell,
  # where it pays h again while deferred) — never in the holder Bellman.
  # Same asymmetry as the existing "full heir taxes, no theta" convention.
  c_mat = matrix(0, n_ages, n_years, dimnames = dimnm)
  for (j in seq_len(n_years)) c_mat[, j] = kg_dyn_tau_eq_flow(prims, j)
  K_of = function(j) {
    surv_w  = (1 - prims$m_eff[, j]) * (1 - prims$r_S[, j])
    route_w = prims$m_eff[, j] * (1 - prims$p_char[, j]) * prims$route[, j]
    surv_w * prims$A + route_w * prims$omega   # vector * matrix scales rows
  }

  # Stationary terminal: T(., t_max + 1) under year-t_max primitives.
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

  # Ground-truth tau_eq for injections at end of year index j0 (DESIGN_LOCK
  # ruling 1): forward-simulate the exact kg_dyn_step_recurrence marginal
  # dynamics for a test dollar in every age cell simultaneously (identity
  # injection matrix; columns = injection age) and accumulate the PV of tax
  # collected, discounted back to year j0. Primitives are held at their
  # year-t_max values beyond the simulation horizon, matching the
  # recursion's stationary terminal assumption.
  #
  # Returns: length-n_ages vector, tau_eq_FD(a, j0).

  n_ages  = length(prims$ages)
  n_years = length(prims$years)

  delta  = diag(n_ages)                # [current age, injection age]
  pv     = numeric(n_ages)
  disc   = beta_by_year[j0]            # discounts year-(j0+1) taxes to j0
  tA     = t(prims$A)
  tOmega = t(prims$omega)

  for (step in seq_len(horizon)) {
    ju  = min(j0 + step, n_years)      # hold t_max primitives beyond horizon
    c_u = kg_dyn_tau_eq_flow(prims, ju)

    pv = pv + disc * as.numeric(crossprod(delta, c_u))

    # Exact marginal delta dynamics: survivors (1-m_eff)(1-r_S) age via A;
    # the routed share of the dying stock moves to heir cells via omega.
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

  # In-pass sanity bounds: tau_eq is a (discounted, at-most-once-ish) tax on
  # one dollar, so it must be nonnegative and cannot meaningfully exceed the
  # max cell MTR. The 1.05 slack covers the model's own realization/death
  # event overlap (extra_R charges r_S on the full stock while deaths also
  # take it; overlap ~ r_S * m_eff per year).
  #
  # carry_slack: with an active wealth tax, the deferred dollar ALSO pays
  # the carrying cost h every surviving-unrealized year, so tau_eq can
  # legitimately exceed the pure-CG cap. Callers pass the OPERATOR bound
  # max(h) * rho / (1 - rho), rho = max over cells/years of
  # beta*(1 - m_eff)*(1 - r_S) — the actual survival-continuation weight
  # incl. mortality + realization exit (deliberately tighter than the bare
  # 1/(1 - beta) geometric bound, which is too loose to catch real bugs).
  # Zero when h = 0, so non-wealth runs keep the original cap exactly.

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

  # Operator bound on the wealth-carry contribution to tau_eq (see
  # kg_dyn_check_tau_eq): each surviving-unrealized year adds at most
  # max(h), discounted-and-survived at most rho per year, so the total
  # carry PV is bounded by max(h) * (rho + rho^2 + ...) = max(h)*rho/(1-rho).

  h_max = max(prims$h)
  if (h_max <= 0) return(0)
  surv = (1 - prims$m_eff) * (1 - prims$r_S)
  rho  = max(sweep(surv, 2, beta_by_year, `*`))
  stopifnot(is.finite(rho), rho >= 0, rho < 1)
  h_max * rho / (1 - rho)
}




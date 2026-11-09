#-------------------------------------------------------------------------------
# test_tau_eq_wealth.R
#
# T3 for the wealth-carry term in the tau_eq flow (kg_dyn_tau_eq_flow adds
# (1 - m_eff)*(1 - r_S)*h — the surviving-unrealized share pays the carrying
# cost while its deferred liability sits in the wealth base):
#
#   T3a  recursion vs finite-difference < 1e-10 with h = 0.005 under
#        step-up / carryover / deemed regime mixes
#   T3b  tau_eq strictly monotone in h (elementwise, h in {0, .005, .01})
#   T3c  1x1 closed form: tau_eq = beta*c/(1 - beta*K) spot check
#   T3d  h = 0 bitwise reproduction (h_bt_mat = NULL vs zero matrix), and
#        the flow helper at h = 0 equals the pre-carry two-term formula
#   T3e  guard: big h trips the un-slacked cap; kg_dyn_carry_slack admits it;
#        slack is 0 when h = 0
#
# Dependency-light synthetic prims (no full-sample data). Sbatch-only:
#   sbatch other/kg_model_tests/wealth_carry_tests.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse) })
for (f in sort(list.files('./src/sim/kg', full.names = TRUE))) source(f)

fail = function(...) stop(sprintf(...), call. = FALSE)
ok   = function(name) cat(sprintf('PASS  %s\n', name))

#===============================================================================
# Synthetic prims builder (bathtub-grid shapes, small ages set)
#===============================================================================

ages  = 60:70
years = 2026:2031
ac = as.character(ages); yc = as.character(years)
na_ = length(ages); ny_ = length(years)

# Row-stochastic aging operator: a -> a+1, top age self-absorbing.
A = matrix(0, na_, na_, dimnames = list(ac, ac))
for (i in 1:(na_ - 1)) A[i, i + 1] = 1
A[na_, na_] = 1

# Row-stochastic heir routing: every decedent age routes uniformly to the
# two youngest heir cells.
omega = matrix(0, na_, na_, dimnames = list(ac, ac))
omega[, 1] = 0.5; omega[, 2] = 0.5

mk = function(v) matrix(v, na_, ny_, dimnames = list(ac, yc))
set.seed(42)
m_eff_m = mk(rep(0.01 + 0.004 * (ages - min(ages)), ny_))
p_char_m = mk(0.05)
r_S_m    = matrix(runif(na_ * ny_, 0.03, 0.12), na_, ny_,
                  dimnames = list(ac, yc))
tau_m    = matrix(runif(na_ * ny_, 0.20, 0.35), na_, ny_,
                  dimnames = list(ac, yc))
beta_by_year = runif(ny_, 0.95, 0.98)

mk_prims = function(route_v, realize_v, h = NULL) {
  list(m_eff = m_eff_m, p_char = p_char_m, r_S = r_S_m, tau = tau_m,
       route = mk(route_v), realize = mk(realize_v),
       h = if (is.null(h)) mk(0) else h,
       A = A, omega = omega, ages = ages, years = years)
}

#===============================================================================
# T3a: recursion vs finite difference, h = 0.005, three regimes
#===============================================================================

h_005 = mk(0.005)
regimes = list(stepup    = c(0, 0),      # route, realize
               carryover = c(0.8, 0),
               deemed    = c(0, 0.9))
for (rg in names(regimes)) {
  pr = mk_prims(regimes[[rg]][1], regimes[[rg]][2], h = h_005)
  rec = kg_dyn_compute_tau_eq(pr, beta_by_year)$tau_eq
  for (j0 in seq_along(years)) {
    fd  = kg_dyn_tau_eq_finite_diff(pr, beta_by_year, j0, horizon = 3000,
                                    tol = 1e-16)
    err = max(abs(fd - rec[, j0]))
    if (err > 1e-10) {
      fail('T3a: %s regime, year idx %d: recursion vs FD err %.3e', rg, j0, err)
    }
  }
  ok(sprintf('T3a recursion == FD (<1e-10) with h = 0.005, %s regime', rg))
}

#===============================================================================
# T3b: monotone in h
#===============================================================================

pr0  = mk_prims(0.5, 0.3, h = mk(0))
pr5  = mk_prims(0.5, 0.3, h = mk(0.005))
pr10 = mk_prims(0.5, 0.3, h = mk(0.010))
te0  = kg_dyn_compute_tau_eq(pr0,  beta_by_year)$tau_eq
te5  = kg_dyn_compute_tau_eq(pr5,  beta_by_year)$tau_eq
te10 = kg_dyn_compute_tau_eq(pr10, beta_by_year)$tau_eq
if (!all(te5 > te0) || !all(te10 > te5)) {
  fail('T3b: tau_eq not strictly monotone in h')
}
ok('T3b tau_eq strictly monotone in h (0 < .005 < .01, elementwise)')

#===============================================================================
# T3c: 1x1 closed form tau_eq = beta*c / (1 - beta*K)
#===============================================================================

A1 = matrix(1, 1, 1, dimnames = list('70', '70'))
pr1 = list(m_eff  = matrix(0.05,  1, 1, dimnames = list('70', '2026')),
           p_char = matrix(0.10,  1, 1, dimnames = list('70', '2026')),
           r_S    = matrix(0.08,  1, 1, dimnames = list('70', '2026')),
           tau    = matrix(0.30,  1, 1, dimnames = list('70', '2026')),
           route  = matrix(0,     1, 1, dimnames = list('70', '2026')),
           realize= matrix(0.9,   1, 1, dimnames = list('70', '2026')),
           h      = matrix(0.004, 1, 1, dimnames = list('70', '2026')),
           A = A1, omega = A1 * 0, ages = 70, years = 2026)
b1 = 0.96
c_hand = 0.08 * 0.30 + 0.05 * (1 - 0.10) * 0.9 * 0.30 +
         (1 - 0.05) * (1 - 0.08) * 0.004
K_hand = (1 - 0.05) * (1 - 0.08) * 1          # surv branch only (route = 0)
tau_eq_hand = b1 * c_hand / (1 - b1 * K_hand)
te1 = kg_dyn_compute_tau_eq(pr1, b1)$tau_eq[1, 1]
if (abs(te1 - tau_eq_hand) > 1e-12) {
  fail('T3c: 1x1 closed form %.15f != recursion %.15f', tau_eq_hand, te1)
}
ok('T3c 1x1 closed form beta*c/(1 - beta*K) matches recursion')

#===============================================================================
# T3d: h = 0 bitwise reproduction + flow helper equals pre-carry formula
#===============================================================================

pr_null = mk_prims(0.5, 0.3, h = NULL)   # mk_prims fills zeros
pr_zero = mk_prims(0.5, 0.3, h = mk(0))
te_null = kg_dyn_compute_tau_eq(pr_null, beta_by_year)
te_zero = kg_dyn_compute_tau_eq(pr_zero, beta_by_year)
if (!identical(te_null, te_zero)) fail('T3d: h NULL vs zero-matrix differ')

# Pre-carry two-term flow, computed exactly as the old c_mat expression:
old_c = pr_zero$r_S * pr_zero$tau +
        pr_zero$m_eff * (1 - pr_zero$p_char) * pr_zero$realize * pr_zero$tau
new_c = sapply(seq_along(years), function(j) kg_dyn_tau_eq_flow(pr_zero, j))
dimnames(new_c) = dimnames(old_c)
if (!identical(unname(old_c), unname(new_c))) {
  fail('T3d: kg_dyn_tau_eq_flow at h = 0 is not bitwise the pre-carry flow')
}
ok('T3d h = 0 bitwise reproduction (recursion + flow helper)')

#===============================================================================
# T3e: guard slack
#===============================================================================

# Big h relative to tau: carry PV pushes tau_eq above the bare 1.05*max_tau
# cap. Long-horizon single cell, low exit rates, h comparable to tau.
prg = list(m_eff  = matrix(0.01, 1, 1, dimnames = list('60', '2026')),
           p_char = matrix(0,    1, 1, dimnames = list('60', '2026')),
           r_S    = matrix(0.02, 1, 1, dimnames = list('60', '2026')),
           tau    = matrix(0.30, 1, 1, dimnames = list('60', '2026')),
           route  = matrix(0,    1, 1, dimnames = list('60', '2026')),
           realize= matrix(0,    1, 1, dimnames = list('60', '2026')),
           h      = matrix(0.02, 1, 1, dimnames = list('60', '2026')),
           A = matrix(1, 1, 1), omega = matrix(0, 1, 1),
           ages = 60, years = 2026)
bg = 0.98
teg = kg_dyn_compute_tau_eq(prg, bg)$tau_eq
trip = tryCatch({ kg_dyn_check_tau_eq(teg, prg$tau, 'S'); FALSE },
                error = function(e) TRUE)
if (!trip) fail('T3e: un-slacked cap did not trip on a high-h cell')
slack = kg_dyn_carry_slack(prg, bg)
kg_dyn_check_tau_eq(teg, prg$tau, 'S', carry_slack = slack)  # must not stop
if (kg_dyn_carry_slack(mk_prims(0.5, 0.3, h = mk(0)), beta_by_year) != 0) {
  fail('T3e: carry slack must be exactly 0 when h = 0')
}
ok('T3e guard: trips without slack, admits with operator slack, 0 at h = 0')

cat('\nALL TAU_EQ WEALTH-CARRY TESTS PASSED\n')

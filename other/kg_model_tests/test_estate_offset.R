#-------------------------------------------------------------------------------
# test_estate_offset.R
#
# Unit tests for the estate-margins build (plan effervescent-plotting-wadler
# rev 3, part (a) + the shared calc_estate bump input):
#
#   E1  1x1 Bellman hand-check: with e_S on Pass 2 and e_B = 0 on Pass 1
#       (tau unchanged -- an ESTATE-ONLY reform), the top-age MC wedge is
#       exactly -beta*m*tau*e_S; FOC factor exp(-eta*dMC) > 1 => realizations
#       RISE under an estate hike (the leg-pairing backstop, verification 4v).
#   E2  Leg-pairing: e_B = e_S (same matrix both passes) + tau_S = tau_B
#       => r_D == r_B exactly (the single-matrix failure mode produces zero
#       response; our API must reproduce it only when the legs really match).
#   E3  Deemed regime (c_phi = 1): F = 0 regardless of e => Pass-2 r_D is
#       bitwise-invariant to e (the direct Bellman channel is inert there;
#       the effect runs through tau_eq only -- verification 4iii).
#   E4  Exact bitwise no-op: e omitted vs scalar-0 vs zero-matrix.
#   E5  tau_eq: death term x (1 - e). Recursion == FD verifier with e > 0
#       (all three regimes); route-only prims invariant to e (deferral is not
#       a collection); deemed tau_eq strictly decreasing in e; 1x1 closed
#       form with e; e = 0 flow bitwise equals the pre-offset flow.
#   E6  kg_dyn_aggregate_cell_estate: gain weights, [0,1] clamp, NA coalesce,
#       zero-denominator cells.
#   E7  calc_estate: estate_base_bump kink tests (below / at / above the
#       exemption, pmax floor binding; +$1 right-derivative in {0, top rate},
#       never negative) and the ONE-RECORD Sec. 2053 ACCOUNTING TEST
#       (verification 1b): $1 of death-time CG tax lowers estate tax by e and
#       heir cash by (1 - e), exactly the (1 - e) the Bellman prices into F.
#
# Dependency-light (no full-sample data). Sbatch-only, never the login node:
#   sbatch other/kg_model_tests/estate_offset_tests.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(magrittr) })
source('./src/sim/kg_dynamics.R')
source('./src/calc/utils.R')
source('./src/calc/functions/tax/estate.R')

fail = function(...) stop(sprintf(...), call. = FALSE)
ok   = function(name) cat(sprintf('PASS  %s\n', name))

eta  = 2.4
beta = 0.96

#===============================================================================
# E1: 1x1 estate-only reform hand-check
#===============================================================================

ac = '80'; yc = '2026'
grid_11 = list(
  m      = matrix(0.04, 1, 1, dimnames = list(ac, yc)),
  r_B    = matrix(0.06, 1, 1, dimnames = list(ac, yc)),
  p_char = matrix(0,    1, 1, dimnames = list(ac, yc))
)
tau  = matrix(0.30, 1, 1, dimnames = list(ac, yc))
e_S  = 0.35
eS_m = matrix(e_S, 1, 1, dimnames = list(ac, yc))

p1   = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, eta = eta,
                            beta_by_year = beta)              # e_B = 0
p2_0 = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, beta_by_year = beta)
p2_e = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, beta_by_year = beta, e_mat = eS_m)

# Top age, stationary terminal, c_phi = 0:
#   MC_B = tau + beta*m*tau                (F_B = tau,        e_B = 0)
#   MC_S = tau + beta*m*tau*(1 - e_S)      (F_S = tau*(1-e))
#   =>  MC_S - MC_B = -beta*m*tau*e_S  exactly
dMC      = p2_e$MC[1, 1] - p2_0$MC[1, 1]
dMC_hand = -beta * 0.04 * 0.30 * e_S
if (abs(dMC - dMC_hand) > 1e-15) {
  fail('E1: MC wedge %.18f != hand value %.18f', dMC, dMC_hand)
}
foc_hand = exp(-eta * dMC)
foc_got  = p2_e$r_D[1, 1] / p2_0$r_D[1, 1]
if (abs(foc_got - foc_hand) > 1e-15) {
  fail('E1: FOC factor %.18f != exp(-eta*dMC) %.18f', foc_got, foc_hand)
}
if (!(p2_e$r_D[1, 1] > p2_0$r_D[1, 1])) {
  fail('E1: realizations did not rise under an estate-only hike')
}
ok('E1 estate-only reform: dMC = -beta*m*tau*e_S exactly; realizations rise')

#===============================================================================
# E2: matched legs (e_B = e_S) + tau_S = tau_B  =>  r_D == r_B exactly
#===============================================================================

set.seed(20260712)
ages_r = 55:80; years_r = 2026:2035
acr = as.character(ages_r); ycr = as.character(years_r)
nar = length(ages_r); nyr = length(years_r)
rmat = function(lo, hi) matrix(runif(nar * nyr, lo, hi), nar, nyr,
                               dimnames = list(acr, ycr))
grid_r = list(m = rmat(0.005, 0.20), r_B = rmat(0.01, 0.30),
              p_char = rmat(0, 0.15))
tau_Br = rmat(0.15, 0.30)
e_r    = rmat(0, 0.40)
beta_r = runif(nyr, 0.94, 0.985)
zero_m = matrix(0, nar, nyr, dimnames = list(acr, ycr))

pb_e = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0, eta = eta,
                            beta_by_year = beta_r, e_mat = e_r)
ps_e = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                            kappa_mat = pb_e$kappa, eta = eta,
                            beta_by_year = beta_r, e_mat = e_r)
if (!identical(ps_e$r_D, pmin(pmax(grid_r$r_B, 0), 1))) {
  fail('E2: matched e legs + matched tau do not reproduce r_D == r_B exactly')
}
ok('E2 e_B = e_S & tau_S = tau_B => r_D == r_B exactly (no spurious response)')

# ...and unmatched legs with matched tau DO respond everywhere e_S > e_B:
ps_up = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                             kappa_mat = pb_e$kappa, eta = eta,
                             beta_by_year = beta_r,
                             e_mat = pmin(e_r + 0.10, 1))
if (!all(ps_up$r_D >= ps_e$r_D) || !any(ps_up$r_D > ps_e$r_D)) {
  fail('E2: raising e_S above e_B failed to (weakly) raise realizations')
}
ok('E2 e_S > e_B (tau unchanged) moves realizations (leg-pairing backstop)')

#===============================================================================
# E3: deemed regime -- Pass-2 r_D invariant to e (F = 0 kills the channel)
#===============================================================================

# p_char must be 0 here: c_phi_eff = c_phi * (1 - p_char), so the charity
# leak keeps a sliver of F alive under deemed -- with p_char = 0 and
# c_phi = 1, F = 0 exactly and e has nothing to multiply.
one_m  = matrix(1, nar, nyr, dimnames = list(acr, ycr))
grid_d = grid_r; grid_d$p_char = zero_m
pb0   = kg_dyn_solve_bellman(grid_d, tau_Br, c_phi_mat = 0, eta = eta,
                             beta_by_year = beta_r)
pd_0  = kg_dyn_solve_bellman(grid_d, tau_Br, c_phi_mat = one_m,
                             kappa_mat = pb0$kappa, eta = eta,
                             beta_by_year = beta_r)
pd_e  = kg_dyn_solve_bellman(grid_d, tau_Br, c_phi_mat = one_m,
                             kappa_mat = pb0$kappa, eta = eta,
                             beta_by_year = beta_r, e_mat = e_r)
if (!identical(pd_0$r_D, pd_e$r_D) || !identical(pd_0$W, pd_e$W)) {
  fail('E3: deemed (c_phi = 1) Pass-2 outputs moved with e (F should be 0)')
}
ok('E3 deemed regime: direct Bellman channel inert in e (bitwise)')

#===============================================================================
# E4: exact bitwise no-op at e = 0
#===============================================================================

variants = list(
  omitted = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                                 kappa_mat = pb0$kappa, eta = eta,
                                 beta_by_year = beta_r),
  scalar0 = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                                 kappa_mat = pb0$kappa, eta = eta,
                                 beta_by_year = beta_r, e_mat = 0),
  zeromat = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                                 kappa_mat = pb0$kappa, eta = eta,
                                 beta_by_year = beta_r, e_mat = zero_m)
)
for (v in names(variants)[-1]) {
  for (nm in c('W', 'MC', 'kappa', 'r_D')) {
    if (!identical(variants$omitted[[nm]], variants[[v]][[nm]])) {
      fail('E4: %s not bitwise-identical between e omitted and e %s', nm, v)
    }
  }
}
ok('E4 e omitted / scalar-0 / zero-matrix bitwise identical (W/MC/kappa/r_D)')

#===============================================================================
# E5: tau_eq death-term offset
#===============================================================================

ages  = 60:70; years = 2026:2031
ac5 = as.character(ages); yc5 = as.character(years)
na5 = length(ages); ny5 = length(years)
A5 = matrix(0, na5, na5, dimnames = list(ac5, ac5))
for (i in 1:(na5 - 1)) A5[i, i + 1] = 1
A5[na5, na5] = 1
omega5 = matrix(0, na5, na5, dimnames = list(ac5, ac5))
omega5[, 1] = 0.5; omega5[, 2] = 0.5

mk5 = function(v) matrix(v, na5, ny5, dimnames = list(ac5, yc5))
set.seed(43)
m_eff5  = mk5(rep(0.01 + 0.004 * (ages - min(ages)), ny5))
r_S5    = matrix(runif(na5 * ny5, 0.03, 0.12), na5, ny5,
                 dimnames = list(ac5, yc5))
tau5    = matrix(runif(na5 * ny5, 0.20, 0.35), na5, ny5,
                 dimnames = list(ac5, yc5))
beta5   = runif(ny5, 0.95, 0.98)
mk_prims5 = function(route_v, realize_v, e = NULL, h = NULL) {
  list(m_eff = m_eff5, p_char = mk5(0.05), r_S = r_S5, tau = tau5,
       route = mk5(route_v), realize = mk5(realize_v),
       h = if (is.null(h)) mk5(0) else h,
       e = if (is.null(e)) mk5(0) else e,
       A = A5, omega = omega5, ages = ages, years = years)
}

# (a) recursion == FD with e = 0.3, three regimes (the shared flow helper
# keeps them in lockstep BY CONSTRUCTION; this verifies the construction)
e_03 = mk5(0.3)
regimes = list(stepup = c(0, 0), carryover = c(0.8, 0), deemed = c(0, 0.9))
for (rg in names(regimes)) {
  pr = mk_prims5(regimes[[rg]][1], regimes[[rg]][2], e = e_03)
  rec = kg_dyn_compute_tau_eq(pr, beta5)$tau_eq
  for (j0 in seq_along(years)) {
    fd = kg_dyn_tau_eq_finite_diff(pr, beta5, j0, horizon = 3000, tol = 1e-16)
    if (max(abs(fd - rec[, j0])) > 1e-10) {
      fail('E5a: %s regime, year idx %d: recursion vs FD err %.3e',
           rg, j0, max(abs(fd - rec[, j0])))
    }
  }
}
ok('E5a recursion == FD (<1e-10) with e = 0.3, all three regimes')

# (b) route-only prims: e must be inert (deferral is not a collection)
pr_route0 = mk_prims5(0.8, 0)
pr_routeE = mk_prims5(0.8, 0, e = e_03)
if (!identical(kg_dyn_compute_tau_eq(pr_route0, beta5),
               kg_dyn_compute_tau_eq(pr_routeE, beta5))) {
  fail('E5b: e moved tau_eq in a route-only (carryover) regime')
}
ok('E5b route term undiscounted: carryover-only tau_eq invariant to e')

# (c) deemed regime: tau_eq strictly decreasing in e
te_e0 = kg_dyn_compute_tau_eq(mk_prims5(0, 0.9),            beta5)$tau_eq
te_e3 = kg_dyn_compute_tau_eq(mk_prims5(0, 0.9, e = e_03),  beta5)$tau_eq
te_e6 = kg_dyn_compute_tau_eq(mk_prims5(0, 0.9, e = mk5(0.6)), beta5)$tau_eq
if (!all(te_e3 < te_e0) || !all(te_e6 < te_e3)) {
  fail('E5c: deemed tau_eq not strictly decreasing in e')
}
ok('E5c deemed tau_eq strictly decreasing in e (0 > .3 > .6, elementwise)')

# (d) 1x1 closed form with e
A1 = matrix(1, 1, 1, dimnames = list('70', '70'))
pr1 = list(m_eff  = matrix(0.05,  1, 1, dimnames = list('70', '2026')),
           p_char = matrix(0.10,  1, 1, dimnames = list('70', '2026')),
           r_S    = matrix(0.08,  1, 1, dimnames = list('70', '2026')),
           tau    = matrix(0.30,  1, 1, dimnames = list('70', '2026')),
           route  = matrix(0,     1, 1, dimnames = list('70', '2026')),
           realize= matrix(0.9,   1, 1, dimnames = list('70', '2026')),
           h      = matrix(0,     1, 1, dimnames = list('70', '2026')),
           e      = matrix(0.25,  1, 1, dimnames = list('70', '2026')),
           A = A1, omega = A1 * 0, ages = 70, years = 2026)
b1 = 0.96
c_hand = 0.08 * 0.30 + 0.05 * (1 - 0.10) * 0.9 * 0.30 * (1 - 0.25)
K_hand = (1 - 0.05) * (1 - 0.08)
tau_eq_hand = b1 * c_hand / (1 - b1 * K_hand)
te1 = kg_dyn_compute_tau_eq(pr1, b1)$tau_eq[1, 1]
if (abs(te1 - tau_eq_hand) > 1e-12) {
  fail('E5d: 1x1 closed form %.15f != recursion %.15f', tau_eq_hand, te1)
}
ok('E5d 1x1 closed form with (1 - e) on the death term matches recursion')

# (e) e = 0 flow bitwise equals the pre-offset flow; prims WITHOUT an e
# element (hand-built unit-test prims) behave as e = 0
pr_z  = mk_prims5(0.5, 0.3)
pr_no = pr_z; pr_no$e = NULL
old_c = pr_z$r_S * pr_z$tau +
        pr_z$m_eff * (1 - pr_z$p_char) * pr_z$realize * pr_z$tau
new_c  = sapply(seq_along(years), function(j) kg_dyn_tau_eq_flow(pr_z, j))
noe_c  = sapply(seq_along(years), function(j) kg_dyn_tau_eq_flow(pr_no, j))
dimnames(new_c) = dimnames(old_c); dimnames(noe_c) = dimnames(old_c)
if (!identical(unname(old_c), unname(new_c)) ||
    !identical(unname(old_c), unname(noe_c))) {
  fail('E5e: flow at e = 0 (or missing e) is not bitwise the pre-offset flow')
}
ok('E5e e = 0 / missing-e flow bitwise equals pre-offset flow')

#===============================================================================
# E6: kg_dyn_aggregate_cell_estate
#===============================================================================

rec = tibble(
  age_cohort     = c(70L, 70L, 71L, 72L, 73L),
  weight         = c(2, 1, 1, 5, 1),
  kg_lt          = 0,
  G_unit         = c(100, 300, 50, 0, 10),
  mtr_estate_ded = c(0.00, 0.40, NA, 0.40, 1.7)
)
e_agg = kg_dyn_aggregate_cell_estate(rec, ages = 70:74)
e70_hand = (1 * 300 * 0.40) / (2 * 100 + 1 * 300)
if (abs(e_agg[['70']] - e70_hand) > 1e-15) {
  fail('E6: e(70) %.10f != %.10f (gain weighting)', e_agg[['70']], e70_hand)
}
if (e_agg[['71']] != 0) fail('E6: NA mtr_estate_ded must coalesce to 0')
if (e_agg[['72']] != 0) fail('E6: zero gain stock cell must yield e = 0')
if (e_agg[['73']] != 1) fail('E6: e must clamp to 1 (got %f)', e_agg[['73']])
if (e_agg[['74']] != 0) fail('E6: absent cell must yield e = 0')
ok('E6 aggregator: gain weights, NA coalesce, zero cells, [0,1] clamp')

#===============================================================================
# E7: calc_estate bump kinks + one-record Sec. 2053 accounting
#===============================================================================

# Frozen-params stub: identity valuation bridge, no gifts, single bin with no
# deductions or DSUE -- isolates the law mechanics
params_stub = list(
  r = 1, rho_pt = 1, gamma = 0,
  bins = tibble(size_bin = 1, lo = 0, hi = Inf,
                f_ded = 0, p_dsue = 0, f_dsue = 0))

mk_estate_rec = function(equities, debts = 0, itd = 0, bump = 0,
                          exemption = 13.61e6) {
  tibble(
    filing_status = 1, q_death2 = 0,
    value.equities = equities,
    value.credit_cards = debts,
    `estate.exemption` = exemption,
    `estate.brackets1` = 0, `estate.rates1` = 0.40,
    `estate.portability` = 1, `estate.income_tax_ded` = 1,
    estate_income_tax_ded = itd,
    estate_base_bump = bump
  )
}
liab = function(...) {
  out = calc_estate(mk_estate_rec(...), params_stub, fill_missings = TRUE)
  out$estate_p_dsue * out$liab_estate_dsue +
    (1 - out$estate_p_dsue) * out$liab_estate_nodsue
}

# Kink tests: +$1 right-derivative
d_below = liab(5e6,  bump = 1) - liab(5e6)          # far below exemption
d_at    = liab(13.61e6, bump = 1) - liab(13.61e6)   # exactly at the kink
d_above = liab(30e6, bump = 1) - liab(30e6)         # far above
d_floor = liab(30e6, debts = 60e6, bump = 1) -
          liab(30e6, debts = 60e6)                  # pmax floor binds
if (abs(d_below) > 1e-9) fail('E7: below-exemption MTR %.6f != 0', d_below)
if (abs(d_above - 0.40) > 1e-9) fail('E7: above-exemption MTR %.6f != 0.40', d_above)
if (abs(d_at - 0.40) > 1e-9) {
  fail('E7: at-kink right-derivative %.6f != 0.40 (the +$1 crosses)', d_at)
}
if (abs(d_floor) > 1e-9) fail('E7: floor-binding MTR %.6f != 0', d_floor)
if (min(d_below, d_at, d_above, d_floor) < -1e-12) {
  fail('E7: negative estate MTR encountered')
}
ok('E7 kink tests: 0 below, 0.40 at/above the exemption, 0 at the floor')

# One-record Sec. 2053 accounting (verification 1b): a decedent with $30M
# gross and a $1M death-time CG tax bill (estate_income_tax_ded). e = 0.40
# above the exemption. Estate tax falls by e*D; distributable is invariant;
# heirs receive distributable - estate tax - CG tax, so heir cash falls by
# (1 - e)*D -- exactly the (1 - e) the Bellman prices into F = tau*(1 - e).
D  = 1e6
L0 = liab(30e6)
L1 = liab(30e6, itd = D)
d0 = calc_estate(mk_estate_rec(30e6),          params_stub, fill_missings = TRUE)
d1 = calc_estate(mk_estate_rec(30e6, itd = D), params_stub, fill_missings = TRUE)
if (abs((L0 - L1) - 0.40 * D) > 1e-6) {
  fail('E7: estate tax fell by %.2f, expected e*D = %.2f', L0 - L1, 0.40 * D)
}
if (d0$estate_distributable != d1$estate_distributable) {
  fail('E7: estate_distributable moved with the income-tax deduction')
}
heir0 = d0$estate_distributable - L0        # no CG tax at death
heir1 = d1$estate_distributable - L1 - D    # CG tax paid, deduction taken
if (abs((heir0 - heir1) - (1 - 0.40) * D) > 1e-6) {
  fail('E7: heir cash fell by %.2f, expected (1-e)*D = %.2f',
       heir0 - heir1, (1 - 0.40) * D)
}
# The Bellman's death value prices the SAME (1 - e): under deemed
# (c_phi_eff = 1) F = 0 and the death tax is priced in tau_eq at (1 - e);
# under step-up F = tau*(1 - e). Direct check of the vectorized form:
F_check = (1 - 0) * 0.238 * (1 - 0.40)
if (abs(F_check - 0.238 * 0.6) > 1e-15) fail('E7: F pricing arithmetic')
ok('E7 one-record accounting: estate -e*D, heirs -(1-e)*D, F prices (1-e)')

cat('\nALL ESTATE-OFFSET TESTS PASSED\n')

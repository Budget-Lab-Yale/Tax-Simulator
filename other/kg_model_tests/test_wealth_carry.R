#-------------------------------------------------------------------------------
# test_wealth_carry.R
#
# T1/T2 for the wealth-tax deferral carrying cost h in the kg Bellman
# (plan: enumerated-meandering-pinwheel; h debits the survivor continuation,
# death_cont = bs*(W_next - h) + bm*F).
#
#   T1a  1x1 grid hand-check: MC_S - MC_B = -beta*(1-m)*h exactly at top age;
#        FOC factor r_D_S/r_D_B matches exp(-eta*dMC) to 1e-15; r_D up, W down.
#   T1b  2-age stationary chain hand-computed from the algebra (verifies h
#        compounding through W_next, not just the top-age direct debit).
#   T2   Exact no-op: rich heterogeneous grid; h omitted vs scalar-0 vs
#        zero-matrix => identical() (bitwise) W/MC/kappa/r_D. tau_S = tau_B
#        and h = 0 => r_D == r_B exactly.
#   +    kg_dyn_aggregate_cell_carry: record-level product (not product of
#        separately averaged rates), gain-weighting, zero-denominator cells.
#   +    kg_dyn_wealth_law_active gate on synthetic tax_law frames.
#
# Dependency-light (no full-sample data). Sbatch-only, never the login node:
#   sbatch other/kg_model_tests/wealth_carry_tests.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse) })
source('./src/sim/kg_dynamics.R')

fail = function(...) stop(sprintf(...), call. = FALSE)
ok   = function(name) cat(sprintf('PASS  %s\n', name))

#===============================================================================
# T1a: 1x1 grid hand-check
#===============================================================================

ac = '80'; yc = '2026'
grid_11 = list(
  m      = matrix(0.04, 1, 1, dimnames = list(ac, yc)),
  r_B    = matrix(0.06, 1, 1, dimnames = list(ac, yc)),
  p_char = matrix(0,    1, 1, dimnames = list(ac, yc))
)
tau  = matrix(0.30, 1, 1, dimnames = list(ac, yc))
beta = 0.96
h    = 0.003   # 1% wealth tax x 30% CG rate
eta  = 2.4

p1   = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, eta = eta,
                            beta_by_year = beta)
p2_0 = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, beta_by_year = beta)
p2_h = kg_dyn_solve_bellman(grid_11, tau, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, beta_by_year = beta,
                            h_mat = matrix(h, 1, 1, dimnames = list(ac, yc)))

# Top age, stationary terminal: W_next = 0, so
#   MC_B = tau + beta*m*tau          (death_cont_B = bm*F, F = tau at c_phi=0)
#   MC_S = tau + beta*(1-m)*(0 - h) + beta*m*tau
#   =>  MC_S - MC_B = -beta*(1-m)*h  exactly
dMC      = p2_h$MC[1, 1] - p2_0$MC[1, 1]
dMC_hand = -beta * (1 - 0.04) * h
if (abs(dMC - dMC_hand) > 1e-15) {
  fail('T1a: MC wedge %.18f != hand value %.18f', dMC, dMC_hand)
}
ok('T1a MC_S - MC_B = -beta*(1-m)*h (|err| <= 1e-15)')

# FOC factor: r_D_S = r_D_B * exp(-eta * dMC), dMC < 0 => factor > 1
foc_hand = exp(-eta * dMC)
foc_got  = p2_h$r_D[1, 1] / p2_0$r_D[1, 1]
if (abs(foc_got - foc_hand) > 1e-15) {
  fail('T1a: FOC factor %.18f != exp(-eta*dMC) %.18f', foc_got, foc_hand)
}
if (!(p2_h$r_D[1, 1] > p2_0$r_D[1, 1])) fail('T1a: r_D did not rise under h')
if (!(p2_h$W[1, 1]   < p2_0$W[1, 1]))   fail('T1a: W did not fall under h')
ok('T1a FOC factor exp(-eta*dMC) to 1e-15; r_D up; W down')

#===============================================================================
# T1b: 2-age stationary chain, hand-computed from the algebra
#===============================================================================

ages2 = c(79, 80); ac2 = as.character(ages2)
m2    = c(0.03, 0.05)
rB2   = c(0.05, 0.07)
tau2  = c(0.28, 0.32)
h2    = c(0.0040, 0.0025)

grid_2 = list(
  m      = matrix(m2,  2, 1, dimnames = list(ac2, yc)),
  r_B    = matrix(rB2, 2, 1, dimnames = list(ac2, yc)),
  p_char = matrix(0,   2, 1, dimnames = list(ac2, yc))
)
tau_m2 = matrix(tau2, 2, 1, dimnames = list(ac2, yc))
h_m2   = matrix(h2,   2, 1, dimnames = list(ac2, yc))

# Independent re-implementation of the sweep algebra (baseline then scenario
# with h), single stationary year: backward in age, W_next from same sweep.
hand_sweep = function(h_vec, kappa_vec = NULL) {
  n = 2
  W = MC = rD = kap = numeric(n)
  bs = beta * (1 - m2); bm = beta * m2
  F_ = tau2                       # c_phi = 0
  for (i in n:1) {
    W_next = if (i == n) 0 else W[i + 1]
    dc  = bs[i] * (W_next - h_vec[i]) + bm[i] * F_[i]
    MCi = tau2[i] + dc
    if (is.null(kappa_vec)) {     # pass 1: r_D = r_B, kappa = MC
      rDi = rB2[i]; kapi = MCi; Ci = 0
    } else {                      # pass 2: FOC closed form + entropy cost
      kapi = kappa_vec[i]
      rDi  = min(rB2[i] * exp(-eta * (MCi - kapi)), 1)
      Ci   = (rDi * log(rDi / rB2[i]) - rDi + rB2[i]) / eta
    }
    W[i]  = kapi * rDi - Ci - tau2[i] * rDi + max(1 - rDi, 0) * dc
    MC[i] = MCi; rD[i] = rDi; kap[i] = kapi
  }
  list(W = W, MC = MC, r_D = rD, kappa = kap)
}

hb = hand_sweep(rep(0, 2))                      # baseline: h never enters
hs = hand_sweep(h2, kappa_vec = hb$kappa)       # scenario with h

q1 = kg_dyn_solve_bellman(grid_2, tau_m2, c_phi_mat = 0, eta = eta,
                          beta_by_year = beta)
q2 = kg_dyn_solve_bellman(grid_2, tau_m2, c_phi_mat = 0, kappa_mat = q1$kappa,
                          eta = eta, beta_by_year = beta, h_mat = h_m2)

for (nm in c('W', 'MC', 'r_D')) {
  err = max(abs(hs[[nm]] - as.numeric(q2[[nm]][, 1])))
  if (err > 1e-14) fail('T1b: %s mismatch vs hand chain (max err %.3e)', nm, err)
}
# The younger cell's MC wedge must compound BOTH its own h debit and the
# W_next drop inherited from the older cell:
dMC79      = q2$MC['79', 1] - q1$MC['79', 1]
dW80       = q2$W['80', 1] - q1$W['80', 1]      # < 0
dMC79_hand = beta * (1 - m2[1]) * (dW80 - h2[1])
if (abs(dMC79 - dMC79_hand) > 1e-14) {
  fail('T1b: age-79 wedge %.18f != bs*(dW80 - h) %.18f', dMC79, dMC79_hand)
}
if (!(abs(dMC79) > beta * (1 - m2[1]) * h2[1])) {
  fail('T1b: age-79 wedge does not exceed its direct debit (no compounding?)')
}
ok('T1b 2-age stationary chain matches hand algebra; h compounds via W_next')

#===============================================================================
# T2: exact bitwise no-op at h = 0
#===============================================================================

set.seed(20260711)
ages_r = 55:80; years_r = 2026:2035
acr = as.character(ages_r); ycr = as.character(years_r)
nar = length(ages_r); nyr = length(years_r)
rmat = function(lo, hi) matrix(runif(nar * nyr, lo, hi), nar, nyr,
                               dimnames = list(acr, ycr))
grid_r = list(m = rmat(0.005, 0.20), r_B = rmat(0.01, 0.30),
              p_char = rmat(0, 0.15))
tau_Br = rmat(0.15, 0.30)
tau_Sr = rmat(0.25, 0.45)
cphi_r = rmat(0, 0.5)
beta_r = runif(nyr, 0.94, 0.985)
zero_m = matrix(0, nar, nyr, dimnames = list(acr, ycr))

pb = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0, eta = eta,
                          beta_by_year = beta_r)
variants = list(
  omitted   = kg_dyn_solve_bellman(grid_r, tau_Sr, c_phi_mat = cphi_r,
                                   kappa_mat = pb$kappa, eta = eta,
                                   beta_by_year = beta_r),
  scalar0   = kg_dyn_solve_bellman(grid_r, tau_Sr, c_phi_mat = cphi_r,
                                   kappa_mat = pb$kappa, eta = eta,
                                   beta_by_year = beta_r, h_mat = 0),
  zeromat   = kg_dyn_solve_bellman(grid_r, tau_Sr, c_phi_mat = cphi_r,
                                   kappa_mat = pb$kappa, eta = eta,
                                   beta_by_year = beta_r, h_mat = zero_m)
)
for (v in names(variants)[-1]) {
  for (nm in c('W', 'MC', 'kappa', 'r_D')) {
    if (!identical(variants$omitted[[nm]], variants[[v]][[nm]])) {
      fail('T2: %s not bitwise-identical between h omitted and h %s', nm, v)
    }
  }
}
ok('T2 h omitted / scalar-0 / zero-matrix bitwise identical (W/MC/kappa/r_D)')

# tau_S = tau_B and h = 0 => r_D == clip(r_B, 0, 1) exactly
p_same = kg_dyn_solve_bellman(grid_r, tau_Br, c_phi_mat = 0,
                              kappa_mat = pb$kappa, eta = eta,
                              beta_by_year = beta_r, h_mat = 0)
if (!identical(p_same$r_D, pmin(pmax(grid_r$r_B, 0), 1))) {
  fail('T2: tau_S = tau_B & h = 0 does not reproduce r_D == r_B exactly')
}
ok('T2 tau_S = tau_B & h = 0 => r_D == r_B exactly')

#===============================================================================
# Aggregator: record-level product, gain-weighted
#===============================================================================

rec = tibble(
  age_cohort    = c(70L, 70L, 71L, 72L),
  weight        = c(2, 1, 1, 5),
  kg_lt         = c(0, 0, 0, 0),
  G_unit        = c(100, 300, 50, 0),
  mtr_net_worth = c(0.00, 0.02, 0.01, NA),
  mtr_kg_lt     = c(0.20, 0.35, NA, 0.30)
)
carry = kg_dyn_aggregate_cell_carry(rec, ages = 70:73)

# age 70: record-level product = (2*100*0*0.20 + 1*300*0.02*0.35) / (2*100 + 1*300)
h70_hand = (1 * 300 * 0.02 * 0.35) / 500
if (abs(carry$h[['70']] - h70_hand) > 1e-15) {
  fail('aggregator: h(70) %.10f != %.10f', carry$h[['70']], h70_hand)
}
# Product of separately averaged rates would be biased low here:
tw70 = (2 * 100 * 0 + 1 * 300 * 0.02) / 500
tc70 = (2 * 100 * 0.20 + 1 * 300 * 0.35) / 500
if (!(h70_hand > tw70 * tc70)) {
  fail('aggregator test fixture no longer exercises Cov(tau_w, tau_cg) > 0')
}
if (abs(carry$tau_w[['70']] - tw70) > 1e-15) fail('aggregator: tau_w(70) wrong')
# age 71: NA mtr_kg_lt coalesced to 0 in the product
if (abs(carry$h[['71']] - 0) > 1e-15) fail('aggregator: h(71) should be 0')
# age 72: G_unit = 0 denominator => 0; age 73: absent cell => 0
if (carry$h[['72']] != 0 || carry$h[['73']] != 0) {
  fail('aggregator: zero-denominator / missing cells must yield 0')
}
ok('aggregator record-level product, gain weights, zero-cell fallbacks')

#===============================================================================
# Wealth-law gate
#===============================================================================

law_off  = tibble(year = 2026:2028, `wealth.rates1` = 0,
                  `ord.rates1` = 0.37)
law_on   = tibble(year = 2026:2028, `wealth.rates1` = 0,
                  `wealth.rates2` = c(0, 0.01, 0.01))
law_none = tibble(year = 2026:2028, `ord.rates1` = 0.37)
law_na   = tibble(year = 2026:2028, `wealth.rates1` = NA_real_)
if (kg_dyn_wealth_law_active(law_off))   fail('gate: all-zero rates => TRUE?')
if (!kg_dyn_wealth_law_active(law_on))   fail('gate: nonzero rate => FALSE?')
if (kg_dyn_wealth_law_active(law_none))  fail('gate: no wealth cols => TRUE?')
if (kg_dyn_wealth_law_active(law_na))    fail('gate: all-NA rates => TRUE?')
ok('wealth-law gate (zero / nonzero / absent / NA)')

cat('\nALL WEALTH-CARRY BELLMAN TESTS PASSED\n')

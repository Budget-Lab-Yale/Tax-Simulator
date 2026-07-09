#-------------------------------------------------------------------------------
# test_naive_limit.R
#
# Verifies the entropy-cost Bellman's defining properties end-to-end through
# backward induction, on a synthetic dependency-light grid (like
# test_planned_timing.R):
#
#   (a) With Phi = 0 (no inert bucket => phi_I = planned_share = 0), c_phi = 0
#       (step-up), and a PERMANENT tau shock, the scenario discretionary rate
#       satisfies
#         log(r_D_S / r_D_B) = -eta * (MC_S - MC_B)   exactly (to 1e-10),
#       elementwise across all (unclipped) cells and with the SAME implied
#       slope dlog(r_D)/dMC = -eta across +1/+5/+10pp shocks. This is the
#       globally constant-semi-elasticity property that the entropy cost buys
#       (and that the old quadratic cost did not have).
#
#   (b) The Phi -> 0 limit nests the naive CBO/JCT revmax arithmetic: with a
#       test-local eta bisected so the aggregate +1pp semi-elasticity equals
#       -2.52, the revenue-maximizing gains rate is ~ 1/2.52 ~= 0.397
#       (+/- an MC-amplification wobble).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse) })
source('./src/sim/kg_dynamics.R')

# ---- synthetic grid --------------------------------------------------------
ages  = 60:80
years = 2026:2030
ac    = as.character(ages); yc = as.character(years)
na_   = length(ages); ny_ = length(years)

# Mild wealth-mortality gradient (keeps MC amplification near-homogeneous).
m_age = 0.01 + 0.003 * (ages - min(ages))
grid  = list(
  m      = matrix(rep(m_age, ny_), na_, ny_, dimnames = list(ac, yc)),
  r_B    = matrix(0.05, na_, ny_, dimnames = list(ac, yc)),
  p_char = matrix(0,    na_, ny_, dimnames = list(ac, yc))
)
beta  = rep(0.96, ny_)

TAU_B = 0.238
tau_B = matrix(TAU_B, na_, ny_, dimnames = list(ac, yc))

solve_pair = function(eta, tau_S_scalar) {
  tau_S = matrix(tau_S_scalar, na_, ny_, dimnames = list(ac, yc))
  p1 = kg_dyn_solve_bellman(grid, tau_B, c_phi_mat = 0, eta = eta,
                            phi_I = 0, planned_share = 0, beta_by_year = beta)
  p2 = kg_dyn_solve_bellman(grid, tau_S, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, phi_I = 0, planned_share = 0,
                            beta_by_year = beta)
  list(p1 = p1, p2 = p2)
}

# ---- (a) constant-semi-elasticity identity ---------------------------------
ETA_A  = 4.0
shocks = c(0.01, 0.05, 0.10)
for (s in shocks) {
  res = solve_pair(ETA_A, TAU_B + s)
  # Tax increase => MC_S > MC_B => r_D_S < r_D_B < r_D_cap = 1: no cell clips.
  dMC = res$p2$MC - res$p1$kappa                  # kappa_pass1 == MC_B (entropy)
  lhs = log(res$p2$r_D / res$p1$r_D)
  stopifnot(max(abs(lhs - (-ETA_A * dMC))) < 1e-10)
  # Implied slope must equal -eta everywhere and identically across shocks.
  stopifnot(max(abs(lhs / dMC + ETA_A)) < 1e-8)
}
cat('(a) constant-semi-elasticity identity holds to 1e-10 across +1/+5/+10pp\n')

# ---- (b) Phi->0 revmax nests the naive 1/|semi| arithmetic -----------------
G_cell = rep(1, na_ * ny_)                          # uniform gain weights
R_base = sum(G_cell * as.vector(solve_pair(1, TAU_B)$p1$r_D))  # eta-invariant

agg_R    = function(eta, tau) sum(G_cell * as.vector(solve_pair(eta, tau)$p2$r_D))
semi_1pp = function(eta) (log(agg_R(eta, TAU_B + 0.01)) - log(R_base)) / 0.01

# Response is DECREASING in eta (a more negative signed semi-elasticity).
TARGET = -2.52
lo = 0.1; hi = 64
stopifnot(semi_1pp(lo) > TARGET, semi_1pp(hi) < TARGET)
for (i in 1:60) {
  mid = (lo + hi) / 2
  if (semi_1pp(mid) < TARGET) hi = mid else lo = mid
}
eta_star = (lo + hi) / 2
cat(sprintf('(b) test-local eta* = %.4f  (aggregate +1pp semi = %+.4f)\n',
            eta_star, semi_1pp(eta_star)))

tau_grid = seq(0.16, 0.60, 0.02)
rev      = sapply(tau_grid, function(t) t * agg_R(eta_star, t))
argmax   = tau_grid[which.max(rev)]
cat('  revenue Laffer curve (tau : revenue / R_base):\n')
print(data.frame(tau = tau_grid, rev = round(rev / R_base, 4)))
cat(sprintf('  revmax argmax = %.3f   (naive 1/2.52 = %.3f)\n', argmax, 1 / 2.52))
stopifnot(abs(argmax - 1 / 2.52) < 0.05)

cat('naive-limit tests passed\n')

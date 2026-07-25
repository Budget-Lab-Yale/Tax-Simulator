#-------------------------------------------------------------------------------
# test_logs_form.R
#
# Unit tests for the NET-OF-TAX ('logs') realization response form
# (plan serialized-sauteeing-cocoa, part A2/A5). The logs form swaps the
# entropy/KL cost for a convex power cost whose FOC generates a constant
# net-of-tax elasticity r_D = r_D_B * ((1 - MC)/(1 - MC_B))^eta_tilde, while
# keeping C'(r_D_B) = 0 so Pass-1 inversion, appliers, tau_eq and the h/e legs
# all carry over. Dependency-light (no full-sample data); sbatch-only:
#   sbatch other/kg_model_tests/kg_unit_tests.sbatch
#
# Checks:
#   (i)   closed form == numeric FOC inversion (uniroot on the payoff
#         derivative), all interior cells, tol 1e-8.
#   (ii)  Pass-1 exactness under logs: r_D = clip(r_B), kappa = MC exactly
#         (interior + r_B = 0 corner + r_B > 1 clip).
#   (iii) exact logs identity log(r_S/r_B) = eta_tilde*[log(1-MC_S) -
#         log(1-MC_B)] across +1/+5/+10pp, with the SAME implied net-of-tax
#         slope eta_tilde everywhere (analog of test_naive_limit's (a)).
#   (iv)  naive revmax nests 1/(1 + eta_tilde) (with beta = 0 so MC = tau).
#   (v)   corners/guards: r_B = 0 stays 0; a big tax cut clips r_D at 1;
#         a cell with MC >= KG_DYN_LOGS_MC_CAP hard-stops under logs but NOT
#         under levels.
#   (vi)  first-order equivalence: with eta_levels = eta_tilde/(1 - MC_B) the
#         two forms agree to O(Delta^2) as the shock Delta -> 0.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse) })
for (f in sort(list.files('./src/sim/kg', full.names = TRUE))) source(f)

fail = function(...) stop(sprintf(...), call. = FALSE)
ok   = function(name) cat(sprintf('PASS  %s\n', name))

# ---- synthetic grid (dependency-light, like test_naive_limit.R) ------------
ages  = 60:80
years = 2026:2030
ac    = as.character(ages); yc = as.character(years)
na_   = length(ages); ny_ = length(years)

m_age = 0.01 + 0.003 * (ages - min(ages))
grid  = list(
  m      = matrix(rep(m_age, ny_), na_, ny_, dimnames = list(ac, yc)),
  r_B    = matrix(0.05, na_, ny_, dimnames = list(ac, yc)),
  p_char = matrix(0,    na_, ny_, dimnames = list(ac, yc))
)
beta  = rep(0.96, ny_)

TAU_B  = 0.238
tau_B  = matrix(TAU_B, na_, ny_, dimnames = list(ac, yc))
ETA_T  = 1.9   # a representative logs eta_tilde (~ the expected pin)

solve_pair = function(eta, tau_S_scalar, form = 'logs', beta_by = beta,
                      g = grid, tB = tau_B) {
  tau_S = matrix(tau_S_scalar, nrow(tB), ncol(tB), dimnames = dimnames(tB))
  p1 = kg_dyn_solve_bellman(g, tB, c_phi_mat = 0, eta = eta,
                            beta_by_year = beta_by, form = form)
  p2 = kg_dyn_solve_bellman(g, tau_S, c_phi_mat = 0, kappa_mat = p1$kappa,
                            eta = eta, beta_by_year = beta_by, form = form)
  list(p1 = p1, p2 = p2)
}

#===============================================================================
# (i) closed form == numeric FOC inversion (uniroot on the payoff derivative)
#===============================================================================
# A tax hike (MC_S > MC_B) => r_S < r_B < cap, so no cell clips. For each
# interior cell the solver's r_D must solve C'(r) = kappa - MC = MC_B - MC,
# i.e. the payoff derivative dW/dr = kappa - C'(r) - MC = 0. C' for the power
# cost is (1 - MC_B)*[(r/r_B)^(1/eta) - 1].
res_i   = solve_pair(ETA_T, TAU_B + 0.05)
MC_B    = res_i$p1$kappa
MC_S    = res_i$p2$MC
r_B_cl  = pmin(pmax(grid$r_B, 0), 1)
r_D_S   = res_i$p2$r_D

max_err = 0
for (idx in which(r_B_cl > 0)) {
  rb   = r_B_cl[idx]; mcb = MC_B[idx]; mcs = MC_S[idx]
  dWdr = function(r) mcb - (1 - mcb) * ((r / rb)^(1 / ETA_T) - 1) - mcs
  # A tax hike pushes r below rb; bracket (tiny, rb].
  root = uniroot(dWdr, c(1e-12, rb), tol = 1e-14)$root
  max_err = max(max_err, abs(root - r_D_S[idx]))
}
if (max_err > 1e-8) fail('(i) closed form vs numeric FOC max err %.3e', max_err)
ok(sprintf('(i) closed form == numeric FOC inversion (max err %.2e)', max_err))

#===============================================================================
# (ii) Pass-1 exactness under logs (r_D = clip(r_B); kappa = MC)
#===============================================================================
# Heterogeneous r_B including a zero corner and an r_B > 1 cell (clips to 1).
set.seed(20260719)
grid_ii = grid
grid_ii$r_B = matrix(runif(na_ * ny_, 0.0, 0.30), na_, ny_,
                     dimnames = list(ac, yc))
grid_ii$r_B[1, 1] = 0        # zero corner: stays 0
grid_ii$r_B[2, 1] = 1.4      # > 1: clips to r_D_B = 1
p1_ii = kg_dyn_solve_bellman(grid_ii, tau_B, c_phi_mat = 0, eta = ETA_T,
                             beta_by_year = beta, form = 'logs')
if (!identical(p1_ii$r_D, pmin(pmax(grid_ii$r_B, 0), 1)))
  fail('(ii) Pass-1 r_D != clip(r_B) under logs')
if (max(abs(p1_ii$kappa - p1_ii$MC)) > 1e-14)
  fail('(ii) Pass-1 kappa != MC under logs (max err %.3e)',
       max(abs(p1_ii$kappa - p1_ii$MC)))
ok('(ii) Pass-1 exact under logs: r_D = clip(r_B), kappa = MC (incl. corners)')

#===============================================================================
# (iii) exact logs identity + constant net-of-tax slope across +1/+5/+10pp
#===============================================================================
shocks = c(0.01, 0.05, 0.10)
for (s in shocks) {
  res = solve_pair(ETA_T, TAU_B + s)
  # Tax increase => MC_S > MC_B => r_D_S < r_D_B: no cell clips.
  dlogNT = log(1 - res$p2$MC) - log(1 - res$p1$kappa)   # log net-of-cost change
  lhs    = log(res$p2$r_D / res$p1$r_D)
  if (max(abs(lhs - ETA_T * dlogNT)) > 1e-10)
    fail('(iii) net-of-tax identity broke at +%gpp (max err %.3e)',
         100 * s, max(abs(lhs - ETA_T * dlogNT)))
  # Implied slope must equal eta_tilde everywhere and identically across shocks.
  if (max(abs(lhs / dlogNT - ETA_T)) > 1e-8)
    fail('(iii) implied net-of-tax slope != eta_tilde at +%gpp', 100 * s)
}
ok('(iii) log(r_S/r_B) = eta_tilde*[log(1-MC_S)-log(1-MC_B)] to 1e-10, +1/5/10pp')

#===============================================================================
# (iv) naive revmax nests 1/(1 + eta_tilde)  (beta = 0 => MC = tau exactly)
#===============================================================================
beta0   = rep(0, ny_)
G_cell  = rep(1, na_ * ny_)                       # uniform gain weights
agg_R   = function(tau) sum(G_cell *
                            as.vector(solve_pair(ETA_T, tau, beta_by = beta0)$p2$r_D))
tau_grid = seq(0.10, 0.70, 0.005)
rev      = sapply(tau_grid, function(t) t * agg_R(t))
argmax   = tau_grid[which.max(rev)]
naive    = 1 / (1 + ETA_T)
cat(sprintf('  net-of-tax revmax argmax = %.3f   (naive 1/(1+%.2f) = %.3f)\n',
            argmax, ETA_T, naive))
if (abs(argmax - naive) > 0.01)
  fail('(iv) revmax argmax %.4f != 1/(1+eta_tilde) %.4f', argmax, naive)
ok('(iv) naive revmax nests 1/(1 + eta_tilde)')

#===============================================================================
# (v) corners / guards
#===============================================================================
# r_B = 0 cell stays 0 in Pass 2 (tax cut, would otherwise rise).
grid_v = grid; grid_v$r_B = matrix(0.0, na_, ny_, dimnames = list(ac, yc))
grid_v$r_B[3, 3] = 0     # explicit zero
p1_v = kg_dyn_solve_bellman(grid_v, tau_B, c_phi_mat = 0, eta = ETA_T,
                            beta_by_year = beta, form = 'logs')
p2_v = kg_dyn_solve_bellman(grid_v, matrix(TAU_B - 0.10, na_, ny_,
                                           dimnames = list(ac, yc)),
                            c_phi_mat = 0, kappa_mat = p1_v$kappa, eta = ETA_T,
                            beta_by_year = beta, form = 'logs')
if (p2_v$r_D[3, 3] != 0) fail('(v) r_B = 0 cell did not stay 0 under logs')
ok('(v) r_B = 0 cell stays 0 under logs (tax cut)')

# Upper clip: high r_B + big tax cut => multiplier pushes r_D above cap = 1.
grid_clip = grid; grid_clip$r_B = matrix(0.9, na_, ny_, dimnames = list(ac, yc))
p1_c = kg_dyn_solve_bellman(grid_clip, tau_B, c_phi_mat = 0, eta = ETA_T,
                            beta_by_year = beta, form = 'logs')
p2_c = kg_dyn_solve_bellman(grid_clip, matrix(0.02, na_, ny_,
                                              dimnames = list(ac, yc)),
                            c_phi_mat = 0, kappa_mat = p1_c$kappa, eta = ETA_T,
                            beta_by_year = beta, form = 'logs')
if (!all(p2_c$r_D <= 1 + 1e-15) || !any(abs(p2_c$r_D - 1) < 1e-12))
  fail('(v) big tax cut did not clip any r_D at the cap 1')
ok('(v) big tax cut clips r_D at the realization cap 1')

# MC >= KG_DYN_LOGS_MC_CAP hard-stops under logs; levels is unconstrained.
grid_hot = list(m = matrix(0.05, 1, 1, dimnames = list('80', '2026')),
                r_B = matrix(0.10, 1, 1, dimnames = list('80', '2026')),
                p_char = matrix(0, 1, 1, dimnames = list('80', '2026')))
tau_hot = matrix(0.99, 1, 1, dimnames = list('80', '2026'))   # MC >= 0.99 > cap
logs_stop = tryCatch({
  kg_dyn_solve_bellman(grid_hot, tau_hot, c_phi_mat = 0, eta = ETA_T,
                       beta_by_year = 1, form = 'logs'); FALSE
}, error = function(e) grepl('logs form', conditionMessage(e)))
if (!isTRUE(logs_stop)) fail('(v) MC >= cap did not hard-stop under logs')
levels_ok = tryCatch({
  kg_dyn_solve_bellman(grid_hot, tau_hot, c_phi_mat = 0, eta = ETA_T,
                       beta_by_year = 1, form = 'levels'); TRUE
}, error = function(e) FALSE)
if (!isTRUE(levels_ok)) fail('(v) levels form wrongly stopped on a high-MC cell')
ok('(v) MC >= cap hard-stops under logs, runs under levels')

#===============================================================================
# (vi) first-order equivalence: eta_levels = eta_tilde/(1 - MC_B) => O(Delta^2)
#===============================================================================
# 1x1 top-age cell so MC_B is a single scalar (a scalar eta can match the
# cell-wise relation). Baseline solve recovers MC_B; then the levels eta that
# shares the local net-of-tax slope is eta_tilde/(1 - MC_B).
g1   = list(m = matrix(0.04, 1, 1, dimnames = list('80', '2026')),
            r_B = matrix(0.06, 1, 1, dimnames = list('80', '2026')),
            p_char = matrix(0, 1, 1, dimnames = list('80', '2026')))
tB1  = matrix(0.30, 1, 1, dimnames = list('80', '2026'))
b1   = 1
p1_1 = kg_dyn_solve_bellman(g1, tB1, c_phi_mat = 0, eta = ETA_T,
                            beta_by_year = b1, form = 'logs')
MC_B1   = p1_1$kappa[1, 1]
eta_lev = ETA_T / (1 - MC_B1)

r_at = function(delta, form, eta) {
  tS = matrix(0.30 + delta, 1, 1, dimnames = list('80', '2026'))
  kg_dyn_solve_bellman(g1, tS, c_phi_mat = 0, kappa_mat = p1_1$kappa,
                       eta = eta, beta_by_year = b1, form = form)$r_D[1, 1]
}
deltas = c(1e-2, 1e-3, 1e-4)
diff_over_d  = numeric(length(deltas))
diff_over_d2 = numeric(length(deltas))
for (k in seq_along(deltas)) {
  d  = deltas[k]
  rl = r_at(d, 'levels', eta_lev)
  rn = r_at(d, 'logs',   ETA_T)
  df = abs(rl - rn)
  diff_over_d [k] = df / d
  diff_over_d2[k] = df / d^2
}
# First order: diff/Delta must fall ~10x per 10x-smaller Delta (O(Delta^2)).
r1 = diff_over_d[2] / diff_over_d[1]
r2 = diff_over_d[3] / diff_over_d[2]
if (!(r1 < 0.2 && r2 < 0.2))
  fail('(vi) diff/Delta not falling ~10x (ratios %.3f, %.3f) -- not O(Delta^2)',
       r1, r2)
# Second order: diff/Delta^2 must be ~constant (the O(Delta^2) coefficient).
c_ratio = diff_over_d2[3] / diff_over_d2[1]
if (!(c_ratio > 0.9 && c_ratio < 1.1))
  fail('(vi) diff/Delta^2 not constant (ratio %.4f) -- wrong local order', c_ratio)
ok(sprintf('(vi) forms agree to O(Delta^2) when eta_levels = eta_tilde/(1-MC_B) [c_ratio %.3f]',
           c_ratio))

cat('\nALL LOGS-FORM TESTS PASSED\n')

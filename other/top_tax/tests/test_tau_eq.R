#-------------------------------------------------------------------------------
# test_tau_eq.R
#
# Unit tests for the tau_eq machinery (DESIGN_LOCK ruling 1): the linear
# backward recursion (kg_dyn_compute_tau_eq) must match the finite-difference
# ground truth (kg_dyn_tau_eq_finite_diff — a forward simulation of the exact
# kg_dyn_step_recurrence marginal dynamics) cell by cell, on synthetic grids
# that exercise every regime. Plus the sanity properties from the build plan:
# bounds, regime ordering, and regime-conditional age monotonicity.
#
# Run via sbatch other/top_tax/tests/test_tau_eq.sbatch (from repo root).
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(data.table)
  library(tibble)
})

`%||%` = function(a, b) if (is.null(a)) b else a

# Source just the machinery under test (kg_dynamics.R needs cohort_bathtub's
# build_aging_matrix; its corp/estate references live inside functions this
# test never calls).
source('./src/sim/cohort_bathtub.R')
source('./src/sim/kg_dynamics.R')

n_fail = 0
check = function(ok, label) {
  status = if (all(ok)) 'PASS' else 'FAIL'
  if (!all(ok)) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}

#-------------------------------------------------------------------------------
# Synthetic grid builders
#-------------------------------------------------------------------------------

make_cells = function(ages, years, m_fun, r_B = 0.06, p_char = 0.02) {
  # Minimal baseline_cells: only the columns kg_dyn_cell_m_eff and the
  # tau_eq primitives consume (G_B, R_B, m, mG_record, mR_record, p_char).
  out = list()
  for (t in years) {
    m = m_fun(ages)
    out[[as.character(t)]] = tibble(
      age       = ages,
      G_B       = 1000,
      R_B       = 1000 * r_B,
      r_B       = r_B,
      m         = m,
      mG_record = m * 1000,   # m_eff == m on the synthetic grid
      mR_record = m * 1000 * r_B,
      p_char    = p_char
    )
  }
  out
}

make_mix = function(ages, years, route, realize) {
  lapply(seq_along(years), function(j) {
    tibble(age = ages,
           delta_vanish  = 1 - route - realize,
           delta_route   = route,
           delta_realize = realize,
           c_phi         = NA_real_)
  })
}

ages  = 18:80
years = 2026:2029
m_fun = function(a) pmin(0.0005 * exp((a - 18) * 0.09), 0.5)  # rising mortality
A     = build_aging_matrix(ages)
omega = {
  # Simple synthetic heir distribution concentrated at ages 45-65
  sh = ifelse(ages %in% 45:65, 1, 0); sh = sh / sum(sh)
  kg_dyn_build_heir_matrix(setNames(sh, as.character(ages)), ages)
}
beta  = rep(0.97, length(years))
tau   = matrix(0.238, length(ages), length(years),
               dimnames = list(as.character(ages), as.character(years)))
r_S_by_year = lapply(seq_along(years), function(j) rep(0.06, length(ages)))
cells = make_cells(ages, years, m_fun)

prims_for = function(route, realize, tau_mat = tau) {
  kg_dyn_tau_eq_primitives(
    baseline_cells = cells,
    years          = years,
    r_S_by_year    = r_S_by_year,
    tau_bt_mat     = tau_mat,
    mix_list       = make_mix(ages, years, route, realize),
    A              = A,
    omega          = omega,
    ages_bathtub   = ages
  )
}

#-------------------------------------------------------------------------------
# 1. Recursion == finite difference, cell by cell, every regime + a mixed one
#-------------------------------------------------------------------------------

for (spec in list(list(name = 'step-up',   route = 0,   realize = 0),
                  list(name = 'carryover', route = 1,   realize = 0),
                  list(name = 'deemed',    route = 0,   realize = 1),
                  list(name = 'mixed',     route = 0.3, realize = 0.4))) {
  prims = prims_for(spec$route, spec$realize)
  rec   = kg_dyn_compute_tau_eq(prims, beta)
  err = max(sapply(seq_along(years), function(j) {
    fd = kg_dyn_tau_eq_finite_diff(prims, beta, j, horizon = 3000, tol = 1e-16)
    max(abs(fd - rec$tau_eq[, j]))
  }))
  check(err < 1e-10,
        sprintf('recursion == finite difference, %s regime (max err %.2e)',
                spec$name, err))
}

# Year-varying primitives: tau ramps up, r_S varies by year, mortality as-is.
tau_ramp = tau; for (j in seq_along(years)) tau_ramp[, j] = 0.20 + 0.02 * j
r_S_vary = lapply(seq_along(years), function(j) rep(0.04 + 0.01 * j, length(ages)))
prims_v = kg_dyn_tau_eq_primitives(cells, years, r_S_vary, tau_ramp,
                                   make_mix(ages, years, 0.5, 0.25),
                                   A, omega, ages)
beta_v  = c(0.96, 0.97, 0.98, 0.965)
rec_v   = kg_dyn_compute_tau_eq(prims_v, beta_v)
err_v = max(sapply(seq_along(years), function(j) {
  fd = kg_dyn_tau_eq_finite_diff(prims_v, beta_v, j, horizon = 3000, tol = 1e-16)
  max(abs(fd - rec_v$tau_eq[, j]))
}))
check(err_v < 1e-10,
      sprintf('recursion == finite difference, year-varying primitives (max err %.2e)',
              err_v))

#-------------------------------------------------------------------------------
# 2. Bounds: 0 <= tau_eq <= max tau (with the event-overlap slack)
#-------------------------------------------------------------------------------

for (spec in list(c(0, 0), c(1, 0), c(0, 1), c(0.3, 0.4))) {
  prims = prims_for(spec[1], spec[2])
  te = kg_dyn_compute_tau_eq(prims, beta)$tau_eq
  check(all(te >= 0) && all(te <= 0.238 * 1.05),
        sprintf('bounds 0 <= tau_eq <= 1.05 * tau (route=%.1f realize=%.1f, range [%.4f, %.4f])',
                spec[1], spec[2], min(te), max(te)))
  check(kg_dyn_check_tau_eq(te, prims$tau, 'test'),
        'kg_dyn_check_tau_eq passes on in-bounds matrix')
}

#-------------------------------------------------------------------------------
# 3. Regime ordering at equal rates: step-up < carryover < deemed
#-------------------------------------------------------------------------------

te_stepup    = kg_dyn_compute_tau_eq(prims_for(0, 0), beta)$tau_eq
te_carryover = kg_dyn_compute_tau_eq(prims_for(1, 0), beta)$tau_eq
te_deemed    = kg_dyn_compute_tau_eq(prims_for(0, 1), beta)$tau_eq
check(all(te_stepup < te_carryover) && all(te_carryover < te_deemed),
      'regime ordering: tau_eq(step-up) < tau_eq(carryover) < tau_eq(deemed)')

#-------------------------------------------------------------------------------
# 4. Regime-conditional age monotonicity (first year column; interior ages,
#    away from the age-80 topcode self-loop)
#-------------------------------------------------------------------------------

interior = 1:(length(ages) - 1)
d_stepup = diff(te_stepup[, 1])
d_deemed = diff(te_deemed[, 1])
# Step-up: death forgives -> tau_eq FALLS with age as mortality rises.
check(all(d_stepup[interior[-length(interior)]] < 0),
      'step-up: tau_eq falls with age (death forgiveness dominates)')
# Deemed: death taxes in full and arrives sooner -> tau_eq RISES with age.
check(all(d_deemed[interior[-length(interior)]] > 0),
      'deemed: tau_eq rises with age (less deferral runway)')

#-------------------------------------------------------------------------------
# 5. Analytic cross-check: constant primitives, no death channel taxes, no
#    routing -> geometric closed form tau_eq = beta*r*tau / (1 - beta*(1-m)(1-r))
#    on a constant-mortality grid (age structure then drops out).
#-------------------------------------------------------------------------------

m_const  = function(a) rep(0.01, length(a))
cells_c  = make_cells(ages, years, m_const)
prims_c  = kg_dyn_tau_eq_primitives(cells_c, years, r_S_by_year, tau,
                                    NULL, A, omega, ages)
te_c     = kg_dyn_compute_tau_eq(prims_c, beta)$tau_eq
closed   = 0.97 * 0.06 * 0.238 / (1 - 0.97 * (1 - 0.01) * (1 - 0.06))
check(max(abs(te_c - closed)) < 1e-12,
      sprintf('closed-form check, constant grid (recursion %.6f vs analytic %.6f)',
              te_c[1, 1], closed))

#-------------------------------------------------------------------------------
# 6. Recurrence consistency: injecting a dollar through kg_dyn_step_recurrence
#    reproduces the FD harness's stock path (the "exact dynamics" claim).
#-------------------------------------------------------------------------------

prims = prims_for(0.3, 0.4)
inject_age = '50'
delta = setNames(rep(0, length(ages)), as.character(ages))
conv  = delta; conv[inject_age] = 1

# Year 1: inject via the recurrence's conv_inflow argument
step1 = kg_dyn_step_recurrence(
  delta_prev = delta, baseline_t = cells[['2026']], A = A, omega = omega,
  r_S_vec = r_S_by_year[[1]], delta_route_vec = rep(0.3, length(ages)),
  conv_inflow_vec = conv)
# Year 2: propagate with zero G_B forcing... G_B terms affect levels, so
# difference two runs (with and without the injection) to isolate the
# marginal path.
step1_base = kg_dyn_step_recurrence(
  delta_prev = delta, baseline_t = cells[['2026']], A = A, omega = omega,
  r_S_vec = r_S_by_year[[1]], delta_route_vec = rep(0.3, length(ages)))
d1 = step1$delta_next - step1_base$delta_next

step2      = kg_dyn_step_recurrence(
  delta_prev = d1, baseline_t = cells[['2027']], A = A, omega = omega,
  r_S_vec = r_S_by_year[[2]], delta_route_vec = rep(0.3, length(ages)))
step2_base = kg_dyn_step_recurrence(
  delta_prev = delta, baseline_t = cells[['2027']], A = A, omega = omega,
  r_S_vec = r_S_by_year[[2]], delta_route_vec = rep(0.3, length(ages)))
d2 = step2$delta_next - step2_base$delta_next

# FD harness path for the same injection
surv_w  = (1 - prims$m_eff[, 2]) * (1 - prims$r_S[, 2])
route_w = prims$m_eff[, 2] * (1 - prims$p_char[, 2]) * prims$route[, 2]
d1_fd   = conv
d2_fd   = as.numeric(t(A) %*% (surv_w * d1_fd) + t(omega) %*% (route_w * d1_fd))

check(max(abs(d1 - conv)) < 1e-12,
      'conv_inflow enters delta_next verbatim (end-of-year injection)')
check(max(abs(d2 - d2_fd)) < 1e-12,
      sprintf('recurrence marginal delta path == FD harness dynamics (max diff %.2e)',
              max(abs(d2 - d2_fd))))

#-------------------------------------------------------------------------------

cat(sprintf('\n%s\n', if (n_fail == 0) 'ALL TESTS PASSED' else
            paste0(n_fail, ' TEST(S) FAILED')))
if (n_fail > 0) quit(status = 1)

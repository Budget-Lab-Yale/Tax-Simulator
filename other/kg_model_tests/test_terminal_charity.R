# kg_dyn_aggregate_cells reaches into corp_incidence.R (corp_kg_state_exposed_value,
# CORP_ASSET_EXPOSURE) and wealth_dynamics.R (wealth_dyn_safe_col), so load the
# full requirements and source the whole src tree (the reconstitute_environment /
# check_core.R convention) rather than a hand-picked file list.
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE))))
library(purrr)
return_vars <<- list()   # some src post-processing files reference this at source time
list.files('./src', recursive = TRUE) %>%
  walk(~ if (.x != 'main.R' && !startsWith(.x, 'slurm/') && !startsWith(.x, 'tests/'))
         source(file.path('./src', .x)))

tol = 8e-4
estate_m = c(10, 50, 100, 200)
q = plogis(KG_DYN_CHAR_EXTENSIVE_INTERCEPT +
             KG_DYN_CHAR_EXTENSIVE_LN_SLOPE * log(estate_m))
c_cond = plogis(KG_DYN_CHAR_INTENSIVE_INTERCEPT +
                  KG_DYN_CHAR_INTENSIVE_LN_SLOPE * log(estate_m))
p = q * c_cond

stopifnot(max(abs(q - c(0.204108612197, 0.348932343681,
                        0.424019437563, 0.502786542168))) < tol)
stopifnot(max(abs(c_cond - c(0.311171916499, 0.489603203436,
                             0.570215169612, 0.647267885253))) < tol)
stopifnot(max(abs(p - c(0.0635128680313, 0.170838393249,
                        0.241782315509, 0.325437581883))) < tol)

records = tibble(
  id = 1:2,
  weight = c(1, 2),
  filing_status = c(1, 1),
  age1 = c(70, 70),
  age2 = c(NA_real_, NA_real_),
  kg_lt = c(10, 20),
  q_death1 = c(0.1, 0.2),
  q_death2 = c(NA_real_, NA_real_),
  `pref.kg_sec121_excl` = c(250000, 250000),
  year = c(2026, 2026)
)

for (nm in ESTATE_ASSET_COLS) records[[nm]] = 0
records[['value.equities']] = c(1e7, 5e7)
records[['value.pass_throughs']] = c(0, 0)
records[['value.primary_home']] = c(0, 0)
records[['value.other_home']] = c(0, 0)
records[['value.re_fund']] = c(0, 0)
for (nm in KG_DYN_ASSET_BASIS_COLS) records[[nm]] = 0
records[['basis.equities']] = c(6e6, 3e7)

records = kg_dyn_attach_record_attrs(records, c('2026' = 1))
stopifnot(all.equal(records$G_unit, c(4e6, 2e7)))
stopifnot(all(records$p_char > 0))

cell = kg_dyn_aggregate_cells(records, ages = 70)
expected_p_char =
  sum(records$weight * records$m_household * records$G_unit * records$p_char) /
  sum(records$weight * records$m_household * records$G_unit)
stopifnot(abs(cell$p_char - expected_p_char) < 1e-12)

grid = list(
  m = matrix(0.1, 1, 1, dimnames = list('70', '2026')),
  r_B = matrix(0.05, 1, 1, dimnames = list('70', '2026')),
  p_char = matrix(0.25, 1, 1, dimnames = list('70', '2026'))
)
tau = matrix(0.2, 1, 1, dimnames = list('70', '2026'))
with_char = kg_dyn_solve_bellman(grid, tau, c_phi_mat = 1, eta = 5,
                                 phi_I = 0, planned_share = 0,
                                 beta_by_year = 1)
without_char = kg_dyn_solve_bellman(
  list(m = grid$m, r_B = grid$r_B,
       p_char = matrix(0, 1, 1, dimnames = dimnames(grid$m))),
  tau, c_phi_mat = 1, eta = 5, phi_I = 0, planned_share = 0,
  beta_by_year = 1
)
stopifnot(with_char$MC[1, 1] > without_char$MC[1, 1])

step = kg_dyn_step_recurrence(
  delta_prev = setNames(0, '70'),
  baseline_t = cell,
  A = matrix(1, 1, 1),
  omega = matrix(1, 1, 1),
  r_S_vec = setNames(cell$r_B, '70'),
  delta_route_vec = 1,
  phi_I = 0
)
stopifnot(abs(step$terminal_char_stock -
              cell$p_char * step$decedent_stock) < 1e-8)
stopifnot(abs(step$taxable_death_stock -
              (1 - cell$p_char) * step$decedent_stock) < 1e-8)

cat('terminal charity tests passed\n')

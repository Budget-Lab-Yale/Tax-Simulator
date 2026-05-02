#-------------------------------------------------------------------------------
# Minimal standalone implementation of the KG dynamics in
# capital_gains_realization.md. Single asset class (sum of all five wealth
# classes), flat baseline tax rate, deterministic heir matrix.
#
# Realization rate is microfounded via the effective tax price P (spec §4):
#
#   P(c)  =  tau_t  -  sum_j  beta^j s_j tau_{t+j}
#                   -  c * sum_j beta^j d_j tau_{t+j}
#
#   r_S   =  r_B * exp( -eta * (P_S - P_B) )
#
# Where (s_j, d_j) come from a competing-risks recursion in (lambda_r, m_path)
# and c in [0,1] is the post-death tax burden share. Step-up: c=0; deemed: c=1;
# carryover: c=theta in (0,1] depending on bequest motive.
#
# Pure functions only -- this file defines constants and exports model
# functions; sourcing it does not run the model. See run_scenarios.R /
# sensitivity.R / calibrate_eta.R for drivers.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

#-------------------------------------------------------------------------------
# Constants -- tweak at top of driver script if desired
#-------------------------------------------------------------------------------

TAX_DATA_DIR  <- "/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026043020/baseline"
YEARS         <- 2026:2050
AGE_MIN       <- 18
AGE_MAX       <- 80
BASELINE_TAU  <- 0.20

# Microfoundation parameters -- see spec §6
ETA           <- 5.527   # calibrated to permanent eta = -0.6 at 5pp hike under step-up
BETA          <- 0.96    # discount factor (~4% rate)
LAMBDA_R      <- 0.05    # voluntary realization hazard; calibrate from data
THETA         <- 0.5     # bequest motive under carryover (default mid-range)
HORIZON       <- 60      # forward-integration horizon for bracket calc

HEIR_SHIFT    <- 30
HEIR_SIGMA    <- 5

ASSET_CLASSES <- c("equities", "pass_throughs", "primary_home", "other_home", "re_fund")

#-------------------------------------------------------------------------------
# Per-record loading and cohort assignment
#-------------------------------------------------------------------------------

load_year = function(year, tax_data_dir = TAX_DATA_DIR) {
  #-----------------------------------------------------------------------------
  # Read one year of tax-data, compute per-record unrealized gain stock and
  # household-level death probability, assign cohort age.
  #
  # Parameters:
  #   - year (int)         : calendar year to load
  #   - tax_data_dir (str) : path to Tax-Data baseline directory
  #
  # Returns: data.table with id, weight, age, G, kg_lt, m
  #-----------------------------------------------------------------------------

  path = file.path(tax_data_dir, paste0("tax_units_", year, ".csv"))

  value_cols = paste0("value.", ASSET_CLASSES)
  basis_cols = paste0("basis.", ASSET_CLASSES)
  keep_cols  = c("id", "weight", "filing_status", "age1", "age2",
                 "kg_lt", "q_death1", "q_death2",
                 value_cols, basis_cols)

  dt = fread(path, select = keep_cols, showProgress = FALSE)

  # per-record unrealized gain stock: sum max(0, value - basis) across classes
  G_mat = as.matrix(dt[, ..value_cols]) - as.matrix(dt[, ..basis_cols])
  G_mat[G_mat < 0] = 0
  dt[, G := rowSums(G_mat)]

  # household death probability: q1 * q2 for joint, else q1
  dt[, m := fifelse(is.na(q_death2) | filing_status != 2,
                    q_death1,
                    q_death1 * q_death2)]
  dt[is.na(m), m := 0]

  # cohort age: max over spouses for joint, else age1
  dt[, age := pmax(age1, age2, na.rm = TRUE)]
  dt[age < AGE_MIN, age := AGE_MIN]
  dt[age > AGE_MAX, age := AGE_MAX]

  dt[, .(id, weight, age, G, kg_lt, m)]
}

#-------------------------------------------------------------------------------
# Cell aggregation
#-------------------------------------------------------------------------------

cell_aggregate = function(records, ages = AGE_MIN:AGE_MAX) {
  #-----------------------------------------------------------------------------
  # Weight-aggregate per-record stocks and flows to age-cell totals. Returns a
  # complete grid over [AGE_MIN, AGE_MAX] with empty cells filled (G_B = 0,
  # R_B = 0, m = 0).
  #
  # Returns: tibble with age, G_B, R_B, r_B, m, n
  #-----------------------------------------------------------------------------

  agg = records[, .(
    G_B   = sum(weight * G,     na.rm = TRUE),
    R_B   = sum(weight * kg_lt, na.rm = TRUE),
    m_num = sum(weight * m,     na.rm = TRUE),
    n     = sum(weight,         na.rm = TRUE)
  ), by = age]

  full = data.table(age = ages)
  out  = agg[full, on = "age"]
  out[is.na(G_B),   G_B   := 0]
  out[is.na(R_B),   R_B   := 0]
  out[is.na(m_num), m_num := 0]
  out[is.na(n),     n     := 0]
  out[, m   := fifelse(n > 0, m_num / n, 0)]
  out[, r_B := fifelse(G_B > 0, R_B / G_B, 0)]
  out[, m_num := NULL]

  setorder(out, age)
  as_tibble(out)
}

#-------------------------------------------------------------------------------
# Heir matrix
#-------------------------------------------------------------------------------

build_heir_matrix = function(ages = AGE_MIN:AGE_MAX,
                              shift = HEIR_SHIFT,
                              sigma = HEIR_SIGMA) {
  #-----------------------------------------------------------------------------
  # Construct a row-stochastic heir-allocation matrix omega[a, h] = share of
  # decedent-age-a gains routed to heir-age-h. Centered at a - shift with
  # Gaussian noise sigma, evaluated on the integer age grid and renormalized
  # row-by-row.
  #
  # Returns: matrix [|ages| x |ages|] with rows = decedent age, cols = heir age.
  #-----------------------------------------------------------------------------

  n = length(ages)
  W = outer(ages, ages, function(a, h) dnorm(h, mean = a - shift, sd = sigma))
  row_sums = rowSums(W)
  W = W / row_sums

  stopifnot(all(abs(rowSums(W) - 1) < 1e-12))

  rownames(W) = colnames(W) = ages
  W
}

build_aging_matrix = function(ages = AGE_MIN:AGE_MAX) {
  # A[a, h] = 1 if h = a + 1 (and a < a_max); A[a_max, a_max] = 1
  n = length(ages)
  A = matrix(0, n, n, dimnames = list(ages, ages))
  for (i in seq_len(n - 1)) A[i, i + 1] = 1
  A[n, n] = 1
  A
}

#-------------------------------------------------------------------------------
# Life table extraction
#-------------------------------------------------------------------------------

extract_life_table = function(baseline_cells, year = NULL) {
  #-----------------------------------------------------------------------------
  # Extract age -> mortality vector (named) from baseline cell aggregates.
  # Used as the input "life table" for the bracket calculation. By default
  # uses the first year in the list; pass `year` to use a specific year.
  #
  # Returns: named numeric vector m[as.character(age)] = m(age)
  #-----------------------------------------------------------------------------

  bc = if (is.null(year)) baseline_cells[[1]] else baseline_cells[[as.character(year)]]
  m_vec = bc$m
  names(m_vec) = as.character(bc$age)
  m_vec
}

#-------------------------------------------------------------------------------
# Effective tax price (spec §4.2-4.3): bracket and P
#-------------------------------------------------------------------------------

compute_bracket = function(a, c_phi, life_table,
                            lambda_r = LAMBDA_R,
                            beta = BETA,
                            horizon = HORIZON,
                            tau_ratio = NULL) {
  #-----------------------------------------------------------------------------
  # Compute the bracket M(c) for a single cell at age a (spec §4.2-4.3).
  #
  #   M(c)  =  sum_{j=1..H} beta^j s_j (tau_{t+j}/tau_t)
  #         +  c * sum_{j=1..H} beta^j d_j (tau_{t+j}/tau_t)
  #
  # where (s_j, d_j) follow the competing-risks recursion in §4.3.
  #
  # Parameters:
  #   - a          (int)  : starting age
  #   - c_phi      (num)  : regime burden share, in [0,1]
  #   - life_table (vec)  : named numeric m[as.character(age)]
  #   - lambda_r   (num)  : voluntary realization hazard
  #   - beta       (num)  : annual discount factor
  #   - horizon    (int)  : integration horizon
  #   - tau_ratio  (vec)  : length-H vector of tau_{t+j}/tau_t. NULL = constant 1.
  #
  # Returns: scalar M(c)
  #-----------------------------------------------------------------------------

  if (is.null(tau_ratio)) tau_ratio = rep(1, horizon)
  stopifnot(length(tau_ratio) == horizon)

  # Future ages, capped at AGE_MAX
  ages_future = pmin(a + 0:(horizon - 1), AGE_MAX)
  m_future    = life_table[as.character(ages_future)]
  m_future[is.na(m_future)] = 0

  # Hazard of "stop holding" each year (capped just under 1 for safety)
  hazard = pmin(lambda_r + m_future, 0.999)

  # Still-holding probability at the start of year j (j = 1..H)
  # S_1 = 1, S_j = prod_{i=1..j-1} (1 - hazard_i)
  S = c(1, cumprod(1 - hazard))[1:horizon]

  # Event probabilities at year j
  s_j = S * lambda_r          # voluntary realization in year j
  d_j = S * m_future          # death (without prior realization) in year j

  # Discount factors beta^j, j = 1..H
  betas = beta ^ (1:horizon)

  M = sum(betas * s_j * tau_ratio) + c_phi * sum(betas * d_j * tau_ratio)
  M
}

compute_brackets = function(ages, c_phi, life_table,
                             lambda_r = LAMBDA_R,
                             beta = BETA,
                             horizon = HORIZON,
                             tau_ratio = NULL) {
  #-----------------------------------------------------------------------------
  # Vectorize compute_bracket across ages. Returns a named numeric vector.
  #-----------------------------------------------------------------------------

  out = sapply(ages, function(a) {
    compute_bracket(a, c_phi, life_table, lambda_r, beta, horizon, tau_ratio)
  })
  names(out) = as.character(ages)
  out
}

effective_tax_price = function(tau, bracket) {
  # P = tau * (1 - M(c)). Vectorized.
  tau * (1 - bracket)
}

#-------------------------------------------------------------------------------
# Realization rate function (microfounded)
#-------------------------------------------------------------------------------

realization_rate = function(r_B, P_S, P_B, eta = ETA) {
  #-----------------------------------------------------------------------------
  # Spec §4.1: r_S = r_B * exp(-eta * (P_S - P_B))
  #
  # When P_S = P_B (no policy change), r_S = r_B exactly.
  # Vectorized over cells.
  #-----------------------------------------------------------------------------

  r_B * exp(-eta * (P_S - P_B))
}

#-------------------------------------------------------------------------------
# One-step recurrence
#-------------------------------------------------------------------------------

step_recurrence = function(delta_prev, baseline_t,
                           A, omega,
                           P_B, P_S,
                           tau_B, tau_S,
                           delta_route, delta_realize,
                           eta = ETA) {
  #-----------------------------------------------------------------------------
  # Spec §3.5 + 5.3 + 8.2 with microfounded r_S from §4.
  #
  # Parameters:
  #   - delta_prev      (num[a]): delta_G in source-age cells, START of year t
  #   - baseline_t      (tbl)   : cell aggregates for year t (from cell_aggregate)
  #   - A               (mat)   : aging matrix, source-row destination-col
  #   - omega           (mat)   : heir matrix, decedent-row heir-col
  #   - P_B, P_S        (num[a]): effective tax price under baseline / reform
  #   - tau_B, tau_S    (num)   : baseline / reform tax rates (for revenue calcs)
  #   - delta_route     (num)   : share of decedent stock routed to heirs
  #   - delta_realize   (num)   : share of decedent stock forcibly realized
  #   - eta             (num)   : behavioral curvature parameter
  #
  # Returns: list with delta_next, delta_R (per-cell vec), R_death (scalar)
  #-----------------------------------------------------------------------------

  G_B  = baseline_t$G_B
  R_B  = baseline_t$R_B
  r_B  = baseline_t$r_B
  m    = baseline_t$m

  r_S  = realization_rate(r_B, P_S, P_B, eta)

  # ---- Survivor flow (§3.2) ----
  bracket    = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m) * bracket             # source-indexed
  delta_surv = as.numeric(crossprod(A, contrib_a))   # destination-indexed

  # ---- Inheritance flow (§3.3.1) ----
  if (delta_route > 0) {
    decedent_stock = m * (G_B + delta_prev)        # source-indexed
    delta_inh = delta_route * as.numeric(crossprod(omega, decedent_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  delta_next = delta_surv + delta_inh

  # ---- Realization-channel ΔR (§5.3) -- this period's flow ----
  G_S = G_B + delta_prev
  denom = r_B * G_B
  ratio_term = ifelse(denom > 0, r_S * G_S / denom, 1)
  delta_R    = R_B * (ratio_term - 1)

  # ---- Deemed-realization revenue (§3.3.2) ----
  R_death = delta_realize * sum(m * (G_B + delta_prev) * tau_S)

  list(delta_next  = delta_next,
       delta_R     = delta_R,
       R_death     = R_death,
       delta_surv  = delta_surv,
       delta_inh   = delta_inh)
}

#-------------------------------------------------------------------------------
# Full simulation
#-------------------------------------------------------------------------------

simulate_scenario = function(scenario,
                              baseline_cells,
                              years = YEARS,
                              ages  = AGE_MIN:AGE_MAX,
                              eta = ETA,
                              beta = BETA,
                              lambda_r = LAMBDA_R,
                              horizon = HORIZON,
                              tau_ratio_S = NULL,
                              tau_ratio_B = NULL,
                              omega = NULL, A = NULL,
                              life_table = NULL) {
  #-----------------------------------------------------------------------------
  # Run the recurrence for one scenario. Pre-computes brackets and effective
  # tax prices once at startup using the supplied life table; r_S follows from
  # the microfoundation.
  #
  # Parameters:
  #   - scenario (list)  : with id, tau_S, c_phi, delta_vanish, delta_route,
  #                        delta_realize
  #   - baseline_cells   : named-by-year list of tibbles from cell_aggregate
  #   - years (int)      : simulation horizon
  #   - ages  (int)      : age grid
  #   - eta, beta, lambda_r, horizon : microfoundation parameters
  #   - tau_ratio_S, tau_ratio_B     : optional length-horizon vectors of
  #                                    tau_{t+j}/tau_t under reform / baseline.
  #                                    Default NULL = constant ratio 1
  #                                    (naive expectations).
  #   - omega, A         : matrices; built from defaults if NULL
  #   - life_table       : named m vector; built from baseline_cells[[1]] if NULL
  #
  # Returns: list with cells (long tibble) and totals (year-level)
  #-----------------------------------------------------------------------------

  if (is.null(omega))      omega      = build_heir_matrix(ages)
  if (is.null(A))          A          = build_aging_matrix(ages)
  if (is.null(life_table)) life_table = extract_life_table(baseline_cells)

  # Pre-compute brackets and effective tax prices (constant across years
  # under naive expectations; would need to be re-computed per-year if
  # tau_ratio varies with t).
  bracket_B = compute_brackets(ages, 0, life_table, lambda_r, beta, horizon, tau_ratio_B)
  bracket_S = compute_brackets(ages, scenario$c_phi, life_table, lambda_r, beta, horizon, tau_ratio_S)
  P_B       = effective_tax_price(BASELINE_TAU,   bracket_B)
  P_S       = effective_tax_price(scenario$tau_S, bracket_S)

  n_age = length(ages)
  delta = rep(0, n_age)

  rows = list()
  totals = list()

  for (i in seq_along(years)) {
    t  = years[i]
    bt = baseline_cells[[as.character(t)]]
    dG_now = delta

    out = step_recurrence(
      delta_prev    = dG_now,
      baseline_t    = bt,
      A             = A,
      omega         = omega,
      P_B           = P_B,
      P_S           = P_S,
      tau_B         = BASELINE_TAU,
      tau_S         = scenario$tau_S,
      delta_route   = scenario$delta_route,
      delta_realize = scenario$delta_realize,
      eta           = eta
    )

    rows[[i]] = tibble(
      scenario   = scenario$id,
      year       = t,
      age        = ages,
      G_B        = bt$G_B,
      R_B        = bt$R_B,
      m          = bt$m,
      P_B        = P_B,
      P_S        = P_S,
      dG         = dG_now,
      dR         = out$delta_R,
      delta_surv = out$delta_surv,
      delta_inh  = out$delta_inh
    )

    R_S      = bt$R_B + out$delta_R
    T_S_real = scenario$tau_S * sum(R_S, na.rm = TRUE)
    T_B_real = BASELINE_TAU   * sum(bt$R_B, na.rm = TRUE)

    totals[[i]] = tibble(
      scenario  = scenario$id,
      year      = t,
      dG_total  = sum(dG_now),
      dR_total  = sum(out$delta_R, na.rm = TRUE),
      dT_real   = T_S_real - T_B_real,
      R_death   = out$R_death,
      dT_total  = (T_S_real - T_B_real) + out$R_death
    )

    delta = out$delta_next
  }

  list(
    cells  = bind_rows(rows),
    totals = bind_rows(totals)
  )
}

#-------------------------------------------------------------------------------
# Convenience: load all years' baselines once
#-------------------------------------------------------------------------------

load_baseline_cells = function(years = YEARS, tax_data_dir = TAX_DATA_DIR,
                                ages = AGE_MIN:AGE_MAX, verbose = TRUE) {
  if (verbose) cat("Loading", length(years), "years of tax-data...\n")
  out = lapply(years, function(t) {
    if (verbose) cat("  ", t, "\n", sep = "")
    rec = load_year(t, tax_data_dir)
    cell_aggregate(rec, ages)
  })
  names(out) = as.character(years)
  out
}

#-------------------------------------------------------------------------------
# Scenario constructor
#-------------------------------------------------------------------------------

make_scenario = function(id, tau_S = BASELINE_TAU, c_phi = 0,
                          delta_vanish = 1, delta_route = 0, delta_realize = 0) {
  #-----------------------------------------------------------------------------
  # Build a scenario spec.
  #
  # c_phi (post-death tax burden share) is the holder's microfoundation hook.
  # The (delta_vanish, delta_route, delta_realize) routing triple governs
  # what mechanically happens to decedent stock at the death event, and
  # should be set consistently with c_phi:
  #
  #   step-up:    c_phi = 0,     delta_vanish  = 1
  #   carryover:  c_phi = theta, delta_route   = 1   (theta in (0, 1])
  #   deemed:     c_phi = 1,     delta_realize = 1
  #-----------------------------------------------------------------------------

  stopifnot(abs(delta_vanish + delta_route + delta_realize - 1) < 1e-12)
  stopifnot(c_phi >= 0 && c_phi <= 1)
  list(
    id            = id,
    tau_S         = tau_S,
    c_phi         = c_phi,
    delta_vanish  = delta_vanish,
    delta_route   = delta_route,
    delta_realize = delta_realize
  )
}

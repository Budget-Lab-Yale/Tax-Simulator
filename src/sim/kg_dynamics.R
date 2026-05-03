#-------------------------------------------------------------------------------
# kg_dynamics.R
#
# Core utilities for the capital-gains dynamics behavioral module. Implements
# the law of motion for the policy-induced delta in unrealized capital gains
# specified in other/kg_model_tests/capital_gains_realization.md, ported from
# the standalone in other/kg_model_tests/kg_minimal.R.
#
# All functions in this file are pure: they take cell-level structures and
# return cell-level structures. The orchestrating behavior module lives at
# config/scenarios/behavior/kg_dynamics/ and threads year-by-year state to
# these utilities. v1 uses a single combined asset bucket (sum across the five
# tracked wealth classes).
#-------------------------------------------------------------------------------



# Constants. Calibrated values match the standalone (calibrate_eta.R targets
# eta_30 = -0.6 at a 5pp hike from a 0.20 baseline under step-up).
KG_DYN_AGE_MIN     = 18
KG_DYN_AGE_MAX     = 80
KG_DYN_HORIZON     = 60        # bracket integration horizon, in years
KG_DYN_BETA        = 0.96      # annual discount factor (~4%)
KG_DYN_LAMBDA_R    = 0.05      # voluntary realization hazard
KG_DYN_HEIR_SHIFT  = 30        # average decedent-to-heir age gap
KG_DYN_HEIR_SIGMA  = 5         # std dev of heir age distribution

KG_DYN_ASSET_VALUE_COLS = c('value.equities', 'value.pass_throughs',
                            'value.primary_home', 'value.other_home',
                            'value.re_fund')
KG_DYN_ASSET_BASIS_COLS = c('basis.equities', 'basis.pass_throughs',
                            'basis.primary_home', 'basis.other_home',
                            'basis.re_fund')



kg_dyn_attach_record_attrs = function(tax_units) {

  #----------------------------------------------------------------------------
  # Adds three derived columns to tax_units used by the bathtub recurrence:
  #   G_unit       : per-record unrealized gain stock, sum_k max(0, value_k -
  #                  basis_k) across the five tracked wealth classes
  #   m_household  : household death probability. q_death1*q_death2 for joint
  #                  filers (both spouses die same year); q_death1 otherwise
  #   age_cohort   : cohort age. max(age1, age2) for joint, age1 otherwise.
  #                  Top-coded at KG_DYN_AGE_MAX, bottom-coded at AGE_MIN.
  #
  # Parameters:
  #   - tax_units (df) : raw tax units with value.*/basis.* and q_death*
  #
  # Returns: tax_units augmented with G_unit, m_household, age_cohort.
  #----------------------------------------------------------------------------

  values = as.matrix(tax_units[, KG_DYN_ASSET_VALUE_COLS])
  basis  = as.matrix(tax_units[, KG_DYN_ASSET_BASIS_COLS])
  diffs  = values - basis
  diffs[is.na(diffs)] = 0
  diffs[diffs < 0]    = 0

  tax_units %>%
    mutate(
      G_unit      = rowSums(diffs),
      m_household = if_else(filing_status == 2 & !is.na(q_death2),
                            q_death1 * q_death2,
                            q_death1),
      m_household = if_else(is.na(m_household), 0, m_household),
      age_cohort  = if_else(filing_status == 2,
                            pmax(age1, age2, na.rm = TRUE),
                            age1),
      age_cohort  = pmax(KG_DYN_AGE_MIN, pmin(KG_DYN_AGE_MAX, age_cohort))
    )
}



kg_dyn_aggregate_cells = function(tax_units, ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Weight-aggregates per-record (G_unit, kg_lt, m_household) to age cells.
  # tax_units must already have G_unit, m_household, age_cohort attached.
  # Returns a complete grid over `ages`; empty cells fill with zeros.
  #
  # Returns: tibble with age, G_B, R_B, r_B, m, n.
  #----------------------------------------------------------------------------

  agg = tax_units %>%
    group_by(age_cohort) %>%
    summarise(G_B   = sum(weight * G_unit, na.rm = TRUE),
              R_B   = sum(weight * kg_lt, na.rm = TRUE),
              m_num = sum(weight * m_household, na.rm = TRUE),
              n     = sum(weight, na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(G_B, R_B, m_num, n), ~ if_else(is.na(.), 0, .)),
           m   = if_else(n   > 0, m_num / n, 0),
           r_B = if_else(G_B > 0, R_B   / G_B, 0)) %>%
    select(age, G_B, R_B, r_B, m, n) %>%
    arrange(age)
}



kg_dyn_build_heir_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    shift = KG_DYN_HEIR_SHIFT,
                                    sigma = KG_DYN_HEIR_SIGMA) {

  #----------------------------------------------------------------------------
  # Builds row-stochastic heir-allocation matrix omega[a, h] = share of
  # decedent-age-a gains routed to heir-age h. Centered at a - shift with
  # Gaussian noise sigma, evaluated on the integer age grid and renormalized
  # row-by-row. v1 default: spec §6.6 leaves this exogenous; we use a Gaussian
  # placeholder until an estate-module hookup is available.
  #----------------------------------------------------------------------------

  W = outer(ages, ages, function(a, h) dnorm(h, mean = a - shift, sd = sigma))
  W = W / rowSums(W)
  stopifnot(all(abs(rowSums(W) - 1) < 1e-12))
  rownames(W) = colnames(W) = ages
  W
}



kg_dyn_build_aging_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # A[a, h] = 1 if h = a + 1; A[a_max, a_max] = 1 (topcode loops). Spec §3.4.
  #----------------------------------------------------------------------------

  n = length(ages)
  A = matrix(0, n, n, dimnames = list(ages, ages))
  for (i in seq_len(n - 1)) A[i, i + 1] = 1
  A[n, n] = 1
  A
}



kg_dyn_compute_bracket = function(a, c_phi, life_table,
                                  lambda_r = KG_DYN_LAMBDA_R,
                                  beta     = KG_DYN_BETA,
                                  horizon  = KG_DYN_HORIZON,
                                  tau_ratio = NULL) {

  #----------------------------------------------------------------------------
  # Computes the bracket M(c) for a single cohort starting at age `a`. Spec §4:
  #   M(c) = sum_j beta^j s_j (tau_{t+j}/tau_t)
  #         + c * sum_j beta^j d_j (tau_{t+j}/tau_t)
  # with (s_j, d_j) following the §4.3 competing-risks recursion.
  #
  # tau_ratio: length-horizon vector of tau_{t+j}/tau_t. NULL = constant 1
  # (naive expectations). v1 always uses NULL.
  #----------------------------------------------------------------------------

  if (is.null(tau_ratio)) tau_ratio = rep(1, horizon)
  stopifnot(length(tau_ratio) == horizon)

  ages_future = pmin(a + 0:(horizon - 1), KG_DYN_AGE_MAX)
  m_future    = life_table[as.character(ages_future)]
  m_future[is.na(m_future)] = 0

  hazard = pmin(lambda_r + m_future, 0.999)
  S      = c(1, cumprod(1 - hazard))[1:horizon]   # still-holding probability

  s_j = S * lambda_r          # voluntary realization in year j
  d_j = S * m_future          # death-without-realization in year j

  betas = beta ^ (1:horizon)
  sum(betas * s_j * tau_ratio) + c_phi * sum(betas * d_j * tau_ratio)
}



kg_dyn_compute_brackets = function(ages, c_phi, life_table,
                                   lambda_r = KG_DYN_LAMBDA_R,
                                   beta     = KG_DYN_BETA,
                                   horizon  = KG_DYN_HORIZON,
                                   tau_ratio = NULL) {

  out = sapply(ages, function(a) {
    kg_dyn_compute_bracket(a, c_phi, life_table, lambda_r, beta, horizon, tau_ratio)
  })
  names(out) = as.character(ages)
  out
}



kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  P_B, P_S, eta, delta_route) {

  #----------------------------------------------------------------------------
  # One-step bathtub recurrence (spec §3.5). Operates on cell vectors indexed
  # by age. Returns delta_next plus diagnostics (r_S, channel split) for the
  # behavior module to use when allocating realizations to records.
  #
  # Parameters:
  #   - delta_prev (num[a]) : start-of-year delta_G (zero on first year)
  #   - baseline_t (tbl)    : output of kg_dyn_aggregate_cells for year t
  #   - A          (mat)    : aging matrix
  #   - omega      (mat)    : heir matrix
  #   - P_B, P_S   (num[a]) : effective tax price under baseline / reform
  #   - eta        (num)    : behavioral curvature
  #   - delta_route (num)   : routing share for carryover stock transfer
  #
  # Returns: list(delta_next, r_S, delta_surv, delta_inh).
  #----------------------------------------------------------------------------

  G_B = baseline_t$G_B
  r_B = baseline_t$r_B
  m   = baseline_t$m

  r_S = r_B * exp(-eta * (P_S - P_B))

  # Survivor flow (spec §3.2)
  inner      = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m) * inner
  delta_surv = as.numeric(crossprod(A, contrib_a))

  # Inheritance flow (spec §3.3.1)
  if (delta_route > 0) {
    decedent_stock = m * (G_B + delta_prev)
    delta_inh      = delta_route * as.numeric(crossprod(omega, decedent_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  list(delta_next = delta_surv + delta_inh,
       r_S        = r_S,
       delta_surv = delta_surv,
       delta_inh  = delta_inh)
}



kg_dyn_resolve_regime = function(regime_code, theta) {

  #----------------------------------------------------------------------------
  # Maps the integer regime code from pref.kg_death_regime to the canonical
  # (c_phi, delta_vanish, delta_route, delta_realize) tuple. Spec §3.3.
  #
  #   0 = step_up           : c_phi = 0,     vanish = 1
  #   1 = carryover         : c_phi = theta, route = 1
  #   2 = deemed_realization: c_phi = 1,     realize = 1
  #----------------------------------------------------------------------------

  if (regime_code == 0) {
    list(c_phi = 0,     delta_vanish = 1, delta_route = 0, delta_realize = 0)
  } else if (regime_code == 1) {
    list(c_phi = theta, delta_vanish = 0, delta_route = 1, delta_realize = 0)
  } else if (regime_code == 2) {
    list(c_phi = 1,     delta_vanish = 0, delta_route = 0, delta_realize = 1)
  } else {
    stop(paste0('Unknown kg_death_regime: ', regime_code,
                ' (expected 0, 1, or 2)'))
  }
}



kg_dyn_apply_to_records = function(tax_units, baseline_cells_t, r_S, delta_prev,
                                    regime, decedent_random) {

  #----------------------------------------------------------------------------
  # Distributes cell-level reform-vs-baseline realization adjustments back to
  # individual tax-unit kg_lt (spec §7.3) and applies per-record fractional
  # deemed-realization burden. Three channels:
  #
  #   rate-channel    : multiply each record's positive kg_lt by r_S/r_B
  #   carryover-prop  : pro-rata extra realization driven by accumulated dG
  #                     (allocated to records with positive kg_lt; falls back
  #                     to G_unit-share if no realizers in cell)
  #   deemed-channel  : per-record fractional augmentation m_household*G_unit
  #
  # Stochastic decedent_flag is set as a side product for distribution
  # analysis: u < m_household marks a record as decedent. Uses the precomputed
  # uniform draws from globals$random_numbers; same draw across scenarios.
  #
  # Parameters:
  #   - tax_units (df)        : with G_unit, m_household, age_cohort attached
  #   - baseline_cells_t (df) : year-t cell aggregates
  #   - r_S (num[a])          : reform realization rate, named by age
  #   - delta_prev (num[a])   : start-of-year delta_G, named by age
  #   - regime (list)         : output of kg_dyn_resolve_regime()
  #   - decedent_random (num) : uniform[0,1] draws, length = nrow(tax_units)
  #
  # Returns: tax_units with modified kg_lt and added decedent_flag.
  #----------------------------------------------------------------------------

  cell_table = baseline_cells_t %>%
    mutate(age         = as.integer(age),
           r_S         = as.numeric(r_S[as.character(age)]),
           dG          = as.numeric(delta_prev[as.character(age)]),
           rate_factor = if_else(r_B > 0, r_S / r_B, 1),
           extra_R     = regime$delta_route * r_S * dG) %>%
    select(age, R_B, G_B, rate_factor, extra_R)

  tax_units %>%
    left_join(cell_table, by = c('age_cohort' = 'age')) %>%
    mutate(
      # Allocation share for cell-level extra_R (spec §7.3)
      allocation = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      kg_lt_rate    = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt),
      kg_lt_carry   = extra_R * allocation,
      kg_lt_deemed  = regime$delta_realize * m_household * G_unit,
      kg_lt         = kg_lt_rate + kg_lt_carry + kg_lt_deemed,
      decedent_flag = as.integer(decedent_random < m_household)
    ) %>%
    select(-rate_factor, -extra_R, -allocation,
           -kg_lt_rate, -kg_lt_carry, -kg_lt_deemed,
           -R_B, -G_B)
}



kg_dyn_state_path = function(scenario_info, year) {
  file.path(scenario_info$output_path,
            'conventional', 'supplemental',
            'kg_dynamics_state',
            paste0(year, '.rds'))
}



#-------------------------------------------------------------------------------
# Bathtub pre-pass orchestration
#
# These utilities are called once per scenario by run_bathtub_pass() (in run.R)
# or by src/slurm/bathtub.R in the SLURM pipeline. They precompute the entire
# delta_G trajectory and per-year reform realization rate r_S, persisting one
# state file per year. The behavior module then reads its year's state and
# applies the precomputed quantities to records via kg_dyn_apply_to_records().
#-------------------------------------------------------------------------------



kg_dyn_aggregate_baseline_cells_from_taxdata = function(scenario_info,
                                                         sample_ids,
                                                         pct_sample,
                                                         ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Reads Tax-Data microdata directly for each simulation year and aggregates
  # to per-age-cell baseline cell tibbles. Used as the bathtub's baseline-side
  # input. Bypasses the simulator's static detail output because the wealth
  # columns (value.*/basis.*) and q_death* are not in detail_vars; they live
  # only in the source Tax-Data csvs.
  #
  # Parameters:
  #   - scenario_info (list) : provides interface_paths$`Tax-Data` and years
  #   - sample_ids (int[])   : ids in the active sample (globals$sample_ids)
  #   - pct_sample (dbl)     : sampling fraction used to scale weights
  #   - ages (int[])         : age grid for cell aggregation
  #
  # Returns: named list of cell tibbles, indexed by year (as character).
  #----------------------------------------------------------------------------

  tax_data_root = scenario_info$interface_paths$`Tax-Data`
  years = scenario_info$years

  cols_to_read = c('id', 'weight', 'filing_status', 'age1', 'age2',
                   'kg_lt', 'q_death1', 'q_death2',
                   KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  out = lapply(years, function(t) {
    file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = cols_to_read, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample) %>%
      kg_dyn_attach_record_attrs() %>%
      kg_dyn_aggregate_cells(ages)
  })
  setNames(out, as.character(years))
}



kg_dyn_run_bathtub_pass = function(scenario_info, tax_law, baseline_cells,
                                    baseline_tau, reform_tau,
                                    eta = 8.488,
                                    ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Sequentially runs the bathtub recurrence across all years in
  # scenario_info$years and persists one state file per year. The state file
  # is the contract consumed by the kg_dynamics behavior module's per-record
  # applier; the module no longer computes the recurrence itself.
  #
  # For year t, kg_dynamics_state/{t}.rds contains:
  #   list(
  #     delta_prev   = numeric vector (named by age) of Δ_G at start of year t,
  #     r_S          = numeric vector (named by age) of reform realization rate,
  #     regime       = list(c_phi, delta_vanish, delta_route, delta_realize),
  #     baseline_t   = tibble of cell aggregates for year t
  #   )
  #
  # Parameters:
  #   - scenario_info (list)   : provides output_path, years
  #   - tax_law (df)           : reform's joined tax_law tibble (has
  #                              pref.kg_death_regime, pref.kg_bequest_motive)
  #   - baseline_cells (list)  : output of kg_dyn_aggregate_baseline_cells_from_taxdata
  #   - baseline_tau (list)    : named list by year of length-|ages| numeric
  #                              vectors giving tau_B(a, t). For scalar mode
  #                              (v1) every cell of a given year carries the
  #                              same value (top kg rate). For cell-MTR mode
  #                              (v2) entries vary by age.
  #   - reform_tau (list)      : same structure as baseline_tau but for tau_S
  #   - eta (dbl)              : behavioral curvature
  #   - ages (int[])           : age grid
  #
  # Returns: invisibly NULL. Side effect: writes per-year state files plus
  #          life_table.rds under {output_path}/conventional/supplemental/
  #          kg_dynamics_state/.
  #----------------------------------------------------------------------------

  years     = scenario_info$years
  state_dir = file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_state')
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  # Life table from year-1 baseline cells (matches v1 / standalone)
  bc1 = baseline_cells[[as.character(min(years))]]
  life_table = setNames(bc1$m, as.character(bc1$age))
  saveRDS(life_table, file.path(state_dir, 'life_table.rds'))

  # Aging and heir matrices are constant across years
  A     = kg_dyn_build_aging_matrix(ages)
  omega = kg_dyn_build_heir_matrix(ages)

  # Bracket cache by c_phi value (recompute only when regime's c_phi changes)
  bracket_cache = list()
  bracket_cache[['0']] = kg_dyn_compute_brackets(ages, c_phi = 0, life_table)

  delta = setNames(rep(0, length(ages)), as.character(ages))

  for (t in years) {
    bt = baseline_cells[[as.character(t)]]

    # Resolve regime for this year from tax_law (constant across filing statuses)
    tlt          = tax_law %>% filter(year == t) %>% slice(1)
    regime_code  = as.numeric(tlt$pref.kg_death_regime)
    bequest      = as.numeric(tlt$pref.kg_bequest_motive)
    regime       = kg_dyn_resolve_regime(regime_code, bequest)

    c_key = format(regime$c_phi, nsmall = 6)
    if (!c_key %in% names(bracket_cache)) {
      bracket_cache[[c_key]] = kg_dyn_compute_brackets(ages, c_phi = regime$c_phi, life_table)
    }
    bracket_B = bracket_cache[['0']]
    bracket_S = bracket_cache[[c_key]]

    P_B = baseline_tau[[as.character(t)]] * (1 - bracket_B)
    P_S = reform_tau  [[as.character(t)]] * (1 - bracket_S)

    step = kg_dyn_step_recurrence(
      delta_prev  = delta,
      baseline_t  = bt,
      A           = A,
      omega       = omega,
      P_B         = P_B,
      P_S         = P_S,
      eta         = eta,
      delta_route = regime$delta_route
    )
    r_S = setNames(step$r_S, as.character(ages))

    # State file for year t carries the inputs the behavior module needs.
    saveRDS(list(delta_prev = delta,
                 r_S        = r_S,
                 regime     = regime,
                 baseline_t = bt),
            file.path(state_dir, paste0(t, '.rds')))

    delta = setNames(step$delta_next, as.character(ages))
  }

  invisible(NULL)
}



kg_dyn_aggregate_cell_mtr = function(records_with_attrs,
                                      ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Gain-stock-weighted cell-MTR aggregation. Records must already carry
  # G_unit, age_cohort, weight, and mtr_kg_lt. Per cell:
  #
  #   tau(a) = sum(weight * G_unit * mtr_kg_lt) / sum(weight * G_unit)
  #
  # Cells with sum(weight * G_unit) == 0 receive tau = 0 (no realizers,
  # no rate elasticity bite anyway).
  #
  # Returns: numeric vector of length |ages|, named by age (as char).
  #----------------------------------------------------------------------------

  agg = records_with_attrs %>%
    group_by(age_cohort) %>%
    summarise(num = sum(weight * G_unit * mtr_kg_lt, na.rm = TRUE),
              den = sum(weight * G_unit,             na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(num = if_else(is.na(num), 0, num),
           den = if_else(is.na(den), 0, den),
           tau = if_else(den > 0, num / den, 0)) %>%
    arrange(age) %>%
    pull(tau)

  setNames(out, as.character(ages))
}



kg_dyn_build_cellmtr_tau_lists = function(scenario_info, baseline_root,
                                            sample_ids, pct_sample,
                                            ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # v2 cell-MTR mode: builds (baseline_tau, reform_tau) lists where each
  # year's vector carries the gain-stock-weighted cell-aggregate of per-record
  # mtr_kg_lt. Reads Tax-Data (for G_unit / cohort / weight) and joins on id
  # with mtr_kg_lt from baseline static detail and reform static detail.
  #
  # Requires the runscript to register mtr_vars = "kg_lt" so that the static
  # pass writes mtr_kg_lt to detail/{year}.csv. The reform's static detail
  # must already exist (for SLURM: produced by Phase 2A; for main.R: produced
  # by the static-only run_sim() call before run_bathtub_pass()).
  #
  # Returns: list(baseline_tau, reform_tau).
  #----------------------------------------------------------------------------

  years         = scenario_info$years
  tax_data_root = scenario_info$interface_paths$`Tax-Data`

  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'q_death1', 'q_death2',
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  baseline_tau = list()
  reform_tau   = list()

  for (t in years) {

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample) %>%
      kg_dyn_attach_record_attrs()

    bl_mtr = file.path(baseline_root, 'baseline', 'static', 'detail',
                        paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    rf_mtr = file.path(scenario_info$output_path, 'static', 'detail',
                        paste0(t, '.csv')) %>%
      fread(select = c('id', 'mtr_kg_lt'), showProgress = FALSE) %>%
      as_tibble()

    baseline_tau[[as.character(t)]] = td %>%
      left_join(bl_mtr, by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)

    reform_tau[[as.character(t)]] = td %>%
      left_join(rf_mtr, by = 'id') %>%
      kg_dyn_aggregate_cell_mtr(ages)
  }

  list(baseline_tau = baseline_tau, reform_tau = reform_tau)
}



kg_dyn_build_scalar_tau_lists = function(scenario_info, tax_law, baseline_root,
                                          ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Helper for v1 scalar-tau mode: builds (baseline_tau, reform_tau) lists in
  # the format expected by kg_dyn_run_bathtub_pass. Reads pref.rates3 from the
  # baseline scenario's persisted tax_law.csv (top kg bracket rate) and from
  # the reform's joined tax_law tibble.
  #
  # Returns: list(baseline_tau, reform_tau), each a named-by-year list of
  #          length-|ages| numeric vectors.
  #----------------------------------------------------------------------------

  years = scenario_info$years
  n_age = length(ages)

  baseline_tax_law = file.path(baseline_root, 'baseline', 'static',
                                'supplemental', 'tax_law.csv') %>%
    read_csv(show_col_types = FALSE)

  baseline_tau = setNames(
    lapply(years, function(t) {
      v = baseline_tax_law %>% filter(year == t) %>% pull(pref.rates3) %>% .[1]
      rep(v, n_age)
    }),
    as.character(years)
  )

  reform_tau = setNames(
    lapply(years, function(t) {
      v = tax_law %>% filter(year == t) %>% pull(pref.rates3) %>% .[1]
      rep(v, n_age)
    }),
    as.character(years)
  )

  list(baseline_tau = baseline_tau, reform_tau = reform_tau)
}

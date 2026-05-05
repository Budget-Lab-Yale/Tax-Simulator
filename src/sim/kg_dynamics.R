#-------------------------------------------------------------------------------
# kg_dynamics.R
#
# Capital-gains dynamics behavioral module. Implements the law of motion for
# the policy-induced delta in unrealized capital gains specified in
# other/kg_model_tests/capital_gains_realization.md.
#
# Architecture:
#   1. Bathtub pre-pass: solves the recurrence sequentially across years and
#      precomputes everything that doesn't depend on per-record kg_lt --
#      cell-level rate factor, lock-in extra realization, and deemed scaling.
#      Persists one state file per year per scenario.
#   2. Behavior module (config/scenarios/behavior/kg_dynamics/eta06.R): pure
#      allocator. Reads its year's state file and translates cell-level
#      quantities to per-record kg_lt adjustments via kg_dyn_apply_to_records.
#
# Current implementation collapses the five tracked wealth classes into a
# single asset bucket; per-asset-class disaggregation is on the roadmap.
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------

KG_DYN_AGE_MIN     = 18
KG_DYN_AGE_MAX     = 80
KG_DYN_BETA        = 0.96      # annual discount factor (~4%)
KG_DYN_LAMBDA_R    = 0.05      # voluntary realization hazard (asset-aggregate)
KG_DYN_HEIR_SHIFT  = 30        # average decedent-to-heir age gap
KG_DYN_HEIR_SIGMA  = 5         # std dev of heir age distribution

# Default eta. Calibrated by other/kg_model_tests/calibrate_eta.R against
# realization-weighted aggregate elasticity = -0.62 under a 1pp uniform MTR
# perturbation, step-up regime. Last calibrated 2026-05-04 against baseline
# run 202605041857 (30 years of real Tax-Data) anchored at sim-year 30
# (calendar 2055) -- the long-run / "permanent" elasticity anchor.
KG_DYN_DEFAULT_ETA = 6.8750

# Within-cell allocation rule for the policy-induced delta dG.
# Determines which "effective cell mortality" the recurrence uses for
# stock-allocation in the death and survivor channels (see spec §3.3.1).
#
#   "G" — dG within a cell is allocated proportional to G_unit (each
#         holder's share of the cell's gain stock).  Effective rate
#         m_eff = sum(w*m*G) / sum(w*G).  This is the simplest rule and
#         corresponds to the inheritance-flow story (heirs receive a share
#         of decedent stock proportional to their existing G).
#
#   "R" — dG within a cell is allocated proportional to positive realized
#         gains (kg_lt > 0).  Effective rate m_eff = sum(w*m*R)/sum(w*R).
#         Corresponds to the lock-in story (deferred realizations stay
#         with the records that were going to realize the most).
#         Falls back to "G" when R_B = 0 (e.g., young heir cohorts under
#         carryover that haven't yet realized).
#
# Both rules give a per-record-correct sum under their respective allocation
# assumption. The choice affects carryover and (mildly) deemed scoring;
# step-up scenarios are unchanged because the death channel is shut off.
KG_DYN_DG_ALLOCATION = 'G'

KG_DYN_ASSET_VALUE_COLS = c('value.equities', 'value.pass_throughs',
                            'value.primary_home', 'value.other_home',
                            'value.re_fund')
KG_DYN_ASSET_BASIS_COLS = c('basis.equities', 'basis.pass_throughs',
                            'basis.primary_home', 'basis.other_home',
                            'basis.re_fund')


# Death-regime taxonomy (spec §3.3). YAML pref.kg_death_regime is an integer
# code; KG_DYN_REGIME_BY_CODE maps it to a name; KG_DYN_REGIMES carries the
# canonical (delta_vanish, delta_route, delta_realize, c_phi_default) tuple
# for each name. The bequest motive theta is supplied separately and overrides
# c_phi_default for carryover.
KG_DYN_REGIME_BY_CODE = c('0' = 'step_up',
                          '1' = 'carryover',
                          '2' = 'deemed_realization')

KG_DYN_REGIMES = list(
  step_up            = list(c_phi_default = 0,
                            delta_vanish  = 1, delta_route = 0, delta_realize = 0),
  carryover          = list(c_phi_default = NA,           # set from theta
                            delta_vanish  = 0, delta_route = 1, delta_realize = 0),
  deemed_realization = list(c_phi_default = 1,
                            delta_vanish  = 0, delta_route = 0, delta_realize = 1)
)



#-------------------------------------------------------------------------------
# Record-level helpers
#-------------------------------------------------------------------------------

kg_dyn_attach_record_attrs = function(tax_units) {

  #----------------------------------------------------------------------------
  # Adds three derived columns to tax_units used by the bathtub recurrence:
  #   G_unit       : per-record unrealized gain stock, sum_k max(0, value_k -
  #                  basis_k) across the five tracked wealth classes
  #   m_household  : household death probability. q_death1*q_death2 for joint
  #                  filers; q_death1 otherwise
  #   age_cohort   : cohort age. max(age1, age2) for joint, age1 otherwise.
  #                  Top-coded at KG_DYN_AGE_MAX, bottom-coded at AGE_MIN.
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



#-------------------------------------------------------------------------------
# Cell aggregation (with sparse-cell fallback)
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cells = function(tax_units, ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Weight-aggregates per-record (G_unit, kg_lt, m_household) to age cells.
  # tax_units must already have G_unit, m_household, age_cohort attached.
  #
  # Note on R_B convention: R_B is the cell's positive realized gains
  # (sum(weight * pmax(kg_lt, 0))), not the signed sum. The spec defines
  # baseline realizations as positive flows out of the unrealized-gain stock,
  # and using positive-only sums keeps r_B >= 0 and ensures the per-record
  # allocation shares (pmax(kg_lt, 0) / R_B) sum to 1.
  #
  # Sparse-cell fallback (spec §5.1): cells with G_B > 0 but R_B = 0 inherit
  # the gain-stock-weighted aggregate r_B across all age cells. This prevents
  # young heir cohorts (carryover / deemed inflows) from getting r_S = 0
  # forever just because they had no historical realization activity.
  #
  # Returns: tibble with age, G_B, R_B, r_B, m, mG_record.
  #----------------------------------------------------------------------------

  agg = tax_units %>%
    group_by(age_cohort) %>%
    summarise(G_B       = sum(weight * G_unit,                       na.rm = TRUE),
              R_B       = sum(weight * pmax(kg_lt, 0),               na.rm = TRUE),
              m_num     = sum(weight * m_household,                  na.rm = TRUE),
              mG_record = sum(weight * m_household * G_unit,         na.rm = TRUE),
              mR_record = sum(weight * m_household * pmax(kg_lt, 0), na.rm = TRUE),
              w_total   = sum(weight,                                na.rm = TRUE),
              .groups   = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(G_B, R_B, m_num, mG_record, mR_record, w_total),
                  ~ if_else(is.na(.), 0, .)),
           m   = if_else(w_total > 0, m_num / w_total, 0),
           r_B = if_else(G_B     > 0, R_B   / G_B,     0))

  # Pooled rate for sparse cells: only consider cells with R_B > 0 so the
  # cells we're imputing don't drag the imputation toward zero. Should be a
  # no-op under the full-sample requirement enforced in run_bathtub_pass(),
  # but kept for safety on edge cases (e.g. carryover heir cohorts at the
  # youngest ages, where a single-year sample may still be empty).
  ok         = out$R_B > 0
  r_B_pooled = if (any(ok)) sum(out$R_B[ok]) / sum(out$G_B[ok]) else 0

  out %>%
    mutate(r_B = if_else(G_B > 0 & R_B == 0, r_B_pooled, r_B)) %>%
    select(age, G_B, R_B, r_B, m, mG_record, mR_record) %>%
    arrange(age)
}



#-------------------------------------------------------------------------------
# Aging and heir matrices
#-------------------------------------------------------------------------------

kg_dyn_build_heir_matrix = function(ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    shift = KG_DYN_HEIR_SHIFT,
                                    sigma = KG_DYN_HEIR_SIGMA) {

  #----------------------------------------------------------------------------
  # Row-stochastic heir-allocation matrix omega[a, h] = share of decedent-age-a
  # gains routed to heir-age h. Centered at a - shift with Gaussian noise
  # sigma. Placeholder until estate module hookup.
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



#-------------------------------------------------------------------------------
# Bracket and effective tax price (spec §4)
#-------------------------------------------------------------------------------

kg_dyn_compute_bracket = function(a, c_phi, life_table,
                                  lambda_r = KG_DYN_LAMBDA_R,
                                  beta     = KG_DYN_BETA,
                                  tau_ratio = NULL) {

  #----------------------------------------------------------------------------
  # Bracket M(c) for a single cohort starting at age `a` (spec §4.2-4.3):
  #   M(c) = sum_j beta^j s_j (tau_{t+j}/tau_t)
  #         + c * sum_j beta^j d_j (tau_{t+j}/tau_t)
  # with (s_j, d_j) from a competing-risks recursion.
  #
  # The integral is summed to a fixed 200-year ceiling. Beta and the
  # competing-risks decay drive the integrand to <1e-10 well before that:
  # beta * (1 - lambda_r - m) ~ 0.82 means terms decay by 0.82 per year, so
  # the contribution past year 60 is below float noise. The ceiling exists
  # only to bound the loop; nothing in the model depends on it.
  #
  # tau_ratio: length-200 vector of tau_{t+j}/tau_t. NULL = constant 1
  # (naive expectations).
  #----------------------------------------------------------------------------

  horizon = 200L

  if (is.null(tau_ratio)) tau_ratio = rep(1, horizon)
  stopifnot(length(tau_ratio) == horizon)

  # Hazard during year t+i-1 (when the holder is age a+i-1):
  #   hazard[i] = lambda_r + m_{a+i-1}.
  #
  # The pmin(..., AGE_MAX) clamp pins all ages above 80 to the 80+ pool's
  # average mortality. For someone starting at 70, years 11+ of the bracket
  # use m_80 (the pool average, ~0.07-0.10) instead of the true age-90+ rate
  # (0.15-0.30). The bracket integrand at those years is tiny under beta and
  # competing-risks decay, so the bias is small -- but real for cohorts that
  # survive deep into the topcode.
  #
  # POTENTIAL IMPROVEMENT: extend the life table beyond age 80 (SSA actuarial
  # data through ~110) and use it here. Keep the recurrence's age=80 topcode
  # as-is -- the cell aggregation still makes sense -- but let the bracket
  # see proper age-specific mortality forever. Reasonable upgrade if anyone
  # ever runs reforms targeted at the elderly (estate-style).
  ages_hazard = pmin(a + 0:(horizon - 1), KG_DYN_AGE_MAX)
  m_hazard    = life_table[as.character(ages_hazard)]
  m_hazard[is.na(m_hazard)] = 0
  hazard      = pmin(lambda_r + m_hazard, 0.999)

  # S[j] = probability of still holding at the start of year t+j (surviving
  # years t..t+j-1). Pairs with beta^j and tau_{t+j} so that s_j and d_j
  # represent events in year t+j; see spec §4.2-4.3.
  S = cumprod(1 - hazard)[1:horizon]

  # Mortality during year t+j (when age a+j): m_{a+j}.
  ages_mort = pmin(a + 1:horizon, KG_DYN_AGE_MAX)
  m_mort    = life_table[as.character(ages_mort)]
  m_mort[is.na(m_mort)] = 0

  s_j = S * lambda_r          # realize in year t+j
  d_j = S * m_mort            # die in year t+j

  betas = beta ^ (1:horizon)
  sum(betas * s_j * tau_ratio) + c_phi * sum(betas * d_j * tau_ratio)
}



kg_dyn_compute_brackets = function(ages, c_phi, life_table,
                                   lambda_r = KG_DYN_LAMBDA_R,
                                   beta     = KG_DYN_BETA,
                                   tau_ratio = NULL) {

  out = sapply(ages, function(a) {
    kg_dyn_compute_bracket(a, c_phi, life_table, lambda_r, beta, tau_ratio)
  })
  names(out) = as.character(ages)
  out
}



#-------------------------------------------------------------------------------
# Recurrence step (spec §3.5)
#-------------------------------------------------------------------------------

kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  P_B, P_S, eta, delta_route) {

  #----------------------------------------------------------------------------
  # One-step bathtub recurrence. Operates on cell vectors indexed by age.
  #
  # Topcode note: the age=80 cell pools every taxpayer age 80+ into one
  # bucket and uses a single weight-averaged m_80. This is refreshed from
  # each year's Tax-Data, so it tracks the true 80+ population mix over
  # time. The remaining approximation is within-pool heterogeneity --
  # someone who's been in the topcode 15 years (true age ~95) has much
  # higher individual mortality than someone newly aged in. Pooled m_80
  # smooths this out. Small effect in practice because most pool weight is
  # on early-80s, but worth flagging if reforms shift the topcode age mix.
  #
  # r_S is clamped to [0, 1] (spec's choice variable is a probability).
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

  G_B       = baseline_t$G_B
  r_B       = baseline_t$r_B
  R_B       = baseline_t$R_B
  m         = baseline_t$m
  mG_record = baseline_t$mG_record
  mR_record = baseline_t$mR_record

  # ----------------------------------------------------------------------------
  # Why we use an effective cell mortality m_eff = sum(w*m*X) / sum(w*X).
  # ----------------------------------------------------------------------------
  #
  # The death channel needs the cell's *decedent stock contribution* --
  # the sum across records of (death prob) * (per-record gain stock):
  #
  #     D = sum_i w_i * m_i * (G_unit_i + dG_i)
  #
  # The naive cell-level form m * (G_B + dG) replaces this with the
  # cell-mean m times the cell-total stock. That equals the per-record
  # sum only if Cov(m, G_unit | cell) = 0 -- and within an age cell that
  # covariance is large and negative (wealth-mortality gradient: wealthier
  # holders carry more G AND die less). At the G-weighted aggregate, the
  # cell-mean form overstates D by ~2.7x in our data.
  #
  # To avoid materializing per-record state for the recurrence, we adopt
  # an assumption about how cell-level dG is split across records, then
  # compute the per-record sum analytically. Two rules are supported via
  # KG_DYN_DG_ALLOCATION (see constants block):
  #
  #   "G": dG_i proportional to G_unit_i. Then
  #          D = mG_record * (G_B + dG) / G_B = m_eff_G * (G_B + dG)
  #        with m_eff_G = mG_record / G_B.
  #
  #   "R": dG_i proportional to pmax(kg_lt_i, 0). Then
  #          D = mR_record * (R_B + ...) / R_B  -- but R_B is realizations,
  #        not stock, so the algebra doesn't collapse the same way. The
  #        practical compromise: use mR_record / R_B as the effective rate
  #        for the part of D weighted by realizations (i.e., the dG that
  #        accumulated via lock-in -- consistent with the per-record
  #        applier's R-weighted lock-in distribution), and fall back to
  #        the G rule when R_B = 0.
  #
  # In both cases m_eff IS the per-record sum -- not an approximation --
  # under the corresponding allocation rule.
  #
  # Step-up scenarios are unaffected: when delta_route = 0, the death
  # channel is shut off, and the (1-m) vs (1-m_eff) misallocation in the
  # survivor channel only shifts stock between "vanish at death" and
  # "stay in the population", which are observationally equivalent under
  # step-up baseline.
  m_eff_G = if_else(G_B > 0, mG_record / G_B, m)
  m_eff_R = if_else(R_B > 0, mR_record / R_B, m_eff_G)  # fall back to G when R_B = 0

  m_eff = switch(KG_DYN_DG_ALLOCATION,
                 G = m_eff_G,
                 R = m_eff_R,
                 stop("Unknown KG_DYN_DG_ALLOCATION rule: ", KG_DYN_DG_ALLOCATION))
  m_eff = pmin(pmax(m_eff, 0), 1)

  r_S = pmin(pmax(r_B * exp(-eta * (P_S - P_B)), 0), 1)

  # Survivor flow (spec §3.2)
  inner      = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m_eff) * inner
  delta_surv = as.numeric(crossprod(A, contrib_a))

  # Inheritance flow (spec §3.3.1)
  if (delta_route > 0) {
    decedent_stock = m_eff * (G_B + delta_prev)
    delta_inh      = delta_route * as.numeric(crossprod(omega, decedent_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  list(delta_next = delta_surv + delta_inh,
       r_S        = r_S,
       delta_surv = delta_surv,
       delta_inh  = delta_inh)
}



#-------------------------------------------------------------------------------
# Regime resolution (named lookup)
#-------------------------------------------------------------------------------

kg_dyn_resolve_regime = function(regime_code, theta) {

  #----------------------------------------------------------------------------
  # Maps the integer regime code from pref.kg_death_regime to the canonical
  # (name, c_phi, delta_vanish, delta_route, delta_realize) tuple. Spec §3.3.
  #
  #   0 = step_up           : c_phi = 0,     vanish = 1
  #   1 = carryover         : c_phi = theta, route = 1
  #   2 = deemed_realization: c_phi = 1,     realize = 1
  #----------------------------------------------------------------------------

  regime_name = KG_DYN_REGIME_BY_CODE[as.character(regime_code)]
  if (is.na(regime_name)) {
    stop(paste0('Unknown kg_death_regime: ', regime_code,
                ' (expected 0=step_up, 1=carryover, 2=deemed_realization)'))
  }

  base  = KG_DYN_REGIMES[[regime_name]]
  c_phi = if (regime_name == 'carryover') theta else base$c_phi_default

  list(name          = regime_name,
       c_phi         = c_phi,
       delta_vanish  = base$delta_vanish,
       delta_route   = base$delta_route,
       delta_realize = base$delta_realize)
}



#-------------------------------------------------------------------------------
# Per-record applier (pure allocator)
#
# Reads the precomputed cell_table from the bathtub state file and translates
# cell-level quantities (rate_factor, extra_R, deemed_factor) to per-record
# kg_lt adjustments.
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, delta_realize,
                                    decedent_random) {

  #----------------------------------------------------------------------------
  # Distributes cell-level reform-vs-baseline realization adjustments to
  # individual tax-unit kg_lt (spec §7.3) and applies per-record fractional
  # deemed-realization burden. Three channels:
  #
  #   rate-channel   : multiply each record's positive kg_lt by rate_factor
  #                    = r_S/r_B (clamped to 1 when r_B = 0)
  #   lock-in extra  : pro-rata share of cell-level extra_R = r_S * dG;
  #                    allocated by positive-kg_lt share if R_B > 0, else by
  #                    G_unit share, else skip
  #   deemed-channel : per-record m_household * G_unit, scaled by cell-level
  #                    deemed_factor = (G_B + dG)/G_B to incorporate
  #                    accumulated stock
  #
  # decedent_flag is a stochastic side product for distribution analysis:
  # u < m_household marks a record as decedent. Uses the precomputed uniform
  # draws from globals$random_numbers, same draw across scenarios.
  #
  # Parameters:
  #   - tax_units (df)        : with G_unit, m_household, age_cohort attached
  #   - cell_table (tbl)      : output of bathtub pre-pass; has age, G_B, R_B,
  #                             rate_factor, extra_R, deemed_factor
  #   - delta_realize (num)   : routing share for forced realization at death
  #   - decedent_random (num) : uniform[0,1] draws, length = nrow(tax_units)
  #
  # Returns: tax_units with modified kg_lt and added decedent_flag.
  #----------------------------------------------------------------------------

  tax_units %>%
    left_join(cell_table, by = c('age_cohort' = 'age')) %>%
    mutate(
      allocation = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      kg_lt_rate    = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt),
      kg_lt_carry   = extra_R * allocation,
      kg_lt_deemed  = delta_realize * m_household * G_unit * deemed_factor,
      kg_lt         = kg_lt_rate + kg_lt_carry + kg_lt_deemed,
      decedent_flag = as.integer(decedent_random < m_household)
    ) %>%
    select(-rate_factor, -extra_R, -deemed_factor, -allocation,
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
# or by src/slurm/bathtub.R. They precompute the entire delta_G trajectory
# plus per-cell rate_factor / extra_R / deemed_factor, persisting one state
# file per year. The behavior module then reads its year's state and applies
# the precomputed quantities to records via kg_dyn_apply_to_records().
#-------------------------------------------------------------------------------



kg_dyn_load_bathtub_inputs = function(scenario_info, baseline_root,
                                       sample_ids, pct_sample,
                                       ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Single Tax-Data pass that builds all three baseline-side inputs the
  # bathtub needs:
  #
  #   - baseline_cells : named list of cell tibbles (G_B, R_B, r_B, m,
  #                      mG_record) per year
  #   - baseline_tau   : named list of length-|ages| named tau vectors per
  #                      year, R-weighted on baseline static detail's
  #                      mtr_kg_lt
  #   - reform_tau     : same shape, R-weighted on reform static detail
  #
  # Bypasses static detail for the cell aggregates because the wealth
  # columns (value.*/basis.*) and q_death* live only in the source
  # Tax-Data csvs. mtr_kg_lt comes from baseline + reform static detail
  # (requires runscript registers mtr_vars = "kg_lt").
  #----------------------------------------------------------------------------

  tax_data_root = scenario_info$interface_paths$`Tax-Data`
  years         = scenario_info$years

  td_cols = c('id', 'weight', 'filing_status', 'age1', 'age2',
              'kg_lt', 'q_death1', 'q_death2',
              KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)

  baseline_cells = list()
  baseline_tau   = list()
  reform_tau     = list()

  for (t in years) {

    td = file.path(tax_data_root, paste0('tax_units_', t, '.csv')) %>%
      fread(select = td_cols, showProgress = FALSE) %>%
      as_tibble() %>%
      filter(id %in% sample_ids) %>%
      mutate(weight = weight / pct_sample) %>%
      kg_dyn_attach_record_attrs()

    baseline_cells[[as.character(t)]] = kg_dyn_aggregate_cells(td, ages)

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

  list(baseline_cells = baseline_cells,
       baseline_tau   = baseline_tau,
       reform_tau     = reform_tau)
}



kg_dyn_build_cell_table = function(baseline_t, r_S_vec, delta_prev,
                                    tau_B_vec, tau_S_vec) {

  #----------------------------------------------------------------------------
  # Assembles the per-cell quantities the per-record applier needs (rate
  # factor, lock-in extra realization, deemed scaling) plus diagnostic
  # quantities used by kg_dyn_build_summary (tau_B, tau_S, m). Persisted into
  # the state file by kg_dyn_run_bathtub_pass.
  #
  # Per-cell quantities (spec §3.5, §5.3, §3.3.2):
  #   rate_factor   = r_S / r_B           (clamped to 1 when r_B = 0)
  #   extra_R       = r_S * dG            (lock-in stock realized at rate r_S;
  #                                        applies under all regimes -- spec's
  #                                        ratio formula uses G_S = G_B + dG)
  #   deemed_factor = (G_B + dG) / G_B    (clamped to >= 0; scales per-record
  #                                        m * G_unit so deemed revenue
  #                                        includes accumulated stock)
  #
  # delta_realize is carried separately (in regime) and gates whether the
  # deemed channel fires at all.
  #----------------------------------------------------------------------------

  baseline_t %>%
    mutate(age           = as.integer(age),
           r_S           = as.numeric(r_S_vec[as.character(age)]),
           dG            = as.numeric(delta_prev[as.character(age)]),
           tau_B         = as.numeric(tau_B_vec[as.character(age)]),
           tau_S         = as.numeric(tau_S_vec[as.character(age)]),
           rate_factor   = if_else(r_B > 0, r_S / r_B, 1),
           extra_R       = r_S * dG,
           deemed_factor = if_else(G_B > 0,
                                   pmax(0, (G_B + dG) / G_B),
                                   1)) %>%
    select(age, G_B, R_B, r_B, r_S, m, mG_record, mR_record, dG,
           tau_B, tau_S, rate_factor, extra_R, deemed_factor)
}



kg_dyn_run_bathtub_pass = function(scenario_info, tax_law, baseline_cells,
                                    baseline_tau, reform_tau,
                                    eta = KG_DYN_DEFAULT_ETA,
                                    ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Sequentially runs the bathtub recurrence across scenario_info$years and
  # persists one state file per year. The state file is the contract consumed
  # by the kg_dynamics behavior module's per-record applier; the module no
  # longer computes any cell-level math itself.
  #
  # State file at kg_dynamics_state/{t}.rds:
  #   list(
  #     regime     = list(name, c_phi, delta_vanish, delta_route, delta_realize),
  #     cell_table = tibble(age, G_B, R_B, r_B, r_S, m, dG, tau_B, tau_S,
  #                          rate_factor, extra_R, deemed_factor)
  #   )
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  years     = scenario_info$years
  state_dir = file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_state')
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  # Life table from year-1 baseline cells
  bc1 = baseline_cells[[as.character(min(years))]]
  life_table = setNames(bc1$m, as.character(bc1$age))
  saveRDS(life_table, file.path(state_dir, 'life_table.rds'))

  A     = kg_dyn_build_aging_matrix(ages)
  omega = kg_dyn_build_heir_matrix(ages)

  # Bracket cache by c_phi value (recompute only when regime's c_phi changes).
  # Always key by format(c_phi, nsmall = 6) so the baseline (c=0) and reform
  # entries can never end up under inconsistent keys.
  bracket_cache = list()
  c_key_B       = format(0, nsmall = 6)

  delta = setNames(rep(0, length(ages)), as.character(ages))

  for (t in years) {
    bt = baseline_cells[[as.character(t)]]

    tlt          = tax_law %>% filter(year == t) %>% slice(1)
    regime_code  = as.numeric(tlt$pref.kg_death_regime)
    bequest      = as.numeric(tlt$pref.kg_bequest_motive)
    regime       = kg_dyn_resolve_regime(regime_code, bequest)

    c_key_S = format(regime$c_phi, nsmall = 6)
    for (k in unique(c(c_key_B, c_key_S))) {
      if (!k %in% names(bracket_cache)) {
        bracket_cache[[k]] = kg_dyn_compute_brackets(ages, c_phi = as.numeric(k), life_table)
      }
    }
    bracket_B = bracket_cache[[c_key_B]]
    bracket_S = bracket_cache[[c_key_S]]

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
    r_S_vec = setNames(step$r_S, as.character(ages))

    cell_table = kg_dyn_build_cell_table(
      baseline_t = bt,
      r_S_vec    = r_S_vec,
      delta_prev = delta,
      tau_B_vec  = setNames(baseline_tau[[as.character(t)]], as.character(ages)),
      tau_S_vec  = setNames(reform_tau  [[as.character(t)]], as.character(ages))
    )

    saveRDS(list(regime     = regime,
                 cell_table = cell_table),
            kg_dyn_state_path(scenario_info, t))

    delta = setNames(step$delta_next, as.character(ages))
  }

  invisible(NULL)
}



#-------------------------------------------------------------------------------
# Cell-MTR tau builder
#
# Each cohort uses its own gain-stock-weighted average effective MTR on
# kg_lt, pulled from the simulator's static detail. This is the only
# supported tau parameterization; flat top-rate proxies are not.
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cell_mtr = function(records_with_attrs,
                                      ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Realization-weighted cell-MTR aggregation. Records must carry G_unit,
  # age_cohort, weight, kg_lt, and mtr_kg_lt. Per cell:
  #
  #   tau(a) = sum(weight * pmax(kg_lt, 0) * mtr_kg_lt) / sum(weight * pmax(kg_lt, 0))
  #
  # The realization-weighting is the right anchor for elasticity calibration:
  # it captures the average MTR on the dollars that actually realize, which
  # is the variable the realization decision responds to. Falls back to
  # gain-stock weighting when a cell has zero positive realizations (young
  # heir cohorts under carryover); falls back to 0 when both are zero.
  #----------------------------------------------------------------------------

  agg = records_with_attrs %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    group_by(age_cohort) %>%
    summarise(num_R = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE),
              den_R = sum(weight * kg_pos,             na.rm = TRUE),
              num_G = sum(weight * G_unit * mtr_kg_lt, na.rm = TRUE),
              den_G = sum(weight * G_unit,             na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(num_R, den_R, num_G, den_G), ~ if_else(is.na(.), 0, .)),
           tau = case_when(
             den_R > 0 ~ num_R / den_R,
             den_G > 0 ~ num_G / den_G,
             TRUE      ~ 0
           )) %>%
    arrange(age) %>%
    pull(tau)

  setNames(out, as.character(ages))
}



#-------------------------------------------------------------------------------
# Post-processing: bathtub diagnostics summary
#-------------------------------------------------------------------------------

kg_dyn_build_summary = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads all per-year bathtub state files for one scenario and writes two
  # diagnostic CSVs:
  #
  #   conventional/supplemental/kg_dynamics_age_profile.csv
  #     Long format (year × age) dump of cell_table. Use this for plots
  #     of dG, r_B, r_S, m, etc. across age and time.
  #
  #   conventional/supplemental/kg_dynamics_summary.csv
  #     Year-level rollup: regime parameters, gain-stock-weighted average
  #     mortality / realization rates / MTRs, channel decomposition of
  #     reform-induced realizations, decedent stock and routing, and an
  #     implied year-by-year semi-elasticity.
  #
  # No-op if the scenario has no bathtub state directory.
  #----------------------------------------------------------------------------

  state_dir = file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_state')
  if (!dir.exists(state_dir)) return(invisible(NULL))

  years = scenario_info$years
  state_files = file.path(state_dir, paste0(years, '.rds'))
  if (!all(file.exists(state_files))) return(invisible(NULL))

  # Long-format age profile + per-year regime metadata
  states = lapply(years, function(t) readRDS(file.path(state_dir, paste0(t, '.rds'))))
  names(states) = as.character(years)

  age_profile = bind_rows(lapply(seq_along(years), function(i) {
    s = states[[i]]
    ct = s$cell_table
    # Backfill columns that may be missing from older state files. For
    # mG_record (added 2026-05), fall back to the cell-product form m * G_B
    # so the summary still computes for legacy state files; new runs use
    # the per-record sum the simulator actually applies.
    if (!('m'         %in% names(ct))) ct$m         = NA_real_
    if (!('tau_B'     %in% names(ct))) ct$tau_B     = NA_real_
    if (!('tau_S'     %in% names(ct))) ct$tau_S     = NA_real_
    if (!('mG_record' %in% names(ct))) ct$mG_record = ct$m * ct$G_B
    if (!('mR_record' %in% names(ct))) ct$mR_record = ct$m * ct$R_B
    ct %>%
      mutate(year   = years[i],
             regime = s$regime$name) %>%
      relocate(year, regime, age)
  }))

  age_profile %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_age_profile.csv'))

  # Year-level rollup
  regime_df = bind_rows(lapply(seq_along(years), function(i) {
    r = states[[i]]$regime
    tibble(year          = years[i],
           regime        = r$name,
           c_phi         = r$c_phi,
           delta_vanish  = r$delta_vanish,
           delta_route   = r$delta_route,
           delta_realize = r$delta_realize)
  }))

  yearly = age_profile %>%
    group_by(year) %>%
    summarise(
      G_B_total      = sum(G_B),
      R_B_total      = sum(R_B),
      dG_total       = sum(dG),
      m_avg_gw       = if_else(sum(G_B) > 0, sum(m     * G_B, na.rm = TRUE) / sum(G_B), NA_real_),
      r_B_avg_gw     = if_else(sum(G_B) > 0, sum(r_B   * G_B) / sum(G_B), 0),
      r_S_avg_gw     = if_else(sum(G_B) > 0, sum(r_S   * G_B) / sum(G_B), 0),
      tau_B_avg_gw   = if_else(sum(G_B) > 0, sum(tau_B * G_B, na.rm = TRUE) / sum(G_B), NA_real_),
      tau_S_avg_gw   = if_else(sum(G_B) > 0, sum(tau_S * G_B, na.rm = TRUE) / sum(G_B), NA_real_),
      tau_B_avg_rw   = if_else(sum(R_B) > 0, sum(tau_B * R_B, na.rm = TRUE) / sum(R_B), NA_real_),
      tau_S_avg_rw   = if_else(sum(R_B) > 0, sum(tau_S * R_B, na.rm = TRUE) / sum(R_B), NA_real_),
      rate_channel   = sum(R_B * (rate_factor - 1)),  # ΔR from rate change on baseline stock
      lockin_channel = sum(extra_R),                  # ΔR from accumulated dG
      # decedent_stock uses the same per-record sum the simulator applies in
      # kg_dyn_apply_to_records (sum(weight * m_household * G_unit), scaled
      # by deemed_factor to fold in accumulated dG). The cell-product form
      # sum(m * (G_B + dG)) ignores within-cell covariance between m and
      # G_unit and overstates the decedent stock by ~3-4x in practice.
      decedent_stock = sum(mG_record * deemed_factor),
      .groups = 'drop'
    ) %>%
    left_join(regime_df, by = 'year') %>%
    mutate(
      inheritance_flow = delta_route   * decedent_stock,
      deemed_realized  = delta_realize * decedent_stock,
      R_S_total        = R_B_total + rate_channel + lockin_channel,
      # eta_implied uses realization-weighted tau to match the calibrator
      # (other/kg_model_tests/calibrate_eta.R). G-weighted columns are kept
      # for inspection but are not the right denominator for the elasticity.
      dlog_tau         = if_else(tau_B_avg_rw > 0 & tau_S_avg_rw > 0,
                                 log(tau_S_avg_rw / tau_B_avg_rw), 0),
      eta_implied      = if_else(R_B_total > 0 & abs(dlog_tau) > 1e-10,
                                 log(R_S_total / R_B_total) / dlog_tau,
                                 NA_real_)
    ) %>%
    select(year, regime, c_phi, delta_vanish, delta_route, delta_realize,
           G_B_total, R_B_total, R_S_total, dG_total,
           m_avg_gw, r_B_avg_gw, r_S_avg_gw,
           tau_B_avg_gw, tau_S_avg_gw, tau_B_avg_rw, tau_S_avg_rw,
           rate_channel, lockin_channel,
           decedent_stock, inheritance_flow, deemed_realized,
           eta_implied)

  yearly %>%
    write_csv(file.path(scenario_info$output_path,
                        'conventional', 'supplemental',
                        'kg_dynamics_summary.csv'))

  invisible(NULL)
}

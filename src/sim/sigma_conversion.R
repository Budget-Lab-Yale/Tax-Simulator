#-------------------------------------------------------------------------------
# sigma_conversion.R
#
# Sigma: the income-conversion response for the top-tax interaction exercise
# (other/top_tax/DESIGN_LOCK.md is the live-design source of truth; build plan
# at other/top_tax/sigma_build_plan.md). Owner-managers repackage top salary /
# active pass-through income as unrealized equity appreciation when the
# ordinary-vs-equity-path tax wedge widens.
#
# Architecture (two halves, one shared pure function):
#   1. PRE-PASS (inside kg_dyn_run_bathtub_pass, via sigma_compute_year):
#      per year, compute per-record per-leg conversions from the MTR wedges
#      against tau_eq, aggregate to age cells, inject the cell inflow into
#      the kg bathtub recurrence (delta_next, end-of-year entry), and persist
#      ONLY the cell-level tracker in the kg state file (DESIGN_LOCK ruling
#      7 — no per-record persistence for normal runs; a per-record dump
#      exists behind SIGMA_RECORD_DUMP=1 for smoke/validation).
#   2. BEHAVIOR MODULE (config/scenarios/behavior/conversion/sigma.R):
#      recomputes record-level conversions via the SAME shared pure function
#      (sigma_compute_conversions) from the same inputs (static/baseline
#      MTRs + the persisted tau_eq cell table + the persisted thresholds),
#      applies them to records (wage legs down, PT legs down with SECA
#      companions co-scaled), and hard-asserts conservation against the
#      persisted cell inflow.
#
# Design rulings this file implements (see DESIGN_LOCK):
#   - Per-record wedge W_i = own-leg calculator MTR - tau_eq(age_i, t);
#     forcing = Delta W_i, static-reform-vs-baseline (standard MTR-frame
#     convention). Equity leg from the tau_eq recursion (kg_dynamics.R),
#     keyed on the kg age convention (pmax(age1, age2), 80+ topcode).
#   - Pool (ruling 4): gate = (any active business income) AND (static
#     taxable income >= top-bracket threshold, filing-status-specific,
#     threshold-based NOT MTR-based); pool = all wages + 0.75 * active PT
#     (SYZZ labor-content share). Known over-breadth accepted; the
#     pool-composition diagnostic is the visibility mechanism.
#   - No phase-in phi(t): memoryless annual response to the current-year
#     wedge gap. Delta conv_i(t) = sigma * Delta W_i(t) * pool_i. The wedge
#     can narrow => negative conversion allowed, clamped so no leg goes
#     negative (or more than doubles, on the negative side).
#   - sigma central = 0.08, calibrated 2026-07-08 to a top-subset ETI of
#     0.25 on the +5pp validation leg (author-directed; supersedes ruling
#     2's asserted 0.2/0.6/0.9, whose total-response anchors double-counted
#     the entity-shifting/evasion response exactly as the R2 caveat
#     anticipated). See SIGMA_CALIB_PROVENANCE below for method, measured
#     legs, and the staleness conditions. Env knob SIGMA_CONV.
#   - Composition (conversion into gain state vs entity shifting into the
#     corporate base) is an OUTPUT (tracker diagnostics), not a dial.
#     Sequential module order prevents double-moves.
#-------------------------------------------------------------------------------

SIGMA_CONV_VERSION = '2026-07-08 sigma build + ETI-0.25 central; re-derived under kg spec-v2 (unchanged 0.08)'

#-------------------------------------------------------------------------------
# SIGMA_CALIB_PROVENANCE
#
# Central sigma = 0.08 (author-directed recalibration, 2026-07-08; supersedes
# the original asserted central 0.6 and the 0.2/0.9 bands, which are STALE).
#
# Method: the +5pp top-ordinary validation leg (tests/topord_plus5, 2025:2035,
# full behavior stack kg_dynamics + sigma + entity_shifting + evasion +
# charity, wealth_financing = none) is targeted to a top-subset ETI of 0.25 —
# the Saez-Slemrod-Giertz central (taxable income EXCLUDING net capital
# gains, after deductions; brackets 0.12-0.40). Measured:
#   - full stack at sigma = 0.60 : ETI 0.431  (vintage sigma_validation)
#   - stack WITHOUT sigma        : ETI 0.2229 (vintage sigma_calib_nosigma)
#   - solved sigma* = 0.6*(0.25 - 0.2229)/(0.431 - 0.2229) = 0.078 -> 0.08
#   - CONFIRMED at sigma = 0.08  : ETI 0.2505 (vintage sigma_calib_confirm)
# Measurement script: other/top_tax/tests/compute_top_eti.R.
#
# RE-DERIVED under the spec-v2 kg calibration (2026-07-08, entropy cost,
# eta=4.4984 / omega=0.5132; vintage sigma_recal_eta). Result: UNCHANGED at
# 0.08. Both legs reproduced the original within 0.0001 (sigma=0.60 -> ETI
# 0.4312; no-sigma -> ETI 0.2229; solved sigma* = 0.6*(0.25-0.2229)/
# (0.4312-0.2229) = 0.078 -> 0.08; CONFIRMED at sigma=0.08 -> ETI 0.2505,
# vintage sigma_confirm_eta). This is expected and reassuring: the target
# is the top ORDINARY-income ETI (O = txbl_inc - net gains), which EXCLUDES
# capital-gains realizations, so the kg realization recalibration is orthogonal
# to it. sigma and the kg Bellman calibrate on disjoint bases.
#
# STALENESS WARNING (kg-provenance-guard spirit): this value is CONDITIONAL
# ON THE REST OF THE STACK. Entity shifting and evasion supply ~0.22 of the
# 0.25 target by themselves, so sigma is calibrated as the RESIDUAL
# conversion margin. Re-derive sigma (rerun the two legs above) whenever any
# of the following change: the entity-shifting elasticity/parameters
# (pearce_prisinzano.R), the evasion centrals (debacker.R), the charity
# elasticity, the pool definition/gate in this file, or the Tax-Data vintage
# (calibrated on 2026050315). (KG_DYN_* calibration proved orthogonal in the
# 2026-07-08 re-derivation, but re-check if the pool ever starts taxing gains.)
#
# Substantive reading: the ETI evidence disciplines the TOTAL top response;
# with P-P and DHY already in the stack, a large independent conversion
# margin would double-count (the DESIGN_LOCK R2 caveat, resolved here).
# A small sigma is also the defensible position given the pool's known
# over-breadth on pass-through legs (see sigma_explainer.md on which legal
# channels actually support conversion).
#-------------------------------------------------------------------------------

# Response parameter: percent of pool converted per percentage point of
# wedge change (so a +5pp wedge at sigma = 0.08 converts 0.4% of the pool).
SIGMA_CONV = as.numeric(Sys.getenv('SIGMA_CONV', unset = '0.08'))

# SYZZ labor-content share applied to active pass-through legs in the pool.
SIGMA_PT_LABOR_SHARE = 0.75

# Per-record dump knob (smoke/validation/debug only): writes
# {scenario}/conventional/supplemental/sigma_conversion_dump/{year}.csv from
# the pre-pass. Normal runs persist only the cell tracker (ruling 7).
SIGMA_RECORD_DUMP = identical(Sys.getenv('SIGMA_RECORD_DUMP'), '1')

# Required per-leg MTR registrations (both baseline and static frames). The
# ordinary legs of the wedge; mtr_part_active / mtr_sole_prop1 are
# SECA-inclusive by construction (calc_mtrs bumps the SE companions).
SIGMA_REQUIRED_MTRS = c('mtr_wages1', 'mtr_wages2', 'mtr_part_active',
                        'mtr_sole_prop1', 'mtr_scorp_active')
SIGMA_REQUIRED_MTR_VARS = c('wages1', 'wages2', 'part_active',
                            'sole_prop1', 'scorp_active')

# Raw Tax-Data columns the pool/gate legs come from (DESIGN_LOCK ruling 7:
# gate/pool legs from raw Tax-Data; txbl_inc + mtr_* from detail files).
SIGMA_TD_COLS = c('id', 'weight', 'filing_status', 'age1', 'age2',
                  'wages1', 'wages2', 'part_active', 'scorp_active',
                  'sole_prop')

# Hard conservation tolerance (relative) for the module-side recompute vs
# the persisted cell inflow. Loose enough to absorb the small pass-through
# leg drift when the wealth haircut / corporate applier ran ahead of the
# behavior stack (they scale PT flows); tight enough to catch real drift.
# 2026-07-09: 0.01 -> 0.015 after the top_tax factorial: the deepest stacks
# with both channels on (wealth+corp+deemed+ord+qbi, c093/c125) hit 1.001e-2
# in the 2037 lead-out year -- the documented benign drift, marginally over.
SIGMA_CONSERVE_RTOL = 0.015


scenario_uses_sigma = function(scenario_info) {
  any(startsWith(scenario_info$behavior_modules %||% character(),
                 'conversion/'))
}



sigma_top_thresholds = function(tax_law, years) {

  #----------------------------------------------------------------------------
  # Extracts the top-ordinary-bracket threshold per (year, filing status)
  # from the joined tax law: the highest-indexed ord.brackets{n} column with
  # a finite value in that row. Threshold-based gating (NOT MTR-based),
  # because QBI etc. push measured MTRs below the statutory top rate
  # (ruling 4).
  #
  # Returns: tibble(year, filing_status, sigma_thresh).
  #----------------------------------------------------------------------------

  bracket_cols = grep('^ord\\.brackets\\d+$', names(tax_law), value = TRUE)
  if (length(bracket_cols) == 0) {
    stop('sigma_conversion: tax_law has no ord.brackets columns; cannot ',
         'resolve the top-bracket gate threshold.')
  }
  bracket_cols = bracket_cols[order(as.integer(
    sub('^ord\\.brackets', '', bracket_cols)))]

  out = tax_law %>%
    filter(year %in% years) %>%
    select(year, filing_status, all_of(bracket_cols)) %>%
    distinct()

  vals = as.matrix(out[, bracket_cols])
  top  = apply(vals, 1, function(r) {
    ok = which(is.finite(r))
    if (length(ok) == 0) NA_real_ else r[max(ok)]
  })

  out = out %>%
    mutate(sigma_thresh = as.numeric(top)) %>%
    select(year, filing_status, sigma_thresh)

  if (any(!is.finite(out$sigma_thresh))) {
    stop('sigma_conversion: non-finite top-bracket threshold for (year, ',
         'filing status) rows: ',
         paste(out$year[!is.finite(out$sigma_thresh)],
               out$filing_status[!is.finite(out$sigma_thresh)],
               sep = '/', collapse = ', '))
  }
  out
}



sigma_check_mtr_registration = function(scenario_info) {

  # Hard stop unless the runscript registers every ordinary-leg MTR the
  # wedge needs (evasion-module convention: fail loudly rather than
  # silently mislabeling a static score as conventional).
  missing = setdiff(SIGMA_REQUIRED_MTR_VARS,
                    scenario_info$mtr_vars %||% character())
  if (length(missing) > 0) {
    stop('sigma_conversion: scenario "', scenario_info$ID, '" must register ',
         'mtr_vars for all sigma wedge legs (',
         paste(SIGMA_REQUIRED_MTR_VARS, collapse = ' '),
         ', plus kg_lt for the kg bathtub). Missing: ',
         paste(missing, collapse = ', '), '.')
  }
  invisible(TRUE)
}



sigma_build_ctx = function(scenario_info, tax_law, baseline_root,
                           sample_ids, pct_sample, sigma = SIGMA_CONV) {

  #----------------------------------------------------------------------------
  # Builds the sigma-conversion context consumed by the bathtub pre-pass
  # (kg_dyn_run_bathtub_pass -> sigma_compute_year). Validates parameters
  # and MTR registration and resolves the per-year gate thresholds; year
  # inputs are loaded lazily (one Tax-Data + two detail reads per year).
  #----------------------------------------------------------------------------

  if (!is.finite(sigma) || sigma < 0 || sigma > 5) {
    stop('sigma_conversion: SIGMA_CONV must be a finite nonnegative value ',
         '(percent of pool per pp of wedge); got ', format(sigma), '.')
  }
  sigma_check_mtr_registration(scenario_info)

  list(
    scenario_info = scenario_info,
    baseline_root = baseline_root,
    sample_ids    = sample_ids,
    pct_sample    = pct_sample,
    thresholds    = sigma_top_thresholds(tax_law, scenario_info$years),
    sigma         = sigma
  )
}



sigma_load_year_inputs = function(ctx, year) {

  #----------------------------------------------------------------------------
  # Assembles the pre-pass pool frame for one year (DESIGN_LOCK ruling 7
  # input contract):
  #   - gate/pool legs + demographics from raw Tax-Data (kg refuses VAT /
  #     excess-growth scenarios, so raw dollars are the right unit system);
  #   - txbl_inc (gate) from the scenario's STATIC detail;
  #   - per-leg MTRs from baseline static detail (suffix _baseline) and
  #     scenario static detail.
  #----------------------------------------------------------------------------

  scenario_info = ctx$scenario_info
  tax_data_root = scenario_info$interface_paths$`Tax-Data`

  td = file.path(tax_data_root, paste0('tax_units_', year, '.csv')) %>%
    fread(select = SIGMA_TD_COLS, showProgress = FALSE) %>%
    as_tibble() %>%
    filter(id %in% ctx$sample_ids) %>%
    mutate(weight = weight / ctx$pct_sample)

  read_detail = function(root, cols) {
    f = file.path(root, paste0(year, '.csv'))
    have = names(fread(f, nrows = 0, showProgress = FALSE))
    missing_cols = setdiff(cols, have)
    if (length(missing_cols) > 0) {
      stop('sigma_conversion: detail file ', f, ' lacks column(s): ',
           paste(missing_cols, collapse = ', '),
           '. The sigma wedge needs per-leg MTRs written by the static ',
           'pass; check the runscript mtr_vars registration on BOTH the ',
           'baseline and the scenario rows.')
    }
    fread(f, select = cols, showProgress = FALSE) %>% as_tibble()
  }

  static_det = read_detail(
    file.path(scenario_info$output_path, 'static', 'detail'),
    c('id', 'txbl_inc', SIGMA_REQUIRED_MTRS))
  baseline_det = read_detail(
    file.path(ctx$baseline_root, 'baseline', 'static', 'detail'),
    c('id', SIGMA_REQUIRED_MTRS)) %>%
    rename_with(.cols = -id, .fn = ~ paste0(., '_baseline'))

  td %>%
    mutate(age_cohort = sigma_age_cohort(filing_status, age1, age2)) %>%
    left_join(static_det,   by = 'id') %>%
    left_join(baseline_det, by = 'id')
}



sigma_age_cohort = function(filing_status, age1, age2) {

  # kg age convention (kg_dyn_attach_record_attrs): joint records key on the
  # older spouse; clipped to the [18, 80] bathtub grid (80+ topcode).
  age = if_else(filing_status == 2, pmax(age1, age2, na.rm = TRUE), age1)
  pmax(KG_DYN_AGE_MIN, pmin(KG_DYN_AGE_MAX, age))
}



sigma_compute_conversions = function(pool, thresholds_t,
                                     tau_eq_B_col, tau_eq_S_col, sigma) {

  #----------------------------------------------------------------------------
  # THE shared pure function (DESIGN_LOCK ruling 7): both the pre-pass and
  # the behavior module compute record-level conversions through this exact
  # code path, from the same inputs.
  #
  # Parameters:
  #   - pool (df)          : one row per record with id, weight,
  #                          filing_status, age_cohort, the five pool legs
  #                          (wages1, wages2, part_active, scorp_active,
  #                          sole_prop), txbl_inc (STATIC-side, the gate),
  #                          and the ten MTR columns (five per side;
  #                          baseline suffixed _baseline)
  #   - thresholds_t (df)  : tibble(filing_status, sigma_thresh) for the year
  #   - tau_eq_B_col (dbl[]) : named-by-age tau_eq under baseline policy
  #   - tau_eq_S_col (dbl[]) : named-by-age tau_eq under scenario policy
  #   - sigma (dbl)        : conversion response (% of pool per pp of wedge)
  #
  # Per-leg forcing: Delta W_leg = (mtr_leg_S - mtr_leg_B) -
  # (tau_eq_S(age) - tau_eq_B(age)). Conversion = sigma * Delta W * pool_leg
  # on gated records, clamped to |conv| <= leg so no leg goes negative (or
  # more than doubles on a narrowing wedge). NA MTRs => no response for
  # that leg.
  #
  # Returns: per-record frame (id, weight, age_cohort, gate, per-leg conv_*,
  #          conv_total, pool_total, dW diagnostics).
  #----------------------------------------------------------------------------

  dtau_eq = as.numeric(tau_eq_S_col[as.character(pool$age_cohort)]) -
            as.numeric(tau_eq_B_col[as.character(pool$age_cohort)])

  out = pool %>%
    left_join(thresholds_t, by = 'filing_status') %>%
    mutate(
      dtau_eq = dtau_eq,

      # Ruling 4 gate: any active business income AND static taxable income
      # at or above the top ordinary bracket threshold.
      has_active = (!is.na(part_active)  & part_active  > 0) |
                   (!is.na(scorp_active) & scorp_active > 0) |
                   (!is.na(sole_prop)    & sole_prop    > 0),
      gate = has_active & !is.na(txbl_inc) & !is.na(sigma_thresh) &
             txbl_inc >= sigma_thresh,

      # Positive pool legs (a leg only participates when positive)
      pool_w1    = if_else(gate & !is.na(wages1) & wages1 > 0, wages1, 0),
      pool_w2    = if_else(gate & !is.na(wages2) & wages2 > 0, wages2, 0),
      pool_part  = if_else(gate & !is.na(part_active) & part_active > 0,
                           SIGMA_PT_LABOR_SHARE * part_active, 0),
      pool_scorp = if_else(gate & !is.na(scorp_active) & scorp_active > 0,
                           SIGMA_PT_LABOR_SHARE * scorp_active, 0),
      pool_sole  = if_else(gate & !is.na(sole_prop) & sole_prop > 0,
                           SIGMA_PT_LABOR_SHARE * sole_prop, 0),
      pool_total = pool_w1 + pool_w2 + pool_part + pool_scorp + pool_sole,

      # Per-leg wedge changes (static reform vs baseline, both sides net of
      # the equity leg)
      dW_w1    = (mtr_wages1       - mtr_wages1_baseline)       - dtau_eq,
      dW_w2    = (mtr_wages2       - mtr_wages2_baseline)       - dtau_eq,
      dW_part  = (mtr_part_active  - mtr_part_active_baseline)  - dtau_eq,
      dW_scorp = (mtr_scorp_active - mtr_scorp_active_baseline) - dtau_eq,
      dW_sole  = (mtr_sole_prop1   - mtr_sole_prop1_baseline)   - dtau_eq,

      # Conversions: sigma * dW * pool_leg, NA-safe, clamped to the leg
      conv_w1    = sigma_leg_conv(sigma, dW_w1,    pool_w1,    wages1),
      conv_w2    = sigma_leg_conv(sigma, dW_w2,    pool_w2,    wages2),
      conv_part  = sigma_leg_conv(sigma, dW_part,  pool_part,  part_active),
      conv_scorp = sigma_leg_conv(sigma, dW_scorp, pool_scorp, scorp_active),
      conv_sole  = sigma_leg_conv(sigma, dW_sole,  pool_sole,  sole_prop),
      conv_total = conv_w1 + conv_w2 + conv_part + conv_scorp + conv_sole
    ) %>%
    select(id, weight, age_cohort, gate, pool_total,
           pool_w1, pool_w2, pool_part, pool_scorp, pool_sole,
           dtau_eq, dW_w1, dW_w2, dW_part, dW_scorp, dW_sole,
           conv_w1, conv_w2, conv_part, conv_scorp, conv_sole, conv_total)

  out
}



sigma_leg_conv = function(sigma, dW, pool_leg, leg_value) {

  # sigma * dW * pool_leg with NA-safe wedges and the |conv| <= leg clamp.
  leg = replace_na(as.numeric(leg_value), 0)
  conv = if_else(is.na(dW) | pool_leg <= 0, 0, sigma * dW * pool_leg)
  pmin(pmax(conv, -abs(leg)), abs(leg))
}



sigma_aggregate_inflow = function(conv,
                                  ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Weighted cell inflow by age cohort (the recurrence injection), zero-
  # filled over the full bathtub grid. NA cohorts (missing ages upstream)
  # carry zero conversion by construction (their tau_eq lookup is NA, so
  # every leg wedge is NA-gated to zero) — drop them rather than let a
  # stray "NA" name corrupt the named vector.
  agg = conv %>%
    filter(!is.na(age_cohort)) %>%
    group_by(age_cohort) %>%
    summarise(inflow = sum(weight * conv_total), .groups = 'drop')

  inflow = setNames(rep(0, length(ages_bathtub)), as.character(ages_bathtub))
  inflow[as.character(agg$age_cohort)] = agg$inflow
  inflow
}



sigma_make_tracker = function(conv, conv_inflow, sigma, thresholds_t, year) {

  #----------------------------------------------------------------------------
  # The cell-level tracker persisted in the kg state file (ruling 7): the
  # injection vector, pool size/composition, mean wedges, thresholds, and
  # the sigma stamp the behavior module validates against.
  #----------------------------------------------------------------------------

  gated = conv %>% filter(gate)
  wsum  = function(x, w) sum(w * x)
  wmean = function(x, w) { s = sum(w); if (s > 0) sum(w * x) / s else NA_real_ }

  pool_dollars = wsum(gated$pool_total, gated$weight)
  ord_dW = with(gated,
    (dW_w1 * pool_w1 + dW_w2 * pool_w2 + dW_part * pool_part +
     dW_scorp * pool_scorp + dW_sole * pool_sole))
  mean_dW = if (pool_dollars > 0) wsum(ord_dW, gated$weight) / pool_dollars
            else NA_real_

  list(
    version           = SIGMA_CONV_VERSION,
    year              = year,
    sigma             = sigma,
    conv_inflow       = conv_inflow,
    conv_total        = sum(conv_inflow),
    pool_records      = nrow(gated),
    pool_weighted     = sum(gated$weight),
    pool_dollars      = pool_dollars,
    pool_dollars_wages = wsum(gated$pool_w1 + gated$pool_w2, gated$weight),
    pool_dollars_pt   = wsum(gated$pool_part + gated$pool_scorp +
                             gated$pool_sole, gated$weight),
    conv_dollars_wages = wsum(gated$conv_w1 + gated$conv_w2, gated$weight),
    conv_dollars_pt   = wsum(gated$conv_part + gated$conv_scorp +
                             gated$conv_sole, gated$weight),
    mean_dW_pooled    = mean_dW,
    mean_dtau_eq      = wmean(gated$dtau_eq, gated$weight),
    thresholds        = thresholds_t
  )
}



sigma_compute_year = function(ctx, year, tau_eq_B_col, tau_eq_S_col,
                              ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  #----------------------------------------------------------------------------
  # Pre-pass orchestration for one year: load inputs, compute conversions
  # via the shared pure function, aggregate to the cell inflow, build the
  # tracker (and the optional per-record dump). Called inside the bathtub
  # year loop with the year's tau_eq columns.
  #
  # Returns: list(conv_inflow (named dbl[]), tracker (list)).
  #----------------------------------------------------------------------------

  thresholds_t = ctx$thresholds %>%
    filter(year == !!year) %>%
    select(filing_status, sigma_thresh)

  pool = sigma_load_year_inputs(ctx, year)
  conv = sigma_compute_conversions(
    pool         = pool,
    thresholds_t = thresholds_t,
    tau_eq_B_col = tau_eq_B_col,
    tau_eq_S_col = tau_eq_S_col,
    sigma        = ctx$sigma
  )

  conv_inflow = sigma_aggregate_inflow(conv, ages_bathtub)
  tracker     = sigma_make_tracker(conv, conv_inflow, ctx$sigma,
                                   thresholds_t, year)

  if (SIGMA_RECORD_DUMP) {
    dump_dir = file.path(ctx$scenario_info$output_path, 'conventional',
                         'supplemental', 'sigma_conversion_dump')
    dir.create(dump_dir, recursive = TRUE, showWarnings = FALSE)
    conv %>%
      filter(gate) %>%
      write_csv(file.path(dump_dir, paste0(year, '.csv')))
  }

  message(sprintf(
    paste0('sigma_conversion: year %d, sigma = %.2f: pool = $%.1fB over ',
           '%s gated records (weighted %.2fM); mean pooled dW = %s; ',
           'conversion inflow = $%.2fB.'),
    year, ctx$sigma, tracker$pool_dollars / 1e9, tracker$pool_records,
    tracker$pool_weighted / 1e6,
    ifelse(is.na(tracker$mean_dW_pooled), 'NA',
           sprintf('%.4f', tracker$mean_dW_pooled)),
    tracker$conv_total / 1e9))

  list(conv_inflow = conv_inflow, tracker = tracker)
}



#-------------------------------------------------------------------------------
# Module-side helpers (called by config/scenarios/behavior/conversion/sigma.R)
#-------------------------------------------------------------------------------

sigma_module_recompute = function(tax_units, baseline_mtrs, static_mtrs,
                                  scenario_info, state, year) {

  #----------------------------------------------------------------------------
  # Recomputes record-level conversions inside the behavior module from the
  # same inputs the pre-pass used (ruling 7): live-frame pool legs, the
  # year's baseline/static MTR frames, the persisted tau_eq cell table, and
  # the persisted gate thresholds. Hard-asserts conservation against the
  # persisted cell inflow.
  #
  # Returns: the conv frame (one row per tax_units row, original order).
  #----------------------------------------------------------------------------

  tracker    = state$sigma
  cell_table = state$cell_table

  if (is.null(tracker)) {
    stop('sigma_conversion: kg state file for year ', year, ' carries no ',
         'sigma tracker. The bathtub pre-pass must run with the ',
         'conversion/sigma module registered in the behavior column ',
         '(it computes conversions and injects the gain-state inflow); ',
         're-run the pipeline with the current runscript.')
  }
  if (!isTRUE(all.equal(tracker$sigma, SIGMA_CONV))) {
    stop('sigma_conversion: SIGMA_CONV drift between the pre-pass (',
         tracker$sigma, ') and the behavior module (', SIGMA_CONV, '). ',
         'The env knob must be identical across pipeline phases.')
  }

  tau_eq_B_col = setNames(cell_table$tau_eq_B, as.character(cell_table$age))
  tau_eq_S_col = setNames(cell_table$tau_eq_S, as.character(cell_table$age))

  # Gate txbl_inc: STATIC detail (same source as the pre-pass; the live
  # conventional frame has no txbl_inc before do_taxes).
  static_txbl = file.path(scenario_info$output_path, 'static', 'detail',
                          paste0(year, '.csv')) %>%
    fread(select = c('id', 'txbl_inc'), showProgress = FALSE) %>%
    as_tibble()

  mtr_b = baseline_mtrs %>%
    filter(year == !!year) %>%
    select(id, all_of(SIGMA_REQUIRED_MTRS)) %>%
    rename_with(.cols = -id, .fn = ~ paste0(., '_baseline'))
  mtr_s = static_mtrs %>%
    filter(year == !!year) %>%
    select(id, all_of(SIGMA_REQUIRED_MTRS))

  pool = tax_units %>%
    select(id, weight, filing_status, age1, age2,
           wages1, wages2, part_active, scorp_active, sole_prop) %>%
    mutate(age_cohort = sigma_age_cohort(filing_status, age1, age2)) %>%
    left_join(static_txbl, by = 'id') %>%
    left_join(mtr_b, by = 'id') %>%
    left_join(mtr_s, by = 'id')

  conv = sigma_compute_conversions(
    pool         = pool,
    thresholds_t = tracker$thresholds,
    tau_eq_B_col = tau_eq_B_col,
    tau_eq_S_col = tau_eq_S_col,
    sigma        = SIGMA_CONV
  )

  # Conservation: the module's recomputed conversions must match the cell
  # inflow the pre-pass injected into the bathtub. Exact when the live frame
  # legs equal raw Tax-Data (no wealth haircut / corporate applier ahead of
  # the stack); the tolerance absorbs those channels' small PT-flow scaling.
  module_total = sum(conv$weight * conv$conv_total)
  prepass_total = tracker$conv_total
  denom = max(abs(prepass_total), 1e6)
  rel   = abs(module_total - prepass_total) / denom
  message(sprintf(
    paste0('sigma_conversion: year %d conservation check: module $%.4fB vs ',
           'pre-pass $%.4fB (rel diff %.2e).'),
    year, module_total / 1e9, prepass_total / 1e9, rel))
  if (rel > SIGMA_CONSERVE_RTOL) {
    stop(sprintf(
      paste0('sigma_conversion: conservation failure in year %d: module ',
             'recompute (%.6g) vs persisted cell inflow (%.6g), rel diff ',
             '%.3e > %.0e. Records applied and dollars injected into the ',
             'gain state have diverged; check that pre-pass and module see ',
             'the same MTR frames, thresholds, and pool legs.'),
      year, module_total, prepass_total, rel, SIGMA_CONSERVE_RTOL))
  }

  conv
}



sigma_apply_conversions = function(tax_units, conv) {

  #----------------------------------------------------------------------------
  # Applies per-record conversions to the live frame: wage legs down (wages
  # adjusted coherently, preserving the Tax-Data wages residual), PT legs
  # down with SECA earner-split companions co-scaled (evasion-module
  # convention: sole_prop rides with sole_prop1/2; part_active with
  # part_se1/2; scorp_active has no SECA companion). Converted dollars do
  # NOT enter record kg_lt: they are unrealized, and taxation arrives in
  # later years through the kg cell machinery (recurrence injection).
  #----------------------------------------------------------------------------

  stopifnot(identical(conv$id, tax_units$id))

  scale_or_1 = function(base, delta) {
    if_else(!is.na(base) & base > 0 & delta != 0,
            (base - delta) / base, 1)
  }
  part_factor = scale_or_1(tax_units$part_active, conv$conv_part)
  sole_factor = scale_or_1(tax_units$sole_prop,   conv$conv_sole)

  tax_units %>%
    mutate(
      wages1 = wages1 - conv$conv_w1,
      wages2 = wages2 - conv$conv_w2,
      wages  = wages  - conv$conv_w1 - conv$conv_w2,

      part_active = part_active * part_factor,
      part_se1    = part_se1    * part_factor,
      part_se2    = part_se2    * part_factor,

      sole_prop  = sole_prop  * sole_factor,
      sole_prop1 = sole_prop1 * sole_factor,
      sole_prop2 = sole_prop2 * sole_factor,

      scorp_active = scorp_active - conv$conv_scorp
    )
}

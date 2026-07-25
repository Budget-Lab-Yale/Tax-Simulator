#-------------------------------------------------------------------------------
# recurrence.R
#
# Bathtub recurrence: the step, the regime mix, the cell table, and the bathtub / frozen pass drivers.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Bathtub recurrence step
#-------------------------------------------------------------------------------

kg_dyn_cell_m_eff = function(baseline_t) {

  # Effective cell mortality m_eff = sum(w*m*X) / sum(w*X). The death
  # channel needs sum_i w_i * m_i * (G_unit_i + dG_i); the naive cell-mean
  # form m * (G_B + dG) overstates that by ~2.7x in our data due to a
  # large negative within-cell Cov(m, G_unit) (wealth-mortality gradient).
  # Allocating dG_i proportional to X_i and summing analytically gives an
  # exact per-record sum, not an approximation. Two rules via
  # KG_DYN_DG_ALLOCATION: "G" (X = G_unit) or "R" (X = pmax(kg_lt, 0),
  # falling back to "G" when R_B = 0).
  #
  # Shared by kg_dyn_step_recurrence and the tau_eq machinery
  # (kg_dyn_tau_eq_primitives) so the two stay in lockstep on what
  # mortality the delta stock experiences.

  m_eff_G = if_else(baseline_t$G_B > 0,
                    baseline_t$mG_record / baseline_t$G_B, baseline_t$m)
  m_eff_R = if_else(baseline_t$R_B > 0,
                    baseline_t$mR_record / baseline_t$R_B, m_eff_G)

  m_eff = switch(KG_DYN_DG_ALLOCATION,
                 G = m_eff_G,
                 R = m_eff_R,
                 stop("Unknown KG_DYN_DG_ALLOCATION rule: ", KG_DYN_DG_ALLOCATION))
  pmin(pmax(m_eff, 0), 1)
}



kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  r_S_vec, delta_route_vec,
                                  conv_inflow_vec = NULL) {

  # One-step bathtub recurrence for delta_G on the [18, 80] grid. r_S_vec is
  # the scenario realization rate: the full-pool Bellman level response plus
  # the retimed short-run timing overlay.
  # delta_route_vec is a length-n_ages cell-level share of the dying stock
  # that routes to heirs (carryover); under per-asset regime mixing it's
  # produced by kg_dyn_build_regime_mix as sum_k share_k(a) * route_k.
  #
  # conv_inflow_vec (optional): length-n_ages vector of sigma-conversion
  # dollars entering the gain state at END of year t (the inheritance-inflow
  # convention, DESIGN_LOCK R6): it joins delta_next directly, participating
  # in realization/death dynamics from t+1 onward. NULL = no conversion
  # channel (identical output).
  #
  # Topcode caveat: the age=80 cell pools all 80+ taxpayers with a single
  # weight-averaged m_80, refreshed from each year's Tax-Data. Within-pool
  # heterogeneity (e.g., 15-year topcode residents vs. newly aged-in) is
  # smoothed out — small effect in practice but worth flagging if reforms
  # shift the topcode age mix.

  G_B       = baseline_t$G_B
  r_B       = baseline_t$r_B
  p_char    = pmin(pmax(baseline_t$p_char, 0), 1)

  m_eff = kg_dyn_cell_m_eff(baseline_t)

  r_S = pmin(pmax(r_S_vec, 0), 1)

  # Survivor flow (spec §3.2)
  inner      = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m_eff) * inner
  delta_surv = as.numeric(crossprod(A, contrib_a))

  # Inheritance flow (spec §3.3.1). delta_route_vec is per-cell so a cell
  # whose regime mix has no carryover share contributes nothing to the
  # routing crossprod even when adjacent cells do.
  decedent_stock      = m_eff * (G_B + delta_prev)
  terminal_char_stock = p_char * decedent_stock
  taxable_death_stock = (1 - p_char) * decedent_stock
  if (any(delta_route_vec > 0)) {
    delta_inh = as.numeric(crossprod(omega,
                                     delta_route_vec * taxable_death_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  # Sigma-conversion inflow: converted compensation enters the gain state at
  # end of year (participates from t+1, like the inheritance inflow).
  conv_inflow = if (is.null(conv_inflow_vec)) {
    rep(0, length(delta_prev))
  } else {
    stopifnot(length(conv_inflow_vec) == length(delta_prev))
    as.numeric(conv_inflow_vec)
  }

  list(delta_next = delta_surv + delta_inh + conv_inflow,
       r_S        = r_S,
       delta_surv = delta_surv,
       delta_inh  = delta_inh,
       conv_inflow = conv_inflow,
       decedent_stock      = decedent_stock,
       terminal_char_stock = terminal_char_stock,
       taxable_death_stock = taxable_death_stock)
}



#-------------------------------------------------------------------------------
# Cell-level regime mix (per-asset codes → per-age vanish/route/realize + c_phi)
#-------------------------------------------------------------------------------

kg_dyn_build_regime_mix = function(regime_codes, theta, baseline_t,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Aggregates per-asset regime codes into cell-level multipliers via
  # gain-stock-weighted shares:
  #   share_k(a)              = G_B_k(a) / G_B(a)
  #   share_primary_above_cap = G_B_primary_above_cap(a) / G_B(a)
  #   delta_{vanish,route,realize}(a) = sum_k share_k(a) * triplet_k$*
  #
  # c_phi(a) (share of cell gain stock taxed at death, the death-state
  # burden share the holder internalizes in the Bellman):
  #   c_phi(a) = sum_{k, deemed}            live_share_k(a)
  #            + theta * sum_{k, carryover} live_share_k(a)   (route internalized)
  # where live_share_k = share_primary_above_cap for primary_home (§121-net),
  # share_k otherwise. §121 nets the exclusion cap under BOTH deemed and
  # carryover (see live_share in the regime loop below).
  #
  # regime_codes : named list of 5 integer codes (one per asset class).
  # theta        : scalar bequest motive in [0, 1].
  # baseline_t   : per-cell tibble with G_B, G_B_{class}, G_B_primary_above_cap.
  #
  # Returns tibble keyed by age with delta_vanish, delta_route, delta_realize,
  # c_phi — each on the bathtub grid [18, 80].

  asset_classes = KG_DYN_ASSET_CLASSES

  missing = setdiff(asset_classes, names(regime_codes))
  if (length(missing) > 0) {
    stop('kg_dyn_build_regime_mix: regime_codes missing asset classes: ',
         paste(missing, collapse = ', '))
  }

  resolve_triplet = function(code) {
    t = KG_DYN_REGIME_TRIPLET[[as.character(code)]]
    if (is.null(t)) {
      stop('kg_dyn_build_regime_mix: unknown regime code ', code,
           ' (expected 0=step_up, 1=carryover, 2=deemed_realization)')
    }
    t
  }
  triplets = lapply(asset_classes, function(k) resolve_triplet(regime_codes[[k]]))
  names(triplets) = asset_classes

  G_B = baseline_t$G_B
  safe_share = function(num) if_else(G_B > 0, num / G_B, 0)

  share = lapply(asset_classes,
                 function(k) safe_share(baseline_t[[paste0('G_B_', k)]]))
  names(share) = asset_classes
  share_primary_above_cap = safe_share(baseline_t$G_B_primary_above_cap)

  n_cells       = length(G_B)
  delta_vanish  = rep(0, n_cells)
  delta_route   = rep(0, n_cells)
  delta_realize = rep(0, n_cells)
  c_phi         = rep(0, n_cells)

  for (k in asset_classes) {
    tr = triplets[[k]]

    # §121 primary-residence exclusion. Under BOTH deemed realization and
    # carryover, only the above-cap primary-home gain is "live": deemed taxes
    # it on the decedent's final return; carryover routes it to heirs. Both are
    # modeled as a death-time basis step-up of up to the §121 cap, so the
    # below-cap portion never enters the taxable/routed stock. Under step-up the
    # whole home gain is forgiven, so §121 is moot (delta_vanish keeps the full
    # share). live_share is §121-net for primary_home, the full share otherwise.
    live_share = if (k == 'primary_home') share_primary_above_cap else share[[k]]

    delta_vanish  = delta_vanish  + share[[k]] * tr$vanish
    delta_route   = delta_route   + live_share * tr$route
    delta_realize = delta_realize + live_share * tr$realize

    # Carryover internalization: holder values theta of the routed stock
    # (§121-net for primary_home).
    c_phi = c_phi + theta * tr$route * live_share

    # Deemed realization burden share (§121-net for primary_home).
    c_phi = c_phi + tr$realize * live_share
  }

  tibble(
    age           = baseline_t$age,
    delta_vanish  = delta_vanish,
    delta_route   = delta_route,
    delta_realize = delta_realize,
    c_phi         = pmin(pmax(c_phi, 0), 1)
  )
}



kg_dyn_build_cell_table = function(baseline_t, year_idx,
                                    r_S_vec,
                                    delta_prev,
                                    tau_B_col, tau_S_col,
                                    W_B_col, W_S_col, MC_B_col, MC_S_col,
                                    kappa_col, r_D_B_col, r_D_S_col,
                                    regime_mix,
                                    planned_diag = NULL,
                                    death_diag = NULL,
                                    corp_debit = NULL,
                                    tau_eq_B_col = NULL,
                                    tau_eq_S_col = NULL,
                                    conv_inflow_vec = NULL,
                                    carry_h_col = NULL,
                                    tau_w_col = NULL,
                                    estate_e_B_col = NULL,
                                    estate_e_S_col = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Assembles per-cell quantities the applier needs:
  #   rate_factor   = r_S / r_B           (clamped to 1 when r_B = 0)
  #   extra_R       = r_S * (dG - corp_gain_debit)  (lock-in stock realized
  #                   at r_S; the corporate-incidence gain-state debit -- the
  #                   PRICE margin of the equity markdown, D18 -- reduces the
  #                   realized deviation stock. It is a per-year LEVEL
  #                   adjustment recomputed from the current markdown
  #                   (corp_kg_state_debit_by_year), NEVER accumulated through
  #                   the recurrence, and deliberately NOT in deemed_factor:
  #                   deemed gains already carry the markdown through the
  #                   record-level value.* columns the corporate applier
  #                   scaled on the conventional frame.)
  #   deemed_factor = (G_B + dG) / G_B    (clamped >= 0; CLEAN dG)
  # Plus diagnostic columns used by kg_dyn_build_summary: per-asset
  # G_B_{class}, G_B_primary_above_cap, cell-level regime-mix outputs
  # (delta_vanish/route/realize, c_phi). Bellman matrices are sliced from
  # the extended grid to the bathtub grid [18, 80] before persisting.
  #
  # corp_debit: optional named (by age) vector of gain-state debit dollars
  # (>= 0 for a hike); NULL for non-corporate scenarios (identical output).

  ages_chr = as.character(ages_bathtub)
  diag_or = function(name, default) {
    v = planned_diag[[name]]
    if (!is.null(v)) return(v)
    if (length(default) == length(ages_chr)) {
      setNames(as.vector(default), ages_chr)
    } else {
      setNames(rep(default, length(ages_chr)), ages_chr)
    }
  }
  death_or = function(name, default) {
    v = death_diag[[name]]
    if (!is.null(v)) return(v)
    if (length(default) == length(ages_chr)) {
      setNames(as.vector(default), ages_chr)
    } else {
      setNames(rep(default, length(ages_chr)), ages_chr)
    }
  }

  mix_lookup = regime_mix
  mix_lookup$age = as.character(mix_lookup$age)

  if (is.null(corp_debit)) {
    corp_debit = setNames(rep(0, length(ages_chr)), ages_chr)
  }

  # tau_eq / sigma-conversion columns: additive diagnostics (zeros when the
  # tau_eq recursion or the conversion channel is off).
  if (is.null(tau_eq_B_col)) {
    tau_eq_B_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }
  if (is.null(tau_eq_S_col)) {
    tau_eq_S_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }
  if (is.null(conv_inflow_vec)) {
    conv_inflow_vec = setNames(rep(0, length(ages_chr)), ages_chr)
  }

  # Wealth-carry columns: additive diagnostics on the same zero-default
  # pattern (all-zero for every non-wealth run — byte-diff tooling should
  # compare revenue/detail CSVs, not the kg state/diagnostic files).
  # carry_h is the h the Bellman/tau_eq actually consumed (post-
  # KG_WEALTH_CARRY_SCALE); tau_w is the plain gain-weighted mtr_net_worth
  # mean, diagnostics only.
  if (is.null(carry_h_col)) {
    carry_h_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }
  if (is.null(tau_w_col)) {
    tau_w_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }

  baseline_t %>%
    mutate(age           = as.integer(age),
           r_S           = as.numeric(r_S_vec     [as.character(age)]),
           r_S_unclipped = as.numeric(diag_or('r_S_unclipped', r_S)[as.character(age)]),
           timing_clipped = as.logical(diag_or('timing_clipped', FALSE)[as.character(age)]),
           r_D_B         = as.numeric(r_D_B_col   [as.character(age)]),
           r_D_S         = as.numeric(r_D_S_col   [as.character(age)]),
           r_planned_B   = as.numeric(diag_or('r_planned_B', 0)[as.character(age)]),
           r_planned_S   = as.numeric(diag_or('r_planned_S', 0)[as.character(age)]),
           r_ordinary_B  = as.numeric(diag_or('r_ordinary_B', r_D_B)[as.character(age)]),
           r_ordinary_S  = as.numeric(diag_or('r_ordinary_S', r_D_S)[as.character(age)]),
           R_planned_B   = as.numeric(diag_or('R_planned_B', 0)[as.character(age)]),
           R_planned_S   = as.numeric(diag_or('R_planned_S', 0)[as.character(age)]),
           planned_timing_shift =
             as.numeric(diag_or('planned_timing_shift', 0)[as.character(age)]),
           dG            = as.numeric(delta_prev  [as.character(age)]),
           tau_B         = as.numeric(tau_B_col   [as.character(age)]),
           tau_S         = as.numeric(tau_S_col   [as.character(age)]),
           W_B           = as.numeric(W_B_col     [as.character(age)]),
           W_S           = as.numeric(W_S_col     [as.character(age)]),
           MC_B          = as.numeric(MC_B_col    [as.character(age)]),
           MC_S          = as.numeric(MC_S_col    [as.character(age)]),
           kappa         = as.numeric(kappa_col   [as.character(age)]),
           delta_vanish  = mix_lookup$delta_vanish [match(as.character(age), mix_lookup$age)],
           delta_route   = mix_lookup$delta_route  [match(as.character(age), mix_lookup$age)],
           delta_realize = mix_lookup$delta_realize[match(as.character(age), mix_lookup$age)],
           c_phi         = mix_lookup$c_phi        [match(as.character(age), mix_lookup$age)],
           decedent_stock =
             as.numeric(death_or('decedent_stock', 0)[as.character(age)]),
           terminal_char_stock =
             as.numeric(death_or('terminal_char_stock', 0)[as.character(age)]),
           taxable_death_stock =
             as.numeric(death_or('taxable_death_stock', 0)[as.character(age)]),
           corp_gain_debit = as.numeric(corp_debit[as.character(age)]),
           corp_gain_debit = if_else(is.na(corp_gain_debit), 0, corp_gain_debit),
           tau_eq_B      = as.numeric(tau_eq_B_col   [as.character(age)]),
           tau_eq_S      = as.numeric(tau_eq_S_col   [as.character(age)]),
           conv_inflow   = as.numeric(conv_inflow_vec[as.character(age)]),
           conv_inflow   = if_else(is.na(conv_inflow), 0, conv_inflow),
           carry_h       = as.numeric(carry_h_col    [as.character(age)]),
           carry_h       = if_else(is.na(carry_h), 0, carry_h),
           tau_w         = as.numeric(tau_w_col      [as.character(age)]),
           tau_w         = if_else(is.na(tau_w), 0, tau_w),
           estate_e_B    = as.numeric(estate_e_B_col [as.character(age)]),
           estate_e_B    = if_else(is.na(estate_e_B), 0, estate_e_B),
           estate_e_S    = as.numeric(estate_e_S_col [as.character(age)]),
           estate_e_S    = if_else(is.na(estate_e_S), 0, estate_e_S),
           rate_factor   = if_else(r_B > 0, r_S / r_B, 1),
           # Clamp the lock-in stock to the cell's gain stock: under
           # permanent rate hikes dG can run sufficiently negative that
           # r_S * dG would subtract more from kg_lt than the cell holds.
           # pmax(., -G_B) caps the drawdown at full depletion of G_B,
           # consistent with deemed_factor's >=0 clamp below. The corporate
           # gain-state debit reduces the realized deviation stock here (and
           # ONLY here -- see docstring).
           extra_R       = r_S * pmax(dG - corp_gain_debit, -G_B),
           deemed_factor = if_else(G_B > 0,
                                   pmax(0, (G_B + dG) / G_B),
                                   1)) %>%
    select(age, G_B, R_B, r_B, r_S, r_S_unclipped, timing_clipped,
           r_planned_B, r_planned_S, r_ordinary_B, r_ordinary_S,
           R_planned_B, R_planned_S, planned_timing_shift,
           m, mG_record, mR_record, dG, corp_gain_debit,
           p_char, p_char_extensive, p_char_intensive, estate_2026_m_avg_dgw,
           G_B_equities, G_B_pass_throughs, G_B_primary_home,
           G_B_other_home, G_B_re_fund, G_B_primary_above_cap,
           delta_vanish, delta_route, delta_realize, c_phi,
           decedent_stock, terminal_char_stock, taxable_death_stock,
           tau_B, tau_S, W_B, W_S, MC_B, MC_S, kappa, r_D_B, r_D_S,
           tau_eq_B, tau_eq_S, conv_inflow, carry_h, tau_w,
           estate_e_B, estate_e_S,
           rate_factor, extra_R, deemed_factor)
}



kg_dyn_run_bathtub_pass = function(scenario_info, tax_law, baseline_cells,
                                    baseline_tau, reform_tau,
                                    reform_tau_timing, heir_dist,
                                    form  = KG_DYN_RESPONSE_FORM,
                                    eta   = kg_dyn_active_eta(form),
                                    timeable_share = kg_dyn_active_timeable_share(form),
                                    timing_window = KG_DYN_TIMING_WINDOW,
                                    ref_wedge     = KG_DYN_TIMING_REF_WEDGE,
                                    corp_debit_by_year = NULL,
                                    sigma_ctx = NULL,
                                    reform_carry = NULL,
                                    baseline_estate = NULL,
                                    reform_estate = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  # Runs the bathtub recurrence across scenario_info$years and persists one
  # state file per year — the contract consumed by the kg_dynamics behavior
  # module's per-record applier. State at kg_dynamics_state/{t}.rds is
  # list(regime, cell_table) plus, for sigma-conversion scenarios, a
  # cell-level sigma tracker (DESIGN_LOCK ruling 7).
  #
  # corp_debit_by_year (optional): per-year named vectors of the corporate
  # gain-state debit (corp_kg_state_debit_by_year; NULL = no corporate
  # channel). Threaded into kg_dyn_build_cell_table's extra_R only -- the
  # RECURRENCE runs on the clean behavioral delta by design (the debit is a
  # level adjustment recomputed each year from the current markdown; routing
  # it through delta would compound it and double-count heirs' markdown,
  # which next year's recomputed debit already covers).
  #
  # reform_carry (optional): per-year list(h, tau_w) of named age vectors
  # from kg_dyn_aggregate_cell_carry (via kg_dyn_load_bathtub_inputs) — the
  # wealth-tax deferral carrying cost. h is packed onto the Bellman grid
  # (age-80 repeated forward, kg_dyn_pack_tau), scaled by the
  # KG_WEALTH_CARRY_SCALE env var (default 1; a DISCLOSED, uncalibrated
  # statutory-vs-effective sensitivity knob — e.g. set to the
  # retained-reported share under avoidance), and threaded into Pass 2 of
  # the Bellman and the scenario-side tau_eq recursion. Pass 1 (baseline)
  # and prims_B NEVER receive h: h_B == 0 by law, asserted in the loader.
  # NULL or all-zero h (every non-wealth scenario) leaves the channel dormant,
  # bar the all-zero carry_h/tau_w diagnostic columns.
  #
  # baseline_estate / reform_estate (optional): per-year named age vectors
  # from kg_dyn_aggregate_cell_estate (via kg_dyn_load_bathtub_inputs) —
  # the LEG-PAIRED estate exposure of the kg death value (cell-aggregated
  # switch-gated mtr_estate_ded, clamped [0, 1]). Packed onto the Bellman
  # grid like tau (age-80 repeated forward). Pass 1 and prims_B receive
  # e_B; Pass 2 and prims_S receive e_S — NEVER a single shared matrix
  # (that would zero out estate-only reforms: e_S > e_B with tau unchanged
  # would give MC_S = MC_B and no realization response). Unlike h there is
  # no zero-baseline invariant: current law HAS an estate tax, so e_B > 0
  # for estate-taxable cells — which also means (1 - e_B) touches the
  # CURRENT-LAW Bellman and hence the eta long-run-elasticity anchor
  # (re-check it when this channel changes). NULL = zeros (unit tests /
  # pre-build callers), leaving the channel dormant bar the all-zero
  # estate_e_B/estate_e_S state columns.
  #
  # Baseline-regime assumption: Pass 1 hard-codes step-up (c_phi = 0,
  # mix_list = NULL); adding baseline estate exposure e_B is correct IN
  # CONJUNCTION with that convention (F_B = tau_B * (1 - e_B), and prims_B
  # has realize = 0 so e_B enters tau_eq_B only through... nothing — the
  # death-realize term is zero under step-up; e_B's bite is in the Bellman).
  # If baselines ever carry carryover/deemed regimes, revisit the
  # baseline-side construction here.
  #
  # sigma_ctx (optional): sigma-conversion context built by
  # sigma_build_ctx() (src/sim/sigma_conversion.R) when the scenario runs
  # the conversion/sigma behavior module. Per year, the pass computes
  # per-record conversions from the per-leg MTR wedges against tau_eq,
  # aggregates them to age cells, and injects the cell inflow into the
  # recurrence's delta_next (end-of-year entry). Only the cell tracker is
  # persisted; the behavior module recomputes record conversions from the
  # same inputs (ruling 7). NULL = no conversion channel.
  #
  # Flow:
  #   1. Build extended-grid baseline cells (bathtub + 81-119 SSA tail).
  #   2. Pack tau matrices (baseline + reform).
  #   3. Pass 1 Bellman (baseline): recover kappa.
  #   4. Resolve per-year scenario regime; Pass 2 Bellman using kappa.
  #   5. Build planned-timing schedule from law-only tau_S minus tau_B
  #      (reform_tau_timing: pre-mech-injection MTRs, so the wedge is
  #      statutory-only; the Bellman's tau_S keeps the mech income effect).
  #   5b. Combine buckets into per-year r_S vectors; run the tau_eq
  #      recursion (baseline + scenario policies) on the bathtub grid.
  #   6. Per year: sigma conversions (when active), run
  #      kg_dyn_step_recurrence with the conversion inflow, build
  #      cell_table, persist.

  # Finite-parameter guards check the ACTIVE form's pair: selecting 'logs'
  # before eta_tilde / timeable_share_logs are pinned hard-stops here exactly
  # like the historical eta = NA bootstrap for levels.
  if (!form %in% c('levels', 'logs'))
    stop(sprintf("kg_dynamics: form must be 'levels' or 'logs'; got '%s'.", form))
  eta_const  = if (identical(form, 'logs')) 'KG_DYN_DEFAULT_ETA_LOGS' else
                                            'KG_DYN_DEFAULT_ETA'
  frac_const = if (identical(form, 'logs')) 'KG_DYN_TIMEABLE_SHARE_LOGS' else
                                            'KG_DYN_TIMEABLE_SHARE'
  if (!is.finite(eta)) {
    stop(sprintf(paste0('kg_dynamics: %s (the %s-form eta) is not set. Pin it ',
         'via the eta_dial protocol under KG_RESPONSE_FORM=%s ',
         '(other/top_tax/eta_dial/) and paste the calibrated value into the ',
         'constants block at the top of src/sim/kg/constants.R.'),
         eta_const, form, form))
  }
  if (!is.finite(timeable_share)) {
    stop(sprintf(paste0('kg_dynamics: %s (the %s-form timeable share) is not ',
         'set. Pin it against the short-run announcement moment under ',
         'KG_RESPONSE_FORM=%s and paste the calibrated value into the ',
         'constants block at the top of src/sim/kg/constants.R.'),
         frac_const, form, form))
  }
  kg_dyn_validate_timing_params(timeable_share = timeable_share,
                                timing_window  = timing_window,
                                ref_wedge      = ref_wedge)

  years     = scenario_info$years
  state_dir = kg_dyn_state_dir(scenario_info)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  # Step 0: build per-year real-rate discount factors from Macro-Projections
  macro_root = scenario_info$interface_paths$`Macro-Projections`
  if (is.null(macro_root)) {
    stop('kg_dynamics: scenario_info$interface_paths$`Macro-Projections` is ',
         'NULL. The bathtub pre-pass needs the Macro-Projections vintage to ',
         'derive the real-rate discount factor for the Bellman.')
  }
  beta_by_year = kg_dyn_load_beta_series(macro_root, years)

  # Step 1: extended grid (mortality tail 81-119)
  life_ext = kg_dyn_load_life_table_extension(years = years)
  grid_ext = kg_dyn_build_extended_grid(baseline_cells, life_ext, years,
                                        ages_bellman = ages_bellman)
  grid_packed = kg_dyn_pack_baseline_grid(grid_ext, years,
                                          ages_bellman = ages_bellman)

  # (Single pool: no r_exog carve-out, so r_D_B = clip(r_B, 0, 1) is always well
  # defined; cells with measured r_B > 1 clip to r_D_B = 1 and still respond.)

  # Step 2: tau matrices
  tau_B_mat = kg_dyn_pack_tau(baseline_tau, years, ages_bellman = ages_bellman)
  tau_S_mat = kg_dyn_pack_tau(reform_tau,   years, ages_bellman = ages_bellman)
  tau_S_timing_mat = kg_dyn_pack_tau(reform_tau_timing, years,
                                     ages_bellman = ages_bellman)

  # Step 2b: wealth-carry matrix (scenario side only; see reform_carry doc).
  # KG_WEALTH_CARRY_SCALE applies at pack time so every consumer (Bellman,
  # tau_eq, guard slack, state-file carry_h column) sees the scaled h.
  carry_scale = as.numeric(Sys.getenv('KG_WEALTH_CARRY_SCALE', unset = '1'))
  if (!is.finite(carry_scale) || carry_scale < 0) {
    stop('kg_dynamics: KG_WEALTH_CARRY_SCALE must be a finite nonnegative ',
         'number; got "', Sys.getenv('KG_WEALTH_CARRY_SCALE'), '".')
  }
  years_chr_all = as.character(years)
  if (is.null(reform_carry)) {
    h_S_mat = matrix(0, length(ages_bellman), length(years),
                     dimnames = list(as.character(ages_bellman),
                                     years_chr_all))
    tau_w_diag = setNames(
      rep(list(setNames(rep(0, length(ages_bathtub)),
                        as.character(ages_bathtub))), length(years)),
      years_chr_all)
  } else {
    h_S_mat = kg_dyn_pack_tau(lapply(reform_carry, `[[`, 'h'), years,
                              ages_bellman = ages_bellman) * carry_scale
    tau_w_diag = lapply(reform_carry, `[[`, 'tau_w')
  }

  # Step 2c: LEG-PAIRED estate-exposure matrices (see the parameter doc).
  # Same pack as tau (age-80 repeated forward across [81, 119]); the
  # aggregator already clamped each cell to [0, 1]. NULL (unit tests /
  # pre-build callers) = zeros = channel dormant.
  zeros_bellman = function() {
    matrix(0, length(ages_bellman), length(years),
           dimnames = list(as.character(ages_bellman), years_chr_all))
  }
  e_B_mat = if (is.null(baseline_estate)) zeros_bellman() else {
    kg_dyn_pack_tau(baseline_estate, years, ages_bellman = ages_bellman)
  }
  e_S_mat = if (is.null(reform_estate)) zeros_bellman() else {
    kg_dyn_pack_tau(reform_estate, years, ages_bellman = ages_bellman)
  }

  # Step 3: baseline Bellman pass (c_phi = 0 across the whole grid under
  # current-law step-up — every asset gets step-up forgiveness). e_mat is
  # the BASELINE-law exposure e_B: current law has an estate tax, so the
  # baseline death value is F_B = tau_B * (1 - e_B) (leg-paired, unlike h).
  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               eta = eta,
                               beta_by_year = beta_by_year,
                               e_mat = e_B_mat,
                               form = form)

  # Step 4: resolve year-by-year per-asset regime codes, build cell-level
  # regime mix (c_phi, delta_vanish/route/realize), and pack the Bellman
  # c_phi matrix on the extended grid.
  ages_bathtub_chr = as.character(ages_bathtub)
  ages_ext_chr     = as.character(setdiff(ages_bellman, ages_bathtub))
  ages_bellman_chr = as.character(ages_bellman)

  regime_list = vector('list', length(years))
  mix_list    = vector('list', length(years))
  c_phi_S_mat = matrix(0, length(ages_bellman), length(years),
                       dimnames = list(ages_bellman_chr, as.character(years)))

  for (j in seq_along(years)) {
    bt  = baseline_cells[[as.character(years[j])]]
    res = kg_dyn_resolve_year_regime(tax_law, years[j], bt, ages_bathtub)
    mix = res$mix

    regime_list[[j]] = res$regime
    mix_list[[j]]    = mix

    # Pack c_phi onto the extended Bellman grid: bathtub values from the
    # mix, age-80 value repeated forward across [81, 119] (same pattern as
    # tau_mat in kg_dyn_pack_tau).
    c_phi_bt_named = setNames(mix$c_phi, as.character(mix$age))
    c_phi_S_mat[ages_bathtub_chr, j] = c_phi_bt_named[ages_bathtub_chr]
    if (length(ages_ext_chr) > 0) {
      c_phi_S_mat[ages_ext_chr, j] =
        c_phi_bt_named[as.character(KG_DYN_AGE_MAX)]
    }
  }

  pass2 = kg_dyn_solve_bellman(grid_packed, tau_S_mat, c_phi_mat = c_phi_S_mat,
                               kappa_mat = pass1$kappa,
                               eta = eta,
                               beta_by_year = beta_by_year,
                               h_mat = h_S_mat,
                               e_mat = e_S_mat,
                               form = form)

  planned_timing = kg_dyn_build_planned_timing(
    baseline_cells = baseline_cells,
    tau_S_mat      = tau_S_timing_mat,
    years          = years,
    tau_B_mat      = tau_B_mat,
    timeable_share = timeable_share,
    timing_window  = timing_window,
    ref_wedge      = ref_wedge,
    ages_bathtub   = ages_bathtub
  )

  # Save life table and heir distribution for later diagnostic inspection
  saveRDS(life_ext,  file.path(state_dir, 'life_table_extension.rds'))
  saveRDS(heir_dist, file.path(state_dir, 'heir_distribution.rds'))

  A     = build_aging_matrix(ages_bathtub)
  omega = kg_dyn_build_heir_matrix(heir_dist, ages_bathtub)
  bathtub_ages_chr = as.character(ages_bathtub)

  # Step 5b: per-year scenario realization rates (all inputs are
  # stock-independent, so the whole schedule is available before the
  # recurrence runs), then the tau_eq recursion on both policies.
  rate_info_list = lapply(seq_along(years), function(j) {
    kg_dyn_build_scenario_rate(
      baseline_t       = baseline_cells[[as.character(years[j])]],
      r_ordinary_S     = pass2$r_D[bathtub_ages_chr, j],
      R_planned_B_col  = planned_timing$R_planned_B[, j],
      R_planned_S_col  = planned_timing$R_planned_S[, j]
    )
  })

  # Scenario-policy primitives: r_S incl. the retimed planned bucket, reform
  # tau, the scenario regime mix. Baseline-policy primitives: realization at
  # r_B, baseline tau, step-up everywhere (mirroring Pass 1's c_phi = 0).
  prims_S = kg_dyn_tau_eq_primitives(
    baseline_cells = baseline_cells,
    years          = years,
    r_S_by_year    = lapply(rate_info_list, `[[`, 'r_S'),
    tau_bt_mat     = tau_S_mat[bathtub_ages_chr, , drop = FALSE],
    mix_list       = mix_list,
    A              = A,
    omega          = omega,
    ages_bathtub   = ages_bathtub,
    h_bt_mat       = h_S_mat[bathtub_ages_chr, , drop = FALSE],
    e_bt_mat       = e_S_mat[bathtub_ages_chr, , drop = FALSE]
  )
  # prims_B DOES receive e_B (leg-paired, unlike h whose baseline is zero by
  # law): under the step-up baseline convention (mix_list = NULL =>
  # realize = 0) the death-realize term is zero anyway, so e_B is inert in
  # tau_eq_B today -- threaded for correctness if baselines ever carry
  # deemed/carryover regimes, and to keep the leg-pairing rule uniform.
  prims_B = kg_dyn_tau_eq_primitives(
    baseline_cells = baseline_cells,
    years          = years,
    r_S_by_year    = lapply(as.character(years),
                            function(t) baseline_cells[[t]]$r_B),
    tau_bt_mat     = tau_B_mat[bathtub_ages_chr, , drop = FALSE],
    mix_list       = NULL,
    A              = A,
    omega          = omega,
    ages_bathtub   = ages_bathtub,
    e_bt_mat       = e_B_mat[bathtub_ages_chr, , drop = FALSE]
  )

  tau_eq_S_mat = kg_dyn_compute_tau_eq(prims_S, beta_by_year)$tau_eq
  tau_eq_B_mat = kg_dyn_compute_tau_eq(prims_B, beta_by_year)$tau_eq
  kg_dyn_check_tau_eq(tau_eq_S_mat, prims_S$tau, 'S',
                      carry_slack = kg_dyn_carry_slack(prims_S, beta_by_year))
  kg_dyn_check_tau_eq(tau_eq_B_mat, prims_B$tau, 'B')

  # Optional in-pass ground-truth check (DESIGN_LOCK ruling 1): verify the
  # linear recursion cell-by-cell against the finite-difference simulation
  # of the exact recurrence dynamics, on the real grid. Cheap; enabled in
  # smoke/validation runs via SIGMA_TAU_EQ_FDCHECK=1.
  if (identical(Sys.getenv('SIGMA_TAU_EQ_FDCHECK'), '1')) {
    for (j in seq_along(years)) {
      fd_S = kg_dyn_tau_eq_finite_diff(prims_S, beta_by_year, j)
      fd_B = kg_dyn_tau_eq_finite_diff(prims_B, beta_by_year, j)
      err  = max(abs(fd_S - tau_eq_S_mat[, j]), abs(fd_B - tau_eq_B_mat[, j]))
      if (err > 1e-8) {
        stop(sprintf(
          paste0('kg_dynamics: tau_eq recursion vs finite difference ',
                 'mismatch at year %d (max abs err %.3e).'),
          years[j], err))
      }
    }
    message('kg_dynamics: tau_eq finite-difference check passed for all ',
            length(years), ' years.')
  }

  # Step 6: year-by-year bathtub recurrence
  delta = setNames(rep(0, length(ages_bathtub)), as.character(ages_bathtub))

  for (j in seq_along(years)) {
    t  = years[j]
    bt = baseline_cells[[as.character(t)]]
    regime = regime_list[[j]]
    mix    = mix_list[[j]]

    rate_info = rate_info_list[[j]]
    r_S_vec = setNames(rate_info$r_S, bathtub_ages_chr)

    # Sigma conversions for year t (end-of-year injection: computed on
    # year-t wedges, enters delta_next below, participates from t+1).
    sigma_year = NULL
    conv_inflow_vec = NULL
    if (!is.null(sigma_ctx)) {
      sigma_year = sigma_compute_year(
        ctx          = sigma_ctx,
        year         = t,
        tau_eq_B_col = setNames(tau_eq_B_mat[, j], bathtub_ages_chr),
        tau_eq_S_col = setNames(tau_eq_S_mat[, j], bathtub_ages_chr),
        ages_bathtub = ages_bathtub
      )
      conv_inflow_vec = sigma_year$conv_inflow
    }

    step = kg_dyn_step_recurrence(
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = mix$delta_route,
      conv_inflow_vec = conv_inflow_vec
    )

    r_S_named = setNames(step$r_S, bathtub_ages_chr)

    cell_table = kg_dyn_build_cell_table(
      baseline_t   = bt,
      year_idx     = j,
      r_S_vec      = r_S_named,
      delta_prev   = delta,
      tau_B_col    = tau_B_mat[bathtub_ages_chr, j],
      tau_S_col    = tau_S_mat[bathtub_ages_chr, j],
      W_B_col      = pass1$W    [bathtub_ages_chr, j],
      W_S_col      = pass2$W    [bathtub_ages_chr, j],
      MC_B_col     = pass1$MC   [bathtub_ages_chr, j],
      MC_S_col     = pass2$MC   [bathtub_ages_chr, j],
      kappa_col    = pass1$kappa[bathtub_ages_chr, j],
      r_D_B_col    = pass1$r_D  [bathtub_ages_chr, j],
      r_D_S_col    = pass2$r_D  [bathtub_ages_chr, j],
      regime_mix   = mix,
      planned_diag  = list(
        r_S_unclipped = setNames(rate_info$r_S_unclipped, bathtub_ages_chr),
        timing_clipped = setNames(rate_info$timing_clipped, bathtub_ages_chr),
        r_planned_B = setNames(rate_info$r_planned_B, bathtub_ages_chr),
        r_planned_S = setNames(rate_info$r_planned_S, bathtub_ages_chr),
        r_ordinary_B = setNames(rate_info$r_ordinary_B, bathtub_ages_chr),
        r_ordinary_S = setNames(rate_info$r_ordinary_S, bathtub_ages_chr),
        R_planned_B = planned_timing$R_planned_B[, j],
        R_planned_S = planned_timing$R_planned_S[, j],
        planned_timing_shift = planned_timing$planned_timing_shift[, j]
      ),
      death_diag = list(
        decedent_stock =
          setNames(step$decedent_stock, bathtub_ages_chr),
        terminal_char_stock =
          setNames(step$terminal_char_stock, bathtub_ages_chr),
        taxable_death_stock =
          setNames(step$taxable_death_stock, bathtub_ages_chr)
      ),
      corp_debit   = if (!is.null(corp_debit_by_year))
                       corp_debit_by_year[[as.character(t)]] else NULL,
      tau_eq_B_col = setNames(tau_eq_B_mat[, j], bathtub_ages_chr),
      tau_eq_S_col = setNames(tau_eq_S_mat[, j], bathtub_ages_chr),
      conv_inflow_vec = conv_inflow_vec,
      carry_h_col  = setNames(h_S_mat[bathtub_ages_chr, j],
                              bathtub_ages_chr),
      tau_w_col    = tau_w_diag[[as.character(t)]],
      estate_e_B_col = setNames(e_B_mat[bathtub_ages_chr, j],
                                bathtub_ages_chr),
      estate_e_S_col = setNames(e_S_mat[bathtub_ages_chr, j],
                                bathtub_ages_chr),
      ages_bathtub = ages_bathtub
    )

    state = list(regime     = regime,
                 cell_table = cell_table)
    if (!is.null(sigma_year)) {
      state$sigma = sigma_year$tracker
    }
    saveRDS(state, kg_dyn_state_path(scenario_info, t))

    delta = setNames(step$delta_next, bathtub_ages_chr)
  }

  invisible(NULL)
}



#-------------------------------------------------------------------------------
# Mechanical (frozen-realization) pass
#
# The static-side counterpart of the bathtub: same recurrence, same regime
# mix, but realization frozen at baseline (r_S = r_B) and no Bellman (no
# Pass 1/Pass 2, no tau, no planned timing — the behavioral margins are all
# shut off by construction). What remains is the policy's mechanical content:
#   - carryover: routed death stock accumulates in dG and is realized at the
#     heir cell's BASELINE rate (extra_R = r_B * dG); rate_factor = 1 so no
#     rate channel.
#   - deemed: dG stays 0 (nothing routes), deemed_factor = 1, and the
#     applier's decedent term delivers the deemed tax on the baseline gain
#     stock at death.
#   - step-up: delta_route = delta_realize = 0 → the pass is a no-op.
# State files land under static/supplemental/kg_dynamics_mech_state and are
# consumed by the STATIC pass in run_one_year via
# kg_dyn_apply_mech_to_records, so mechanical effects reach static detail,
# static revenue, and the distribution tables. behavioral = conventional −
# static then falls out of the existing outputs.
#-------------------------------------------------------------------------------

kg_dyn_run_frozen_pass = function(scenario_info, tax_law, baseline_cells,
                                   heir_dist,
                                   ages_bathtub = KG_DYN_AGE_MIN:
                                                  KG_DYN_AGE_MAX) {

  years     = scenario_info$years
  state_dir = kg_dyn_mech_state_dir(scenario_info)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  A     = build_aging_matrix(ages_bathtub)
  omega = kg_dyn_build_heir_matrix(heir_dist, ages_bathtub)

  ages_chr = as.character(ages_bathtub)
  delta    = setNames(rep(0, length(ages_bathtub)), ages_chr)

  for (j in seq_along(years)) {
    t   = years[j]
    bt  = baseline_cells[[as.character(t)]]
    res = kg_dyn_resolve_year_regime(tax_law, t, bt, ages_bathtub)
    mix = res$mix

    # Frozen realization: scenario rate is the baseline rate everywhere.
    r_S_vec = setNames(bt$r_B, ages_chr)

    step = kg_dyn_step_recurrence(
      delta_prev      = delta,
      baseline_t      = bt,
      A               = A,
      omega           = omega,
      r_S_vec         = r_S_vec,
      delta_route_vec = mix$delta_route
    )

    # Stock entering year t (same timing convention as the bathtub's
    # cell_table: dG is delta_prev, realized this year at r_S = r_B).
    dG  = as.numeric(delta[as.character(bt$age)])
    mxi = match(bt$age, mix$age)

    cell_table = bt %>%
      mutate(
        age           = as.integer(age),
        dG            = dG,
        r_S           = r_B,
        rate_factor   = 1,
        # pmax(dG, -G_B) kept for symmetry with the bathtub; under frozen
        # realization dG >= 0 always (delta_inh >= 0, survivor flow decays
        # geometrically), so the clamp is inert here.
        extra_R       = r_B * pmax(dG, -G_B),
        deemed_factor = if_else(G_B > 0, pmax(0, (G_B + dG) / G_B), 1),
        delta_vanish  = mix$delta_vanish [mxi],
        delta_route   = mix$delta_route  [mxi],
        delta_realize = mix$delta_realize[mxi],
        c_phi         = mix$c_phi        [mxi],
        decedent_stock      = step$decedent_stock,
        terminal_char_stock = step$terminal_char_stock,
        taxable_death_stock = step$taxable_death_stock,
        delta_inh           = step$delta_inh
      ) %>%
      select(age, G_B, R_B, r_B, r_S, m, mG_record, mR_record, dG,
             p_char, p_char_extensive, p_char_intensive,
             G_B_equities, G_B_pass_throughs, G_B_primary_home,
             G_B_other_home, G_B_re_fund, G_B_primary_above_cap,
             delta_vanish, delta_route, delta_realize, c_phi,
             decedent_stock, terminal_char_stock, taxable_death_stock,
             delta_inh, rate_factor, extra_R, deemed_factor)

    saveRDS(list(regime     = res$regime,
                 cell_table = cell_table),
            kg_dyn_mech_state_path(scenario_info, t))

    delta = setNames(step$delta_next, ages_chr)
  }

  invisible(NULL)
}




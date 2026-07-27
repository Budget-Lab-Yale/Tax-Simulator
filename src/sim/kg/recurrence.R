#-------------------------------------------------------------------------------
# recurrence.R
#
# Contains functions to step the stock of unrealized gains forward, and the
# drivers that run the pass
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Stepping the stock forward
#-------------------------------------------------------------------------------

kg_dyn_cell_m_eff = function(baseline_t) {

  # Averages mortality within a cell, weighting by whatever the change in gains
  # is assumed to follow. Weighting by taxpayer instead overstates the death flow
  # by a factor of about 2.7 in this data, because within a cell mortality and
  # holdings are strongly negatively correlated. Spreading the change over records
  # in proportion to their holdings and then summing is exact rather than
  # approximate.
  #
  # The kg setting dg_allocation chooses the weight: holdings, or realizations
  # falling back to holdings where a cell realizes nothing.
  #
  # Shared with the tau_eq machinery, so that the two agree on what mortality the
  # change in gains faces.
  #
  # Returns: numeric vector of mortality rates by cell.

  m_eff_G = if_else(baseline_t$G_B > 0,
                    baseline_t$mG_record / baseline_t$G_B, baseline_t$m)
  m_eff_R = if_else(baseline_t$R_B > 0,
                    baseline_t$mR_record / baseline_t$R_B, m_eff_G)

  dg_allocation = as.character(kg_setting('dg_allocation'))
  m_eff = switch(dg_allocation,
                 G = m_eff_G,
                 R = m_eff_R,
                 stop("Unknown kg.dg_allocation rule: ", dg_allocation))
  pmin(pmax(m_eff, 0), 1)
}



kg_dyn_step_recurrence = function(delta_prev, baseline_t, A, omega,
                                  r_S_vec, delta_route_vec,
                                  conv_inflow_vec = NULL) {

  # Steps the change in the stock of unrealized gains forward one year.
  #
  # Three things happen. Survivors keep the share they do not realize, and age.
  # Gains held by decedents leave the stock, and under carryover the routed share
  # of them arrives in the heirs' cells. Compensation converted into equity, if
  # that module is running, enters at year end.
  #
  # Parameters:
  #   - delta_prev (dbl[])       : last year's change in the stock, by age
  #   - baseline_t (df)          : the year's baseline cells
  #   - A, omega (matrix)        : the aging and heir-routing operators
  #   - r_S_vec (dbl[])          : scenario realization rate by age
  #   - delta_route_vec (dbl[])  : share of the dying stock routed to heirs
  #   - conv_inflow_vec (dbl[])  : converted compensation entering at year end;
  #                                NULL for none
  #
  # Note that the age-80 cell pools everyone 80 and older behind one average
  # mortality rate, refreshed each year from Tax-Data. That smooths over the
  # difference between someone who has been in the pool fifteen years and someone
  # who just turned 80. The effect is small, but worth remembering for a reform
  # that shifts the age mix within the pool.
  #
  # Returns: list of the new change in the stock and the flows behind it.

  G_B       = baseline_t$G_B
  r_B       = baseline_t$r_B
  p_char    = pmin(pmax(baseline_t$p_char, 0), 1)

  m_eff = kg_dyn_cell_m_eff(baseline_t)

  r_S = pmin(pmax(r_S_vec, 0), 1)

  # Survivors
  inner      = (1 - r_S) * delta_prev + G_B * (r_B - r_S)
  contrib_a  = (1 - m_eff) * inner
  delta_surv = as.numeric(crossprod(A, contrib_a))

  # Inheritances. The routed share is per cell, so a cell whose assets all get a
  # step-up contributes nothing here even where neighboring cells do.
  decedent_stock      = m_eff * (G_B + delta_prev)
  terminal_char_stock = p_char * decedent_stock
  taxable_death_stock = (1 - p_char) * decedent_stock
  if (any(delta_route_vec > 0)) {
    delta_inh = as.numeric(crossprod(omega,
                                     delta_route_vec * taxable_death_stock))
  } else {
    delta_inh = rep(0, length(delta_prev))
  }

  # Converted compensation, entering at year end so that it starts realizing and
  # dying next year, as inheritances do.
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
# Mixing the treatment of gains at death across asset classes
#-------------------------------------------------------------------------------

kg_dyn_build_regime_mix = function(regime_codes, theta, baseline_t,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Averages the treatment of gains at death across asset classes within a cell,
  # weighting each class by its share of the cell's gains. That gives, per cell,
  # the share of gains that is forgiven, the share routed to heirs, and the share
  # taxed at death.
  #
  # It also gives the share of the cell's gains the holder expects to be taxed if
  # they die, which is what enters the realization choice. Gains taxed at death
  # count in full; gains routed to heirs count only in proportion to how much the
  # holder cares about the heir's tax bill.
  #
  # Parameters:
  #   - regime_codes (list) : treatment code per asset class
  #   - theta (dbl)         : weight the holder puts on the heir's tax bill
  #   - baseline_t (df)      : the year's cells, with gains by asset class
  #
  # Returns: tibble by age of the forgiven, routed and taxed shares, and the share
  #          the holder internalizes.

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

    # On a primary home, only the gain above the §121 exclusion is at stake.
    # Whether the gain is taxed at death or routed to the heir, the exclusion is
    # modeled as a step-up in basis up to the cap, so the portion below it never
    # enters either stock. Where the whole gain is forgiven anyway the exclusion
    # does not matter. Every other asset class puts its full share at stake.
    live_share = if (k == 'primary_home') share_primary_above_cap else share[[k]]

    delta_vanish  = delta_vanish  + share[[k]] * tr$vanish
    delta_route   = delta_route   + live_share * tr$route
    delta_realize = delta_realize + live_share * tr$realize

    # The holder counts the routed gain only in part, and the gain taxed at death
    # in full.
    c_phi = c_phi + theta * tr$route * live_share
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

  # Assembles the three quantities the record allocation needs, along with the
  # diagnostic columns the summary reads.
  #
  # rate_factor is the ratio of the scenario realization rate to the baseline one.
  # extra_R is the realized part of the change in the stock of gains. deemed_factor
  # scales gains at death to the changed stock.
  #
  # Under a corporate reform, the equity markdown reduces the change in the stock
  # before it is realized. That debit is recomputed from the current markdown each
  # year rather than carried through the recurrence, and it deliberately does not
  # enter deemed_factor: gains at death already see the markdown through the asset
  # values the corporate step scaled on the record.
  #
  # Returns: tibble of one row per cell (df).

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

  # Diagnostic columns, zero when the module that fills them is not running.
  if (is.null(tau_eq_B_col)) {
    tau_eq_B_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }
  if (is.null(tau_eq_S_col)) {
    tau_eq_S_col = setNames(rep(0, length(ages_chr)), ages_chr)
  }
  if (is.null(conv_inflow_vec)) {
    conv_inflow_vec = setNames(rep(0, length(ages_chr)), ages_chr)
  }

  # Wealth tax columns, zero for every run without a wealth tax. carry_h is the
  # carrying cost the realization choice actually used; tau_w is the plain average
  # wealth rate, for diagnostics only.
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
           # Cap the drawdown at the cell's whole stock of gains. Under a
           # permanent rate increase the change in the stock can go negative
           # enough that realizing at the scenario rate would take out more than
           # the cell holds. The clamp below on gains at death does the same.
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
                                    form  = kg_dyn_response_form(),
                                    eta   = kg_dyn_active_eta(form),
                                    timeable_share = kg_dyn_active_timeable_share(form),
                                    timing_window = kg_setting('timing_window'),
                                    ref_wedge     = kg_setting('timing_ref_wedge'),
                                    corp_debit_by_year = NULL,
                                    sigma_ctx = NULL,
                                    reform_carry = NULL,
                                    baseline_estate = NULL,
                                    reform_estate = NULL,
                                    ages_bathtub = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX,
                                    ages_bellman = KG_DYN_AGE_MIN:
                                                    KG_DYN_AGE_MAX_BELLMAN) {

  # Runs the whole pass for one scenario and writes one state file per year, which
  # is what the behavior module then reads to adjust records.
  #
  # The order of work is:
  #
  #   1. build the baseline cells, over the age grid extended past 80
  #   2. assemble the marginal rate matrices for both legs
  #   3. solve the baseline realization choice, recovering the benefit of
  #      realizing
  #   4. resolve each year's treatment of gains at death and solve the scenario
  #   5. build the retiming schedule, combine it with the long-run response into a
  #      realization rate per year, and price a dollar of gain for both legs
  #   6. for each year, compute any conversions, step the stock forward, build the
  #      cell table and write it out
  #
  # Parameters worth explaining:
  #
  #   - corp_debit_by_year : the corporate equity markdown's debit to the stock of
  #                          gains. Enters the realized stock only. The recurrence
  #                          itself runs on the clean change in gains: routing the
  #                          debit through it would compound it year over year and
  #                          double-count the markdown heirs already see, which
  #                          next year's recomputed debit covers anyway.
  #   - reform_carry       : the wealth tax cost of deferring, by year. Scaled by
  #                          the kg.wealth_carry_scale setting, which is an
  #                          uncalibrated sensitivity knob, then used in the
  #                          scenario solve and the scenario pricing. The baseline
  #                          never receives it, since a baseline has no wealth tax.
  #   - baseline_estate,
  #     reform_estate      : the estate tax offset on gains taxed at death, one per
  #                          leg. These must stay separate. Sharing one would
  #                          silently kill estate-only reforms, where the gains
  #                          rate is unchanged and the estate offset is the only
  #                          thing that moves. Note that unlike the wealth tax cost
  #                          this is nonzero in the baseline, since current law has
  #                          an estate tax, which means it touches the baseline
  #                          solve and so the elasticity anchor. Re-check the
  #                          calibration when this changes.
  #   - sigma_ctx          : context for the income conversion module, when the
  #                          scenario runs it. Conversions are computed per record,
  #                          aggregated to cells, and added to the stock at year
  #                          end. Only the cell totals are written out; the module
  #                          recomputes the record-level conversions itself.
  #
  # The baseline solve assumes step-up on everything. The baseline estate offset is
  # consistent with that, but if a baseline ever carries carryover or deemed
  # realization, revisit how the baseline side is built here.
  #
  # Returns: invisibly NULL.

  # Check the active form's parameters are pinned. Choosing a form whose pair has
  # not been calibrated stops here rather than simulating an uncalibrated model.
  if (!form %in% c('levels', 'logs'))
    stop(sprintf("kg_dynamics: form must be 'levels' or 'logs'; got '%s'.", form))
  eta_const  = if (identical(form, 'logs')) 'kg.eta_logs' else 'kg.eta'
  frac_const = if (identical(form, 'logs')) 'kg.timeable_share_logs' else
                                            'kg.timeable_share'
  if (!is.finite(eta)) {
    stop(sprintf(paste0('kg_dynamics: %s (the %s-form eta) is not set. Pin it ',
         'via the eta_dial protocol under kg.response_form=%s ',
         '(other/top_tax/eta_dial/) and record the calibrated value in ',
         'config/calibrations/kg/bathtub.yaml.'),
         eta_const, form, form))
  }
  if (!is.finite(timeable_share)) {
    stop(sprintf(paste0('kg_dynamics: %s (the %s-form timeable share) is not ',
         'set. Pin it against the short-run announcement moment under ',
         'kg.response_form=%s and record the calibrated value in ',
         'config/calibrations/kg/bathtub.yaml.'),
         frac_const, form, form))
  }
  kg_dyn_validate_timing_params(timeable_share = timeable_share,
                                timing_window  = timing_window,
                                ref_wedge      = ref_wedge)

  years     = scenario_info$years
  state_dir = kg_dyn_state_dir(scenario_info)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

  # Build the discount factors, from real interest rates
  macro_root = scenario_info$interface_paths$`Macro-Projections`
  if (is.null(macro_root)) {
    stop('kg_dynamics: scenario_info$interface_paths$`Macro-Projections` is ',
         'NULL. The pre-pass needs it to derive the real interest rate the ',
         'realization choice discounts at.')
  }
  beta_by_year = kg_dyn_load_beta_series(macro_root, years)

  # Extend the age grid past 80 with life table mortality
  life_ext = kg_dyn_load_life_table_extension(years = years)
  grid_ext = kg_dyn_build_extended_grid(baseline_cells, life_ext, years,
                                        ages_bellman = ages_bellman)
  grid_packed = kg_dyn_pack_baseline_grid(grid_ext, years,
                                          ages_bellman = ages_bellman)

  # Marginal rates on gains, by leg
  tau_B_mat = kg_dyn_pack_tau(baseline_tau, years, ages_bellman = ages_bellman)
  tau_S_mat = kg_dyn_pack_tau(reform_tau,   years, ages_bellman = ages_bellman)
  tau_S_timing_mat = kg_dyn_pack_tau(reform_tau_timing, years,
                                     ages_bellman = ages_bellman)

  # The wealth tax cost of deferring, on the scenario side only. Scale it here,
  # so that everything downstream sees the same scaled value.
  carry_scale = as.numeric(kg_setting('wealth_carry_scale'))
  if (!is.finite(carry_scale) || carry_scale < 0) {
    stop('kg_dynamics: kg.wealth_carry_scale must be a finite ',
         'nonnegative number; got "', carry_scale, '".')
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

  # The estate tax offset, one matrix per leg. Absent for unit tests and for
  # callers that predate the channel, which leaves it dormant.
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

  # Solve the baseline. Nothing is taxed at death, since current law gives every
  # asset a step-up in basis, but the baseline estate offset still applies.
  pass1 = kg_dyn_solve_bellman(grid_packed, tau_B_mat, c_phi_mat = 0,
                               eta = eta,
                               beta_by_year = beta_by_year,
                               e_mat = e_B_mat,
                               form = form)

  # Resolve each year's treatment of gains at death and average it to cells
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

    # Carry the age-80 value forward over the extended ages, as the rates are
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




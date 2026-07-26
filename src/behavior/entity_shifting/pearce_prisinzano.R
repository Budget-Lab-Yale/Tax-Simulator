do_entity_shifting = function(tax_units, baseline_mtrs, static_mtrs,
                              scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Models business income shifting across entity type as a function of
  # the tax differential between corporate and pass-through taxation. Based on
  # Pearce and Prisinzano (2018) working paper. Assumptions are detailed below.
  #
  # Hardened 2026-07-08 per other/top_tax/DESIGN_LOCK.md ruling 5 (before the
  # sigma validation runs): SECA companion co-scaling (evasion-module
  # pattern), order/required-MTR guards, and a conservation diagnostic. The
  # dividends=gains offset assumption is retained (verified gap, no ruling
  # to change it).
  #
  # Role in the top-tax decomposition: this module is the ENTITY-FORM leg —
  # business income moving between the corporate base and the pass-through
  # base (a conservation flow, offset in corporate receipts + shareholder
  # distributions). It is distinct from the sigma conversion leg (payment
  # FORM of labor compensation moving into the kg gain state) and runs AFTER
  # it in the pinned order kg_dynamics -> conversion/sigma ->
  # entity_shifting -> evasion, so the two never move the same dollar twice.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with calculated variables
  #   - baseline_mtrs (df)   : year-id indexed tibble of MTRs under the
  #                            baseline; must carry mtr_kg_lt and
  #                            mtr_part_active
  #   - static_mtrs (df)     : year-id indexed tibble of MTRs under the
  #                            static counterfactual; same required columns
  #   - scenario_info (list) : get_scenario_info() object
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: tax units dataframe, with updated values for active partnership
  #          income (SECA companions co-scaled) and long-term capital gains,
  #          and with the implied corporate tax change attributable to
  #          shifting.
  #----------------------------------------------------------------------------

  modules = scenario_info$behavior_modules %||% character()

  # The retained-earnings leg uses the kg bathtub's expected-PV tax price
  # whenever that machinery is active.  The explicit legacy module preserves
  # the former Pearce--Prisinzano 25%-of-statutory-rate approximation for
  # A/B comparisons.  Entity-only scenarios retain that approximation because
  # no tau_eq state exists to price their deferred gain.
  legacy_module = any(basename(modules) == 'pearce_prisinzano_legacy.R')
  uses_kg       = scenario_uses_kg_dynamics(scenario_info)
  use_tau_eq    = uses_kg && !legacy_module

  # The pinned-order guard that used to sit here is gone: the loader puts the
  # families in that order before anything runs (src/sim/behavior.R).

  # Required-MTR guard: fail loudly rather than silently skipping the
  # response (which would mislabel a static score as conventional).
  required = c('mtr_kg_lt', 'mtr_part_active')
  missing  = if (is.null(static_mtrs) || is.null(baseline_mtrs)) required else
             setdiff(required, intersect(names(static_mtrs),
                                         names(baseline_mtrs)))
  if (length(missing) > 0) {
    stop('do_entity_shifting(): requires registered MTRs for kg_lt and ',
         'part_active (mtr_vars = "kg_lt part_active", mtr_types = ',
         '"nextdollar nextdollar"). The runscript for scenario "',
         scenario_info$ID, '" is missing: ',
         paste(missing, collapse = ', '), '.')
  }

  # --- Parameters -------------------------------------------------------------
  # Here, in the module, because this module is their only reader. A variant is a
  # copy of this file with different numbers, listed by a different behavior
  # alternative -- the same rule every other behavior module follows.
  #
  # The semi-elasticity of the pass-through share of business income with respect
  # to the corporate-versus-pass-through tax differential. Pearce and Prisinzano
  # (2018), Table IV.B preferred results, evaluated at pass-through's 0.6 share of
  # business income.
  e = 0.3788 / 0.6

  # Share of corporate earnings assumed paid out currently rather than retained.
  # A judgment call with no recorded derivation. That leg's tax is still proxied
  # by mtr_kg_lt -- the historical module's dividends-equal-gains simplification,
  # carried forward. The retained share is priced properly by tau_eq whenever the
  # bathtub is running: expected present-value tax per dollar newly entering the
  # unrealized-gain state, by age, year and policy/death regime.
  alpha = 0.45

  # The superseded retained-earnings deferral proxy: 25 percent of the statutory
  # rate. tau_eq replaced it. It survives for the two cases where tau_eq does not
  # exist or is deliberately not used -- an entity-only scenario with no bathtub,
  # and the explicit legacy comparison module that exists to measure what the
  # tau_eq change did.
  beta_legacy = 0.25

  tax_units_with_tau_eq = if (use_tau_eq) {
    if (!'age_cohort' %in% names(tax_units)) {
      stop('do_entity_shifting(): kg_dynamics is active but age_cohort is ',
           'missing. Entity shifting must run after kg_dynamics.')
    }
    state_path = kg_dyn_state_path(scenario_info, tax_units$year[1])
    if (!file.exists(state_path)) {
      stop('do_entity_shifting(): missing kg bathtub state file at ', state_path,
           '. tau_eq pricing requires the kg pre-pass before the conventional ',
           'pass.')
    }
    cell_table = readRDS(state_path)$cell_table
    required_tau = c('age', 'tau_eq_B', 'tau_eq_S')
    missing_tau  = setdiff(required_tau, names(cell_table))
    if (length(missing_tau) > 0) {
      stop('do_entity_shifting(): kg state lacks required tau_eq columns: ',
           paste(missing_tau, collapse = ', '), '.')
    }
    tau_eq_by_age = cell_table %>%
      transmute(age_cohort = age,
                tau_eq_baseline = tau_eq_B,
                tau_eq_policy = tau_eq_S)
    if (any(!is.finite(tau_eq_by_age$tau_eq_baseline)) ||
        any(!is.finite(tau_eq_by_age$tau_eq_policy)) ||
        any(tau_eq_by_age$tau_eq_baseline < 0) ||
        any(tau_eq_by_age$tau_eq_policy < 0)) {
      stop('do_entity_shifting(): kg tau_eq values must be finite and ',
           'nonnegative.')
    }
    message('do_entity_shifting(): pricing retained corporate earnings with ',
            'kg tau_eq (age/year/policy-specific expected-PV tax).')
    tax_units %>% left_join(tau_eq_by_age, by = 'age_cohort')
  } else {
    message('do_entity_shifting(): using legacy retained-earnings deferral ',
            'proxy (beta = ', beta_legacy,
            if (legacy_module) '; explicit legacy module).' else
              '; kg_dynamics not active).')
    tax_units %>%
      mutate(tau_eq_baseline = NA_real_, tau_eq_policy = NA_real_)
  }

  # Read baseline tax law and extract corporate rate
  corp.rate_baseline = globals$baseline_root %>%
    file.path('baseline/static/supplemental/tax_law.csv') %>%
    read_csv(show_col_types = F) %>%
    select(year, corp.rate_baseline = corp.rate) %>%
    distinct()


  new_values = tax_units_with_tau_eq %>%

    # Join MTRs
    left_join(baseline_mtrs %>%
                rename_with(.cols = -c(id, year),
                            .fn   = ~ paste0(., '_baseline')),
              by = c('id', 'year')) %>%
    left_join(static_mtrs, by = c('id', 'year')) %>%

    # Join baseline corporate rate
    left_join(corp.rate_baseline, by = 'year') %>%

    mutate(

      # Calculate change in tax differential, "tau-tau". First, the tax rate
      # on corporate distributions. For computational convenience we assume the
      # tax rate faced by dividends and capital gains is the same; this
      # formulation is not robust to a pre-Bush-style rate differential. The
      # retained share is tau_eq directly (rather than tau_eq/mtr_kg_lt),
      # avoiding a zero-rate division and keeping the two tax-price objects
      # explicit.
      tau_dist_policy = if (use_tau_eq) {
        alpha * mtr_kg_lt + (1 - alpha) * tau_eq_policy
      } else {
        mtr_kg_lt * (alpha + (1 - alpha) * beta_legacy)
      },
      tau_dist_baseline = if (use_tau_eq) {
        alpha * mtr_kg_lt_baseline + (1 - alpha) * tau_eq_baseline
      } else {
        mtr_kg_lt_baseline * (alpha + (1 - alpha) * beta_legacy)
      },

      # The current-year kg_lt adjustment is a reporting/bookkeeping proxy
      # for the shareholder-side tax price above.  Convert tau_eq back to an
      # equivalent fraction of this record's kg MTR only for that adjustment;
      # the behavioral wedge itself uses tau_eq directly.  At a zero or
      # missing record MTR there is no well-defined conversion, so retain the
      # historical proxy (it has no current tax effect at a zero MTR).
      retained_equiv_factor = if (use_tau_eq) {
        if_else(!is.na(mtr_kg_lt) & abs(mtr_kg_lt) > 1e-9,
                pmin(1, pmax(0, tau_eq_policy / mtr_kg_lt)),
                beta_legacy)
      } else {
        beta_legacy
      },
      shareholder_offset_factor = alpha + (1 - alpha) * retained_equiv_factor,

      # Next, the net corporate rate
      tau_corp_policy   = corp.rate          + (1 - corp.rate)          * tau_dist_policy,
      tau_corp_baseline = corp.rate_baseline + (1 - corp.rate_baseline) * tau_dist_baseline,

      # Finally, the pass-through rate. Assumed to be equal for all types of
      # pass-through income, another computationally minded assumption
      tau_pass_policy   = mtr_part_active,
      tau_pass_baseline = mtr_part_active_baseline,

      # Calculate change in tax differential
      delta_tau_tau = (tau_corp_policy - tau_pass_policy) - (tau_corp_baseline - tau_pass_baseline),

      # Calculate implied shifting of business income into pass-through.
      # NA-safe: records with no measured MTR on either side do not respond.
      percent_shifted = if_else(is.na(delta_tau_tau), 0, e * delta_tau_tau),
      amount_shifted  = pmax(0, part_active + part_passive - part_active_loss -
                                part_passive_loss - part_179 + scorp_active +
                                scorp_passive - scorp_active_loss - scorp_passive_loss -
                                scorp_179 + sole_prop) * percent_shifted,

      # Adjust pass-through income, co-scaling the partnership SECA
      # earner-split companions (part_se1/2) with their parent aggregate so
      # the payroll frame stays consistent (evasion-module convention: one
      # bounded shared percent scales all legs in the group). The factor is
      # 1 + percent_shifted, NOT (part_active + amount_shifted)/part_active:
      # amount_shifted is a percent of the record's ENTIRE pass-through basis
      # (partnership + scorp + sole_prop), so dividing it by part_active alone
      # is degenerate when part_active is small relative to the other legs
      # (2026-07-09 top_tax factorial: per-record factors in the hundreds,
      # se base +/- $13-26T, payroll head +$500B/yr on a +5pp ord change).
      # When part_active <= 0 pre-shift there is no companion basis to
      # scale; the companions are left untouched (accepted, small).
      .part_factor = if_else(!is.na(part_active) & part_active > 0 &
                               amount_shifted != 0,
                             1 + percent_shifted,
                             1),
      part_active = part_active + amount_shifted,
      part_se1    = part_se1 * .part_factor,
      part_se2    = part_se2 * .part_factor,

      # Adjust corporate distributions. Shareholders receive only the
      # after-corporate-tax portion of shifted income, per equation (1) of the
      # paper. Assume any reduction operates through capital gain, to prevent
      # dividends from being negative. A situation where the revenue effect is
      # correct but the micro-level output cannot be interpreted literally, not
      # unlike standard capital gains elasticity modeling
      kg_offset = amount_shifted * (1 - corp.rate) * shareholder_offset_factor,
      kg_lt     = kg_lt - kg_offset,

      # Calculate implied change in corporate tax revenue
      corp_tax_change = -amount_shifted * corp.rate

    ) %>%
    select(part_active, part_se1, part_se2, kg_lt, corp_tax_change,
           amount_shifted, kg_offset, weight)

  # Conservation diagnostic (DESIGN_LOCK ruling 5): pass-through dollars in
  # must equal corporate-base dollars out, which land as the corporate
  # revenue change (rate x amount) plus the shareholder distribution offset
  # (after-corporate-tax portion, expressed through kg_lt) plus retained
  # after-tax earnings not distributed this year.
  year_t   = tax_units$year[1]
  pt_in    = sum(new_values$weight * new_values$amount_shifted)
  kg_out   = sum(new_values$weight * new_values$kg_offset)
  corp_chg = sum(new_values$weight * new_values$corp_tax_change)
  message(sprintf(
    paste0('do_entity_shifting(): year %d conservation: PT dollars in = ',
           '$%.3fB; corporate revenue change = $%.3fB; shareholder ',
           'distribution offset (via kg_lt) = $%.3fB; retained/undistributed ',
           'residual = $%.3fB.'),
    year_t, pt_in / 1e9, corp_chg / 1e9, kg_out / 1e9,
    (pt_in + corp_chg - kg_out) / 1e9))

  # Replace old values with new and return
  tax_units %>%
    select(-part_active, -part_se1, -part_se2, -kg_lt) %>%
    bind_cols(new_values %>%
                select(part_active, part_se1, part_se2, kg_lt,
                       corp_tax_change)) %>%
    return()
}

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

  # Order guard: entity shifting must run after kg_dynamics and
  # conversion/sigma (when present) and before evasion (when present) —
  # the pinned order that prevents double-moving the same dollar.
  fam_pos = function(prefix) {
    i = which(startsWith(modules, prefix))
    if (length(i) == 0) NA_integer_ else min(i)
  }
  order_req = c(fam_pos('kg_dynamics/'), fam_pos('conversion/'),
                fam_pos('entity_shifting/'), fam_pos('evasion/'))
  present = order_req[!is.na(order_req)]
  if (is.unsorted(present, strictly = TRUE)) {
    stop('do_entity_shifting(): behavior modules must run in the pinned ',
         'order kg_dynamics -> conversion/sigma -> entity_shifting -> ',
         'evasion. Scenario "', scenario_info$ID, '" has: ',
         paste(modules, collapse = ' '), '.')
  }

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

  # Set semi elasticity, starting with Pearce and Prisinzano's Table IV.B preferred
  # results, evaluated at pass-through's share of business income
  e = 0.3788 / 0.6

  # Set other parameters -- assuming 45% of distributions are paid as
  # dividends and the ETR on gains, reflecting the "benefit of deferral", is
  # 25% of the actual rate as per the paper. A future version of this module
  # should more realistically model the micro-level behavior that gives rise
  # to the 25% benefit-of-deferral value
  alpha = 0.45
  beta  = 0.25

  # Read baseline tax law and extract corporate rate
  corp.rate_baseline = globals$baseline_root %>%
    file.path('baseline/static/supplemental/tax_law.csv') %>%
    read_csv(show_col_types = F) %>%
    select(year, corp.rate_baseline = corp.rate) %>%
    distinct()


  new_values = tax_units %>%

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
      # formulation is not robust to a pre-Bush-style rate differential
      tau_dist_policy   = mtr_kg_lt          * (alpha + (1 - alpha) * beta),
      tau_dist_baseline = mtr_kg_lt_baseline * (alpha + (1 - alpha) * beta),

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
      kg_offset = amount_shifted * (1 - corp.rate) * (alpha + (1 - alpha) * beta),
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

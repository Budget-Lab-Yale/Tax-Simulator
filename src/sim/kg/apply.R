#-------------------------------------------------------------------------------
# apply.R
#
# Contains functions to allocate cell-level results to records, and to aggregate
# records up to cells
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Spreading the cell results over records changes kg_lt three ways:
#
#   rate      records with gains scale by the ratio of the scenario realization
#             rate to the baseline one
#   lock-in   the cell's realized change in the stock of gains is spread over
#             records in proportion to realizations, or to holdings where the
#             cell realized nothing
#   at death  gains deemed realized at death are summed by asset class, netting
#             the §121 exclusion off a primary home, and scaled to the changed
#             stock of gains
#
# Gains deemed realized at death do not go into kg_lt. See the note in the
# function on why.
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, realize_by_asset,
                                   regime_codes = NULL,
                                   death_excl_married = NULL) {

  # Take only the columns needed, by position rather than by joining. The cell
  # table carries about 35 diagnostic columns and there are 220,000 records per
  # scenario-year, so a join here is expensive for no reason.
  idx           = match(tax_units$age_cohort, cell_table$age)
  rate_factor   = cell_table$rate_factor  [idx]
  extra_R       = cell_table$extra_R      [idx]
  deemed_factor = cell_table$deemed_factor[idx]
  R_B           = cell_table$R_B          [idx]
  G_B           = cell_table$G_B          [idx]
  p_char        = cell_table$p_char       [idx]

  missing = setdiff(KG_DYN_ASSET_CLASSES, names(realize_by_asset))
  if (length(missing) > 0) {
    stop('kg_dyn_apply_to_records: realize_by_asset missing asset classes: ',
         paste(missing, collapse = ', '))
  }

  # Scale down gains realized at death for noncompliance and valuation games.
  # This is a measurement parameter, not tax law, and it does not affect what the
  # taxpayer takes into account when deciding whether to realize during life.
  deemed_avoidance = kg_setting('deemed_avoidance')
  if (!is.finite(deemed_avoidance) ||
      deemed_avoidance < 0 || deemed_avoidance > 1) {
    stop(sprintf(
      'kg_dyn_apply_to_records: kg.deemed_avoidance must be in [0, 1]; got %s.',
      format(deemed_avoidance)))
  }
  avoidance_keep = 1 - deemed_avoidance

  # The haircut discounts asset value, leaving basis alone, so the discounted
  # gain is the marked-down value less basis, floored at zero. Do this per record
  # rather than on a cell average: because of the floor, an average basis-to-value
  # ratio above the haircut would zero out a whole asset class, even though the
  # records with the lowest basis still have a taxable gain. At no haircut this
  # returns the full gain. A primary home nets its §121 exclusion afterward.
  needed = c(KG_DYN_ASSET_VALUE_COLS, KG_DYN_ASSET_BASIS_COLS)
  miss = setdiff(needed, names(tax_units))
  if (length(miss) > 0) {
    stop('kg_dyn_apply_to_records: tax_units missing value/basis columns: ',
         paste(miss, collapse = ', '))
  }
  disc_gain = function(cls) {
    v = replace_na(as.numeric(tax_units[[paste0('value.', cls)]]), 0)
    b = replace_na(as.numeric(tax_units[[paste0('basis.', cls)]]), 0)
    pmax(0, avoidance_keep * v - b)
  }
  sec121 = replace_na(as.numeric(tax_units$`pref.kg_sec121_excl`), 0)
  g_primary_above_cap = pmax(0, disc_gain('primary_home') - sec121)

  stakes = cbind(equities      = disc_gain('equities'),
                 pass_throughs = disc_gain('pass_throughs'),
                 primary_home  = g_primary_above_cap,
                 other_home    = disc_gain('other_home'),
                 re_fund       = disc_gain('re_fund'))
  realize_vec = unlist(realize_by_asset[colnames(stakes)])

  # The death-gain exclusion removes the first dollars of the decedent's gain
  # pro-rata across the classes whose regime is carryover or deemed
  # realization, after the haircut and §121, so only the taxed share of the
  # excess lands on the final return. A single filer's own and married
  # amounts give two branches, which run_one_year() prices separately and
  # blends by the widowhood probability.
  excl_own = if ('pref.kg_death_gain_excl' %in% names(tax_units)) {
    replace_na(as.numeric(tax_units$`pref.kg_death_gain_excl`), 0)
  } else {
    rep(0, nrow(tax_units))
  }
  excl_active = any(excl_own > 0) ||
    (!is.null(death_excl_married) &&
       isTRUE(as.numeric(death_excl_married) > 0))

  if (excl_active) {
    if (is.null(regime_codes) || is.null(death_excl_married) ||
        !is.finite(as.numeric(death_excl_married))) {
      stop('kg_dyn_apply_to_records: the death-gain exclusion is active but ',
           'regime_codes or death_excl_married was not supplied.')
    }
    eligible = sapply(colnames(stakes),
                      function(k) as.numeric(regime_codes[[k]]) %in% c(1, 2))
    excl_2x  = if_else(tax_units$filing_status %in% c(1, 4),
                       as.numeric(death_excl_married), excl_own)
    pool = rowSums(stakes[, eligible, drop = FALSE])
    f_x  = ifelse(pool > 0, pmax(0, pool - excl_own) / pool, 0)
    f_2x = ifelse(pool > 0, pmax(0, pool - excl_2x) / pool, 0)
  } else {
    f_x = f_2x = rep(1, nrow(tax_units))
  }

  taxed_stake         = stakes %*% realize_vec
  deemed_per_record_x  = as.numeric(taxed_stake) * f_x
  deemed_per_record_2x = as.numeric(taxed_stake) * f_2x

  # Weight on the holdings share: 0 spreads by realizations, 1 by holdings, and
  # anything between blends the two.
  applier_allocation = as.character(kg_setting('applier_allocation'))
  alpha_G = switch(applier_allocation,
                   R = 0,
                   G = 1,
                   suppressWarnings(as.numeric(applier_allocation)))
  if (!is.finite(alpha_G) || alpha_G < 0 || alpha_G > 1) {
    stop("kg_dyn_apply_to_records: kg.applier_allocation must be 'R', ",
         "'G', or a number in [0, 1]; got '", applier_allocation, "'.")
  }

  tax_units %>%
    mutate(
      # Each share sums to 1 within a cell, falling back to the other where a
      # cell has no realizations or holds no gains, so any blend of them does too.
      share_R = case_when(
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        G_B > 0 ~ G_unit         / G_B,
        TRUE    ~ 0
      ),
      share_G = case_when(
        G_B > 0 ~ G_unit         / G_B,
        R_B > 0 ~ pmax(kg_lt, 0) / R_B,
        TRUE    ~ 0
      ),
      allocation = (1 - alpha_G) * share_R + alpha_G * share_G,
      # Three columns come out of this. kg_lockin is the record's share of the
      # cell's realized change in the stock of gains, and goes into kg_lt. In the
      # conventional pass it mixes the lock-in and carryover effects; in the
      # frozen pass, where the realization rate does not move, it is carryover
      # alone. kg_deemed_full is what would land on the final return if the
      # household died this year, and kg_deemed is that times the chance it does.
      # Under the death-gain exclusion the two branches, own and married
      # amounts, are kept separately as kg_deemed_full_x and kg_deemed_full_2x;
      # kg_deemed_full is their expectation over the widowhood probability.
      #
      # Neither death column goes into kg_lt. Drawing decedents at random would
      # put roughly plus or minus half on deemed revenue, since expected death
      # gains sit in a few records and a draw held fixed across years makes the
      # error persist. Adding the expected gain instead would linearize the rate
      # schedule, taxing a fraction of the gain at the during-life margin rather
      # than averaging the outcomes for living and dead. So run_one_year() prices
      # it properly, as the chance of death times the difference in tax with and
      # without the gain, computed over the whole frame. That leaves kg_lt as the
      # living taxpayer's frame, which is what the marginal rates should measure.
      kg_lockin         = extra_R * allocation,
      p_widow           = if ('p_widow' %in% names(tax_units)) p_widow else 0,
      kg_deemed_full_x  = (1 - p_char) * deemed_factor * deemed_per_record_x,
      kg_deemed_full_2x = (1 - p_char) * deemed_factor * deemed_per_record_2x,
      kg_deemed_full    = p_widow * kg_deemed_full_2x +
                          (1 - p_widow) * kg_deemed_full_x,
      kg_deemed         = m_household * kg_deemed_full,
      kg_lt = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt) +
              kg_lockin
    ) %>%
    select(-allocation, -share_R, -share_G)
}



kg_dyn_apply_mech_to_records = function(tax_units, scenario_info, year) {

  # Applies the frozen pass's results to records on the static pass, through the
  # same function the conventional pass uses. The realization rate does not move
  # here, so the rate channel does nothing and what reaches records is the
  # carryover realization and the gains deemed realized at death. Decedents are
  # drawn the same way as on the conventional pass, so the two passes mark the
  # same records and their difference can be read record by record.
  #
  # Returns: tax units with kg_lt adjusted and the decomposition columns (df).

  state_path = kg_dyn_mech_state_path(scenario_info, year)
  if (!file.exists(state_path)) {
    stop('kg_dynamics: missing mechanical state file at ', state_path,
         '. The frozen pre-pass (kg_dyn_run_frozen_pass) must run before ',
         'the static pass for kg_dynamics scenarios. In main.R sequential ',
         'mode this happens automatically inside do_scenario(); in SLURM ',
         'mode it is Phase 1B (src/slurm/frozen.R).')
  }
  state = readRDS(state_path)

  cpiu_by_year = kg_dyn_load_cpiu_levels(
    scenario_info$interface_paths$`Macro-Projections`,
    years = year
  )
  tax_units = kg_dyn_attach_record_attrs(
    tax_units,
    cpiu_by_year       = cpiu_by_year,
    death_excl_married = state$regime$death_excl_married
  )

  kg_dyn_apply_to_records(
    tax_units          = tax_units,
    cell_table         = state$cell_table,
    realize_by_asset   = state$regime$realize,
    regime_codes       = state$regime$codes,
    death_excl_married = state$regime$death_excl_married
  )
}



#-------------------------------------------------------------------------------
# Aggregating record rates to cells
#
# Each cell gets its own average marginal rate on gains, measured through the
# simulator rather than proxied by a flat top rate.
#-------------------------------------------------------------------------------

kg_dyn_aggregate_cell_mtr = function(records_with_attrs,
                                      ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Averages the marginal rate on gains within each cell, weighting by
  # realizations. That is the right anchor for calibrating an elasticity, since it
  # is the rate on the dollars that actually realize. Falls back to weighting by
  # holdings in cells that realize nothing, such as young heirs under carryover,
  # and to zero where a cell has neither.
  #
  # Returns: named numeric vector of rates by age.

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



kg_dyn_aggregate_cell_carry = function(records_with_attrs,
                                        ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Averages the wealth tax cost of deferring a gain within each cell, weighting
  # by holdings. The average is taken of each record's own wealth rate times its
  # own gains rate, not of the two rates separately: the records above $50M in net
  # worth are also the ones in the top bracket paying the NIIT, so the product of
  # the averages understates the average of the product.
  #
  # Weighted by holdings throughout, with no fallback to realizations, because
  # this prices the dollars that stay deferred rather than the ones that realize.
  # Cells holding no gains get zero. The plain average wealth rate is also
  # returned, for diagnostics only.
  #
  # Returns: list of the carrying cost and the average wealth rate, by age.

  agg = records_with_attrs %>%
    group_by(age_cohort) %>%
    summarise(num_h = sum(weight * G_unit *
                            coalesce(mtr_net_worth, 0) *
                            coalesce(mtr_kg_lt, 0), na.rm = TRUE),
              num_w = sum(weight * G_unit * coalesce(mtr_net_worth, 0),
                          na.rm = TRUE),
              den   = sum(weight * G_unit, na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(num_h, num_w, den), ~ if_else(is.na(.), 0, .)),
           h     = if_else(den > 0, num_h / den, 0),
           tau_w = if_else(den > 0, num_w / den, 0)) %>%
    arrange(age)

  list(h     = setNames(out$h,     as.character(ages)),
       tau_w = setNames(out$tau_w, as.character(ages)))
}



kg_dyn_aggregate_cell_estate = function(records_with_attrs,
                                         ages = KG_DYN_AGE_MIN:KG_DYN_AGE_MAX) {

  # Averages the estate tax offset within each cell, weighting by holdings. The
  # record-level rate is the marginal estate rate times the law switch for whether
  # income tax at death is deductible, so a reform that turns the deduction off
  # zeroes this without touching the estate rate itself.
  #
  # Weighted by holdings, for the same reason as the carrying cost: this prices
  # the dollars that stay deferred until death. Cells holding no gains get zero,
  # and so do cells below the estate exemption.
  #
  # Capped at 1 per cell, so that numerical noise in the rate near the
  # unified-credit kink can never make the offset exceed the tax it offsets. Cell
  # averages sit inside the range anyway; the cap is a guard.
  #
  # Note that holdings and estate exposure are strongly correlated at the top, so
  # this average compresses a very skewed distribution across records. The
  # per-year diagnostic written by kg_dyn_load_bathtub_inputs() shows that.
  #
  # Returns: named numeric vector of the offset by age.

  agg = records_with_attrs %>%
    group_by(age_cohort) %>%
    summarise(num_e = sum(weight * G_unit * coalesce(mtr_estate_ded, 0),
                          na.rm = TRUE),
              den   = sum(weight * G_unit, na.rm = TRUE),
              .groups = 'drop') %>%
    rename(age = age_cohort)

  out = tibble(age = ages) %>%
    left_join(agg, by = 'age') %>%
    mutate(across(c(num_e, den), ~ if_else(is.na(.), 0, .)),
           e = if_else(den > 0, pmin(pmax(num_e / den, 0), 1), 0)) %>%
    arrange(age)

  setNames(out$e, as.character(ages))
}




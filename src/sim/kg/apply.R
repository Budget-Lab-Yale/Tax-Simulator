#-------------------------------------------------------------------------------
# apply.R
#
# Per-record appliers (pure allocators) and the cell-level MTR / carry / estate aggregators they pair with.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Per-record applier (pure allocator). Reads the precomputed cell_table from
# the bathtub state file and translates cell-level quantities into per-record
# kg_lt adjustments via three channels (spec §7.3):
#   rate     : kg_lt > 0 → kg_lt * rate_factor (= r_S/r_B, clamped to 1)
#   lock-in  : extra_R = r_S * dG, allocated by positive-kg_lt share if
#              R_B > 0, else by G_unit share, else skip
#   deemed   : asset-aware. For each asset class k:
#                contribution_k = realize_k * gain_k_i        (k ≠ primary)
#                contribution_primary = realize_primary *
#                                       pmax(0, gain_primary_i - sec121_i)
#              Summed and scaled by (G_B + dG)/G_B (deemed_factor) into
#              kg_deemed_full; kg_deemed = m_household * kg_deemed_full.
#              realize_k comes from regime$realize, the year-level per-asset
#              deemed indicators from the regime mix. Deemed gains do NOT
#              enter kg_lt — run_one_year prices them via a two-leg
#              expected-tax recompute (see kg_deemed comment below).
#-------------------------------------------------------------------------------

kg_dyn_apply_to_records = function(tax_units, cell_table, realize_by_asset) {

  # Pull just the columns the applier consumes from cell_table via a
  # vectorized match() — avoids hash-joining the ~35-column diagnostics
  # table (with all the Bellman/timing/regime columns) onto 220k records
  # per scenario-year.
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

  # Applier-only deemed avoidance haircut. Data-calibration constant
  # (assumption kg.deemed_avoidance), NOT tax law. Scales the per-record deemed
  # contribution to reflect noncompliance / valuation games; does not touch
  # c_phi or the Bellman.
  deemed_avoidance = economy_param('kg', 'deemed_avoidance')
  if (!is.finite(deemed_avoidance) ||
      deemed_avoidance < 0 || deemed_avoidance > 1) {
    stop(sprintf(
      'kg_dyn_apply_to_records: assumption kg.deemed_avoidance must be in [0, 1]; got %s.',
      format(deemed_avoidance)))
  }
  avoidance_keep = 1 - deemed_avoidance

  # The avoidance haircut is a VALUE discount (valuation games mark down the
  # asset value; basis is unchanged), applied PER RECORD so cross-sectional
  # dispersion in basis/value is preserved: discounted gain = pmax(0, keep*value
  # - basis). A uniform average basis/value ratio would (Jensen, at the pmax(0)
  # kink) zero out a whole class once the mean basis/value exceeds keep, even
  # though the low-basis tail still has taxable gain. value.*/basis.* are on
  # tax_units (read by kg_dyn_attach_record_attrs); the result is a dollar gain
  # amount, so it still rides deemed_factor = (G_B + dG)/G_B for the dG
  # evolution -- we only need baseline value/basis at the record, never through
  # the recurrence. Equals the full gain stock at keep = 1. primary_home nets
  # the §121 exclusion off the discounted gain.
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

  deemed_per_record =
      realize_by_asset[['equities']]      * disc_gain('equities') +
      realize_by_asset[['pass_throughs']] * disc_gain('pass_throughs') +
      realize_by_asset[['primary_home']]  * g_primary_above_cap +
      realize_by_asset[['other_home']]    * disc_gain('other_home') +
      realize_by_asset[['re_fund']]       * disc_gain('re_fund')

  # Resolve the allocation knob to a numeric weight on the G (holdings)
  # share: 'R' = 0 (historical), 'G' = 1, or a numeric blend in [0, 1].
  applier_allocation = as.character(economy_param('kg', 'applier_allocation'))
  alpha_G = switch(applier_allocation,
                   R = 0,
                   G = 1,
                   suppressWarnings(as.numeric(applier_allocation)))
  if (!is.finite(alpha_G) || alpha_G < 0 || alpha_G > 1) {
    stop("kg_dyn_apply_to_records: assumption kg.applier_allocation must be 'R', ",
         "'G', or a number in [0, 1]; got '", applier_allocation, "'.")
  }

  tax_units %>%
    mutate(
      # Each share sums to 1 within an age cell (with cross-fallbacks when a
      # cell has no realizations / no gain stock), so any convex blend does
      # too.
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
      # Decomposition columns:
      #   kg_lockin      — this record's share of the cell's realized dG stock
      #                    (extra_R). In the conventional pass this blends
      #                    lock-in and carryover survival; in the mechanical
      #                    frozen pass (r_S = r_B) it is pure carryover
      #                    realization. Enters kg_lt directly.
      #   kg_deemed_full — the record's full deemed death gain (post-
      #                    avoidance, §121-net, scaled by deemed_factor):
      #                    what lands on the final return IF the household
      #                    dies this year. NOT added to kg_lt here.
      #   kg_deemed      — m_household * kg_deemed_full, the expected deemed
      #                    gain (diagnostics / ETR denominators / heir
      #                    reattribution identification).
      # Deemed death gains deliberately do NOT enter kg_lt. A stochastic
      # decedent draw puts ~±50% sampling error on deemed revenue (expected
      # death gains are concentrated in a few records, and a draw fixed
      # across years makes the error persistent); a fractional m*G injection
      # linearizes the rate schedule (Jensen: taxes m*G at the inter-vivos
      # margin instead of averaging the alive/dead outcomes). Instead,
      # run_one_year computes liab_deemed = m * [T(y + kg_deemed_full) -
      # T(y)] via a second full-frame recompute — the exact expectation with
      # record-level nonlinearity intact — and folds it into liab_iit_net.
      # The kg_lt frame stays alive-leg, so MTRs and tau are pure
      # inter-vivos margins.
      kg_lockin      = extra_R * allocation,
      kg_deemed_full = (1 - p_char) * deemed_factor * deemed_per_record,
      kg_deemed      = m_household * kg_deemed_full,
      kg_lt = if_else(kg_lt > 0, kg_lt * rate_factor, kg_lt) +
              kg_lockin
    ) %>%
    select(-allocation, -share_R, -share_G)
}



kg_dyn_apply_mech_to_records = function(tax_units, scenario_info, year) {

  # Static-pass injection: reads the year's mechanical state file and applies
  # it to records via the same applier the conventional behavior module uses
  # (kg_dyn_apply_to_records). With rate_factor = 1 the rate channel is
  # inert; what lands on records is the carryover realization (kg_lockin)
  # and the mechanical deemed death gains (kg_deemed). Returns tax_units
  # with adjusted kg_lt plus the kg_lockin / kg_deemed columns and
  # decedent_flag (same RNG draw as the conventional pass, so the two
  # passes stamp identical decedents and conventional − static decomposes
  # record by record).

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
  tax_units = kg_dyn_attach_record_attrs(tax_units,
                                         cpiu_by_year = cpiu_by_year)

  kg_dyn_apply_to_records(
    tax_units        = tax_units,
    cell_table       = state$cell_table,
    realize_by_asset = state$regime$realize
  )
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

  # Realization-weighted cell-MTR aggregation: per cell
  #   tau(a) = sum(w * pmax(kg_lt, 0) * mtr_kg_lt) / sum(w * pmax(kg_lt, 0))
  # The right anchor for elasticity calibration — average MTR on the dollars
  # that realize. Falls back to gain-stock weighting when R = 0 (e.g., young
  # heir cohorts under carryover), then to 0 when both are zero.

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

  # Gain-weighted cell aggregation of the wealth-tax deferral carrying cost:
  #   h(a) = sum(w * G_unit * mtr_net_worth * mtr_kg_lt) / sum(w * G_unit)
  # The numerator is the RECORD-LEVEL PRODUCT h_i = tau_w,i * tau_cg,i —
  # never the product of separately averaged rates: Cov(tau_w, tau_cg) > 0
  # (the >$50M records are also the top-bracket/NIIT CG records), so
  # mean(tau_w)*mean(tau_cg) understates mean(tau_w*tau_cg).
  #
  # Pure gain-weighting, no realization-weighted branch (unlike
  # kg_dyn_aggregate_cell_mtr): h prices the dollars that STAY deferred,
  # not the dollars that realize. Cells with zero gain stock get h = 0.
  # Also emits the plain gain-weighted tau_w mean — DIAGNOSTICS ONLY (state
  # file / age-profile column); nothing downstream prices off it.

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

  # Gain-weighted cell aggregation of the estate exposure of the death value:
  #   e(a) = sum(w * G_unit * mtr_estate_ded) / sum(w * G_unit)
  # mtr_estate_ded is the SWITCH-GATED marginal estate rate
  # (estate.income_tax_ded x mtr_estate, derived in run.R's static pass):
  # per-record, per-leg-law by construction, so a reform that sets
  # estate.income_tax_ded = 0 zeroes this exposure while the raw mtr_estate
  # is unchanged.
  #
  # Pure gain-weighting, no realization-weighted branch (same reasoning as
  # kg_dyn_aggregate_cell_carry): e prices the dollars that STAY deferred
  # and die, not the dollars that realize. Cells with zero gain stock get
  # e = 0. Records below the estate exemption have mtr_estate_ded = 0, so
  # below-exemption cells are exact no-ops.
  #
  # CLAMPED to [0, 1] per cell: numerical MTR noise near the unified-credit
  # kink must never create negative death-tax costs or (1 - e) < 0 in the
  # Bellman / tau_eq. (Record-level mtr_estate is a right-derivative of a
  # graduated schedule, so cell means live in [0, top rate] anyway; the
  # clamp is a guard, not a correction.)
  #
  # Gain-weighting note (see the per-year exposure diagnostic written by
  # kg_dyn_load_bathtub_inputs): within-age gain x estate-exposure
  # correlation is strong at the top, so the cell mean compresses a very
  # skewed record-level distribution — the diagnostic makes that visible.

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




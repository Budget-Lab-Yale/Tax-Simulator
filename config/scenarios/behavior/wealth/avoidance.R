#-------------------------------------------------------------------------------
# WEALTH_AVOID_PROVENANCE
#
# Within-year wealth-tax avoidance / evasion elasticities. SEMI-elasticities of
# taxable wealth with respect to the marginal wealth-tax rate (the exp(mtr * e)
# form, == apply_mtr_elasticity's 'semi' type with a baseline MTR of 0). Two
# classes, mirroring the standalone Wealth-Tax-Simulator's do_avoidance():
#   - public_e  : publicly traded / liquid / marketable wealth
#   - private_e : closely-held business and other nonfinancial wealth
#
# CALIBRATION STATUS: SEEDED, NOT RECALIBRATED. These values are copied verbatim
# from the standalone (config/scenarios/nickel_dime.R: public_e = -7,
# private_e = -17) so a wealth-tax score on this model can be compared
# apples-to-apples with the standalone on the BEHAVIORAL side. They are
# deliberately extreme — at a 3% marginal rate they imply ~19% / ~40% erosion of
# marketable / closely-held wealth — and MUST be re-justified / recalibrated on
# Tax-Data before any published estimate. Treat the conventional (post-avoidance)
# wealth-tax score as illustrative until then. Bump WEALTH_AVOID_VERSION and this
# block when recalibrated.
#-------------------------------------------------------------------------------

WEALTH_AVOID_VERSION  = '2026-06-23 seeded-from-standalone (UNCALIBRATED)'
WEALTH_AVOID_PUBLIC_E  = -7
WEALTH_AVOID_PRIVATE_E = -17


do_wealth = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Models within-year wealth-tax avoidance / evasion as a reduction in the
  # REPORTED (taxable) wealth base. Two separate semi-elasticities are applied
  # to the marketable and closely-held component sums of net worth, then the
  # base is re-summed minus debts and written back to the MATERIALIZED net_worth
  # column. The raw value.* columns are left UNTOUCHED, so estate liability and
  # capital income (which read value.*) are unaffected — the "isolated"
  # requirement. calc_wealth() then reprices liab_wealth on the avoided base in
  # the conventional pass; the static - conventional gap IS the avoidance
  # response (the standalone's static_wealth_data vs reported_wealth_data).
  #
  # This is an "implement-any-logic" module (like employment/bastian.R), not a
  # one-line apply_mtr_elasticity() call, because the dual-class response scales
  # two component sums rather than a single registered variable — but it reads
  # the static MTR through the same id/year-indexed machinery.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units, pre tax calculation, with
  #                            the materialized net_worth column and raw value.*
  #   - baseline_mtrs (df)   : baseline MTRs (unused: the baseline net_worth MTR
  #                            is 0 by construction — no wealth tax under current
  #                            law — so the 'semi' baseline term is 0)
  #   - static_mtrs (df)     : static-counterfactual MTRs, must carry
  #                            mtr_net_worth (the statutory marginal wealth rate)
  #   - scenario_info (list) : get_scenario_info() object (unused here)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with net_worth overwritten by the avoided
  #          (reported) base. value.* unchanged.
  #----------------------------------------------------------------------------

  # The wealth MTR must be registered for this module to do anything. Fail loudly
  # rather than silently skipping avoidance (which would mislabel a static score
  # as conventional).
  if (is.null(static_mtrs) || !('mtr_net_worth' %in% names(static_mtrs))) {
    stop('do_wealth(): the wealth-avoidance module requires a registered ',
         'net_worth MTR (mtr_vars = "net_worth", mtr_types = "nextdollar") so ',
         'it can read the statutory marginal wealth rate. The runscript for ',
         'scenario "', scenario_info$ID, '" does not provide mtr_net_worth.')
  }

  message('do_wealth(): applying wealth-avoidance elasticities (',
          WEALTH_AVOID_VERSION, '; public_e=', WEALTH_AVOID_PUBLIC_E,
          ', private_e=', WEALTH_AVOID_PRIVATE_E, ')')

  tax_units %>%

    # Join the static marginal wealth rate (0 below the exemption => no response)
    left_join(static_mtrs %>% select(id, year, mtr_net_worth),
              by = c('id', 'year')) %>%
    mutate(

      mtr_net_worth = replace_na(mtr_net_worth, 0),

      # Component sums of the economic balance sheet (raw value.*, untouched)
      .marketable   = rowSums(across(all_of(WEALTH_MARKETABLE_COLS),   ~ replace_na(., 0))),
      .closely_held = rowSums(across(all_of(WEALTH_CLOSELY_HELD_COLS), ~ replace_na(., 0))),
      .debts        = rowSums(across(all_of(WEALTH_DEBT_COLS),         ~ replace_na(., 0))),

      # Semi-elasticity response (baseline wealth MTR = 0): shrink each class'
      # reported value by exp(mtr * e). Debts are not avoided (no incentive).
      net_worth = .marketable   * exp(mtr_net_worth * WEALTH_AVOID_PUBLIC_E) +
                  .closely_held * exp(mtr_net_worth * WEALTH_AVOID_PRIVATE_E) -
                  .debts
    ) %>%
    select(-mtr_net_worth, -.marketable, -.closely_held, -.debts) %>%
    return()
}

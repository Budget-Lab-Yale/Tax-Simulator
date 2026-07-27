#-------------------------------------------------------------------------------
# estate/avoidance.R
#
# Contains the reported-estate response
#-------------------------------------------------------------------------------

# This module sets the share of the reported gross estate that is hidden from the
# estate tax, which calc_estate then reads. Three things contribute to it.
#
# First, wealth the wealth tax response concealed. Second, income the evasion
# module hid: the evaded share of a record's closely held income pulls the
# matching share of its closely held assets out of the reported estate. Both are
# inert where the module in question did not run.
#
# Third, the estate tax's own effect on reporting. Assume an elasticity of the
# reported estate with respect to the net-of-estate-tax rate of 0.16 (Kopczuk and
# Slemrod 2001).
#
#   retained = ((1 - tau_S) / (1 - tau_B)) ^ 0.16
#
# where the two rates are the record's marginal estate rates under the baseline and
# the scenario. The form is exact for a large reform and handles a record that was
# not previously taxable at all.
#
# That last response keys off the change in the rate, never its level. Avoidance at
# the current rate is already in the valuation bridge, which is fitted to reported
# estates in the SOI data, so responding to the level would count it twice. Where
# estate law does not change, the ratio is one and this does nothing, which is why
# it is safe to load in every conventional run.
#
# The concealment and valuation split that the wealth response uses does not apply
# here: the base in question is the estate itself, and both kinds reduce it through
# the same column. The three contributions stack on the retained share, so the
# hidden share can never exceed everything.
#
# As with the wealth response, this reduces the taxable estate only. What heirs
# inherit, and the asset columns themselves, are untouched: heirs receive the
# hidden assets in full.
#
# Two caveats on the source. The estimate bundles timing, avoidance, valuation and
# some real change in accumulation, and part of the response it measures is
# charitable planning, which this reduced form partly absorbs. And it is an
# elasticity of the reported taxable estate, applied here to the reported gross
# estate, which overstates it slightly above the exemption. Both are disclosed in
# the methodology memo.
#
# For scale: at this elasticity, raising the top rate from 40% to 55% hides about
# 4.5% of the top bracket's reported estate. That is an order of magnitude below
# the wealth tax response, as it should be, since the estate tax is paid once and
# the wealth tax recurs.
#-------------------------------------------------------------------------------

ESTATE_AVOID_VERSION = '2026-07-16 standalone estate reporting module (split from wealth/avoidance)'

# The elasticity, here because this module is the only thing that reads it.
# Kopczuk and Slemrod (2001) report pooled estimates between 0.10 and 0.22; quote
# the band rather than the point. Caveats in the header above.
ESTATE_REPORT_EPS = 0.16


do_estate = function(tax_units, baseline_mtrs, static_mtrs, scenario_info, indexes) {

  #----------------------------------------------------------------------------
  # Sets the share of the reported gross estate hidden from the estate tax, from the
  # three contributions in the header. calc_estate reads the column and reduces the
  # taxable estate; what heirs inherit is untouched.
  #
  # Runs after the evasion and wealth modules where they are present, since it reads
  # what they left on the record. The order is fixed in src/sim/behavior.R. Where
  # estate law does not change this does nothing.
  #
  # Parameters:
  #   - tax_units (df)       : tibble of tax units with raw value.* columns,
  #                            plus (optionally) evasion_g_* factors persisted
  #                            by evasion/debacker and wealth_c_pub /
  #                            wealth_c_priv persisted by wealth/avoidance
  #   - baseline_mtrs (df)   : baseline MTRs; mtr_estate is LOAD-BEARING
  #                            (current law has a 40% top estate rate)
  #   - static_mtrs (df)     : static-counterfactual MTRs; must carry
  #                            mtr_estate (the un-switched marginal estate rate)
  #   - scenario_info (list) : get_scenario_info() object (ID for messages;
  #                            output_path for diagnostics)
  #   - indexes (df)         : generate_indexes() object (unused here)
  #
  # Returns: full tax_units tibble with the estate_concealed_frac column set
  #          and the transient upstream factors (evasion_g_*, wealth_c_*)
  #          dropped. All other columns unchanged.
  #----------------------------------------------------------------------------

  # Ordering is fixed in src/sim/behavior.R, which puts this module last. The
  # defaults below cover the case where the modules it reads from did not run at all.

  # The own-rate response needs the marginal estate rate on both legs. The baseline
  # rate is genuinely nonzero, current law having a 40% top rate, so the baseline
  # join carries real weight. A missing baseline column usually means the baseline
  # vintage predates this module: re-run the baseline static pass with "estate"
  # registered in its mtr_vars.
  if (is.null(static_mtrs) || !('mtr_estate' %in% names(static_mtrs))) {
    stop('do_estate(): the estate own-rate response requires a registered ',
         'estate MTR (mtr_vars = "estate", mtr_types = "nextdollar") on the ',
         'scenario row. The runscript for scenario "', scenario_info$ID,
         '" does not provide mtr_estate in static MTRs.')
  }
  if (is.null(baseline_mtrs) || !('mtr_estate' %in% names(baseline_mtrs))) {
    stop('do_estate(): the estate own-rate response requires mtr_estate in ',
         'BASELINE MTRs (register "estate" in the baseline row\'s mtr_vars). ',
         'Missing there usually means a pre-build baseline vintage -- re-run ',
         'the baseline static pass with current code. Scenario: "',
         scenario_info$ID, '".')
  }

  message('do_estate(): applying estate reporting response (',
          ESTATE_AVOID_VERSION, '; estate_report_eps=',
          ESTATE_REPORT_EPS, ')')

  year = tax_units$year[1]

  # Default the factors from the earlier modules where those did not run, so each
  # contribution does nothing but the code path stays the same.
  for (g in c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 1
  }
  for (g in c('wealth_c_pub', 'wealth_c_priv')) {
    if (!(g %in% names(tax_units))) tax_units[[g]] = 0
  }

  # Sums columns per record, treating a missing cell as zero
  sum_cols = function(d, cols) {
    m = as.matrix(d[, cols, drop = FALSE])
    m[is.na(m)] = 0
    rowSums(m)
  }

  df = tax_units %>%
    left_join(static_mtrs %>% select(id, year, mtr_estate_S = mtr_estate),
              by = c('id', 'year')) %>%
    left_join(baseline_mtrs %>% select(id, year, mtr_estate_B = mtr_estate),
              by = c('id', 'year')) %>%
    mutate(mtr_estate_S = replace_na(mtr_estate_S, 0),
           mtr_estate_B = replace_na(mtr_estate_B, 0))

  # Component sums off the raw balance sheet, which is left alone
  mkt   = sum_cols(df, WEALTH_MARKETABLE_COLS)
  clh   = sum_cols(df, WEALTH_CLOSELY_HELD_COLS)
  gross = mkt + clh

  # How much of the record's closely held income the evasion module hid, weighted
  # across its positive legs. Reading the legs after wealth concealment is exact,
  # since that scaled every closely held leg by the same factor and it cancels in
  # the ratio.
  lp_schc = pmax(replace_na(df$sole_prop, 0), 0)
  lp_pt   = pmax(replace_na(df$part_active,   0), 0) +
            pmax(replace_na(df$part_passive,  0), 0) +
            pmax(replace_na(df$scorp_active,  0), 0) +
            pmax(replace_na(df$scorp_passive, 0), 0)
  lp_rent = pmax(replace_na(df$rent, 0), 0)
  leg_tot = lp_schc + lp_pt + lp_rent
  evaded  = ifelse(leg_tot > 0,
                   (lp_schc * (1 - df$evasion_g_schc) +
                    lp_pt   * (1 - df$evasion_g_pt) +
                    lp_rent * (1 - df$evasion_g_rent)) / leg_tot,
                   0)
  evaded  = pmax(pmin(evaded, 1), 0)

  # Concealment and evasion are two ways a closely held asset leaves the
  # authority's sight, so combine them without counting the overlap twice: wealth is
  # concealed first, then evasion hides a share of what remains visible. Legal
  # valuation gaming stays visible to the estate, as it should.
  c_pub  = pmax(pmin(replace_na(df$wealth_c_pub,  0), 1), 0)
  c_priv = pmax(pmin(replace_na(df$wealth_c_priv, 0), 1), 0)
  estate_c_priv = c_priv + (1 - c_priv) * evaded
  estate_union  = ifelse(gross > 0,
                         (c_pub * mkt + estate_c_priv * clh) / gross, 0)

  # The estate's own effect on reporting, from the change in the marginal rate. Both
  # rates are capped before the ratio is taken. Unchanged law leaves this at one, and
  # a record that was not previously taxable falls out of the same formula. A rate
  # cut gives more than one, so previously unreported estate reappears.
  tau_eS = pmin(pmax(df$mtr_estate_S, 0), 1 - 1e-6)
  tau_eB = pmin(pmax(df$mtr_estate_B, 0), 1 - 1e-6)
  retained_estate = ((1 - tau_eS) / (1 - tau_eB)) ^ ESTATE_REPORT_EPS
  f_estate = 1 - retained_estate

  # Stack the three on the retained share, which cannot hide more than everything.
  # Floored at -1, so that even an extreme rate cut cannot more than double the
  # reported base. This moves the taxable estate only; what heirs inherit does not
  # change.
  df$estate_concealed_frac = pmax(1 - (1 - estate_union) * retained_estate, -1)

  if (any(is.na(df$estate_concealed_frac))) {
    stop('do_estate(): NA introduced in estate_concealed_frac.')
  }

  #--- Diagnostics -------------------------------------------------------------
  w = df$weight
  diag = tibble(
    year                        = year,
    version                     = ESTATE_AVOID_VERSION,
    estate_report_eps           = ESTATE_REPORT_EPS,
    estate_union_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * estate_union * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_hidden_from_evasion  = sum(w * (1 - c_priv) * evaded * clh),
    estate_mtr_B_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * tau_eB * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_mtr_S_wmean_grosspos = if (sum(w * (gross > 0)) > 0)
                                    sum(w * tau_eS * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_own_rate_f_wmean     = if (sum(w * (gross > 0)) > 0)
                                    sum(w * f_estate * (gross > 0)) /
                                    sum(w * (gross > 0)) else 0,
    estate_own_rate_hidden      = sum(w * f_estate * (1 - estate_union) * gross),
    estate_concealed_frac_wmean = if (sum(w * (gross > 0)) > 0)
                                    sum(w * df$estate_concealed_frac) /
                                    sum(w * (gross > 0)) else 0)

  diag_dir = file.path(scenario_info$output_path, 'conventional', 'supplemental')
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(diag, file.path(diag_dir, paste0('estate_avoidance_', year, '.csv')))

  # Drop the joined rates and the factors from the earlier modules, keeping the
  # concealed share itself, which calc_estate reads off the frame.
  df %>%
    select(-mtr_estate_S, -mtr_estate_B,
           -any_of(c('evasion_g_schc', 'evasion_g_pt', 'evasion_g_rent',
                     'wealth_c_pub', 'wealth_c_priv'))) %>%
    return()
}

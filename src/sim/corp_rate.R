#-------------------------------------------------------------------------------
# corp_rate.R
#
# Contains functions to score a change in the corporate statutory rate
#-------------------------------------------------------------------------------

# The corporate rate is scored on-model rather than off it, as depreciation is
# scored in the cost recovery interface. Rate policy is set through the rate
# parameter in corp.yaml, and this file maps a change in it to a change in
# corporate revenue on top of the CBO baseline receipts line. The same change
# feeds the incidence channel on the conventional side and the distributional
# smear on the static side. What remains off-model is corporate changes other than
# the rate and depreciation.
#
# No new interface is involved: the revenue change is a function of the baseline
# and scenario rates and the CBO receipts level, all of which the model already
# has.
#
# Assume that the corporate tax base has a constant elasticity with respect to the
# net-of-tax rate, following Coles, Patel, Seegert and Smith (2022). Writing the
# baseline rate t0, the scenario rate t, baseline receipts R0 and the elasticity
# e, the base and revenue are
#
#   B(t) = (R0 / t0) * ((1 - t) / (1 - t0)) ^ e
#   R(t) = t * B(t)
#
# The static estimate holds the base at its baseline level, so it is simply the
# rate change times that base. The conventional estimate lets the base erode.
# Revenue is maximized at a rate of 1 / (1 + e).
#
# The elasticity of 0.367 is the avoidance-only taxable income response those
# authors report, re-based onto the statutory rather than the effective net-of-tax
# rate. It is deliberately not their headline effective-rate figure of 0.91, nor
# the total statutory response of 0.55, nor the economic-only piece of 0.30. An
# elasticity is a response per unit change in a particular rate, and the policy
# lever here is the statutory rate, so the denominator has to match. Using the
# effective-based figure against a statutory change would count the gap between
# the two twice.
#
# Two known biases, which should be stated whenever the estimate is quoted.
#
# The estimate is an upper bound on the lasting effect. The elasticity is measured
# by bunching at the zero-income kink, so it captures the reduction in taxable
# income within a single year and cannot see it reversed later. Shifting income
# between years requires a rate difference between them, and a flat change in the
# statutory rate leaves no such difference in the long run, only a one-time shift
# at enactment. Treating the whole elasticity as permanent therefore overstates
# the persistent response.
#
# And the base is too large. Dividing all CBO corporate receipts by the statutory
# rate treats every dollar of them as taxed at that rate, which includes the
# minimum tax and the international provisions that are not.
#
# The elasticity is corp.rate_eti in the economy leg. Other self-consistent
# statutory values, for reference: 0.182 economic, 0.549 total.

# A rate change smaller than this counts as no change.
CORP_RATE_EPS = 1e-12



corp_rate_read_series = function(scenario_tax_law_path) {

  #----------------------------------------------------------------------------
  # Reads the baseline and scenario corporate rates by year, from the written tax
  # law files. The same files distribution.R and the entity-shifting module read.
  # The scenario rate comes from the path given; the baseline rate always from the
  # baseline run's own tax law.
  #
  # Parameters:
  #   - scenario_tax_law_path (str) : path to this scenario's tax_law.csv
  #
  # Returns: tibble of year and the two rates, or NULL where either file is
  #          missing, which the caller reads as no rate change.
  #----------------------------------------------------------------------------

  base_path = file.path(globals$baseline_root,
                        'baseline/static/supplemental/tax_law.csv')
  if (!file.exists(scenario_tax_law_path) || !file.exists(base_path)) {
    return(NULL)
  }

  t_scen = read_csv(scenario_tax_law_path, show_col_types = FALSE) %>%
    distinct(year, t = corp.rate)
  t_base = read_csv(base_path, show_col_types = FALSE) %>%
    distinct(year, t0 = corp.rate)

  t_scen %>%
    left_join(t_base, by = 'year') %>%
    arrange(year)
}



corp_rate_delta = function(rate_series, rev_corp, static,
                           eti = economy_param('corp', 'rate_eti')) {

  #----------------------------------------------------------------------------
  # Computes the change in corporate revenue for the pass being run, in billions,
  # from the relationship in the header.
  #
  # Parameters:
  #   - rate_series (df|NULL) : the two rates by year; NULL for no rate change
  #   - rev_corp (df)         : CBO baseline corporate receipts by year, billions
  #   - static (bool)         : TRUE holds the base fixed; FALSE lets it erode
  #   - eti (dbl)             : elasticity of the base to the net-of-tax rate
  #
  # Returns: tibble of year and the revenue change, zero where the rate does not
  #          move or a rate is unavailable.
  #----------------------------------------------------------------------------

  if (is.null(rate_series)) {
    return(rev_corp %>% transmute(year, delta = 0))
  }

  rev_corp %>%
    left_join(rate_series, by = 'year') %>%
    mutate(
      ratio     = t / t0,
      raw_delta = if (isTRUE(static)) {
        rev_corp * (ratio - 1)
      } else {
        rev_corp * (ratio * ((1 - t) / (1 - t0)) ^ eti - 1)
      },
      delta = if_else(
        is.na(t) | is.na(t0) | is.na(rev_corp) | abs(t - t0) < CORP_RATE_EPS,
        0, raw_delta
      )
    ) %>%
    select(year, delta)
}

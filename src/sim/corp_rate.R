#-------------------------------------------------------------------------------
# On-model corporate statutory-rate revenue module
#
# Moves the revenue estimation for corporate STATUTORY-RATE changes ONTO the
# model, out of Off-Model-Estimates (OME) -- exactly as depreciation lives in
# the Cost-Recovery-Simulator interface. Rate policy is configured through the
# existing `corp.yaml` `rate` parameter (`corp.rate` in the written tax_law.csv
# sidecar). A crude single-elasticity module maps a change in that rate to a
# corporate revenue delta on top of the CBO baseline corporate receipts line
# (`rev_corp`). The same delta feeds the corp_incidence wedge (conventional
# stream; see src/sim/corp_incidence.R corp_read_wedge) and the distribution
# smear (static stream; see distribution.R get_other_taxes). After this change
# OME carries only "corporate changes ex depreciation AND ex statutory rate".
#
# NO NEW INTERFACE: unlike cost recovery (external repo, pre-baked deltas), the
# rate delta is a pure function of `corp.rate` (baseline vs scenario, from the
# tax_law.csv sidecars) and `rev_corp` (Macro-Projections / CBO). No vintage,
# no dep.* runscript columns.
#
# METHOD (all per year; t0 = baseline corp.rate, t = scenario corp.rate,
# R0 = CBO rev_corp level, e = CORP_RATE_ETI, B0 = R0 / t0):
#   Revenue Laffer curve with a CONSTANT NET-OF-TAX elasticity (Coles-Patel-
#   Seegert-Smith 2022 "Form A"):
#     B(t) = B0 * [(1 - t) / (1 - t0)] ^ e
#     R(t) = t * B(t)
#   Static delta       (base held at baseline) : (t - t0) * B0 = R0 * (t/t0 - 1)
#   Conventional delta (Form A base erosion)   : R(t) - R0 =
#        R0 * ( (t/t0) * ((1 - t)/(1 - t0))^e  -  1 )
#   Both are 0 when t == t0 (dormant). Revenue-max statutory rate t* = 1/(1+e).
#
# ELASTICITY (CORP_RATE_ETI = 0.367): the CPSS (2022, JAR 60(3)) AVOIDANCE-only
# taxable-income response (Table 5: 5.96% income change) re-based onto the
# STATUTORY net-of-tax denominator (16.25%). This is NOT their headline
# effective-rate CETI (0.91 = 8.93/9.76), NOR the total statutory (0.55), NOR
# the economic-only piece (0.30). The value is WELDED to the statutory rate
# concept: an elasticity is response-per-unit-of-a-rate-concept, and our policy
# lever is the statutory rate, so the denominator must be the statutory
# net-of-tax change. (Using the effective-based 0.91 against a statutory rate
# move would double-count the effective/statutory wedge.)
#
# KNOWN BIASES / HEALTH WARNINGS (never silently drop these when quoting):
#   (1) UPPER BOUND on the steady-state effect. The avoidance elasticity is a
#       bunching estimate at the $0 kink: it measures the CONTEMPORANEOUS
#       one-year taxable-income reduction and cannot observe intertemporal
#       reversal. Timing shifting arbitrages a rate DIFFERENTIAL across periods
#       (CPSS identify it off the permanent within-year 0%/15% kink); a flat
#       statutory-rate LEVEL change has no steady-state differential to
#       arbitrage (only a one-time transition-year shift at enactment). So
#       treating the full 0.367 as a permanent base elasticity overstates the
#       persistent response, all else equal.
#   (2) WHOLE-rev_corp base. B0 = rev_corp / t0 treats ALL CBO corporate
#       receipts as levied at the statutory rate; it includes CAMT / GILTI /
#       BEAT that are not, so the rate-sensitive base is overstated.
#
# Sweeps: override the elasticity via the CORP_RATE_ETI env var (mirrors the
# CORP_SIGMA_N / CORP_KAPPA convention in corp_incidence.R). Menu of
# self-consistent statutory-based values for reference: economic 0.182,
# avoidance 0.367 (default), total 0.549.
#-------------------------------------------------------------------------------

# Statutory net-of-tax elasticity of the corporate tax base (default: CPSS 2022
# avoidance component, re-based to the statutory denominator). Env-overridable.
CORP_RATE_ETI = as.numeric(Sys.getenv('CORP_RATE_ETI', unset = '0.367'))

# Below this |t - t0| the rate change is treated as no change (dormant).
CORP_RATE_EPS = 1e-12



corp_rate_read_series = function(scenario_tax_law_path) {

  #----------------------------------------------------------------------------
  # Reads the per-year baseline vs scenario statutory corporate rate from the
  # written supplemental/tax_law.csv sidecars (the aggregate access point; the
  # same reads used by distribution.R get_other_taxes and the entity-shifting
  # module pearce_prisinzano.R). The scenario rate comes from the passed path;
  # the baseline rate always from globals$baseline_root's baseline tax law.
  #
  # Parameters:
  #   - scenario_tax_law_path (str) : path to this scenario's tax_law.csv
  #
  # Returns: tibble(year, t0, t) -- t0 = baseline corp.rate, t = scenario
  #          corp.rate. NULL if either sidecar is unavailable (caller treats
  #          a missing series as no rate change).
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



corp_rate_delta = function(rate_series, rev_corp, static, eti = CORP_RATE_ETI) {

  #----------------------------------------------------------------------------
  # The pass-appropriate corporate statutory-rate revenue delta ($B), via the
  # Form A revenue Laffer curve (see module header). Pure and unit-testable.
  #
  # Parameters:
  #   - rate_series (df|NULL) : tibble(year, t0, t) from corp_rate_read_series
  #   - rev_corp (df)         : tibble(year, rev_corp) -- CBO baseline corporate
  #                             receipts level, $B, over the years to book
  #   - static (lgl)          : TRUE -> mechanical static delta (t-t0)*B0;
  #                             FALSE -> Form A conventional (base-eroded) delta
  #   - eti (dbl)             : statutory net-of-tax base elasticity
  #
  # Returns: tibble(year, delta) aligned to rev_corp$year (0 where the rate is
  #          unchanged, the series is NULL, or t/t0 is unavailable).
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

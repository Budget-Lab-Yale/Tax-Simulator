#-------------------------------------------------------------------------------
# estate.R
#
# Weights side of the on-model estate tax: frozen measurement parameter
# loading, household mortality (the "extra weight" that converts per-record
# conditional liability into expected deaths and dollars), and the per-year
# totals contract consumed by calc_receipts() and SLURM Phase 3a.
#
# The liability calculation itself is calc_estate() in
# src/calc/functions/tax/estate.R — pure, weight- and mortality-free. The
# split is deliberate: mortality (and its donor-clone cluster cap) is a
# population-level weights operation, not a per-record tax calculation.
#-------------------------------------------------------------------------------


get_estate_params = function(tax_data_path = NULL) {

  #----------------------------------------------------------------------------
  # Loads the frozen estate measurement parameters (valuation bridge, per-bin
  # SOI-estimated inputs, gift add-back, cluster cap). These are MEASUREMENT,
  # not tax law: they are deliberately outside the tax law override machinery
  # so reform scenarios cannot touch them. Regenerated offline by
  # other/estate_tax/write_frozen_params.R after any re-calibration.
  #
  # Parameters:
  #   - tax_data_path (str) : scenario's Tax-Data interface path; when
  #                           supplied, warns if the run's vintage differs
  #                           from the one the parameters were calibrated on
  #                           (r/rho_pt and the cluster cap are
  #                           vintage-specific)
  #
  # Returns: list with r, rho_pt, gamma, cluster_death_weight_cap,
  #          tax_data_vintage, and bins (tibble: size_bin, lo, hi, f_ded,
  #          p_dsue, f_dsue, sorted by lo and tiling [0, Inf))
  #----------------------------------------------------------------------------

  params = read_yaml(economy_param('estate', 'valuation_bridge'))
  params$bins = as_tibble(params$bins) %>%
    mutate(hi = if_else(is.na(hi) | hi == 'Inf', Inf, as.numeric(hi))) %>%
    arrange(lo)

  # The bins must tile [0, Inf): the calculator assigns every record a bin
  if (params$bins$lo[1] != 0 ||
      !is.infinite(params$bins$hi[nrow(params$bins)]) ||
      any(head(params$bins$hi, -1) != tail(params$bins$lo, -1))) {
    stop('Estate valuation parameter bins do not tile [0, Inf); regenerate ',
         'config/calibrations/estate/bridge.yaml')
  }

  if (!is.null(tax_data_path) &&
      !grepl(params$tax_data_vintage, tax_data_path, fixed = TRUE)) {
    warning(sprintf(
      paste0('estate: frozen valuation parameters were calibrated on ',
             'Tax-Data vintage %s but this run uses %s. Estate tax levels ',
             'and deltas are STALE for this vintage — r/rho_pt and the ',
             'donor-clone cluster cap are vintage-specific. Re-run the ',
             'calibration (other/estate_tax/calibrate_estate_v2.R) and ',
             'regenerate config/calibrations/estate/bridge.yaml before ',
             'using estate results.'),
      params$tax_data_vintage, tax_data_path))
  }

  return(params)
}


calc_estate_mortality = function(tax_units, cluster_cap, cluster_floor = 5e6) {

  #----------------------------------------------------------------------------
  # Household death-event probability for the estate tax: q1 * q2 for joint
  # returns with a living spouse (the both-die event matching the calculator's
  # 2x-exemption path), q1 otherwise — then the absolute cluster death-weight
  # cap (thoughts doc §10f, ported from estate_module.R::
  # apply_cluster_abscap_mortality).
  #
  # The cap is the locked donor-clone guard: Tax-Data replicates thin donor
  # pools into byte-identical wealth clusters that inherit one donor's age;
  # elderly donors create clusters with pathological aggregate death-weight.
  # Any exact-gross cluster (n >= 2, gross > cluster_floor) whose total
  # death-weight sum(weight * m) exceeds cluster_cap expected deaths has its
  # members' m scaled down so the cluster totals the cap. Only the few
  # pathological mega-clusters trip an absolute threshold; everything else
  # stays exactly raw. The cap VALUE is re-derived per Tax-Data vintage
  # (frozen in config/calibrations/estate/bridge.yaml); the RULE is generic.
  #
  # This is a cross-RECORD operation (cluster grouping over the full year
  # population) — the reason it lives here and not in the pure calculator.
  # Weights are already rescaled by 1 / pct_sample upstream (run_one_year),
  # so the cap comparison is unbiased at any sample share, just noisier.
  #
  # Parameters:
  #   - tax_units (df)      : full year population with weight, filing_status,
  #                           q_death1, q_death2, and wealth columns
  #   - cluster_cap (dbl)   : maximum expected deaths per exact-gross cluster
  #   - cluster_floor (dbl) : minimum gross for cluster detection
  #
  # Returns: numeric vector of household death probabilities (estate_m),
  #          aligned to tax_units rows
  #----------------------------------------------------------------------------

  tax_units %>%
    mutate(
      .q1 = replace_na(q_death1, 0),
      .q2 = replace_na(q_death2, 0),
      .m_raw = if_else(filing_status == 2 & .q2 > 0, .q1 * .q2, .q1),
      .economic_gross = rowSums(across(all_of(ESTATE_ASSET_COLS),
                                       ~ replace_na(., 0))),
      .cluster_key = if_else(.economic_gross > cluster_floor,
                             round(.economic_gross), NA_real_)
    ) %>%
    group_by(.cluster_key) %>%
    mutate(
      .dw_raw = sum(weight * .m_raw),
      .capped = !is.na(.cluster_key) & n() >= 2 & .dw_raw > cluster_cap,
      .m      = if_else(.capped, .m_raw * cluster_cap / .dw_raw, .m_raw)
    ) %>%
    ungroup() %>%
    pull(.m)
}


get_estate_totals = function(tax_units, year) {

  #----------------------------------------------------------------------------
  # Aggregates per-record estate detail into the per-year totals contract:
  # expected calendar-year estate tax liability and expected taxable returns.
  # Pure weights-times-states arithmetic on the five persisted estate columns
  # — also reconstructable from detail files (SLURM Phase 3a does exactly
  # that).
  #
  # The DSUE/no-DSUE branches are blended here, by state probability:
  #   E[tax]     = sum( w * m * (p * T_dsue + (1 - p) * T_nodsue) )
  #   E[returns] = sum( w * m * (p * 1(T_dsue > 0) + (1 - p) * 1(T_nodsue > 0)) )
  # Indicators are blended per branch because 1(E[T] > 0) != E[1(T > 0)] for
  # records straddling the unified-credit kink.
  #
  # Parameters:
  #   - tax_units (df) : records with weight, estate_m, estate_p_dsue,
  #                      liab_estate_dsue, liab_estate_nodsue
  #   - year (int)     : calendar (death) year
  #
  # Returns: 1-row tibble(year, est_tax_exp, est_returns); est_tax_exp in $B
  #          (matching receipts units), est_returns in counts
  #----------------------------------------------------------------------------

  tax_units %>%
    summarise(
      year = !!year,
      est_tax_exp = sum(weight * estate_m *
                          (estate_p_dsue * liab_estate_dsue +
                           (1 - estate_p_dsue) * liab_estate_nodsue)) / 1e9,
      est_returns = sum(weight * estate_m *
                          (estate_p_dsue * (liab_estate_dsue > 0) +
                           (1 - estate_p_dsue) * (liab_estate_nodsue > 0)))
    )
}


get_estate_totals_from_detail = function(detail_root, years) {

  #----------------------------------------------------------------------------
  # Rebuilds the per-year estate totals contract from already-written detail
  # files. Used by calc_receipts() as the baseline-leg fallback when the
  # baseline's totals/estate.csv does not exist yet: SLURM Phase 3a runs all
  # scenarios as a parallel array, so a counterfactual's receipts job can
  # land before the baseline's aggregation job — but baseline DETAIL files
  # are guaranteed by then (Phase 2 already read them). Detail weights are
  # already rescaled by 1 / pct_sample at write time.
  #
  # Parameters:
  #   - detail_root (str) : directory containing {year}.csv detail files
  #   - years (int[])     : years to aggregate
  #
  # Returns: tibble(year, est_tax_exp, est_returns), or NULL if the detail
  #          files are missing or predate the estate columns
  #----------------------------------------------------------------------------

  needed = c('weight', 'estate_m', 'estate_p_dsue',
             'liab_estate_dsue', 'liab_estate_nodsue')
  paths = file.path(detail_root, paste0(years, '.csv'))
  if (!all(file.exists(paths))) {
    return(NULL)
  }
  if (!all(needed %in% names(fread(paths[1], nrows = 0, showProgress = FALSE)))) {
    return(NULL)
  }

  map2_dfr(paths, years,
           ~ fread(.x, select = needed, showProgress = FALSE) %>%
             as_tibble() %>%
             get_estate_totals(.y))
}

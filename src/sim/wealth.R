#-------------------------------------------------------------------------------
# wealth.R
#
# Totals contract for the annual net-worth (wealth) tax: per-year aggregation
# of the per-record liability column into the weighted level consumed by
# calc_receipts() and SLURM Phase 3a.
#
# The liability calculation itself is calc_wealth() in
# src/calc/functions/tax/wealth.R -- pure and weight-free. Unlike the estate
# tax there is no mortality / weights side: a wealth tax is assessed on the
# LIVING population every year, so the only population operation is the
# weighted sum here (no estate_m, no cluster cap, no DSUE blend).
#-------------------------------------------------------------------------------


get_wealth_totals = function(tax_units, year) {

  #----------------------------------------------------------------------------
  # Aggregates per-record wealth detail into the per-year totals contract:
  # expected calendar-year wealth tax liability and the count of taxable
  # returns. Pure weights-times-liability arithmetic on the persisted
  # liab_wealth column -- also reconstructable from detail files (SLURM Phase
  # 3a, get_wealth_from_detail()).
  #
  # Parameters:
  #   - tax_units (df) : records with weight and liab_wealth
  #   - year (int)     : calendar year
  #
  # Returns: 1-row tibble(year, wealth_tax, wealth_returns); wealth_tax in $B
  #          (matching receipts units), wealth_returns in counts
  #----------------------------------------------------------------------------

  tax_units %>%
    summarise(
      year           = !!year,
      wealth_tax     = sum(weight * liab_wealth) / 1e9,
      wealth_returns = sum(weight * (liab_wealth > 0))
    )
}


get_wealth_from_detail = function(detail_root, years) {

  #----------------------------------------------------------------------------
  # Rebuilds the per-year wealth totals contract from already-written detail
  # files. Mirrors get_estate_totals_from_detail(): a fallback for any caller
  # that has detail but not the totals CSV (e.g. ad-hoc re-aggregation). Detail
  # weights are already rescaled by 1 / pct_sample at write time.
  #
  # Parameters:
  #   - detail_root (str) : directory containing {year}.csv detail files
  #   - years (int[])     : years to aggregate
  #
  # Returns: tibble(year, wealth_tax, wealth_returns), or NULL if the detail
  #          files are missing or predate the wealth columns
  #----------------------------------------------------------------------------

  needed = c('weight', 'liab_wealth')
  paths  = file.path(detail_root, paste0(years, '.csv'))
  if (!all(file.exists(paths))) {
    return(NULL)
  }
  if (!all(needed %in% names(fread(paths[1], nrows = 0, showProgress = FALSE)))) {
    return(NULL)
  }

  map2_dfr(paths, years,
           ~ fread(.x, select = needed, showProgress = FALSE) %>%
             as_tibble() %>%
             get_wealth_totals(.y))
}

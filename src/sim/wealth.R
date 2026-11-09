#-------------------------------------------------------------------------------
# wealth.R
#
# Contains functions to aggregate wealth tax liability to yearly totals
#-------------------------------------------------------------------------------

# Liability itself is calculated per record by calc_wealth(). Unlike the estate
# tax there is no mortality to apply: a wealth tax falls on the living every year,
# so all that happens here is a weighted sum.
#-------------------------------------------------------------------------------


get_wealth_totals = function(tax_units, year) {

  #----------------------------------------------------------------------------
  # Sums wealth tax liability and taxable returns for one year.
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
  # Rebuilds those totals from detail files already written, for a caller that has
  # the detail but not the totals. Detail weights are already rescaled on the way
  # out, so no further adjustment is needed.
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

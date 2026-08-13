#-------------------------------------------------------
# Function to calculate state child deductions (NC-style)
#-------------------------------------------------------

return_vars$calc_st_child_ded = c('st_child_ded')


calc_st_child_ded = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates a per-qualifying-child deduction using an AGI table. The table
  # is filing-status mapped in YAML and is intentionally independent of the
  # federal CTC dollar amount: a state can retain its own thresholds while
  # relying on the federal CTC-qualifying-child definition.
  #
  # count_offset excludes the first N qualifying children from the count, for
  # states whose deduction reaches only the SECOND and later dependents
  # (NM 7-2-39: "$4,000 for each dependent ... Subtract 1 from total
  # dependents"). Defaults to 0, i.e. every qualifying child counts.
  #----------------------------------------------------------------------------

  req_vars = c(
    'agi',
    'n_dep_ctc',
    'st_child_ded.style',
    'st_child_ded.count_offset'
  )

  tax_unit %<>% parse_calc_fn_input(req_vars, fill_missings)

  st_child_ded = rep(0, nrow(tax_unit))
  upper   = st_family_matrix(tax_unit, 'st_child_ded.agi_bounds', 1:7)
  amounts = st_family_matrix(tax_unit, 'st_child_ded.amounts', 1:7,
                             require_sentinel = FALSE)

  if (!is.null(upper) && !is.null(amounts)) {
    per_child = st_band_value(tax_unit$agi, upper, amounts)
    n_qual    = pmax(0, tax_unit$n_dep_ctc - tax_unit$st_child_ded.count_offset)
    st_child_ded = if_else(tax_unit$st_child_ded.style == 1,
                           n_qual * per_child, 0)
  }

  tibble(st_child_ded = st_child_ded)
}

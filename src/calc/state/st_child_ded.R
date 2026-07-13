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
  #----------------------------------------------------------------------------

  req_vars = c(
    'agi',
    'n_dep_ctc',
    'st_child_ded.style'
  )

  tax_unit %<>% parse_calc_fn_input(req_vars, fill_missings)

  n = nrow(tax_unit)
  cols = colnames(tax_unit)
  bounds_cols = paste0('st_child_ded.agi_bounds', 1:7)
  amount_cols = paste0('st_child_ded.amounts', 1:7)
  st_child_ded = rep(0, n)

  if (all(c(bounds_cols, amount_cols) %in% cols) &&
      any(!is.na(tax_unit$st_child_ded.agi_bounds1))) {
    upper = as.matrix(tax_unit[bounds_cols])
    lower = cbind(-Inf, upper[, -7, drop = FALSE])
    amounts = as.matrix(tax_unit[amount_cols])
    per_child = rowSums(amounts *
                          (tax_unit$agi > lower & tax_unit$agi <= upper),
                         na.rm = TRUE)
    st_child_ded = if_else(tax_unit$st_child_ded.style == 1,
                           pmax(0, tax_unit$n_dep_ctc) * per_child, 0)
  }

  tibble(st_child_ded = st_child_ded)
}

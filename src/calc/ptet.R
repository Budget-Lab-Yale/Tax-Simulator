#------------------------------------------------------------------
# ptet.R
#
# Pass-through entity taxes (PTET): elective state income taxes paid
# by partnerships and S corporations on their owners' shares, which
# move owners' state tax out from under the federal SALT cap.
# Design: research/state_tax/ptet/ptet_methodology.md (decision S48)
# and research/state_tax/plan.md section 3.2. The state side is
# src/calc/state/st_ptet.R
#------------------------------------------------------------------

# Values of ptet.mode (config/scenarios/tax_law/baseline/ptet.yaml)
PTET_MODE_LEGACY   = 0   # do_salt_workaround_baseline(): a share of itemized SALT
PTET_MODE_REPORTED = 1   # microdata are reported amounts; no federal adjustment


calc_ptet = function(tax_units) {

  #----------------------------------------------------------------------------
  # Federal side of pass-through entity taxes in the baseline data prep.
  #
  # ptet.mode selects the method:
  #   0 : do_salt_workaround_baseline(), unchanged
  #   1 : nothing is moved. The microdata's pass-through income and itemized
  #       state income tax are reported amounts, already net of the PTET for
  #       owners whose entity elected (decision S48), so the file is the
  #       federal baseline as it stands. Who elected, and in which state, is
  #       attributed by the state calculator (calc_st_ptet())
  #
  # Parameters:
  #   - tax_units (df) : tibble of tax unit data, with tax law joined
  #
  # Returns: tibble of updated tax unit data, with salt_workaround_part and
  #          salt_workaround_scorp holding the amounts moved (df).
  #----------------------------------------------------------------------------

  mode = unique(tax_units$ptet.mode)
  if (length(mode) != 1 || !(mode %in% c(PTET_MODE_LEGACY, PTET_MODE_REPORTED))) {
    stop('calc_ptet(): ptet.mode must be ', PTET_MODE_LEGACY, ' or ',
         PTET_MODE_REPORTED, ' and constant within a year; got ',
         paste(mode, collapse = ', '))
  }
  if (mode == PTET_MODE_LEGACY) {
    return(do_salt_workaround_baseline(tax_units))
  }

  tax_units %>%
    mutate(salt_workaround_part = 0, salt_workaround_scorp = 0) %>%
    return()
}

#-------------------------------------------------------------------------------
# payroll.R
#
# Contains the predicate identifying reforms that change employer-side payroll
# tax law
#-------------------------------------------------------------------------------

# The employer payroll wage adjustment holds total employer cost fixed and lets
# wages absorb the employer tax change. It runs inside do_taxes whenever a
# baseline employer payroll table is supplied, and so is a transmission channel:
# it moves the income tax base in response to a payroll provision. It runs on the
# mechanical and conventional passes and not on the static one, which means a
# scenario whose reform touches employer payroll law needs the mechanical pass
# even when no other channel is live.

.payroll_cache = new.env(parent = emptyenv())

# Employer-side subparameters of pr.yaml. The wage rescale reads liab_fica_er,
# so the self-employment rate is not among them: a SECA record gets no wage
# adjustment, which is an asymmetry the rescale has always carried.
PAYROLL_ER_SUBPARAMS = c('oasdi_er_rates', 'oasdi_er_brackets',
                         'hi_er_rates',    'hi_er_brackets')



scenario_uses_er_payroll_reform = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reports whether a scenario's reform changes employer-side payroll law, by
  # comparing the reform's employer subparameters against the baseline's on
  # content rather than on whether the reform names them. A reform restating a
  # subparameter unchanged is not a payroll reform.
  #
  # Cached per scenario, keyed on the tax law layer rather than the scenario ID,
  # so two scenarios sharing a layer resolve once.
  #
  # Parameters:
  #   - scenario_info (list) : output of get_scenario_info()
  #
  # Returns: TRUE if any employer payroll subparameter differs from baseline
  #          (bool).
  #----------------------------------------------------------------------------

  if (scenario_info$ID == 'baseline') return(FALSE)

  law_id = as.character(scenario_info$tax_law_id)
  if (identical(law_id, 'default')) return(FALSE)

  hit = .payroll_cache[[law_id]]
  if (!is.null(hit)) return(hit)

  reform   = load_tax_law_input(tax_law_path(law_id))
  baseline = load_baseline_tax_law_input()

  changed = PAYROLL_ER_SUBPARAMS %>%
    map_lgl(.f = ~ !is.null(reform$pr[[.x]]) &&
                   !identical(reform$pr[[.x]], baseline$pr[[.x]])) %>%
    any()

  .payroll_cache[[law_id]] = changed
  changed
}

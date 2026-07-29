#-------------------------------------------------------------------------------
# test_er_payroll_predicate.R
#
# Checks that scenario_uses_er_payroll_reform() fires on the employer-side
# payroll fixtures and stays silent on the employee-side one and on default law
#-------------------------------------------------------------------------------

n_fail = 0

check = function(label, ok) {
  cat(if (isTRUE(ok)) 'PASS  ' else 'FAIL  ', label, '\n', sep = '')
  if (!isTRUE(ok)) n_fail <<- n_fail + 1
  invisible(NULL)
}

# The predicate reads raw tax law YAML through tax_law_path(), so a scenario_info
# stub carrying the ID and the tax law cell is all it needs.
si = function(id, law) list(ID = id, tax_law_id = law)

check('default law is not a payroll reform',
      !scenario_uses_er_payroll_reform(si('plain', 'default')))

check('baseline is never a payroll reform',
      !scenario_uses_er_payroll_reform(si('baseline', 'tests/pr_test/er_oasi_1pp')))

check('a reform touching no payroll parameter is not a payroll reform',
      !scenario_uses_er_payroll_reform(si('sd', 'tests/sd_bump_10k')))

check('employee-side HI change is not an employer payroll reform',
      !scenario_uses_er_payroll_reform(si('ee_hi', 'tests/pr_test/ee_hi_1pp')))

check('employer-side OASDI rate change is a payroll reform',
      scenario_uses_er_payroll_reform(si('er_oasi', 'tests/pr_test/er_oasi_1pp')))

check('employer-side HI rate change is a payroll reform',
      scenario_uses_er_payroll_reform(si('er_hi', 'tests/pr_test/er_hi_1pp')))

check('employer-side taxable maximum change is a payroll reform',
      scenario_uses_er_payroll_reform(si('er_taxmax', 'tests/pr_test/er_oasi_taxmax')))

cat('\n', n_fail, ' failure(s)\n', sep = '')
if (n_fail > 0) quit(status = 1)

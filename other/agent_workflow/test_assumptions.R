#-------------------------------------------------------------------------------
# test_assumptions.R
#
# Unit checks for the model-assumptions layer (src/misc/assumptions.R). Runs
# standalone: no simulation, no interfaces. Exits nonzero on any failure.
#-------------------------------------------------------------------------------

setwd('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator')

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)
  ))
)

source('./src/misc/assumptions.R')

fails = 0
check = function(label, ok) {
  cat(if (isTRUE(ok)) '  PASS  ' else '  FAIL  ', label, '\n', sep = '')
  if (!isTRUE(ok)) fails <<- fails + 1
}
expect_error = function(label, expr) {
  ok = inherits(try(force(expr), silent = TRUE), 'try-error')
  check(label, ok)
}

cat('\n== load and validate ==\n')
defaults = assumptions_load_defaults()
check('at least one channel loaded', length(defaults) >= 1)
check('sigma channel present',       'sigma' %in% names(defaults))
check('sigma.conv value is 0.16',    identical(defaults$sigma$conv$value, 0.16))
check('sigma.conv kind calibrated',  identical(defaults$sigma$conv$kind, 'calibrated'))

cat('\n== schema enforcement ==\n')
expect_error('unknown kind rejected',
  assumptions_validate(list(x = list(a = list(value = 1, kind = 'wishful')))))
expect_error('calibrated missing fields rejected',
  assumptions_validate(list(x = list(a = list(value = 1, kind = 'calibrated')))))
expect_error('bare value rejected',
  assumptions_validate(list(x = list(a = 1))))
expect_error('nonexistent invalidated_by rejected',
  assumptions_validate(list(x = list(a = list(
    value = 1, kind = 'calibrated', set = 'x', target = 'x',
    derived_under = list(tax_data = '1'), rederive = 'x',
    invalidated_by = c('src/does_not_exist.R'))))))

cat('\n== resolve: defaults ==\n')
plain = assumptions_resolve(defaults, list(ID = 'baseline', assumptions = NA))
check('resolves to default value',   identical(plain$values$sigma$conv, 0.16))
check('no overrides recorded',       nrow(plain$overrides) == 0)

cat('\n== resolve: dotted column override ==\n')
over = assumptions_resolve(defaults, list(ID           = 'x',
                                          assumptions  = NA,
                                          `assumption.sigma.conv` = '0.30'))
check('override applied',            identical(over$values$sigma$conv, 0.30))
check('override stays numeric',      is.numeric(over$values$sigma$conv))
check('other values untouched',      identical(over$values$sigma$pt_labor_share, 0.75))
check('override recorded once',      nrow(over$overrides) == 1)
check('override source is runscript', identical(over$overrides$source[1], 'runscript'))
check('override records the default', identical(over$overrides$default[1], '0.16'))

cat('\n== resolve: blanks are not overrides ==\n')
blank = assumptions_resolve(defaults, list(ID = 'x', assumptions = NA,
                                           `assumption.sigma.conv` = NA))
check('NA column ignored',           nrow(blank$overrides) == 0)
blank2 = assumptions_resolve(defaults, list(ID = 'x', assumptions = NA,
                                            `assumption.sigma.conv` = ''))
check('empty column ignored',        nrow(blank2$overrides) == 0)

cat('\n== resolve: bad input rejected ==\n')
expect_error('unknown channel rejected',
  assumptions_resolve(defaults, list(ID = 'x', assumptions = NA,
                                     `assumption.nope.conv` = '1')))
expect_error('unknown name rejected',
  assumptions_resolve(defaults, list(ID = 'x', assumptions = NA,
                                     `assumption.sigma.nope` = '1')))
expect_error('non-numeric override rejected',
  assumptions_resolve(defaults, list(ID = 'x', assumptions = NA,
                                     `assumption.sigma.conv` = 'banana')))
expect_error('missing override folder rejected',
  assumptions_resolve(defaults, list(ID = 'x', assumptions = 'no_such_folder')))

cat('\n== activate and read ==\n')
expect_error('reading before activation errors', assumption('sigma', 'conv'))
assumptions_activate(plain)
check('assumption() reads active set', identical(assumption('sigma', 'conv'), 0.16))
assumptions_activate(over)
check('activation swaps the set',      identical(assumption('sigma', 'conv'), 0.30))
expect_error('unknown channel errors',  assumption('nope', 'conv'))
expect_error('unknown name errors',     assumption('sigma', 'nope'))

cat('\n== staleness ==\n')
# The two live vintages differ by design: kg is pinned on Tax-Data 2026070814
# while sigma and the levels timeable share were derived under 2026050315. A
# single 'clean' run therefore cannot exist for every entry at once, so the
# clean-case check is run per entry against that entry's own pinned vintages.
live = list(tax_data = '2026050315', macro_projections = '2026022522')
sigma_only = list(sigma = defaults$sigma)
clean = assumptions_check_staleness(sigma_only, plain, live, enforce = FALSE)
check('clean when vintages and hashes match', length(clean) == 0)

kg_only  = list(kg = defaults$kg)
kg_live  = list(tax_data = '2026070814', macro_projections = '2026022522')
kg_clean = suppressWarnings(assumptions_check_staleness(kg_only, plain, kg_live,
                                                        enforce = FALSE))
check('kg hashes match the shipped code',
      !any(grepl('has changed since', kg_clean, fixed = TRUE)))
check('only the known 2026050315 timeable-share entry reads stale',
      all(grepl('timeable_share', kg_clean, fixed = TRUE)))

stale_v = suppressWarnings(assumptions_check_staleness(
  defaults, plain, list(tax_data = '9999999999',
                        macro_projections = '2026022522'), enforce = FALSE))
sigma_finding = stale_v[grepl('sigma.conv', stale_v, fixed = TRUE)]
check('trips on a changed vintage', length(stale_v) >= 1)
check('names the constant',         length(sigma_finding) == 1)
check('names the rederive script',
      grepl('compute_top_eti', sigma_finding[1], fixed = TRUE))
check('flags every calibrated entry whose vintage moved',
      length(stale_v) == sum(unlist(lapply(defaults, function(ch)
        vapply(ch, function(e) identical(e$kind, 'calibrated') &&
                               'tax_data' %in% names(e$derived_under),
               logical(1))))))

expect_error('hard stop when enforcing',
  assumptions_check_staleness(defaults, plain,
                              list(tax_data = '9999999999',
                                   macro_projections = '2026022522'),
                              enforce = TRUE))

skipped = suppressWarnings(assumptions_check_staleness(
  defaults, over, list(tax_data = '9999999999',
                       macro_projections = '2026022522'), enforce = FALSE))
check('an overridden value is exempt from the staleness check',
      !any(grepl('sigma.conv', skipped, fixed = TRUE)))

# Hash arm: a dependency edited since pinning must trip the check.
tmp_defaults = list(sigma = defaults$sigma)
tmp_defaults$sigma$conv$invalidated_by_hashes[['src/sim/sigma_conversion.R']] =
  'deadbeefdeadbeefdeadbeefdeadbeef'
stale_h = suppressWarnings(assumptions_check_staleness(
  tmp_defaults, plain, live, enforce = FALSE))
check('trips on an edited dependency file', length(stale_h) == 1)
check('names the changed file',
      grepl('sigma_conversion.R', stale_h[1], fixed = TRUE))
check('names the constant whose dependency moved',
      grepl('sigma.conv', stale_h[1], fixed = TRUE))

cat('\n== manifest ==\n')
man = assumptions_manifest(defaults, over, 'test_scenario')
check('one row per assumption',   nrow(man) == length(unlist(lapply(defaults, names))))
check('carries the scenario ID',  all(man$ID == 'test_scenario'))
check('flags the overridden row',
      isTRUE(man$overridden[man$channel == 'sigma' & man$name == 'conv']))
check('records the used value',
      identical(man$value[man$channel == 'sigma' & man$name == 'conv'], '0.3'))
check('non-overridden rows say default',
      identical(man$source[man$channel == 'sigma' & man$name == 'pt_labor_share'],
                'default'))
check('carries kind', identical(man$kind[man$channel == 'sigma' & man$name == 'conv'],
                                'calibrated'))

cat('\n', strrep('-', 50), '\n', sep = '')
if (fails == 0) {
  cat('ALL CHECKS PASSED\n')
} else {
  cat(fails, ' CHECK(S) FAILED\n', sep = '')
  quit(status = 1)
}

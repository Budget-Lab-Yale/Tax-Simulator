#-------------------------------------------------------------------------------
# test_scenario_config.R
#
# Unit tests for src/misc/scenario_config.R (the three-leg resolution engine).
# Builds a synthetic leg tree under a temp dir, swaps CONFIG_LEG_ROOTS to point
# at it, and exercises load / validate / resolve / precedence / locked / roles /
# pass guard / staleness (vintage arm, hash arm, conditioned_on, pointer) /
# parse_year_spec. Run via sbatch (never the login node):
#   sbatch other/config_redesign/run_tests.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
source('./src/misc/scenario_config.R')

n_pass = 0; n_fail = 0
check = function(label, expr) {
  ok = tryCatch(isTRUE(expr), error = function(e) { message('  error: ', conditionMessage(e)); FALSE })
  if (ok) { n_pass <<- n_pass + 1; cat('PASS ', label, '\n') }
  else    { n_fail <<- n_fail + 1; cat('FAIL ', label, '\n') }
}
expect_error = function(label, expr, pattern) {
  err = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  check(label, !is.null(err) && grepl(pattern, err))
}

#--------------------------
# Synthetic leg tree setup
#--------------------------

root = file.path(tempdir(), 'cfgtest')
unlink(root, recursive = TRUE)
for (d in c('economy/default', 'economy/alternatives/alt',
            'economy/alternatives/sets/nested',
            'economy/alternatives/badset', 'economy/alternatives/lockedset',
            'economy/alternatives/novalue', 'economy/alternatives/warnonly',
            'behavior/default', 'behavior/alternatives/stack'))
  dir.create(file.path(root, d), recursive = TRUE)

# A dependency file + a pointer target for calibrated entries
dep_file = file.path(root, 'dep.R'); writeLines('x = 1', dep_file)
dir.create(file.path(root, 'profiles/default'), recursive = TRUE)
ptr_file = file.path(root, 'profiles/default/s.csv'); writeLines('age,p,s', ptr_file)

dep_hash = unname(tools::md5sum(dep_file))
ptr_hash = unname(tools::md5sum(ptr_file))

write_yaml(list(
  `_channel` = list(role = 'state'),
  tax_data_vintage = list(value = '2026070814', kind = 'structural', note = 'pin'),
  tax_data_id      = list(value = 'baseline',   kind = 'structural', note = 'pin')
), file.path(root, 'economy/default/interfaces.yaml'))

write_yaml(list(
  `_channel` = list(role = 'transmission'),
  sigma_n = list(value = 0.375, kind = 'sourced', citation = 'OTA/TPC'),
  bridge  = list(value = 'x',   kind = 'judgment', note = 'locked one', locked = TRUE),
  eta     = list(value = 2.5, kind = 'calibrated', set = '2026-07-01',
                 target = 'target', derived_under = list(tax_data = '2026070814'),
                 invalidated_by = list(dep_file),
                 invalidated_by_hashes = setNames(list(dep_hash), dep_file),
                 rederive = 'script.R'),
  profile = list(value = 'default', kind = 'calibrated', set = '2026-07-01',
                 pointer_root = file.path(root, 'profiles'),
                 target = 'table pointer', derived_under = list(tax_data = '2026070814'),
                 invalidated_by = list(ptr_file),
                 invalidated_by_hashes = setNames(list(ptr_hash), ptr_file),
                 rederive = 'script.R'),
  mixed   = list(value = 1, kind = 'structural', note = 'state entry in transmission channel',
                 role = 'state'),
  soft    = list(value = 3.0, kind = 'calibrated', set = '2026-07-01',
                 enforcement = 'warn',
                 target = 'warn-level entry', derived_under = list(tax_data = '2026070814'),
                 invalidated_by = list(dep_file),
                 invalidated_by_hashes = setNames(list(dep_hash), dep_file),
                 rederive = 'script.R')
), file.path(root, 'economy/default/corp.yaml'))

write_yaml(list(sigma_n = list(value = 0.5)), file.path(root, 'economy/alternatives/alt/corp.yaml'))
write_yaml(list(sigma_n = list(value = 0.61), eta = list(value = 3.1)),
           file.path(root, 'economy/alternatives/sets/nested/corp.yaml'))
write_yaml(list(eta = list(value = 3.1), profile = list(value = 'default')),
           file.path(root, 'economy/alternatives/warnonly/corp.yaml'))
write_yaml(list(nope    = list(value = 1)),   file.path(root, 'economy/alternatives/badset/corp.yaml'))
write_yaml(list(bridge  = list(value = 'y')), file.path(root, 'economy/alternatives/lockedset/corp.yaml'))
write_yaml(list(sigma_n = list(note = 'no value here')),
           file.path(root, 'economy/alternatives/novalue/corp.yaml'))

write_yaml(list(
  e = list(value = -0.5, kind = 'judgment', note = 'central'),
  conditioned = list(value = 0.16, kind = 'calibrated', set = '2026-07-12',
                     target = 'ETI', derived_under = list(tax_data = '2026070814'),
                     invalidated_by = list(dep_file),
                     invalidated_by_hashes = setNames(list(dep_hash), dep_file),
                     rederive = 'script.R',
                     conditioned_on = list(`behavior.charity.e` = -1.0))
), file.path(root, 'behavior/default/charity.yaml'))
write_yaml(list(e = list(value = -1.0)), file.path(root, 'behavior/alternatives/stack/charity.yaml'))

CONFIG_LEG_ROOTS = list(economy = file.path(root, 'economy'),
                        behavior = file.path(root, 'behavior'))

#------------
# Load/validate
#------------

eco = config_load_defaults('economy')
beh = config_load_defaults('behavior')
check('economy channels loaded',        setequal(names(eco$entries), c('interfaces', 'corp')))
check('channel role recorded',          identical(eco$roles$corp, 'transmission'))
check('entry-level role override',      identical(eco$entries$corp$mixed$role, 'state'))
check('entry inherits channel role',    identical(eco$entries$corp$sigma_n$role, 'transmission'))

# Validation failures
bad = eco; bad$entries$corp$sigma_n$kind = 'vibes'
expect_error('unknown kind rejected', config_validate('economy', bad$entries), 'kind must be one of')
bad2 = eco; bad2$entries$corp$eta$invalidated_by = list('/nope/missing.R')
expect_error('dangling invalidated_by rejected', config_validate('economy', bad2$entries), 'does not exist')
bad3 = eco; bad3$entries$corp$profile$value = 'missing_profile'
expect_error('dangling pointer rejected', config_validate('economy', bad3$entries), 'does not exist under')

#------------
# Resolve / precedence
#------------

r0 = config_resolve('economy', eco)
check('default resolve value',      identical(r0$values$corp$sigma_n, 0.375))
check('default resolve no overrides', nrow(r0$overrides) == 0)

r1 = config_resolve('economy', eco, alternative = 'alt')
check('alternative override applies',  identical(r1$values$corp$sigma_n, 0.5))
check('alternative override recorded', r1$overrides$source[1] == 'alternative:alt')

r2 = config_resolve('economy', eco, alternative = 'sets/nested')
check('nested alternative resolves', identical(r2$values$corp$sigma_n, 0.61))
check('alternative keeps default type', is.numeric(r2$values$corp$sigma_n))
check('unset entries keep default', identical(r2$values$corp$mixed, 1L) ||
                                    identical(r2$values$corp$mixed, 1))

expect_error('unknown alternative errors',
             config_resolve('economy', eco, alternative = 'nope'),
             'Unknown economy alternative')
expect_error('unknown entry errors', config_resolve('economy', eco, alternative = 'badset'),
             'unknown economy entry')
expect_error('locked entry refuses override',
             config_resolve('economy', eco, alternative = 'lockedset'), 'locked')
expect_error('alternative entry without value errors',
             config_resolve('economy', eco, alternative = 'novalue'), 'must supply a `value`')

rb = config_resolve('behavior', beh, alternative = 'stack')
check('behavior alternative param override', identical(rb$values$charity$e, -1.0))

#------------
# Activation / accessors / pass guard
#------------

config_activate(economy = r0, behavior = rb)
check('economy_param reads',   identical(economy_param('corp', 'sigma_n'), 0.375))
check('behavior_param reads',  identical(behavior_param('charity', 'e'), -1.0))

config_set_pass('static')
expect_error('transmission read refused on static pass',
             economy_param('corp', 'sigma_n'), 'STATIC')
check('state entry readable on static pass',
      identical(economy_param('corp', 'mixed'), 1L) || identical(economy_param('corp', 'mixed'), 1))
config_set_pass('conventional')
check('transmission readable on conventional', identical(economy_param('corp', 'sigma_n'), 0.375))
config_set_pass(NA)

config_activate(economy = r0, behavior = NULL)
expect_error('fail-closed when behavior leg absent',
             behavior_param('charity', 'e'), 'before any scenario was activated')

#------------
# Staleness
#------------

iv = config_interface_vintages(r0)
check('interface vintages derived from leg', identical(iv$tax_data, '2026070814'))

f_clean = config_check_staleness('economy', eco, r0, iv,
                                 cross_values = list(behavior = rb$values), enforce = FALSE)
check('clean staleness', length(f_clean) == 0)

# vintage arm
iv_stale = list(tax_data = '2099010101')
f_v = suppressWarnings(config_check_staleness('economy', eco, r0, iv_stale, enforce = FALSE))
check('vintage arm trips', any(grepl('pinned against tax_data', f_v)))

# hash arm
writeLines('x = 2', dep_file)
f_h = suppressWarnings(config_check_staleness('economy', eco, r0, iv, enforce = FALSE))
check('hash arm trips', any(grepl('has changed since', f_h)))
writeLines('x = 1', dep_file)

# an alternative's override suppresses that entry's check (overriding IS the
# acknowledgment)
f_ov = config_check_staleness('economy', eco, r2, iv_stale, enforce = FALSE) %>% suppressWarnings()
check('alternative override suppresses that entry',
      !any(grepl('corp.eta:', f_ov, fixed = TRUE)))

# enforcement: warn -- warns and is returned, but never stops
r_warn = config_resolve('economy', eco, alternative = 'warnonly')
f_soft = tryCatch({
  withCallingHandlers(
    config_check_staleness('economy', eco, r_warn, list(tax_data = '2099010101'),
                           enforce = TRUE),
    warning = function(w) invokeRestart('muffleWarning'))
  }, error = function(e) conditionMessage(e))
check('warn-level entry does not stop the run',
      is.character(f_soft) && any(grepl('^corp.soft:', f_soft)))

# conditioned_on arm (behavior leg entry conditioned on charity.e = -1)
rb_default = config_resolve('behavior', beh)   # e = -0.5, violates the condition
f_c = suppressWarnings(config_check_staleness('behavior', beh, rb_default, iv,
                                              cross_values = list(behavior = rb_default$values),
                                              enforce = FALSE))
check('conditioned_on trips on mismatch', any(grepl('calibrated under behavior.charity.e', f_c)))
f_c2 = config_check_staleness('behavior', beh, rb, iv,
                              cross_values = list(behavior = rb$values), enforce = FALSE)
check('conditioned_on clean on match', !any(grepl('conditioned', f_c2)))

# enforce = TRUE stops
expect_error('enforced staleness stops',
             config_check_staleness('economy', eco, r0, iv_stale, enforce = TRUE),
             'STALE CALIBRATION')

#------------
# parse_year_spec
#------------

check('year single',  identical(parse_year_spec('2030'), 2030L))
check('year range',   identical(parse_year_spec('2026:2028'), 2026:2028))
check('year list',    identical(parse_year_spec('2033 2027 2030'), c(2027L, 2030L, 2033L)))
expect_error('year malformed', parse_year_spec('2026:2027:2028'), 'Malformed')
expect_error('year empty', parse_year_spec(''), 'Empty')

#------------
# Manifest
#------------

m = config_manifest('economy', eco, r2, 'test_scenario')
check('manifest rows cover entries', nrow(m) == 8)
check('manifest records override source',
      m$source[m$channel == 'corp' & m$name == 'sigma_n'] == 'alternative:sets/nested')
check('manifest carries role column', all(c('state', 'transmission') %in% m$role))
check('manifest alternative column', all(m$alternative == 'sets/nested'))

# A leg with no value entries at all is the behavior leg's normal state once its
# modules carry their own parameters. The manifest still has to produce a
# well-formed (empty) table: bind_rows() over nothing gives a 0x0 tibble, and
# the run died writing scenario_config.csv the first time this was not handled.
m_empty = config_manifest('behavior', list(entries = list()),
                          list(leg = 'behavior', alternative = 'default',
                               values = list(), roles = NULL,
                               overrides = tibble(channel = character(),
                                                  name = character(),
                                                  source = character()),
                               waivers = list()),
                          'baseline')
check('manifest of an entry-less leg is an empty typed table',
      nrow(m_empty) == 0 &&
      all(c('ID', 'leg', 'alternative', 'channel', 'name', 'value', 'kind',
            'role', 'overridden', 'source') %in% names(m_empty)))
check('an entry-less leg binds with a populated one',
      nrow(bind_rows(m, m_empty)) == nrow(m))

#------------
# Reserved default name
#------------

dir.create(file.path(root, 'economy/alternatives/default'), recursive = TRUE)
expect_error('alternatives/default is refused',
             config_load_defaults('economy'), 'reserved runscript cell value')
unlink(file.path(root, 'economy/alternatives/default'), recursive = TRUE)
check('reserved word resolves to the default layer',
      identical(config_resolve('economy', config_load_defaults('economy'),
                               alternative = 'default')$alternative, 'default'))

cat(sprintf('\n%d passed, %d failed\n', n_pass, n_fail))
if (n_fail > 0) stop('scenario_config tests FAILED') else cat('ALL_TESTS_PASS\n')

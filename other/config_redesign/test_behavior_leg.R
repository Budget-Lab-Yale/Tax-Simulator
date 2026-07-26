#-------------------------------------------------------------------------------
# test_behavior_leg.R
#
# Unit tests for the behavior leg loader (src/sim/behavior.R): the two written
# forms of the kg_dynamics section, the pinned-order sort, applier injection,
# and every parse-time check that replaced an in-module guard.
#
# Builds a synthetic behavior tree under a temp dir, swaps CONFIG_LEG_ROOTS at
# it, and points the module paths at throwaway files -- so the tests say what
# the loader does, not what today's stacks happen to contain. Run via sbatch:
#   sbatch other/config_redesign/run_tests.sbatch . \
#          other/config_redesign/test_behavior_leg.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
source('./src/misc/scenario_config.R')
source('./src/sim/behavior.R')
# scenario_uses_kg_dynamics lives with the kg state helpers; the test covers it
# because it is the predicate the whole split-pass orchestration keys on.
source('./src/sim/kg/state.R')

n_pass = 0; n_fail = 0
check = function(label, expr) {
  ok = tryCatch(isTRUE(expr), error = function(e) {
    message('  error: ', conditionMessage(e)); FALSE })
  if (ok) { n_pass <<- n_pass + 1; cat('PASS ', label, '\n') }
  else    { n_fail <<- n_fail + 1; cat('FAIL ', label, '\n') }
}
expect_error = function(label, expr, pattern) {
  err = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  check(label, !is.null(err) && grepl(pattern, err))
}

#--------------------------------
# Synthetic tree: modules + legs
#--------------------------------

root = file.path(tempdir(), 'behtest')
unlink(root, recursive = TRUE)

# Throwaway module files, one per family the tests use. Family = parent folder.
mod_root = file.path(root, 'modules')
FAMILIES = c('kg_dynamics', 'conversion', 'entity_shifting', 'evasion',
             'wealth', 'charity', 'estate', 'ot', 'employment')
for (fam in FAMILIES) {
  dir.create(file.path(mod_root, fam), recursive = TRUE)
  writeLines(sprintf('do_%s = function(...) invisible(NULL)', fam),
             file.path(mod_root, fam, 'm.R'))
}
m = function(fam) file.path(mod_root, fam, 'm.R')

# The applier constant has to point inside the synthetic tree for these tests
BEHAVIOR_KG_APPLIER = m('kg_dynamics')

for (d in c('default', 'alternatives/full', 'alternatives/list_form',
            'alternatives/map_form', 'alternatives/no_kg',
            'alternatives/unranked', 'alternatives/lists_applier',
            'alternatives/conv_no_kg', 'alternatives/piece_no_module',
            'alternatives/module_no_piece', 'alternatives/no_bathtub',
            'alternatives/bad_piece', 'alternatives/wealth_no_estate',
            'alternatives/evasion_no_estate', 'alternatives/missing_file',
            'alternatives/dup'))
  dir.create(file.path(root, 'behavior', d), recursive = TRUE)

CONFIG_LEG_ROOTS = list(economy  = file.path(root, 'economy'),
                        behavior = file.path(root, 'behavior'))

wr = function(alt, body) {
  d = if (identical(alt, 'default')) file.path(root, 'behavior/default')
      else file.path(root, 'behavior/alternatives', alt)
  writeLines(body, file.path(d, 'behavior.yaml'))
}

wr('default', c('kg_dynamics: none', 'modules: []'))

# The full stack, deliberately written OUT of execution order so the sort has
# something to do: estate before evasion, charity first.
wr('full', c(
  'kg_dynamics: [bathtub, conversion, entity_shifting]',
  'modules:',
  paste0('  - ', m('charity')),
  paste0('  - ', m('estate')),
  paste0('  - ', m('evasion')),
  paste0('  - ', m('entity_shifting')),
  paste0('  - ', m('conversion'))))

wr('list_form', c('kg_dynamics: [bathtub]', 'modules: []'))
wr('map_form',  c('kg_dynamics:', '  bathtub: requirements.txt', 'modules: []'))
wr('no_kg',     c('kg_dynamics: none',
                  'modules:', paste0('  - ', m('employment'))))
# Unranked families keep the order they are listed in, after the ranked ones
wr('unranked', c('kg_dynamics: none', 'modules:',
                 paste0('  - ', m('ot')),
                 paste0('  - ', m('employment')),
                 paste0('  - ', m('charity'))))

wr('lists_applier', c('kg_dynamics: [bathtub]', 'modules:',
                      paste0('  - ', m('kg_dynamics'))))
wr('conv_no_kg', c('kg_dynamics: none', 'modules:',
                   paste0('  - ', m('conversion'))))
wr('piece_no_module', c('kg_dynamics: [bathtub, conversion]', 'modules: []'))
wr('module_no_piece', c('kg_dynamics: [bathtub]', 'modules:',
                        paste0('  - ', m('conversion'))))
wr('no_bathtub', c('kg_dynamics: [conversion]', 'modules:',
                   paste0('  - ', m('conversion'))))
wr('bad_piece', c('kg_dynamics: [bathtub, teapot]', 'modules: []'))
wr('wealth_no_estate', c('kg_dynamics: none', 'modules:',
                         paste0('  - ', m('wealth'))))
wr('evasion_no_estate', c('kg_dynamics: none', 'modules:',
                          paste0('  - ', m('evasion'))))
wr('missing_file', c('kg_dynamics: none', 'modules:',
                     paste0('  - ', file.path(mod_root, 'charity/gone.R'))))
wr('dup', c('kg_dynamics: none', 'modules:',
            paste0('  - ', m('charity')),
            paste0('  - ', m('charity'))))

#--------------------------------
# Family extraction and the sort
#--------------------------------

check('family is the parent folder name',
      identical(behavior_family(c('src/behavior/charity/50.R',
                                  'src/behavior/kg/62.R')),
                c('charity', 'kg')))

check('sort puts the pinned families in pinned order',
      identical(behavior_family(behavior_order(c(m('estate'), m('conversion'),
                                                 m('kg_dynamics'), m('evasion')))),
                c('kg_dynamics', 'conversion', 'evasion', 'estate')))

check('unranked families come last, in listed order',
      identical(behavior_family(behavior_order(c(m('ot'), m('charity'),
                                                 m('employment')))),
                c('charity', 'ot', 'employment')))

check('sort is stable within a family',
      identical(behavior_order(c('a/charity/x.R', 'a/charity/y.R')),
                c('a/charity/x.R', 'a/charity/y.R')))

#----------------------
# Resolution behaviour
#----------------------

check('default layer is no response at all', {
  s = behavior_resolve('default')
  identical(s$kg_dynamics, 'none') && length(s$modules) == 0
})

check('an absent cell resolves to the default layer',
      identical(behavior_resolve(NA)$alternative, 'default') &&
      identical(behavior_resolve(NULL)$alternative, 'default') &&
      identical(behavior_resolve('')$alternative, 'default'))

check('the full stack resolves into execution order, applier first', {
  s = behavior_resolve('full')
  identical(s$families, c('kg_dynamics', 'conversion', 'entity_shifting',
                          'evasion', 'charity', 'estate'))
})

check('the applier is injected, not listed', {
  s = behavior_resolve('full')
  identical(s$modules[1], BEHAVIOR_KG_APPLIER) &&
    !(BEHAVIOR_KG_APPLIER %in% s$listed)
})

check('kg_dynamics: none injects no applier',
      length(behavior_resolve('no_kg')$modules) == 1)

check('the list form names pieces with no stamp path', {
  s = behavior_resolve('list_form')
  identical(s$kg_pieces, 'bathtub') && identical(s$kg_dynamics$bathtub, '')
})

check('the mapping form carries each piece its stamp path', {
  s = behavior_resolve('map_form')
  identical(s$kg_pieces, 'bathtub') &&
    identical(s$kg_dynamics$bathtub, 'requirements.txt')
})

check('both kg forms mean the same thing to the activation predicate', {
  si_l = list(resolved_behavior = list(spec = behavior_resolve('list_form')))
  si_m = list(resolved_behavior = list(spec = behavior_resolve('map_form')))
  si_n = list(resolved_behavior = list(spec = behavior_resolve('default')))
  scenario_uses_kg_dynamics(si_l) && scenario_uses_kg_dynamics(si_m) &&
    !scenario_uses_kg_dynamics(si_n)
})

check('unranked-only stacks keep their listed order',
      identical(behavior_resolve('unranked')$families,
                c('charity', 'ot', 'employment')))

expect_error('an unknown alternative folder is refused',
             behavior_resolve('nope'), 'no behavior.yaml')

#---------------------------------------------
# The checks that replaced the in-module ones
#---------------------------------------------

ok = function(alt) {
  suppressWarnings(behavior_validate_spec(behavior_resolve(alt)))
}
bad = function(label, alt, pattern) {
  expect_error(label, behavior_validate_spec(behavior_resolve(alt)), pattern)
}

check('a well-formed stack validates', isTRUE(ok('full')))
check('the default layer validates',   isTRUE(ok('default')))

bad('listing the applier by hand is refused', 'lists_applier',
    'must not be listed under modules')
bad('conversion without the bathtub is refused', 'conv_no_kg',
    'kg_dynamics is none')
bad('a bound piece with no module is refused', 'piece_no_module',
    'no conversion/ module is listed')
bad('a module with an unbound piece is refused', 'module_no_piece',
    'does not bind the `conversion` piece')
bad('kg_dynamics without bathtub is refused', 'no_bathtub',
    'does not bind `bathtub`')
bad('an unknown kg piece is refused', 'bad_piece',
    'unknown kg_dynamics piece')
bad('wealth without estate is refused', 'wealth_no_estate',
    'no estate/ module')
bad('a module file that does not exist is refused', 'missing_file',
    'module file does not exist')
bad('the same module twice is refused', 'dup',
    'listed twice')

check('evasion without estate warns rather than stopping', {
  w = NULL
  withCallingHandlers(
    behavior_validate_spec(behavior_resolve('evasion_no_estate')),
    warning = function(x) { w <<- conditionMessage(x); invokeRestart('muffleWarning') })
  !is.null(w) && grepl('no estate/ module', w)
})

check('the error message shows the order the stack would have run in', {
  err = tryCatch(behavior_validate_spec(behavior_resolve('lists_applier')),
                 error = function(e) conditionMessage(e))
  grepl('Modules run in this order', err)
})

cat(sprintf('\n%d passed, %d failed\n', n_pass, n_fail))
if (n_fail > 0) { cat('BEHAVIOR_LEG_TESTS_FAIL\n'); quit(status = 1) }
cat('BEHAVIOR_LEG_TESTS_PASS\n')

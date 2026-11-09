#-------------------------------------------------------------------------------
# check_behavior_order.R
#
# Migration safety for the behavior leg. Before the rebuild, a runscript's
# behavior cell was a space-delimited module list and the modules ran in the
# order written there. Now the cell names a folder and the loader sorts the
# stack against one pinned family order. Those two things must agree, or the
# rebuild silently changed what the model does.
#
# This reads the PRE-MIGRATION behavior cells out of git (at the commit given,
# defaulting to the commit before the behavior leg went live), resolves each
# one's folder through the live loader, and compares the resulting order to the
# order that was written down. A difference is reported, never accepted.
#
#   sbatch other/config_redesign/run_tests.sbatch . \
#          other/config_redesign/check_behavior_order.R
#-------------------------------------------------------------------------------

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = TRUE)
  ))
)
return_vars = list()
invisible(lapply(
  list.files('./src', pattern = '[.]R$', recursive = TRUE) %>%
    purrr::keep(~ .x != 'main.R' & !startsWith(.x, 'slurm/') &
                  !startsWith(.x, 'tests/') & !startsWith(.x, 'behavior/')),
  function(f) tryCatch(source(file.path('./src', f)),
                       error = function(e)
                         cat('note: could not source', f, '--',
                             conditionMessage(e), '\n'))))

# The commit whose runscripts still carry module lists. Phase 4 step 1 is the
# last such commit; override with a command-line argument if that moves.
args = commandArgs(trailingOnly = TRUE)
REF  = if (length(args) > 0) args[1] else '6891c48de'

# Module list -> the folder it was migrated to. The one place this mapping is
# written down for verification purposes; the runscripts themselves are the
# source of truth for what is live.
MAP = c(
  'entity_shifting/pearce_prisinzano'                                                                                     = 'entity_shifting',
  'kg_dynamics/turnover entity_shifting/pearce_prisinzano'                                                                 = 'kg_entity_shifting',
  'kg_dynamics/turnover'                                                                                                  = 'kg_dynamics_only',
  'kg/62'                                                                                                                 = 'kg_62',
  'kg/70'                                                                                                                 = 'kg_70',
  'kg/72'                                                                                                                 = 'kg_72',
  'employment/bastian'                                                                                                    = 'employment',
  'employment/bastian child_earnings/34'                                                                                   = 'employment_child_earnings',
  'child_earnings/34'                                                                                                     = 'child_earnings',
  'charity/50 employment/bastian child_earnings/34'                                                                        = 'charity_employment_child_earnings',
  'ot/france'                                                                                                             = 'ot_france',
  'ot/france_full'                                                                                                        = 'ot_france_full',
  'ot/france_1yr'                                                                                                         = 'ot_france_1yr',
  'entity_shifting/pearce_prisinzano tips/fringe_low ot/france auto/hanson'                                                = 'obbba_stack',
  'wealth/avoidance estate/avoidance'                                                                                      = 'wealth_estate_avoidance',
  'kg_dynamics/turnover entity_shifting/pearce_prisinzano evasion/debacker charity/100'                                     = 'multi_module_smoke',
  'kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker charity/50'                     = 'top_tax_no_estate',
  'kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker charity/50 estate/avoidance'    = 'top_tax_full',
  'kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker wealth/avoidance charity/50 estate/avoidance' = 'top_tax_full_wealth'
)

# Every runscript that carried a module list at REF, excluding the two sets the
# rebuild deliberately left on the old schema.
listed = system2('git', c('ls-tree', '-r', '--name-only', REF,
                          'config/runscripts'), stdout = TRUE) %>%
  purrr::keep(~ grepl('[.]csv$', .x)) %>%
  purrr::discard(~ grepl('^config/runscripts/(archive|private)/', .x))

n_checked = 0; n_same = 0; problems = c()

for (f in listed) {
  txt = tryCatch(system2('git', c('show', paste0(REF, ':', f)), stdout = TRUE),
                 warning = function(w) character())
  if (length(txt) < 2) next
  rs = tryCatch(readr::read_csv(I(txt), show_col_types = FALSE,
                                col_types = readr::cols(.default = 'c')),
                error = function(e) NULL)
  if (is.null(rs) || !('behavior' %in% names(rs))) next

  for (i in seq_len(nrow(rs))) {
    cell = rs$behavior[i]
    if (is.na(cell) || !nzchar(trimws(cell))) next
    cell = trimws(cell)
    n_checked = n_checked + 1

    folder = MAP[[cell]]
    if (is.null(folder)) {
      problems = c(problems, sprintf('%s [%s]: module list has no folder in MAP: %s',
                                     f, rs$ID[i], cell))
      next
    }

    # What ran before: the cell, verbatim.
    before = strsplit(cell, ' ')[[1]]
    # What runs now: the folder resolved and sorted, with the applier injected.
    after  = behavior_resolve(folder)$modules
    # Compare on {family}/{module}, the form the cell used.
    after_short = sub('^src/behavior/', '', sub('[.]R$', '', after))

    if (identical(before, after_short)) {
      n_same = n_same + 1
    } else {
      problems = c(problems, sprintf(
        '%s [%s]:\n      was: %s\n      now: %s',
        f, rs$ID[i], paste(before, collapse = ' '),
        paste(after_short, collapse = ' ')))
    }
  }
}

cat(sprintf('\n%d behavior cells checked at %s; %d resolve to the same order\n',
            n_checked, REF, n_same))
if (length(problems) > 0) {
  cat('\nORDER OR MAPPING DIFFERENCES (each needs a decision, not a shrug):\n  - ',
      paste(problems, collapse = '\n  - '), '\n', sep = '')
  cat('BEHAVIOR_ORDER_CHECK_FAIL\n')
  quit(status = 1)
}
cat('BEHAVIOR_ORDER_CHECK_PASS\n')

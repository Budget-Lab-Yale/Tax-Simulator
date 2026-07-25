#-----------------------------------------------------------------------------
# cli.R
#
# Shared command-line argument parsing for the two run entry points
# (src/main.R and src/slurm/setup.R). One implementation so the positional
# contract cannot drift between them.
#
# Positional contract (the user_id argument was RETIRED 2026-07-25; it was
# never read by any code):
#
#   main.R:  runscript scenario_id local vintage pct_sample stacked
#            baseline_vintage delete_detail multicore            (9 args)
#   setup.R: runscript scenario_id local vintage pct_sample stacked
#            baseline_vintage delete_detail                      (8 args)
#-----------------------------------------------------------------------------


parse_cli_args = function(args, context = c('main', 'slurm_setup')) {

  #----------------------------------------------------------------------------
  # Parses positional command-line arguments for a simulation run.
  #
  # Parameters:
  #   - args (chr)    : character vector from commandArgs(trailingOnly = TRUE)
  #   - context (str) : 'main' (expects multicore as a 9th arg) or
  #                     'slurm_setup' (8 args; multicore is forced to 'none'
  #                     by the SLURM pipeline)
  #
  # Returns: named list with elements runscript_names, scenario_id, local,
  #          vintage, pct_sample, stacked, baseline_vintage, delete_detail,
  #          multicore ('none' under slurm_setup)
  #----------------------------------------------------------------------------

  context    = match.arg(context)
  n_expected = if (context == 'main') 9L else 8L
  usage = paste0(
    'Usage: Rscript ',
    if (context == 'main') 'src/main.R' else 'src/slurm/setup.R',
    ' <runscript> <scenario_id> <local> <vintage> <pct_sample> <stacked>',
    ' <baseline_vintage> <delete_detail>',
    if (context == 'main') ' <multicore>' else ''
  )

  # The retired user_id argument sat in position 3; an invocation carrying it
  # is exactly one argument too long. Fail with the specific fix rather than
  # a generic usage message.
  if (length(args) == n_expected + 1L) {
    stop('Expected ', n_expected, ' args (got ', length(args), ') -- the ',
         'user_id argument was retired 2026-07-25; remove it (old position 3) ',
         'from your invocation or sbatch script.\n', usage)
  }
  if (length(args) != n_expected) {
    stop('Expected ', n_expected, ' args, got ', length(args), '.\n', usage)
  }

  parse_null = function(x) if (x == 'NULL') NULL else x

  list(
    runscript_names  = args[1],
    scenario_id      = parse_null(args[2]),
    local            = as.integer(args[3]),
    vintage          = parse_null(args[4]),
    pct_sample       = as.numeric(args[5]),
    stacked          = as.integer(args[6]),
    baseline_vintage = parse_null(args[7]),
    delete_detail    = as.integer(args[8]),
    multicore        = if (context == 'main') args[9] else 'none'
  )
}

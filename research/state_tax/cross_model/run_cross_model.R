#-----------------------------------------------------------------------------
# run_cross_model.R
#
# CLI entrypoint for the cross-model state-tax validation harness
# (src/tests/state/test_state_cross_model.R). Run from the repo root:
#
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_tax/cross_model/run_cross_model.R \
#     --states IL --years 2019 --models taxsim [--n 20000] [--n-pe 1500] \
#     [--chunk-size 10000] [--force-prepare] \
#     [--pe-python /path/to/venv/bin/python]
#
#   --states ALL expands to every state with a baseline config
#   --years accepts "2017:2020" or "2019 2020"
#
# Canonical year split is enforced downstream: TAXSIM covers <= 2020,
# PolicyEngine covers >= 2021 (design of record, plan Phase 5).
#-----------------------------------------------------------------------------

if (!file.exists('./src/main.R')) {
  stop('Run from the Tax-Simulator repo root')
}

# Load packages and source model functions (mirrors src/main.R)
suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)
  ))
)
return_vars = list()
list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
  walk(.f = ~ if (.x != 'main.R' && !startsWith(.x, 'slurm/')) source(file.path('./src/', .x)))


#-----------------
# Parse arguments
#-----------------

args = commandArgs(trailingOnly = T)

get_arg = function(flag, default = NULL) {
  i = which(args == flag)
  if (length(i) == 0) return(default)
  vals = c()
  j = i + 1
  while (j <= length(args) && !startsWith(args[j], '--')) {
    vals = c(vals, args[j])
    j = j + 1
  }
  if (length(vals) == 0) return(TRUE)   # bare flag
  return(vals)
}

# Where the harness keeps its two kinds of thing. Code and the machine-read
# known-differences fixture live with the test (cross_model_harness_dir(),
# src/tests/state/cross_model), so their paths are stable under the archiving
# and reorganizing that research/ exists to do. Everything the harness WRITES --
# results, per-state reports, the federal pre-pass cache -- is research evidence
# and lands here, where it gets read, cited and archived from.
records_dir = './research/state_tax/cross_model'

# Regenerate per-state reports from persisted results and exit
if (isTRUE(get_arg('--report-only', FALSE))) {
  paths = cross_model_report(file.path(records_dir, 'results'))
  message('Wrote ', length(paths), ' state reports')
  quit(save = 'no', status = 0)
}

states = get_arg('--states')
if (is.null(states)) stop('--states is required (codes or ALL)')
if (length(states) == 1 && toupper(states) == 'ALL') {
  states = list.dirs('./config/scenarios/tax_law_state/baseline',
                     recursive = F, full.names = F) %>%
    toupper()
}
states = toupper(states)

years_raw = get_arg('--years')
if (is.null(years_raw)) stop('--years is required (e.g. 2019 or 2017:2020)')
years = years_raw %>%
  map(~ if (str_detect(.x, ':')) {
        bounds = as.integer(str_split_1(.x, ':'))
        seq(bounds[1], bounds[2])
      } else as.integer(.x)) %>%
  unlist()

models        = get_arg('--models', c('taxsim', 'policyengine'))
n             = as.integer(get_arg('--n', '20000'))
n_pe          = as.integer(get_arg('--n-pe', '1500'))
chunk_size    = as.integer(get_arg('--chunk-size', '10000'))
force_prepare = isTRUE(get_arg('--force-prepare', FALSE))
venv_python   = get_arg('--pe-python')

out_dir     = get_arg('--out',   file.path(records_dir, 'results'))
cache_dir   = get_arg('--cache', file.path(records_dir, 'cache'))

stopifnot(all(models %in% c('taxsim', 'policyengine')))
if (any(years < 2017 | years > 2024)) {
  stop('Validation window is 2017-2024 (historical microdata years)')
}


#--------------
# Build globals
#--------------

# Baseline-only runscript covering the validation window; local output root
globals = parse_globals(
  runscript_name   = 'tests/cross_model',
  scenario_id      = NULL,
  local            = 1,
  vintage          = NULL,
  baseline_vintage = NULL,
  pct_sample       = 1,
  multicore        = 'none'
)


#-----
# Run
#-----

message('Cross-model validation: ', paste(states, collapse = ' '),
        ' | years ', paste(years, collapse = ' '),
        ' | models ', paste(models, collapse = ' '))

cells = cross_model_run(
  states        = states,
  years         = years,
  models        = models,
  n             = n,
  n_pe          = n_pe,
  out_dir       = out_dir,
  cache_dir     = cache_dir,
  venv_python   = venv_python,
  chunk_size    = chunk_size,
  force_prepare = force_prepare
)

message('\nCell summaries:')
cells %>%
  filter(!is.na(match_100)) %>%
  mutate(across(c(match_15, match_100, share_both_zero), ~ round(.x, 4)),
         across(ends_with('_diff'), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print(row.names = F)

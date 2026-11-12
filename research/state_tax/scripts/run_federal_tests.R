# Run the federal test suites.
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_tax/scripts/run_federal_tests.R         # fixtures only (~1 min)
#   Rscript research/state_tax/scripts/run_federal_tests.R full    # + full-sample microdata; batch job, ~15 GB
suppressPackageStartupMessages(invisible(capture.output(
  lapply(readLines('./requirements.txt'), library, character.only = T))))
return_vars = list()
list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
  walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)))
suites = c('test_tax_law', 'test_salt_workaround', 'test_calc_ptet')
if ('full' %in% commandArgs(trailingOnly = TRUE)) {
  globals = parse_globals(
    runscript_name   = SALT_WORKAROUND_PINNED_RUNSCRIPT,
    scenario_id      = NULL,
    local            = 1,
    vintage          = NULL,
    baseline_vintage = NULL,
    pct_sample       = 1,
    multicore        = 'none'
  )
  suites = c(suites, 'test_salt_workaround_totals')
}
results = map_chr(suites, function(f) {
  r = tryCatch({ get(f)(); 'PASS' }, error = function(e) paste('FAIL:', conditionMessage(e)))
  cat(sprintf('RESULT %-28s %s\n', f, r))
  r
})
if (any(results != 'PASS')) quit(status = 1)
cat('FEDERAL_TESTS_SUCCESS\n')

# Run the state-tax test suites and check the known-differences file loads.
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_tax/scripts/run_state_tests.R            # law + calculator
#   Rscript research/state_tax/scripts/run_state_tests.R quick      # law only (~2 min)
suppressPackageStartupMessages(invisible(capture.output(
  lapply(readLines('./requirements.txt'), library, character.only = T))))
return_vars = list()
list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
  walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)))
suites = if ('quick' %in% commandArgs(trailingOnly = TRUE)) 'test_state_tax_law' else c('test_state_tax_law', 'test_state_calc')
for (f in suites) {
  r = tryCatch({ get(f)(); 'PASS' }, error = function(e) paste('FAIL:', conditionMessage(e)))
  cat(sprintf('RESULT %-22s %s\n', f, r))
}
kd = cross_model_load_known_diffs(cross_model_known_diffs_path())
cat('RESULT known_differences loads:', nrow(kd), 'rows,', sum(kd$action == 'exclude'), 'exclude\n')

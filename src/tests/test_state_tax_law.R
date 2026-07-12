#---------------------------------------------------------------
# Tests for state tax law configuration machinery
#
# Defines functions only (this file is sourced by main.R's recursive
# walk; side effects at source time would break model startup).
#
# Run manually:
#   module load R/4.4.2-gfbf-2024a
#   Rscript -e "
#     suppressPackageStartupMessages(invisible(capture.output(
#       lapply(readLines('./requirements.txt'), library, character.only = T))));
#     return_vars = list();
#     list.files('./src', recursive = T) %>%
#       walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)));
#     test_state_tax_law()"
#---------------------------------------------------------------


test_state_tax_law = function() {

  #----------------------------------------------------------------------------
  # Runs all state tax law configuration tests, stopping on first failure.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  test_reference_key_tolerance()
  test_pilot_state_values()
  message('test_state_tax_law: ALL TESTS PASSED')
  invisible(TRUE)
}



test_reference_key_tolerance = function() {

  #----------------------------------------------------------------------------
  # Regression test for the "reference" metadata key (plan §2.2): parsing a
  # parameter whose subparameters carry a "reference" citation must succeed and
  # produce output IDENTICAL to the same parameter without the key. The
  # critical case is an INDEXED subparameter under indexation_defaults -- the
  # historical failure mode was a map2() length mismatch in parse_subparam()
  # (an unindexed-only test passes vacuously via the early return).
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  yaml_with_ref = "
indexation_defaults:
  i_measure:
    '2017': cpi
  i_base_year: 2020
  i_direction: 0
  i_increment: 10

indexed_subparam:
  value: 1000
  reference: 'Form TEST line 1; 35 ILCS 5/000; https://example.gov'
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default

unindexed_subparam:
  value: 5
  reference: 'Form TEST line 2'
"
  yaml_without_ref = yaml_with_ref %>%
    str_remove_all('\n  reference: .*')

  # Constant-growth synthetic index series
  test_indexes = expand_grid(series = c('cpi'), year = 2015:2035) %>%
    mutate(growth = 0.02)

  parse_test_yaml = function(yaml_text) {
    read_yaml(text = yaml_text) %>%
      parse_param(name = 'test', years = 2017:2035, indexes = test_indexes)
  }

  with_ref = parse_test_yaml(yaml_with_ref)
  no_ref   = parse_test_yaml(yaml_without_ref)

  stopifnot(
    'reference key changed parsed output' =
      identical(with_ref, no_ref),
    'unindexed subparam value wrong' =
      all(filter(with_ref, subparameter == 'unindexed_subparam')$value == 5),
    'indexed subparam not growing after base year' =
      filter(with_ref, subparameter == 'indexed_subparam', year == 2035)$value[1] >
      filter(with_ref, subparameter == 'indexed_subparam', year == 2021)$value[1]
  )

  message('test_reference_key_tolerance: PASSED')
  invisible(TRUE)
}

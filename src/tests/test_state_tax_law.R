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



test_pilot_state_values = function() {

  #----------------------------------------------------------------------------
  # Spot-checks parsed pilot-state tax law values against primary-source
  # figures (see reference fields in config/scenarios/tax_law_state/baseline).
  # Uses synthetic constant-growth indexes; asserted values are all
  # transcribed (pre-projection) years, so they do not depend on the index.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  test_indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)

  law = build_state_tax_law(states  = c('IL', 'CO', 'NY'),
                            years   = 2017:2035,
                            indexes = test_indexes)

  pick = function(st, yr, fs, var) {
    law %>%
      filter(state == st, year == yr, filing_status == fs) %>%
      pull(!!sym(var))
  }

  stopifnot(
    # Structure: all states, all years x 4 filing statuses, no NA core cols.
    # Narrow year windows must also parse (every year-keyed value list is
    # anchored at 2017 -- plan §2.2 encoding convention)
    'row count wrong'   = nrow(law) == 3 * length(2017:2035) * 4,
    'single-year build broke' =
      nrow(build_state_tax_law(c('IL', 'CO', 'NY'), 2020, test_indexes)) == 12,
    'core rates NA'     = !anyNA(law$st_ord.rates1),
    'start_point NA'    = !anyNA(law$st_agi.start_point),

    # Illinois (see il/ yaml reference fields)
    'IL 2017 blended rate'   = pick('IL', 2017, 1, 'st_ord.rates1') == 0.043549,
    'IL 2018 rate'           = pick('IL', 2018, 1, 'st_ord.rates1') == 0.0495,
    'IL 2024 exemption'      = pick('IL', 2024, 1, 'st_exempt.personal_amount') == 2775,
    'IL 2023 frozen exemption' = pick('IL', 2023, 1, 'st_exempt.personal_amount') == 2425,
    'IL exemption cliff'     = pick('IL', 2020, 2, 'st_exempt.po_thresh') == 500000,
    'IL 2017 EITC 14pct'     = pick('IL', 2017, 1, 'st_credits.eitc_match') == 0.14,
    'IL 2023 EITC 20pct'     = pick('IL', 2023, 1, 'st_credits.eitc_match') == 0.20,
    'IL starts from fed AGI' = pick('IL', 2020, 1, 'st_agi.start_point') == 1,

    # Colorado (see co/ yaml reference fields)
    'CO 2017 rate'           = pick('CO', 2017, 1, 'st_ord.rates1') == 0.0463,
    'CO 2019 TABOR rate'     = pick('CO', 2019, 1, 'st_ord.rates1') == 0.045,
    'CO 2021 TABOR rate'     = pick('CO', 2021, 1, 'st_ord.rates1') == 0.045,
    'CO 2024 TABOR rate'     = pick('CO', 2024, 1, 'st_ord.rates1') == 0.0425,
    'CO 2025 rate'           = pick('CO', 2025, 1, 'st_ord.rates1') == 0.044,
    'CO starts from fed txbl' = pick('CO', 2020, 1, 'st_agi.start_point') == 2,
    'CO EITC 50pct 2023-25'  = pick('CO', 2023, 1, 'st_credits.eitc_match') == 0.50 &&
                               pick('CO', 2025, 1, 'st_credits.eitc_match') == 0.50,
    'CO EITC 25pct 2026'     = pick('CO', 2026, 1, 'st_credits.eitc_match') == 0.25,
    'CO addback three regimes' =
      pick('CO', 2022, 2, 'st_ded.addback_cap') == 60000 &&
      pick('CO', 2023, 2, 'st_ded.addback_cap') == 16000 &&
      pick('CO', 2026, 2, 'st_ded.addback_cap') == 2000,
    'CO addback thresh'      = pick('CO', 2022, 1, 'st_ded.addback_cap_thresh') == 400000 &&
                               pick('CO', 2023, 1, 'st_ded.addback_cap_thresh') == 300000,
    'CO SS full sub 65+ 2022' = pick('CO', 2021, 1, 'st_agi.ss_full_sub_65plus') == 0 &&
                                pick('CO', 2022, 1, 'st_agi.ss_full_sub_65plus') == 1,
    'CO pension caps'        = pick('CO', 2020, 1, 'st_agi.pension_excl_65plus') == 24000 &&
                               pick('CO', 2020, 1, 'st_agi.pension_excl_under65') == 20000,
    'CO FATC 2024-25 only'   = pick('CO', 2024, 1, 'st_credits.fatc_young_amount') == 3200 &&
                               pick('CO', 2025, 1, 'st_credits.fatc_young_amount') == 3273 &&
                               pick('CO', 2026, 1, 'st_credits.fatc_young_amount') == 0,

    # New York (see ny/ yaml reference fields)
    'NY 2021 top rates'      = pick('NY', 2021, 1, 'st_ord.rates8')  == 0.0965 &&
                               pick('NY', 2021, 1, 'st_ord.rates10') == 0.109,
    'NY 2033 reversion'      = pick('NY', 2033, 1, 'st_ord.rates8')  == 0.0882,
    'NY 2026 bottom rate cut' = pick('NY', 2026, 1, 'st_ord.rates1') == 0.039,
    'NY single top bracket'  = pick('NY', 2021, 1, 'st_ord.brackets8') == 1077550,
    'NY MFS uses single schedule' =
      pick('NY', 2021, 3, 'st_ord.brackets8') == 1077550,
    'NY joint top bracket'   = pick('NY', 2021, 2, 'st_ord.brackets8') == 2155350,
    'NY std deduction joint' = pick('NY', 2024, 2, 'st_ded.std_amount') == 16050,
    'NY dep exemption'       = pick('NY', 2024, 1, 'st_exempt.dep_amount') == 1000 &&
                               pick('NY', 2024, 1, 'st_exempt.personal_amount') == 0,
    'NY EITC 30pct'          = pick('NY', 2020, 1, 'st_credits.eitc_match') == 0.30,
    'NY pease 2017 single'   = pick('NY', 2017, 1, 'st_ded.pease_thresh') == 261500,
    'NY pease 2023 joint'    = pick('NY', 2023, 2, 'st_ded.pease_thresh') == 375850,
    'NY ESCC 2025 restructure' = pick('NY', 2025, 1, 'st_credits.ctc_old_amount') == 330 &&
                                 pick('NY', 2026, 1, 'st_credits.ctc_old_amount') == 500,
    'NY itemized decoupling' = pick('NY', 2017, 1, 'st_ded.item_coupling') == 1 &&
                               pick('NY', 2018, 1, 'st_ded.item_coupling') == 0,
    'NY filing threshold'    = pick('NY', 2020, 1, 'st_filing.req_income_thresh') == 4000
  )

  message('test_pilot_state_values: PASSED')
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

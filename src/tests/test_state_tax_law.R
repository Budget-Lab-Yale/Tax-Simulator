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
  test_state_registry()
  test_nm_std_mirrors_federal()
  test_state_yaml_conventions()
  test_state_param_validation()
  test_state_rollout_tracker()
  test_pilot_state_values()
  test_first_wave_state_values()
  test_special_state_values()
  test_state_credit_tables()
  test_state_conformity_groups()
  test_state_reference_law_builder()
  test_state_reference_context_builder()
  test_state_conformity_context_routing()
  test_state_federal_conformity_guard()
  message('test_state_tax_law: ALL TESTS PASSED')
  invisible(TRUE)
}



test_state_credit_tables = function() {

  #----------------------------------------------------------------------------
  # Confirms the generic dense-table loader preserves California's published
  # CalEITC values, the 2017 footnote tail, and explicit future carry-forward.
  #----------------------------------------------------------------------------

  law = build_state_tax_law(
    states = 'CA', years = 2017:2026,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
              mutate(growth = 0.025)
  )
  table = attr(law, 'credit_tables')
  lookup = function(table_year, table_children, income) {
    table %>%
      filter(year == table_year, key_concept == table_children,
             income_lower <= income, income_upper >= income) %>%
      pull(value)
  }

  stopifnot(
    'CA table row count wrong' = nrow(table) == 20738,
    'CA 2025 two-child peak wrong' = lookup(2025, 2, 9825) == 3339,
    'CA 2017 footnote tail wrong' = lookup(2017, 0, 15008) == 1,
    'CA 2017 footnote end wrong' = length(lookup(2017, 0, 15009)) == 0,
    'CA 2020 malformed HTML band repair failed' =
      lookup(2020, 0, 86) == 5 && lookup(2020, 0, 1101) == 73,
    'CA table future carry-forward wrong' =
      all(state_credit_tables_for_year(table, 'CA', 2026)$year == 2025)
  )

  message('test_state_credit_tables: PASSED')
  invisible(TRUE)
}



test_state_federal_conformity_guard = function() {

  #----------------------------------------------------------------------------
  # Fixed/selective states must not silently consume a federal reform's AGI
  # until the shared reference-law context is available. Rolling states remain
  # eligible for those calculations.
  #----------------------------------------------------------------------------

  indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  ca_law = build_state_tax_law('CA', 2025, indexes)
  il_law = build_state_tax_law('IL', 2025, indexes)

  blocked = tryCatch(
    {
      validate_state_federal_conformity(ca_law, 'tests/federal_reform')
      FALSE
    },
    error = function(e) str_detect(conditionMessage(e), 'fixed/selective') &&
                        str_detect(conditionMessage(e), 'CA')
  )

  stopifnot(
    'baseline federal law should be permitted for fixed states' =
      isTRUE(validate_state_federal_conformity(ca_law, 'baseline')),
    'fixed-conformity state did not block a federal reform' = blocked,
    'rolling-conformity state was incorrectly blocked' =
      isTRUE(validate_state_federal_conformity(il_law, 'tests/federal_reform'))
  )

  message('test_state_federal_conformity_guard: PASSED')
  invisible(TRUE)
}



test_state_conformity_groups = function() {

  #----------------------------------------------------------------------------
  # Locks the shared numeric-group contract and California's two dated group
  # assignments. The groups remain intentionally unavailable until their
  # selective-adoption overlays are researched and validated.
  #----------------------------------------------------------------------------

  indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  groups = load_state_conformity_groups()
  ca_law = build_state_tax_law('CA', 2017:2025, indexes)
  ca_2017 = state_conformity_groups_for_law(
    filter(ca_law, year == 2017), groups
  )
  ca_2025 = state_conformity_groups_for_law(
    filter(ca_law, year == 2025), groups
  )

  # Regression: a mixed list of a group-declaring state (CA) and a
  # non-declaring rolling state (IL) leaves st_agi.conformity_group NA for IL
  # after the wide pivot. Resolution must NOT error, and IL must map to the
  # rolling group 0. build_state_reference_tax_laws exercises the same path.
  mixed_law = build_state_tax_law(c('IL', 'CA'), 2025, indexes)
  mixed = state_conformity_groups_for_law(mixed_law, groups)
  mixed_refs = build_state_reference_tax_laws(mixed_law, indexes, groups)

  stopifnot(
    'rolling group is malformed' =
      nrow(filter(groups, conformity_group == 0, ready)) == 1,
    'California 2017-2024 group wrong' =
      ca_2017$conformity_group == 1 && !ca_2017$ready,
    'California 2025+ group wrong' =
      ca_2025$conformity_group == 2 && !ca_2025$ready,
    'IL in a mixed list must resolve to rolling group 0' =
      mixed$conformity_group[mixed$state == 'IL'] == 0,
    'CA in a mixed list must keep its declared group' =
      mixed$conformity_group[mixed$state == 'CA'] == 2,
    'mixed-list reference build must not error and skips unready CA' =
      length(mixed_refs) == 0
  )

  message('test_state_conformity_groups: PASSED')
  invisible(TRUE)
}



test_state_reference_law_builder = function() {

  #----------------------------------------------------------------------------
  # Uses an existing federal test overlay as a stand-in reference package to
  # prove the generic builder constructs one law per numeric group, independent
  # of state identity. This is not a California legal bridge.
  #----------------------------------------------------------------------------

  indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  groups = tibble(
    conformity_group = c(0L, 7L),
    label = c('rolling', 'test_reference'),
    ready = c(TRUE, TRUE),
    reference_tax_law_id = c(NA_character_, 'tests/wo_ot')
  )
  law = build_state_tax_law('IL', 2025, indexes) %>%
    mutate(st_agi.conformity_group = 7)
  reference_laws = build_state_reference_tax_laws(law, indexes, groups)

  stopifnot(
    'reference law was not keyed by group' = identical(names(reference_laws), '7'),
    'reference overlay was not parsed' =
      is.infinite(reference_laws[['7']]$below.tip_ded_limit[1])
  )

  message('test_state_reference_law_builder: PASSED')
  invisible(TRUE)
}



test_state_reference_context_builder = function() {

  #----------------------------------------------------------------------------
  # Exercises the context handoff without requiring a full Tax-Data record.
  # The mock calculator exposes the inputs it receives: reference policy must
  # replace scenario policy, the original filing status must be restored, and
  # a post-behavior wage change must survive into the reference pass.
  #----------------------------------------------------------------------------

  normal_law = tibble(year = 2025, filing_status = 1, test_policy = 1)
  reference_laws = list(
    '7' = tibble(year = 2025, filing_status = 1, test_policy = 2)
  )
  scenario_calc = tibble(
    id = 1, year = 2025, filing_status = 4, filing_status_input = 1,
    test_policy = 1, wages = 51000, agi = 41000, expanded_inc = 51000
  )
  original_do_taxes = do_taxes
  assign('do_taxes', function(tax_units, ...) tax_units, envir = .GlobalEnv)
  on.exit(assign('do_taxes', original_do_taxes, envir = .GlobalEnv), add = TRUE)

  contexts = build_state_reference_contexts(
    tax_units_calc     = scenario_calc,
    normal_tax_law     = normal_law,
    reference_tax_laws = reference_laws,
    vars_1040          = character()
  )
  context = contexts[['7']]

  stopifnot(
    'reference context group key wrong' = identical(names(contexts), '7'),
    'reference law did not replace scenario law' = context$test_policy == 2,
    'reference context did not restore filed status' = context$filing_status == 1,
    'reference context lost post-behavior income' = context$wages == 51000,
    'scenario calculation output was not cleared' = !('expanded_inc' %in% names(context))
  )

  message('test_state_reference_context_builder: PASSED')
  invisible(TRUE)
}



test_state_conformity_context_routing = function() {

  #----------------------------------------------------------------------------
  # A state must use the group context rather than scenario federal outputs.
  # The reference unit's $51k AGI represents a post-behavior record evaluated
  # under a fixed definition; the scenario unit's $41k AGI represents a reform
  # that changed the federal definition. Reusing group 7 is the cache contract.
  #----------------------------------------------------------------------------

  indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  groups = tibble(
    conformity_group = c(0L, 7L),
    label = c('rolling', 'test_reference'),
    ready = c(TRUE, TRUE),
    reference_tax_law_id = c(NA_character_, 'tests/wo_ot')
  )
  law = build_state_tax_law('IL', 2025, indexes) %>%
    mutate(st_agi.conformity_group = 7)
  scenario_context = st_test_unit(list(agi = 41000)) %>%
    mutate(id = 1, year = 2025)
  reference_context = scenario_context %>% mutate(agi = 51000)
  weights = tibble(id = 1, state = 'IL', weight = 1)

  fixed = get_state_totals(
    scenario_context, law, weights, 2025,
    state_tax_contexts = list('7' = reference_context),
    conformity_groups = groups
  )
  rolling = get_state_totals(
    scenario_context,
    mutate(law, st_agi.conformity_group = 0),
    weights, 2025,
    conformity_groups = groups
  )
  fixed_same = get_state_totals(
    scenario_context, law, weights, 2025,
    state_tax_contexts = list('7' = scenario_context),
    conformity_groups = groups
  )
  missing_context = tryCatch(
    {
      get_state_totals(
        scenario_context, law, weights, 2025,
        state_tax_contexts = list(), conformity_groups = groups
      )
      FALSE
    },
    error = function(e) str_detect(conditionMessage(e), 'Missing reference-law')
  )
  fixed_agi = fixed %>% filter(variable == 'st_agi') %>% pull(value)
  rolling_agi = rolling %>% filter(variable == 'st_agi') %>% pull(value)

  stopifnot(
    'fixed group did not use reference context' = fixed_agi == 51000,
    'rolling group did not use scenario context' = rolling_agi == 41000,
    'identical laws should produce identical state results' =
      identical(rolling %>% arrange(variable), fixed_same %>% arrange(variable)),
    'ready group accepted a missing context' = missing_context
  )

  message('test_state_conformity_context_routing: PASSED')
  invisible(TRUE)
}



test_state_registry = function() {

  registry_path = './config/scenarios/tax_law_state/jurisdictions.yaml'
  baseline_root = './config/scenarios/tax_law_state/baseline'
  registry = read_yaml(registry_path)
  profiles = c('broad_iit', 'zero', 'narrow_investment_iit',
               'capital_gains_and_transfer')
  registered = sort(toupper(names(registry)))
  configured = sort(toupper(basename(list.dirs(baseline_root,
                                                recursive = FALSE))))
  enabled = sort(toupper(names(registry)[map_lgl(registry, ~ isTRUE(.x$enabled))]))

  stopifnot(
    'state registry has invalid profile' =
      all(map_chr(registry, 'profile') %in% profiles),
    'configured state missing from registry' = identical(configured, registered),
    'states = all does not use enabled registry' =
      identical(sort(parse_states_value('all')), enabled)
  )

  message('test_state_registry: PASSED')
  invisible(TRUE)
}



test_nm_std_mirrors_federal = function() {

  #----------------------------------------------------------------------------
  # New Mexico's PIT-1 line 12 subtracts the federal standard or itemized
  # deduction ON THE RETURN, so baseline/nm/ded.yaml duplicates the federal
  # std.yaml rather than inheriting it through a federal-taxable-income start
  # (which would also hand NM the QBI and OBBBA below-the-line deductions --
  # $304 of tax on test NM-3 alone). A duplicate silently drifts, so this test
  # holds the two together: every year from 2018 and every filing status, NM's
  # standard-deduction parameters must equal the federal ones under the SAME
  # index. 2017 is excluded by design -- NM starts from federal taxable income
  # that year, so its state deduction is deliberately zero.
  #
  # If this fails after a federal standard-deduction change, the fix is to
  # update baseline/nm/ded.yaml, not to relax the test.
  #
  # Returns: TRUE invisibly if the mirror holds (throws otherwise).
  #----------------------------------------------------------------------------

  years   = 2017:2030
  indexes = expand_grid(series = c('cpi', 'chained_cpi'), year = 2015:2036) %>%
            mutate(growth = 0.025)

  nm = build_state_tax_law(states = 'NM', years = years, indexes = indexes) %>%
    filter(year >= 2018) %>%
    select(year, filing_status,
           std_amount = st_ded.std_amount,
           aged       = st_ded.std_aged_addl,
           blind      = st_ded.std_blind_addl,
           dep_floor  = st_ded.std_dependent_floor,
           dep_earned = st_ded.std_dependent_earned_add)

  fed = build_tax_law_from_id('baseline', years, indexes) %>%
    filter(year >= 2018) %>%
    select(year, filing_status,
           fed_std_amount = std.value,
           fed_aged       = std.bonus,
           fed_blind      = std.bonus,
           fed_dep_floor  = std.dep_floor,
           fed_dep_earned = std.dep_earned_bonus)

  joined = nm %>% inner_join(fed, by = c('year', 'filing_status'))
  stopifnot('NM/federal std comparison lost rows' =
              nrow(joined) == nrow(nm) && nrow(nm) > 0)

  mismatched = joined %>%
    filter(std_amount != fed_std_amount | aged != fed_aged |
           blind != fed_blind | dep_floor != fed_dep_floor |
           dep_earned != fed_dep_earned)

  if (nrow(mismatched) > 0) {
    stop('baseline/nm/ded.yaml has drifted from the federal std.yaml in ',
         nrow(mismatched), ' year-status cells, first at ',
         mismatched$year[1], ' status ', mismatched$filing_status[1],
         ': NM std ', mismatched$std_amount[1], ' vs federal ',
         mismatched$fed_std_amount[1])
  }

  message('test_nm_std_mirrors_federal: PASSED (', nrow(joined),
          ' year-status cells)')
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



test_first_wave_state_values = function() {

  #----------------------------------------------------------------------------
  # Spot-checks the first broad-IIT rollout states against the values
  # transcribed from their primary DOR forms.  These tests also lock the
  # reusable child-deduction, retirement-exclusion, and dependent-credit
  # configuration shapes before later states reuse them.
  #----------------------------------------------------------------------------

  test_indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  law = build_state_tax_law(states = c('AZ', 'GA', 'NC'),
                            years = 2017:2035,
                            indexes = test_indexes)

  pick = function(st, yr, fs, var) {
    law %>%
      filter(state == st, year == yr, filing_status == fs) %>%
      pull(!!sym(var))
  }

  stopifnot(
    'first-wave state row count wrong' =
      nrow(law) == 3 * length(2017:2035) * 4,

    # Arizona Form 140 / Tax Tables X and Y
    'AZ 2017 graduated schedule wrong' =
      pick('AZ', 2017, 1, 'st_ord.rates1') == 0.0259 &&
      pick('AZ', 2017, 1, 'st_ord.rates5') == 0.0454,
    'AZ 2022 two-rate schedule wrong' =
      pick('AZ', 2022, 1, 'st_ord.rates1') == 0.0255 &&
      pick('AZ', 2022, 1, 'st_ord.rates2') == 0.0298,
    'AZ 2025 standard deduction and charity add-on wrong' =
      pick('AZ', 2025, 1, 'st_ded.std_amount') == 15750 &&
      pick('AZ', 2025, 1, 'st_ded.std_char_share') == 0.34,
    'AZ dependent credit starts in 2019' =
      pick('AZ', 2018, 1, 'st_credits.dep_credit_style') == 0 &&
      pick('AZ', 2019, 1, 'st_credits.dep_credit_style') == 1 &&
      pick('AZ', 2019, 1, 'st_credits.dep_credit_young_amount') == 100,

    # Georgia Form 500 / IND-CR 202
    'GA historical and current rates wrong' =
      pick('GA', 2017, 1, 'st_ord.rates6') == 0.06 &&
      pick('GA', 2019, 1, 'st_ord.rates6') == 0.0575 &&
      pick('GA', 2024, 1, 'st_ord.rates1') == 0.0539 &&
      pick('GA', 2025, 1, 'st_ord.rates1') == 0.0519,
    'GA retirement earned cap wrong' =
      pick('GA', 2023, 1, 'st_agi.retirement_excl_earned_cap') == 4000 &&
      pick('GA', 2024, 1, 'st_agi.retirement_excl_earned_cap') == 5000 &&
      pick('GA', 2025, 1, 'st_agi.retirement_excl_65plus') == 65000,
    'GA 2024 exemption transition wrong' =
      pick('GA', 2023, 1, 'st_exempt.personal_amount') == 2700 &&
      pick('GA', 2024, 1, 'st_exempt.personal_amount') == 0 &&
      pick('GA', 2024, 1, 'st_exempt.dep_amount') == 4000,
    'GA care-credit match and refundability wrong' =
      pick('GA', 2024, 1, 'st_credits.cdctc_match') == 0.30 &&
      pick('GA', 2025, 1, 'st_credits.cdctc_match') == 0.50 &&
      pick('GA', 2025, 1, 'st_credits.cdctc_refundable') == 0,

    # North Carolina D-400 / G.S. 105-153.5 and 105-153.7
    'NC enacted rate sequence wrong' =
      pick('NC', 2017, 1, 'st_ord.rates1') == 0.05499 &&
      pick('NC', 2024, 1, 'st_ord.rates1') == 0.045 &&
      pick('NC', 2025, 1, 'st_ord.rates1') == 0.0425 &&
      pick('NC', 2026, 1, 'st_ord.rates1') == 0.0399,
    'NC standard deduction history wrong' =
      pick('NC', 2021, 1, 'st_ded.std_amount') == 10750 &&
      pick('NC', 2022, 1, 'st_ded.std_amount') == 12750,
    'NC child deduction table wrong' =
      pick('NC', 2017, 1, 'st_child_ded.style') == 0 &&
      pick('NC', 2018, 1, 'st_child_ded.style') == 1 &&
      pick('NC', 2018, 1, 'st_child_ded.amounts1') == 2500 &&
      pick('NC', 2022, 1, 'st_child_ded.amounts1') == 3000,
    'NC component itemization wrong' =
      pick('NC', 2025, 1, 'st_ded.item_component_style') == 1 &&
      pick('NC', 2025, 3, 'st_ded.item_prop_tax_cap') == 5000
  )

  message('test_first_wave_state_values: PASSED')
  invisible(TRUE)
}



test_special_state_values = function() {

  test_indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)
  law = build_state_tax_law(states = c('NH', 'TN', 'WA'),
                            years = 2017:2035,
                            indexes = test_indexes)

  pick = function(st, yr, fs, var) {
    law %>%
      filter(state == st, year == yr, filing_status == fs) %>%
      pull(!!sym(var))
  }

  stopifnot(
    'special-state row count wrong' = nrow(law) == 3 * length(2017:2035) * 4,
    'NH program ends in 2025' =
      pick('NH', 2024, 1, 'st_programs.narrow_iit') == 1 &&
      pick('NH', 2025, 1, 'st_programs.narrow_iit') == 0,
    'NH rate schedule wrong' =
      pick('NH', 2017, 1, 'st_investment_income.rate') == 0.05 &&
      pick('NH', 2023, 1, 'st_investment_income.rate') == 0.04 &&
      pick('NH', 2024, 1, 'st_investment_income.rate') == 0.03,
    'TN rate schedule wrong' =
      pick('TN', 2017, 1, 'st_investment_income.rate') == 0.04 &&
      pick('TN', 2018, 1, 'st_investment_income.rate') == 0.03 &&
      pick('TN', 2019, 1, 'st_investment_income.rate') == 0.02 &&
      pick('TN', 2020, 1, 'st_investment_income.rate') == 0.01 &&
      pick('TN', 2021, 1, 'st_investment_income.rate') == 0,
    'TN age-100 change wrong' =
      pick('TN', 2017, 1, 'st_investment_income.age_100_full_exempt') == 0 &&
      pick('TN', 2018, 1, 'st_investment_income.age_100_full_exempt') == 1,
    'WA capital gains starts in 2022' =
      pick('WA', 2021, 1, 'st_programs.ltcg_excise') == 0 &&
      pick('WA', 2022, 1, 'st_programs.ltcg_excise') == 1,
    'WA 2025 capital gains changes wrong' =
      pick('WA', 2025, 1, 'st_capital_gains.standard_deduction') == 278000 &&
      pick('WA', 2025, 1, 'st_capital_gains.surtax_rate') == 0.029 &&
      pick('WA', 2025, 1, 'st_capital_gains.surtax_threshold') == 1000000,
    'WA WFTC amounts wrong' =
      pick('WA', 2022, 1, 'st_transfers.wftc_max_amount2') == 600 &&
      pick('WA', 2025, 1, 'st_transfers.wftc_max_amount1') == 335 &&
      pick('WA', 2025, 2, 'st_transfers.wftc_max_income_joint2') == 57554,
    'WA WFTC MFS change wrong' =
      pick('WA', 2022, 3, 'st_transfers.wftc_mfs_eligible') == 0 &&
      pick('WA', 2023, 3, 'st_transfers.wftc_mfs_eligible') == 1
  )

  message('test_special_state_values: PASSED')
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



test_state_yaml_conventions = function() {

  #----------------------------------------------------------------------------
  # Enforces baseline state-YAML authoring conventions that make the rollout
  # maintainable at scale: each registered profile has its required files,
  # every subparameter has a reference citation, and every year-keyed value
  # series is anchored at 2017 or earlier.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  baseline_root = './config/scenarios/tax_law_state/baseline'
  registry = read_yaml('./config/scenarios/tax_law_state/jurisdictions.yaml')
  required_files = list(
    broad_iit = c('agi.yaml', 'credits.yaml', 'ded.yaml',
                  'exempt.yaml', 'filing.yaml', 'ord.yaml'),
    zero = c('agi.yaml', 'credits.yaml', 'ded.yaml',
             'exempt.yaml', 'filing.yaml', 'ord.yaml'),
    narrow_investment_iit = c('programs.yaml', 'investment_income.yaml'),
    capital_gains_and_transfer = c('programs.yaml', 'capital_gains.yaml',
                                   'transfers.yaml')
  )

  state_dirs = list.dirs(path = baseline_root,
                         recursive = FALSE,
                         full.names = TRUE)

  missing_files = c()
  convention_errors = c()

  for (state_dir in state_dirs) {
    state = toupper(basename(state_dir))
    profile = registry[[state]]$profile
    missing = setdiff(required_files[[profile]], basename(list.files(state_dir,
                                                                      full.names = TRUE)))
    if (length(missing) > 0) {
      missing_files = c(
        missing_files,
        paste0(basename(state_dir), ': missing ', paste(missing, collapse = ', '))
      )
    }

    for (yaml_file in list.files(state_dir, pattern = '\\.yaml$', full.names = TRUE)) {
      raw = read_yaml(yaml_file)
      param_names = setdiff(names(raw), c('indexation_defaults',
                                          'filing_status_mapper',
                                          'documented_not_modeled'))

      # Documentation-only entries are skipped by the parser but must still
      # carry citations (they are transcription, held to the same standard)
      for (doc_name in names(raw$documented_not_modeled)) {
        doc_entry = raw$documented_not_modeled[[doc_name]]
        if (!is.list(doc_entry) ||
            is.null(doc_entry$reference) ||
            !nzchar(trimws(as.character(doc_entry$reference)))) {
          convention_errors = c(
            convention_errors,
            paste0(basename(state_dir), '/', basename(yaml_file), ': ',
                   'documented_not_modeled entry ', doc_name,
                   ' missing reference')
          )
        }
      }

      for (param_name in param_names) {
        subparam = raw[[param_name]]

        if (is.null(subparam$reference) ||
            !nzchar(trimws(as.character(subparam$reference)))) {
          convention_errors = c(
            convention_errors,
            paste0(basename(state_dir), '/', basename(yaml_file), ': ',
                   param_name, ' missing reference')
          )
        }

        value_names = names(subparam$value)
        if (!is.null(value_names) &&
            length(value_names) > 0 &&
            all(!is.na(value_names)) &&
            all(str_detect(value_names, '^\\d{4}$')) &&
            min(as.integer(value_names)) > 2017) {
          convention_errors = c(
            convention_errors,
            paste0(basename(state_dir), '/', basename(yaml_file), ': ',
                   param_name, ' value series starts after 2017')
          )
        }
      }
    }
  }

  if (length(missing_files) > 0 || length(convention_errors) > 0) {
    stop(
      c(
        'State YAML convention failures:',
        missing_files,
        convention_errors
      ) %>% paste(collapse = '\n')
    )
  }

  message('test_state_yaml_conventions: PASSED')
  invisible(TRUE)
}



test_state_param_validation = function() {

  #----------------------------------------------------------------------------
  # Exercises the load-time parameter-name validator (2026-07-17 review items
  # #1/#2): (a) every configured state passes the validator -- the standing
  # retroactive audit that no encoded YAML carries a name the calculators do
  # not read; (b) documentation-only entries quarantined under
  # documented_not_modeled never surface as parameter columns; (c) an
  # unknown/misspelled parameter fails loudly at load time instead of
  # silently defaulting the intended parameter to a no-op.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  test_indexes = expand_grid(series = 'cpi', year = 2015:2036) %>%
    mutate(growth = 0.025)

  # Schema integrity (params_schema.yaml -> st_param_schema()): defaults
  # resolve with correct .inf/-.inf/null handling, and every feature-gate
  # sentinel is a member of a declared vector family
  schema_defaults = st_param_defaults()
  schema_registry = st_param_name_registry()
  stopifnot(
    'schema defaults are missing or unnamed' =
      length(schema_defaults) > 150 && !is.null(names(schema_defaults)),
    'schema .inf default did not parse as numeric infinity' =
      is.infinite(schema_defaults[['st_agi.ss_5564_agi_limit']]) &&
      schema_defaults[['st_agi.ss_5564_agi_limit']] > 0,
    'schema -.inf default did not parse as negative infinity' =
      schema_defaults[['st_transfers.wftc_max_age']] == -Inf,
    'schema null default did not parse as NA' =
      is.na(schema_defaults[['st_ded.std_dependent']]),
    'a sentinel does not match any declared family pattern' =
      all(map_lgl(st_param_vector_sentinels(),
                  ~ any(str_detect(.x, schema_registry$families))))
  )

  # (a) all configured states parse through the validator
  configured = list.dirs('./config/scenarios/tax_law_state/baseline',
                         recursive = FALSE) %>%
    basename() %>%
    toupper()
  law = build_state_tax_law(configured, 2017:2035, test_indexes)

  # (b) quarantined documentation-only entries must not become columns
  stopifnot(
    'documented_not_modeled entries leaked into parsed law' =
      !any(c('st_agi.conformity_year', 'st_agi.sub_529_single',
             'st_agi.sub_529_joint', 'st_agi.sub_529_cap',
             'st_agi.govt_pension_full_sub', 'st_credits.k12_credit_rate',
             'st_credits.tuition_credit_rate', 'st_ded.item_base_pre_tcja',
             'st_ded.salt_cap_applies') %in% names(law))
  )

  # (c) an unknown name fails at load time, identifying the state and name
  tmp_root = file.path(tempdir(), 'state_param_validation')
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  zz_dir = file.path(tmp_root, 'baseline', 'zz')
  dir.create(zz_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c('rates:',
               '  value: 0.05',
               "  reference: 'test schedule'"),
             file.path(zz_dir, 'ord.yaml'))
  writeLines(c('start_point:',
               '  value: 1',
               "  reference: 'test starting point'",
               'pension_sub_shre:',
               '  value: 1.0',
               "  reference: 'test (misspelled on purpose)'",
               'documented_not_modeled:',
               '  doc_only_entry:',
               '    value: 42',
               "    reference: 'test doc-only entry'"),
             file.path(zz_dir, 'agi.yaml'))

  caught = tryCatch(
    {
      parse_one_state('ZZ', tmp_root, 'baseline', 2017:2020, test_indexes)
      FALSE
    },
    error = function(e) {
      str_detect(conditionMessage(e), 'Unknown state tax law parameter') &&
        str_detect(conditionMessage(e), 'pension_sub_shre') &&
        str_detect(conditionMessage(e), 'ZZ')
    }
  )

  # Fixing the name must parse cleanly, with the doc-only entry skipped
  fixed_yaml = readLines(file.path(zz_dir, 'agi.yaml')) %>%
    str_replace('pension_sub_shre', 'pension_sub_share')
  writeLines(fixed_yaml, file.path(zz_dir, 'agi.yaml'))
  fixed = parse_one_state('ZZ', tmp_root, 'baseline', 2017:2020, test_indexes)

  stopifnot(
    'unknown parameter did not fail at load time' = caught,
    'corrected parameter did not parse' =
      'st_agi.pension_sub_share' %in% names(fixed),
    'documentation-only entry became a column' =
      !any(str_detect(names(fixed), 'doc_only_entry'))
  )

  message('test_state_param_validation: PASSED')
  invisible(TRUE)
}



test_state_rollout_tracker = function() {

  #----------------------------------------------------------------------------
  # Checks the state-parameter rollout tracker used by the parallel workflow.
  # The tracker should cover all 50 states plus DC exactly once, use only
  # allowed status values, and any state marked yaml_dir = done must have a
  # baseline config directory in the repo.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  tracker = read_csv('./research/state_tax/state_parameter_rollout.csv',
                     show_col_types = FALSE)
  allowed = c('todo', 'in_progress', 'done', 'blocked_weights', 'n/a')
  status_cols = c('source_packet', 'yaml_dir', 'worksheet_tests',
                  'cross_model', 'aggregate')

  stopifnot(
    'tracker row count wrong' = nrow(tracker) == 51,
    'tracker states not unique' = n_distinct(tracker$state) == 51,
    'tracker state code malformed' = all(str_detect(tracker$state, '^[A-Z]{2}$')),
    'tracker has invalid status' =
      all(unlist(tracker[status_cols]) %in% allowed)
  )

  encoded_states = tracker %>%
    filter(yaml_dir == 'done') %>%
    pull(state) %>%
    tolower()

  stopifnot(
    'tracker marks missing yaml dir as done' =
      all(dir.exists(file.path('./config/scenarios/tax_law_state/baseline',
                               encoded_states)))
  )

  message('test_state_rollout_tracker: PASSED')
  invisible(TRUE)
}

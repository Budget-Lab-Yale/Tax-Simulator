#---------------------------------------------------------------
# The supplemental-inputs interface (src/data/supplemental_inputs.R,
# decision S43) on a temp-directory fixture: one producer vintage
# with manifest.csv, dependencies.csv, a unit-keyed and a state-
# keyed year file.
#
# Cases: the schema loads; a well-formed year reads, filters and
# keys both file types; an uncovered record is NA and coverage
# counts it; a missing year file, duplicate key, unknown state,
# unknown variable, unpublished variable, bad type and a column
# collision all stop; the runscript rules (opt-in needs states,
# opt-in needs a pin, frozen across scenarios, pins blanked when
# not opted in); the pin check accepts a matching Tax-Data
# vintage, a declared equivalence, and refuses another; and the
# INVARIANCE that the state calculator gives identical output with
# no overlay, an all-NA overlay, and an all-default overlay.
#
# Defines functions only (sourced by main.R's recursive walk).
# Run manually after sourcing ./src:  test_supplemental_inputs_reader()
#---------------------------------------------------------------


test_supplemental_inputs_reader = function() {

  #----------------------------------------------------------------------------
  # Runs the reader, runscript-rule, pin-check and invariance tests.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  expect_stop = function(expr, pattern, what) {
    msg = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(msg))          stop('FAIL ', what, ': did not stop')
    if (!grepl(pattern, msg))  stop('FAIL ', what, ': unexpected message: ', msg)
    invisible(TRUE)
  }

  schema = supplemental_schema()
  stopifnot(all(c('rent_paid', 'tenure', 'interest_us_share', 'elective_deferral1') %in% schema$vars$variable),
            identical(schema$defaults$interest_us_share, 0.15),
            identical(schema$defaults$interest_own_state_share, 0.75),
            identical(schema$defaults$tenure, 0L),
            identical(schema$defaults$living_apart, FALSE))

  # --- fixture: <vintage>/baseline/{manifest.csv, supplemental_*_2023.csv.gz} + dependencies.csv
  vint = file.path(tempdir(), 'sup_fixture'); root = file.path(vint, 'baseline')
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  units = tibble(id = c(1, 2, 3), weight = c(10, 4, 0.5))
  write_csv(tibble(variable = c('care_exp_uncapped', 'elective_deferral1', 'rent_paid', 'tenure'),
                   key      = c('unit', 'person', 'state', 'state'),
                   type     = c('dbl', 'dbl', 'dbl', 'int'),
                   units = '', description = '', source_vars = '', method = ''),
            file.path(root, 'manifest.csv'))
  data.table::fwrite(tibble(id = c(1, 2), care_exp_uncapped = c(8000, NA), elective_deferral1 = c(5000, 1000)),
                     file.path(root, 'supplemental_unit_2023.csv.gz'), compress = 'gzip')
  data.table::fwrite(tibble(id    = c(1, 1, 2, 3),
                            state = c('CT', 'NY', 'CT', 'NY'),
                            rent_paid = c(12000, 0, 9000, NA),
                            tenure    = c(2L, 1L, 2L, 0L)),
                     file.path(root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  write_csv(tibble(ID = 'sup', interface = c('Tax-Data', 'Affordability-Index'), version = 1,
                   vintage = c('TD_A', 'sup_fixture'), scenario = 'baseline'),
            file.path(vint, 'dependencies.csv'))
  sw = tibble(id = c(1, 1, 2, 2, 3, 3), state = c('CT', 'NY', 'CT', 'NY', 'CT', 'NY'),
              weight = c(6, 4, 1, 3, 0.25, 0.25))
  roots = list(`Affordability-Index` = root)

  # --- reads, filters, keys both file types; NA = uncovered; coverage
  out = build_supplemental_inputs(units, 2023, c('care_exp_uncapped', 'rent_paid', 'tenure'),
                                  roots, states = c('CT', 'NY'), state_weights = sw)
  stopifnot(identical(names(out$unit), c('id', 'sup.care_exp_uncapped')),
            nrow(out$unit) == 2, is.na(out$unit$sup.care_exp_uncapped[out$unit$id == 2]),
            setequal(names(out$state), c('id', 'state', 'sup.rent_paid', 'sup.tenure')),   # schema order, not request order
            nrow(out$state) == 4, is.integer(out$state$sup.tenure))
  cov = out$coverage
  stopifnot(nrow(cov) == 1 + 2 * 2,                               # one unit var; two state vars x two states
            cov$n_records_covered[cov$variable == 'care_exp_uncapped'] == 1,
            abs(cov$share_weighted[cov$variable == 'care_exp_uncapped'] - 10 / 14.5) < 1e-12,
            cov$n_records_covered[cov$variable == 'rent_paid' & cov$state == 'NY'] == 1,   # id 3 is NA
            abs(cov$share_weighted[cov$variable == 'rent_paid' & cov$state == 'NY'] - 4 / 7.25) < 1e-12)

  # --- attach: unit by id, state filtered then by id; row order kept
  ctx = tibble(id = c(3, 1, 2), x = 1:3)
  at = attach_supplemental_inputs(ctx, out, 'NY')
  stopifnot(identical(at$id, c(3, 1, 2)), identical(at$x, 1:3),
            is.na(at$sup.rent_paid[1]), at$sup.rent_paid[2] == 0, is.na(at$sup.rent_paid[3]),
            is.na(at$sup.care_exp_uncapped[1]), at$sup.care_exp_uncapped[2] == 8000)
  stopifnot(identical(attach_supplemental_inputs(ctx, NULL, 'NY'), ctx))

  # --- not requested = NULL
  stopifnot(is.null(build_supplemental_inputs(units, 2023, NULL, roots)),
            is.null(build_supplemental_inputs(units, 2023, character(), roots)))

  # --- failures name their cause
  expect_stop(build_supplemental_inputs(units, 2022, 'rent_paid', roots, states = 'CT'),
              'no state-keyed supplemental inputs for 2022', 'a missing year file stops')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', roots, states = 'CT', pct_sample = 1) %>%
                { build_supplemental_inputs(units %>% filter(id != 3), 2023, 'rent_paid', roots, states = 'CT') },
              'not in the TY2023 tax units', 'an orphan id stops on the full sample')
  stopifnot(!is.null(build_supplemental_inputs(units %>% filter(id != 3), 2023, 'rent_paid', roots,
                                               states = 'CT', pct_sample = 0.5)))
  expect_stop(build_supplemental_inputs(units, 2023, 'not_a_variable', roots),
              'unknown variable', 'an unknown variable stops')
  expect_stop(build_supplemental_inputs(units, 2023, 'pension_public', roots),
              'no pinned producer publishes', 'an unpublished variable stops')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', list()),
              'no producer interface path', 'no producer stops')
  expect_stop(build_supplemental_inputs(units %>% mutate(rent_paid = 0), 2023, 'rent_paid', roots, states = 'CT'),
              'does not allow shadowing', 'a collision with an existing column stops')
  stopifnot(!is.null(build_supplemental_inputs(units %>% mutate(elective_deferral1 = 0), 2023,
                                               'elective_deferral1', roots)))    # shadow: allow
  dup_root = file.path(tempdir(), 'sup_dup', 'baseline'); dir.create(dup_root, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(root, 'manifest.csv'), dup_root, overwrite = TRUE)
  data.table::fwrite(tibble(id = c(1, 1, 2), state = c('CT', 'CT', 'NY'), rent_paid = 1, tenure = 1L),
                     file.path(dup_root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', list(`Affordability-Index` = dup_root), states = 'CT'),
              'duplicate', 'a duplicate key stops')
  data.table::fwrite(tibble(id = 1, state = 'ZZ', rent_paid = 1, tenure = 1L),
                     file.path(dup_root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', list(`Affordability-Index` = dup_root)),
              'unknown state', 'an unknown state stops')
  data.table::fwrite(tibble(id = 1, state = 'CT', rent_paid = 'twelve', tenure = 1L),
                     file.path(dup_root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', list(`Affordability-Index` = dup_root)),
              'cannot be read as dbl', 'a bad type stops')
  expect_stop(build_supplemental_inputs(units, 2023, 'rent_paid', list(`Affordability-Index` = root, Other = dup_root)),
              'more than one pinned producer', 'two producers publishing one variable stops')
  # Values a cast would silently reinterpret stop instead
  data.table::fwrite(tibble(id = 1, state = 'CT', rent_paid = 1, tenure = 1.6),
                     file.path(dup_root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  expect_stop(build_supplemental_inputs(units, 2023, 'tenure', list(`Affordability-Index` = dup_root)),
              'outside its domain', 'a fractional int stops')
  data.table::fwrite(tibble(id = 1, state = 'CT', rent_paid = 1, tenure = 7L),
                     file.path(dup_root, 'supplemental_state_2023.csv.gz'), compress = 'gzip')
  expect_stop(build_supplemental_inputs(units, 2023, 'tenure', list(`Affordability-Index` = dup_root)),
              'outside its domain', 'a code outside the schema values stops')
  stopifnot(identical(sup_out_of_domain(c(0, 1, NA, 2), 'lgl'), c(FALSE, FALSE, FALSE, TRUE)),
            identical(sup_out_of_domain(c(TRUE, NA), 'lgl'), c(FALSE, FALSE)),
            identical(sup_out_of_domain(c(-2.5, NA), 'dbl'), c(FALSE, FALSE)),
            identical(sup_out_of_domain(c(2, 2.5), 'int'), c(FALSE, TRUE)))

  # --- runscript rules
  producers = 'Affordability-Index'
  rs = tibble(ID = c('baseline', 'reform', 'fed'), years = '2023:2023',
              states = c('CT NY', 'CT', NA),
              supplemental_inputs = c('rent_paid tenure', 'tenure rent_paid', NA),
              `dep.Affordability-Index.vintage` = 'sup_fixture', `dep.Affordability-Index.ID` = 'baseline')
  stopifnot(isTRUE(validate_runscript_supplemental(rs, producers)))
  b = blank_supplemental_pins(rs, producers)
  stopifnot(is.na(b$`dep.Affordability-Index.vintage`[3]), is.na(b$`dep.Affordability-Index.ID`[3]),
            b$`dep.Affordability-Index.vintage`[1] == 'sup_fixture')
  expect_stop(validate_runscript_supplemental(rs %>% mutate(supplemental_inputs = replace(supplemental_inputs, 3, 'rent_paid')), producers),
              'without `states`', 'opting in on a federal-only row stops')
  expect_stop(validate_runscript_supplemental(rs %>% mutate(supplemental_inputs = replace(supplemental_inputs, 2, 'tenure')), producers),
              'frozen across scenarios', 'a counterfactual with a different variable set stops')
  expect_stop(validate_runscript_supplemental(rs %>% mutate(`dep.Affordability-Index.vintage` = replace(`dep.Affordability-Index.vintage`, 2, 'other')), producers),
              'frozen across scenarios', 'a counterfactual with a different producer pin stops')
  expect_stop(blank_supplemental_pins(rs %>% select(-starts_with('dep.')), producers),
              'pin no producer vintage', 'opting in without a pin stops')
  # A runscript with no baseline row is checked against the stored baseline
  stored = file.path(tempdir(), 'sup_stored')
  dir.create(file.path(stored, 'baseline', 'static', 'supplemental'), recursive = TRUE, showWarnings = FALSE)
  write_csv(tibble(year = 2023, variable = c('rent_paid', 'tenure'), interface = producers, vintage = 'sup_fixture'),
            file.path(stored, 'baseline', 'static', 'supplemental', 'supplemental_inputs_coverage.csv'))
  write_csv(tibble(ID = 'baseline', interface = producers, version = 1, vintage = 'sup_fixture', scenario = 'baseline'),
            file.path(stored, 'dependencies.csv'))
  cf_only = rs %>% filter(ID != 'baseline')
  stopifnot(isTRUE(validate_supplemental_stored_baseline(cf_only, stored, producers)))
  expect_stop(validate_supplemental_stored_baseline(cf_only %>% mutate(supplemental_inputs = 'tenure'), stored, producers),
              'stored baseline .* ran with', 'a counterfactual unlike the stored baseline variable set stops')
  expect_stop(validate_supplemental_stored_baseline(cf_only %>% mutate(`dep.Affordability-Index.vintage` = 'other'), stored, producers),
              'stored baseline pins', 'a counterfactual unlike the stored baseline pin stops')
  unlink(file.path(stored, 'baseline'), recursive = TRUE)                    # stored baseline ran without an overlay
  expect_stop(validate_supplemental_stored_baseline(cf_only, stored, producers),
              'ran with \\(\\)', 'opting in against a stored baseline without supplemental inputs stops')
  stopifnot(isTRUE(validate_supplemental_stored_baseline(cf_only %>% mutate(supplemental_inputs = NA), stored, producers)))
  unlink(stored, recursive = TRUE)
  no_opt = rs %>% mutate(supplemental_inputs = NA)
  b = blank_supplemental_pins(no_opt, producers)
  stopifnot(all(is.na(b$`dep.Affordability-Index.vintage`)))
  stopifnot(identical(parse_supplemental_value(' rent_paid   tenure '), c('rent_paid', 'tenure')),
            identical(parse_supplemental_value(NA), character()),
            identical(parse_years_value('2022:2025'), 2022:2025))

  # --- pin check: identical vintage, declared equivalence, refusal
  eq_file = file.path(tempdir(), 'sup_equiv.yaml')
  writeLines(c('equivalences:', "  - vintages: ['TD_A', 'TD_A_republished']", '    years: [2023]'), eq_file)
  stopifnot(isTRUE(tax_data_vintages_match('TD_A', 'TD_A')),
            isTRUE(tax_data_vintages_match('TD_A', 'TD_A_republished', 2023, path = eq_file)),
            !tax_data_vintages_match('TD_A', 'TD_A_republished', 2022:2023, path = eq_file),
            !tax_data_vintages_match('TD_A', 'TD_A_republished', NULL, path = eq_file),
            !tax_data_vintages_match('TD_A', 'TD_B', 2023, path = eq_file))
  paths = tibble(ID = 'baseline', interface = 'Affordability-Index', path = root)
  deps_ok  = tibble(ID = 'baseline', interface = c('Tax-Data', 'Affordability-Index'), version = 1,
                    vintage = c('TD_A', 'sup_fixture'), scenario = 'baseline')
  deps_bad = deps_ok %>% mutate(vintage = if_else(interface == 'Tax-Data', 'TD_B', vintage))
  rs1 = tibble(ID = 'baseline', years = '2023:2023', states = 'CT', supplemental_inputs = 'rent_paid')
  stopifnot(isTRUE(validate_supplemental_pins(deps_ok, paths, rs1, producers = producers)))
  expect_stop(validate_supplemental_pins(deps_bad, paths, rs1, producers = producers),
              'was built on Tax-Data TD_A', 'a producer built on another Tax-Data vintage is refused')
  expect_stop(validate_supplemental_pins(deps_ok, paths, rs1 %>% mutate(supplemental_inputs = 'pension_public'), producers = producers),
              'published by 0 pinned producer', 'an unpublished variable is refused at parse time')
  expect_stop(validate_supplemental_pins(deps_ok, paths, rs1 %>% mutate(supplemental_inputs = 'nope'), producers = producers),
              'not in', 'an unknown variable is refused at parse time')
  stopifnot(isTRUE(validate_supplemental_pins(deps_ok, paths %>% filter(interface != 'Affordability-Index'), rs1, producers = producers)))

  # --- resolver: the Tax-Data equivalent column switches a variable on
  u = tibble(id = 1:2, trad_contr_er1 = c(5000, 0))
  r = ensure_st_data(u)
  stopifnot(identical(r$elective_deferral1, c(5000, 0)), all(r$has_elective_deferral1),
            identical(r$elective_deferral2, c(0, 0)), !any(r$has_elective_deferral2),
            identical(r$rent_paid, c(0, 0)), !any(r$has_rent_paid))
  # The overlay wins where it has a value; a record it leaves NA keeps the
  # equivalent's real value rather than taking the default
  r2 = ensure_st_data(u %>% mutate(sup.elective_deferral1 = c(NA, 7000)))
  stopifnot(identical(r2$elective_deferral1, c(5000, 7000)),
            identical(r2$has_elective_deferral1, c(TRUE, TRUE)))
  # ... and the default only when neither source has a value
  r3 = ensure_st_data(tibble(id = 1:2, trad_contr_er1 = c(NA, 0), sup.elective_deferral1 = c(NA, NA)))
  stopifnot(identical(r3$elective_deferral1, c(0, 0)),
            identical(r3$has_elective_deferral1, c(FALSE, TRUE)))
  # A native column (not the equivalent) is the fallback the same way
  r4 = ensure_st_data(tibble(id = 1:2, elective_deferral1 = c(3000, 0), sup.elective_deferral1 = c(NA, 7000)))
  stopifnot(identical(r4$elective_deferral1, c(3000, 7000)))

  # --- split classes come from the schema
  sc = supplemental_split_classes()
  stopifnot('rent_paid' %in% sc$halve, 'elective_deferral' %in% sc$person_bases,
            identical(sc$person_empty$elective_deferral2, 0))

  unlink(c(vint, dirname(dup_root), eq_file), recursive = TRUE)

  test_supplemental_inputs_invariance()
  message('test_supplemental_inputs_reader: all passed')
  invisible(TRUE)
}



test_supplemental_inputs_invariance = function() {

  #----------------------------------------------------------------------------
  # The calculator contract for optional data (decision S43): with no overlay,
  # with an overlay present but every value NA, and with an overlay present at
  # the schema defaults, do_state_taxes() returns identical results. Run on
  # synthetic post-federal units (st_test_unit(), test_state_calc.R) for a
  # rolling state, a state that reads the interest shares, and a state with
  # the married-separate election (so the split classes are exercised).
  #----------------------------------------------------------------------------

  law = build_state_tax_law(
    states  = c('IL', 'MN', 'MT', 'HI', 'KY', 'PA'),
    years   = 2023,
    indexes = expand_grid(series = 'cpi', year = 2015:2036) %>% mutate(growth = 0.025)
  )
  credit_tables = attr(law, 'credit_tables')
  schema = supplemental_schema()

  units = bind_rows(
    st_test_unit(list(id = 1, filing_status = 1, agi = 60000, txbl_inc = 45000, wages1 = 60000, ei1 = 60000,
                      txbl_int = 2000, exempt_int = 1500, txbl_pens_dist = 0)),
    st_test_unit(list(id = 2, filing_status = 2, age1 = 68, age2 = 66, agi = 90000, txbl_inc = 60000,
                      wages1 = 30000, wages2 = 20000, ei1 = 30000, ei2 = 20000, txbl_int = 5000,
                      exempt_int = 4000, txbl_pens_dist = 30000, gross_ss = 20000, txbl_ss = 10000,
                      n_dep = 1, n_dep_ctc = 1, dep_age1 = 8, care_exp = 3000)),
    st_test_unit(list(id = 3, filing_status = 4, agi = 25000, txbl_inc = 5000, wages1 = 25000, ei1 = 25000,
                      n_dep = 2, n_dep_ctc = 2, n_dep_eitc = 2, dep_age1 = 3, dep_age2 = 5, eitc = 5000))
  ) %>% mutate(year = 2023)

  overlay_na = units %>% select(id)
  overlay_default = units %>% select(id)
  for (i in seq_len(nrow(schema$vars))) {
    v = schema$vars$variable[i]
    overlay_na[[paste0(SUP_PREFIX, v)]]      = sup_cast(NA, schema$vars$type[i])
    overlay_default[[paste0(SUP_PREFIX, v)]] = schema$defaults[[v]]
  }

  for (st in c('IL', 'MN', 'MT', 'HI', 'KY', 'PA')) {
    law_st = law %>% filter(state == st, year == 2023)
    run = function(u) {
      u %>%
        left_join(law_st %>% select(-state), by = c('year', 'filing_status')) %>%
        do_state_taxes(credit_tables = state_credit_tables_for_year(credit_tables, st, 2023),
                       law_mfs = law_st %>% filter(filing_status == 3) %>%
                                 select(-state, -year, -filing_status))
    }
    base = run(units)
    with_na  = run(units %>% left_join(overlay_na, by = 'id'))
    with_def = run(units %>% left_join(overlay_default, by = 'id'))
    if (!identical(base, with_na)) {
      stop('FAIL invariance (', st, '): an all-NA overlay changed the state output')
    }
    if (!identical(base, with_def)) {
      stop('FAIL invariance (', st, '): an all-default overlay changed the state output')
    }
    stopifnot(any(abs(base$liab_st_iit) > 0))   # the cases actually compute something
  }

  # And the shares really are consumed: a non-default US-obligation share moves
  # a state that subtracts it (MN carries sub_us_int = 1)
  law_mn = law %>% filter(state == 'MN', year == 2023)
  run_mn = function(u) u %>% left_join(law_mn %>% select(-state), by = c('year', 'filing_status')) %>%
    do_state_taxes(credit_tables = state_credit_tables_for_year(credit_tables, 'MN', 2023),
                   law_mfs = law_mn %>% filter(filing_status == 3) %>% select(-state, -year, -filing_status))
  moved = run_mn(units %>% mutate(sup.interest_us_share = 1))
  stopifnot(any(moved$st_agi < run_mn(units)$st_agi))

  message('test_supplemental_inputs_invariance: all passed')
  invisible(TRUE)
}

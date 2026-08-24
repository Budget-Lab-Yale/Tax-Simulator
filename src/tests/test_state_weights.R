#---------------------------------------------------------------
# Tests for the state-weights data layer (src/data/state_weights.R)
#
# Defines functions only (this file is sourced by main.R's recursive
# walk; side effects at source time would break model startup).
#
# Synthetic frames only -- nothing here reads the IPUMS extract, which
# OOMs the login node. The extract-backed checks live in
# research/state_weights/nonfiler_residual/03_diagnose_current_nonfilers.R.
#
# Run manually:
#   module load R/4.4.2-gfbf-2024a
#   Rscript -e "
#     suppressPackageStartupMessages(invisible(capture.output(
#       lapply(readLines('./requirements.txt'), library, character.only = T))));
#     return_vars = list();
#     list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
#       walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)));
#     test_state_weights()"
#---------------------------------------------------------------


test_state_weights = function() {

  #----------------------------------------------------------------------------
  # Runs all state-weights data-layer tests, stopping on first failure.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  test_gq_classification()
  test_gq_differentiation()
  test_gq_guards()
  test_adult_x_vector()
  test_nonfiler_targets_count_adults()
  message('test_state_weights: ALL TESTS PASSED')
  invisible(TRUE)
}


#-------------------------------------------------------------------------------
# Synthetic ACS person frame.
#
# One California household plus five group-quarters singles, all survey year
# 2022. Deliberately covers every branch of classify_gq() and both sides of the
# dorm-student income test:
#
#   p1/p2  married couple 40/38, $60k joint      -> filer household unit
#   p3     their child, 10                       -> dependent
#   p4     prisoner 30, $0, GQ=3                 -> non-filer unit, RETAINED
#   p5     dorm student 20, $2k, GQ=4 SCHOOL=2   -> RECLASSIFIED out of the units
#   p6     dorm student 21, $30k, GQ=4 SCHOOL=2  -> above threshold, stays a FILER
#   p7     barracks 22, $40k, GQ=4 SCHOOL=1      -> income test, FILER
#   p8     nursing home 80, $9k, GQ=3            -> non-filer unit, RETAINED
#
# Every person carries PERWT = 100, so weighted counts are readable by eye.
#-------------------------------------------------------------------------------
synthetic_acs_frame = function() {

  person = function(pernum, age, inc, gq, school, marst = 6, sploc = 0,
                    momloc = 0, poploc = 0, serial = 1) {
    data.table::data.table(
      YEAR = 2022L, STATEFIP = 6L, PERWT = 100, PERNUM = pernum,
      SERIAL = serial, SAMPLE = 202201L, AGE = age, MARST = marst,
      SPLOC = sploc, MOMLOC = momloc, POPLOC = poploc, INCTOT = inc,
      GQ = gq, SCHOOL = school)
  }

  data.table::rbindlist(list(
    person(1, 40, 35000, 1, 1, marst = 1, sploc = 2),
    person(2, 38, 25000, 1, 1, marst = 1, sploc = 1),
    person(3, 10,     0, 1, 2, momloc = 2, poploc = 1),
    person(1, 30,     0, 3, 1, serial = 2),
    person(1, 20,  2000, 4, 2, serial = 3),
    person(1, 21, 30000, 4, 2, serial = 4),
    person(1, 22, 40000, 4, 1, serial = 5),
    person(1, 80,  9000, 3, 1, serial = 6)))
}


test_gq_classification = function() {

  #----------------------------------------------------------------------------
  # classify_gq() assigns the four types on the documented IPUMS codes.
  #
  # The dorm predicate (GQ==4 & in school & 18-24) is shared with the Stage D
  # T7 tabulation; on the TY2022 extract it reproduces F6 exactly -- 3.61M
  # institutional, 2.81M dorm students, 1.74M other. Verified 2026-08-24.
  #----------------------------------------------------------------------------

  got = classify_gq(GQ     = c(1L, 2L, 5L, 3L, 4L, 4L, 4L, 4L),
                    SCHOOL = c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L),
                    AGE    = c(30L, 30L, 30L, 30L, 20L, 30L, 20L, 17L))

  stopifnot(identical(got, c('household', 'household', 'household',
                             'institutional',
                             'dorm_student',            # in school, 18-24
                             'other_noninstitutional',  # in school but 30
                             'other_noninstitutional',  # 18-24 but not in school
                             'other_noninstitutional'))) # in school but 17

  message('  test_gq_classification: PASSED')
}


test_gq_differentiation = function() {

  #----------------------------------------------------------------------------
  # build_acs_margins() differentiated GQ treatment (task B1, decision D4).
  #
  # Four properties, each one a way the treatment could go wrong:
  #   1. state_pop is IDENTICAL across treatments. The anchors are PEP resident
  #      adults with no GQ subtraction (design memo 3.0), so reclassifying a
  #      dorm student must change who heads a unit and NOT how many people live
  #      in the state. This is the invariant the whole design rests on.
  #   2. Only the below-threshold dorm student leaves the non-filer margin.
  #   3. Institutional residents are retained as own-state non-filer units.
  #   4. Filer counts are untouched -- the reclassified population is, by
  #      construction, below the filing threshold.
  #----------------------------------------------------------------------------

  acs = synthetic_acs_frame()
  v0  = build_acs_margins(acs, 2022, gq_treatment = 'v0')
  dif = build_acs_margins(acs, 2022, gq_treatment = 'differentiated')

  # 1. the population invariant
  stopifnot(isTRUE(all.equal(v0$state_pop, dif$state_pop)),
            dif$state_pop$pop == 800)

  # v0 emits no GQ objects; differentiated does, and both label themselves
  stopifnot(is.null(v0$gq_composition), is.null(v0$gq_reclassified),
            v0$gq_treatment == 'v0', dif$gq_treatment == 'differentiated')

  # 2. exactly one student reclassified -- the $2k one, not the $30k one
  stopifnot(nrow(dif$gq_reclassified) == 1,
            dif$gq_reclassified$persons == 100,
            sum(v0$nonfiler_margins$n_units) -
              sum(dif$nonfiler_margins$n_units) == 100)

  # composition covers all three GQ types on the full frame
  comp = dif$gq_composition
  stopifnot(comp[gq_type == 'institutional',          sum(persons)] == 200,
            comp[gq_type == 'dorm_student',           sum(persons)] == 200,
            comp[gq_type == 'other_noninstitutional', sum(persons)] == 100)

  # 3. both institutional residents survive as non-filer units, in their own
  #    state and their own age bands
  nf = dif$nonfiler_margins
  stopifnot(nrow(nf) == 2,
            all(nf$state == 'CA'),
            setequal(as.character(nf$age_band), c('26_34', '75p')),
            sum(nf$n_units) == 200)

  # 4. filers unchanged: the household, the $30k student, the barracks resident
  stopifnot(sum(v0$filer_units$n_units) == sum(dif$filer_units$n_units),
            sum(dif$filer_units$n_units) == 300)

  message('  test_gq_differentiation: PASSED')
}


test_gq_guards = function() {

  #----------------------------------------------------------------------------
  # The two loud failures. Both exist because the silent version is worse:
  # margins built without GQ would look complete and quietly carry the v0
  # composition, and an unrecognized GQ code would be swept into 'household'.
  #----------------------------------------------------------------------------

  acs = synthetic_acs_frame()

  missing_cols = try(
    build_acs_margins(acs[, !c('GQ', 'SCHOOL')], 2022), silent = TRUE)
  stopifnot(inherits(missing_cols, 'try-error'),
            grepl('needs GQ, SCHOOL', missing_cols))

  bad_code = data.table::copy(acs)
  bad_code[1, GQ := 6L]
  unknown_gq = try(build_acs_margins(bad_code, 2022), silent = TRUE)
  stopifnot(inherits(unknown_gq, 'try-error'),
            grepl('unexpected IPUMS GQ code', unknown_gq))

  message('  test_gq_guards: PASSED')
}


test_adult_x_vector = function() {

  #----------------------------------------------------------------------------
  # The D5 adult x-vector: 1 + (filing_status == 2).
  #
  # Kept distinct from n_indiv, which is HT2's "individuals" concept and adds
  # dependents. Confusing the two would silently inflate every non-filer target
  # by the dependent count, so the test asserts they differ where dependents
  # exist.
  #----------------------------------------------------------------------------

  tu = data.table::data.table(
    filing_status = c(1L, 2L, 4L, 2L, 1L),
    n_dep         = c(0L, 0L, 2L, 3L, 1L))

  adults = puf_series_x(tu, 'n_adults')
  indiv  = puf_series_x(tu, 'n_indiv')

  stopifnot(
    # single/HoH count one adult, joint counts two -- dependents never counted
    identical(as.numeric(adults), c(1, 2, 1, 2, 1)),
    # n_indiv adds dependents, so the two agree ONLY with no dependents
    identical(as.numeric(indiv), c(1, 2, 3, 5, 2)),
    all(adults == indiv | tu$n_dep > 0),
    # MFS (status 3) is one adult, like single
    puf_series_x(data.table::data.table(filing_status = 3L, n_dep = 0L),
                 'n_adults') == 1)

  message('  test_adult_x_vector: PASSED')
}


test_nonfiler_targets_count_adults = function() {

  #----------------------------------------------------------------------------
  # build_weight_inputs() must target ADULTS on the non-filer partition, and
  # the identity that matters is that each cell's targets sum to the PUF's
  # ADULT total for that cell -- not its unit total. Before D5 landed these
  # were the same number, which is exactly why the mismatch was invisible.
  #
  # Runs on synthetic PUF records and synthetic margins, so no HT2 or ACS read.
  #----------------------------------------------------------------------------

  # Two non-filer records in one cell: one single, one joint => 3 adults, 2 units
  tu = data.table::data.table(
    id = 1:3, weight = c(100, 100, 100), filer = c(0L, 0L, 1L),
    filing_status = c(1L, 2L, 1L), n_dep = 0L, age1 = c(30L, 30L, 40L),
    agi = c(0, 0, 50000),
    wages = c(0,0,50000), txbl_int = 0, exempt_int = 0, div_ord = 0, div_pref = 0,
    kg_st = 0, kg_lt = 0, txbl_pens_dist = 0, txbl_ira_dist = 0, gross_ss = 0,
    sole_prop = 0, eitc = 0)

  # Margins put the cell 60/40 across two states, in ADULT terms
  margins = list(nonfiler_margins = data.table::data.table(
    state       = c('CA', 'NY'),
    age_band    = factor('26_34', levels = AGE_BANDS),
    income_tier = factor('neg_zero',
                    levels = c('neg_zero','1_10k','10_25k','25_50k','50k_plus')),
    n_units     = c(60, 40),
    n_adults    = c(120, 40)))     # CA all-joint, NY all-single: NOT 60/40

  ht2 = data.table::data.table(
    state = rep(c('CA','NY'), each = 10), agi_stub = rep(1:10, 2),
    variable = 'n_returns', value = 1000, year = 2022L)

  inp = build_weight_inputs(tu, 2022, ht2 = ht2, acs_margins = margins,
                            verbose = FALSE)
  tn = inp$nonfilers$targets
  stopifnot(length(tn) == 2, all(vapply(tn, `[[`, character(1), 'series') == 'n_adults'))

  # the x-vector reached the target, per record
  stopifnot(identical(as.numeric(tn[[1]]$x), c(1, 2)))

  # PUF adults in the cell = 100*1 + 100*2 = 300, split by the ADULT shares
  # 120/160 and 40/160 -> 225 / 75.  A unit-denominated target would have
  # split 300 by 60/100 and 40/100 -> 180 / 120, so this distinguishes them.
  got = sort(vapply(tn, `[[`, numeric(1), 'target'))
  stopifnot(isTRUE(all.equal(got, c(75, 225))),
            isTRUE(all.equal(sum(got), 300)))

  message('  test_nonfiler_targets_count_adults: PASSED')
}

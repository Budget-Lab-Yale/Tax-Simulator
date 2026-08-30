# =============================================================================
# Tests for src/data/asec_tax_units.R (group C stages C0-C2)
#
# Functions only (src/ is sourced recursively). Run via:
#   Rscript -e 'library(data.table); source("src/data/state_weights.R");
#               source("src/data/asec_tax_units.R");
#               source("src/tests/test_asec_tax_units.R"); test_asec_tax_units()'
#
# Fixtures are constructed households, one per D-A2 rule, sized so a wrong
# pass produces a DIFFERENT unit count -- not a plausible one.
# =============================================================================

#' One person row with fixture defaults. Income concepts are supplied directly
#' (the builder takes a cleaned person table; NIU handling is read_asec()'s
#' job and is tested against the real extract, not fixtures).
.fixture_person <- function(SERIAL, PERNUM, AGE,
                            RELATE = 101L, MARST = 6L, SPLOC = 0L,
                            MOMLOC = 0L, POPLOC = 0L,
                            FTYPE = 1L, FAMID = 1L, FAMREL = 1L,
                            SCHLCOLL = 5L, ASECWT = 1000,
                            wages = 0, se = 0, interest = 0, retirement = 0,
                            ss = 0) {
  data.table(
    SERIAL = SERIAL, PERNUM = PERNUM, AGE = AGE, RELATE = RELATE,
    MARST = MARST, SPLOC = SPLOC, MOMLOC = MOMLOC, POPLOC = POPLOC,
    FTYPE = FTYPE, FAMID = FAMID, FAMREL = FAMREL, SCHLCOLL = SCHLCOLL,
    ASECWT = ASECWT,
    INCWAGE = wages, INCINT = interest, INCDIVID = 0, INCRENT = 0,
    INCSS = ss, INCUNEMP = 0,
    se_income = se, earned_income = wages + se,
    investment_income = interest,
    retirement_income = retirement,
    gross_income = wages + se + interest + retirement,
    total_income = wages + se + interest + retirement + ss,
    gross_income_inc_kg = NA_real_)
}

test_unit_rules <- function() {

  fixtures <- rbindlist(list(
    # HH 1: married couple, two young children -> 1 joint unit, 2 QC deps.
    .fixture_person(1, 1, 40, RELATE = 101L, MARST = 1L, SPLOC = 2L, wages = 60000),
    .fixture_person(1, 2, 38, RELATE = 201L, MARST = 1L, SPLOC = 1L, FAMREL = 2L, wages = 20000),
    .fixture_person(1, 3,  8, RELATE = 301L, MOMLOC = 2L, POPLOC = 1L, FAMREL = 3L),
    .fixture_person(1, 4, 16, RELATE = 301L, MOMLOC = 2L, POPLOC = 1L, FAMREL = 3L),

    # HH 2: unmarried mother + 20yo FULL-TIME student with small wages
    # -> 1 HoH unit; the student is a QC dependent AND a dependent scoring
    # unit (aged 15+).
    .fixture_person(2, 1, 45, wages = 50000),
    .fixture_person(2, 2, 20, RELATE = 301L, MOMLOC = 1L, FAMREL = 3L,
                    SCHLCOLL = 3L, wages = 3000),

    # HH 3: same shape but the 20yo is NOT a student -> fails the QC age test,
    # and wages 30000 >> the QR income limit -> heads their OWN unit; the
    # parent has no qualifying child -> single, not HoH.
    .fixture_person(3, 1, 45, wages = 50000),
    .fixture_person(3, 2, 20, RELATE = 301L, MOMLOC = 1L, FAMREL = 3L,
                    SCHLCOLL = 5L, wages = 30000),

    # HH 4: householder couple + related subfamily (their adult daughter,
    # NO earnings, + her baby) -> subfamily ABSORBED: 1 joint unit with 2
    # dependents (daughter 22 = adult dep, baby).
    .fixture_person(4, 1, 62, RELATE = 101L, MARST = 1L, SPLOC = 2L, wages = 80000),
    .fixture_person(4, 2, 60, RELATE = 201L, MARST = 1L, SPLOC = 1L, FAMREL = 2L),
    .fixture_person(4, 3, 22, RELATE = 301L, MOMLOC = 2L, POPLOC = 1L,
                    FTYPE = 3L, FAMID = 2L, FAMREL = 1L),
    .fixture_person(4, 4,  1, RELATE = 901L, MOMLOC = 3L,
                    FTYPE = 3L, FAMID = 2L, FAMREL = 3L),

    # HH 5: as HH 4 but the daughter EARNS -> subfamily NOT absorbed: she
    # heads her own unit (single-with-QC = HoH) with the baby as her dep.
    .fixture_person(5, 1, 62, RELATE = 101L, MARST = 1L, SPLOC = 2L, wages = 80000),
    .fixture_person(5, 2, 60, RELATE = 201L, MARST = 1L, SPLOC = 1L, FAMREL = 2L),
    .fixture_person(5, 3, 22, RELATE = 301L, MOMLOC = 2L, POPLOC = 1L,
                    FTYPE = 3L, FAMID = 2L, FAMREL = 1L, wages = 25000),
    .fixture_person(5, 4,  1, RELATE = 901L, MOMLOC = 3L,
                    FTYPE = 3L, FAMID = 2L, FAMREL = 3L),

    # HH 6: householder + elderly parent with only Social Security (SS is NOT
    # in Mok gross income) -> parent passes the QR income test -> dependent.
    .fixture_person(6, 1, 50, wages = 70000),
    .fixture_person(6, 2, 80, RELATE = 501L, FAMREL = 4L, ss = 15000),

    # HH 7: two unrelated roommates -> two separate single units, never
    # dependents (the member-of-household QR route is deliberately unmodeled).
    .fixture_person(7, 1, 30, wages = 40000),
    .fixture_person(7, 2, 31, RELATE = 1115L, FTYPE = 5L, FAMID = 2L,
                    FAMREL = 0L, wages = 1000)
  ))

  built <- build_asec_tax_units(fixtures, tax_year = 2022)
  u <- built$units
  base <- u[unit_type == "nondependent"]

  # ---- unit counts per household -------------------------------------------
  counts <- base[, .N, keyby = .(hh = floor(unit_id / 100))]
  expected <- data.table(hh = c(1, 2, 3, 3, 4, 5, 5, 6, 7, 7),
                         key = "hh")[, .(N_exp = .N), keyby = hh]
  stopifnot(identical(counts$N, expected$N_exp))

  # ---- statuses and dependents ---------------------------------------------
  g <- function(serial) base[floor(unit_id / 100) == serial][order(unit_id)]
  stopifnot(
    g(1)$filing_status == "joint",  g(1)$n_dep == 2, g(1)$n_dep_qc == 2,
    g(2)$filing_status == "hoh",    g(2)$n_dep == 1, g(2)$n_dep_adult == 1,
    identical(g(3)$filing_status, c("single", "single")), all(g(3)$n_dep == 0),
    g(4)$filing_status == "joint",  g(4)$n_dep == 2, g(4)$n_dep_adult == 1,
    identical(g(5)$filing_status, c("joint", "hoh")),
    identical(g(5)$n_dep, c(0L, 1L)),
    g(6)$filing_status == "single", g(6)$n_dep == 1, g(6)$n_dep_adult == 1,
    identical(g(7)$filing_status, c("single", "single")), all(g(7)$n_dep == 0)
  )

  # ---- unit income is head + spouse, never dependents' ----------------------
  stopifnot(g(1)$gross_income == 80000,   # 60k + 20k, not + the kids
            g(2)$gross_income == 50000)   # not + the student's 3k

  # ---- dependent scoring units: exactly the 15+ dependents ------------------
  # HH1's 16yo, HH2's student, HH4's daughter, HH6's parent -- and NOT the
  # babies (under 15, outside the ASEC income universe and Mok's panel E)
  dep <- u[unit_type == "dependent"]
  stopifnot(nrow(dep) == 4,
            setequal(dep$age_head, c(16, 20, 22, 80)),
            dep[age_head == 20, gross_income] == 3000)

  # ---- weights: unit weight is the HEAD's, not a sum ------------------------
  stopifnot(all(base$weight == 1000))

  message("  test_unit_rules: PASSED")
  invisible(TRUE)
}

test_support_test <- function() {
  # One household: householder earns 90k; 20yo full-time student child earns
  # `kid_wages`. Per-capita household income = (90k + kid)/2; the child is
  # self-supporting -- and so NOT a dependent -- when own income exceeds
  # support_mult x that share.
  hh <- function(kid_wages) rbindlist(list(
    .fixture_person(1, 1, 50, wages = 90000),
    .fixture_person(1, 2, 20, RELATE = 301L, MOMLOC = 1L, FAMREL = 3L,
                    SCHLCOLL = 3L, wages = kid_wages)))

  # 3k < 0.5 * (93k/2) -> dependent
  a <- build_asec_tax_units(hh(3000), 2022)$units
  stopifnot(a[unit_type == "nondependent", .N] == 1,
            a[unit_type == "nondependent", n_dep] == 1)

  # 40k > 0.5 * (130k/2) -> self-supporting -> own unit
  b <- build_asec_tax_units(hh(40000), 2022)$units
  stopifnot(b[unit_type == "nondependent", .N] == 2)

  # the multiplier is a real dial: at 1.0 the 40k kid is dependent again
  # (40k < 1.0 * 65k)
  c_ <- build_asec_tax_units(hh(40000), 2022, support_mult = 1.0)$units
  stopifnot(c_[unit_type == "nondependent", .N] == 1)

  message("  test_support_test: PASSED")
  invisible(TRUE)
}

test_filing_requirement <- function() {
  mk <- function(gross, earned = gross, se = 0, status = "single",
                 age = 40, age_sp = NA_real_, type = "nondependent") {
    data.table(unit_id = 1, filing_status = status, age_head = age,
               age_spouse = age_sp, gross_income = gross,
               earned_income = earned, se_income = se,
               investment_income = gross - earned, unit_type = type)
  }
  f <- function(...) add_filing_requirement(mk(...), 2022)

  stopifnot(
    # Chart A: single threshold 12,950, age-65 14,700
    f(12949)$must_file == FALSE,
    f(12950)$must_file == TRUE,
    f(13000, age = 66)$must_file == FALSE,
    f(14700, age = 66)$must_file == TRUE,
    # joint: 25,900 / 27,300 (one 65+) / 28,700 (both)
    f(25899, status = "joint", age_sp = 40)$must_file == FALSE,
    f(25900, status = "joint", age_sp = 40)$must_file == TRUE,
    f(27000, status = "joint", age = 66, age_sp = 40)$must_file == FALSE,
    f(28000, status = "joint", age = 66, age_sp = 66)$must_file == FALSE,
    f(28700, status = "joint", age = 66, age_sp = 66)$must_file == TRUE,
    # Chart C: $400 of SE income compels filing at any gross income
    f(500, earned = 500, se = 500)$must_file == TRUE,
    f(399, earned = 399, se = 399)$must_file == FALSE,
    # Chart B, dependents: unearned > 1,150; earned > 12,950;
    # gross > max(1,150, earned + 400)
    f(1100, earned = 0, type = "dependent")$must_file == FALSE,
    f(1200, earned = 0, type = "dependent")$must_file == TRUE,
    f(5000, earned = 5000, type = "dependent")$must_file == FALSE,
    f(5500, earned = 5000, type = "dependent")$must_file == TRUE,  # gross > earned+400
    f(13000, earned = 13000, type = "dependent")$must_file == TRUE
  )

  # D-A5 sensitivity: a single filer pushed over the threshold ONLY by
  # investment income must show must_file TRUE but must_file_ex_investment
  # FALSE -- that divergence is the whole point of carrying both.
  z <- f(13500, earned = 11000)   # 2,500 investment income
  stopifnot(z$must_file == TRUE, z$must_file_ex_investment == FALSE)

  # An unlisted year fails loudly rather than borrowing a neighbour's
  # constants. The year is DERIVED from the table, not written in: this test
  # named 2019, and adding 2019 to support the group D builds turned a real
  # assertion into a failing one. Anything outside the covered range works.
  unlisted <- max(as.integer(names(FILING_THRESHOLD_COMPONENTS))) + 1L
  err <- tryCatch({ add_filing_requirement(mk(10000), unlisted); FALSE },
                  error = function(e) TRUE)
  stopifnot(err)

  message("  test_filing_requirement: PASSED")
  invisible(TRUE)
}

test_mutual_claim_cycle <- function() {
  # The TY2017 production run found this: a 20yo full-time-student HOUSEHOLDER
  # living with their low-income parent. Pass 2 makes the householder the
  # parent's qualifying child; without the hh_is_dep guard, pass 4 then made
  # the parent the householder's qualifying relative -- a mutual-claim cycle
  # the law forbids (a dependent cannot claim dependents, IRC 152(b)(1)).
  # Expected: ONE unit, headed by the parent, with the householder as its
  # dependent -- the qualifying-child claim takes precedence.
  hh <- rbindlist(list(
    .fixture_person(1, 1, 20, RELATE = 101L, MOMLOC = 2L, FAMREL = 1L,
                    SCHLCOLL = 3L, wages = 500),
    .fixture_person(1, 2, 55, RELATE = 501L, FAMREL = 4L, wages = 3000)))
  b <- build_asec_tax_units(hh, 2022)
  u <- b$units
  stopifnot(u[unit_type == "nondependent", .N] == 1,
            u[unit_type == "nondependent", head_pernum] == 2,   # the parent
            u[unit_type == "nondependent", n_dep] == 1,
            b$persons[PERNUM == 1, role] == "dependent",
            b$persons[PERNUM == 2, role] == "primary")

  # And the legitimate CHAIN still resolves by law: baby -> teen mother ->
  # grandparent means the grandparent claims BOTH (a dependent cannot claim),
  # which is exactly what the re-root loop implements.
  hh2 <- rbindlist(list(
    .fixture_person(2, 1, 45, wages = 60000),
    .fixture_person(2, 2, 17, RELATE = 301L, MOMLOC = 1L, FAMREL = 3L, SCHLCOLL = 1L),
    .fixture_person(2, 3,  1, RELATE = 901L, MOMLOC = 2L, FAMREL = 3L)))
  b2 <- build_asec_tax_units(hh2, 2022)
  stopifnot(b2$units[unit_type == "nondependent", .N] == 1,
            b2$units[unit_type == "nondependent", n_dep] == 2,
            all(b2$persons[PERNUM %in% 2:3, unit_head] == 1))

  message("  test_mutual_claim_cycle: PASSED")
  invisible(TRUE)
}

test_qr_income_limit <- function() {
  # the qualifying-relative limit gates pass 4: at 2022's $4,400 an elderly
  # parent with $5,000 of PENSION income (in gross income, unlike SS) is NOT
  # a dependent
  hh <- function(pension) rbindlist(list(
    .fixture_person(1, 1, 50, wages = 70000),
    .fixture_person(1, 2, 80, RELATE = 501L, FAMREL = 4L, retirement = pension)))
  a <- build_asec_tax_units(hh(3000), 2022)$units
  b <- build_asec_tax_units(hh(5000), 2022)$units
  stopifnot(a[unit_type == "nondependent", .N] == 1,
            b[unit_type == "nondependent", .N] == 2)
  message("  test_qr_income_limit: PASSED")
  invisible(TRUE)
}

#' Every Chart A threshold re-derived from standard deduction + personal
#' exemption + the age-65 addition. Lives here rather than executing at source
#' time in asec_tax_units.R, because main.R sources src/ recursively on every
#' production run and files there must define, not run.
test_threshold_tables <- function() {
  check_filing_requirement_params()
  yrs <- names(FILING_THRESHOLD_COMPONENTS)
  stopifnot(length(yrs) >= 7)
  # every year the builder can be asked for is covered by the check
  for (y in yrs) stopifnot(!is.null(filing_requirement_params(as.integer(y))))
  message(sprintf("  test_threshold_tables: %d years internally consistent (%s)",
                  length(yrs), paste(yrs, collapse = ", ")))
  invisible(TRUE)
}

test_asec_tax_units <- function() {
  test_threshold_tables()
  test_unit_rules()
  test_support_test()
  test_filing_requirement()
  test_qr_income_limit()
  test_mutual_claim_cycle()
  message("test_asec_tax_units: ALL TESTS PASSED")
  invisible(TRUE)
}

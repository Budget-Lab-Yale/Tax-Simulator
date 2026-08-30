# =============================================================================
# ASEC TAX UNITS: reader, income concepts, unit builder, filing threshold
#
# Group C, stages C0-C2 of the non-filer population build. The design of record
# is research/state_weights/nonfiler_residual/10_asec_tax_unit_design.md §4
# (conventions D-A1..D-A7); the plan is research/state_weights/plan.md §3.
#
# Functions only (src/ is sourced recursively by main.R). Drivers live in
# research/state_weights/nonfiler_pool/. Requires src/data/state_weights.R for
# raw_data_root() -- both are always sourced together, by the simulator's
# recursive walk and by the research drivers alike.
#
# The reader trio (read_asec / strip_labels / niu_code) is PROMOTED from
# research/state_weights/nonfiler_residual/09_asec_tax_unit_diagnostics.R
# (one definition per computation); script 09 now sources this file.
#
# Two standing rules enforced here rather than remembered:
#   * S12 -- Census tax-model recodes (FILESTAT, ADJGINC, TAXINC, DEPSTAT as a
#     pointer, ...) never enter construction. They ride along as BENCHMARK
#     columns only (D-A1 says keep FILESTAT precisely because it is calibrated).
#   * S16 -- INCRETIR is not comparable across the 2019 ASEC redesign. The
#     harmonised retirement series is built here and nothing downstream may
#     touch INCRETIR alone across that break.
# =============================================================================


# -----------------------------------------------------------------------------
# Paths and reader (promoted from script 09, unchanged behavior)
# -----------------------------------------------------------------------------

asec_dir <- function() file.path(raw_data_root(), "CPS-ASEC/cps_asec_common")

# ASEC year Y asks about income in calendar year Y-1.
asec_sample_dir <- function(tax_year) {
  file.path(asec_dir(), sprintf("cps%d_03s", tax_year + 1L))
}

# IPUMS puts value labels onto haven attributes; drop them so data.table
# arithmetic and comparisons behave like plain numerics.
strip_labels <- function(dt) dt[, lapply(.SD, function(v) { attributes(v) <- NULL; v })]

read_asec <- function(tax_year, cols = NULL) {
  dd  <- asec_sample_dir(tax_year)
  ddi <- ipumsr::read_ipums_ddi(list.files(dd, "\\.xml$", full.names = TRUE))
  x   <- as.data.table(ipumsr::read_ipums_micro(ddi, verbose = FALSE))
  if (!is.null(cols)) x <- x[, ..cols]
  x <- strip_labels(x)
  x[, tax_year := tax_year]
  setattr(x, "ddi", ddi)
  x[]
}

# NIU codes differ per variable (INCSS 999999, INCINT 9999999, INCWAGE
# 99999999, INCRANN 0, ...). Read them off the DDI rather than hardcoding --
# a guessed cutoff caught one sentinel and not another and produced a $2.46
# quadrillion capital-gains total (2026-08-27). This is 09's original helper,
# kept for its callers: it asserts the large-sentinel convention.
niu_code <- function(ddi, var, x) {
  vl <- ipumsr::ipums_val_labels(ddi, tidyselect::all_of(var))
  hit <- vl$val[stringr::str_detect(vl$lbl, stringr::regex("N\\.?I\\.?U", ignore_case = TRUE))]
  stopifnot(length(hit) == 1, max(x[[var]]) == hit)
  hit
}

#' NIU-blank one income column to 0, handling BOTH sentinel conventions:
#' the large sentinel (INCWAGE 99999999) and the zero sentinel (INCRANN 0 --
#' where NIU and "no income" coincide, so no blanking is needed). A variable
#' with several NIU-ish labels (INCTOT carries Missing + NIU) blanks them all.
#' Returns a plain numeric vector; never NA -- an ASEC income NIU means the
#' person is outside the item's universe, which for unit totals is zero income,
#' not missing data.
clean_asec_income <- function(x, ddi, var) {
  z  <- as.numeric(x[[var]])
  vl <- ipumsr::ipums_val_labels(ddi, tidyselect::all_of(var))
  sentinels <- vl$val[stringr::str_detect(
    vl$lbl, stringr::regex("N\\.?I\\.?U|missing", ignore_case = TRUE))]
  # Top-down, skipping sentinels absent from the data: INCWAGE carries BOTH
  # 99999999 = NIU and 99999998 = "Missing (1962-1966 only)", and the second
  # never occurs in our years -- requiring it to be >= max would (and did)
  # fail on a label that is pure history. A sentinel that IS present must be
  # the current maximum, or real values sit above it and blanking would
  # corrupt them; that stops, never warns.
  for (s in sort(setdiff(sentinels, 0), decreasing = TRUE)) {
    if (!any(z == s)) next
    stopifnot(s == max(z))
    z[z == s] <- 0
  }
  z
}


# -----------------------------------------------------------------------------
# C0 -- income concepts (design note D-A5; decisions S16)
# -----------------------------------------------------------------------------

# Gross-income components, per Mok (2017) p.22: "the log of gross income
# (excluding taxable Social Security income) that would be reported on a tax
# return". Social Security is EXCLUDED from gross income by her definition.
# Components below are the taxable ASEC items available in the common extract;
# nontaxable items (workers' comp, veterans' benefits, public assistance, SSI,
# educational assistance, child support, other assistance) are excluded.
# INCDISAB and INCSURV are partially-taxable in law and included whole here --
# a documented approximation, biased toward filing at the threshold margin.
# Alimony (on Mok's list) has no separable ASEC variable for 2014+ (it is
# inside INCOTHER, which IS included) -- the INCALIM dead end recorded in the
# extract config.
ASEC_GROSS_INCOME_ITEMS <- c(
  "INCWAGE",              # wages and salaries
  "INCBUS", "INCFARM",    # self-employment (nonfarm, farm)
  "INCINT", "INCDIVID",   # interest, dividends
  "INCRENT",              # rent and royalties
  "INCUNEMP",             # unemployment compensation (taxable)
  "INCSURV", "INCDISAB",  # survivor / disability pensions (non-SS)
  "INCOTHER"              # other income n.s. (contains alimony from 2014)
)

# The harmonised-retirement components (S16): the ASEC 2019 redesign split
# pre-2019 INCRETIR (retirement accounts + pensions + annuities, ages 15+) into
# these five, and narrowed INCRETIR itself to retirement accounts at ages 58+.
ASEC_RETIREMENT_COMPONENTS <- c("INCRET1", "INCRET2", "INCPEN1", "INCPEN2", "INCRANN")

#' Add person-level income concepts to a raw ASEC read.
#'
#' Adds, per person (all NIU-blanked to 0):
#'   retirement_income     the S16 harmonised series (bridged across 2018)
#'   se_income             INCBUS + INCFARM (the $400 rule, earned income)
#'   earned_income         wages + se_income (dependent floor, subfamily rule)
#'   investment_income     interest + dividends + rent (the D-A5 sensitivity)
#'   gross_income          Mok's concept: taxable items EXCLUDING Social
#'                         Security, INCLUDING harmonised retirement
#'   gross_income_inc_kg   gross_income + INCCAPG where the variable exists
#'                         (income years 2018+; NA_real_ before -- S16 records
#'                         the asymmetry as hitting the benchmark, not the
#'                         model, so it must stay visible rather than zero)
#'
#' @param x    data.table from read_asec() (needs its ddi attribute)
#' @param ddi  the DDI (defaults to the attribute read_asec() stamped)
add_asec_income_concepts <- function(x, ddi = attr(x, "ddi")) {
  stopifnot(!is.null(ddi))
  yr <- x$tax_year[1]

  for (v in ASEC_GROSS_INCOME_ITEMS)
    data.table::set(x, j = v, value = clean_asec_income(x, ddi, v))

  # Harmonised retirement (S16). Never INCRETIR alone across 2018: it breaks
  # -61% in dollars / -51% in recipients at the redesign, while the bridge
  # moves +6.3%/yr with recipients in step (verified 2026-08-27).
  if (yr <= 2017) {
    x[, retirement_income := clean_asec_income(x, ddi, "INCRETIR")]
  } else {
    stopifnot(all(ASEC_RETIREMENT_COMPONENTS %in% names(x)))
    for (v in ASEC_RETIREMENT_COMPONENTS)
      data.table::set(x, j = v, value = clean_asec_income(x, ddi, v))
    x[, retirement_income := INCRET1 + INCRET2 + INCPEN1 + INCPEN2 + INCRANN]
  }

  x[, se_income         := INCBUS + INCFARM]
  x[, earned_income     := INCWAGE + se_income]
  x[, investment_income := INCINT + INCDIVID + INCRENT]
  x[, gross_income      := INCWAGE + se_income + investment_income + INCUNEMP +
                           INCSURV + INCDISAB + INCOTHER + retirement_income]

  # Capital gains exist as a survey item only from income year 2018 (INCCAPG).
  x[, gross_income_inc_kg := if ("INCCAPG" %in% names(x)) {
    gross_income + clean_asec_income(x, ddi, "INCCAPG")
  } else NA_real_]

  x[]
}


# -----------------------------------------------------------------------------
# C1 -- the unit builder (design note D-A2, rule for rule)
# -----------------------------------------------------------------------------

# IPUMS CPS codes, read off the DDI 2026-08-28 (never from memory):
RELATE_HEAD          <- 101L
RELATE_SPOUSE        <- c(201L, 202L, 203L)
FTYPE_PRIMARY        <- 1L
FTYPE_NONFAM_HEAD    <- 2L
FTYPE_RELATED_SUBFAM <- 3L
FTYPE_UNREL_SUBFAM   <- 4L
FTYPE_SECONDARY      <- 5L
FAMREL_REFERENCE     <- 1L
MARST_SPOUSE_PRESENT <- 1L
SCHLCOLL_FULLTIME    <- c(1L, 3L)   # high school / college, full time
EDUC_HS_DIPLOMA      <- 73L         # first attainment code AT/above HS diploma
EDUC_BACHELORS       <- 111L        # first code at/above bachelor's degree
RACE_WHITE           <- 100L
RACE_BLACK           <- 200L

# Qualifying-child ages (IRC 152(c)): under 19, or under 24 if a full-time
# student. SCHLCOLL's universe is 16-54, so the student test is observable
# exactly where it binds.
QC_AGE_LIMIT         <- 19L
QC_STUDENT_AGE_LIMIT <- 24L

# TRIM3 support test (approved decision Q6, 2026-08-28): household spending is
# proxied by household income divided equally across members; a person is
# self-supporting -- and so cannot be claimed -- when their own income exceeds
# SUPPORT_MULT of that per-capita share. Mok never defines her support test
# operationally; this is the one recorded deviation from "follow Mok".
# The flip-count under 0.4/0.6 is a required output of the build driver.
TRIM3_SUPPORT_MULT <- 0.5

#' Qualifying-relative gross-income limit (IRC 152(d)(1)(B)): the exemption
#' amount, which survives TCJA as an inflation-indexed limit for dependency
#' even though the deduction itself is zero.
qualifying_relative_income_limit <- function(tax_year) {
  limits <- c(`2014` = 3950, `2015` = 4000, `2016` = 4050, `2017` = 4050,
              `2018` = 4150, `2019` = 4200, `2020` = 4300, `2021` = 4300,
              `2022` = 4400, `2023` = 4700, `2024` = 5050)
  out <- limits[as.character(tax_year)]
  stopifnot(!is.na(out))
  unname(out)
}

#' Survey-agnostic subfamily classification (approved decision Q7: the rule is
#' written ONCE, with the survey as an argument, because the ACS reconstructs
#' subfamilies from different inputs and two implementations of one rule is how
#' they drift).
#'
#' ASEC: subfamilies come straight off FTYPE/FAMID -- Census identifies them.
#' ACS: not implemented yet; the stub stops loudly so the eventual implementer
#' lands here and extends ONE function rather than writing a second.
#'
#' @param persons data.table with (asec) FTYPE, FAMID
#' @return integer subfamily group id, 0 for persons not in a subfamily
assign_subfamily <- function(persons, survey = c("asec", "acs")) {
  survey <- match.arg(survey)
  if (survey == "acs") {
    stop("assign_subfamily(): the ACS side is not implemented yet. Extend THIS ",
         "function (IPUMS USA SUBFAM/SFTYPE/SFRELATE differ from Census's ",
         "definition -- see 10_asec_tax_unit_design.md §7.4) rather than ",
         "writing a second rule.")
  }
  stopifnot(all(c("FTYPE", "FAMID") %in% names(persons)))
  fifelse(persons$FTYPE %in% c(FTYPE_RELATED_SUBFAM, FTYPE_UNREL_SUBFAM),
          as.integer(persons$FAMID), 0L)
}

#' Build tax units on one ASEC year, D-A2 rule for rule.
#'
#' Rules (10_asec_tax_unit_design.md §4, D-A2):
#'   1. Householder is the primary; spouses in one household are always one
#'      unit (every SPLOC pair, not only the householder's).
#'   2. Children attach via MOMLOC/POPLOC.
#'   3. Related subfamily whose head has NO earned income -> dependents of the
#'      householder's unit; unrelated subfamily -> its own unit.
#'   4. Adult dependency uses the TRIM3 support test, not age alone.
#'   5. Dependents who meet the filing requirement head their own (dependent)
#'      unit -- constructed here as dependent-headed SCORING units (Mok panel
#'      E); whether they file is what the filing model decides.
#'
#' D-A4: dependents are constructed, retained and TAGGED, never dropped. The
#' emitted pool later excludes them (they ride filer records as dependent
#' slots); the tag is what makes the anchor netting self-consistent.
#'
#' D-A6: filing status is the LEGAL rule only -- HoH requires an unmarried head
#' with a qualifying-child dependent. The Mok-style reallocation of qualifying
#' children across unmarried partners is a separate, explicitly flagged step
#' (not here).
#'
#' @param x            person-level data.table: needs SERIAL, PERNUM, AGE,
#'                     RELATE, MARST, SPLOC, MOMLOC, POPLOC, FTYPE, FAMID,
#'                     SCHLCOLL, ASECWT, and the C0 income concepts
#' @param tax_year     income year (for the qualifying-relative limit)
#' @param support_mult TRIM3 multiplier (default 0.5; drivers sweep 0.4/0.6)
#' @return list(persons, units): persons gains unit_id/role/is_dependent;
#'         units is one row per unit incl. dependent-headed scoring units
#' @param support_unit whether the support test divides resources across the
#'        HOUSEHOLD (TRIM3's phrasing, the default) or the FAMILY. In a
#'        multi-family household -- exactly where subfamilies live -- the
#'        family is arguably the truer support unit, and nobody had tested
#'        which matters. Measured 2026-08-28: it does not (see the note).
build_asec_tax_units <- function(x, tax_year, support_mult = TRIM3_SUPPORT_MULT,
                                 support_unit = c("household", "family")) {
  support_unit <- match.arg(support_unit)

  need <- c("SERIAL", "PERNUM", "AGE", "RELATE", "MARST", "SPLOC", "MOMLOC",
            "POPLOC", "FTYPE", "FAMID", "SCHLCOLL", "ASECWT",
            "gross_income", "earned_income", "total_income")
  stopifnot(all(need %in% names(x)))
  p <- copy(x)
  qr_limit <- qualifying_relative_income_limit(tax_year)

  # --- household aggregates for the support test -----------------------------
  # Household income per capita proxies per-capita spending (TRIM3). Household,
  # not family: the design note's phrase is "household spending divided
  # equally". The test runs on TOTAL income (the survey's INCTOT concept,
  # Social Security and transfers included), NOT the tax gross-income concept:
  # support is paid from all resources, and running it on Mok's ex-SS measure
  # made every co-resident elderly person living on their own Social Security
  # look resourceless -- the TY2017 build tagged 19.2M adult dependents against
  # a 13.8M survey benchmark before this was caught (2026-08-28). The tax
  # concept keeps its two jobs: the QR income limit and the filing threshold.
  if (support_unit == "household") {
    p[, `:=`(hh_income = sum(total_income), hh_size = .N), by = SERIAL]
  } else {
    stopifnot("FAMUNIT" %in% names(p))
    p[, `:=`(hh_income = sum(total_income), hh_size = .N), by = .(SERIAL, FAMUNIT)]
  }
  p[, self_supporting := total_income > support_mult * (hh_income / hh_size)]

  p[, subfam_id := assign_subfamily(p, "asec")]

  # --- pass 1: spouse pairs are one unit (rule 1) ----------------------------
  # Key each unit by the head's PERNUM within SERIAL. For a spouse pair the
  # head is the householder if present in the pair, else the lower PERNUM.
  p[, unit_head := NA_integer_]
  p[, role := NA_character_]

  sp <- p[MARST == MARST_SPOUSE_PRESENT & SPLOC > 0]
  if (nrow(sp)) {
    pairs <- sp[PERNUM < SPLOC, .(SERIAL, a = PERNUM, b = SPLOC)]
    # a partner reported spouse-present whose SPLOC points at a nonexistent
    # line would silently orphan the spouse; stop instead
    chk <- merge(pairs, p[, .(SERIAL, b = PERNUM, ok = TRUE)],
                 by = c("SERIAL", "b"), all.x = TRUE)
    stopifnot(!anyNA(chk$ok))
    hh_head <- p[RELATE == RELATE_HEAD, .(SERIAL, hh = PERNUM)]
    pairs <- merge(pairs, hh_head, by = "SERIAL", all.x = TRUE)
    pairs[, head := fifelse(!is.na(hh) & hh == b, b, a)]
    # one long assignment table -- (SERIAL, PERNUM, head, role) per partner --
    # rather than positional i.-column arithmetic inside two update joins
    assign_sp <- rbindlist(list(
      pairs[, .(SERIAL, PERNUM = a, head, role = fifelse(head == a, "primary", "spouse"))],
      pairs[, .(SERIAL, PERNUM = b, head, role = fifelse(head == b, "primary", "spouse"))]))
    p[assign_sp, on = .(SERIAL, PERNUM),
      `:=`(unit_head = i.head, role = i.role)]
  }

  # --- pass 2: qualifying children via MOMLOC/POPLOC (rule 2) ----------------
  # A child attaches to a parent's unit when: a parent pointer resolves, the
  # child is not married-spouse-present (a joint unit is never a dependent),
  # the age/student test passes, and the child is not self-supporting.
  p[, parent_loc := fifelse(MOMLOC > 0, MOMLOC, POPLOC)]
  p[, is_full_time_student := SCHLCOLL %in% SCHLCOLL_FULLTIME]
  p[, qc_age_ok := AGE < QC_AGE_LIMIT |
                   (AGE < QC_STUDENT_AGE_LIMIT & is_full_time_student)]
  p[, is_qc := parent_loc > 0 &
               MARST != MARST_SPOUSE_PRESENT &
               qc_age_ok &
               !self_supporting]

  # The parent's own unit head: parents already in spouse pairs carry
  # unit_head from pass 1; unmarried parents head their own unit.
  parent_head <- p[, .(SERIAL, parent_loc = PERNUM,
                       parent_unit = fifelse(is.na(unit_head), PERNUM, unit_head))]
  p <- merge(p, parent_head, by = c("SERIAL", "parent_loc"),
             all.x = TRUE, sort = FALSE)
  p[is_qc & !is.na(parent_unit) & is.na(unit_head),
    `:=`(unit_head = parent_unit, role = "dependent")]

  # --- pass 3: related subfamilies with a no-earnings head (rule 3) ----------
  # The whole subfamily becomes dependents of the HOUSEHOLDER's unit when the
  # subfamily head is unmarried, has no earned income, and is not
  # self-supporting; an earning subfamily head keeps their own unit (their
  # children attached to THEM in pass 2 and stay attached).
  # Householder table for passes 3 and 4, computed AFTER pass 2 because it
  # must know whether the householder is by now someone's dependent: a
  # young student householder living with a low-income parent is that
  # parent's qualifying child (pass 2), and the parent must then NOT become
  # the householder's qualifying relative -- a dependent cannot claim
  # dependents (IRC 152(b)(1)), and the qualifying-child claim takes
  # precedence. Without this guard the two claims form a cycle that the
  # re-root loop below rightly refuses to untangle.
  hh_head_tbl <- p[RELATE == RELATE_HEAD,
                   .(SERIAL, hh_unit = PERNUM,
                     hh_is_dep = fifelse(is.na(role), FALSE, role == "dependent"))]

  p[, sf_head_pernum := fifelse(subfam_id > 0 & FAMREL == FAMREL_REFERENCE,
                                PERNUM, NA_integer_)]
  sf <- p[subfam_id > 0,
          .(sf_head = na.omit(sf_head_pernum)[1]), by = .(SERIAL, subfam_id)]
  sf <- merge(sf,
              p[, .(SERIAL, sf_head = PERNUM, head_earned = earned_income,
                    head_marst = MARST, head_self_sup = self_supporting,
                    head_ftype = FTYPE)],
              by = c("SERIAL", "sf_head"), all.x = TRUE)
  sf <- merge(sf, hh_head_tbl, by = "SERIAL", all.x = TRUE)
  absorb <- sf[head_ftype == FTYPE_RELATED_SUBFAM &
               head_marst != MARST_SPOUSE_PRESENT &
               head_earned == 0 & head_self_sup == FALSE &
               !is.na(hh_unit) & hh_is_dep == FALSE]
  if (nrow(absorb)) {
    # The WHOLE subfamily becomes dependents of the householder, overriding
    # pass 2: a child attached to the subfamily head there would otherwise sit
    # in a unit whose head just became a dependent -- a unit with no primary.
    p[absorb, on = .(SERIAL, subfam_id),
      `:=`(unit_head = i.hh_unit, role = "dependent")]
  }

  # --- pass 4: qualifying relatives (rule 4) ----------------------------------
  # Other RELATED household members (never spouses/partners of the head, never
  # unrelated persons -- the member-of-household route for unrelated persons is
  # deliberately not modeled, per D-A6's legal-rule-first ordering): unmarried,
  # gross income under the exemption limit, not self-supporting -> dependents
  # of the householder's unit.
  p <- merge(p, hh_head_tbl, by = "SERIAL", all.x = TRUE, sort = FALSE)
  p[is.na(unit_head) &
    RELATE %in% c(301L, 303L, 501L, 701L, 901L, 1001L) &
    MARST != MARST_SPOUSE_PRESENT &
    gross_income < qr_limit &
    self_supporting == FALSE &
    !is.na(hh_unit) & hh_unit != PERNUM & hh_is_dep == FALSE,
    `:=`(unit_head = hh_unit, role = "dependent")]

  # --- pass 5: everyone left heads their own unit ------------------------------
  p[is.na(unit_head), `:=`(unit_head = PERNUM, role = "primary")]

  # --- re-root: a dependent may not head a unit --------------------------------
  # Pass 2 attaches children to a parent whom pass 3 or 4 can later demote to
  # a dependent (a no-earnings related-subfamily head, or a low-income "other
  # relative" parent). Their children must follow the parent to the claimer's
  # unit, and chains can run several links (grandchild -> mother ->
  # grandmother -> householder), so iterate to a fixed point. A bounded loop:
  # ten links would mean a pointer cycle, which is corrupt data, not depth.
  for (i in 1:10) {
    head_map <- p[, .(SERIAL, h = PERNUM, h_role = role, h_up = unit_head)]
    p[head_map, on = .(SERIAL, unit_head = h),
      `:=`(h_role = i.h_role, h_up = i.h_up)]
    bad <- p$h_role == "dependent" & p$unit_head != p$PERNUM
    if (!any(bad)) break
    stopifnot(i < 10, all(p$h_up[bad] != p$unit_head[bad]))
    p[bad, unit_head := h_up]
  }
  p[, c("h_role", "h_up") := NULL]
  # numeric on purpose: SERIAL * 100L overflows 32-bit integers for large
  # serials, and integer NA there would silently merge units
  p[, unit_id := as.numeric(SERIAL) * 100 + unit_head]
  p[, is_dependent := role == "dependent"]

  # Every unit must have exactly one primary; a violation means two passes
  # claimed the same person and the build is wrong, not the data.
  chk <- p[role == "primary", .N, by = unit_id]
  stopifnot(all(chk$N == 1), uniqueN(p$unit_id) == nrow(chk))

  units <- build_asec_unit_table(p, tax_year)
  list(persons = p[], units = units)
}

#' Collapse the person table to one row per unit, plus dependent-headed
#' scoring units (D-A2 rule 5 / Mok panel E) for dependents aged 15+ (the
#' ASEC income universe -- younger dependents cannot carry income and are not
#' in Mok's frame).
build_asec_unit_table <- function(p, tax_year) {

  # Head demographics ride along when the raw columns were read (the filing
  # model scores on the PRIMARY taxpayer's characteristics, per Mok's Table 14
  # title); a build from a fixture without them still works.
  demo_cols <- intersect(c("EDUC", "RACE", "HISPAN", "SEX"), names(p))

  hs <- p[role %in% c("primary", "spouse")]
  core <- hs[, {
    is_head <- role == "primary"
    has_sp  <- any(role == "spouse")
    out <- list(
      weight        = ASECWT[is_head],
      filing_status = if (has_sp) "joint" else NA_character_,
      age_head      = AGE[is_head],
      age_spouse    = if (has_sp) as.numeric(AGE[role == "spouse"][1]) else NA_real_,
      # unit income = head + spouse (a dependent's income belongs to the
      # dependent, who is scored as their own dependent-headed unit)
      gross_income        = sum(gross_income),
      gross_income_inc_kg = sum(gross_income_inc_kg),
      earned_income       = sum(earned_income),
      se_income           = sum(se_income),
      investment_income   = sum(investment_income),
      retirement_income   = sum(retirement_income),
      INCWAGE = sum(INCWAGE), INCINT = sum(INCINT), INCDIVID = sum(INCDIVID),
      INCRENT = sum(INCRENT), INCSS = sum(INCSS), INCUNEMP = sum(INCUNEMP),
      SERIAL = SERIAL[1], head_pernum = PERNUM[is_head])
    for (v in demo_cols) out[[paste0(tolower(v), "_head")]] <- .SD[[v]][is_head]
    if ("SEX" %in% demo_cols)
      out$sex_spouse <- if (has_sp) as.numeric(.SD$SEX[role == "spouse"][1]) else NA_real_
    out
  }, by = unit_id]

  deps <- p[role == "dependent",
            .(n_dep = .N,
              n_dep_qc = sum(is_qc),
              n_dep_adult = sum(AGE >= 18),
              dep_ages = list(sort(AGE))),
            by = unit_id]
  units <- merge(core, deps, by = "unit_id", all.x = TRUE)
  units[is.na(n_dep), `:=`(n_dep = 0L, n_dep_qc = 0L, n_dep_adult = 0L)]

  # Legal filing status (D-A6): HoH needs an unmarried head with at least one
  # qualifying-child dependent. The cost-of-keeping-up-home test is
  # unobservable and omitted -- the documented approximation of the legal rule.
  units[is.na(filing_status),
        filing_status := fifelse(n_dep_qc > 0, "hoh", "single")]
  units[, unit_type := "nondependent"]

  # Dependent-headed scoring units: dependents aged 15+ (the ASEC income
  # universe), income already on their own person record. They are ALSO
  # counted in their claimer's n_dep -- that is D-A4's retain-and-tag, not a
  # double count: the claimer's unit carries them as dependents, and this row
  # exists to score their own filing. The 1e9 offset keeps the id space
  # disjoint from base units (max base id ~ SERIAL*100+99, well under 1e9
  # only for small serials -- hence asserted below, not assumed).
  dep_units <- p[role == "dependent" & AGE >= 15, {
    out <- list(
      unit_id       = as.numeric(SERIAL) * 100 + PERNUM + 1e9,
      weight        = ASECWT,
      filing_status = "single",
      age_head      = as.numeric(AGE),
      age_spouse    = NA_real_,
      gross_income = gross_income, gross_income_inc_kg = gross_income_inc_kg,
      earned_income = earned_income, se_income = se_income,
      investment_income = investment_income, retirement_income = retirement_income,
      INCWAGE = INCWAGE, INCINT = INCINT, INCDIVID = INCDIVID,
      INCRENT = INCRENT, INCSS = INCSS, INCUNEMP = INCUNEMP,
      SERIAL = SERIAL, head_pernum = PERNUM)
    for (v in demo_cols) out[[paste0(tolower(v), "_head")]] <- .SD[[v]]
    if ("SEX" %in% demo_cols) out$sex_spouse <- NA_real_
    c(out, list(n_dep = 0L, n_dep_qc = 0L, n_dep_adult = 0L,
                unit_type = "dependent"))
  }]
  units[, dep_ages := NULL]   # list-column: useful upstream, unstackable here
  stopifnot(nrow(dep_units) == 0 || max(units$unit_id) < 1e9)

  out <- rbindlist(list(units, dep_units), use.names = TRUE)
  out[, tax_year := tax_year]
  stopifnot(uniqueN(out$unit_id) == nrow(out))
  out[]
}


# -----------------------------------------------------------------------------
# C2 -- the filing threshold (design memo §3.2.1)
# -----------------------------------------------------------------------------

#' IRS Form 1040 Chart A/B/C filing-requirement parameters, per tax year.
#' Chart A thresholds are gross-income floors by status x age-65; Chart B is
#' the dependent floor (earned / unearned / the earned+increment rule); Chart C
#' is the $400 self-employment rule. Values from the year's Form 1040
#' instructions. Extend the table when a new build year is added -- the
#' stopifnot makes an unlisted year fail loudly rather than borrow a neighbor.
#' Through 2017 a Chart A threshold is the standard deduction plus the personal
#' exemption(s); from 2018 TCJA repealed the exemption and it is the standard
#' deduction alone. The age-65 rows add that year's ADDITIONAL standard
#' deduction, once per qualifying spouse. `check_filing_requirement_params()`
#' below re-derives every entry from those components, so a transcription slip
#' fails at source time rather than silently shifting the above/below split.
filing_requirement_params <- function(tax_year) {
  tbl <- list(
    # -- pre-TCJA: standard deduction + personal exemption ---------------------
    `2014` = list(single = 10150, single_65 = 11700,
                  joint = 20300, joint_one65 = 21500, joint_both65 = 22700,
                  hoh = 13050, hoh_65 = 14600,
                  dep_unearned = 1000, dep_earned = 6200, dep_increment = 350),
    `2015` = list(single = 10300, single_65 = 11850,
                  joint = 20600, joint_one65 = 21850, joint_both65 = 23100,
                  hoh = 13250, hoh_65 = 14800,
                  dep_unearned = 1050, dep_earned = 6300, dep_increment = 350),
    `2016` = list(single = 10350, single_65 = 11900,
                  joint = 20700, joint_one65 = 21950, joint_both65 = 23200,
                  hoh = 13350, hoh_65 = 14900,
                  dep_unearned = 1050, dep_earned = 6300, dep_increment = 350),
    `2017` = list(single = 10400, single_65 = 11950,
                  joint = 20800, joint_one65 = 22050, joint_both65 = 23300,
                  hoh = 13400, hoh_65 = 14950,
                  dep_unearned = 1050, dep_earned = 6350, dep_increment = 350),
    # -- TCJA: standard deduction only, no personal exemption ------------------
    `2018` = list(single = 12000, single_65 = 13600,
                  joint = 24000, joint_one65 = 25300, joint_both65 = 26600,
                  hoh = 18000, hoh_65 = 19600,
                  dep_unearned = 1050, dep_earned = 12000, dep_increment = 350),
    `2019` = list(single = 12200, single_65 = 13850,
                  joint = 24400, joint_one65 = 25700, joint_both65 = 27000,
                  hoh = 18350, hoh_65 = 20000,
                  dep_unearned = 1100, dep_earned = 12200, dep_increment = 350),
    `2020` = list(single = 12400, single_65 = 14050,
                  joint = 24800, joint_one65 = 26100, joint_both65 = 27400,
                  hoh = 18650, hoh_65 = 20300,
                  dep_unearned = 1100, dep_earned = 12400, dep_increment = 350),
    `2021` = list(single = 12550, single_65 = 14250,
                  joint = 25100, joint_one65 = 26450, joint_both65 = 27800,
                  hoh = 18800, hoh_65 = 20500,
                  dep_unearned = 1100, dep_earned = 12550, dep_increment = 350),
    `2022` = list(single = 12950, single_65 = 14700,
                  joint = 25900, joint_one65 = 27300, joint_both65 = 28700,
                  hoh = 19400, hoh_65 = 21150,
                  dep_unearned = 1150, dep_earned = 12950, dep_increment = 400),
    `2023` = list(single = 13850, single_65 = 15700,
                  joint = 27700, joint_one65 = 29200, joint_both65 = 30700,
                  hoh = 20800, hoh_65 = 22650,
                  dep_unearned = 1250, dep_earned = 13850, dep_increment = 400),
    `2024` = list(single = 14600, single_65 = 16550,
                  joint = 29200, joint_one65 = 30750, joint_both65 = 32300,
                  hoh = 21900, hoh_65 = 23850,
                  dep_unearned = 1300, dep_earned = 14600, dep_increment = 450)
  )
  out <- tbl[[as.character(tax_year)]]
  stopifnot(!is.null(out))
  out
}

#' The components each Chart A threshold is built from: standard deduction by
#' status, personal exemption (0 from 2018), and the additional standard
#' deduction for age 65. Kept separate from the table above so the two can be
#' checked against each other.
FILING_THRESHOLD_COMPONENTS <- list(
  #             sd_single sd_joint sd_hoh    pe  add_single add_married
  `2014` = c( 6200, 12400,  9100, 3950, 1550, 1200),
  `2015` = c( 6300, 12600,  9250, 4000, 1550, 1250),
  `2016` = c( 6300, 12600,  9300, 4050, 1550, 1250),
  `2017` = c( 6350, 12700,  9350, 4050, 1550, 1250),
  `2018` = c(12000, 24000, 18000,    0, 1600, 1300),
  `2019` = c(12200, 24400, 18350,    0, 1650, 1300),
  `2020` = c(12400, 24800, 18650,    0, 1650, 1300),
  `2021` = c(12550, 25100, 18800,    0, 1700, 1350),
  `2022` = c(12950, 25900, 19400,    0, 1750, 1400),
  `2023` = c(13850, 27700, 20800,    0, 1850, 1500),
  `2024` = c(14600, 29200, 21900,    0, 1950, 1550)
)

#' Re-derive every Chart A threshold from its components and stop on any
#' mismatch. Called at source time: these constants decide which units are
#' obligated to file, so a wrong digit moves the above/below partition and
#' every downstream target with it.
check_filing_requirement_params <- function() {
  for (y in names(FILING_THRESHOLD_COMPONENTS)) {
    k  <- FILING_THRESHOLD_COMPONENTS[[y]]
    sd_single <- k[1]; sd_joint <- k[2]; sd_hoh <- k[3]
    pe <- k[4]; add_s <- k[5]; add_m <- k[6]
    fp <- filing_requirement_params(as.integer(y))
    expected <- list(
      single       = sd_single + pe,
      single_65    = sd_single + pe + add_s,
      joint        = sd_joint  + 2 * pe,
      joint_one65  = sd_joint  + 2 * pe + add_m,
      joint_both65 = sd_joint  + 2 * pe + 2 * add_m,
      hoh          = sd_hoh    + pe,
      hoh_65       = sd_hoh    + pe + add_s
    )
    for (nm in names(expected)) {
      if (!isTRUE(all.equal(fp[[nm]], expected[[nm]]))) {
        stop(sprintf(paste("filing_requirement_params(%s)$%s = %s but its",
                           "components imply %s"),
                     y, nm, fp[[nm]], expected[[nm]]), call. = FALSE)
      }
    }
    # Chart B: the dependent earned-income floor IS the single standard
    # deduction in every year the IRS has published.
    stopifnot(fp$dep_earned == sd_single)
  }
  invisible(TRUE)
}
check_filing_requirement_params()

SE_FILING_FLOOR <- 400   # Chart C: net self-employment earnings of $400+

#' Add `must_file` (the legal filing requirement) and `filing_threshold` (the
#' gross-income floor the unit faces) to a unit table.
#'
#' Three routes into a filing requirement, matching the Form 1040 charts:
#'   A. gross income at/above the status x age-65 threshold
#'   B. (dependent units) unearned above the floor, earned above the standard
#'      deduction, or gross above max(unearned floor, earned + increment)
#'   C. net self-employment earnings of $400 or more
#'
#' Per D-A5 the sensitivity `must_file_ex_investment` is carried alongside:
#' the same test with investment income removed from gross income, because
#' ASEC interest runs 2.6x SOI and the filer count must be shown not to turn
#' on it.
add_filing_requirement <- function(units, tax_year) {
  fp <- filing_requirement_params(tax_year)

  units[, threshold := fcase(
    filing_status == "joint" & age_head >= 65 & !is.na(age_spouse) & age_spouse >= 65,
      fp$joint_both65,
    filing_status == "joint" & (age_head >= 65 | (!is.na(age_spouse) & age_spouse >= 65)),
      fp$joint_one65,
    filing_status == "joint",  fp$joint,
    filing_status == "hoh" & age_head >= 65,    fp$hoh_65,
    filing_status == "hoh",    fp$hoh,
    age_head >= 65,            fp$single_65,
    default = fp$single
  )]

  units[, unearned_income := gross_income - earned_income]
  units[, must_file := fifelse(
    unit_type == "dependent",
    unearned_income > fp$dep_unearned |
      earned_income > fp$dep_earned |
      gross_income  > pmax(fp$dep_unearned, earned_income + fp$dep_increment),
    gross_income >= threshold
  )]
  units[abs(se_income) >= SE_FILING_FLOOR, must_file := TRUE]

  units[, gross_income_ex_investment := gross_income - investment_income]
  units[, must_file_ex_investment := fifelse(
    unit_type == "dependent",
    (unearned_income - investment_income) > fp$dep_unearned |
      earned_income > fp$dep_earned |
      gross_income_ex_investment >
        pmax(fp$dep_unearned, earned_income + fp$dep_increment),
    gross_income_ex_investment >= threshold
  )]
  units[abs(se_income) >= SE_FILING_FLOOR, must_file_ex_investment := TRUE]

  units[]
}

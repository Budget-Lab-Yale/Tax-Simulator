# =============================================================================
# state_weights.R  —  Phase 1 prototype: split state weights for the PUF
#
# Produces `state_weights_{year}.csv` (id, state, weight) that make the national
# PUF-based microdata state-representative. The real methods (calibration,
# gradient) satisfy the per-record split constraint  Σ_state W[i,state] =
# weight_i  over all 53 jurisdictions. The "placeholder" stopgap does NOT (see
# build_state_weights). Either way, federal aggregates are invariant to the
# with-state / without-state switch NOT because of this split constraint but
# because the federal totals are computed from the untouched `weight` column;
# state weights never enter them.
#
# Two interchangeable methods, selected by build_state_weights(method=):
#   "calibration" — Approach A: classical raking / calibration to targets
#                   (OTA TP-6 / TPC lineage).
#   "gradient"    — Approach B: differentiable reweighting via softmax logits and
#                   analytic-gradient descent (PolicyEngine ECPS lineage). Torch is
#                   not installed on the cluster, so the prototype uses a
#                   dependency-free matrix implementation of the same objective.
#
# Records are PARTITIONED by the exogenous `filer` flag and each partition targets
# a different source:
#   filers     → IRS SOI Historic Table 2 (state × AGI-class), FILERS ONLY.
#   non-filers → ACS/Census state × age × income population margins.
# Both partitions share the per-record split constraint, so their union preserves
# every national total. See other/state_tax_research/state_tax_implementation_plan.md
# §2.1 "Filers vs non-filers".
#
# Design + rationale: other/state_tax_research/state_tax_implementation_plan.md,
#                     other/state_tax_research/state_weights_ml_alternative.md
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

# -----------------------------------------------------------------------------
# Jurisdiction set. 50 states + DC are modeled; PR and OA (SOI "Other Areas") are
# carried as no-tax buckets so weights still sum to the national total (plan §5.4).
# -----------------------------------------------------------------------------
STATE_JURISDICTIONS <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
  "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
  "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
  "WV","WI","WY")                       # 51 modeled
NONTAX_BUCKETS <- c("PR","OA")          # carried, no state income tax calc

# -----------------------------------------------------------------------------
# Shared raw-data stores. Derived from the production output root in
# config/interfaces/output_roots.yaml (raw_data is its sibling directory), so
# no additional machine paths are hardcoded here.
#   IRS-Ind: SOI individual income tax mirror -- by-geography (HT2 here) plus
#            national Pub 1304 by-size tables (github.com/johniselin-budget-lab/
#            IRS-Ind, renamed from IRS-GEO 2026-08; see NOTES.md at the store
#            root for data caveats)
#   ACS:     IPUMS USA 1-year fixed-width extracts, us{year}a/ vintages
# -----------------------------------------------------------------------------
raw_data_root <- function() {
  file.path(yaml::read_yaml("./config/interfaces/output_roots.yaml")$production,
            "raw_data")
}
ht2_path <- function(year) {
  file.path(raw_data_root(), "IRS-Ind/state/HT2", sprintf("ht2_%d.csv.gz", year))
}
acs_extract_dir <- function(acs_year) {
  file.path(raw_data_root(), "ACS/acs_common", sprintf("us%da", acs_year))
}

# -----------------------------------------------------------------------------
# SOI HT2 AGI_STUB → [lower, upper) AGI bracket, in nominal dollars.
# 10-class scheme, verified from the data (2017–2022 all carry stubs 1..10, with
# stub 1 = "under $1": it holds negative AGI). A record's stub is found by
# lower ≤ agi < upper. Pre-2017 HT2 used a coarser scheme; extend here if needed.
# -----------------------------------------------------------------------------
ht2_stub_breaks <- function(year) {
  data.table(
    agi_stub = 1:10,
    lower = c(-Inf,       1,   10e3, 25e3, 50e3,  75e3, 100e3, 200e3, 500e3, 1e6),
    upper = c(   1,   10e3,   25e3, 50e3, 75e3, 100e3, 200e3, 500e3,   1e6, Inf))
}

# -----------------------------------------------------------------------------
# read_ht2(): read one HT2 state × AGI CSV into a tidy long table of filer targets.
# Amounts (A*) are in $thousands in the file and converted to dollars. Counts (N*)
# are numbers of returns. Comma-formatted numeric strings are parsed.
#
# Returns a data.table: state, agi_stub, variable, value  (state totals, stub 0,
# and the US row are dropped; only the 51 + PR + OA jurisdictions × stubs 1..K).
# -----------------------------------------------------------------------------
# SOI code -> target-series name. Counts: N1 total returns; MARS1/2/4
# single/joint/HoH returns; N2 is "number of exemptions" through TY2017 and
# "number of individuals" from TY2018 (TCJA; SOI kept the column name and
# repointed the concept -- both count people represented on returns, and
# calibration is within-year, so the relabel never crosses a target cell).
# Amounts (A*) and their return counts (N*) follow the plan §2.1 target set:
# AGI, wages, interest, dividends, capital gains, SALT income/sales tax,
# real-estate tax, mortgage interest, EITC. Itemized-deduction items (18425/
# 18500/19300) reflect post-TCJA itemizer collapse from 2018 -- within-year
# targets, so levels are comparable to same-year PUF Schedule A amounts.
HT2_TARGET_MAP <- c(
  N1     = "n_returns", MARS1 = "n_single", MARS2 = "n_joint",
  MARS4  = "n_hoh",     N2    = "n_indiv",
  A00100 = "agi_amt",        N00200 = "n_wages",      A00200 = "wages_amt",
  N00300 = "n_int",          A00300 = "int_amt",
  N00600 = "n_div",          A00600 = "div_amt",
  N01000 = "n_kg",           A01000 = "kg_amt",
  N18425 = "n_salt_inc",     A18425 = "salt_inc_amt",
  N18450 = "n_salt_sales",   A18450 = "salt_sales_amt",
  N18500 = "n_re_tax",       A18500 = "re_tax_amt",
  N19300 = "n_mort_int",     A19300 = "mort_int_amt",
  N59660 = "n_eitc",         A59660 = "eitc_amt")

read_ht2 <- function(path, year) {

  # Files in the IRS-GEO mirror are gzipped; quoted comma-formatted numbers
  # are handled by fread + the comma-stripping parse below
  d <- if (grepl("\\.gz$", path)) {
    fread(cmd = paste("zcat", shQuote(path)), colClasses = "character")
  } else {
    fread(path, colClasses = "character")
  }
  setnames(d, toupper(names(d)))

  parse_num <- function(x) as.numeric(str_replace_all(x, ",", ""))

  # Tolerate the pre-2014 AGI column name
  if (!("A00100" %in% names(d)) && "A001" %in% names(d)) {
    setnames(d, "A001", "A00100")
  }
  keep <- intersect(names(HT2_TARGET_MAP), names(d))
  keep_amounts <- keep[startsWith(keep, "A")]

  d[, STATE := toupper(trimws(STATE))]
  d[, AGI_STUB := as.integer(AGI_STUB)]
  d <- d[!(STATE %in% c("US","")) & AGI_STUB != 0]              # drop US total & stub-0 total

  d[, (keep) := lapply(.SD, parse_num), .SDcols = keep]

  long <- melt(d[, c("STATE","AGI_STUB", keep), with = FALSE],
               id.vars = c("STATE","AGI_STUB"),
               variable.name = "variable", value.name = "value")
  # amounts ($ thousands in HT2) -> dollars
  long[variable %in% keep_amounts, value := value * 1000]
  long[, variable := HT2_TARGET_MAP[as.character(variable)]]
  setnames(long, c("STATE","AGI_STUB"), c("state","agi_stub"))
  long[, year := year][]
}

# -----------------------------------------------------------------------------
# ACS side: non-filer population margins.
#
# HT2 covers filers only, so the non-filer PUF partition is targeted to ACS/Census
# state × age × income margins instead (plan §2.1). This builds those margins from
# the local IPUMS USA extract; shared cluster copies live at
# /nfs/roberts/project/pi_nrs36/shared/raw_data/ACS/acs_common (us{year}a/
# vintages 2006-2024: usa_{year}a.dat.gz + DDI xml + variables.csv). The authority for WHO is a non-filer is the Tax-Data
# `filer` flag on the PUF; ACS supplies only the geographic MARGINS across which the
# non-filer partition is spread — so the ACS filing model here need only be a
# reasonable v0, refined against the reconciliation total (plan risk item).
#
# v0 tax-unit + filing model (documented approximations, to refine later):
#   - Couples (MARST==1 & SPLOC>0) file jointly, head = lower PERNUM.
#   - Children (AGE < 19, parent pointer present, not married) are dependents.
#   - All other adults head their own unit (single, or HoH if they have dependents).
#   - Qualifying-relative dependency NOT modeled (minor); student ages 19–24 not
#     separated (SCHOOL is in the common extract; wiring it is a v1 upgrade --
#     see other/state_tax_research/nonfiler_residual_design.md §3.2).
#   - Filer if unit gross income ≥ the filing threshold (standard-deduction proxy by
#     filing status, year-specific); else non-filer. Ignores the $400 SE rule,
#     dependents' own filing, and elderly/blind bumps (approximation).
# -----------------------------------------------------------------------------
FIPS_TO_STATE <- c(
  "1"="AL","2"="AK","4"="AZ","5"="AR","6"="CA","8"="CO","9"="CT","10"="DE",
  "11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL","18"="IN","19"="IA",
  "20"="KS","21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI","27"="MN",
  "28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ","35"="NM",
  "36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR","42"="PA","44"="RI",
  "45"="SC","46"="SD","47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA",
  "54"="WV","55"="WI","56"="WY")

# Standard-deduction filing thresholds by year × filing status (nominal $).
filing_threshold <- function(year) {
  # single, joint, hoh — standard deduction (the dominant filing-requirement floor)
  tbl <- list(
    `2017` = c(single = 10400, joint = 20800, hoh = 13400),
    `2018` = c(single = 12000, joint = 24000, hoh = 18000),
    `2019` = c(single = 12200, joint = 24400, hoh = 18350),
    `2020` = c(single = 12400, joint = 24800, hoh = 18650),
    `2021` = c(single = 12550, joint = 25100, hoh = 18800),
    `2022` = c(single = 12950, joint = 25900, hoh = 19400))
  tbl[[as.character(year)]]
}

age_band <- function(age) {
  cut(age, breaks = c(-Inf, 25, 35, 45, 55, 65, 75, Inf),
      labels = c("u25","25_34","35_44","45_54","55_64","65_74","75p"), right = FALSE)
}
income_tier <- function(inc) {
  cut(pmax(inc, 0), breaks = c(-Inf, 1, 10e3, 25e3, 50e3, Inf),
      labels = c("neg_zero","1_10k","10_25k","25_50k","50k_plus"), right = FALSE)
}

#' Read the needed variables from a shared IPUMS USA fixed-width extract
#' (us{year}a/: usa_{year}a.dat.gz + variables.csv giving column positions).
#' Applies implied-decimal scaling (PERWT carries 2) and recodes the IPUMS
#' INCTOT N/A sentinel (9999999, the 7-digit max) to NA -- without which every
#' child inflates unit income by ~$10M and everyone looks like a filer.
read_acs_extract <- function(acs_year,
                             cols = c("YEAR","STATEFIP","PERWT","PERNUM","SERIAL","SAMPLE",
                                      "AGE","MARST","SPLOC","MOMLOC","POPLOC","INCTOT")) {

  dir <- acs_extract_dir(acs_year)
  v   <- fread(file.path(dir, "variables.csv"))
  v   <- v[var_name %in% cols]
  stopifnot(all(cols %in% v$var_name))

  a <- readr::read_fwf(
    file      = file.path(dir, sprintf("usa_%da.dat.gz", acs_year)),
    col_positions = readr::fwf_positions(start = v$start, end = v$end,
                                         col_names = v$var_name),
    col_types = paste(rep("d", nrow(v)), collapse = ""),
    progress  = FALSE
  ) |> as.data.table()

  # Implied decimals
  for (i in which(v$imp_decim > 0)) {
    nm <- v$var_name[i]
    a[, (nm) := get(nm) / 10^v$imp_decim[i]]
  }
  # IPUMS N/A sentinel for INCTOT
  if ("INCTOT" %in% cols) {
    a[INCTOT >= 9999998, INCTOT := NA_real_]
  }
  a[]
}

#' Build ACS non-filer margins + state population totals from an IPUMS extract.
#' @param acs data.table from read_acs_extract() (or any source with the same
#'        columns); `year` is the TAX year the margins will target
#' @return list(nonfiler_margins = dt[state, age_band, income_tier, n_units],
#'              filer_units      = dt[state, n_units],
#'              state_pop        = dt[state, pop])   all weighted to population.
build_acs_margins <- function(acs, year, acs_year = NULL) {

  a <- as.data.table(acs)
  # De-pool: the extract may stack multiple ACS 1-year samples (summing PERWT across
  # them double-counts population). Keep exactly one survey YEAR.
  yrs <- sort(unique(a$YEAR))
  if (is.null(acs_year)) {
    acs_year <- max(yrs)
    if (length(yrs) > 1)
      message("  ACS extract pools years ", paste(yrs, collapse=","),
              "; using YEAR=", acs_year, " (pass acs_year= to override)")
  }
  a <- a[YEAR == acs_year]
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]                                       # drop PR/other non-mapped
  a[, hh_id := paste(SAMPLE, SERIAL, sep = "-")]

  # --- v0 tax units from pointers -------------------------------------------
  a[, married_present := MARST == 1 & SPLOC > 0]
  a[, couple_head := fifelse(married_present, pmin(PERNUM, SPLOC), NA_integer_)]
  a[, parent_target := fifelse(MOMLOC > 0, MOMLOC, fifelse(POPLOC > 0, POPLOC, NA_integer_))]
  a[, is_child := AGE < 19 & !married_present & !is.na(parent_target)]
  a[, self_head := fifelse(married_present, couple_head, PERNUM)]

  # map a child to its parent's self-head; else self
  heads <- a[, .(hh_id, PERNUM, self_head)]
  a <- merge(a, heads[, .(hh_id, parent_target = PERNUM, parent_head = self_head)],
             by = c("hh_id","parent_target"), all.x = TRUE, sort = FALSE)
  a[, head_pernum := fifelse(is_child & !is.na(parent_head), parent_head, self_head)]
  a[, tu_id := paste(hh_id, head_pernum, sep = "-")]
  a[, role := fifelse(head_pernum == PERNUM, "head",
              fifelse(married_present & SPLOC == head_pernum, "spouse", "dependent"))]

  # --- unit-level: income, filing status, filer flag ------------------------
  units <- a[, .(
    state       = first(state),
    weight      = PERWT[role == "head"][1],            # head's person weight = unit weight
    head_age    = AGE[role == "head"][1],
    has_spouse  = any(role == "spouse"),
    n_dep       = sum(role == "dependent"),
    gross_inc   = sum(pmax(INCTOT, 0), na.rm = TRUE)   # unit gross income (loss-floored)
  ), by = tu_id]

  # Drop malformed units (no resolvable head row -> NA weight/age; ~331 of
  # 198M units in the 2022 extract, from edge cases in the pointer logic)
  n_bad <- units[is.na(weight) | is.na(head_age), .N]
  if (n_bad > 0) {
    message("  dropping ", n_bad, " units with unresolved head (NA weight/age)")
    units <- units[!is.na(weight) & !is.na(head_age)]
  }

  units[, filing_status := fifelse(has_spouse, "joint",
                           fifelse(n_dep > 0, "hoh", "single"))]
  thr <- filing_threshold(year)
  units[, threshold := thr[filing_status]]
  units[, is_filer := gross_inc >= threshold]

  units[, `:=`(age_band = age_band(head_age), income_tier = income_tier(gross_inc))]

  nonfiler_margins <- units[is_filer == FALSE,
                            .(n_units = sum(weight)),
                            by = .(state, age_band, income_tier)][order(state, age_band, income_tier)]
  filer_units <- units[is_filer == TRUE,
                       .(n_units = sum(weight)), by = state][order(state)]
  state_pop <- a[, .(pop = sum(PERWT)), by = state][order(state)]

  list(nonfiler_margins = nonfiler_margins[, year := year],
       filer_units      = filer_units[, year := year],
       state_pop        = state_pop[, year := year],
       unit_summary     = units[, .(n_units = sum(weight, na.rm = TRUE),
                                     n_filers = sum(weight * is_filer, na.rm = TRUE),
                                     n_nonfilers = sum(weight * !is_filer, na.rm = TRUE),
                                     n_units_na_wt = sum(is.na(weight)))],
       acs_year         = acs_year)
}

# -----------------------------------------------------------------------------
# HT2 filing-status identities: convert return counts into counts of PEOPLE on
# filed returns, by state. One definition per computation -- the reconciliation
# diagnostic below, the residual non-filer anchors (nonfiler_residual_design.md
# §3.1), and any external consumer all call this.
#
# Identities and documented approximations:
#   married filing adults = 2 * MARS2 + (N1 - MARS1 - MARS2 - MARS4)
#                           [residual is MFS-dominated; QSS (~1M unmarried
#                            returns) is misclassified as married]
#   single filing adults  = MARS1 + MARS4
#   dependents            = N2 - (N1 + MARS2)
#                           [includes adult dependents; a dependent who files
#                            their own return is double-counted (own return +
#                            parent's N2); N2 = exemptions pre-2018]
# PR/OA are excluded to match ACS/PEP coverage.
# -----------------------------------------------------------------------------
ht2_filing_persons <- function(ht2) {
  irs <- dcast(as.data.table(ht2)[!(state %in% NONTAX_BUCKETS) &
                                  variable %in% c("n_returns","n_single","n_joint","n_hoh","n_indiv"),
                                  .(value = sum(value)), by = .(state, variable)],
               state ~ variable, value.var = "value")
  irs[, .(state,
          married_filing_adults = 2 * n_joint + (n_returns - n_single - n_joint - n_hoh),
          single_filing_adults  = n_single + n_hoh,
          dependents            = n_indiv - (n_returns + n_joint))]
}

# -----------------------------------------------------------------------------
# Individual-level IRS vs ACS comparison (reconciliation diagnostic).
#
# Compares weighted counts of PEOPLE -- single adults, married adults, and
# children/dependents -- between HT2 (who appears on filed returns) and the
# ACS (everyone), by state. Unlike the filing-unit comparison, this does not
# depend on the v0 ACS tax-unit/filing model at all: the ACS side is a direct
# person-level tabulation, and the difference ACS - IRS estimates the
# non-filer population by group and state.
#
# Construction and documented approximations:
#   IRS side: ht2_filing_persons() above (identities + caveats documented there)
#   ACS (person-level PERWT sums):
#     married adults = AGE >= 18 & MARST in (1,2)   [separated -> single,
#                      matching filing rules]
#     single adults  = AGE >= 18 & !married
#     children       = AGE < 18   [vs IRS dependents, which include 18+
#                      students/relatives -- expect ACS < IRS here for that
#                      reason and the reverse from never-claimed children]
# -----------------------------------------------------------------------------
compare_individuals_acs_irs <- function(ht2, acs) {

  irs <- ht2_filing_persons(ht2)
  setnames(irs, c("married_filing_adults","single_filing_adults","dependents"),
                c("irs_married_adults","irs_single_adults","irs_children"))

  a <- as.data.table(acs)
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]
  acs_tab <- a[, .(
    acs_married_adults = sum(PERWT * (AGE >= 18 & MARST %in% 1:2)),
    acs_single_adults  = sum(PERWT * (AGE >= 18 & !(MARST %in% 1:2))),
    acs_children       = sum(PERWT * (AGE < 18))
  ), by = state]

  out <- merge(irs, acs_tab, by = "state")
  for (g in c("married_adults", "single_adults", "children")) {
    out[[paste0("nonfiler_", g)]] <- out[[paste0("acs_", g)]] - out[[paste0("irs_", g)]]
    out[[paste0("irs_share_", g)]] <- out[[paste0("irs_", g)]] / out[[paste0("acs_", g)]]
  }
  out[order(state)]
}

# -----------------------------------------------------------------------------
# Wage-based IRS vs ACS/QWI comparison (reconciliation diagnostic, JI
# 2026-07-13): wage earners and total wage dollars by state, IRS records vs
# two independent sources.
#
# DOLLARS are the clean comparison: HT2 A00200 (wages on filed returns) vs
# ACS INCWAGE person sums vs QWI total payroll. COUNTS carry documented
# concept gaps: HT2 N00200 counts RETURNS with wages (a two-earner joint
# return is one), ACS counts PERSONS with wage income, QWI counts JOBS
# (multi-jobholders duplicated).
#
# Source caveats:
#   ACS: INCWAGE reference period is the 12 months before interview, so the
#        2022 1-year sample spans calendar 2021-2022 income; residence-based.
#   QWI: UI-covered employment only (excludes most self-employment, some
#        federal/agricultural work) and WORKPLACE-based -- state comparisons
#        against residence-based IRS/ACS diverge sharply in commuter
#        geographies (DC especially). Requires CENSUS_API_KEY (free:
#        https://api.census.gov/data/key_signup.html).
# -----------------------------------------------------------------------------
compare_wages_acs_irs <- function(ht2, acs) {

  irs <- dcast(as.data.table(ht2)[!(state %in% NONTAX_BUCKETS) &
                                  variable %in% c("n_wages","wages_amt"),
                                  .(value = sum(value)), by = .(state, variable)],
               state ~ variable, value.var = "value")

  a <- as.data.table(acs)
  stopifnot("INCWAGE" %in% names(a))
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]
  a[INCWAGE >= 999998, INCWAGE := NA_real_]          # IPUMS N/A / missing codes
  acs_tab <- a[, .(
    acs_wage_earners = sum(PERWT * (!is.na(INCWAGE) & INCWAGE > 0)),
    acs_wages        = sum(PERWT * fifelse(is.na(INCWAGE), 0, INCWAGE))
  ), by = state]

  out <- merge(irs, acs_tab, by = "state")
  out[, `:=`(irs_share_earners = n_wages / acs_wage_earners,
             irs_share_wages   = wages_amt / acs_wages)]
  setnames(out, c("n_wages","wages_amt"), c("irs_returns_wages","irs_wages"))
  out[order(state)]
}

#' State-level annual employment and payroll from Census QWI (LEHD sex-age
#' endpoint). API facts learned empirically (2026-07-13): the 'for' clause
#' does not accept state:* (loop states); the Payroll variable is NULL at
#' state tabulations, so annual payroll is proxied as
#'   Σ_quarters 3 * EmpS * EarnS
#' (full-quarter stable jobs x average monthly earnings x 3 months) --
#' understates total payroll by unstable-job earnings; document when
#' comparing levels. Jobs are not persons (multi-jobholders duplicated) and
#' geography is WORKPLACE-based.
#'
#' The sex x age cross IS fully published: pass sex ('1','2') and agegrp
#' ('A01'..'A08', WIA bands) vectors to pull demographic cells -- the basis
#' for candidate person-level wage/employment targets matched to the PUF's
#' individual earners (wages1/2 x male1/2 x age1/2).
fetch_qwi <- function(year, sex = "0", agegrp = "A00",
                      states = names(FIPS_TO_STATE),
                      key = Sys.getenv("CENSUS_API_KEY")) {

  if (!nzchar(key)) {
    stop("fetch_qwi(): set CENSUS_API_KEY ",
         "(free signup: https://api.census.gov/data/key_signup.html)")
  }
  grid <- CJ(fips = states, q = 1:4, sx = sex, ag = agegrp)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i]
    url <- sprintf(paste0("https://api.census.gov/data/timeseries/qwi/sa",
                          "?get=Emp,EmpS,EarnS&for=state:%s&time=%d-Q%d",
                          "&sex=%s&agegrp=%s&key=%s"),
                   g$fips, year, g$q, g$sx, g$ag, key)
    j <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (is.null(j) || nrow(j) < 2) return(NULL)
    dt <- as.data.table(j[-1, , drop = FALSE])
    setnames(dt, as.character(j[1, ]))
    dt[, quarter := g$q]
    dt
  })
  out <- rbindlist(rows, fill = TRUE)
  out[, c("Emp","EmpS","EarnS") := lapply(.SD, as.numeric),
      .SDcols = c("Emp","EmpS","EarnS")]
  out[, .(qwi_jobs_avg      = mean(Emp,  na.rm = TRUE),
          qwi_stable_avg    = mean(EmpS, na.rm = TRUE),
          qwi_payroll_proxy = sum(3 * EmpS * EarnS, na.rm = TRUE)),
      by = .(statefips = state, sex, agegrp)][
    , state := FIPS_TO_STATE[as.character(as.integer(statefips))]][
    !is.na(state)][order(state, sex, agegrp)]
}

# -----------------------------------------------------------------------------
# LODES (LEHD Origin-Destination Employment Statistics): the residence-basis
# answer to QWI's workplace-basis problem (JI 2026-07-13). Same underlying
# LEHD job frame as QWI, published both ways:
#   RAC (Residence Area Characteristics): jobs by HOME census block --
#     aggregated to state, this is residence-based employment directly,
#     with age bands (CA01 <=29 / CA02 30-54 / CA03 55+) and monthly
#     earnings bands (CE01 <=$1,250 / CE02 $1,251-3,333 / CE03 >$3,333).
#   OD (main = in-state residents, aux = out-of-state residents): home x
#     work block flows -- aggregated, the state-to-state commuter matrix
#     that converts workplace payroll to residence basis. Verified 2022:
#     only 31.0% of DC-workplace primary jobs are held by DC residents
#     (MD 39.0%, VA 25.9%) -- the entire QWI DC/MD/VA anomaly, quantified.
#
# Concept notes: JT01 = primary jobs (one per worker, closest to persons);
# point-in-time job counts sit below ACS/IRS any-wage-in-year earner counts
# by construction. Same UI-covered universe as QWI. LODES8 covers through
# 2022 on 2020 blocks. Block geocodes must be read as CHARACTER (15 digits;
# integer64 breaks substr-based state extraction).
# -----------------------------------------------------------------------------
LODES_BASE <- "https://lehd.ces.census.gov/data/lodes/LODES8"

#' Residence-based state employment from LODES RAC, with age/earnings bands.
fetch_lodes_rac <- function(year, states = tolower(STATE_JURISDICTIONS),
                            jt = "JT01") {
  rbindlist(lapply(states, function(st) {
    d <- tryCatch(
      fread(sprintf("%s/%s/rac/%s_rac_S000_%s_%d.csv.gz",
                    LODES_BASE, tolower(st), tolower(st), jt, year)),
      error = function(e) NULL)
    if (is.null(d)) {
      message("  LODES RAC unavailable: ", st, " ", year)
      return(NULL)
    }
    data.table(state = toupper(st),
               workers_res  = sum(d$C000),
               age_u30      = sum(d$CA01), age_30_54 = sum(d$CA02),
               age_55p      = sum(d$CA03),
               earn_low     = sum(d$CE01), earn_mid  = sum(d$CE02),
               earn_high    = sum(d$CE03))
  }))
}

#' State-to-state commuter matrix from LODES OD (jobs by work state x
#' residence state). Downloads main + aux per WORK state requested.
fetch_lodes_od_matrix <- function(year, work_states, jt = "JT01") {
  rbindlist(lapply(work_states, function(st) {
    od <- rbindlist(lapply(c("main", "aux"), function(part) {
      tryCatch(
        fread(sprintf("%s/%s/od/%s_od_%s_%s_%d.csv.gz",
                      LODES_BASE, tolower(st), tolower(st), part, jt, year),
              colClasses = list(character = c("w_geocode", "h_geocode")))[
          , .(h_geocode, S000)],
        error = function(e) NULL)
    }))
    if (nrow(od) == 0) return(NULL)
    od[, res_state := FIPS_TO_STATE[as.character(as.integer(substr(h_geocode, 1, 2)))]]
    od[, .(jobs = sum(S000)), by = res_state][
      , `:=`(work_state = toupper(st), share = jobs / sum(jobs))][]
  }))
}

# =============================================================================
# PUF-SIDE TARGET ASSEMBLY (plan §2.1)
#
# Maps PUF records onto the ingested targets and builds the (w, P0, targets)
# inputs both engines consume, per partition:
#   filers     -> HT2 state x AGI-stub cells, one target per (state, stub,
#                 series) with shared row/x vectors per (stub, series)
#   non-filers -> ACS state x age-band x income-tier cells
#
# NORMALIZATION (key design point): split weights control only the GEOGRAPHIC
# distribution -- national totals are fixed by construction. Raw HT2/ACS
# levels differ from PUF nationals (coverage, vintage), which would make
# level targets infeasible. So each target is the PUF's own national total
# for that (cell, series), distributed across states by the HT2/ACS SHARES:
#   target(st, cell, v) = PUF_total(cell, v) * share_HT2(st | cell, v).
# Diagnostics against raw HT2 levels remain a separate validation step.
#
# v1 documented approximations:
#   - capital gains in AGI proxied by kg_st + kg_lt with the $3,000 loss cap
#   - SALT income+sales targeted combined (PUF salt_inc_sales is the greater-
#     of election; HT2 splits A18425/A18450 -- elections are mutually
#     exclusive per return, so counts/amounts add)
#   - stub-1 (negative-AGI) amount targets are skipped by the calibration
#     engine's positivity guard; count targets still bind there
#   - non-filer gross income proxy mirrors the ACS INCTOT unit construction
# =============================================================================

# PUF x-vector for one HT2 target series (NULL if inputs absent)
puf_series_x <- function(tu, series) {
  has <- function(...) all(c(...) %in% names(tu))
  kg_agi <- function() {
    kg <- tu$kg_st + tu$kg_lt
    fifelse(kg < 0, pmax(kg, -3000), kg)
  }
  switch(series,
    n_returns  = rep(1, nrow(tu)),
    n_single   = as.numeric(tu$filing_status == 1),
    n_joint    = as.numeric(tu$filing_status == 2),
    n_hoh      = as.numeric(tu$filing_status == 4),
    n_indiv    = 1 + (tu$filing_status == 2) + tu$n_dep,
    agi_amt    = if (has("agi")) tu$agi,
    n_wages    = if (has("wages")) as.numeric(tu$wages != 0),
    wages_amt  = if (has("wages")) tu$wages,
    n_int      = if (has("txbl_int")) as.numeric(tu$txbl_int != 0),
    int_amt    = if (has("txbl_int")) tu$txbl_int,
    n_div      = if (has("div_ord","div_pref")) as.numeric(tu$div_ord + tu$div_pref != 0),
    div_amt    = if (has("div_ord","div_pref")) tu$div_ord + tu$div_pref,
    n_kg       = if (has("kg_st","kg_lt")) as.numeric(kg_agi() != 0),
    kg_amt     = if (has("kg_st","kg_lt")) kg_agi(),
    n_salt     = if (has("salt_inc_sales")) as.numeric(tu$salt_inc_sales != 0),
    salt_amt   = if (has("salt_inc_sales")) tu$salt_inc_sales,
    n_re_tax   = if (has("salt_prop")) as.numeric(tu$salt_prop != 0),
    re_tax_amt = if (has("salt_prop")) tu$salt_prop,
    n_mort_int = if (has("first_mort_int","second_mort_int"))
                   as.numeric(tu$first_mort_int + tu$second_mort_int != 0),
    mort_int_amt = if (has("first_mort_int","second_mort_int"))
                   tu$first_mort_int + tu$second_mort_int,
    n_eitc     = if (has("eitc")) as.numeric(tu$eitc > 0),
    eitc_amt   = if (has("eitc")) tu$eitc,
    NULL)
}

# HT2 series consumed per PUF series (salt combines income + sales elections)
HT2_SERIES_FOR <- list(
  n_returns = "n_returns", n_single = "n_single", n_joint = "n_joint",
  n_hoh = "n_hoh", n_indiv = "n_indiv",
  agi_amt = "agi_amt", n_wages = "n_wages", wages_amt = "wages_amt",
  n_int = "n_int", int_amt = "int_amt", n_div = "n_div", div_amt = "div_amt",
  n_kg = "n_kg", kg_amt = "kg_amt",
  n_salt = c("n_salt_inc","n_salt_sales"), salt_amt = c("salt_inc_amt","salt_sales_amt"),
  n_re_tax = "n_re_tax", re_tax_amt = "re_tax_amt",
  n_mort_int = "n_mort_int", mort_int_amt = "mort_int_amt",
  n_eitc = "n_eitc", eitc_amt = "eitc_amt")

# Assign each record to its HT2 AGI stub (lower <= agi < upper)
assign_ht2_stub <- function(agi, year) {
  breaks <- ht2_stub_breaks(year)
  findInterval(agi, c(breaks$lower, Inf), rightmost.closed = FALSE)
}

# Gross-income proxy for non-filer cell assignment, mirroring the ACS INCTOT
# unit construction (sum of positive incomes)
puf_gross_income <- function(tu) {
  pmax(tu$wages, 0) + pmax(tu$txbl_int, 0) + pmax(tu$exempt_int, 0) +
    pmax(tu$div_ord + tu$div_pref, 0) + pmax(tu$kg_st + tu$kg_lt, 0) +
    pmax(tu$txbl_pens_dist, 0) + pmax(tu$txbl_ira_dist, 0) +
    pmax(tu$gross_ss, 0) + pmax(tu$sole_prop, 0)
}

#' Assemble engine inputs for one year: partitions, priors, and targets.
#' @param tax_units PUF records incl. baseline-calculated `agi` and `eitc`
#'        (join from baseline detail when running standalone)
#' @param ht2 long target table from read_ht2(); read from the store if NULL
#' @param acs_margins list from build_acs_margins(); built if NULL (slow)
#' @return list(jurisdictions, filers = list(idx, w, P0, targets),
#'              nonfilers = list(idx, w, P0, targets))
build_weight_inputs <- function(tax_units, year, ht2 = NULL, acs_margins = NULL,
                                verbose = TRUE) {

  tu <- as.data.table(tax_units)
  if (is.null(ht2)) ht2 <- read_ht2(ht2_path(year), year)
  if (is.null(acs_margins)) {
    acs_margins <- build_acs_margins(read_acs_extract(min(year, 2022)), year)
  }

  # Jurisdiction order defines P0/engine state indices: HT2 areas as published
  # (51 + OA, + PR from 2018), sorted for determinism
  jurisdictions <- sort(unique(ht2$state))
  S <- length(jurisdictions)
  st_idx <- setNames(seq_len(S), jurisdictions)

  #---------------------------
  # Filer partition (HT2)
  #---------------------------

  idx_f <- which(tu$filer == 1)
  tu_f  <- tu[idx_f]
  stub  <- assign_ht2_stub(tu_f$agi, year)

  # Prior: HT2 return shares within stub
  ht2_ret <- dcast(ht2[variable == "n_returns"], agi_stub ~ state, value.var = "value")
  P0_f <- matrix(0, nrow(tu_f), S)
  for (s10 in sort(unique(stub))) {
    shares <- as.numeric(ht2_ret[agi_stub == s10, ..jurisdictions])
    shares <- pmax(shares, 0); shares <- shares / sum(shares)
    P0_f[stub == s10, ] <- matrix(shares, sum(stub == s10), S, byrow = TRUE)
  }

  # Targets: PUF national totals per (stub, series), distributed by HT2 shares.
  # kg_amt is excluded: capital-gains x-vectors are sign-mixed within stubs
  # and several (stub, state) HT2 cells are net-negative -- negative targets
  # flip multiplicative-IPF cells negative and poison entire state columns
  # (root-caused 2026-07-13; n_kg still binds; the gradient engine can
  # reclaim kg_amt later since it has no positivity constraint)
  targets_f <- list()
  skipped <- character(0)
  n_dropped_nonpos <- 0
  for (series in setdiff(names(HT2_SERIES_FOR), "kg_amt")) {
    x_all <- puf_series_x(tu_f, series)
    if (is.null(x_all)) { skipped <- c(skipped, series); next }
    ht2_v <- ht2[variable %in% HT2_SERIES_FOR[[series]],
                 .(value = sum(value)), by = .(state, agi_stub)]
    for (s10 in sort(unique(stub))) {
      rows  <- which(stub == s10)
      x     <- x_all[rows]
      puf_total <- sum(tu_f$weight[rows] * x)
      cell  <- ht2_v[agi_stub == s10]
      denom <- sum(cell$value)
      if (denom <= 0 || puf_total == 0) next
      for (k in seq_len(nrow(cell))) {
        tgt <- puf_total * cell$value[k] / denom
        if (!is.finite(tgt)) next
        # Non-positive targets (negative HT2 cells, e.g. agi_amt in stub 1)
        # cannot be calibrated multiplicatively; drop and count
        if (tgt <= 0) { n_dropped_nonpos <- n_dropped_nonpos + 1; next }
        targets_f[[length(targets_f) + 1]] <- list(
          rows = rows, state = st_idx[[cell$state[k]]], x = x,
          target = tgt, lambda = 1,
          # metadata (ignored by the engines; used by diagnostics)
          series = series, stub = s10, state_code = cell$state[k])
      }
    }
  }
  if (verbose) {
    message(sprintf("  filer targets: %d (%d records; %d non-positive dropped%s)",
                    length(targets_f), nrow(tu_f), n_dropped_nonpos,
                    if (length(skipped)) paste0("; series without inputs: ",
                                                paste(skipped, collapse = " ")) else ""))
  }

  #---------------------------
  # Non-filer partition (ACS)
  #---------------------------

  idx_n <- which(tu$filer == 0)
  tu_n  <- tu[idx_n]
  nm    <- as.data.table(acs_margins$nonfiler_margins)
  cell_n <- paste(age_band(tu_n$age1), income_tier(puf_gross_income(tu_n)), sep = "|")
  nm[, cell := paste(age_band, income_tier, sep = "|")]

  # Prior: ACS state shares within cell; fallback to overall non-filer shares
  overall <- nm[, .(n = sum(n_units)), by = state]
  overall_shares <- setNames(rep(0, S), jurisdictions)
  overall_shares[overall$state] <- overall$n / sum(overall$n)

  P0_n <- matrix(0, nrow(tu_n), S)
  targets_n <- list()
  for (cl in sort(unique(cell_n))) {
    rows <- which(cell_n == cl)
    m_cl <- nm[cell == cl]
    if (nrow(m_cl) == 0) {
      P0_n[rows, ] <- matrix(overall_shares, length(rows), S, byrow = TRUE)
      next
    }
    shares <- setNames(rep(0, S), jurisdictions)
    shares[m_cl$state] <- m_cl$n_units / sum(m_cl$n_units)
    P0_n[rows, ] <- matrix(shares, length(rows), S, byrow = TRUE)

    puf_total <- sum(tu_n$weight[rows])
    x <- rep(1, length(rows))
    for (k in seq_len(nrow(m_cl))) {
      tgt <- puf_total * m_cl$n_units[k] / sum(m_cl$n_units)
      if (tgt <= 0) next
      targets_n[[length(targets_n) + 1]] <- list(
        rows = rows, state = st_idx[[m_cl$state[k]]], x = x,
        target = tgt, lambda = 1,
        series = "n_units", cell = cl, state_code = m_cl$state[k])
    }
  }
  if (verbose) {
    message(sprintf("  non-filer targets: %d (%d records, %d cells)",
                    length(targets_n), nrow(tu_n), length(unique(cell_n))))
  }

  list(jurisdictions = jurisdictions,
       filers    = list(idx = idx_f, w = tu_f$weight, P0 = P0_f, targets = targets_f),
       nonfilers = list(idx = idx_n, w = tu_n$weight, P0 = P0_n, targets = targets_n))
}

#' Fit both partitions with the chosen engine and assemble long split weights.
#' @return data.table(id, state, weight) with Σ_state weight = national weight
build_split_weights <- function(tax_units, year,
                                method = c("calibration", "gradient"),
                                inputs = NULL, ...) {
  method <- match.arg(method)
  tu <- as.data.table(tax_units)
  if (is.null(inputs)) inputs <- build_weight_inputs(tu, year)

  fit_fn <- if (method == "gradient") fit_gradient else fit_calibration
  out <- list()
  for (part in c("filers", "nonfilers")) {
    p <- inputs[[part]]
    if (length(p$idx) == 0) next
    fit <- fit_fn(p$w, p$P0, p$targets, ...)

    # The split constraint must hold EXACTLY here -- a violation is a defect
    # (the 2026-07-13 leak came from negative P cells being dropped by the
    # weight filter below; negative targets are now blocked at assembly)
    stopifnot(all(fit$P >= 0), max(abs(rowSums(fit$P) - 1)) < 1e-8)

    W <- p$w * fit$P
    dt <- data.table(id = rep(tu$id[p$idx], ncol(W)),
                     state = rep(inputs$jurisdictions, each = nrow(W)),
                     weight = as.vector(W))
    out[[part]] <- dt[weight > 0]
  }
  rbindlist(out)
}

# =============================================================================
# WEIGHTING ENGINES
#
# Both operate on a common problem: N records (a filer or non-filer partition),
# S states, national weights w (length N), a prior share matrix P0 (N×S, rows sum
# to 1), and a target list. They return a share matrix P (N×S) with W = w * P.
#
# A target is list(rows = integer idx into 1..N, state = s in 1..S, x = numeric
# length-|rows| record values, target = scalar, lambda = weight). The predicted
# total is  sum_{i in rows} w[i] * P[i, s] * x[i].  (x ≡ 1 for count targets,
# x = agi for amount targets.)  The shared invariant Σ_s P[i,s] = 1 holds by
# construction in both engines, so Σ_s W[i,s] = w[i] always.
# =============================================================================

# --- Approach B: differentiable reweighting (matrix analytic-gradient) --------
# torch is not installed on the cluster; this is a dependency-free implementation
# of the identical softmax objective (state_weights_ml_alternative.md §2). Logits
# theta (N×S); P = softmax(theta); loss = Σ_t lambda_t ((That_t-T_t)/T_t)^2 +
# beta * Σ_i KL(P[i,]||P0[i,]). Adam on theta.
#
# VECTORIZED (2026-07-13): targets sharing a rows set (all series within a
# stub, by assembly construction; identified via the stub/cell metadata) are
# stacked into one group -- X (n_g × K) holds the w·x columns, T/L (K × S)
# the targets/lambdas -- so each Adam step does ~10 GEMMs instead of ~10,700
# per-target scatter updates:
#   predicted totals (all series × states):  crossprod(X, P[rows, ])
#   gradient accumulation:                    G[rows, ] += X %*% C
# Targets without metadata become singleton groups (still exact; used by the
# self-test). Verified against finite differences in .state_weights_selftest().
fit_gradient <- function(w, P0, targets, beta = 1e-3,
                         lr = 0.1, n_steps = 500, verbose = FALSE,
                         lr_schedule = c("constant", "cosine"),
                         theta0 = NULL) {
  # lr_schedule: "cosine" anneals lr from lr to ~0 over n_steps (half-cosine);
  # theta0: optional warm-start logits (e.g. a prior run's theta). The KL
  # anchor stays P0 regardless -- warm-starting changes the path, not the
  # objective.
  lr_schedule <- match.arg(lr_schedule)
  N <- length(w); S <- ncol(P0)

  # ---- group targets by shared rows (stub / cell metadata; else singleton)
  key <- vapply(seq_along(targets), function(i) {
    t <- targets[[i]]
    if (!is.null(t$stub)) paste0("stub_", t$stub)
    else if (!is.null(t$cell)) paste0("cell_", t$cell)
    else paste0("tgt_", i)
  }, character(1))
  groups <- lapply(split(seq_along(targets), key), function(idx) {
    rows <- targets[[idx[1]]]$rows
    # one X column per distinct series in the group; map each target to its
    # column and its state
    ser <- vapply(idx, function(i) {
      t <- targets[[i]]
      if (is.null(t$series)) paste0("x", i) else t$series
    }, character(1))
    useries <- unique(ser)
    X <- matrix(0, length(rows), length(useries))
    Tm <- matrix(NA_real_, length(useries), S)
    Lm <- matrix(0, length(useries), S)
    for (j in seq_along(idx)) {
      t <- targets[[idx[j]]]
      stopifnot(identical(t$rows, rows))       # grouping precondition
      k <- match(ser[j], useries)
      X[, k] <- w[rows] * t$x
      Tm[k, t$state] <- t$target
      Lm[k, t$state] <- t$lambda
    }
    list(rows = rows, X = X, Tm = Tm, Lm = Lm, has = !is.na(Tm))
  })

  theta <- if (is.null(theta0)) log(pmax(P0, 1e-12)) else theta0  # warm start at the prior
  m <- matrix(0, N, S); v <- matrix(0, N, S)             # Adam moments
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  softmax_rows <- function(z) { z <- z - apply(z, 1, max); e <- exp(z); e / rowSums(e) }

  loss_hist <- numeric(n_steps)
  for (step in seq_len(n_steps)) {
    P <- softmax_rows(theta)
    G <- matrix(0, N, S)                                 # dLoss_fit / dP
    loss_fit <- 0
    for (g in groups) {
      That  <- crossprod(g$X, P[g$rows, , drop = FALSE]) # K × S predicted totals
      resid <- (That - g$Tm) / g$Tm                      # NA where untargeted
      loss_fit <- loss_fit + sum(g$Lm * resid^2, na.rm = TRUE)
      C <- ifelse(g$has, 2 * g$Lm * resid / g$Tm, 0)     # K × S coefficient
      G[g$rows, ] <- G[g$rows, ] + g$X %*% C
    }
    # KL(P||P0) gradient wrt P, plus softmax backprop:  gtheta = P*(g - rowSums(P*g))
    Greg <- beta * (log(pmax(P, 1e-12)) - log(pmax(P0, 1e-12)) + 1)
    g <- G + Greg
    gtheta <- P * (g - rowSums(P * g))
    # Adam
    m <- b1 * m + (1 - b1) * gtheta
    v <- b2 * v + (1 - b2) * gtheta^2
    mhat <- m / (1 - b1^step); vhat <- v / (1 - b2^step)
    lr_t <- if (lr_schedule == "cosine") lr * 0.5 * (1 + cos(pi * step / n_steps)) else lr
    theta <- theta - lr_t * mhat / (sqrt(vhat) + eps)
    loss_kl <- beta * sum(P * (log(pmax(P,1e-12)) - log(pmax(P0,1e-12))))
    loss_hist[step] <- loss_fit + loss_kl
    if (verbose && step %% max(1, n_steps %/% 10) == 0)
      message(sprintf("  step %d  loss_fit=%.4e  loss_kl=%.4e", step, loss_fit, loss_kl))
  }
  list(P = softmax_rows(theta), loss_hist = loss_hist, theta = theta)
}

# --- Approach A: classical raking / IPF ---------------------------------------
# Seeded at the prior P0, iteratively rescales each (target) so predicted totals
# approach targets, then renormalizes rows to restore Σ_s P[i,s] = 1. Count and
# amount targets are handled by the generic x-vector; targets and x must be
# non-negative (enforced at assembly). Hardened 2026-07-13 after root-causing
# the first full-scale fit:
#   - DAMPED: the per-pass factor is clamped to [1/f_max, f_max] so no single
#     extreme cell (OA/PR wages showed raw factors up to ~530) can distort the
#     shared rows for every other target in one pass; convergence is
#     geometric instead of cyclic
#   - denominator floor: targets whose predicted total is vanishingly small
#     relative to the target are skipped that pass and reported as unfittable
#     if still starved at exit (never silently no-opped)
fit_calibration <- function(w, P0, targets, n_iter = 200, tol = 1e-4,
                            f_max = 2, floor_rel = 1e-9, verbose = FALSE) {
  P <- P0
  for (iter in seq_len(n_iter)) {
    maxrel <- 0
    for (t in targets) {
      That <- sum(w[t$rows] * P[t$rows, t$state] * t$x)
      if (That > floor_rel * t$target) {
        f <- min(max(t$target / That, 1 / f_max), f_max)
        P[t$rows, t$state] <- P[t$rows, t$state] * f      # damped rescale
        maxrel <- max(maxrel, abs(t$target / That - 1))
      }
    }
    rs <- rowSums(P); P <- P / rs                          # restore row-sum = 1
    if (verbose && iter %% 10 == 0) message(sprintf("  iter %d  max|rel err|=%.4f", iter, maxrel))
    if (maxrel < tol) break
  }
  # Report targets still starved at exit
  unfittable <- which(vapply(targets, function(t) {
    sum(w[t$rows] * P[t$rows, t$state] * t$x) <= floor_rel * t$target
  }, logical(1)))
  if (verbose && length(unfittable) > 0) {
    message("  unfittable targets at exit: ", length(unfittable))
  }
  list(P = P, iters = iter, maxrel = maxrel, unfittable = unfittable)
}

# --- self-test: gradient correctness (finite differences) + both engines ------
# Runs a tiny synthetic problem; not part of the production path.
.state_weights_selftest <- function() {
  set.seed(1)
  N <- 6; S <- 3
  w  <- c(10, 20, 5, 8, 12, 15)
  P0 <- matrix(1/S, N, S)
  # two count targets: state 1 total = 25, state 2 total = 30 (of total 70)
  targets <- list(
    list(rows = 1:N, state = 1L, x = rep(1, N), target = 25, lambda = 1),
    list(rows = 1:N, state = 2L, x = rep(1, N), target = 30, lambda = 1))
  # finite-difference check of the analytic gradient at theta0
  loss_at <- function(theta) {
    z <- theta - apply(theta,1,max); e <- exp(z); P <- e/rowSums(e)
    lf <- 0
    for (t in targets) { That <- sum(w[t$rows]*P[t$rows,t$state]*t$x); lf <- lf + t$lambda*((That-t$target)/t$target)^2 }
    lf
  }
  theta0 <- matrix(0, N, S)
  P <- matrix(1/S, N, S)
  G <- matrix(0, N, S)
  for (t in targets) { That <- sum(w[t$rows]*P[t$rows,t$state]*t$x)
    G[t$rows,t$state] <- G[t$rows,t$state] + t$lambda*2*((That-t$target)/t$target)/t$target*w[t$rows]*t$x }
  gtheta <- P*(G - rowSums(P*G))
  num <- matrix(0, N, S); h <- 1e-6
  for (i in 1:N) for (s in 1:S) { d <- matrix(0,N,S); d[i,s] <- h
    num[i,s] <- (loss_at(theta0+d) - loss_at(theta0-d))/(2*h) }
  max_grad_err <- max(abs(gtheta - num))

  rg <- fit_gradient(w, P0, targets, beta = 0, lr = 0.2, n_steps = 2000)
  rc <- fit_calibration(w, P0, targets, n_iter = 200)
  totals <- function(P) c(sum(w*P[,1]), sum(w*P[,2]), sum(w*P[,3]))
  cat("gradient vs finite-diff max err:", format(max_grad_err, digits=3), "\n")
  cat("row-sum invariant (grad):", all(abs(rowSums(rg$P)-1) < 1e-8),
      " (calib):", all(abs(rowSums(rc$P)-1) < 1e-8), "\n")
  cat("target = [25, 30, 15]\n")
  cat("gradient totals:   ", round(totals(rg$P),2), "\n")
  cat("calibration totals:", round(totals(rc$P),2), "\n")
  invisible(list(max_grad_err = max_grad_err, grad = totals(rg$P), calib = totals(rc$P)))
}



# -----------------------------------------------------------------------------
# build_state_weights(): the runtime dispatcher (plan §2.1). Returns long
# split weights (id, state, weight). The real methods satisfy
# Σ_state w_{i,state} = weight_i across ALL 53 jurisdictions; the placeholder
# does not (see below). Federal aggregates are invariant to the with-state mode
# regardless, because federal totals use the untouched `weight` column.
#
# Methods:
#   "placeholder" -- gives EACH requested jurisdiction 1/53 of the national
#                    weight (fixed denominator 53). Exists so the Phase 4
#                    orchestration can run before the Phase 1 bake-off lands.
#                    State LEVELS are meaningless (every state = 1/53 of the
#                    nation) AND the emitted rows do NOT sum to weight_i when a
#                    subset of states is requested: Σ over emitted states =
#                    (n_states / 53) * weight_i. Do not reconstruct a national
#                    total by summing state.csv under the placeholder. The file
#                    contract and downstream machinery are real.
#   "calibration" -- Approach A (fit_calibration); not yet wired to HT2/ACS
#                    target ingestion at runtime.
#   "gradient"    -- Approach B (fit_gradient); ditto.
# -----------------------------------------------------------------------------
build_state_weights = function(tax_units, year,
                               method = c('placeholder', 'calibration', 'gradient'),
                               states = NULL) {

  method = match.arg(method)
  jurisdictions = c(STATE_JURISDICTIONS, NONTAX_BUCKETS)
  if (is.null(states)) {
    states = jurisdictions
  }

  if (method != 'placeholder') {
    stop('build_state_weights(): method "', method, '" is not yet wired into ',
         'the runtime (Phase 1 bake-off pending); use "placeholder"')
  }

  tax_units %>%
    select(id, weight) %>%
    expand_grid(state = states) %>%
    mutate(weight = weight / length(jurisdictions)) %>%
    select(id, state, weight) %>%
    return()
}

# =============================================================================
# FILING MODEL: Mok probits, the Pub 5785 hazard, and their combination
#
# Group C stage C3 of the non-filer population build (plan of record:
# research/state_weights/plan.md §3; method: nonfiler_residual_design.md
# §3.2.2). Functions only; the driver is nonfiler_pool/02_filing_model.R.
#
# Architecture (design memo §3.2.2, decision D3):
#   BELOW the filing threshold  Mok (2017) CBO WP 2017-06 Table 14 -- fourteen
#                               group-specific probits estimated on the 2007
#                               ASEC linked to the IRS Individual Master File.
#                               Every regressor is CPS-native, which is what
#                               makes the transplant legitimate: the linkage
#                               bought the identification, scoring needs only
#                               survey variables.
#   ABOVE the threshold         an IRS Pub 5785 non-filing hazard: a national
#                               scalar (11.19M obligated non-filing units,
#                               TY2014-16 average) allocated by the
#                               publication's relative risks. The scalar is a
#                               STARTING level -- C4's joint calibration
#                               chooses it together with the group constants
#                               against the residual anchors.
#
# The name of this file anticipates F4 (research/state_weights/plan.md §5),
# which plans exactly this split so Affordability-Index can source the filing
# model without the weights machinery.
# =============================================================================


# -----------------------------------------------------------------------------
# Coefficients
# -----------------------------------------------------------------------------

MOK_COEFS_PATH <- "research/state_weights/nonfiler_residual/resources/mok_coefs.csv"

# Mok's fourteen groups: married x age-65 x {0,1,2+} dependents, plus
# dependent-headed units x age-65. Kept as the canonical ordering.
MOK_GROUPS <- c(
  "unmarried_u65_dep0", "unmarried_u65_dep1", "unmarried_u65_dep2p",
  "unmarried_65p_dep0", "unmarried_65p_dep1", "unmarried_65p_dep2p",
  "married_u65_dep0",   "married_u65_dep1",   "married_u65_dep2p",
  "married_65p_dep0",   "married_65p_dep1",   "married_65p_dep2p",
  "dependent_u65",      "dependent_65p")

MOK_TERMS <- c(
  "intercept", "log_gross_income", "negative_gross_income",
  "src_wages", "src_interest", "src_dividends", "src_self_employment",
  "src_rental", "src_retirement", "src_social_security",
  "means_tested_transfers", "n_medicaid",
  "educ_less_than_hs", "educ_college",
  "race_black", "race_hispanic", "race_other")

#' Read Mok's Table 14 into a groups x terms coefficient matrix.
#'
#' Three cells are empty in the transcription and PUBLISHED that way
#' (dependent_65p src_self_employment printed as '.'; the dependent panels
#' carry no retirement row) -- they enter the fitted equation as zero, which
#' is what "dropped from the model" means at scoring time.
#'
#' @return list(coefs = matrix [group x term], filing_rates = named vector of
#'         Mok's published weighted filing rates -- the C3 context gate)
read_mok_coefs <- function(path = MOK_COEFS_PATH) {
  raw <- fread(path)
  stopifnot(setequal(unique(raw$group_id), MOK_GROUPS),
            setequal(unique(raw$term), MOK_TERMS),
            nrow(raw) == length(MOK_GROUPS) * length(MOK_TERMS))

  raw[, coefficient := suppressWarnings(as.numeric(coefficient))]
  n_empty <- raw[is.na(coefficient), .N]
  stopifnot(n_empty == 3)   # exactly the three published gaps; more means a
                            # transcription regression, fewer means a silent fill
  raw[is.na(coefficient), coefficient := 0]

  coefs <- matrix(0, length(MOK_GROUPS), length(MOK_TERMS),
                  dimnames = list(MOK_GROUPS, MOK_TERMS))
  for (i in seq_len(nrow(raw)))
    coefs[raw$group_id[i], raw$term[i]] <- raw$coefficient[i]

  rates <- raw[, .(rate = unique(filing_rate_weighted)), by = group_id]
  stopifnot(nrow(rates) == length(MOK_GROUPS))
  list(coefs = coefs,
       filing_rates = setNames(rates$rate, rates$group_id))
}


# -----------------------------------------------------------------------------
# Covariates (Mok p.22-23, definitions verified against the paper 2026-08-28)
# -----------------------------------------------------------------------------

# EDUC recode cutpoints. The omitted education category is HS-to-some-college
# (settled from the coefficient signs, approved 2026-08-28 with JI's review of
# Table 14 carried as an open item): less-than-HS is everything below a
# diploma, college is bachelor's and above, and associate degrees fall in the
# omitted middle.
EDUC_HS_DIPLOMA_CODE <- 73L
EDUC_BACHELORS_CODE  <- 111L
HIMCAIDLY_YES        <- 2L

#' Attach Mok's seventeen covariates to the unit table.
#'
#' Household-level pieces (the Medicaid count, SNAP) come from the person
#' table; everything else derives from unit income already aggregated over
#' head + spouse. Definitions, from the paper:
#'   gross income        EXCLUDES Social Security (built upstream in C0)
#'   means-tested        "SNAP, housing assistance, TANF, or energy
#'                       assistance". The extract carries SNAP (FOODSTAMP) and
#'                       public assistance (INCWELFR); housing and energy
#'                       assistance are NOT pulled -- a RECORDED DEVIATION,
#'                       biased toward understating receipt, pending an IPUMS
#'                       availability check (site was down 2026-08-28).
#'   n_medicaid          household members covered by Medicaid last year
#'
#' @param units   from build_asec_tax_units()
#' @param persons its companion person table; must carry on_medicaid,
#'                foodstamp_amount and welfare_income (cleaned in the C0
#'                driver -- NIU handling never happens this far downstream)
add_mok_covariates <- function(units, persons) {
  stopifnot(all(c("on_medicaid", "foodstamp_amount", "welfare_income")
                %in% names(persons)),
            all(c("educ_head", "race_head", "hispan_head") %in% names(units)))

  hh <- persons[, .(n_medicaid   = sum(on_medicaid),
                    hh_foodstamp = any(foodstamp_amount > 0)),
                by = SERIAL]
  units <- merge(units, hh, by = "SERIAL", all.x = TRUE, sort = FALSE)

  # Welfare income of the unit's own members: head + spouse for nondependent
  # units; the dependent's own record for dependent-headed scoring units.
  welf_nondep <- persons[role %in% c("primary", "spouse"),
                         .(welfare_unit = sum(welfare_income)), by = unit_id]
  units <- merge(units, welf_nondep, by = "unit_id", all.x = TRUE, sort = FALSE)
  welf_dep <- persons[, .(SERIAL, head_pernum = PERNUM, welfare_own = welfare_income)]
  units <- merge(units, welf_dep, by = c("SERIAL", "head_pernum"),
                 all.x = TRUE, sort = FALSE)
  units[unit_type == "dependent", welfare_unit := welfare_own]
  units[, welfare_own := NULL]
  stopifnot(!anyNA(units$welfare_unit), !anyNA(units$n_medicaid))

  units[, `:=`(
    intercept             = 1,
    log_gross_income      = log(pmax(gross_income, 1)),
    negative_gross_income = as.numeric(gross_income < 0),
    src_wages             = as.numeric(INCWAGE > 0),
    src_interest          = as.numeric(INCINT != 0),
    src_dividends         = as.numeric(INCDIVID != 0),
    src_self_employment   = as.numeric(se_income != 0),
    src_rental            = as.numeric(INCRENT != 0),
    src_retirement        = as.numeric(retirement_income != 0),
    src_social_security   = as.numeric(INCSS > 0),
    means_tested_transfers = as.numeric(hh_foodstamp | welfare_unit > 0),
    educ_less_than_hs     = as.numeric(educ_head < EDUC_HS_DIPLOMA_CODE),
    educ_college          = as.numeric(educ_head >= EDUC_BACHELORS_CODE),
    race_hispanic         = as.numeric(hispan_head > 0 & hispan_head < 900)
  )]
  units[, race_black := as.numeric(race_hispanic == 0 & race_head == 200)]
  units[, race_other := as.numeric(race_hispanic == 0 &
                                   !race_head %in% c(100, 200))]
  units[]
}

#' Assign each unit to its Mok group.
assign_mok_group <- function(units) {
  units[, mok_group := fifelse(
    unit_type == "dependent",
    fifelse(age_head >= 65, "dependent_65p", "dependent_u65"),
    paste0(fifelse(filing_status == "joint", "married", "unmarried"),
           fifelse(age_head >= 65, "_65p", "_u65"),
           fcase(n_dep == 0, "_dep0", n_dep == 1, "_dep1", n_dep >= 2, "_dep2p")))]
  stopifnot(all(units$mok_group %in% MOK_GROUPS))
  units[]
}

#' Probit score: P(files) per unit under Mok's coefficients.
score_mok <- function(units, coefs) {
  X <- as.matrix(units[, ..MOK_TERMS])
  stopifnot(!anyNA(X))
  xb <- rowSums(X * coefs[units$mok_group, , drop = FALSE])
  units[, p_file_mok := pnorm(xb)]
  units[]
}


# -----------------------------------------------------------------------------
# The Pub 5785 hazard (above the threshold)
# -----------------------------------------------------------------------------

PUB5785_PATH <- "research/state_weights/nonfiler_residual/resources/pub5785_table3_notfiler_units.csv"
PUB5785_TARGET_UNITS <- 11.19e6   # above-threshold non-filing units, TY2014-16
                                  # average -- the v1 STARTING level (D3);
                                  # C4 re-chooses the scalar jointly

#' Non-filing probability for units ABOVE the filing threshold: a national
#' level allocated by Pub 5785 Table 3's relative risks.
#'
#' For each characteristic c (married, wages, self-employment, interest,
#' dividends, pensions, UI), the tilt is
#'     share of above-threshold NON-FILERS with c   (Table 3)
#'   / share of above-threshold UNITS with c        (our data)
#' composed multiplicatively -- a naive-Bayes approximation, adopted
#' deliberately for v1 (the design names Erard et al. (2020) as the upgrade
#' path, and warns its MARRIED coefficient flips sign across vintages, so no
#' single published model is treated as settled). The scalar then solves
#' sum(w * p_nonfile) = target on the above-threshold subset, with
#' probabilities capped at 0.95 -- the cap re-solved iteratively so the
#' target still holds.
#'
#' @param units scored unit table (needs must_file and the src_* covariates)
#' @param target_units national above-threshold non-filing units
pub5785_hazard <- function(units, target_units = PUB5785_TARGET_UNITS,
                           path = PUB5785_PATH) {
  # counts only: Table 3 carries a count row AND an amount row under the same
  # measure name for wages etc., so the concept filter is load-bearing
  t3 <- fread(path)[concept == "count_millions"]
  stopifnot(uniqueN(t3$measure) == nrow(t3))
  n_total <- t3[measure == "tax_units", avg_2014_2016]
  share_nf <- c(
    married  = t3[measure == "mfj_tax_units",     avg_2014_2016] / n_total,
    wages    = t3[measure == "wages",             avg_2014_2016] / n_total,
    se       = t3[measure == "net_business_farm", avg_2014_2016] / n_total,
    interest = t3[measure == "interest",          avg_2014_2016] / n_total,
    dividends = t3[measure == "dividends",        avg_2014_2016] / n_total,
    pensions = t3[measure == "pensions",          avg_2014_2016] / n_total,
    ui       = t3[measure == "unemployment_compensation", avg_2014_2016] / n_total)
  stopifnot(length(share_nf) == 7, !anyNA(share_nf))

  ab <- units[must_file == TRUE]
  stopifnot(nrow(ab) > 0)
  W <- ab[, sum(weight)]
  has <- data.table(
    married  = ab$filing_status == "joint",
    wages    = ab$src_wages == 1,
    se       = ab$src_self_employment == 1,
    interest = ab$src_interest == 1,
    dividends = ab$src_dividends == 1,
    pensions = ab$src_retirement == 1,
    ui       = ab$INCUNEMP > 0)
  share_pop <- vapply(names(share_nf),
                      function(c_) ab[has[[c_]], sum(weight)] / W, numeric(1))

  # relative risk per characteristic; a unit WITHOUT c gets the complementary
  # tilt (1 - share_nf) / (1 - share_pop) so the factors stay probability-
  # consistent rather than only up-weighting
  risk <- rep(1, nrow(ab))
  for (c_ in names(share_nf)) {
    rr_yes <- share_nf[[c_]] / share_pop[[c_]]
    rr_no  <- (1 - share_nf[[c_]]) / (1 - share_pop[[c_]])
    risk <- risk * fifelse(has[[c_]], rr_yes, rr_no)
  }

  # solve the scalar with the cap enforced (a handful of extreme-risk units
  # would otherwise take p > 1 and silently under-deliver the target)
  p <- rep(0, nrow(ab)); s <- target_units / sum(ab$weight * risk)
  for (i in 1:20) {
    p <- pmin(s * risk, 0.95)
    gap <- target_units - sum(ab$weight * p)
    if (abs(gap) < 1) break
    uncapped <- p < 0.95
    stopifnot(any(uncapped))
    s <- s + gap / sum(ab$weight[uncapped] * risk[uncapped])
  }

  units[, p_nonfile_hazard := NA_real_]
  units[must_file == TRUE, p_nonfile_hazard := p]
  units[]
}


# -----------------------------------------------------------------------------
# Combination
# -----------------------------------------------------------------------------

#' P(files) for every unit: Mok's probit below the filing threshold, one minus
#' the Pub 5785 hazard above it.
score_filing_model <- function(units, coefs,
                               target_above = PUB5785_TARGET_UNITS) {
  units <- assign_mok_group(units)
  units <- score_mok(units, coefs)
  units <- pub5785_hazard(units, target_units = target_above)
  units[, p_file := fifelse(must_file, 1 - p_nonfile_hazard, p_file_mok)]
  stopifnot(!anyNA(units$p_file), all(units$p_file >= 0 & units$p_file <= 1))
  units[]
}

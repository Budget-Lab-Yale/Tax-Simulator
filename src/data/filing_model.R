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

# Table 3 total income $409.4B over 11.19M units, TY2014-16 average. The
# eighth calibration constraint -- see the concept caveat in pub5785_hazard().
PUB5785_TARGET_MEAN_INCOME <- 409.4e9 / 11.19e6   # $36,586

#' Non-filing probability for units ABOVE the filing threshold: Pub 5785
#' Table 3's published composition, imposed by raking.
#'
#' v1 composed per-characteristic relative risks MULTIPLICATIVELY (naive
#' Bayes) and solved one scalar. That form does not reproduce the marginals it
#' is built from -- measured 2026-08-29, it achieved wages 55.2% against a
#' 61.7% target, self-employment 39.2% against 44.6%, and dividends 4.8%
#' against 11.0%, because the seven characteristics are strongly dependent in
#' our data and the independence approximation compounds the error. Raking
#' (iterative proportional fitting) imposes each marginal in turn and repeats
#' to a fixed point, which is the standard remedy and lands every one inside
#' `RAKE_TOL`.
#'
#' Eight constraints, not seven. The categorical marginals control WHO has
#' each income source but say nothing about HOW MUCH, and reweighting on
#' presence indicators left the emitted units at $44,207 of wages per
#' wage-earning unit against a published $33,638. Table 3 also publishes total
#' income for the group, so the mean is imposed as an eighth constraint via an
#' exponential tilt -- the entropy-calibration form for a continuous margin,
#' solved by bisection inside each sweep.
#'
#' CONCEPT CAVEAT: Table 3's total income is the 1040 concept; `gross_income`
#' here is the model's own gross-income measure, which excludes Social
#' Security. Table 3's taxable Social Security is $9.3B of $409.4B (2.3%), so
#' the mean target is understated by roughly that much rather than exactly
#' comparable. Recorded rather than adjusted.
#'
#' @param units scored unit table (needs must_file, gross_income, src_*)
#' @param target_units national above-threshold non-filing units
#' @param target_mean_income mean total income per non-filing unit; NULL skips
#'   the eighth constraint and rakes on the seven categorical margins only
PUB5785_CAP      <- 0.95   # no unit is more than 95% likely to be a non-filer
RAKE_SWEEPS      <- 50L
RAKE_TOL         <- 1e-3   # 0.1pp on every margin; the cap makes
                           # exactness unattainable, so this is the
                           # practical floor rather than a slack choice

pub5785_hazard <- function(units, target_units = PUB5785_TARGET_UNITS,
                           target_mean_income = NULL,
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

  # Start from the v1 relative-risk product. Raking converges from any
  # positive start; starting here keeps the naive-Bayes tilt as the prior and
  # lets the sweeps correct only where the independence assumption failed.
  risk <- rep(1, nrow(ab))
  for (c_ in names(share_nf)) {
    rr_yes <- share_nf[[c_]] / share_pop[[c_]]
    rr_no  <- (1 - share_nf[[c_]]) / (1 - share_pop[[c_]])
    risk <- risk * fifelse(has[[c_]], rr_yes, rr_no)
  }
  w <- ab$weight
  p <- pmin(risk * target_units / sum(w * risk), PUB5785_CAP)

  y <- ab$gross_income
  if (!is.null(target_mean_income)) {
    stopifnot(min(y) < target_mean_income, max(y) > target_mean_income)
  }

  #' One exponential tilt on income, solved so the p-weighted mean hits m.
  #' Monotone in lambda, so bisection is safe; the bracket widens until it
  #' straddles rather than assuming a scale.
  tilt_to_mean <- function(p, m) {
    f <- function(lam) {
      q <- p * exp(lam * (y - m) / sd(y))
      sum(w * q * (y - m)) / sum(w * q)
    }
    lo <- -1; hi <- 1
    for (k in 1:40) { if (f(lo) < 0) break; lo <- lo * 2 }
    for (k in 1:40) { if (f(hi) > 0) break; hi <- hi * 2 }
    if (f(lo) >= 0 || f(hi) <= 0) return(p)   # unreachable: leave untilted
    for (k in 1:60) {
      mid <- (lo + hi) / 2
      if (f(mid) > 0) hi <- mid else lo <- mid
    }
    p * exp(((lo + hi) / 2) * (y - m) / sd(y))
  }

  #' Scale to the target COUNT without breaking the cap: capped units cannot
  #' absorb more, so the adjustment goes proportionally to those still free.
  #' A plain rescale-then-clip silently loses the mass it clips.
  solve_level <- function(p, target) {
    for (k in 1:50) {
      gap <- target - sum(w * p)
      if (abs(gap) < 1) break
      unc <- p < PUB5785_CAP
      if (!any(unc)) break
      p[unc] <- pmin(p[unc] * (1 + gap / sum(w[unc] * p[unc])), PUB5785_CAP)
    }
    p
  }

  worst <- NA_real_
  for (sweep in seq_len(RAKE_SWEEPS)) {
    for (c_ in names(share_nf)) {
      cur <- sum(w[has[[c_]]] * p[has[[c_]]]) / sum(w * p)
      p <- p * fifelse(has[[c_]], share_nf[[c_]] / cur,
                       (1 - share_nf[[c_]]) / (1 - cur))
      p <- pmin(p, PUB5785_CAP)
    }
    if (!is.null(target_mean_income)) {
      p <- pmin(tilt_to_mean(p, target_mean_income), PUB5785_CAP)
    }
    # level last, so the count holds at the point the sweep is judged
    p <- solve_level(p, target_units)

    achieved <- vapply(names(share_nf),
                       function(c_) sum(w[has[[c_]]] * p[has[[c_]]]) / sum(w * p),
                       numeric(1))
    worst <- max(abs(achieved - share_nf))
    if (worst < RAKE_TOL && abs(sum(w * p) - target_units) < 1e3) break
  }

  # The cap can make a margin unreachable. That is a finding about the target
  # against our population, never something to pass over in silence.
  if (worst >= RAKE_TOL) {
    warning(sprintf(paste("pub5785_hazard: raking stopped at %.2fpp on the",
                          "worst margin after %d sweeps (%.1f%% of weight at",
                          "the %.2f cap)"),
                    100 * worst, RAKE_SWEEPS,
                    100 * sum(w[p >= PUB5785_CAP]) / sum(w), PUB5785_CAP),
            call. = FALSE)
  }

  p <- solve_level(p, target_units)   # the count is the one exact constraint
  stopifnot(abs(sum(w * p) - target_units) < 1e3, all(p >= 0), all(p <= PUB5785_CAP))

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
                               target_above = PUB5785_TARGET_UNITS,
                               target_mean_income = PUB5785_TARGET_MEAN_INCOME) {
  units <- assign_mok_group(units)
  units <- score_mok(units, coefs)
  units <- pub5785_hazard(units, target_units = target_above,
                          target_mean_income = target_mean_income)
  units[, p_file := fifelse(must_file, 1 - p_nonfile_hazard, p_file_mok)]
  stopifnot(!anyNA(units$p_file), all(units$p_file >= 0 & units$p_file <= 1))
  units[]
}

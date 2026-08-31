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
# NOTE: the TY2014-16 averages (11.19M units, $36,586 mean income) used to
# live here as constants. They are gone -- everything now goes through
# pub5785_targets(), which reads the year's own column from the CSV and
# derives the average from the same source when asked for it. Two copies of a
# published figure, one of them hand-typed, is how a basis goes stale.

# Publication 5785 covers these tax years and no others.
PUB5785_YEARS <- 2014:2016

# Targets for years the publication does not reach, produced by
# nonfiler_pool/10_project_targets.R as rates against our own above-threshold
# population. Absent until that script runs, in which case
# pub5785_targets_for_year() falls back to the average and says so.
PUB5785_PROJECTED_PATH <- file.path("research/state_weights/nonfiler_pool",
                                    "results/pub5785_projected_targets.csv")

# S20: the pandemic filing adjustment. For 2020-21 the projected hazard level
# is INFEASIBLE against the residual anchors -- the stimulus-induced filing
# surge collapsed the residual count of non-filing adults faster than the held
# level allows (band 18_25 over-subscribed by 0.469M/0.096M adults). The
# adjustment deflates the hazard per age band by observed excess filing
# (Pub 1304 T1.6 filing adults against a 2019->2023 rate counterfactual),
# attributed to the above-threshold stock at its share of each band's
# non-filers. Produced by nonfiler_pool/16_pandemic_filing_adjustment.R;
# absent for every other year, in which case nothing here changes.
PANDEMIC_ADJ_PATH <- function(tax_year) {
  sprintf(paste0("research/state_weights/nonfiler_pool/results/",
                 "pandemic_filing_adjustment_%d.csv"), tax_year)
}

#' The seven Table 3 characteristics, as predicates over a unit table.
#'
#' Defined ONCE. `pub5785_hazard()` rakes to these and `02_filing_model.R`'s
#' margin gate measures against them; when the gate kept its own copy, a change
#' to a mapping here would have left the gate silently checking the old
#' definition and reporting a pass. The non-obvious ones are `pensions`, which
#' is retirement income rather than a pension-specific item, and `ui`, which
#' reads the raw ASEC variable because there is no src_ flag for it.
#'
#' @param units a unit table carrying filing_status, the src_* flags, INCUNEMP
#' @return named list of logical vectors, in Table 3's own order
pub5785_characteristics <- function(units) {
  need <- c("filing_status", "src_wages", "src_self_employment", "src_interest",
            "src_dividends", "src_retirement", "INCUNEMP")
  missing <- setdiff(need, names(units))
  if (length(missing)) {
    stop("pub5785_characteristics(): missing ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  list(married   = units$filing_status == "joint",
       wages     = units$src_wages == 1,
       se        = units$src_self_employment == 1,
       interest  = units$src_interest == 1,
       dividends = units$src_dividends == 1,
       pensions  = units$src_retirement == 1,
       ui        = units$INCUNEMP > 0)
}


#' Pub 5785 Table 3 targets for ONE tax year.
#'
#' The averages above were applied to TY2017 and TY2022 without re-basing --
#' the same stale-basis error this branch found in the wage benchmark, and the
#' income constraint doubled what rides on it. The publication reports each
#' year separately and the series moves fast (10.57M / 11.09M / 11.90M units;
#' $34,730 / $36,664 / $38,202 mean income), so an average is not a stand-in
#' for any of them.
#'
#' @param tax_year a year in PUB5785_YEARS, or NULL for the 3-year average
#' @return list(units, mean_income, shares, basis) -- `basis` names the column
#'   used, and travels with the result so downstream reporting can state it
pub5785_targets <- function(tax_year = NULL, path = PUB5785_PATH) {
  col <- if (is.null(tax_year)) {
    "avg_2014_2016"
  } else {
    if (!(tax_year %in% PUB5785_YEARS)) {
      stop(sprintf(paste("Pub 5785 covers %d-%d; TY%d has no published column.",
                         "Project the targets rather than silently borrowing",
                         "the average -- see plan.md group D stage B."),
                   min(PUB5785_YEARS), max(PUB5785_YEARS), tax_year),
           call. = FALSE)
    }
    sprintf("ty%d", tax_year)
  }

  t3 <- fread(path)
  stopifnot(col %in% names(t3))
  cnt <- t3[concept == "count_millions"]
  amt <- t3[concept == "amount_billions"]
  stopifnot(uniqueN(cnt$measure) == nrow(cnt))

  n_total <- cnt[measure == "tax_units", get(col)]
  shares <- c(
    married   = cnt[measure == "mfj_tax_units",     get(col)],
    wages     = cnt[measure == "wages",             get(col)],
    se        = cnt[measure == "net_business_farm", get(col)],
    interest  = cnt[measure == "interest",          get(col)],
    dividends = cnt[measure == "dividends",         get(col)],
    pensions  = cnt[measure == "pensions",          get(col)],
    ui        = cnt[measure == "unemployment_compensation", get(col)]) / n_total
  stopifnot(length(shares) == 7, !anyNA(shares), all(shares > 0), all(shares < 1))

  units <- n_total * 1e6
  list(units       = units,
       mean_income = amt[measure == "total_income", get(col)] * 1e9 / units,
       shares      = shares,
       basis       = col)
}

#' The targets to use for a build year, published or otherwise.
#'
#' Within PUB5785_YEARS this is the year's own column. Outside it there is no
#' published figure, and the honest options are a projection (stage B) or the
#' TY2014-16 average carried over unchanged. The average is what the build did
#' silently until 2026-08-29; it is still the fallback, but it now announces
#' itself and stamps `basis` so every downstream report can say which footing
#' it stands on.
pub5785_targets_for_year <- function(tax_year,
                                     projected_path = PUB5785_PROJECTED_PATH) {
  if (tax_year %in% PUB5785_YEARS) return(pub5785_targets(tax_year))

  if (file.exists(projected_path)) {
    yr_ <- tax_year          # distinct name: `tax_year` is also a column here
    pr  <- fread(projected_path)[tax_year == yr_]
    if (nrow(pr)) {
      get1 <- function(cmp) {
        v <- pr[component == cmp, value]
        stopifnot(length(v) == 1, is.finite(v))
        v
      }
      shares <- vapply(c("married", "wages", "se", "interest", "dividends",
                         "pensions", "ui"),
                       function(c_) get1(paste0("share_", c_)), numeric(1))
      stopifnot(all(shares > 0), all(shares < 1))
      return(list(units       = get1("units"),
                  mean_income = get1("mean_income"),
                  shares      = shares,
                  basis       = sprintf("projected_from_%d-%d",
                                        min(PUB5785_YEARS), max(PUB5785_YEARS))))
    }
  }

  out <- pub5785_targets(NULL)
  out$basis <- sprintf("avg_2014_2016 (NOT re-based to TY%d)", tax_year)
  message(sprintf(paste("  Pub 5785 has no TY%d column and no projection row:",
                        "using the TY2014-16 average unchanged (%.2fM units,",
                        "$%s mean income). The series ran 10.57/11.09/11.90M,",
                        "so this is a stale basis, not a neutral one. Run",
                        "nonfiler_pool/10_project_targets.R %d."),
                  tax_year, out$units / 1e6,
                  format(round(out$mean_income), big.mark = ","), tax_year))
  out
}

#' The S20 pandemic filing adjustment for one year, or NULL when the year has
#' none. Returns the CELL-space (7-band) deflator vector -- the file is in the
#' 6-band target space because the anchors are, and 65p is expanded onto
#' 65_74/75p, which the anchor cannot split -- plus the deflated national
#' units total and a basis suffix for downstream reporting.
pandemic_filing_adjustment <- function(tax_year) {
  path <- PANDEMIC_ADJ_PATH(tax_year)
  if (!file.exists(path)) return(NULL)
  adj <- fread(path)
  stopifnot(setequal(adj$band, TARGET_AGE_BANDS),
            all(is.finite(adj$m_central)),
            all(adj$m_central > 0 & adj$m_central <= 1),
            all(is.finite(adj$units_deflated)))
  m6 <- setNames(adj$m_central, adj$band)
  m7 <- setNames(m6[as.character(target_age_band(AGE_BANDS))], AGE_BANDS)
  stopifnot(!anyNA(m7))
  list(m              = m7,
       units_deflated = adj[, sum(units_deflated)],
       basis_suffix   = "+S20_pandemic_deflation")
}

#' The hazard level a year is actually held to: the trend level, deflated by
#' the S20 adjustment when one exists. THE single source for every gate that
#' checks the level (02's gate 2, 04's preservation assertion) -- the trend
#' accessor pub5785_targets_for_year() deliberately keeps returning the
#' UNDEFLATED level, because the hazard must be raked and level-solved at
#' trend before the deflator applies; pre-deflating the target would
#' double-deflate.
pub5785_effective_units <- function(tax_year) {
  adj <- pandemic_filing_adjustment(tax_year)
  if (is.null(adj)) pub5785_targets_for_year(tax_year)$units
  else adj$units_deflated
}


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
#' @param targets a `pub5785_targets()` list: units, mean_income, shares, basis
#' @param use_mean_income FALSE rakes on the seven categorical margins only,
#'   dropping the eighth constraint -- for isolating its effect, not production
#' @param band_deflators a `pandemic_filing_adjustment()` list, or NULL (every
#'   year but 2020-21). Applied multiplicatively per age band AFTER the trend
#'   rake and level solve: m <= 1 never violates the cap, and multiply is
#'   exact per unit, so the aggregate lands on the deflated total to floating
#'   point with no re-solve. Deliberately NOT re-raked afterwards -- the
#'   characteristics correlate with age, so re-raking would partially undo the
#'   band-specific deflation; the margin drift is reported per year in
#'   hazard_margins_{year}.csv, S17-style.
PUB5785_CAP      <- 0.95   # no unit is more than 95% likely to be a non-filer
RAKE_SWEEPS      <- 50L
RAKE_TOL         <- 1e-3   # 0.1pp on every margin; the cap makes
                           # exactness unattainable, so this is the
                           # practical floor rather than a slack choice

pub5785_hazard <- function(units, targets, use_mean_income = TRUE,
                           band_deflators = NULL) {
  stopifnot(is.list(targets),
            all(c("units", "mean_income", "shares", "basis") %in% names(targets)))
  target_units <- targets$units
  share_nf     <- targets$shares
  target_mean_income <- if (use_mean_income) targets$mean_income else NULL
  stopifnot(length(share_nf) == 7, !anyNA(share_nf))

  ab <- units[must_file == TRUE]
  stopifnot(nrow(ab) > 0)
  W <- ab[, sum(weight)]
  has <- pub5785_characteristics(ab)
  stopifnot(identical(names(has), names(share_nf)))
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
  sd_y <- stats::sd(y)
  stopifnot(is.finite(sd_y), sd_y > 0)

  #' One exponential tilt on income, solved so the p-weighted mean hits m.
  #' Monotone in lambda, so bisection is safe; the bracket widens until it
  #' straddles rather than assuming a scale.
  tilt_to_mean <- function(p, m) {
    # z is fixed across every evaluation; sd() and the centring used to be
    # recomputed inside f(), which runs up to 140 times per sweep and 50 sweeps
    # per year over a ~200k-row table.
    z   <- (y - m) / sd_y
    dev <- y - m
    f <- function(lam) {
      q <- p * exp(lam * z)
      s <- sum(w * q)
      if (!is.finite(s) || s <= 0) return(NA_real_)   # over/underflowed
      sum(w * q * dev) / s
    }
    # Widen until the root is bracketed. isTRUE() matters: far enough out,
    # exp(lam * z) underflows to 0 or overflows to Inf for every unit and f()
    # is NaN -- a bare `if (f(lo) < 0)` then throws "missing value where
    # TRUE/FALSE needed" instead of falling through to the guard below, which
    # is the behaviour actually wanted.
    lo <- -1; hi <- 1
    for (k in 1:40) { if (isTRUE(f(lo) < 0)) break; lo <- lo * 2 }
    for (k in 1:40) { if (isTRUE(f(hi) > 0)) break; hi <- hi * 2 }
    if (!isTRUE(f(lo) < 0) || !isTRUE(f(hi) > 0)) return(p)   # leave untilted
    for (k in 1:60) {
      mid <- (lo + hi) / 2
      if (isTRUE(f(mid) > 0)) hi <- mid else lo <- mid
    }
    p * exp(((lo + hi) / 2) * z)
  }

  #' Scale to the target COUNT without breaking the cap: capped units cannot
  #' absorb more, so the adjustment goes proportionally to those still free.
  #' A plain rescale-then-clip silently loses the mass it clips.
  solve_level <- function(p, target) {
    # The capped mass alone can exceed the target. Then no reduction of the
    # free units reaches it, the multiplier below goes negative, and without
    # the pmax() the next iteration divides by a negative denominator and the
    # run dies later at an opaque `all(p >= 0)`. Diagnose it here instead.
    capped_mass <- sum(w[p >= PUB5785_CAP] * PUB5785_CAP)
    if (capped_mass > target) {
      stop(sprintf(paste("pub5785_hazard(): units already at the %.2f cap carry",
                         "%.2fM of the %.2fM target on their own. The target is",
                         "unreachable from this population without raising the",
                         "cap -- a finding about the target, not a solver",
                         "setting."),
                   PUB5785_CAP, capped_mass / 1e6, target / 1e6), call. = FALSE)
    }
    for (k in 1:50) {
      gap <- target - sum(w * p)
      if (abs(gap) < 1) break
      unc <- p < PUB5785_CAP
      if (!any(unc)) break
      p[unc] <- pmax(pmin(p[unc] * (1 + gap / sum(w[unc] * p[unc])),
                          PUB5785_CAP), 0)
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
    # Name the margin. A bare "worst margin" number invites the assumption
    # that more sweeps or a looser cap would fix it; for self-employment
    # neither does. Solver sensitivity measured 2026-08-29 on TY2022: sweeps
    # 50 -> 1000 moved the miss only 3.38pp -> 3.08pp, and the cap
    # 0.95 -> 0.999 only to 2.23pp. The constraint is the ASEC's joint
    # distribution -- see S17 in research/decisions_log.md, and
    # results/hazard_margins_{year}.csv for the accepted residual per year.
    #
    # The miss scales with the TARGET LEVEL, which is the part worth knowing:
    # in the three years Pub 5785 publishes, the margin is hit to within
    # 0.20pp. It only opens once the level is projected above what those years
    # demanded. So part of the gap belongs to the projection, not to the
    # survey.
    off <- achieved - share_nf
    nm  <- names(off)[which.max(abs(off))]
    warning(sprintf(paste("pub5785_hazard: raking left %s at %+.2fpp after %d",
                          "sweeps (worst of seven; %.1f%% of weight at the",
                          "%.2f cap). Reweighting cannot create a joint",
                          "distribution the source data lacks."),
                    nm, 100 * off[[nm]], RAKE_SWEEPS,
                    100 * sum(w[p >= PUB5785_CAP]) / sum(w), PUB5785_CAP),
            call. = FALSE)
  }

  p <- solve_level(p, target_units)   # the count is the one exact constraint
  stopifnot(abs(sum(w * p) - target_units) < 1e3, all(p >= 0), all(p <= PUB5785_CAP))

  # S20: the pandemic deflation, per age band, after the trend solve. The
  # closing assertion against the adjustment file's own total is a STALENESS
  # guard, not a tolerance fudge: multiply is exact, so a miss means the file
  # was built against a different trend scoring than this one.
  if (!is.null(band_deflators)) {
    stopifnot(c("m", "units_deflated") %in% names(band_deflators),
              "age_head" %in% names(ab))
    m_u <- band_deflators$m[as.character(age_band(ab$age_head))]
    stopifnot(!anyNA(m_u), all(m_u > 0 & m_u <= 1))
    p <- p * m_u
    stopifnot(abs(sum(w * p) - band_deflators$units_deflated) < 1e3,
              all(p >= 0), all(p <= PUB5785_CAP))
  }

  units[, p_nonfile_hazard := NA_real_]
  units[must_file == TRUE, p_nonfile_hazard := p]
  units[]
}


# -----------------------------------------------------------------------------
# Combination
# -----------------------------------------------------------------------------

#' P(files) for every unit: Mok's probit below the filing threshold, one minus
#' the Pub 5785 hazard above it. `band_deflators` (S20, 2020-21 only) passes
#' through to the hazard; NULL leaves every other year bit-identical.
score_filing_model <- function(units, coefs, targets, band_deflators = NULL) {
  units <- assign_mok_group(units)
  units <- score_mok(units, coefs)
  units <- pub5785_hazard(units, targets = targets,
                          band_deflators = band_deflators)
  units[, p_file := fifelse(must_file, 1 - p_nonfile_hazard, p_file_mok)]
  stopifnot(!anyNA(units$p_file), all(units$p_file >= 0 & units$p_file <= 1))
  units[]
}

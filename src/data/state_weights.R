# =============================================================================
# state_weights.R  —  Phase 1 prototype: split state weights for the PUF
#
# Produces `state_weights_{year}.csv` (id, state, weight) that make the national
# PUF-based microdata state-representative, subject to the per-record split
# constraint  Σ_state W[i,state] = weight_i  (so federal aggregates are invariant
# to the with-state / without-state mode switch).
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
read_ht2 <- function(path, year) {

  d <- fread(path, colClasses = "character")
  setnames(d, toupper(names(d)))

  parse_num <- function(x) as.numeric(str_replace_all(x, ",", ""))

  # Core target columns, tolerant of cross-year naming drift.
  agi_col <- if ("A00100" %in% names(d)) "A00100" else "A001"   # AGI amount ($000s)
  keep_counts  <- intersect(c("N1","MARS1","MARS2","MARS4","N2"), names(d))
  keep_amounts <- intersect(c(agi_col, "A00200", "A59660"), names(d))  # AGI, wages, EITC amt

  d[, STATE := toupper(trimws(STATE))]
  d[, AGI_STUB := as.integer(AGI_STUB)]
  d <- d[!(STATE %in% c("US","")) & AGI_STUB != 0]              # drop US total & stub-0 total

  num_cols <- c(keep_counts, keep_amounts)
  d[, (num_cols) := lapply(.SD, parse_num), .SDcols = num_cols]

  long <- melt(d[, c("STATE","AGI_STUB", num_cols), with = FALSE],
               id.vars = c("STATE","AGI_STUB"),
               variable.name = "variable", value.name = "value")
  # amounts → dollars
  long[variable %in% keep_amounts, value := value * 1000]
  long[, variable := fifelse(variable == agi_col, "agi_amt",
                      fifelse(variable == "A00200", "wages_amt",
                      fifelse(variable == "A59660", "eitc_amt",
                      fifelse(variable == "N1", "n_returns",
                      fifelse(variable == "MARS1", "n_single",
                      fifelse(variable == "MARS2", "n_joint",
                      fifelse(variable == "MARS4", "n_hoh",
                      fifelse(variable == "N2", "n_indiv", as.character(variable)))))))))]
  setnames(long, c("STATE","AGI_STUB"), c("state","agi_stub"))
  long[, year := year][]
}

# -----------------------------------------------------------------------------
# ACS side: non-filer population margins.
#
# HT2 covers filers only, so the non-filer PUF partition is targeted to ACS/Census
# state × age × income margins instead (plan §2.1). This builds those margins from
# the local IPUMS USA extract. The authority for WHO is a non-filer is the Tax-Data
# `filer` flag on the PUF; ACS supplies only the geographic MARGINS across which the
# non-filer partition is spread — so the ACS filing model here need only be a
# reasonable v0, refined against the reconciliation total (plan risk item).
#
# v0 tax-unit + filing model (documented approximations, to refine later):
#   - Couples (MARST==1 & SPLOC>0) file jointly, head = lower PERNUM.
#   - Children (AGE < 19, parent pointer present, not married) are dependents.
#   - All other adults head their own unit (single, or HoH if they have dependents).
#   - Qualifying-relative dependency NOT modeled (minor); student ages 19–24 not
#     separable (extract lacks SCHOOL).
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

#' Build ACS non-filer margins + state population totals from the local IPUMS extract.
#' @return list(nonfiler_margins = dt[state, age_band, income_tier, n_units],
#'              state_pop        = dt[state, pop])   both weighted to population.
build_acs_margins <- function(ipums_csv, year, acs_year = NULL,
                              cols = c("YEAR","STATEFIP","PERWT","PERNUM","SERIAL","SAMPLE",
                                       "AGE","MARST","SPLOC","MOMLOC","POPLOC","INCTOT")) {

  a <- fread(ipums_csv, select = cols)
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

  units[, filing_status := fifelse(has_spouse, "joint",
                           fifelse(n_dep > 0, "hoh", "single"))]
  thr <- filing_threshold(year)
  units[, threshold := thr[filing_status]]
  units[, is_filer := gross_inc >= threshold]

  units[, `:=`(age_band = age_band(head_age), income_tier = income_tier(gross_inc))]

  nonfiler_margins <- units[is_filer == FALSE,
                            .(n_units = sum(weight)),
                            by = .(state, age_band, income_tier)][order(state, age_band, income_tier)]
  state_pop <- a[, .(pop = sum(PERWT)), by = state][order(state)]

  list(nonfiler_margins = nonfiler_margins[, year := year],
       state_pop        = state_pop[, year := year],
       unit_summary     = units[, .(n_units = sum(weight, na.rm = TRUE),
                                     n_filers = sum(weight * is_filer, na.rm = TRUE),
                                     n_nonfilers = sum(weight * !is_filer, na.rm = TRUE),
                                     n_units_na_wt = sum(is.na(weight)))],
       acs_year         = acs_year)
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
fit_gradient <- function(w, P0, targets, beta = 1e-3,
                         lr = 0.1, n_steps = 500, verbose = FALSE) {
  N <- length(w); S <- ncol(P0)
  theta <- log(pmax(P0, 1e-12))                          # warm start at the prior
  m <- matrix(0, N, S); v <- matrix(0, N, S)             # Adam moments
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  softmax_rows <- function(z) { z <- z - apply(z, 1, max); e <- exp(z); e / rowSums(e) }

  loss_hist <- numeric(n_steps)
  for (step in seq_len(n_steps)) {
    P <- softmax_rows(theta)
    W <- w * P
    G <- matrix(0, N, S)                                 # dLoss_fit / dP
    loss_fit <- 0
    for (t in targets) {
      That <- sum(w[t$rows] * P[t$rows, t$state] * t$x)
      resid <- (That - t$target) / t$target
      loss_fit <- loss_fit + t$lambda * resid^2
      G[t$rows, t$state] <- G[t$rows, t$state] +
        t$lambda * 2 * resid / t$target * w[t$rows] * t$x
    }
    # KL(P||P0) gradient wrt P, plus softmax backprop:  gtheta = P*(g - rowSums(P*g))
    Greg <- beta * (log(pmax(P, 1e-12)) - log(pmax(P0, 1e-12)) + 1)
    g <- G + Greg
    gtheta <- P * (g - rowSums(P * g))
    # Adam
    m <- b1 * m + (1 - b1) * gtheta
    v <- b2 * v + (1 - b2) * gtheta^2
    mhat <- m / (1 - b1^step); vhat <- v / (1 - b2^step)
    theta <- theta - lr * mhat / (sqrt(vhat) + eps)
    loss_kl <- beta * sum(P * (log(pmax(P,1e-12)) - log(pmax(P0,1e-12))))
    loss_hist[step] <- loss_fit + loss_kl
    if (verbose && step %% max(1, n_steps %/% 10) == 0)
      message(sprintf("  step %d  loss_fit=%.4e  loss_kl=%.4e", step, loss_fit, loss_kl))
  }
  list(P = softmax_rows(theta), loss_hist = loss_hist)
}

# --- Approach A: classical raking / IPF ---------------------------------------
# Seeded at the prior P0, iteratively rescales each (target) so predicted totals
# approach targets, then renormalizes rows to restore Σ_s P[i,s] = 1. Count and
# amount targets are handled by the generic x-vector. Converges to a calibrated
# split; the row renormalization is what enforces the split constraint each pass.
fit_calibration <- function(w, P0, targets, n_iter = 50, tol = 1e-4, verbose = FALSE) {
  P <- P0
  for (iter in seq_len(n_iter)) {
    maxrel <- 0
    for (t in targets) {
      That <- sum(w[t$rows] * P[t$rows, t$state] * t$x)
      if (That > 0) {
        f <- t$target / That
        P[t$rows, t$state] <- P[t$rows, t$state] * f      # rescale toward target
        maxrel <- max(maxrel, abs(f - 1))
      }
    }
    rs <- rowSums(P); P <- P / rs                          # restore row-sum = 1
    if (verbose && iter %% 10 == 0) message(sprintf("  iter %d  max|f-1|=%.4f", iter, maxrel))
    if (maxrel < tol) break
  }
  list(P = P, iters = iter)
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

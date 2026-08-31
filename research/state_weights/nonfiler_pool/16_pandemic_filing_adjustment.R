#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 16_pandemic_filing_adjustment.R  (decision S20)
#
# The S20 pandemic deflators: per-band multipliers on the above-threshold
# filing hazard for the years the stimulus-induced filing surge makes the
# projected Pub 5785 level INFEASIBLE against the residual anchors (2020: band
# 18_25 over-subscribed by 0.469M adults; 2021: by 0.096M).
#
# METHOD. The EIP-era surge is treated as an exogenous, observable draw from
# the non-filer stock:
#
#   * EXCESS FILING per 6-band, X_b = max(0, (rate_obs - rate_cf)) * adults_b,
#     where rate = filing adults / PEP adults from the residual-anchor files
#     (Pub 1304 T1.6 levels, S15-corrected) and the counterfactual is a linear
#     interpolation of each band's filing RATE between 2019 and 2023 -- the
#     two nearest years outside the excursion (returns per adult .6101 in
#     2019, .6314 in 2020, .6008 in 2023, fully reverted). Deflate only,
#     never inflate: X is clipped at zero.
#   * ATTRIBUTION theta_b = the above-threshold share of the band's
#     non-filing adults (hh_above / achieved from the calibrated endpoint
#     years), interpolated the same way. Induced above-threshold filers are
#     A_b = theta_b * X_b.
#   * DEFLATOR m_b = 1 - A_b / hh_above_trend_b, clipped to [0.05, 1], where
#     hh_above_trend_b is this script's OWN trend scoring of the year (the
#     same deterministic path 02 runs without the adjustment) -- so the script
#     is idempotent and cannot be poisoned by a scored_units file that already
#     carries the deflation.
#
# FEASIBILITY CEILING, closed form. 04's identity per 7-band cell c, with the
# dependent shifts at zero (the worst case 04 falls back to) and the hazard
# scaled by m: below(delta) + m*hh_above_c + gq_c = anchor_c - dep_below0_c -
# m*dep_above_c, and 04 demands 2% of the below-threshold mass stay in play.
# Solving for the LARGEST m the identity tolerates:
#
#   m_max(c) = (anchor_c - dep_below0_c - gq_c - 0.02*mass_c)
#              / (hh_above_c + dep_above_c)
#
# The 6-band ceiling is the min over its cells. Where the central m exceeds
# the ceiling, m is lowered to it and `floor_binding` says so -- this is where
# the identity's own minimum deflation (option 3's bound) surfaces honestly.
# 04 remains the verifier.
#
# SENSITIVITY (report-only columns, central is production):
#   m_theta_low_edge -- the ceiling itself: the LEAST deflation the identity
#                       allows (theta -> 0 is refuted by the infeasibility
#                       that motivated S20).
#   m_theta1         -- theta = 1: every excess filer drawn from the
#                       above-threshold stock, floored at 0.05.
#
# Writes: results/pandemic_filing_adjustment_{year}.csv, one row per 6-band:
#   band, pep_adults, filing_rate_obs, filing_rate_cf, excess_adults, theta,
#   induced_above_adults, hh_above_trend_adults, units_trend, m_raw,
#   m_ceiling, floor_binding, m_central, m_theta_low_edge, m_theta1,
#   units_deflated, tax_year
#
# Consumed by src/data/filing_model.R::pandemic_filing_adjustment() -> 02 -> 04.
# Rebuild order: 16 -> 02 {year} -> 04 {year}.
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/16_pandemic_filing_adjustment.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')
source('src/data/filing_model.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2020L, 2021L)
RES   <- 'research/state_weights/nonfiler_pool/results'
ANCH  <- 'research/state_weights/nonfiler_residual/results'
SHAPE <- 'research/state_weights/nonfiler_residual/resources'

# Counterfactual endpoint years: the nearest observed years OUTSIDE the
# pandemic filing excursion. 2022 still shows elevated filing under this
# counterfactual (~0.77M excess in 18_25) but is feasible and published;
# S20's scope is the infeasible years only.
CF_YEARS <- c(2019L, 2023L)
MARGIN   <- 0.02   # 04's headroom gate, mirrored exactly
M_FLOOR  <- 0.05   # no band's hazard is deflated below 5% of trend

mok <- read_mok_coefs()

read_anchor <- function(y) {
  fread(file.path(ANCH, sprintf('national_anchor_%d.csv', y)))[
    band != 'total_18p', .(band, pep_adults, filing_adults)]
}
read_theta <- function(y) {
  cal <- fread(file.path(RES, sprintf('calibration_%d.csv', y)))
  cal[, band6 := as.character(target_age_band(band))]
  cal[, .(theta = sum(hh_above) / sum(achieved)), by = .(band = band6)]
}

cf <- lapply(setNames(CF_YEARS, CF_YEARS), read_anchor)
th <- lapply(setNames(CF_YEARS, CF_YEARS), read_theta)

for (yr in YEARS) {
  message('=== TY', yr)
  stopifnot(yr > min(CF_YEARS), yr < max(CF_YEARS))
  lam <- (yr - CF_YEARS[1]) / (CF_YEARS[2] - CF_YEARS[1])

  #---------------------------------------------------------------------------
  # Trend scoring: the same deterministic path 02 runs, WITHOUT the deflator.
  # Self-contained so the script cannot read a scored_units file that already
  # carries the S20 deflation.
  #---------------------------------------------------------------------------
  st <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  tg <- pub5785_targets_for_year(yr)
  u  <- score_filing_model(add_mok_covariates(st$units, st$persons),
                           mok$coefs, tg)
  trend_total <- u[must_file == TRUE, sum(weight * p_nonfile_hazard)]
  stopifnot(abs(trend_total - tg$units) < 1e3)

  u[, band := as.character(age_band(age_head))]

  # What the deflator actually scales in 02: EVERY must_file unit (dependent-
  # headed and under-18-headed included), in UNITS -- 02's gate 2 space.
  units_trend <- u[must_file == TRUE,
                   .(units_trend = sum(weight * p_nonfile_hazard)), by = band]

  # 04's identity pieces, in ADULTS, nondependent 18+ frame
  nd <- u[unit_type == 'nondependent' & age_head >= 18]
  nd[, w_adults := weight * fifelse(filing_status == 'joint', 2, 1)]
  hh_above <- nd[must_file == TRUE,
                 .(hh_above = sum(w_adults * p_nonfile_hazard)), by = band]
  mass     <- nd[must_file == FALSE, .(mass = sum(w_adults)), by = band]

  # Zero-shift dependent netting, split by the claimed dependent's unit arm
  # (04:252-257's join): below-threshold dependents at raw Mok probabilities,
  # above-threshold dependents at the trend hazard -- the arm m scales.
  deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                     .(AGE, ASECWT,
                       unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
  deps <- merge(deps,
                u[unit_type == 'dependent',
                  .(unit_id, must_file, p_file, p_nonfile_hazard)],
                by = 'unit_id', all.x = TRUE)
  stopifnot(!anyNA(deps$p_file))
  deps[, band := as.character(age_band(AGE))]
  dep_terms <- deps[, .(
    dep_below0 = sum(ASECWT * (1 - p_file) * (must_file == FALSE)),
    dep_above  = sum(ASECWT * fifelse(must_file, p_nonfile_hazard, 0))),
    by = band]

  anchor <- fread(file.path(SHAPE, sprintf('nonfiler_age_shape_%d.csv', yr)))[
    , .(band, anchor = residual_nonfiling_adults)]
  gq_b <- fread(file.path(RES, sprintf('gq_backfill_summary_%d.csv', yr)))[
    band != 'u18', .(gq_nonfiling = sum(nonfiling_adults)), by = band]

  cell <- Reduce(function(a, b) merge(a, b, by = 'band', all = TRUE),
                 list(anchor, hh_above, mass, dep_terms, gq_b, units_trend))
  for (v in setdiff(names(cell), 'band')) cell[is.na(get(v)), (v) := 0]
  stopifnot(setequal(cell$band, AGE_BANDS), all(cell$hh_above > 0))

  # The closed-form ceiling per cell, then the 6-band minimum
  cell[, m_max := (anchor - dep_below0 - gq_nonfiling - MARGIN * mass) /
                  (hh_above + dep_above)]
  cell[, band6 := as.character(target_age_band(band))]
  band6 <- cell[, .(hh_above    = sum(hh_above),
                    units_trend = sum(units_trend),
                    m_ceiling   = pmin(min(m_max), 1)), by = .(band = band6)]
  if (band6[, any(m_ceiling < M_FLOOR)]) {
    stop(sprintf(paste('band(s) %s infeasible even at maximal deflation',
                       '(ceiling < %.2f) -- the residual count refuses the',
                       'hazard outright; S20 cannot rescue TY%d.'),
                 band6[m_ceiling < M_FLOOR, paste(band, collapse = ', ')],
                 M_FLOOR, yr))
  }

  #---------------------------------------------------------------------------
  # Excess filing and attribution, in the 6-band anchor space
  #---------------------------------------------------------------------------
  obs <- read_anchor(yr)
  X <- merge(obs, cf[[1]][, .(band, f19 = filing_adults / pep_adults)],
             by = 'band')
  X <- merge(X, cf[[2]][, .(band, f23 = filing_adults / pep_adults)],
             by = 'band')
  X[, rate_obs := filing_adults / pep_adults]
  X[, rate_cf  := f19 * (1 - lam) + f23 * lam]
  X[, excess   := pmax((rate_obs - rate_cf) * pep_adults, 0)]

  theta <- merge(th[[1]][, .(band, t19 = theta)],
                 th[[2]][, .(band, t23 = theta)], by = 'band')
  theta[, theta := t19 * (1 - lam) + t23 * lam]
  stopifnot(theta[, all(theta > 0 & theta < 1)])

  adj <- Reduce(function(a, b) merge(a, b, by = 'band'),
                list(X[, .(band, pep_adults, rate_obs, rate_cf, excess)],
                     theta[, .(band, theta)], band6))
  stopifnot(nrow(adj) == length(TARGET_AGE_BANDS))
  adj[, induced_above := theta * excess]
  adj[, m_raw     := 1 - induced_above / hh_above]
  adj[, m_central := pmin(pmax(m_raw, M_FLOOR), 1)]
  adj[, floor_binding := m_central > m_ceiling]
  adj[floor_binding == TRUE, m_central := m_ceiling]
  adj[, m_theta_low_edge := m_ceiling]
  adj[, m_theta1 := pmin(pmax(1 - excess / hh_above, M_FLOOR), 1)]
  adj[, units_deflated := m_central * units_trend]
  adj[, tax_year := yr]
  stopifnot(adj[, all(m_central >= M_FLOOR & m_central <= 1)])

  for (i in seq_len(nrow(adj)))
    message(sprintf(paste('  %-6s excess %5.2fM  theta %.3f  induced %5.2fM',
                          ' of hh_above %5.2fM  ->  m %.3f%s'),
                    adj$band[i], adj$excess[i] / 1e6, adj$theta[i],
                    adj$induced_above[i] / 1e6, adj$hh_above[i] / 1e6,
                    adj$m_central[i],
                    fifelse(adj$floor_binding[i],
                            sprintf('  [CEILING BINDING: central %.3f > max %.3f]',
                                    pmin(pmax(adj$m_raw[i], M_FLOOR), 1),
                                    adj$m_ceiling[i]), '')))
  message(sprintf(paste('  level: %.2fM trend units -> %.2fM deflated (-%.1f%%);',
                        'excess filing %.2fM adults, %.2fM attributed above',
                        'threshold'),
                  adj[, sum(units_trend)] / 1e6, adj[, sum(units_deflated)] / 1e6,
                  100 * (1 - adj[, sum(units_deflated) / sum(units_trend)]),
                  adj[, sum(excess)] / 1e6, adj[, sum(induced_above)] / 1e6))

  setnames(adj, c('excess', 'induced_above', 'hh_above',
                  'rate_obs', 'rate_cf'),
           c('excess_adults', 'induced_above_adults', 'hh_above_trend_adults',
             'filing_rate_obs', 'filing_rate_cf'))
  setcolorder(adj, c('band', 'pep_adults', 'filing_rate_obs', 'filing_rate_cf',
                     'excess_adults', 'theta', 'induced_above_adults',
                     'hh_above_trend_adults', 'units_trend', 'm_raw',
                     'm_ceiling', 'floor_binding', 'm_central',
                     'm_theta_low_edge', 'm_theta1', 'units_deflated',
                     'tax_year'))
  f <- file.path(RES, sprintf('pandemic_filing_adjustment_%d.csv', yr))
  fwrite(adj, f)
  message('  wrote ', f)
}

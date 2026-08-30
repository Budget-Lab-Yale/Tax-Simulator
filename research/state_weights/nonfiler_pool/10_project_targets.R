#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 10_project_targets.R  (group D stage B)
#
# Publication 5785 covers TY2014-2016 and stops. Every build year after that
# was using the THREE-YEAR AVERAGE unchanged -- 11.19M units and a $36,586 mean
# income applied to TY2017 and TY2022 alike, with nothing in the build aware of
# which year its targets came from. That is the same stale-basis error this
# branch found in the wage benchmark, and adding the income constraint on
# 2026-08-29 doubled what rides on it.
#
# This script replaces the average with a projection, on the principle JI chose
# (2026-08-29): express each target as a RATE AGAINST OUR OWN above-threshold
# population, measure the rate in the three published years, and carry the rate
# forward. The above-threshold population is computed from the filing-
# requirement test on real law, so a projection built this way MOVES WHEN THE
# THRESHOLD MOVES -- which is what has to happen across TCJA, and what no
# external series would do for us.
#
# The three relationships, measured 2026-08-29:
#
#   level        N* / above-threshold units    0.0897  0.0920  0.0970   rising
#   mean income  m* / our mean gross income    0.486   0.495   0.496    flat
#   composition  q_c / our own share of c      see below, CV 2.3-13.1%
#
# The mean-income ratio being flat to within 2% across three years is the
# strongest evidence that the whole approach is sound: obligated non-filers
# earn a near-constant fraction of what the above-threshold population earns.
#
# EXTRAPOLATION RULE. A linear trend on three points is carried at most
# TREND_HORIZON years past the last published year; BEYOND that horizon the
# rate reverts to the last OBSERVED value, not to the frozen fitted one. Three
# observations cannot support a long extrapolation, and TCJA (2018) changed
# the filing threshold itself, so a rate estimated entirely under pre-TCJA law
# has no claim on 2022.
#
# The reversion is not a stylistic preference -- it was forced. Carrying the
# frozen 2018 fitted rate (0.1038) to TY2022 gives 13.65M obligated
# non-filers, and 04_calibrate.R then refuses the year outright: the
# above-threshold hazard alone over-subscribes the 18-25 band, leaving -1.3%
# of its below-threshold mass in play. The residual-count anchor and that
# target are mutually inconsistent, and the band solve is built to stop rather
# than clip. Reverting to the last observed rate is the conservative reading
# the data will actually support.
#
# Both projections are reported at every year so the spread between them is
# visible rather than buried in a chosen constant.
#
# Writes: results/pub5785_projected_targets.csv  (read by
#         pub5785_targets_for_year() in src/data/filing_model.R)
#
# Login-node safe. Requires units_{year}.rds from 01_build_units.R for every
# calibration AND projection year.
#   Rscript research/state_weights/nonfiler_pool/10_project_targets.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')
source('src/data/filing_model.R')

args        <- commandArgs(trailingOnly = TRUE)
TARGET_YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES         <- 'research/state_weights/nonfiler_pool/results'

CAL_YEARS     <- PUB5785_YEARS          # 2014:2016, the published columns
TREND_HORIZON <- 2L                     # years past max(CAL_YEARS) a trend runs
CHARACTERISTICS <- c('married', 'wages', 'se', 'interest', 'dividends',
                     'pensions', 'ui')

#-------------------------------------------------------------------------------
# Our above-threshold population, from the unit build alone.
#
# Deliberately reads units_{y}.rds and NOT scored_units_{y}.rds: `must_file`
# and `weight` are set by the filing-requirement test, before any hazard
# scoring, so there is no circularity in using them to set the hazard's target.
#-------------------------------------------------------------------------------
own_above_threshold <- function(year) {
  f <- file.path(RES, sprintf('units_%d.rds', year))
  if (!file.exists(f)) {
    stop(sprintf(paste('no units_%d.rds -- run 01_build_units.R %d first.',
                       'The projection needs OUR above-threshold population',
                       'in the target year, not just in the published ones.'),
                 year, year), call. = FALSE)
  }
  st <- readRDS(f)
  # The src_* presence flags are built by add_mok_covariates(), not by
  # 01_build_units.R. Reading the raw units table gives columns that are
  # ABSENT, and `w[a$src_wages == 1]` on an absent column is silently
  # zero-length -- every share came out 0.0000 the first time this ran.
  a <- add_mok_covariates(st$units, st$persons)[must_file == TRUE]
  need <- c('src_wages', 'src_self_employment', 'src_interest', 'src_dividends',
            'src_retirement', 'INCUNEMP', 'filing_status', 'gross_income')
  stopifnot(all(need %in% names(a)))
  w <- a$weight
  W <- sum(w)
  list(units       = W,
       mean_income = sum(w * a$gross_income) / W,
       shares      = c(
         married   = sum(w[a$filing_status == 'joint']),
         wages     = sum(w[a$src_wages == 1]),
         se        = sum(w[a$src_self_employment == 1]),
         interest  = sum(w[a$src_interest == 1]),
         dividends = sum(w[a$src_dividends == 1]),
         pensions  = sum(w[a$src_retirement == 1]),
         ui        = sum(w[a$INCUNEMP > 0])) / W)
}

#' Carry a three-point series forward. `trend` fits a line and evaluates it,
#' frozen at the horizon; `hold` repeats the last observation. Returns both,
#' always, because the gap between them IS the projection uncertainty and
#' reporting one number would hide it.
carry <- function(values, years, to_year, horizon = TREND_HORIZON) {
  last_y  <- max(years)
  eval_at <- min(to_year, last_y + horizon)
  fit      <- lm(values ~ years)
  monotone <- all(diff(values) > 0) || all(diff(values) < 0)
  hold     <- values[which.max(years)]
  frozen   <- to_year > last_y + horizon
  list(trend    = if (frozen) hold
                  else unname(predict(fit, data.frame(years = eval_at))),
       hold     = hold,
       monotone = monotone,
       frozen   = frozen,
       eval_at  = if (frozen) last_y else eval_at)
}

#-------------------------------------------------------------------------------
# Measure the three relationships in the published years
#-------------------------------------------------------------------------------
message('=== calibration years (Pub 5785 published): ',
        paste(CAL_YEARS, collapse = ', '))

cal <- rbindlist(lapply(CAL_YEARS, function(y) {
  tg <- pub5785_targets(y)
  ow <- own_above_threshold(y)
  data.table(year        = y,
             pub_units   = tg$units,
             own_units   = ow$units,
             rate        = tg$units / ow$units,
             pub_mean    = tg$mean_income,
             own_mean    = ow$mean_income,
             mean_ratio  = tg$mean_income / ow$mean_income,
             characteristic = names(tg$shares),
             share_ratio = unname(tg$shares / ow$shares[names(tg$shares)]))
}))

lvl <- unique(cal[, .(year, pub_units, own_units, rate, pub_mean, own_mean,
                      mean_ratio)])
message('  level rate N*/above-threshold: ',
        paste(sprintf('%d %.4f', lvl$year, lvl$rate), collapse = '  '))
message('  mean-income ratio            : ',
        paste(sprintf('%d %.3f', lvl$year, lvl$mean_ratio), collapse = '  '))
message(sprintf('  the mean-income ratio varies by %.1f%% across the three years',
                100 * sd(lvl$mean_ratio) / mean(lvl$mean_ratio)))

#-------------------------------------------------------------------------------
# Project
#-------------------------------------------------------------------------------
out <- rbindlist(lapply(TARGET_YEARS, function(y) {
  ow <- own_above_threshold(y)

  r  <- carry(lvl$rate,       lvl$year, y)
  m  <- carry(lvl$mean_ratio, lvl$year, y)

  # The level trends and is used trended; the mean-income ratio is flat to
  # within 2% and is HELD, because fitting a line to three near-identical
  # numbers extrapolates noise.
  rate_used <- r$trend
  mean_used <- m$hold

  sh <- rbindlist(lapply(CHARACTERISTICS, function(c_) {
    v <- cal[characteristic == c_][order(year)]
    k <- carry(v$share_ratio, v$year, y)
    # Trend only where the RATIO moves monotonically across all three years;
    # otherwise hold. A non-monotone three-point series is noise, not a trend.
    ratio_used <- if (k$monotone) k$trend else k$hold
    q <- ratio_used * ow$shares[[c_]]
    data.table(characteristic = c_,
               share_ratio    = ratio_used,
               method         = if (k$monotone) 'trend' else 'hold_last',
               value          = min(max(q, 1e-4), 0.9999))
  }))
  if (any(sh$value >= 0.9999 | sh$value <= 1e-4)) {
    warning(sprintf('TY%d: a projected share hit its bound and was clipped', y),
            call. = FALSE)
  }

  message(sprintf(paste('  TY%d: rate %.4f (hold %.4f) x %.2fM above-threshold',
                        '= %.2fM units | mean $%s (ratio %.3f)%s'),
                  y, rate_used, r$hold, ow$units / 1e6,
                  rate_used * ow$units / 1e6,
                  format(round(mean_used * ow$mean_income), big.mark = ','),
                  mean_used,
                  if (r$frozen)
                    sprintf(' | BEYOND +%d yrs: reverted to the last OBSERVED rate (%d)',
                            TREND_HORIZON, r$eval_at) else ''))

  rbind(
    data.table(tax_year = y, component = 'units',
               value = rate_used * ow$units,
               method = if (r$frozen) 'hold_last_observed' else 'trend',
               rate = rate_used, own_base = ow$units,
               alt_hold = r$hold * ow$units),
    data.table(tax_year = y, component = 'mean_income',
               value = mean_used * ow$mean_income,
               method = 'hold_last',
               rate = mean_used, own_base = ow$mean_income,
               alt_hold = m$trend * ow$mean_income),
    sh[, .(tax_year = y, component = paste0('share_', characteristic),
           value, method, rate = share_ratio,
           own_base = ow$shares[characteristic], alt_hold = NA_real_)]
  )
}))

fwrite(out, file.path(RES, 'pub5785_projected_targets.csv'))
message('  wrote pub5785_projected_targets.csv (', nrow(out), ' rows)')

#-------------------------------------------------------------------------------
# What changed against the average that was being used
#-------------------------------------------------------------------------------
avg <- pub5785_targets(NULL)
message('\n=== against the TY2014-16 average previously applied to every year')
for (y in TARGET_YEARS) {
  u <- out[tax_year == y & component == 'units', value]
  m <- out[tax_year == y & component == 'mean_income', value]
  message(sprintf(paste('  TY%d units %.2fM vs %.2fM (%+.1f%%) | mean income',
                        '$%s vs $%s (%+.1f%%)'),
                  y, u / 1e6, avg$units / 1e6, 100 * (u / avg$units - 1),
                  format(round(m), big.mark = ','),
                  format(round(avg$mean_income), big.mark = ','),
                  100 * (m / avg$mean_income - 1)))
}

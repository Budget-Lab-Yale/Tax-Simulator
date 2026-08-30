#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 14_nonfiler_weight_targets.R  (group E-prep stage 1, the S18(b) series)
#
# Tax-Data ages non-filer weights by demographic population factors alone,
# which holds the non-filing rate constant inside every cell. The observed
# residual count of non-filing adults did nothing of the kind -- 48.5M (2017),
# 40.8M (2020), 46.5M (2021) -- and we now hold it for every year 2014-2023.
# S18(b): target the OBSERVED counts through the last observed year, then
# phase into the demographic target rather than switching at the seam.
#
# This script produces the series Tax-Data consumes: one growth factor per
# (band, year), cumulative from the 2017 base file, 2018-2097.
#
#   2018..T        factor_b(y) = R_b(y) / R_b(2017), R_b observed
#                  (national_anchor_{y}.csv, band residuals)
#   T+1..T+K       R_b(y) = N_b(y) * s_b(y), where the residual SHARE of band
#                  adults moves linearly from its observed value at T to its
#                  pre-pandemic norm (mean of 2017-2019):
#                  s_b(y) = s_b(T) + (sbar_b - s_b(T)) * (y - T)/K
#   beyond T+K     R_b(y) = N_b(y) * sbar_b   (pure demographic target)
#
# WHY THE PHASE-IN TARGETS THE NORM rather than freezing s_b(T): T's own level
# may be transitorily depressed or elevated (2023 sits one year after the
# stimulus-filing reversion), and locking it in forever would project a
# possibly-transient filing-rate level to 2097. Decaying to the 2017-2019 norm
# says "shocks wash out"; K controls how fast. Both K = 10 (primary) and K = 5
# are emitted as columns so the choice stays visible.
#
# N_b comes from the Macro-Projections demographic cells -- verified this
# session to BE CBO's Social Security area population -- so the projection
# side of this series and the rest of Tax-Data's aging share one demography.
# R_b is PEP-based (the anchor basis); the s_b ratio therefore straddles the
# ~1.2% PEP-vs-ssArea universe wedge. That is absorbed, deliberately: the
# factors are RATIOS anchored at 2017, so a stable wedge cancels, and the
# handoff-year alignment (S19) is where the wedge itself is dealt with.
#
# ASSUMPTION carried to Tax-Data: the emitted pool's weights sum to the
# residual NET of claimed-dependent netting; scaling them by the growth of the
# GROSS band residual assumes the netting share within band is stable in time.
#
# Writes: results/nonfiler_weight_targets.csv
#         (band, year, factor_phase10, factor_phase5, source)
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/14_nonfiler_weight_targets.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readr); library(yaml)
})
source('src/data/state_weights.R')

RES        <- 'research/state_weights/nonfiler_pool/results'
ANCH       <- 'research/state_weights/nonfiler_residual/results'
BASE_YEAR  <- 2017L
LAST_OBS   <- 2023L          # last year with a built national anchor
NORM_YEARS <- 2017:2019      # the pre-pandemic share norm
END_YEAR   <- 2097L          # Tax-Data's projection horizon
BANDS      <- c('18_25', '26_34', '35_44', '45_54', '55_64', '65p')

MACRO_VINTAGE <- c(model = 'Macro-Projections', version = 'v3', vintage = '2026071916')
MACRO <- model_data_path(MACRO_VINTAGE[['model']], MACRO_VINTAGE[['version']],
                         MACRO_VINTAGE[['vintage']], 'baseline')

#-------------------------------------------------------------------------------
# Observed band residuals, every built anchor year
#-------------------------------------------------------------------------------
obs <- rbindlist(lapply(BASE_YEAR:LAST_OBS, function(y) {
  f <- file.path(ANCH, sprintf('national_anchor_%d.csv', y))
  if (!file.exists(f)) {
    stop(sprintf('no national anchor for %d -- the observed series must be
  complete from %d to %d before a projection hangs off its end', y, BASE_YEAR,
                 LAST_OBS), call. = FALSE)
  }
  d <- fread(f)[band != 'total_18p']
  stopifnot(identical(sort(d$band), sort(BANDS)))
  d[, .(year = y, band, residual = residual_nonfiling_adults)]
}))

#-------------------------------------------------------------------------------
# Band adults from the Macro-Projections demography (== CBO ssArea)
#-------------------------------------------------------------------------------
macro <- rbindlist(list(
  as.data.table(read_csv(file.path(MACRO, 'historical.csv'),  show_col_types = FALSE)),
  as.data.table(read_csv(file.path(MACRO, 'projections.csv'), show_col_types = FALSE))
), fill = TRUE)
cells <- melt(macro[, c('year', grep('^(un)?married_[0-9]+$', names(macro),
                                     value = TRUE)), with = FALSE],
              id.vars = 'year', variable.name = 'k', value.name = 'n')
cells[, age := as.integer(sub('.*_', '', as.character(k)))]
nb <- cells[age >= 18 & year >= BASE_YEAR & year <= END_YEAR,
            .(adults = sum(n)), by = .(year, band = as.character(a16_band(age)))]
stopifnot(!anyNA(nb$adults), all(BANDS %in% nb$band))

#-------------------------------------------------------------------------------
# Shares: observed path, its norm, and the phased projection
#-------------------------------------------------------------------------------
sh <- merge(obs, nb, by = c('year', 'band'))
sh[, s := residual / adults]
norm <- sh[year %in% NORM_YEARS, .(sbar = mean(s)), by = band]
sT   <- sh[year == LAST_OBS, .(band, sT = s)]

message('residual share of band adults, observed at T and the 2017-19 norm:')
for (b in BANDS) {
  message(sprintf('  %-6s s(%d) = %.4f | norm = %.4f | gap %+.4f',
                  b, LAST_OBS, sT[band == b, sT], norm[band == b, sbar],
                  sT[band == b, sT] - norm[band == b, sbar]))
}

proj <- CJ(year = (LAST_OBS + 1L):END_YEAR, band = BANDS)
proj <- merge(proj, norm, by = 'band')
proj <- merge(proj, sT, by = 'band')
proj <- merge(proj, nb, by = c('year', 'band'))
for (K in c(10L, 5L)) {
  lam <- pmin(1, (proj$year - LAST_OBS) / K)
  proj[, (sprintf('residual_phase%d', K)) :=
         adults * (sT + (sbar - sT) * lam)]
}

#-------------------------------------------------------------------------------
# Factors, cumulative from the base year
#-------------------------------------------------------------------------------
base <- obs[year == BASE_YEAR, .(band, r0 = residual)]
out <- rbind(
  merge(obs[year > BASE_YEAR], base, by = 'band')[
    , .(band, year, factor_phase10 = residual / r0,
        factor_phase5 = residual / r0, source = 'observed')],
  merge(proj, base, by = 'band')[
    , .(band, year, factor_phase10 = residual_phase10 / r0,
        factor_phase5 = residual_phase5 / r0, source = 'projected')]
)
setorder(out, year, band)
stopifnot(!anyNA(out$factor_phase10), all(out$factor_phase10 > 0),
          nrow(out) == length(BANDS) * (END_YEAR - BASE_YEAR))

fwrite(out, file.path(RES, 'nonfiler_weight_targets.csv'))
message(sprintf('wrote nonfiler_weight_targets.csv (%d rows, %d-%d)',
                nrow(out), BASE_YEAR + 1L, END_YEAR))
message('total non-filing adults implied (phase-10), selected years:')
tot <- merge(out, base, by = 'band')[, .(M = sum(factor_phase10 * r0) / 1e6), by = year]
for (y in c(2018, 2020, 2023, 2025, 2028, 2033, 2050)) {
  message(sprintf('  %d  %.2fM%s', y, tot[year == y, M],
                  if (y <= LAST_OBS) '  (observed)' else ''))
}

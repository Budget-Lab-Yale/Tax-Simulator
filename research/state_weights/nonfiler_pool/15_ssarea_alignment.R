#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 15_ssarea_alignment.R  (group E-prep stage 2, decision S19)
#
# The handoff-year basis alignment. Two mismatches sat between the filer and
# non-filer sides of the projection:
#
#   UNITS     filer targets are RETURNS, the non-filer anchor is ADULTS
#   UNIVERSE  the anchor is Census PEP resident population; the demography
#             that carries every weight forward is CBO's Social Security area
#             population (verified this session to BE Macro-Projections'
#             married_{age} cells) -- ~1.2% apart
#
# Because ages are frozen and every cell is scaled by N_c(y)/N_c(T), a
# partition that holds CELL BY CELL at the handoff year T stays exact under
# aging forever after. This script builds that partition at T = 2023 on the
# ssArea universe (S19: ssArea at the handoff year only; 2014-2022 anchors and
# all state work stay PEP), in adults:
#
#   filing adults + non-filer target + claimed-dependent netting = ssArea adults
#
# THE CELL IS THE AGE BAND, asserted; the marital split is PUBLISHED BUT NOT
# ASSERTED. Two reasons, and the second is a finding. First, the band is what
# aging actually preserves: the non-filer weight path scales by band (commit 2
# of the Tax-Data branch), so band-level closure at T is the exact condition
# for the identity to self-preserve. Second, a band x married partition is NOT
# IDENTIFIABLE from T1.6: it assigns a joint return's two adults to the
# PRIMARY's band while the population counts each adult at their own age, and
# the collision is not theoretical -- the unmarried 35_44 cell's slack
# (ssArea minus filing adults, 0.96M) is SMALLER than that band's claimed-
# dependent netting, so forcing the marital split produces a negative
# non-filer target in a cell that plainly has non-filers. The convention wedge
# the anchor documentation warns about, seen in the arithmetic.
#
# Products:
#   ssarea_alignment_2023.csv   the band partition (asserted) with the marital
#                               columns (informational), plus the per-band
#                               scale the pool needs to close on ssArea
#   ssarea_wedge_2023.csv       PEP vs ssArea residual per band -- the wedge,
#                               named rather than absorbed
#
# Cell conventions, stated:
#   * T1.6 assigns a joint return's two adults to the PRIMARY's band (its own
#     footnote); MFJ and MFS adults are married, single and HoH unmarried.
#     QSS rides inside the MFJ block (the T1.6 fold) and is treated married --
#     ~0.08M, immaterial at cell scale.
#   * The S15 level correction (out-of-state + QSS double-count, carried at
#     0.59% for 2023) is applied pro rata across cells, as the anchor does.
#   * Claimed adult dependents net out as UNMARRIED in their own band -- a
#     married claimed dependent is possible but cannot be identified, and the
#     pool's own netting is >97% unmarried.
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/15_ssarea_alignment.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(readxl); library(yaml)
})
source('src/data/state_weights.R')

RES  <- 'research/state_weights/nonfiler_pool/results'
ANCH <- 'research/state_weights/nonfiler_residual/results'
T_YEAR <- 2023L
BANDS  <- c('18_25', '26_34', '35_44', '45_54', '55_64', '65p')
CBO_SSAREA <- file.path('/nfs/roberts/project/pi_nrs36/shared/raw_data',
                        'CBO-Demographic-Projections',
                        '57059-2026-01-Demographic-Projections/CSV Files',
                        'ssArea_pop_byYearAgeMaritalSex.csv')

#-------------------------------------------------------------------------------
# ssArea adults by band x married, at T
#-------------------------------------------------------------------------------
cbo <- fread(CBO_SSAREA)[year == T_YEAR]
cbo[, age_i := suppressWarnings(as.integer(sub('\\+', '', age)))]
cbo[age == '100+', age_i := 100L]
stopifnot(!anyNA(cbo$age_i))
ss <- cbo[age_i >= 18,
          .(ss_adults = sum(number_of_people)),
          by = .(band = as.character(a16_band(age_i)),
                 married = marital == 'married')]

#-------------------------------------------------------------------------------
# Filing adults by band x married, T1.6, with the carried S15 correction
#-------------------------------------------------------------------------------
t16 <- read_pub1304_t16(T_YEAR)
fa <- t16[block != 'all' & band != 'u18',
          .(filing_adults = sum(n_returns * fifelse(block == 'mfj', 2, 1))),
          by = .(band, married = block %in% c('mfj', 'mfs'))]
u18 <- t16[block == 'all' & band == 'u18', sum(n_returns)]
fa[band == '18_25' & married == FALSE, filing_adults := filing_adults - u18]

# the anchor's corrected level, recovered from its own file so the two cannot
# drift: correction fraction = 1 - corrected / published
anchor <- fread(file.path(ANCH, sprintf('national_anchor_%d.csv', T_YEAR)))
f_corr <- 1 - anchor[band != 'total_18p', sum(filing_adults)] / fa[, sum(filing_adults)]
stopifnot(f_corr > 0.004, f_corr < 0.009)
fa[, filing_adults := filing_adults * (1 - f_corr)]

#-------------------------------------------------------------------------------
# The pool's side at T: emitted non-filing adults and the dependent netting
#-------------------------------------------------------------------------------
pool <- fread(file.path(RES, sprintf('nonfiler_pool_%d.csv.gz', T_YEAR)))
emit <- pool[, .(emitted = sum(weight * (1 + (filing_status == 2)))),
             by = .(band = as.character(a16_band(pmax(18L, as.integer(age1)))),
                    married = filing_status == 2)]

st <- readRDS(file.path(RES, sprintf('units_%d.rds', T_YEAR)))
u  <- readRDS(file.path(RES, sprintf('calibrated_units_%d.rds', T_YEAR)))
deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                   .(SERIAL, PERNUM, AGE, ASECWT,
                     unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
deps <- merge(deps, u[unit_type == 'dependent', .(unit_id, p_file_cal)],
              by = 'unit_id', all.x = TRUE)
deps[is.na(p_file_cal), p_file_cal := 0]
net <- deps[, .(netting = sum(ASECWT * (1 - p_file_cal))),
            by = .(band = as.character(a16_band(AGE)))]
net[, married := FALSE]

#-------------------------------------------------------------------------------
# The partition on ssArea: BAND level asserted, marital informational
#-------------------------------------------------------------------------------
by_band <- function(d, v) d[, setNames(.(sum(get(v))), v), by = band]
cells <- Reduce(function(a, b) merge(a, b, by = 'band', all = TRUE),
                list(by_band(ss, 'ss_adults'), by_band(fa, 'filing_adults'),
                     by_band(emit, 'emitted'), by_band(net, 'netting')))
stopifnot(!anyNA(cells))

cells[, nonfiler_target := ss_adults - filing_adults - netting]
cells[, scale_to_ssarea := nonfiler_target / emitted]

# informational marital columns: the split T1.6 CAN express (primary-band
# convention), published so the reader sees both it and why it is not asserted
mar <- merge(ss[married == TRUE, .(band, ss_married = ss_adults)],
             fa[married == TRUE, .(band, filing_married = filing_adults)],
             by = 'band')
cells <- merge(cells, mar, by = 'band')

# Feasibility and closure at the BAND -- the cell aging actually scales. A
# negative target would say the universe holds fewer adults than its own
# filers, which is arithmetic, not judgement. Closure is by construction;
# assert it anyway so a later edit that breaks the construction announces
# itself.
stopifnot(all(cells$nonfiler_target > 0),
          all(abs(cells[, filing_adults + nonfiler_target + netting - ss_adults]) < 1))

fwrite(cells[match(BANDS, band)],
       file.path(RES, sprintf('ssarea_alignment_%d.csv', T_YEAR)))

#-------------------------------------------------------------------------------
# The wedge, named: PEP residual vs ssArea residual per band
#-------------------------------------------------------------------------------
wedge <- merge(anchor[band != 'total_18p',
                      .(band, pep_adults, residual_pep = residual_nonfiling_adults)],
               cells[, .(band, ss_adults,
                         residual_ssarea = nonfiler_target + netting)],
               by = 'band')
wedge[, `:=`(adults_wedge_pct   = 100 * (ss_adults / pep_adults - 1),
             residual_wedge_pct = 100 * (residual_ssarea / residual_pep - 1))]
fwrite(wedge[match(BANDS, band)], file.path(RES, sprintf('ssarea_wedge_%d.csv', T_YEAR)))

#-------------------------------------------------------------------------------
# Report
#-------------------------------------------------------------------------------
message(sprintf('=== TY%d handoff partition on ssArea (S19)', T_YEAR))
message(sprintf('  ssArea adults %.2fM | filing %.2fM | non-filer target %.2fM | netting %.2fM',
                cells[, sum(ss_adults)] / 1e6, cells[, sum(filing_adults)] / 1e6,
                cells[, sum(nonfiler_target)] / 1e6, cells[, sum(netting)] / 1e6))
message(sprintf('  pool emitted (PEP basis) %.2fM -> per-band scale to ssArea: %.3f to %.3f',
                cells[, sum(emitted)] / 1e6,
                cells[, min(scale_to_ssarea)], cells[, max(scale_to_ssarea)]))
message('  the wedge, per band (ssArea vs PEP):')
for (i in seq_len(nrow(wedge))) {
  w <- wedge[match(BANDS, band)][i]
  message(sprintf('    %-6s adults %+5.2f%% | residual %+6.2f%%',
                  w$band, w$adults_wedge_pct, w$residual_wedge_pct))
}
message('  wrote ssarea_alignment_', T_YEAR, '.csv, ssarea_wedge_', T_YEAR, '.csv')

#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 13_filer_return_counts.R  (group E-prep stage 1)
#
# Tax-Data ages filer weights on observed IRS return counts only through 2019
# (resources/return_counts_2019.csv), then switches to demographic population
# factors -- which hold the filing rate constant inside each cell. The observed
# rate was anything but constant: returns per adult ran .5991 (2018), .6101
# (2019), .6314 (2020, the stimulus-filing spike), .6142, .6118, and .6008 in
# 2023 -- fully reverted to the pre-pandemic norm. A demographic path from 2019
# misses the whole excursion, and a base year inside it (2020-2022) would bake
# part of the spike into every projected year. 2023 is the year to hand off.
#
# This script builds the successor file, resources/return_counts_2023.csv, in
# the identical (filing_status, age_group) x year shape, from the same
# published table the 2019 file came from: Pub 1304 Table 1.6 (returns by
# marital status x age), parsed by read_pub1304_t16() -- the same reader the
# residual anchors are built on, so the filer targets and the non-filer anchor
# share one source and one parse.
#
# Mapping (verified against the existing file, which must REPRODUCE):
#   block  mfj (incl. surviving spouses) -> filing_status 2
#          mfs -> 3, hoh -> 4, single -> 1
#   band   u18 + 18_25 -> age_group 1 (project_puf: age1 < 26), 26_34 -> 2,
#          35_44 -> 3, 45_54 -> 4, 55_64 -> 5, 65p -> 6
#
# Writes: results/return_counts_2023.csv  (copied to Tax-Data resources/ on
#         the asec-nonfiler-pool branch)
#
# Login-node safe, seconds.
#   Rscript research/state_weights/nonfiler_pool/13_filer_return_counts.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(readxl); library(yaml)
})
source('src/data/state_weights.R')

RES   <- 'research/state_weights/nonfiler_pool/results'
YEARS <- 2017:2023
LEGACY <- file.path('/nfs/roberts/project/pi_nrs36/ji252/repos/Tax-Data',
                    'resources/return_counts_2019.csv')

BLOCK_TO_STATUS <- c(mfj = 2L, qss = 2L, mfs = 3L, hoh = 4L, single = 1L)
BAND_TO_GROUP   <- c(u18 = 1L, `18_25` = 1L, `26_34` = 2L, `35_44` = 3L,
                     `45_54` = 4L, `55_64` = 5L, `65p` = 6L)

counts <- rbindlist(lapply(YEARS, function(y) {
  t16 <- read_pub1304_t16(y)
  d <- t16[block != 'all']
  d[, `:=`(filing_status = BLOCK_TO_STATUS[block],
           age_group     = BAND_TO_GROUP[band])]
  stopifnot(!anyNA(d$filing_status), !anyNA(d$age_group))
  out <- d[, .(n = sum(n_returns)), by = .(filing_status, age_group)]
  stopifnot(nrow(out) == 24L)
  # the 24 cells must partition the block totals they came from
  stopifnot(abs(out[, sum(n)] - d[, sum(n_returns)]) < 0.5)
  out[, year := y]
  out
}))

wide <- dcast(counts, filing_status + age_group ~ year, value.var = 'n')

#-------------------------------------------------------------------------------
# Gate: the 2017-2019 columns must REPRODUCE the file Tax-Data has been
# running on. Anything else means the parse or the mapping differs from
# however that file was built, and the difference has to be named before the
# extension can be trusted.
#-------------------------------------------------------------------------------
legacy <- fread(LEGACY)
chk <- merge(wide[, .(filing_status, age_group, `2017`, `2018`, `2019`)],
             legacy, by = c('filing_status', 'age_group'),
             suffixes = c('_new', '_old'))
stopifnot(nrow(chk) == 24L)
for (y in c('2017', '2018', '2019')) {
  gap <- chk[[paste0(y, '_new')]] - chk[[paste0(y, '_old')]]
  if (any(abs(gap) > 0.5)) {
    bad <- chk[abs(gap) > 0.5]
    stop(sprintf('%s does not reproduce return_counts_2019.csv in %d cell(s), first: status %d group %d (new %s vs old %s)',
                 y, nrow(bad), bad$filing_status[1], bad$age_group[1],
                 format(bad[[paste0(y, '_new')]][1], big.mark = ','),
                 format(bad[[paste0(y, '_old')]][1], big.mark = ',')),
         call. = FALSE)
  }
}
message('gate: 2017-2019 reproduce return_counts_2019.csv exactly, all 72 cells')

#-------------------------------------------------------------------------------
# Report the series the extension exists to capture
#-------------------------------------------------------------------------------
tot <- counts[, .(returns = sum(n)), keyby = year]
message('total returns (M): ',
        paste(sprintf('%d %.2f', tot$year, tot$returns / 1e6), collapse = '  '))
message(sprintf(paste('2020 spike and reversion: 2019 %.2fM -> 2020 %.2fM',
                      '(%+.1f%%) -> 2023 %.2fM'),
                tot[year == 2019, returns] / 1e6, tot[year == 2020, returns] / 1e6,
                100 * (tot[year == 2020, returns] / tot[year == 2019, returns] - 1),
                tot[year == 2023, returns] / 1e6))

setorder(wide, filing_status, age_group)
# match the legacy file's row order (status 2, 3, 4, 1) so diffs read cleanly
wide <- wide[order(match(filing_status, c(2L, 3L, 4L, 1L)), age_group)]
fwrite(wide, file.path(RES, 'return_counts_2023.csv'))
message('wrote return_counts_2023.csv (24 rows x ', length(YEARS), ' years)')

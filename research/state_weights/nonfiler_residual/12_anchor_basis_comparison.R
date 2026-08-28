#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 12_anchor_basis_comparison.R  (JI, 2026-08-27)
#
# The state residual anchors and the national residual anchor are built on
# DIFFERENT sources for the same quantity, and nothing reconciles them:
#
#   national_anchor_{year}.csv   filing adults from Pub 1304 Table 1.6
#   residual_anchors_{year}.csv  filing adults from the HT2 identities
#
# So the 51 state anchors do not sum to the national anchor -- by -1.4% in
# TY2017 and +2.1% in TY2022, with the sign FLIPPING between the two anchor
# years. The fit targets the state file; the plan quotes the national one.
#
# This script does two things and changes nothing:
#
#   PART A  Decomposes the national gap between the two constructions into
#           named components, instead of carrying it as a 0.3-0.5% "the two
#           SOI routes disagree" constant. Two universe differences are
#           identifiable and pull in OPPOSITE directions:
#             + HT2 drops the out-of-state buckets (OA, PR) that Pub 1304
#               counts, so the HT2 route is short by that much;
#             - HT2 cannot exclude under-18 filers (it has no age), so its
#               subtrahend contains minors that PEP's 18+ numerator does not,
#               and the residual is understated by that much.
#
#   PART B  Builds the state anchors under three bases and reports the
#           per-state difference against each state's own tolerance (script
#           08), so the choice is made against a measured spread:
#             A   current: pep_st - ht2_filing_adults_st
#             B1  naive proposal: T1.6 national level x HT2 state share
#             B2  universe-matched: (T1.6 level - out-of-state) x HT2 share
#           A and B1 differ by a UNIFORM scale factor on the subtrahend, so
#           the per-state effect is proportional to filing/residual -- largest
#           exactly where the non-filer share is smallest, which is the same
#           amplification structure the tolerance has. Whether the two offset
#           is the question this reports rather than assumes.
#
# Login-node safe (reads two CSVs, one HT2 file and one .xls per year).
#   Rscript research/state_weights/nonfiler_residual/12_anchor_basis_comparison.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(stringr)
  library(yaml); library(readxl)
})
source('src/data/state_weights.R')

ANCHOR_YEARS <- c(2017, 2022)
RESULTS <- 'research/state_weights/nonfiler_residual/results'

#' Filing adults 18+ from Pub 1304 Table 1.6, with the components named.
#' Reproduces 02_build_residual_anchors.R exactly: 2 adults per joint return
#' (primary's band), 1 otherwise; under-18 filers -- published only in the
#' all-returns block, and sitting inside the status blocks' "Under 26" rows --
#' subtracted from the 18-25 band.
t16_filing_adults <- function(year) {
  t16 <- read_pub1304_t16(year)
  fa <- t16[block != 'all' & band != 'u18',
            sum(n_returns * fifelse(block == 'mfj', 2, 1))]
  u18 <- t16[block == 'all' & band == 'u18', sum(n_returns)]
  # T1.6 publishes FOUR status blocks, and the joint one is titled "Returns of
  # married persons filing jointly AND RETURNS OF SURVIVING SPOUSES" -- verified
  # against the sheet 2026-08-27. So QSS is folded into mfj here and in Table
  # 1.2, is separately published nowhere, and is counted as TWO adults by the
  # line above when a surviving spouse files alone. MFS, by contrast, is its own
  # block. Both facts are what Part C uses.
  stopifnot(setequal(unique(t16$block), c('all','mfj','mfs','hoh','single')))
  list(filing_adults_18p = fa - u18,
       u18_filers        = u18,
       mfs_returns       = t16[block == 'mfs', sum(n_returns)])
}

out_all <- list()

for (yr in ANCHOR_YEARS) {

  message('=== TY', yr)

  anchors <- fread(file.path(RESULTS, sprintf('residual_anchors_%d.csv', yr)))
  tol     <- fread(file.path(RESULTS, sprintf('residual_tolerance_%d.csv', yr)))
  stopifnot(nrow(anchors) == 51)

  ht2 <- read_ht2(ht2_path(yr), yr)
  t16 <- t16_filing_adults(yr)

  # HT2 route, split by universe. ht2_filing_persons() drops the out-of-state
  # buckets by default; asking for them explicitly is what makes the wedge a
  # measurement rather than an omission.
  fa_states_full <- ht2_filing_persons(ht2)
  fa_states <- fa_states_full[
    , .(state, fa = married_filing_adults + single_filing_adults)]
  oos <- ht2_filing_persons(ht2, states = NONTAX_BUCKETS)
  fa_oos <- if (nrow(oos)) oos[, sum(married_filing_adults + single_filing_adults)] else 0

  # The state anchors must be the same HT2 tabulation script 02 wrote, or the
  # comparison is against a different file than the one the fit targets.
  chk <- merge(anchors[, .(state, fa_file = filing_adults_ht2)], fa_states, by = 'state')
  stopifnot(nrow(chk) == 51, max(abs(chk$fa_file - chk$fa)) < 1)

  A  <- fa_states[, sum(fa)]          # HT2 identity, 51 states
  D  <- t16$filing_adults_18p         # Pub 1304 T1.6, all returns, 18+ only

  #-------------------------------------------------------------------------
  # PART A -- decompose the national gap
  #-------------------------------------------------------------------------
  comparable <- A + fa_oos - t16$u18_filers
  message(sprintf('  PART A  filing adults, TY%d', yr))
  message(sprintf('    HT2 identity, 51 states            %10.3fM', A / 1e6))
  message(sprintf('    + out-of-state buckets (%s)%s   %10.3fM',
                  paste(sort(oos$state), collapse = '/'),
                  strrep(' ', max(0, 8 - nchar(paste(sort(oos$state), collapse = '/')))),
                  fa_oos / 1e6))
  message(sprintf('    - T1.6 under-18 filers             %10.3fM', t16$u18_filers / 1e6))
  message(sprintf('    = comparable to T1.6               %10.3fM', comparable / 1e6))
  message(sprintf('    Pub 1304 T1.6, filing adults 18+   %10.3fM', D / 1e6))
  message(sprintf('    unexplained remainder              %10.3fM  (%+.3f%% of T1.6)',
                  (comparable - D) / 1e6, 100 * (comparable / D - 1)))
  message(sprintf('    raw gap before decomposition                   %+.3f%%',
                  100 * (D / A - 1)))

  # Hoisted from Part C: the QSS double-count is part of the level B2 uses, so
  # it has to be known before Part B can reproduce what script 02 implements.
  mfs_qss_51  <- fa_states_full[, sum(mfs_qss_returns)]
  mfs_qss_oos <- if (nrow(oos)) oos[, sum(mfs_qss_returns)] else 0
  qss_implied <- (mfs_qss_51 + mfs_qss_oos) - t16$mfs_returns

  #-------------------------------------------------------------------------
  # PART B -- the three bases
  #-------------------------------------------------------------------------
  st <- merge(anchors[, .(state, pep_adults_18p, fa_ht2 = filing_adults_ht2,
                          residual_A = residual_nonfiling_adults_ht2basis,
                          residual_implemented = residual_nonfiling_adults)],
              tol[state != 'US', .(state, tolerance_pct)], by = 'state')
  stopifnot(nrow(st) == 51)

  st[, share_ht2 := fa_ht2 / A]
  st[, `:=`(fa_B1 = D * share_ht2,
            fa_B2 = (D - fa_oos - qss_implied) * share_ht2)]
  st[, `:=`(residual_B1 = pep_adults_18p - fa_B1,
            residual_B2 = pep_adults_18p - fa_B2)]

  # The point of the change: state anchors sum to a national level by
  # construction. B1 to (PEP - T1.6); B2 to (PEP - T1.6 + out-of-state).
  pep_51 <- st[, sum(pep_adults_18p)]
  stopifnot(abs(st[, sum(residual_B1)] - (pep_51 - D)) < 1,
            abs(st[, sum(residual_B2)] - (pep_51 - (D - fa_oos - qss_implied))) < 1)

  # B2 is what script 02 now implements (S15), so this script has become a
  # standing regression check on the builder rather than a one-off comparison:
  # if 02 and this arithmetic ever disagree, one of them has drifted.
  stopifnot(isTRUE(all.equal(st$residual_B2, st$residual_implemented,
                             tolerance = 1e-6)))

  st[, `:=`(diff_B1_pct = 100 * (residual_B1 / residual_A - 1),
            diff_B2_pct = 100 * (residual_B2 / residual_A - 1))]
  st[, `:=`(B1_within_tol = abs(diff_B1_pct) <= tolerance_pct,
            B2_within_tol = abs(diff_B2_pct) <= tolerance_pct)]

  message(sprintf('  PART B  state anchors, three bases (TY%d)', yr))
  message(sprintf(paste('    total  A %8.3fM (historical)   B1 %8.3fM   B2 %8.3fM',
                        '<- IMPLEMENTED, and what national_anchor now carries'),
                  st[, sum(residual_A)] / 1e6, st[, sum(residual_B1)] / 1e6,
                  st[, sum(residual_B2)] / 1e6))
  # NOTE on reading the tolerance column now that B2 is implemented: the
  # tolerance file is regenerated ON the new residual, so "outside own
  # tolerance" measures how far the HISTORICAL basis sits from the current one.
  # It is a record of the size of the change, not a live warning.
  for (v in c('B1', 'B2')) {
    d <- st[[paste0('diff_', v, '_pct')]]
    w <- st[[paste0(v, '_within_tol')]]
    message(sprintf('    %s vs A: mean %+.2f%%  range %+.2f%% (%s) to %+.2f%% (%s)  |  %d of 51 beyond the current tolerance',
                    v, mean(d), min(d), st$state[which.min(d)],
                    max(d), st$state[which.max(d)], sum(!w)))
  }
  worst <- st[order(-abs(diff_B2_pct))][1:5]
  message('    largest B2 moves: ',
          paste(sprintf('%s %+.2f%% (tol %.2f%%)', worst$state,
                        worst$diff_B2_pct, worst$tolerance_pct), collapse = ', '))


  #-------------------------------------------------------------------------
  # PART C -- MFS and QSS: what each source can and cannot say
  #-------------------------------------------------------------------------
  # HT2's status residual is MFS + QSS (no MFS series exists anywhere in HT2).
  # T1.6 publishes MFS as its own block but folds QSS into joint. Netting one
  # off the other is the only route to QSS -- and QSS is also the size of the
  # double-count in `filing_adults`, since T1.6's joint block gives every
  # surviving-spouse return two adults when it carries one.
  #
  # The netting only works on a MATCHED universe, which is Part A's lesson
  # applied: T1.6 counts every filed return, so the HT2 side must include the
  # out-of-state buckets or the difference is contaminated by them.
  message(sprintf('  PART C  MFS/QSS, TY%d', yr))
  message(sprintf('    HT2 status residual (MFS+QSS), 51 states   %8.3fM returns', mfs_qss_51 / 1e6))
  message(sprintf('    + out-of-state buckets                     %8.3fM returns', mfs_qss_oos / 1e6))
  message(sprintf('    Pub 1304 T1.6 MFS, published               %8.3fM returns', t16$mfs_returns / 1e6))
  message(sprintf('    => implied QSS                             %8.3fM returns', qss_implied / 1e6))
  if (qss_implied < 0) {
    message('    NEGATIVE, so the two are not reconcilable as published: T1.6 MFS ',
            'exceeds the whole HT2 status residual. QSS cannot be recovered this ',
            'way and decision #5 cannot lean on it.')
  } else {
    message(sprintf('    QSS as 2 adults instead of 1 overstates T1.6 by %.3fM (%.3f%%), ',
                    qss_implied / 1e6, 100 * qss_implied / D),
            sprintf('closing %.0f%% of Part A\'s unexplained remainder',
                    100 * qss_implied / abs(comparable - D)))
  }

  st[, year := yr]
  f <- file.path(RESULTS, sprintf('anchor_basis_comparison_%d.csv', yr))
  fwrite(st[order(state)], f)
  message('  wrote ', f)
  out_all[[as.character(yr)]] <- st
}

message('\nDone. Both years written to ', RESULTS, '/anchor_basis_comparison_{year}.csv')

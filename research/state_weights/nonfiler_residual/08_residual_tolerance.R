#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 08_residual_tolerance.R  (Stage D, design memo §8 / todo P5)
#
# Compute the residual anchor's tolerance instead of picking it.
#
# The structural fact this exists for: the residual is a SMALL DIFFERENCE OF TWO
# LARGE NUMBERS. Non-filing adults are ~18% of adults, so the residual is
#
#     residual = PEP adults - filing adults
#
# with both inputs roughly 5x the answer. A 1% error in either input is therefore
# a ~5% error in the residual, and the amplification is worse in states where the
# non-filer share is smallest -- 9.4x in SD against 3.6x in MS. A flat tolerance
# would be wrong by ~2.6x across states, and wrong in the counter-intuitive
# direction: the states with the FEWEST non-filers need the WIDEST tolerance.
#
# Two measured input-error components, both taken from the data rather than
# assumed (see the constants below for provenance). They are combined in
# quadrature because they are independent: one is a return-side construction
# difference, the other a population-vintage difference.
#
# Login-node safe. Writes results/residual_tolerance_{year}.csv.
#   Rscript research/state_weights/nonfiler_residual/08_residual_tolerance.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table) })

ANCHOR_YEARS <- c(2017, 2022)
RESULTS <- 'research/state_weights/nonfiler_residual/results'

#-----------------------------------------------------------------------------
# Input-error components. MEASURED, not chosen -- update these if the underlying
# comparisons are re-run, and record what changed.
#-----------------------------------------------------------------------------

# Filing adults: the two independent SOI routes to the same quantity disagree.
# Pub 1304 Table 1.6 (returns x marital x age) vs the HT2 filing-status
# identities: -0.31% (TY2017), +0.47% (TY2022). Take the larger as a symmetric
# bound. Verified 04_findings.md §2 and re-measured 2026-08-19.
E_FILING_ADULTS <- 0.005

# Population: the PEP vintage current when SSA published vs the vintage in
# Census-PEP/. SSA OASDI-SC Table 1 gives 65+ population 57,794,852 for 2022
# against our 57,505,037 -- 0.50%. Measured 2026-08-19; see
# raw_data/SSA-OASDI-SC/NOTES.md §7.
E_PEP <- 0.005

#-----------------------------------------------------------------------------

for (yr in ANCHOR_YEARS) {
  # The anchors themselves, one file per year (script 02). NOT T5, which is the
  # v0-vs-anchor diagnostic and exists only for 2022 -- reading it for both years
  # silently stamped 2022's tolerances on the 2017 file (fixed 2026-08-19).
  f <- file.path(RESULTS, sprintf('residual_anchors_%d.csv', yr))
  if (!file.exists(f)) {
    stop('TY', yr, ': no ', basename(f), '. Run 02_build_residual_anchors.R first.')
  }
  anchors <- fread(f)

  stopifnot(all(c('state', 'pep_adults_18p', 'filing_adults',
                  'residual_nonfiling_adults') %in% names(anchors)),
            nrow(anchors) == 51)
  # A non-positive residual would make the amplification meaningless rather than
  # merely large; fail loudly instead of emitting a nonsense tolerance.
  stopifnot(all(anchors$residual_nonfiling_adults > 0))

  out <- anchors[, .(state,
                     pep_adults_18p, filing_adults, residual_nonfiling_adults)]
  # Amplification: the factor by which a proportional input error becomes a
  # proportional residual error. d(resid)/resid = (input/resid) * d(input)/input.
  out[, amp_pep     := pep_adults_18p / residual_nonfiling_adults]
  out[, amp_filing  := filing_adults  / residual_nonfiling_adults]
  out[, tolerance_pct := 100 * sqrt((amp_pep * E_PEP)^2 +
                                    (amp_filing * E_FILING_ADULTS)^2)]
  out[, tolerance_adults := residual_nonfiling_adults * tolerance_pct / 100]

  national <- data.table(
    state                     = 'US',
    pep_adults_18p            = sum(anchors$pep_adults_18p),
    filing_adults             = sum(anchors$filing_adults),
    residual_nonfiling_adults = sum(anchors$residual_nonfiling_adults))
  national[, amp_pep    := pep_adults_18p / residual_nonfiling_adults]
  national[, amp_filing := filing_adults  / residual_nonfiling_adults]
  national[, tolerance_pct := 100 * sqrt((amp_pep * E_PEP)^2 +
                                         (amp_filing * E_FILING_ADULTS)^2)]
  national[, tolerance_adults := residual_nonfiling_adults * tolerance_pct / 100]

  out <- rbind(national, out[order(state)])
  dest <- file.path(RESULTS, sprintf('residual_tolerance_%d.csv', yr))
  fwrite(out, dest)

  message(sprintf('TY%d: national +/-%.1f%% (+/-%.2fM); states %.1f%% (%s) to %.1f%% (%s)',
                  yr, national$tolerance_pct, national$tolerance_adults / 1e6,
                  min(out[state != 'US']$tolerance_pct),
                  out[state != 'US'][which.min(tolerance_pct), state],
                  max(out[state != 'US']$tolerance_pct),
                  out[state != 'US'][which.max(tolerance_pct), state]))
  message('  wrote ', dest)
}

message('\nNOT in this budget, deliberately:')
message('  - adult-dependent netting (~5.5M, 12% of the residual) is a BIAS to')
message('    remove in the estimate; only its estimation error belongs here.')
message('  - EEDATA 1% sampling error touches the covered-worker margin only.')
message('  - the ~17% ASEC income understatement touches the filing model, not')
message('    the anchor.')

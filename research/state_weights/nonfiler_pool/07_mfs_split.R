#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 07_mfs_split.R  (group C, stage C6 -- design note D-A3: "MFS is a calibrated
#                  post-step, never part of the estimation frame")
#
# WHAT THIS DOES AND DOES NOT AFFECT, stated first because the scope is
# narrower than the stage's name suggests and the reason is substantive:
#
#   Married-filing-separately is a FILING STATUS, so only units the model
#   scores as FILERS can have it. The emitted non-filer pool is therefore
#   UNCHANGED by this stage -- a non-filing couple has no filing status to
#   split, and the state-weights non-filer targets count ADULTS (F1a/D5), which
#   a split does not move. That is why C7 shipped before C6 rather than after.
#
#   What the split DOES produce: the filer-side status mix, which is the
#   remaining half of the C1 gate (our joint count is inflated by exactly the
#   couples who file separately), and the calibrated MFS share as a documented
#   parameter for the state-margin work in group F.
#
# METHOD (D-A3 stage 2). Split a calibrated share of joint FILER units into
# MFS pairs, targeted to SOI's published MFS return count (Pub 1304 T1.6:
# 3.213M TY2017, 3.993M TY2022). Two returns per split couple, so the couple
# count is half the return count.
#
# The split is applied as an EXPECTED-WEIGHT split, not a random draw: each
# joint filer unit contributes share s of its weight to MFS and (1-s) to
# joint. Deterministic, hits the target exactly, and consistent with the
# expected-weight convention C7 emits under. Uniform s, because nothing in
# the ASEC predicts who files separately -- the real drivers (liability
# separation, income-driven student-loan repayment, a separating couple whose
# survey record still shows both spouses) are unobserved here. Reported as a
# calibration, never as an observation, exactly as D-A3 requires.
#
# THE STATE DIMENSION IS OUT OF SCOPE HERE and is already answered elsewhere:
# the pool is a NATIONAL object (state allocation is the weights' job, group
# F), and decision Q3 settled that the HT2 status residual can carry the state
# distribution because it is 93%+ MFS -- surviving spouses are only 1.4-2.2%
# of it (0.084M / 0.056M returns, measured 2026-08-27).
#
# Writes: results/mfs_split_{year}.csv
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/07_mfs_split.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml); library(readxl)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'

# Mok's one empirical anchor on the split (design note D-A3): 12% of her
# constructed units that matched MULTIPLE 1040s were MFS. Different
# denominator from ours -- hers is couples where both spouses appear as a
# primary taxpayer somewhere, which is close to the set that DID split --
# so it bounds rather than targets. Recorded, not used as the target.
MOK_MULTI_1040_MFS_SHARE <- 0.12

out <- list()

for (yr in YEARS) {
  message('=== TY', yr)

  u   <- readRDS(file.path(RES, sprintf('calibrated_units_%d.rds', yr)))
  t16 <- read_pub1304_t16(yr)

  soi_mfs_returns <- t16[block == 'mfs', sum(n_returns)]
  soi_joint       <- t16[block == 'mfj', sum(n_returns)]   # includes QSS
  soi_hoh         <- t16[block == 'hoh', sum(n_returns)]
  soi_single      <- t16[block == 'single', sum(n_returns)]

  # Filer side, expected weights (the same convention C7 emits the pool under)
  nd <- u[unit_type == 'nondependent' & age_head >= 18]
  nd[, w_filer := weight * p_file_cal]
  dep_filers <- u[unit_type == 'dependent', sum(weight * p_file_cal)]

  joint_filers  <- nd[filing_status == 'joint',  sum(w_filer)]
  hoh_filers    <- nd[filing_status == 'hoh',    sum(w_filer)]
  single_filers <- nd[filing_status == 'single', sum(w_filer)]

  # The split: couples who file separately produce TWO returns each
  split_couples <- soi_mfs_returns / 2
  s <- split_couples / joint_filers
  stopifnot(s > 0, s < 1)

  joint_after  <- joint_filers - split_couples
  mfs_returns  <- split_couples * 2
  returns_after <- joint_after + mfs_returns + hoh_filers + single_filers + dep_filers

  message(sprintf('  joint filer couples %.2fM | SOI MFS %.3fM returns = %.3fM couples',
                  joint_filers / 1e6, soi_mfs_returns / 1e6, split_couples / 1e6))
  message(sprintf(paste('  calibrated split share s = %.4f of joint filers',
                        '(Mok multi-1040 anchor %.2f, different denominator)'),
                  s, MOK_MULTI_1040_MFS_SHARE))

  mix <- data.table(
    status = c('joint', 'mfs', 'hoh', 'single', 'dependent filers', 'TOTAL'),
    ours_before = c(joint_filers, 0, hoh_filers, single_filers, dep_filers,
                    joint_filers + hoh_filers + single_filers + dep_filers),
    ours_after  = c(joint_after, mfs_returns, hoh_filers, single_filers,
                    dep_filers, returns_after),
    soi = c(soi_joint, soi_mfs_returns, soi_hoh, soi_single, NA_real_,
            t16[block == 'all', sum(n_returns)]))
  mix[, ratio_after := ours_after / soi]

  message('  filer status mix (returns, millions):')
  for (i in seq_len(nrow(mix)))
    message(sprintf('    %-17s before %6.2f | after %6.2f | SOI %6.2f | ratio %s',
                    mix$status[i], mix$ours_before[i] / 1e6, mix$ours_after[i] / 1e6,
                    mix$soi[i] / 1e6,
                    fifelse(is.na(mix$ratio_after[i]), '   --',
                            sprintf('%.3f', mix$ratio_after[i]))))

  # The split moves the joint ratio toward SOI and adds the missing category;
  # it does NOT fix the total, which is the filing model's business.
  message(sprintf(paste('  joint ratio to SOI: %.3f -> %.3f | MFS now present',
                        'where it was structurally absent'),
                  joint_filers / soi_joint, joint_after / soi_joint))

  mix[, `:=`(tax_year = yr, split_share = s)]
  out[[as.character(yr)]] <- mix
  fwrite(mix, file.path(RES, sprintf('mfs_split_%d.csv', yr)))
  message('  wrote mfs_split_', yr, '.csv')
}

message('\nThe emitted pool is unchanged by this stage, by construction: a ',
        'non-filing couple has no filing status to split, and the non-filer ',
        'targets count adults.')

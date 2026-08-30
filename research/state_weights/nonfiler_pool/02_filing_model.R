#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 02_filing_model.R  (group C, stage C3 -- plan: research/state_weights/plan.md
#                     §3 step 3; method: nonfiler_residual_design.md §3.2.2)
#
# Score the filing model on the C1 units: Mok's fourteen probits below the
# filing threshold, the Pub 5785 hazard above it. Gates:
#
#   * scored group filing rates BESIDE Mok's TY2006 published rates -- context,
#     not equality: sixteen years of population change separate the two, and
#     C4's calibration owns the level. A rate outside [0,1] or a group order
#     inversion (married > unmarried, u65 > 65p) would be a construction bug.
#   * the hazard hits its 11.19M starting target exactly (it is solved for).
#   * the IMPLIED national non-filer level against the residual anchors --
#     the first uncalibrated look at the distance C4 has to close.
#
# DEFERRED, recorded rather than silent: the Cilke (1998) comparison fit the
# design memo wanted alongside. Five of his 24 covariates have no clean ASEC
# mapping in the current extract (labor-force status recodes, house_or_apt,
# public housing) -- mapping them approximately would produce a comparison of
# approximations, not of models. Needs its own pass before C4 closes, or a
# decision to drop it.
#
# Writes: results/scored_units_{year}.rds, results/filing_model_gates_{year}.csv
#
# Login-node safe.
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_weights/nonfiler_pool/02_filing_model.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')
source('src/data/filing_model.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'
ANCH  <- 'research/state_weights/nonfiler_residual/results'

mok <- read_mok_coefs()

for (yr in YEARS) {
  message('=== TY', yr)
  st <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  u <- add_mok_covariates(st$units, st$persons)
  tg <- pub5785_targets_for_year(yr)
  u <- score_filing_model(u, mok$coefs, tg)

  #---------------------------------------------------------------------------
  # Gate 1: group rates beside Mok's published TY2006 rates
  #---------------------------------------------------------------------------
  grp <- u[, .(units_M = sum(weight) / 1e6,
               p_file  = sum(weight * p_file) / sum(weight),
               p_mok_raw = sum(weight * p_file_mok) / sum(weight)),
           keyby = mok_group]
  grp[, mok_published_ty2006 := mok$filing_rates[mok_group]]
  grp <- grp[match(MOK_GROUPS, mok_group)]
  message('  scored filing rates (combined model) vs Mok TY2006 published:')
  for (i in seq_len(nrow(grp)))
    message(sprintf('    %-22s %6.2fM   %.3f  (Mok %.2f)',
                    grp$mok_group[i], grp$units_M[i], grp$p_file[i],
                    grp$mok_published_ty2006[i]))

  # construction-bug checks, not fit checks: ordering that Mok's own rates
  # obey must survive the transplant
  r <- setNames(grp$p_file, grp$mok_group)
  stopifnot(r['married_u65_dep0'] > r['unmarried_u65_dep0'],
            r['unmarried_u65_dep0'] > r['unmarried_65p_dep0'],
            r['dependent_u65'] < min(r[!startsWith(names(r), 'dependent')]),
            all(grp$p_file > 0 & grp$p_file < 1))

  #---------------------------------------------------------------------------
  # Gate 2: the hazard delivered its target
  #---------------------------------------------------------------------------
  above_nonfile <- u[must_file == TRUE, sum(weight * p_nonfile_hazard)]
  stopifnot(abs(above_nonfile - tg$units) < 1e3)
  message(sprintf(paste('  hazard: %.2fM above-threshold non-filing units',
                        '(target %.2fM on basis %s, solved); max p_nonfile %.3f'),
                  above_nonfile / 1e6, tg$units / 1e6, tg$basis,
                  u[must_file == TRUE, max(p_nonfile_hazard)]))

  #---------------------------------------------------------------------------
  # Gate 3: the implied national level against the anchors (uncalibrated)
  #---------------------------------------------------------------------------
  # Non-filing ADULTS among nondependent units: 1 adult for single/HoH heads,
  # 2 for joint. Dependent units never add adults here -- their people are
  # already counted through the units that claim them (and the anchor nets
  # claimed dependents out by C4's self-consistent netting; at THIS stage the
  # comparison is to the raw anchor, so state both).
  nd <- u[unit_type == 'nondependent']
  nd[, n_adults := fifelse(filing_status == 'joint', 2, 1)]
  implied_nonfiler_adults <- nd[, sum(weight * n_adults * (1 - p_file))]

  anch <- fread(file.path(ANCH, sprintf('national_anchor_%d.csv', yr)))
  target_raw <- anch[band == 'total_18p', residual_nonfiling_adults]

  # our own constructed netting quantity (C4's design choice): claimed adult
  # dependents, weighted by their own person weights
  own_adult_deps <- st$persons[is_dependent == TRUE & AGE >= 18, sum(ASECWT)]

  message(sprintf(paste('  implied non-filing adults (household frame,',
                        'uncalibrated): %.1fM'), implied_nonfiler_adults / 1e6))
  message(sprintf(paste('  anchor %.1fM raw | %.1fM net of our constructed',
                        'adult dependents (%.1fM) | ASEC frame lacks the',
                        '~8.2M GQ persons C5 adds'),
                  target_raw / 1e6,
                  (target_raw - own_adult_deps) / 1e6, own_adult_deps / 1e6))

  gates <- data.table(
    tax_year = yr, grp[, .(mok_group, units_M, p_file, mok_published_ty2006)],
    implied_nonfiler_adults_M = implied_nonfiler_adults / 1e6,
    anchor_raw_M = target_raw / 1e6,
    own_adult_deps_M = own_adult_deps / 1e6)
  fwrite(gates, file.path(RES, sprintf('filing_model_gates_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Gate 4: what the raking ACHIEVED against what it targeted (S17)
  #
  # The self-employment margin cannot be hit, and the reason is the ASEC's
  # joint distribution rather than the solver -- settled as S17, not a standing
  # question. Recording the residual per year per characteristic makes the size
  # of the accepted gap a committed number that moves with the data, instead of
  # a warning in a log nobody keeps. Every characteristic is written, not just
  # the failing one, so a NEW margin starting to drift is visible.
  #---------------------------------------------------------------------------
  ab <- u[must_file == TRUE]
  aw <- ab$weight; ap <- ab$p_nonfile_hazard
  has_c <- list(married  = ab$filing_status == 'joint',
                wages    = ab$src_wages == 1,
                se       = ab$src_self_employment == 1,
                interest = ab$src_interest == 1,
                dividends = ab$src_dividends == 1,
                pensions = ab$src_retirement == 1,
                ui       = ab$INCUNEMP > 0)
  marg <- rbindlist(lapply(names(tg$shares), function(c_) {
    data.table(tax_year        = yr,
               characteristic  = c_,
               target_share    = tg$shares[[c_]],
               achieved_share  = sum(aw[has_c[[c_]]] * ap[has_c[[c_]]]) / sum(aw * ap),
               own_pop_share   = sum(aw[has_c[[c_]]]) / sum(aw),
               basis           = tg$basis)
  }))
  marg[, residual_pp := 100 * (achieved_share - target_share)]
  fwrite(marg, file.path(RES, sprintf('hazard_margins_%d.csv', yr)))
  worst <- marg[which.max(abs(residual_pp))]
  message(sprintf(paste('  margins: worst %s %+.2fpp (target %.1f%%, achieved',
                        '%.1f%%, our population %.1f%%); the rest within %.2fpp'),
                  worst$characteristic, worst$residual_pp,
                  100 * worst$target_share, 100 * worst$achieved_share,
                  100 * worst$own_pop_share,
                  marg[characteristic != worst$characteristic,
                       max(abs(residual_pp))]))
  saveRDS(u, file.path(RES, sprintf('scored_units_%d.rds', yr)))
  message('  wrote scored_units_', yr, '.rds, filing_model_gates_', yr, '.csv')
}

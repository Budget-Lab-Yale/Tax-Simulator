#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 04_calibrate.R  (group C, stage C4 -- plan: research/state_weights/plan.md §3
#                  step 4; the anchors are S15's corrected basis)
#
# Joint calibration: band-level intercept shifts on the below-threshold probit
# index, solved together with the (held) Pub 5785 hazard against the residual
# anchors and the 7-band age shape. The parameterization, and why:
#
#   * SEVEN deltas, one per anchor age band, applied to the probit INDEX of
#     below-threshold nondependent household units -- Mok's slopes carry the
#     within-band composition, the deltas own the level. Band deltas rather
#     than the fourteen group constants because the anchor's information IS
#     seven band counts: fourteen group constants against seven constraints is
#     underdetermined, seven band deltas is exactly determined (one monotone
#     1-D root per band, no optimizer).
#   * the hazard scalar is held at its Pub 5785 solution (11.19M above-
#     threshold non-filing units) -- the anchor cannot separately identify the
#     above-threshold share, so the external level is the identifying
#     restriction, exactly as D3 intended.
#   * dependent-headed units keep their raw Mok scores: they enter the
#     accounting as NETTING (claimed dependents are not pool units), so
#     calibrating them would move both sides of the same equation.
#
# The accounting identity, per band b (all quantities adults 18+):
#
#   HH_below_b(delta_b) + HH_above_b + GQ_nonfiling_b
#     = anchor_b - dep_nonfiling_b
#
# anchor_b is the corrected-basis residual (S15); dep_nonfiling_b is OUR OWN
# constructed claimed-adult-dependent netting -- the self-consistent choice
# the plan records, with sensitivity runs at the external bracket edges
# (HT2 identity floor 5.58M; DEPSTAT 13.80M) reported per decision Q5.
#
# THERE IS DELIBERATELY NO SEPARATE DORM TERM, and the first version of this
# script had one -- the feasibility gate caught it (TY2022 band 18_25 went to
# a NEGATIVE need). The CPS counts dormitory students as members of their
# PARENTS' household -- the opposite of the ACS, which counts them at the
# dorm -- so the ASEC-constructed dependent netting already contains them,
# and an ACS dorm subtraction on top removed the same ~2.4M people twice.
# Nationally every person is counted once: dorm students through their
# parents' ASEC household, institutional and other GQ through the ACS
# backfill. The state-margin pipeline's dorm netting (B1/F1c) is a different
# object -- it corrects the MARGIN -- and is untouched by this.
#
# Inputs: scored_units_{year}.rds (02), units_{year}.rds (01, for persons),
#         gq_backfill_summary_{year}.csv (03),
#         nonfiler_residual/resources/nonfiler_age_shape_{year}.csv,
#         nonfiler_residual/results/residual_anchors_{year}.csv (dorm check)
# Writes: calibration_{year}.csv, netting_sensitivity_{year}.csv,
#         calibrated_units_{year}.rds
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/04_calibrate.R [year ...]
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
SHAPE <- 'research/state_weights/nonfiler_residual/resources'

# External bracket for the adult-dependent netting (decision Q5, measured
# 2026-08-27): the HT2 identity is a floor on return-claimed adult dependents,
# DEPSTAT is the survey benchmark. Our own constructed count is the central
# case; the edges scale it.
DEP_BRACKET <- list(`2017` = c(ht2_floor = 5.58e6, depstat = 13.80e6),
                    `2022` = c(ht2_floor = 5.58e6, depstat = 13.80e6))

#' Solve the seven band deltas for one netting scenario.
#'
#' Feasibility: the below-threshold arm must be able to supply
#' `target - fixed` non-filing adults -- between 0 (delta -> +Inf, everyone
#' files) and its full mass (delta -> -Inf, no one files). An infeasible band
#' is a FINDING about the netting or the GQ assumption, never something to
#' clip silently: under strict = TRUE (the central scenario) it stops; under
#' strict = FALSE (the Q5 bracket edges) the band is pinned at its nearest
#' boundary and the shortfall REPORTED -- an edge the data refuse is exactly
#' what the sensitivity exists to discover.
#'
#' @param below  below-threshold nondependent units: band, w_adults, xb
#' @param fixed  data.table band, fixed_nonfiling (hazard + GQ contributions)
#' @param target data.table band, target (netted anchor)
solve_band_deltas <- function(below, fixed, target, strict = TRUE) {
  out <- merge(fixed, target, by = 'band')
  out[, `:=`(delta = NA_real_, achieved = NA_real_, below_mass = NA_real_,
             shortfall = 0)]
  for (b in out$band) {
    sub  <- below[band == b]
    mass <- sub[, sum(w_adults)]
    need <- out[band == b, target - fixed_nonfiling]
    out[band == b, below_mass := mass]
    if (need < 0 || need > mass) {
      msg <- sprintf(paste('band %s infeasible: need %.3fM non-filing adults',
                           'from a below-threshold mass of %.3fM',
                           '(fixed contributions already %.3fM vs target %.3fM)'),
                     b, need / 1e6, mass / 1e6,
                     out[band == b, fixed_nonfiling] / 1e6,
                     out[band == b, target] / 1e6)
      if (strict) stop(msg)
      message('    INFEASIBLE EDGE, pinned: ', msg)
      pinned <- max(0, min(need, mass))
      out[band == b, `:=`(delta = fifelse(need > mass, -8, 8),
                          achieved = pinned + fixed_nonfiling,
                          shortfall = need - pinned)]
      next
    }
    f <- function(d) sub[, sum(w_adults * (1 - pnorm(xb + d)))] - need
    r <- uniroot(f, lower = -8, upper = 8, tol = 1e-10)
    out[band == b, `:=`(delta = r$root,
                        achieved = need + fixed_nonfiling)]
  }
  out[]
}

for (yr in YEARS) {
  message('=== TY', yr)
  cy <- as.character(yr)

  u  <- readRDS(file.path(RES, sprintf('scored_units_%d.rds', yr)))
  st <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  gq <- fread(file.path(RES, sprintf('gq_backfill_summary_%d.csv', yr)))
  shape <- fread(file.path(SHAPE, sprintf('nonfiler_age_shape_%d.csv', yr)))
  ranch <- fread(file.path(ANCH, sprintf('residual_anchors_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Assemble the identity's pieces, all in the 7-band space
  #---------------------------------------------------------------------------
  nd <- u[unit_type == 'nondependent' & age_head >= 18]
  n_u18_heads <- u[unit_type == 'nondependent' & age_head < 18, sum(weight)]
  nd[, band := as.character(age_band(age_head))]
  nd[, n_adults := fifelse(filing_status == 'joint', 2, 1)]
  nd[, w_adults := weight * n_adults]

  below <- nd[must_file == FALSE, .(band, w_adults, xb = qnorm(p_file_mok))]
  above <- nd[must_file == TRUE,
              .(hh_above = sum(w_adults * p_nonfile_hazard)), by = band]

  gq_b <- gq[band != 'u18',
             .(gq_nonfiling = sum(nonfiling_adults)), by = band]
  fixed <- merge(above, gq_b, by = 'band', all = TRUE)
  for (v in c('hh_above', 'gq_nonfiling')) fixed[is.na(get(v)), (v) := 0]
  fixed[, fixed_nonfiling := hh_above + gq_nonfiling]

  # Dorm consistency tripwire (INFORMATIONAL -- dorm students are NOT a term
  # in the identity, see the header): 03's below-threshold dorm count against
  # the committed anchor column, same classify_gq() on the same ACS. They
  # differ ~2.4% because the threshold tests differ deliberately (03 uses the
  # pool's own concept: INCTOT - INCSS, 65+ variant, $400 SE rule); 5% is
  # enough to catch a drifted classify_gq() or a wrong ACS year.
  dorm_03 <- gq[band != 'u18', sum(dorm_dependents)]
  dorm_anch <- ranch[, sum(dorm_dependents)]
  stopifnot(abs(dorm_03 / dorm_anch - 1) < 0.05)

  # Our claimed-adult-dependent netting, non-filing portion, by the
  # dependent's OWN band: join each 18+ dependent to their scoring unit.
  deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                     .(SERIAL, PERNUM, AGE, ASECWT,
                       unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
  deps <- merge(deps, u[unit_type == 'dependent', .(unit_id, p_file)],
                by = 'unit_id', all.x = TRUE)
  stopifnot(!anyNA(deps$p_file))
  deps[, band := as.character(age_band(AGE))]
  dep_claimed_total <- deps[, sum(ASECWT)]
  dep_net <- deps[, .(dep_nonfiling = sum(ASECWT * (1 - p_file))), by = band]

  anchor <- shape[, .(band, anchor = residual_nonfiling_adults)]

  #---------------------------------------------------------------------------
  # Solve, central netting + the two bracket edges
  #---------------------------------------------------------------------------
  scenarios <- c(central = 1,
                 ht2_floor = DEP_BRACKET[[cy]]['ht2_floor'] / dep_claimed_total,
                 depstat   = DEP_BRACKET[[cy]]['depstat']   / dep_claimed_total)

  sens <- list(); cal <- NULL
  for (s in names(scenarios)) {
    k <- scenarios[[s]]
    tgt <- merge(anchor, dep_net, by = 'band', all.x = TRUE)
    tgt[is.na(dep_nonfiling), dep_nonfiling := 0]
    tgt[, target := anchor - k * dep_nonfiling]
    sol <- solve_band_deltas(below, fixed, tgt[, .(band, target)],
                             strict = (s == 'central'))
    sol <- merge(sol, tgt[, .(band, anchor, dep_nonfiling)], by = 'band')
    sol[, `:=`(scenario = s, netting_scale = k, tax_year = yr)]
    sens[[s]] <- sol
    if (s == 'central') cal <- sol
    message(sprintf(paste('  %-9s netting %.2fM (scale %.2f): pool non-filing',
                          'adults %.2fM; deltas %s'),
                    s, k * dep_claimed_total / 1e6, k,
                    sol[, sum(achieved)] / 1e6,
                    sol[, paste(sprintf('%s %+.2f', band, delta), collapse = ' ')]))
  }
  sens <- rbindlist(sens)
  fwrite(sens, file.path(RES, sprintf('netting_sensitivity_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Apply the central calibration; gates
  #---------------------------------------------------------------------------
  band_map <- setNames(cal$delta, cal$band)
  u[, band := as.character(age_band(age_head))]
  u[, p_file_cal := p_file]
  u[unit_type == 'nondependent' & age_head >= 18 & must_file == FALSE,
    p_file_cal := pnorm(qnorm(p_file_mok) + band_map[band])]
  stopifnot(!anyNA(u$p_file_cal), all(u$p_file_cal >= 0 & u$p_file_cal <= 1))

  # Gate 1: the identity closes exactly, band by band
  chk <- u[unit_type == 'nondependent' & age_head >= 18,
           .(pool = sum(weight * fifelse(filing_status == 'joint', 2, 1) *
                        (1 - p_file_cal))), by = band]
  chk <- merge(chk, merge(cal[, .(band, achieved)],
                          fixed[, .(band, gq_nonfiling)], by = 'band'),
               by = 'band')
  stopifnot(all(abs(chk$pool + chk$gq_nonfiling - chk$achieved) < 1))

  # Gate 2: calibrated group rates stay probabilities with sane ordering
  grp <- u[unit_type == 'nondependent',
           .(p_raw = sum(weight * p_file) / sum(weight),
             p_cal = sum(weight * p_file_cal) / sum(weight)), keyby = mok_group]
  message('  calibrated group rates (raw -> calibrated):')
  for (i in seq_len(nrow(grp)))
    message(sprintf('    %-22s %.3f -> %.3f', grp$mok_group[i],
                    grp$p_raw[i], grp$p_cal[i]))

  total <- cal[, sum(achieved)] + dep_net[, sum(dep_nonfiling)]
  message(sprintf(paste('  identity: pool %.2fM + dependent netting %.2fM',
                        '(dorm students inside it, per the CPS residence',
                        'convention) = %.2fM vs anchor %.2fM | %.0fk under-18',
                        'unit heads outside the 18+ frame'),
                  cal[, sum(achieved)] / 1e6,
                  dep_net[, sum(dep_nonfiling)] / 1e6, total / 1e6,
                  anchor[, sum(anchor)] / 1e6, n_u18_heads / 1e3))
  stopifnot(abs(total - anchor[, sum(anchor)]) < 1)

  fwrite(cal, file.path(RES, sprintf('calibration_%d.csv', yr)))
  saveRDS(u, file.path(RES, sprintf('calibrated_units_%d.rds', yr)))
  message('  wrote calibration_', yr, '.csv, netting_sensitivity_', yr,
          '.csv, calibrated_units_', yr, '.rds')
}

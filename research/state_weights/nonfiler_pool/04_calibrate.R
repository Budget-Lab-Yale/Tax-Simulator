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
#     threshold non-filing units) -- the residual count cannot separately
#     identify the above-threshold share, so the external level is the
#     identifying restriction, exactly as D3 intended.
#   * ONE global adjustment to the wage-presence coefficient, gamma, added
#     2026-08-28. The seven band deltas fix HOW MANY non-filing adults sit in
#     each age band; nothing fixed WHICH ones, and within a band the model
#     takes the lowest predicted filing probabilities. Wage presence is one of
#     the strongest positive predictors of filing in Mok's equations, so the
#     income-less were selected first and wage-earners last: measured against
#     Pub 5785 Table 1, the emitted records carried 8.17M people with wages
#     where 15.96M (scaled to our universe) was implied -- half as many
#     earners, each earning 2.2x as much. Mok reports the same bias in her own
#     results: "simulated filers are slightly more likely to report wage and
#     salary income than are filers."
#
#     gamma shifts the probit index by gamma * 1(has wages) for below-
#     threshold units. One parameter against one constraint, solved OUTSIDE
#     the band loop because the two interact -- every trial gamma re-solves
#     all seven deltas, so the band counts hold exactly at the solution.
#   * dependent-headed units are calibrated to MOK'S OWN PUBLISHED RATES
#     (0.10 under 65, 0.23 at 65+), added 2026-08-28. They were previously left
#     at raw scores on the reasoning that they are the netting, so calibrating
#     them would move both sides of the same equation. That reasoning was
#     wrong: their filing status decides not only how many adults net out but
#     WHOSE INCOME appears in the emitted records, and the transplanted probit
#     scored them at 0.216 against Mok's published 0.10 -- more than double,
#     which put 3.18M working claimed dependents on the filer side. The target
#     is external to the residual count (it is Mok's own tabulation for exactly
#     the group her coefficients score), so it is solved FIRST and the netting
#     is computed from the calibrated probabilities.
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

# Pub 5785 Table 1, TY2014-16 average: persons identified from information
# returns with no 1040, and how many of them have wages. The wage CONSTRAINT
# uses the count rather than the dollars deliberately: the dollar figures are
# not two measurements of one quantity. $480.6B (SSA covered wages less the
# W-2 study's filers) is an unallocated RESIDUAL; Pub 5785's is the IDENTIFIED
# population, and its TY2014-16 average must not be quoted against a TY2017
# residual -- the series rises ~11.9%/yr. See notes/anchor_basis_comparison.md
# (2026-08-29). The person count is a direct tabulation, scaled to our
# universe, since Pub 5785's frame is people WITH an information return and
# ours is every non-filing adult.
#
# KNOWN DEFECT, not yet fixed: gamma reaches only the BELOW-threshold arm --
# 1.46M of 7.87M emitted earners and $10.3B of $274.2B -- and is already at its
# floor. The binding problem is the hazard, which misses its own Table 3
# marginals (wages 55.2% vs 61.7%, dividends 4.8% vs 11.0%) and has no control
# over wage AMOUNTS. Raking fixes the marginals; the amounts need an
# income-level constraint. Same note, "The wage constraint is on the wrong arm".
PUB5785_PERSONS <- 50.49e6
PUB5785_WAGE_EARNERS <- 15.96e6

# gamma is FLOORED, and the floor is the point. Mok's wage-presence
# coefficients run about +0.60 to +0.72 across the large groups, and the
# relationship is not an artefact: wage earners have withholding to reclaim and
# EITC eligibility, so they file. gamma = -0.7 NEUTRALISES wage presence as a
# predictor; anything beyond it INVERTS the sign, which no story supports.
# Unfloored, the solver runs to -3.0 and still falls short of the Pub 5785
# count -- which is evidence about the TARGET, not licence to keep going. The
# residual gap at the floor is reported as a finding.
GAMMA_FLOOR <- -0.7

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
solve_band_deltas <- function(below, fixed, target, strict = TRUE, gamma = 0) {
  out <- merge(fixed, target, by = 'band')
  out[, `:=`(delta = NA_real_, achieved = NA_real_, below_mass = NA_real_,
             shortfall = 0)]
  parts <- split(below, below$band)
  for (b in out$band) {
    sub  <- parts[[b]]
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
      out[band == b, `:=`(delta = fifelse(need > mass, -8, 8),   # gamma-invariant
                          achieved = pinned + fixed_nonfiling,
                          shortfall = need - pinned)]
      next
    }
    f <- function(d) sub[, sum(w_adults * (1 - pnorm(xb + gamma * src_wages + d)))] - need
    r <- uniroot(f, lower = -8, upper = 8, tol = 1e-10)
    out[band == b, `:=`(delta = r$root,
                        achieved = need + fixed_nonfiling)]
  }
  out[]
}

#' Person-level wage-earners among the emitted records, at a given gamma and
#' its solved band deltas. Counts PEOPLE with wages, not adults in a unit that
#' has wages -- a one-earner couple contributes one, which is what Pub 5785
#' counts.
emitted_wage_earners <- function(below, sol, gamma, fixed_earners) {
  d <- setNames(sol$delta, sol$band)
  fixed_earners +
    below[, sum(w_earners * (1 - pnorm(xb + gamma * src_wages + d[band])))]
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

  # person-level wage-earners per unit: how many of head/spouse have wages
  earners <- st$persons[role %in% c('primary', 'spouse'),
                        .(n_earners = sum(INCWAGE > 0)), by = unit_id]
  nd <- merge(nd, earners, by = 'unit_id', all.x = TRUE)
  nd[is.na(n_earners), n_earners := 0L]
  nd[, w_earners := weight * n_earners]

  below <- nd[must_file == FALSE,
              .(band, w_adults, w_earners, src_wages, xb = qnorm(p_file_mok))]
  above <- nd[must_file == TRUE,
              .(hh_above = sum(w_adults * p_nonfile_hazard)), by = band]

  # wage-earners the calibration cannot move: above-threshold units (the hazard
  # is held) and the ACS group-quarters records (scored on a different frame)
  fixed_earners <- nd[must_file == TRUE, sum(w_earners * p_nonfile_hazard)] +
    { g <- fread(file.path(RES, sprintf('gq_persons_%d.csv.gz', yr)))
      g[!is.na(p_file) & AGE >= 18, sum(PERWT * (1 - p_file) * (INCWAGE > 0))] }

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
                     .(SERIAL, PERNUM, AGE, ASECWT, INCWAGE,
                       unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
  deps <- merge(deps, u[unit_type == 'dependent', .(unit_id, p_file)],
                by = 'unit_id', all.x = TRUE)
  stopifnot(!anyNA(deps$p_file))
  deps[, band := as.character(age_band(AGE))]
  dep_claimed_total <- deps[, sum(ASECWT)]

  anchor <- shape[, .(band, anchor = residual_nonfiling_adults)]

  # --- calibrate the dependent-headed units to Mok's published rates ---------
  # (needs `anchor`, `fixed` and `below`, all built above, for the cap)
  # One intercept shift per age side, each solved against the rate Mok
  # publishes for that panel, then the netting is recomputed from the
  # calibrated probabilities.
  #
  # BELOW-THRESHOLD DEPENDENT UNITS ONLY (changed 2026-08-29). v1 applied the
  # shift to every dependent scoring unit on the reasoning that the rate is a
  # property of the panel. That silently overwrote the hazard on the 1.66M
  # dependent-headed units ABOVE the threshold, where filing is required and
  # Mok's rate has no standing -- and it broke a target asserted one script
  # earlier, pushing above-threshold non-filing to 11.79M against the 11.19M
  # `02_filing_model.R:78` had just checked exactly.
  #
  # ASSUMPTION, since Mok does not report whether her dependent panel is
  # restricted to units below the filing requirement: her published 0.10 /
  # 0.23 describe OPTIONAL filing, so they are imposed on the units whose
  # filing is optional, and obligated dependent units keep the hazard. The
  # alternative reading -- treat 0.10 as panel-wide and solve the below-
  # threshold shift so the blended rate hits it -- would drive below-threshold
  # dependents to about 0.056 and tighten the 18-25 feasibility problem
  # further. The panel-wide rate this choice implies is reported below.
  mok_rates <- read_mok_coefs()$filing_rates
  dep_u <- u[unit_type == 'dependent' & must_file == FALSE]
  stopifnot(nrow(dep_u) > 0)
  dep_u[, side := fifelse(age_head >= 65, '65p', 'u65')]
  dep_shift <- c(u65 = NA_real_, `65p` = NA_real_)
  for (sd in names(dep_shift)) {
    sub <- dep_u[side == sd]
    tgt <- mok_rates[[paste0('dependent_', sd)]]
    f <- function(d) sub[, sum(weight * pnorm(qnorm(p_file_mok) + d))] /
                     sub[, sum(weight)] - tgt
    dep_shift[[sd]] <- uniroot(f, lower = -5, upper = 5, tol = 1e-10)$root
    message(sprintf(paste('  dependent units %-3s: scored %.3f -> Mok published',
                          '%.2f at shift %+.3f (%.2fM units)'),
                    sd, sub[, sum(weight * p_file_mok)] / sub[, sum(weight)],
                    tgt, dep_shift[[sd]], sub[, sum(weight)] / 1e6))
  }
  # FEASIBILITY CAP on the under-65 shift, and the cap is itself a result.
  # Mok's 0.10 puts so many young claimed dependents into non-filing that they
  # plus the above-threshold hazard would consume the ENTIRE 18-25 residual
  # count -- TY2022 asks for -1.03M non-filing adults from that band, which is
  # impossible. The three inputs (the residual count by band, Mok's published
  # dependent rate, the hazard level) are mutually inconsistent at the young
  # end, and Mok's rate is the one to give: it is published for HER TY2006
  # constructed population, whose dependent definition is not ours. So move as
  # far toward 0.10 as the band can absorb, and report where it stopped.
  apply_shift <- function(sh) {
    q <- copy(deps)
    d2 <- dep_u[, .(unit_id, p2 = pnorm(qnorm(p_file_mok) +
                                        sh[fifelse(age_head >= 65, '65p', 'u65')]))]
    q <- merge(q, d2, by = 'unit_id', all.x = TRUE)
    q[!is.na(p2), p_file := p2]
    q[, .(dep_nonfiling = sum(ASECWT * (1 - p_file))), by = band]
  }
  headroom <- function(sh) {
    dn <- merge(anchor, apply_shift(sh), by = 'band', all.x = TRUE)
    dn[is.na(dep_nonfiling), dep_nonfiling := 0]
    dn <- merge(dn, fixed[, .(band, fixed_nonfiling)], by = 'band')
    dn <- merge(dn, below[, .(mass = sum(w_adults)), by = band], by = 'band')
    dn[, min((anchor - dep_nonfiling - fixed_nonfiling) / mass)]
  }
  MARGIN <- 0.02          # keep 2% of each band's below-threshold mass in play
  if (headroom(dep_shift) < MARGIN) {
    cap <- uniroot(function(z) headroom(c(u65 = z, `65p` = dep_shift[['65p']])) - MARGIN,
                   lower = dep_shift[['u65']], upper = 0, tol = 1e-8)$root
    sub <- dep_u[side == 'u65']
    message(sprintf(paste('  dependent u65 shift CAPPED at %+.3f (rate %.3f) --',
                          'Mok\'s %+.3f / 0.10 would leave the 18-25 band with',
                          'no room at all. The residual count refuses it.'),
                    cap, sub[, sum(weight * pnorm(qnorm(p_file_mok) + cap))] /
                         sub[, sum(weight)], dep_shift[['u65']]))
    dep_shift[['u65']] <- cap
  }

  dep_u[, p_file_dep := pnorm(qnorm(p_file_mok) + dep_shift[side])]
  deps <- merge(deps, dep_u[, .(unit_id, p_file_dep)], by = 'unit_id', all.x = TRUE)
  deps[!is.na(p_file_dep), p_file := p_file_dep]

  # what the choice above implies panel-wide, obligated dependent units included
  all_dep <- u[unit_type == 'dependent']
  panel <- merge(all_dep[, .(unit_id, weight, must_file, p_file)],
                 dep_u[, .(unit_id, p_file_dep)], by = 'unit_id', all.x = TRUE)
  panel[!is.na(p_file_dep), p_file := p_file_dep]
  message(sprintf(paste('  dependent panel-wide filing rate %.3f (Mok publishes',
                        '%.2f u65 / %.2f 65p for the panel); %.2fM of %.2fM',
                        'dependent units are above the threshold and keep the',
                        'hazard'),
                  panel[, sum(weight * p_file) / sum(weight)],
                  mok_rates[['dependent_u65']], mok_rates[['dependent_65p']],
                  panel[must_file == TRUE, sum(weight)] / 1e6,
                  panel[, sum(weight)] / 1e6))

  dep_net <- deps[, .(dep_nonfiling = sum(ASECWT * (1 - p_file))), by = band]


  #---------------------------------------------------------------------------
  # Solve, central netting + the two bracket edges
  #---------------------------------------------------------------------------
  scenarios <- c(central = 1,
                 ht2_floor = DEP_BRACKET[[cy]]['ht2_floor'] / dep_claimed_total,
                 depstat   = DEP_BRACKET[[cy]]['depstat']   / dep_claimed_total)

  # The wage-earner target: Pub 5785's count scaled to our universe, less the
  # earners among claimed dependents (whom the calibration does not control).
  universe   <- anchor[, sum(anchor)]
  dep_earner <- deps[INCWAGE > 0, sum(ASECWT * (1 - p_file))]
  wage_target <- PUB5785_WAGE_EARNERS * universe / PUB5785_PERSONS - dep_earner

  sens <- list(); cal <- NULL; gam <- NULL
  for (s in names(scenarios)) {
    k <- scenarios[[s]]
    tgt <- merge(anchor, dep_net, by = 'band', all.x = TRUE)
    tgt[is.na(dep_nonfiling), dep_nonfiling := 0]
    tgt[, target := anchor - k * dep_nonfiling]
    tgtb <- tgt[, .(band, target)]

    solve_at <- function(g) solve_band_deltas(below, fixed, tgtb,
                                              strict = (s == 'central'), gamma = g)
    # Outer solve on gamma: every trial re-solves the seven deltas, so the band
    # counts hold exactly whatever gamma turns out to be. Only the central
    # scenario is constrained on wages -- the bracket edges exist to show what
    # the netting alone does, and adding a second moving part would confound
    # them.
    if (s == 'central') {
      f <- function(g) emitted_wage_earners(below, solve_at(g), g, fixed_earners) -
                       wage_target
      lo <- f(GAMMA_FLOOR); hi <- f(0)
      if (lo * hi > 0) {
        gamma <- if (abs(lo) < abs(hi)) GAMMA_FLOOR else 0
        got <- emitted_wage_earners(below, solve_at(gamma), gamma, fixed_earners)
        message(sprintf(paste('    wage target NOT reachable with a credible',
                              'coefficient: want %.2fM earners, %.2fM at the',
                              'gamma floor %.2f -- pinned. Closing the rest',
                              'would need the wage coefficient to change sign,',
                              'so the remaining %.2fM is a question about the',
                              'TARGET, not the model. See the header.'),
                        wage_target / 1e6, got / 1e6, gamma,
                        (wage_target - got) / 1e6))
      } else {
        gamma <- uniroot(f, lower = GAMMA_FLOOR, upper = 0, tol = 1e-8)$root
      }
      gam <- gamma
    } else {
      gamma <- 0
    }

    sol <- solve_at(gamma)
    sol <- merge(sol, tgt[, .(band, anchor, dep_nonfiling)], by = 'band')
    sol[, `:=`(scenario = s, netting_scale = k, tax_year = yr, gamma = gamma)]
    sens[[s]] <- sol
    if (s == 'central') cal <- sol
    message(sprintf(paste('  %-9s netting %.2fM (scale %.2f): pool non-filing',
                          'adults %.2fM; gamma %+.3f; deltas %s'),
                    s, k * dep_claimed_total / 1e6, k,
                    sol[, sum(achieved)] / 1e6, gamma,
                    sol[, paste(sprintf('%s %+.2f', band, delta), collapse = ' ')]))
  }
  message(sprintf(paste('  wage constraint: %.2fM person-earners targeted',
                        '(Pub 5785 %.2fM scaled by %.2fM/%.2fM, less %.2fM among',
                        'claimed dependents); achieved %.2fM at gamma %+.3f'),
                  wage_target / 1e6, PUB5785_WAGE_EARNERS / 1e6,
                  universe / 1e6, PUB5785_PERSONS / 1e6, dep_earner / 1e6,
                  emitted_wage_earners(below, cal, gam, fixed_earners) / 1e6, gam))
  sens <- rbindlist(sens)
  fwrite(sens, file.path(RES, sprintf('netting_sensitivity_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Apply the central calibration; gates
  #---------------------------------------------------------------------------
  band_map <- setNames(cal$delta, cal$band)
  u[, band := as.character(age_band(age_head))]
  u[, p_file_cal := p_file]
  u[unit_type == 'nondependent' & age_head >= 18 & must_file == FALSE,
    p_file_cal := pnorm(qnorm(p_file_mok) + gam * src_wages + band_map[band])]
  u[unit_type == 'dependent' & must_file == FALSE,
    p_file_cal := pnorm(qnorm(p_file_mok) +
                        dep_shift[fifelse(age_head >= 65, '65p', 'u65')])]
  stopifnot(!anyNA(u$p_file_cal), all(u$p_file_cal >= 0 & u$p_file_cal <= 1))

  # The hazard's target must SURVIVE calibration. It did not before
  # 2026-08-29: the dependent shift reached above-threshold units and nothing
  # re-checked the level afterwards. Assert it here, where it can break.
  above_after <- u[must_file == TRUE, sum(weight * (1 - p_file_cal))]
  stopifnot(abs(above_after - PUB5785_TARGET_UNITS) < 1e4)

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

#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 02_build_residual_anchors.R  (Stage D, research/state_weights/nonfiler_residual_design.md §4.2)
#
# Build the residual non-filer anchors for one tax year:
#
#   national_anchor_{year}.csv     age band x {pep_adults, filing_adults,
#                                  residual_nonfiling_adults} -- filing side
#                                  from Pub 1304 Table 1.6 (returns by marital
#                                  status x age), the age dimension HT2 lacks
#   residual_anchors_{year}.csv    state x {pep_adults_18p, married/single
#                                  filing adults (HT2 identities via
#                                  ht2_filing_persons()), mfs_qss_returns (the
#                                  HT2 status residual -- MFS plus qualifying
#                                  surviving spouse, since HT2 publishes no MFS
#                                  series; added 2026-08-27 for decision #5,
#                                  see notes/anchor_basis_comparison.md Part C),
#                                  residual, AND the
#                                  dorm-netted residual -- two universes, two
#                                  columns, because consumers differ:
#                                    residual_nonfiling_adults
#                                      raw: PEP adults - filing adults, so it
#                                      still contains adult dependents
#                                    residual_nonfiling_adults_net_dorm
#                                      less the dormitory students that
#                                      build_acs_margins() removes from the
#                                      non-filer margin (B1) -- the universe
#                                      that matches the margin and the PUF
#                                      non-filer partition. NA unless script
#                                      03 --acs has been run; see ORDERING at
#                                      the netting block below.
#   nonfiler_wage_margin_{year}.csv state wage margin: HT2 returns-with-wages
#                                  and wage dollars against two covered-worker
#                                  frames -- QCEW (jobs and payroll) and SSA
#                                  EEDATA Table 4 (persons and taxable
#                                  earnings, HI/Medicare coverage)
#   ssa_age_margin_{year}.csv      state x SSA age band covered-worker counts
#                                  (EEDATA Table 5), the input to the state x
#                                  age allocation of the residual (D6)
#
# Universe (design memo §3.0): PEP RESIDENT population, no group-quarters
# subtraction -- the PUF/DINA universe includes GQ residents. Age bands follow
# Table 1.6 (18-25, 26-34, 35-44, 45-54, 55-64, 65+), not age_band().
#
# Universe tags travel with the SSA margins (memo §7.3): EEDATA is
# `covered_worker_hi`, OASDI is `beneficiary`. Neither is `resident` -- both
# are administrative person-level universes -- so they enter as SHAPES over
# the PEP-based residual, never as levels.
#
# Known wedges carried, not resolved (memo §3.1/fn.8): return-state vs
# residence; MFS/QSS residual in the identities; adult dependents claimed on
# returns sit inside the residual (national wedge quantified in T1, script 03);
# MFJ spouses are assigned the primary taxpayer's age band.
#
# Usage (repo root, login node OK):
#   Rscript research/state_weights/nonfiler_residual/02_build_residual_anchors.R [year]
# Default runs both anchor years (2017, 2022).
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml); library(readxl)
})
source('src/data/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
years <- if (length(args)) as.integer(args) else c(2017L, 2022L)
res_dir <- 'research/state_weights/nonfiler_residual/results'
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

# Anchor age bands = Pub 1304 Table 1.6 bands (adults 18+)
# a16_band()/TARGET_AGE_BANDS now live in src/data/state_weights.R (sourced
# above) -- one definition, shared with age_band()/target_age_band() so the
# anchor bands and the fit's cell bands cannot drift apart again. Verified
# identical to the local copy this replaces (2026-08-19).
A16_BANDS <- TARGET_AGE_BANDS

# Pub 1304 Table 1.6 (returns by filing-status block x age band) and Table 1.7
# (dependent returns) are read by read_pub1304_t16() / read_pub1304_t17_total()
# in src/data/state_weights.R -- promoted there 2026-08-27 so script 12's
# basis comparison reads the same national level this script anchors on.

#---------------------------------------------------------------
# Census PEP: resident population by state x single year of age
#---------------------------------------------------------------

read_pep <- function(year) {
  f <- file.path(raw_data_root(), 'Census-PEP',
                 # The intercensal file is the REVISED 2010-2019 series and
                 # stops at POPESTIMATE2019; the 2024 vintage starts at 2020.
                 # The boundary was <= 2020, which only ever failed on a TY2020
                 # build -- 2017 and 2022 both land on the right side of it.
                 if (year <= 2019) 'sc-est2020int-alldata6.csv' else
                                   'sc-est2024-alldata6.csv')
  pep <- fread(f, showProgress = FALSE)
  pop_col <- sprintf('POPESTIMATE%d', year)
  stopifnot(pop_col %in% names(pep), all(pep$AGE <= 85 | pep$AGE == 999))
  # Totals: SEX 0 and ORIGIN 0 are marginal rows; RACE 1-6 partition (no total)
  pep <- pep[SEX == 0 & ORIGIN == 0 & AGE != 999,
             .(pop = sum(get(pop_col))), by = .(STATE, AGE)]
  pep[, state := FIPS_TO_STATE[as.character(STATE)]]
  stopifnot(!anyNA(pep$state), uniqueN(pep$state) == 51)
  pep[]
}

#---------------------------------------------------------------
# Build per year
#---------------------------------------------------------------

for (yr in years) {
  message('=== TY', yr)

  pep <- read_pep(yr)
  message(sprintf('  PEP resident pop: %.2fM total, %.2fM adults 18+',
                  pep[, sum(pop)] / 1e6, pep[AGE >= 18, sum(pop)] / 1e6))

  t16 <- read_pub1304_t16(yr)
  t17_dep_returns <- read_pub1304_t17_total(yr)

  # Cross-check the label parse against the published all-returns total
  t16_total <- t16[block == 'all', sum(n_returns)]
  blocks_total <- t16[block != 'all', sum(n_returns)]
  message(sprintf('  T1.6 all-returns %.1fM | status blocks sum %.1fM (gap %.2f%%) | T1.7 dependent returns %.2fM',
                  t16_total / 1e6, blocks_total / 1e6,
                  100 * (blocks_total / t16_total - 1), t17_dep_returns / 1e6))

  # Filing adults by band: 2 per MFJ return (primary's band), 1 otherwise.
  # The status blocks carry no "Under 18" row -- under-18 filers (published
  # only in the all-returns block) sit inside the single block's "Under 26",
  # so they are subtracted from the 18_25 band here.
  fa <- t16[block != 'all' & band != 'u18',
            .(filing_adults = sum(n_returns * fifelse(block == 'mfj', 2, 1))),
            by = band]
  u18_filers <- t16[block == 'all' & band == 'u18', sum(n_returns)]
  fa[band == '18_25', filing_adults := filing_adults - u18_filers]

  # HT2 identities by state. Read BEFORE the national anchor because the two
  # universe corrections below are measured off HT2 and applied to T1.6's level.
  ht2 <- read_ht2(ht2_path(yr), yr)
  fp  <- ht2_filing_persons(ht2)
  ht2_filing_adults <- fp[, sum(married_filing_adults + single_filing_adults)]

  #--------------------------------------------------------------------------
  # THE ANCHOR BASIS (S15, JI 2026-08-27). Pub 1304 owns the LEVEL, HT2 owns
  # the STATE SHARES, and two universe corrections come off the level first.
  #
  # This closes what the state-anchor comment below had deferred as "state
  # shares of the T1.6-consistent national level come later". Before it, the
  # national anchor used T1.6 and the state anchors used the HT2 identities, so
  # the 51 states summed to -1.36% (2017) / +2.14% (2022) of the national
  # figure with the sign FLIPPING between the anchor years -- and the fit
  # targets the state file while every document quoted the national one.
  # Measured in notes/anchor_basis_comparison.md.
  #
  #   (a) OUT-OF-STATE FILERS come out of the level. SOI's Other Areas footnote:
  #       "returns filed from Army Post Office and Fleet Post Office addresses
  #       by members of the armed forces stationed overseas; and returns filed
  #       by other U.S. citizens abroad". Both sit OUTSIDE the Census resident
  #       population -- which excludes overseas forces and citizens abroad --
  #       so subtracting them from PEP was subtracting people the denominator
  #       never contained. Removed, not reallocated. ~1.0-1.2M.
  #
  #   (b) THE QSS DOUBLE-COUNT comes out too. T1.6 has four status blocks and
  #       folds qualifying surviving spouses into the joint one ("Returns of
  #       married persons filing jointly AND RETURNS OF SURVIVING SPOUSES"), so
  #       the 2-adults-per-joint-return rule above gives a surviving spouse who
  #       filed ALONE two adults. QSS is published nowhere, so it is derived:
  #       HT2's status residual (MFS + QSS, since HT2 has no MFS series) less
  #       T1.6's published MFS, on a matched universe. 0.06-0.08M.
  #
  # Both corrections RAISE the residual, and together by 2.2-2.6%.
  #
  # ⚠ ASSUMPTION, not a finding (JI 2026-08-27): the out-of-state removal is
  # spread PRO RATA across age bands, because HT2's bucket carries no age. The
  # footnote covers two populations with opposite age profiles -- overseas
  # forces, who skew 18-34, and the far larger "other U.S. citizens abroad",
  # who do not -- and the bucket is a mailing-address artifact rather than a
  # residency determination, so nothing in the data supports concentrating it.
  # The choice moves the 18_25 band's residual by +1.3% (pro rata) to +11.7%
  # (all of it in 18_25); the total is unaffected either way. It matters because
  # nonfiler_age_shape_{year}.csv is a validation target for the constructed
  # pool, so if that validation later disagrees at the young end, look here
  # first. The QSS correction is spread the same way and is immaterial at its
  # magnitude.
  #--------------------------------------------------------------------------
  fa_oos      <- {
    o <- ht2_filing_persons(ht2, states = NONTAX_BUCKETS)
    if (nrow(o)) o[, sum(married_filing_adults + single_filing_adults)] else 0
  }
  mfs_qss     <- fp[, sum(mfs_qss_returns)] +
                 { o <- ht2_filing_persons(ht2, states = NONTAX_BUCKETS)
                   if (nrow(o)) o[, sum(mfs_qss_returns)] else 0 }
  qss_implied <- mfs_qss - t16[block == 'mfs', sum(n_returns)]
  # A negative implied QSS would mean T1.6's published MFS exceeds the whole
  # HT2 status residual, which is impossible on a matched universe and is how
  # the missing out-of-state buckets were found in the first place.
  stopifnot(qss_implied >= 0)

  fa[, filing_adults_t16_published := filing_adults]
  level_correction <- fa_oos + qss_implied
  fa[, filing_adults := filing_adults -
       level_correction * filing_adults_t16_published /
       sum(filing_adults_t16_published)]
  level_51 <- fa[, sum(filing_adults)]

  message(sprintf(paste('  anchor basis (S15): T1.6 %.3fM - out-of-state %.3fM',
                        '- QSS double-count %.3fM = level %.3fM (%+.2f%%)'),
                  fa[, sum(filing_adults_t16_published)] / 1e6, fa_oos / 1e6,
                  qss_implied / 1e6, level_51 / 1e6,
                  -100 * level_correction / fa[, sum(filing_adults_t16_published)]))

  # National anchor: PEP adults by band minus the corrected filing adults
  pep_nat <- pep[AGE >= 18, .(pep_adults = sum(pop)), by = .(band = as.character(a16_band(AGE)))]
  nat <- merge(pep_nat, fa[, .(band, filing_adults)], by = 'band', all = TRUE)
  nat <- nat[match(A16_BANDS, band)]
  nat[, residual_nonfiling_adults := pep_adults - filing_adults]
  nat <- rbind(nat, data.table(band = 'total_18p', t(colSums(nat[, -1]))))
  # The published-vs-HT2 gap, kept as a diagnostic on the two SOURCE FAMILIES.
  # It is NOT the corrected level (reported above) -- labelling it T1.6 after
  # the correction would misname the quantity. The gap does not decompose into
  # named universe differences; notes/anchor_basis_comparison.md Part A.
  message(sprintf(paste('  source families, filing adults 18+: T1.6 as published',
                        '%.1fM (excl. %.2fM under-18 filers) vs HT2 identities',
                        '%.1fM (gap %+.2f%%)'),
                  fa[, sum(filing_adults_t16_published)] / 1e6, u18_filers / 1e6,
                  ht2_filing_adults / 1e6,
                  100 * (fa[, sum(filing_adults_t16_published)] / ht2_filing_adults - 1)))
  message(sprintf('  national residual non-filing adults 18+: %.1fM (%.1f%% of PEP adults)',
                  nat[band == 'total_18p', residual_nonfiling_adults] / 1e6,
                  100 * nat[band == 'total_18p', residual_nonfiling_adults / pep_adults]))

  # --- the 7-band CELL-space age shape (D1's input) -------------------------
  # `nat` above is the 6-band TARGET space, and it stays that way: script 03's
  # T1/T2 read national_anchor_{year}.csv and the state targets can be no finer
  # than T1.6's "65 and over". This is the separate, finer object -- emitted as
  # its own artifact rather than by restructuring `nat` -- because the CELL
  # space is 7 bands and the Tax-Data non-filer age draw needs the split.
  #
  # 65+ is splittable NATIONALLY and only nationally: SOI's IRA study Table 4
  # gives filers by five-year band, so `65 under 70`+`70 under 75` and
  # `75 under 80`+`80 and over` aggregate exactly onto 65_74/75p. Used as a
  # SHARE of T1.6's level, never as a level -- see read_soi_ira_age_split().
  #
  # WHY THIS FILE EXISTS: Tax-Data draws the non-filer age group from
  # `floor(runif(...))` over the DINA ageprim buckets
  # (Tax-Data src/impute_nonfilers.R:92-96), which puts 41-43% of non-filing
  # adults at 65+ against this anchor's ~25% and 9-10% at 18-25 against ~22-24%
  # (finding F2). The age composition is not blurred, it is INVERTED, and this
  # is the shape that replaces the draw.
  #
  # ⚠ CONVENTION WEDGE, carried not resolved (same one the anchor already has):
  # PEP counts each person at their OWN age; T1.6 assigns a joint return's two
  # filing adults to the PRIMARY's band (its own footnote). So the residual by
  # band mixes conventions, and the Tax-Data draw it feeds assigns the PRIMARY's
  # age. That is exact for the single/HoH majority and approximate for the ~17%
  # of non-filer units that are joint. Do not read `share` as a distribution of
  # non-filing adults over their own ages.
  ira <- read_soi_ira_age_split(yr)
  fa_65p <- fa[band == '65p', filing_adults]
  shape <- data.table(
    band       = c('18_25','26_34','35_44','45_54','55_64','65_74','75p'),
    age_group  = c(1L, 2L, 3L, 4L, 5L, 6L, 6L),   # Tax-Data's own coding
    pep_adults = c(nat[band %in% A16_BANDS[1:5]][match(A16_BANDS[1:5], band), pep_adults],
                   pep[AGE >= 65 & AGE < 75, sum(pop)],
                   pep[AGE >= 75, sum(pop)]),
    filing_adults = c(fa[match(A16_BANDS[1:5], band), filing_adults],
                      fa_65p * ira$share_65_74,
                      fa_65p * ira$share_75p))
  shape[, residual_nonfiling_adults := pep_adults - filing_adults]
  shape[, share := residual_nonfiling_adults / sum(residual_nonfiling_adults)]
  # within-age_group share, so D1 can draw the coarse group then split 65+
  shape[, share_within_age_group := share / sum(share), by = age_group]
  shape[, `:=`(year = yr, ira_share_65_74 = ira$share_65_74)]

  # The 7-band object must reconcile to the 6-band one it refines, exactly:
  # only the 65p row is being split, so everything else must be untouched.
  stopifnot(
    all(shape$residual_nonfiling_adults > 0),
    isTRUE(all.equal(sum(shape$pep_adults), nat[band == 'total_18p', pep_adults])),
    isTRUE(all.equal(sum(shape$filing_adults), nat[band == 'total_18p', filing_adults])),
    isTRUE(all.equal(shape[age_group == 6, sum(pep_adults)],
                     nat[band == '65p', pep_adults])),
    isTRUE(all.equal(shape[age_group == 6, sum(filing_adults)], fa_65p)),
    isTRUE(all.equal(sum(shape$share), 1)))
  message(sprintf(paste('  age shape (7-band): 18_25 %.1f%% | 65_74 %.1f%% | 75p %.1f%%;',
                        'IRA 65_74 share %.4f, non-filing rate %.1f%% vs %.1f%%'),
                  100 * shape[band == '18_25', share],
                  100 * shape[band == '65_74', share],
                  100 * shape[band == '75p', share], ira$share_65_74,
                  100 * shape[band == '65_74', residual_nonfiling_adults / pep_adults],
                  100 * shape[band == '75p', residual_nonfiling_adults / pep_adults]))

  # State anchor: PEP 18+ minus the T1.6-consistent level distributed by HT2
  # state shares (S15, implemented 2026-08-27 -- this is the "come later" the
  # previous version of this comment deferred).
  #
  # BOTH BASES ARE KEPT, as columns, the way the dorm netting below is: the
  # primary columns are the corrected basis and are what consumers get by
  # default, and the `_ht2basis` pair is retained so the comparison stays
  # readable rather than only living in a note.
  st <- merge(pep[AGE >= 18, .(pep_adults_18p = sum(pop)), by = state],
              fp, by = 'state')
  st[, filing_adults_ht2 := married_filing_adults + single_filing_adults]
  st[, residual_nonfiling_adults_ht2basis := pep_adults_18p - filing_adults_ht2]

  st[, ht2_filing_share := filing_adults_ht2 / sum(filing_adults_ht2)]
  st[, filing_adults := level_51 * ht2_filing_share]
  st[, residual_nonfiling_adults := pep_adults_18p - filing_adults]
  st[, residual_share_of_adults := residual_nonfiling_adults / pep_adults_18p]

  # The whole point of the basis change: the 51 states now sum to the national
  # anchor BY CONSTRUCTION, because PEP's state counts sum to PEP's national
  # count and the shares sum to one. If this ever fails the two files have
  # drifted apart again, which is the defect S15 exists to close.
  stopifnot(
    isTRUE(all.equal(st[, sum(residual_nonfiling_adults)],
                     nat[band == 'total_18p', residual_nonfiling_adults],
                     tolerance = 1e-6)),
    all(st$residual_nonfiling_adults > 0),
    all(st$filing_adults > 0))

  message(sprintf(paste('  state anchors: %.3fM (corrected basis) vs %.3fM (HT2',
                        'basis) = %+.2f%%; sums to the national anchor exactly'),
                  st[, sum(residual_nonfiling_adults)] / 1e6,
                  st[, sum(residual_nonfiling_adults_ht2basis)] / 1e6,
                  100 * (st[, sum(residual_nonfiling_adults)] /
                         st[, sum(residual_nonfiling_adults_ht2basis)] - 1)))
  message(sprintf('    per-state change: %+.2f%% (%s) to %+.2f%% (%s)',
                  100 * min(st$residual_nonfiling_adults / st$residual_nonfiling_adults_ht2basis - 1),
                  st[which.min(residual_nonfiling_adults / residual_nonfiling_adults_ht2basis), state],
                  100 * max(st$residual_nonfiling_adults / st$residual_nonfiling_adults_ht2basis - 1),
                  st[which.max(residual_nonfiling_adults / residual_nonfiling_adults_ht2basis), state]))

  # --- dorm-student netting (D1's "net of claimed adult dependents") ---------
  # PEP places a dormitory student in the INSTITUTION state and they are not a
  # filing adult, so they survive `pep_adults_18p - filing_adults` and sit
  # inside the raw residual. `build_acs_margins()` (task B1) removes exactly
  # those people from the non-filer MARGIN, on the ground that they are
  # dependents on a parent's return. Left alone, the two objects then measure
  # different universes -- the margin excludes them, the anchor keeps them --
  # and every comparison between the two inherits the gap. It is 14-20% of the
  # anchor in VT/RI/DC/MA/ND/CT.
  #
  # So the netted column exists, as a SECOND column rather than a replacement:
  # `residual_tolerance_*`, T5 and the F5 population identity all read the
  # anchor as a level, and they do not all want the same universe. Consumers
  # name the one they mean.
  #
  # This is the DORM subtraction only, and deliberately not the full
  # adult-dependent pool. The full pool via `dependents - PEP under-18` lands
  # at 5.58M nationally (11.7% of the residual, reproducing the design memo's
  # ~5.5M/12%), but its STATE distribution assumes every under-18 is claimed,
  # which is wrong exactly where non-filing parents are common; measured
  # 2026-08-24 it degrades every metric and moves DC opposite to the
  # dorm-specific netting. It needs a real state child-claiming estimate.
  #
  # ORDERING: this reads script 03's --acs output, which does NOT read the
  # anchors, so there is no cycle -- but it does mean the full sequence is
  # 01 -> 03 --acs -> 02 -> 03 --tables. Absent that file the column is NA and
  # says so, rather than silently reporting the raw residual as netted.
  gq_f <- file.path(res_dir, sprintf('acs_gq_reclassified_%d.csv', yr))
  if (file.exists(gq_f)) {
    gq <- fread(gq_f)
    stopifnot('state' %in% names(gq), 'persons' %in% names(gq))
    st <- merge(st, gq[, .(state, dorm_dependents = persons)], by = 'state',
                all.x = TRUE)
    st[is.na(dorm_dependents), dorm_dependents := 0]
    st[, residual_nonfiling_adults_net_dorm :=
         residual_nonfiling_adults - dorm_dependents]
    # A netted residual that went non-positive would mean the ACS assigned a
    # state more dorm students than it has non-filing adults: impossible, and a
    # sign the two sides were built on different years or universes.
    stopifnot(all(st$residual_nonfiling_adults_net_dorm > 0),
              all(st$dorm_dependents >= 0))
    message(sprintf(paste('  dorm-student netting: %.2fM of %.2fM (%.1f%%);',
                          'state range %.1f%% (%s) to %.1f%% (%s)'),
                    st[, sum(dorm_dependents)] / 1e6,
                    st[, sum(residual_nonfiling_adults)] / 1e6,
                    100 * st[, sum(dorm_dependents) / sum(residual_nonfiling_adults)],
                    100 * min(st$dorm_dependents / st$residual_nonfiling_adults),
                    st[which.min(dorm_dependents / residual_nonfiling_adults), state],
                    100 * max(st$dorm_dependents / st$residual_nonfiling_adults),
                    st[which.max(dorm_dependents / residual_nonfiling_adults), state]))
  } else {
    st[, `:=`(dorm_dependents = NA_real_,
              residual_nonfiling_adults_net_dorm = NA_real_)]
    message('  dorm-student netting: SKIPPED -- no ', basename(gq_f),
            ' (run 03_diagnose_current_nonfilers.R --acs ', yr,
            ' then re-run this script)')
  }

  # SSA OASDI beneficiaries aged 65+ (December stock) -- the state x age input
  # for the elderly end of D6, where Table 1.6 stops at a single 65+ band. A
  # SHAPE, not a level: it is a beneficiary universe and a point-in-time stock
  # against the residual's annual resident flow.
  st <- merge(st, read_ssa_oasdi_65p(yr)[, .(state, beneficiaries_65p)],
              by = 'state')
  pep65 <- pep[AGE >= 65, .(pep_65p = sum(pop)), by = state]
  st <- merge(st, pep65, by = 'state')
  st[, ssa_65p_coverage := beneficiaries_65p / pep_65p]
  message(sprintf('  OASDI 65+ beneficiaries: %.1fM = %.1f%% of PEP 65+ (state range %.1f%%-%.1f%%)',
                  st[, sum(beneficiaries_65p)] / 1e6,
                  100 * st[, sum(beneficiaries_65p) / sum(pep_65p)],
                  100 * min(st$ssa_65p_coverage), 100 * max(st$ssa_65p_coverage)))
  message(sprintf('  state residual shares: %.1f%% (min %s) to %.1f%% (max %s)',
                  100 * min(st$residual_share_of_adults), st[which.min(residual_share_of_adults), state],
                  100 * max(st$residual_share_of_adults), st[which.max(residual_share_of_adults), state]))

  # Wage margin: HT2 returns-with-wages / wage dollars vs QCEW; SSA
  # persons-with-wages pending (store blocked; see 01_fetch README)
  #
  # STATE-LEVEL products only, and their two inputs are short series: QCEW is
  # on disk for 2017 and 2022, SSA EEDATA-SC for 2017-2023 (both manual
  # downloads -- the cluster is 403-blocked). The NATIONAL anchor and the age
  # shape need neither, and group D builds years those series do not reach, so
  # a missing input skips these two products with a message instead of killing
  # the year. Nothing here is silently defaulted: the CSVs are simply absent.
  qcew_f <- file.path(raw_data_root(), 'BLS-QCEW',
                      sprintf('qcew_state_totals_%d.csv', yr))
  ssa_f  <- ssa_workbook('SSA-EEDATA-SC', yr)
  # ABSENT and CORRUPT are different, and only the first is a skip. ssa.gov
  # 403s land an HTML error page under an .xlsx name (which is why
  # 13_verify_ssa_backfill.R exists), and a blanket tryCatch would report that
  # file as missing and silently drop the state products. A parse failure on a
  # file that IS there propagates.
  ee <- if (file.exists(ssa_f)) read_ssa_eedata_hi(yr) else NULL
  do_wage_margin <- file.exists(qcew_f) && !is.null(ee)
  if (!do_wage_margin) {
    message(sprintf(paste('  SKIP state wage margin and SSA age margin for TY%d:',
                          '%s. The national anchor and age shape are unaffected.'),
                    yr, paste(c(if (!file.exists(qcew_f)) 'no QCEW state totals',
                                if (is.null(ee)) 'no SSA EEDATA workbook'),
                              collapse = ' and ')))
  }
  wm <- NULL
  if (do_wage_margin) {
  ht2_w <- dcast(ht2[variable %in% c('n_wages', 'wages_amt') & !(state %in% NONTAX_BUCKETS),
                     .(value = sum(value)), by = .(state, variable)],
                 state ~ variable, value.var = 'value')
  qcew <- fread(qcew_f)[state != 'US']
  wm <- merge(ht2_w, qcew[, .(state, qcew_avg_emplvl = annual_avg_emplvl,
                              qcew_wages = total_annual_wages)], by = 'state')
  # SSA EEDATA Table 4: persons with HI-covered wage-and-salary earnings, and
  # those earnings. Persons, unlike QCEW's average monthly employment level,
  # are the right denominator for HT2's returns-with-wages -- the remaining
  # wedge is returns vs persons (joint returns, multiple earners), not jobs vs
  # persons. Earnings are uncapped under HI, so the dollar ratio is meaningful.
  wm <- merge(wm, ee$persons[, .(state,
                                 ssa_covered_persons = hi_persons_wage_salary,
                                 ssa_covered_wages   = hi_wage_salary_earnings)],
              by = 'state')
  wm[, `:=`(ht2_returns_per_ssa_person = n_wages / ssa_covered_persons,
            ht2_wages_per_ssa_wages    = wages_amt / ssa_covered_wages,
            ht2_wages_per_qcew_wages   = wages_amt / qcew_wages)]
  message(sprintf('  SSA HI-covered wage earners: %.1fM, $%.3fT | HT2 returns-with-wages per covered person %.3f (range %.3f-%.3f)',
                  wm[, sum(ssa_covered_persons)] / 1e6,
                  wm[, sum(ssa_covered_wages)] / 1e12,
                  wm[, sum(n_wages) / sum(ssa_covered_persons)],
                  wm[, min(ht2_returns_per_ssa_person)],
                  wm[, max(ht2_returns_per_ssa_person)]))
  }

  fwrite(nat, file.path(res_dir, sprintf('national_anchor_%d.csv', yr)))
  # The age shape goes to resources/, not results/: it is a committed INPUT to
  # the Tax-Data rework (D1), not a regenerable diagnostic.
  shape_dir <- 'research/state_weights/nonfiler_residual/resources'
  fwrite(shape[, .(year, band, age_group, pep_adults, filing_adults,
                   residual_nonfiling_adults, share, share_within_age_group,
                   ira_share_65_74)],
         file.path(shape_dir, sprintf('nonfiler_age_shape_%d.csv', yr)))
  fwrite(st[order(state)], file.path(res_dir, sprintf('residual_anchors_%d.csv', yr)))
  if (do_wage_margin) {
    fwrite(wm[order(state)], file.path(res_dir, sprintf('nonfiler_wage_margin_%d.csv', yr)))
    fwrite(ee$age, file.path(res_dir, sprintf('ssa_age_margin_%d.csv', yr)))
  }
  message(sprintf('  wrote national_anchor / residual_anchors%s CSVs',
                  if (do_wage_margin)
                    ' / nonfiler_wage_margin / ssa_age_margin' else ''))
}

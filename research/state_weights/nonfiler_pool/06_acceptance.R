#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 06_acceptance.R  (group C, stage C8 -- plan: research/state_weights/plan.md §7
#                   "the pool must beat DINA on all four measurable dimensions,
#                   not just the level")
#
# Compares the emitted pool against the file it replaces (the production
# Tax-Data non-filer append, DINA-derived) on the four dimensions the plan
# names, plus the level.
#
# THE HONEST STRUCTURE, stated because it decides how to read the output:
#
#   BY CONSTRUCTION (not independent tests -- the calibration targeted them):
#     * the level, and the age composition by band. C4 solved seven band
#       deltas against exactly these, so the pool matches to the assertion
#       tolerance and DINA's deviation is the whole content of the comparison.
#       Reported because DINA CANNOT match them -- that is the finding -- but
#       never as evidence the model is right.
#
#   INDEPENDENT (nothing in the calibration touched these):
#     * wage mass against the administratively implied non-filer total
#     * Social Security receipt against Pub 5785's rate
#     * interest / dividend / capital-gain receipt against Pub 5785's ceilings
#     These are the real tests. Mok's slopes carry income-source presence, but
#     no target on any of them entered the fit.
#
# TWO CONCEPT WEDGES, handled rather than ignored:
#   1. Pub 5785 Table 1 is PERSON level ("potential non-filers"); the pool is
#      units. Receipt rates are computed person-weighted (a joint unit with
#      interest contributes 2 persons with interest) so the comparison is
#      like-for-like.
#   2. The administratively implied wage total (SSA HI covered wages less HT2
#      on-return wages) covers everyone not on a return, INCLUDING claimed
#      dependents who work -- students above all. The pool excludes them by
#      design, so the reconciliation adds the constructed dependents' wages
#      back before comparing.
#
# Writes: results/acceptance_{year}.csv (the verdict table)
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/06_acceptance.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'
ANCH  <- 'research/state_weights/nonfiler_residual/results'
SHAPE <- 'research/state_weights/nonfiler_residual/resources'

PUB5785_T1 <- file.path(ANCH, '..', 'resources',
                        'pub5785_table1_potential_nonfilers.csv')

# Two TY2017 wage benchmarks, and they are NOT the same quantity -- see
# notes/anchor_basis_comparison.md, "Resolved, mostly" (2026-08-29).
#   * The RESIDUAL: SSA covered wages less the W-2 study's filers. A 6% sliver
#     of two ~$8T aggregates, so it absorbs every wage dollar not tied to a
#     filed return -- unlinkable W-2s, SSN/ITIN mismatches, decedents. An upper
#     bound on what could exist, not a count of anyone.
#   * The IDENTIFIED population: Pub 5785 Table 1, people the IRS matched to an
#     information return and no 1040. This is the right target for a file of
#     constructible records. Pub 5785 has no TY2017, and the series rises
#     ~11.9%/yr (215.3 / 242.5 / 269.8 for TY2014-16), so TY2017 is projected:
#     $295B at nominal wage growth, $338B at the series' own trend. Quoting the
#     TY2014-16 average of $242.5B against a TY2017 figure -- as this file and
#     the plan both did -- manufactured about 40% of an apparent 2x gap.
RESIDUAL_IMPLIED_WAGES_2017   <- 480.6e9
PUB5785_IMPLIED_WAGES_2017    <- c(low = 295e9, high = 338e9)

verdicts <- list()

for (yr in YEARS) {
  message('=== TY', yr)
  cy <- as.character(yr)

  pool <- fread(file.path(RES, sprintf('nonfiler_pool_%d.csv.gz', yr)))
  st   <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  cal  <- fread(file.path(RES, sprintf('calibration_%d.csv', yr)))
  shp  <- fread(file.path(SHAPE, sprintf('nonfiler_age_shape_%d.csv', yr)))
  # OPTIONAL since 2026-08-30: 02_build_residual_anchors.R skips this product
  # for years without QCEW state totals or an SSA EEDATA-SC workbook, which is
  # every year except 2017 and 2022 today. A bare fread() killed the whole
  # acceptance run for those years.
  wm_path <- file.path(ANCH, sprintf('nonfiler_wage_margin_%d.csv', yr))
  wm <- if (file.exists(wm_path)) fread(wm_path) else NULL

  # The handoff year's EMITTED file carries the S19 per-band ssArea scale
  # (05_emit_pool.R), but every benchmark here is on the PEP anchor basis --
  # the scale is a deliberate basis change, not model content, so it is
  # divided back out before comparing. Which state the file is in is DETECTED
  # against the emit audit, not assumed: band adults matching post_adults
  # means scaled (de-scale), matching pre_adults means unscaled (leave), and
  # anything else is a stale-audit stop.
  audit_path <- file.path(RES, sprintf('ssarea_scale_audit_%d.csv', yr))
  if (file.exists(audit_path)) {
    audit <- fread(audit_path)[band != 'total']
    bands <- c('18_25', '26_34', '35_44', '45_54', '55_64', '65p')
    pool[, band := bands[age_group]]
    band_adults <- pool[, .(adults = sum(weight * (1 + (filing_status == 2)))),
                        by = band][audit, on = 'band']
    if (band_adults[, all(abs(adults - post_adults) < 1)]) {
      pool[audit, on = 'band', weight := weight / i.scale_to_ssarea]
      message('  S19: emitted file is ssArea-scaled; scale divided out for ',
              'the PEP-basis comparisons below')
    } else if (!band_adults[, all(abs(adults - pre_adults) < 1)]) {
      stop('pool band adults match neither side of ', audit_path,
           ' -- re-run 05_emit_pool.R so the audit matches the file')
    }
    pool[, band := NULL]
  }

  # person-weight: adults represented by each pool record
  pool[, n_adults := 1 + (filing_status == 2)]
  pool[, w_adults := weight * n_adults]
  pool_adults <- pool[, sum(w_adults)]

  #---------------------------------------------------------------------------
  # The file being replaced: the production Tax-Data non-filer append
  #---------------------------------------------------------------------------
  roots <- read_yaml('./config/interfaces/output_roots.yaml')
  vint  <- read_yaml('./config/interfaces/interface_versions.yaml')$`Tax-Data`$default_vintage
  td <- fread(file.path(roots$production, 'model_data/Tax-Data/v1', vint,
                        'baseline', sprintf('tax_units_%d.csv', yr)),
              select = c('weight', 'filer', 'filing_status', 'age1',
                         'wages', 'txbl_int', 'div_ord', 'div_pref',
                         'kg_st', 'kg_lt', 'gross_ss', 'txbl_pens_dist'))
  dina <- td[filer == 0]
  dina[, n_adults := 1 + (filing_status == 2)]
  dina[, w_adults := weight * n_adults]
  dina_adults <- dina[, sum(w_adults)]
  message(sprintf('  file being replaced: vintage %s, %.2fM non-filer adults',
                  vint, dina_adults / 1e6))

  #---------------------------------------------------------------------------
  # BY CONSTRUCTION 1 -- the level
  #---------------------------------------------------------------------------
  anchor_total <- shp[, sum(residual_nonfiling_adults)]
  netting <- anchor_total - cal[, sum(achieved)]
  message(sprintf(paste('  [construction] level: pool %.2fM + netting %.2fM =',
                        'anchor %.2fM | the replaced file: %.2fM = %.2f of the',
                        'netted target'),
                  pool_adults / 1e6, netting / 1e6, anchor_total / 1e6,
                  dina_adults / 1e6, dina_adults / (anchor_total - netting)))

  #---------------------------------------------------------------------------
  # BY CONSTRUCTION 2 -- age composition, on the NETTED target
  #---------------------------------------------------------------------------
  # The netted target by band is what C4 solved for: anchor minus the
  # claimed-dependent netting. Both files exclude claimed dependents (they
  # ride filer records), so it is the common target.
  band_of <- function(a) as.character(age_band(a))
  pool_band <- pool[, .(pool = sum(w_adults)), by = .(band = band_of(age1))]
  dina_band <- dina[, .(dina = sum(w_adults)), by = .(band = band_of(age1))]
  tgt <- cal[, .(band, target = achieved)]
  ages <- merge(merge(tgt, pool_band, by = 'band', all.x = TRUE),
                dina_band, by = 'band', all.x = TRUE)
  for (v in c('pool', 'dina')) ages[is.na(get(v)), (v) := 0]
  ages[, `:=`(pool_ratio = pool / target, dina_ratio = dina / target)]
  message('  [construction] age composition, ratio to the netted band target:')
  for (i in seq_len(nrow(ages)))
    message(sprintf('    %-6s target %5.2fM | pool %5.2fM = %.3f | replaced %5.2fM = %.3f',
                    ages$band[i], ages$target[i] / 1e6, ages$pool[i] / 1e6,
                    ages$pool_ratio[i], ages$dina[i] / 1e6, ages$dina_ratio[i]))
  age_mard_pool <- ages[, 100 * mean(abs(pool_ratio - 1))]
  age_mard_dina <- ages[, 100 * mean(abs(dina_ratio - 1))]
  message(sprintf('    MARD: pool %.2f%% (by construction) vs replaced %.2f%%',
                  age_mard_pool, age_mard_dina))

  #---------------------------------------------------------------------------
  # INDEPENDENT 1 -- wage mass
  #---------------------------------------------------------------------------
  # BOTH benchmarks below are TY2017 figures. They are only meaningful against
  # the TY2017 pool, so the whole block is gated on the year rather than
  # quietly dividing another year's wages by a 2017 denominator -- which is the
  # same stale-basis error this work exists to remove.
  # NA rather than absent, so the verdict table keeps five rows in every year
  # and a reader can see that the dimension was not comparable rather than
  # wondering why it vanished.
  pool_wages <- dina_wages <- dep_wages <- implied <- NA_real_
  wage_comparable <- !is.null(wm) && yr == 2017L
  if (wage_comparable) {
    ssa_wages <- wm[, sum(ssa_covered_wages)]
    ht2_wages <- wm[, sum(wages_amt)]
    implied   <- ssa_wages - ht2_wages

    pool_wages <- pool[, sum(weight * wages)]
    dina_wages <- dina[, sum(weight * wages)]
    # the concept wedge: claimed dependents work and are not pool units
    dep_wages <- st$persons[is_dependent == TRUE, sum(ASECWT * INCWAGE)]

    message(sprintf(paste('  [INDEPENDENT] wage mass, TWO benchmarks:',
                          'RESIDUAL $%.1fB (SSA covered $%.1fB less HT2 on-return',
                          '$%.1fB; recorded $%.1fB for TY2017) = an upper bound |',
                          'IDENTIFIED (Pub 5785 TY2014-16, projected to TY2017)',
                          '$%.0f-%.0fB -- the SAME BASIS the hazard is calibrated',
                          'on is the TY2014-16 average $242.5B'),
                    implied / 1e9, ssa_wages / 1e9, ht2_wages / 1e9,
                    RESIDUAL_IMPLIED_WAGES_2017 / 1e9,
                    PUB5785_IMPLIED_WAGES_2017[['low']] / 1e9,
                    PUB5785_IMPLIED_WAGES_2017[['high']] / 1e9))
    pub_mid <- mean(PUB5785_IMPLIED_WAGES_2017)
    message(sprintf(paste('    pool $%.1fB = %.2f of the residual, %.2f of the',
                          'identified midpoint | + claimed-dependent wages $%.1fB',
                          '= %.2f / %.2f | replaced file $%.1fB = %.2f / %.2f'),
                    pool_wages / 1e9, pool_wages / implied, pool_wages / pub_mid,
                    dep_wages / 1e9,
                    (pool_wages + dep_wages) / implied,
                    (pool_wages + dep_wages) / pub_mid,
                    dina_wages / 1e9, dina_wages / implied, dina_wages / pub_mid))
  } else {
    message(sprintf(paste('  [INDEPENDENT] wage mass SKIPPED for TY%d: %s. Both',
                          'benchmarks are TY2017 levels and a ratio against',
                          'them would not be on-basis.'),
                    yr, if (is.null(wm)) 'no state wage margin for this year'
                        else 'benchmarks are TY2017-only'))
  }

  #---------------------------------------------------------------------------
  # INDEPENDENT 2+3 -- receipt rates against Pub 5785 Table 1
  #---------------------------------------------------------------------------
  # Table 1 counts persons in the information-return universe of potential
  # non-filers; the rate is the TY2014-16 mean count over the mean total.
  t1 <- fread(PUB5785_T1)[concept == 'count_millions']
  yrs <- c('ty2014', 'ty2015', 'ty2016')
  t1[, avg := rowMeans(as.matrix(.SD)), .SDcols = yrs]
  denom <- t1[measure == 'total_population', avg]
  stopifnot(length(denom) == 1, denom > 40, denom < 60)   # ~50M, or the row moved
  bench <- setNames(t1$avg / denom, t1$measure)

  # THE UNIVERSE WEDGE, and it is the one that decides how to read these
  # rates. Pub 5785's universe is people who appear on an INFORMATION RETURN
  # but filed no 1040 -- so by construction everyone in it has at least one
  # reported income source. Our pool (and DINA's file) also contains adults
  # with NO reported income of any kind, who cannot be in Pub 5785's frame at
  # all and who are disproportionately young and income-less. Comparing raw
  # rates across the two universes understates every receipt rate we compute.
  # So both are reported: the whole pool, and the Pub-5785-comparable subset
  # with any information-return income.
  pool[, any_info_return := wages > 0 | txbl_int > 0 | div_pref > 0 |
                            kg_lt > 0 | txbl_pens_dist > 0 | gross_ss > 0 |
                            ui > 0 | sole_prop != 0]
  dina[, any_info_return := wages > 0 | txbl_int > 0 |
                            div_ord + div_pref > 0 | kg_st + kg_lt > 0 |
                            txbl_pens_dist > 0 | gross_ss > 0]
  pool_info <- pool[any_info_return == TRUE, sum(w_adults)]
  dina_info <- dina[any_info_return == TRUE, sum(w_adults)]
  message(sprintf(paste('  universe wedge: %.1f%% of pool adults have an',
                        'information-return income source (%.2fM of %.2fM);',
                        'replaced file %.1f%%'),
                  100 * pool_info / pool_adults, pool_info / 1e6,
                  pool_adults / 1e6, 100 * dina_info / dina_adults))

  # person-weighted receipt rates, both files, both universes
  rate_pool <- function(cond) pool[eval(cond), sum(w_adults)] / pool_adults
  rate_dina <- function(cond) dina[eval(cond), sum(w_adults)] / dina_adults
  rate_pool_i <- function(cond) pool[eval(cond), sum(w_adults)] / pool_info
  rate_dina_i <- function(cond) dina[eval(cond), sum(w_adults)] / dina_info

  rec <- data.table(
    source = c('wages', 'interest', 'dividends', 'capital_gains',
               'pensions', 'social_security'),
    benchmark = c(bench[['wages']], bench[['interest']], bench[['dividends']],
                  bench[['capital_gains']], bench[['pensions']],
                  bench[['social_security']]),
    pool = c(rate_pool(quote(wages > 0)), rate_pool(quote(txbl_int > 0)),
             rate_pool(quote(div_pref > 0)), rate_pool(quote(kg_lt > 0)),
             rate_pool(quote(txbl_pens_dist > 0)), rate_pool(quote(gross_ss > 0))),
    replaced = c(rate_dina(quote(wages > 0)), rate_dina(quote(txbl_int > 0)),
                 rate_dina(quote(div_ord + div_pref > 0)),
                 rate_dina(quote(kg_st + kg_lt > 0)),
                 rate_dina(quote(txbl_pens_dist > 0)),
                 rate_dina(quote(gross_ss > 0))),
    pool_i = c(rate_pool_i(quote(wages > 0)), rate_pool_i(quote(txbl_int > 0)),
               rate_pool_i(quote(div_pref > 0)), rate_pool_i(quote(kg_lt > 0)),
               rate_pool_i(quote(txbl_pens_dist > 0)),
               rate_pool_i(quote(gross_ss > 0))),
    repl_i = c(rate_dina_i(quote(wages > 0)), rate_dina_i(quote(txbl_int > 0)),
               rate_dina_i(quote(div_ord + div_pref > 0)),
               rate_dina_i(quote(kg_st + kg_lt > 0)),
               rate_dina_i(quote(txbl_pens_dist > 0)),
               rate_dina_i(quote(gross_ss > 0))))
  message(paste('  [INDEPENDENT] receipt rates, person-weighted, vs Pub 5785',
                'Table 1 (i = Pub-5785-comparable universe):'))
  for (i in seq_len(nrow(rec)))
    message(sprintf(paste('    %-16s benchmark %5.1f%% | pool %5.1f%% (i %5.1f%%)',
                          '| replaced %5.1f%% (i %5.1f%%)'),
                    rec$source[i], 100 * rec$benchmark[i],
                    100 * rec$pool[i], 100 * rec$pool_i[i],
                    100 * rec$replaced[i], 100 * rec$repl_i[i]))

  # closeness on the COMPARABLE universe -- the like-for-like statistic
  rec[, `:=`(pool_gap = abs(pool_i - benchmark),
             repl_gap = abs(repl_i - benchmark))]
  message(sprintf(paste('    mean absolute gap on the comparable universe:',
                        'pool %.1fpp vs replaced %.1fpp -- pool closer on %d of %d'),
                  100 * rec[, mean(pool_gap)], 100 * rec[, mean(repl_gap)],
                  rec[pool_gap < repl_gap, .N], nrow(rec)))

  #---------------------------------------------------------------------------
  # Verdict
  #---------------------------------------------------------------------------
  v <- data.table(
    tax_year = yr,
    dimension = c('level', 'age composition', 'wage mass',
                  'investment receipt', 'SS receipt'),
    kind = c('construction', 'construction', 'INDEPENDENT',
             'INDEPENDENT', 'INDEPENDENT'),
    pool = c(sprintf('%.2fM (+%.2fM netting = anchor)', pool_adults / 1e6, netting / 1e6),
             sprintf('MARD %.2f%%', age_mard_pool),
             if (wage_comparable)
               sprintf('$%.1fB = %.2f implied (%.2f with dependents)',
                       pool_wages / 1e9, pool_wages / implied,
                       (pool_wages + dep_wages) / implied)
             else sprintf('$%.1fB (no on-basis benchmark for TY%d)',
                          pool[, sum(weight * wages)] / 1e9, yr),
             sprintf('int %.1f%% div %.1f%% kg %.1f%%',
                     100 * rec[source == 'interest', pool_i],
                     100 * rec[source == 'dividends', pool_i],
                     100 * rec[source == 'capital_gains', pool_i]),
             sprintf('%.1f%%', 100 * rec[source == 'social_security', pool_i])),
    replaced = c(sprintf('%.2fM = %.2f of target', dina_adults / 1e6,
                         dina_adults / (anchor_total - netting)),
                 sprintf('MARD %.2f%%', age_mard_dina),
                 if (wage_comparable)
                   sprintf('$%.1fB = %.2f implied', dina_wages / 1e9,
                           dina_wages / implied)
                 else sprintf('$%.1fB', dina[, sum(weight * wages)] / 1e9),
                 sprintf('int %.1f%% div %.1f%% kg %.1f%%',
                         100 * rec[source == 'interest', repl_i],
                         100 * rec[source == 'dividends', repl_i],
                         100 * rec[source == 'capital_gains', repl_i]),
                 sprintf('%.1f%%', 100 * rec[source == 'social_security', repl_i])),
    benchmark = c(sprintf('%.2fM anchor', anchor_total / 1e6),
                  'netted band target',
                  if (wage_comparable) sprintf('$%.1fB implied', implied / 1e9)
                  else 'TY2017-only; not compared',
                  sprintf('int %.1f%% div %.1f%% kg %.1f%%',
                          100 * bench[['interest']], 100 * bench[['dividends']],
                          100 * bench[['capital_gains']]),
                  sprintf('%.1f%%', 100 * bench[['social_security']])),
    pool_better = c(TRUE, age_mard_pool < age_mard_dina,
                    if (wage_comparable)
                      abs(pool_wages - implied) < abs(dina_wages - implied)
                    else NA,
                    rec[source %in% c('interest','dividends','capital_gains'),
                        mean(pool_gap) < mean(repl_gap)],
                    rec[source == 'social_security', pool_gap < repl_gap]))
  verdicts[[cy]] <- v
  message('')
  message('  VERDICT:')
  for (i in seq_len(nrow(v)))
    message(sprintf('    %-19s [%-12s] %s', v$dimension[i], v$kind[i],
                    fifelse(v$pool_better[i], 'pool better', 'POOL NOT BETTER')))
  fwrite(v, file.path(RES, sprintf('acceptance_%d.csv', yr)))
  message('  wrote acceptance_', yr, '.csv')
}

all_v <- rbindlist(verdicts)
message('\n=== C8 summary: ', all_v[pool_better == TRUE, .N], ' of ',
        nrow(all_v), ' dimension-years favour the pool ===')
if (all_v[pool_better == FALSE, .N])
  message('NOT BETTER on: ', all_v[pool_better == FALSE,
          paste(sprintf('TY%d %s', tax_year, dimension), collapse = '; ')])

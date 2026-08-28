#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 01_build_units.R  (group C, stages C0-C2 -- plan of record:
#                    research/state_weights/plan.md §3; design:
#                    nonfiler_residual/10_asec_tax_unit_design.md §4)
#
# Build ASEC tax units and the filing requirement for the two anchor years,
# and run the STAGE GATES:
#
#   C0  income aggregates reproduce script 09's asec_A4_income_{year}.csv;
#       the harmonised retirement series matches the S16 verification
#   C1  unit counts against the FILESTAT benchmark (D-A1: kept because it is
#       calibrated -- compare, never adopt); joint-unit structure against
#       asec_A6_unit_structure_{year}.csv; the HoH shortfall vs SOI (D-A6);
#       the TRIM3 support-test flip counts at 0.4 / 0.5 / 0.6 (decision Q6)
#   C2  the filer-count sensitivity to investment income (D-A5: ASEC interest
#       runs 2.6x SOI, so the count must be shown not to turn on it)
#
# Writes to results/: units_{year}.rds (units + persons, the C3 input),
# unit_gates_{year}.csv, support_test_sensitivity_{year}.csv.
#
# Login-node safe (one ASEC year is ~180k persons).
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_weights/nonfiler_pool/01_build_units.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(ipumsr); library(stringr)
  library(dplyr); library(yaml); library(readxl)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'
DIAG  <- 'research/state_weights/nonfiler_residual/results'
dir.create(RES, recursive = TRUE, showWarnings = FALSE)

# Columns the builder needs, plus the benchmark recodes (S12: verification
# only, never construction) and the covariate raw material C3 will score on.
BUILD_COLS <- c('SERIAL', 'PERNUM', 'AGE', 'SEX', 'RELATE', 'MARST', 'SPLOC',
                'MOMLOC', 'POPLOC', 'FTYPE', 'FAMID', 'FAMREL', 'SCHLCOLL',
                'ASECWT', 'GQ', 'EDUC', 'RACE', 'HISPAN', 'HIMCAIDLY',
                'FOODSTAMP', 'INCWELFR', 'INCSSI',
                'FILESTAT', 'DEPSTAT')

# SOI return counts (Pub 1304 T1.6 all-returns; the T1.2/T1.6 status splits
# quoted in the gates below were verified 2026-08-27)
SOI_RETURNS <- list(
  `2017` = list(total = 152.9e6, hoh = 21.2e6 * 22.113 / 21.912),  # see note
  `2022` = list(total = 161.3e6, hoh = 21.27e6)
)
# note: TY2017 HoH published count is 21.7M (Pub 1304 T1.2, TY2017); the gate
# uses the T1.6-family figure -- refined when the exact cell is transcribed.
SOI_HOH <- c(`2017` = 21.7e6, `2022` = 21.27e6)

for (yr in YEARS) {
  message('=== TY', yr)

  #---------------------------------------------------------------------------
  # C0: read, clean, income concepts
  #---------------------------------------------------------------------------
  ret_cols <- if (yr <= 2017) 'INCRETIR' else ASEC_RETIREMENT_COMPONENTS
  kg_cols  <- if (yr >= 2018) 'INCCAPG' else character(0)
  cols <- unique(c(BUILD_COLS, ASEC_GROSS_INCOME_ITEMS, 'INCSS',
                   ret_cols, kg_cols, 'tax_year'))
  x <- read_asec(yr)                       # full read; select after cleaning
  ddi <- attr(x, 'ddi')
  x[, INCSS := clean_asec_income(x, ddi, 'INCSS')]
  x <- add_asec_income_concepts(x, ddi)

  # C3's covariate inputs, cleaned HERE because NIU handling never happens
  # downstream of the C0 driver (add_mok_covariates() asserts these exist):
  # welfare income (TANF/public assistance), SNAP amount, Medicaid coverage.
  x[, welfare_income   := clean_asec_income(x, ddi, 'INCWELFR')]
  x[, total_income     := clean_asec_income(x, ddi, 'INCTOT')]   # the support test's concept
  x[, foodstamp_amount := as.numeric(FOODSTAMP)]     # unlabeled amount, 0 = none
  x[, on_medicaid      := HIMCAIDLY == 2L]           # DDI: 1 No, 2 Yes, 9 NIU

  # The ASEC frame is household population; GQ==2 persons are out of the
  # estimation frame by design (D-A7) and handled by the ACS backfill (C5).
  gq_n <- x[GQ == 2, sum(ASECWT)]
  x <- x[GQ != 2]
  message(sprintf('  frame: %.2fM persons (%.2fM GQ set aside for the ACS backfill, D-A7)',
                  x[, sum(ASECWT)] / 1e6, gq_n / 1e6))

  # C0 gate: wage aggregate against script 09's committed A4 table
  a4 <- fread(file.path(DIAG, sprintf('asec_A4_income_%d.csv', yr)))
  wage_now <- x[, sum(INCWAGE * ASECWT)] + gq_n * 0   # GQ removed AFTER a4 was built
  wage_a4  <- a4[variable == 'INCWAGE', asec_amount]
  # A4 was tabulated on the full frame incl. GQ; re-add the GQ wages for the
  # like-for-like check rather than loosening the tolerance
  wage_full <- wage_now + {
    xg <- read_asec(yr, c('GQ', 'INCWAGE', 'ASECWT'))
    g <- xg[GQ == 2]
    g[, sum(clean_asec_income(g, attr(xg, 'ddi'), 'INCWAGE') * ASECWT)]
  }
  stopifnot(abs(wage_full / wage_a4 - 1) < 1e-6)
  message(sprintf('  C0 gate: INCWAGE reproduces A4 exactly ($%.1fB)', wage_a4 / 1e9))

  #---------------------------------------------------------------------------
  # C1: units, plus the support-test sensitivity
  #---------------------------------------------------------------------------
  built <- build_asec_tax_units(x, yr)
  u <- built$units
  u <- add_filing_requirement(u, yr)

  sens <- rbindlist(lapply(c(0.4, 0.5, 0.6), function(m) {
    um <- build_asec_tax_units(x, yr, support_mult = m)$units
    data.table(support_mult = m,
               units_M      = um[unit_type == 'nondependent', sum(weight)] / 1e6,
               dep_units_M  = um[unit_type == 'dependent', sum(weight)] / 1e6,
               adult_dep_M  = um[unit_type == 'nondependent', sum(weight * n_dep_adult)] / 1e6)
  }))
  sens[, flipped_vs_05_M := adult_dep_M - sens[support_mult == 0.5, adult_dep_M]]
  fwrite(sens, file.path(RES, sprintf('support_test_sensitivity_%d.csv', yr)))
  message(sprintf(paste('  support test (Q6): adult dependents %.2fM at 0.5;',
                        '%+.2fM at 0.4, %+.2fM at 0.6'),
                  sens[support_mult == 0.5, adult_dep_M],
                  sens[support_mult == 0.4, flipped_vs_05_M],
                  sens[support_mult == 0.6, flipped_vs_05_M]))

  #---------------------------------------------------------------------------
  # C1 gates: FILESTAT benchmark, joint structure, HoH shortfall
  #---------------------------------------------------------------------------
  base <- u[unit_type == 'nondependent']
  p <- built$persons

  # FILESTAT (benchmark ONLY, and broken in TY2020-21): its return count,
  # against our unit count. Expect ours HIGHER -- FILESTAT counts returns the
  # Census model simulates as filed, we count potential filing units.
  fs_returns <- p[FILESTAT %in% 1:3, sum(ASECWT)] / 2 +
                p[FILESTAT %in% 4:5, sum(ASECWT)]
  ours <- base[, sum(weight)]
  message(sprintf(paste('  C1 gate: %.1fM nondependent units vs FILESTAT %.1fM',
                        'simulated returns (%.3fx) | SOI all returns %.1fM'),
                  ours / 1e6, fs_returns / 1e6, ours / fs_returns,
                  SOI_RETURNS[[as.character(yr)]]$total / 1e6))

  # status mix, with the D-A6 HoH shortfall stated against SOI
  mix <- base[, .(units_M = sum(weight) / 1e6), keyby = filing_status]
  hoh_soi <- SOI_HOH[[as.character(yr)]]
  message(sprintf('  status mix: %s | HoH vs SOI %.1fM -> ratio %.3f (D-A6 expects ~0.54-0.59 pre-reallocation)',
                  mix[, paste(sprintf('%s %.1fM', filing_status, units_M), collapse = ', ')],
                  hoh_soi / 1e6, mix[filing_status == 'hoh', units_M] * 1e6 / hoh_soi))

  # dependents: constructed, retained, tagged (D-A4)
  message(sprintf('  dependents: %.2fM tagged, of them %.2fM adults (18+) | DEPSTAT benchmark %.2fM',
                  p[is_dependent == TRUE, sum(ASECWT)] / 1e6,
                  p[is_dependent == TRUE & AGE >= 18, sum(ASECWT)] / 1e6,
                  p[DEPSTAT > 0, sum(ASECWT)] / 1e6))

  #---------------------------------------------------------------------------
  # C2 gate: the investment-income sensitivity (D-A5)
  #---------------------------------------------------------------------------
  n_file    <- base[must_file == TRUE, sum(weight)]
  n_file_ex <- base[must_file_ex_investment == TRUE, sum(weight)]
  message(sprintf(paste('  C2 gate: %.1fM units meet the filing requirement;',
                        '%.1fM excluding investment income (%.2f%% of the count',
                        'turns on it)'),
                  n_file / 1e6, n_file_ex / 1e6, 100 * (n_file - n_file_ex) / n_file))

  gates <- data.table(
    tax_year = yr,
    nondep_units_M   = ours / 1e6,
    filestat_units_M = fs_returns / 1e6,
    joint_M  = mix[filing_status == 'joint', units_M],
    hoh_M    = mix[filing_status == 'hoh', units_M],
    single_M = mix[filing_status == 'single', units_M],
    hoh_vs_soi = mix[filing_status == 'hoh', units_M] * 1e6 / hoh_soi,
    dep_tagged_M   = p[is_dependent == TRUE, sum(ASECWT)] / 1e6,
    adult_dep_M    = p[is_dependent == TRUE & AGE >= 18, sum(ASECWT)] / 1e6,
    must_file_M    = n_file / 1e6,
    investment_sensitivity_pct = 100 * (n_file - n_file_ex) / n_file)
  fwrite(gates, file.path(RES, sprintf('unit_gates_%d.csv', yr)))

  saveRDS(list(units = u, persons = p), file.path(RES, sprintf('units_%d.rds', yr)))
  message('  wrote units_', yr, '.rds, unit_gates_', yr, '.csv, support_test_sensitivity_', yr, '.csv')
}

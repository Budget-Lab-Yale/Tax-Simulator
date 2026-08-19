#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 01_fetch_residual_inputs.R  (Stage D, research/state_weights/nonfiler_residual_design.md §4.1)
#
# Fetch the external inputs for the residual non-filer anchors into new
# shared-store families (one manifest.csv per family, IRS-Ind conventions):
#
#   Census-PEP/    state x single-year-age x sex x race/origin population
#                  estimates: sc-est2020int-alldata6.csv (intercensal 2010-2020,
#                  covers TY2017) and sc-est2024-alldata6.csv (2020-2024,
#                  covers TY2022). Totals rows: SEX==0 & ORIGIN==0, sum RACE 1-6.
#   BLS-QCEW/      state total covered employment and wages from the CEW open
#                  data API (own_code 0, industry 10, state agglvl 50) --
#                  cross-check only, never a target (design memo §3.1).
#   SSA-OASDI-SC/  OASDI beneficiaries by state and county   } ssa.gov returns
#   SSA-EEDATA-SC/ covered workers/earnings by state         } 403 to this host
#                  (Akamai bot-blocking of the cluster egress IP, verified
#                  2026-08-16 with browser user-agents). The script creates the
#                  family with a README_MANUAL_DOWNLOAD.md and skips gracefully;
#                  re-run after placing the files to register them in the
#                  manifest.
#
# Pub 1304 by-size tables are ALREADY in raw_data/IRS-Ind/national/by_size/
# (maintained by the IRS-Ind downloader) -- nothing to fetch here.
# IRS Pub 5785 (above-threshold non-filer composition) is fetched to the
# scratch working dir for hand-transcription into resources/.
#
# Run from the repo root. Login-node safe (all downloads are small).
#   Rscript research/state_weights/nonfiler_residual/01_fetch_residual_inputs.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(yaml); library(tools)
})
source('src/data/state_weights.R')   # raw_data_root(), FIPS_TO_STATE

ANCHOR_YEARS <- c(2017, 2022)
SCRATCH <- '/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/nonfiler_residual'
dir.create(SCRATCH, recursive = TRUE, showWarnings = FALSE)

#---------------------------------------
# Manifest helpers (IRS-Ind conventions)
#---------------------------------------

manifest_row <- function(path, url, year) {
  data.table(path = path, source_url = url, year = year,
             bytes = file.size(path), md5 = as.character(md5sum(path)),
             retrieved = as.character(Sys.Date()))
}

write_manifest <- function(family_dir, rows) {
  mf <- file.path(family_dir, 'manifest.csv')
  # `retrieved` must be read back as character: fread parses the ISO date as
  # IDate, and rbindlist then coerces the incoming character date to integer,
  # silently blanking the retrieval date of every already-registered file.
  old <- if (file.exists(mf)) fread(mf, colClasses = c(retrieved = 'character')) else NULL
  out <- rbindlist(list(old, rows), use.names = TRUE, fill = TRUE)
  out <- unique(out, by = 'path', fromLast = TRUE)
  fwrite(out[order(path)], mf)
}

# Idempotent download; returns a manifest row (or NULL on skip/failure)
fetch_file <- function(url, dest, year = NA_integer_) {
  if (file.exists(dest) && file.size(dest) > 0) {
    message('  exists, skipping: ', basename(dest))
    return(manifest_row(dest, url, year))
  }
  ok <- tryCatch({
    download.file(url, dest, mode = 'wb', quiet = TRUE)
    file.exists(dest) && file.size(dest) > 0
  }, error = function(e) { message('  FAILED: ', url, ' (', conditionMessage(e), ')'); FALSE })
  if (!ok) { unlink(dest); return(NULL) }
  message('  fetched: ', basename(dest), ' (', round(file.size(dest) / 1e6, 1), ' MB)')
  manifest_row(dest, url, year)
}

#---------------------------------------
# 1. Census PEP
#---------------------------------------

message('Census-PEP')
pep_dir <- file.path(raw_data_root(), 'Census-PEP')
dir.create(pep_dir, showWarnings = FALSE)
pep_files <- list(
  list(url  = 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/intercensal/state/asrh/sc-est2020int-alldata6.csv',
       year = 2017L),
  list(url  = 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/asrh/sc-est2024-alldata6.csv',
       year = 2022L))
pep_rows <- rbindlist(lapply(pep_files, function(f) {
  fetch_file(f$url, file.path(pep_dir, basename(f$url)), f$year)
}))
write_manifest(pep_dir, pep_rows)

#---------------------------------------
# 2. BLS QCEW state totals
#---------------------------------------

message('BLS-QCEW')
qcew_dir <- file.path(raw_data_root(), 'BLS-QCEW')
dir.create(qcew_dir, showWarnings = FALSE)
qcew_rows <- list()
for (yr in ANCHOR_YEARS) {
  out_file <- file.path(qcew_dir, sprintf('qcew_state_totals_%d.csv', yr))
  if (file.exists(out_file)) { message('  exists, skipping: ', basename(out_file)); next }
  areas <- c('US000', sprintf('%02d000', as.integer(names(FIPS_TO_STATE))))
  keep_cols <- c('area_fips', 'year', 'annual_avg_estabs', 'annual_avg_emplvl',
                 'total_annual_wages', 'avg_annual_pay')
  st_tabs <- lapply(areas, function(a) {
    url <- sprintf('https://data.bls.gov/cew/data/api/%d/a/area/%s.csv', yr, a)
    x <- tryCatch(fread(url, showProgress = FALSE,
                        colClasses = list(character = 'area_fips')),
                  error = function(e) { message('  FAILED: ', a); NULL })
    if (is.null(x)) return(NULL)
    # Total covered, all industries: own 0 / industry "10"; agglvl 50 = state
    # total, 10 = national total
    x[own_code == 0 & industry_code == '10' & agglvl_code %in% c(10L, 50L),
      ..keep_cols]
  })
  qcew <- rbindlist(st_tabs)
  stopifnot(nrow(qcew) == length(areas))
  # State area codes are {fips}000: recover the state fips by integer division
  qcew[, state := fifelse(area_fips == 'US000', 'US',
                          FIPS_TO_STATE[as.character(as.integer(sub('US', '99', area_fips)) %/% 1000)])]
  stopifnot(!anyNA(qcew$state))
  fwrite(qcew, out_file)
  message('  wrote ', basename(out_file), ' (', nrow(qcew), ' areas)')
  qcew_rows[[length(qcew_rows) + 1]] <- manifest_row(
    out_file, sprintf('https://data.bls.gov/cew/data/api/%d/a/area/{AREA}.csv', yr), yr)
}
if (length(qcew_rows)) write_manifest(qcew_dir, rbindlist(qcew_rows))

#---------------------------------------
# 3. SSA statcomps (blocked host: create family + manual instructions,
#    register any manually-placed files)
#---------------------------------------

# SSA names its per-year workbooks with a two-digit DATA year (oasdi_sc22.xlsx
# = data year 2022). The flat-series JSONs span 1999-2025 and have no year.
ssa_data_year <- function(path) {
  yr2 <- sub('^[a-z]+_sc([0-9]{2})\\.xlsx?$', '\\1', basename(path))
  if (yr2 == basename(path)) NA_integer_ else 2000L + as.integer(yr2)
}

ssa_families <- list(
  `SSA-OASDI-SC` = list(
    what = 'OASDI Beneficiaries by State and County',
    url  = 'https://www.ssa.gov/policy/docs/statcomps/oasdi_sc/',
    need = paste('Per anchor year (2017, 2022): the state-level table(s) with',
                 'beneficiary counts by state x age group (the 65-and-over rows',
                 'are the anchor margin -- design memo §3.1). Download the',
                 'year\'s tables (xlsx or the single "entire publication" file)',
                 'and place them here named oasdi_sc_{year}_*.xlsx.')),
  `SSA-EEDATA-SC` = list(
    what = 'Earnings and Employment Data by State and County (covered workers)',
    url  = 'https://www.ssa.gov/policy/docs/statcomps/eedata_sc/',
    need = paste('Per anchor year (2017, 2022): the state table with number of',
                 'persons with Social-Security-covered wages and total covered',
                 'wages by state (the W-2 universe regardless of filing).',
                 'Place here named eedata_sc_{year}_*.xlsx.')))

for (fam in names(ssa_families)) {
  message(fam)
  fdir <- file.path(raw_data_root(), fam)
  dir.create(fdir, showWarnings = FALSE)
  info <- ssa_families[[fam]]
  readme <- file.path(fdir, 'README_MANUAL_DOWNLOAD.md')
  writeLines(c(
    sprintf('# %s -- manual download required', fam),
    '',
    sprintf('Source: %s (%s)', info$url, info$what),
    '',
    'ssa.gov returns HTTP 403 to this cluster\'s egress IP (bot blocking,',
    'verified 2026-08-16), so these files must be downloaded on a workstation',
    'and copied here.',
    '',
    paste('Needed:', info$need),
    '',
    'Then re-run 01_fetch_residual_inputs.R to register the files in',
    'manifest.csv. Consumed by: Tax-Simulator',
    'research/state_weights/nonfiler_residual/ (Stage D anchors).'), readme)
  # NOTES.md documents the family (IRS-Ind convention); it is not data
  placed <- setdiff(list.files(fdir, full.names = TRUE),
                    file.path(fdir, c(basename(readme), 'manifest.csv', 'NOTES.md')))
  if (length(placed)) {
    write_manifest(fdir, rbindlist(lapply(placed, function(p)
      manifest_row(p, paste0(info$url, ' (manual)'), ssa_data_year(p)))))
    message('  registered ', length(placed), ' manually-placed file(s)')
  } else {
    message('  BLOCKED (ssa.gov 403) -- wrote README_MANUAL_DOWNLOAD.md, no files yet')
  }
}

#---------------------------------------
# 4. IRS Pub 5785 (transcription source)
#---------------------------------------

message('IRS Pub 5785 (to scratch, for hand-transcription)')
invisible(fetch_file('https://www.irs.gov/pub/irs-pdf/p5785.pdf',
                     file.path(SCRATCH, 'p5785.pdf')))

message('Done.')

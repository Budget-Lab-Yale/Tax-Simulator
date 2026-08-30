#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 13_verify_ssa_backfill.R  (group D, proposal 6)
#
# Run this after hand-copying SSA EEDATA-SC workbooks into the raw-data store.
# ssa.gov returns 403 to this cluster (re-verified 2026-08-30 with a browser
# user-agent and against web.archive.org), so those files arrive by hand and
# nothing has checked them before the anchor build tries to parse them.
#
# What it does, in order:
#   1. reports which years are present and which are still missing
#   2. actually PARSES each new one through read_ssa_eedata_hi() -- a file that
#      downloaded as an error page, or as the county rather than the state
#      table, is the failure mode worth catching here rather than three
#      scripts later
#   3. registers it in manifest.csv with size and md5
#   4. prints the single command that rebuilds what the file unblocks
#
# Login-node safe, seconds to run.
#   Rscript research/state_weights/nonfiler_residual/13_verify_ssa_backfill.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr)
})
source('src/data/state_weights.R')

FAMILY   <- 'SSA-EEDATA-SC'
WANTED   <- 2014:2023        # the years the non-filer build can use
NEEDED   <- 2014:2016        # calibration years, the ones that were missing
SRC_URL  <- 'https://www.ssa.gov/policy/docs/statcomps/eedata_sc/ (manual)'

dir_path <- file.path(raw_data_root(), FAMILY)
stopifnot(dir.exists(dir_path))

present <- WANTED[file.exists(vapply(WANTED,
                                     function(y) ssa_workbook(FAMILY, y),
                                     character(1)))]
missing <- setdiff(WANTED, present)

message('=== ', FAMILY, ' in ', dir_path)
message('  present: ', if (length(present)) paste(present, collapse = ', ') else 'none')
if (length(missing)) {
  message('  MISSING: ', paste(missing, collapse = ', '))
  message('    expected filenames: ',
          paste(basename(vapply(missing, function(y) ssa_workbook(FAMILY, y),
                                character(1))), collapse = ', '))
}

#-------------------------------------------------------------------------------
# Parse each present file. A silent 403 saves an HTML error page under an .xlsx
# name; a wrong pick from the landing page saves the county table. Both parse
# to something, so check the shape, not just that the file opens.
#-------------------------------------------------------------------------------
ok <- integer(0)
for (y in present) {
  f <- ssa_workbook(FAMILY, y)
  res <- tryCatch({
    ee <- read_ssa_eedata_hi(y)
    p  <- ee$persons
    stopifnot(is.data.table(p), nrow(p) >= 50,
              all(c('state', 'hi_persons_wage_salary',
                    'hi_wage_salary_earnings') %in% names(p)),
              all(p$hi_persons_wage_salary > 0))
    sprintf('%2d states | %.1fM covered persons | $%.3fT covered wages',
            nrow(p), sum(p$hi_persons_wage_salary) / 1e6,
            sum(p$hi_wage_salary_earnings) / 1e12)
  }, error = function(e) paste('PARSE FAILED --', conditionMessage(e)))

  bad <- startsWith(res, 'PARSE FAILED')
  message(sprintf('  %d  %-18s %s', y, basename(f), res))
  if (!bad) ok <- c(ok, y)
}

#-------------------------------------------------------------------------------
# Register in the manifest (append only; existing rows are left alone)
#-------------------------------------------------------------------------------
mpath <- file.path(dir_path, 'manifest.csv')
man   <- fread(mpath, colClasses = 'character')
added <- 0L
for (y in ok) {
  f <- ssa_workbook(FAMILY, y)
  if (f %in% man$path) next
  man <- rbind(man, data.table(
    path       = f,
    source_url = SRC_URL,
    year       = as.character(y),
    bytes      = as.character(file.size(f)),
    md5        = as.character(tools::md5sum(f)),
    retrieved  = as.character(Sys.Date())), fill = TRUE)
  added <- added + 1L
}
if (added) {
  setorder(man, year, path)
  fwrite(man, mpath)
  message(sprintf('  registered %d new file(s) in manifest.csv', added))
} else {
  message('  manifest.csv already current')
}

#-------------------------------------------------------------------------------
# What to run next
#-------------------------------------------------------------------------------
unblocked <- intersect(ok, NEEDED)
message('')
if (!length(setdiff(NEEDED, ok))) {
  message('=== all three calibration years are in. Rebuild their state products:')
  message('    Rscript research/state_weights/nonfiler_residual/02_build_residual_anchors.R ',
          paste(NEEDED, collapse = ' '))
  message('  That writes nonfiler_wage_margin_{year}.csv and ssa_age_margin_{year}.csv,')
  message('  which the anchor build has been SKIPPING for those years. QCEW state')
  message('  totals for 2014-2016 are already in place, so nothing else is missing.')
  message('  The national anchor and age shape do not change -- they never needed SSA.')
} else if (length(unblocked)) {
  message('=== partial. Rebuild what is now available:')
  message('    Rscript research/state_weights/nonfiler_residual/02_build_residual_anchors.R ',
          paste(unblocked, collapse = ' '))
  message('  Still missing: ', paste(setdiff(NEEDED, ok), collapse = ', '))
} else {
  message('=== nothing new. See ', file.path(dir_path, 'README_MANUAL_DOWNLOAD.md'))
}

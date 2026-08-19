#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 06_verify_ssa_inputs.R  (Stage D)
#
# Verify that the manually-downloaded SSA statcomps files are the RIGHT files,
# before anything is built on them. ssa.gov 403-blocks automated retrieval
# (Akamai bot-blocking -- it refuses curl, .NET and the WebFetch service alike,
# on TLS fingerprint, not user agent), so these two families are placed by hand
# and nothing has checked them until now.
#
# What it does: for each anchor year, find the workbook, read the national
# ("All areas") row of the sheet the anchors will consume, and compare against
# control totals verified against the publications on 2026-08-19. A file that
# matches these is the right file; a file that does not is the wrong year, the
# wrong publication, or a per-state extract rather than the all-tables workbook.
#
# Login-node safe (reads two ~2 MB workbooks per year).
#   Rscript other/state_tax_research/nonfiler_residual/06_verify_ssa_inputs.R
#   Rscript ... 06_verify_ssa_inputs.R --dir /some/staging/dir   # test a copy
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readxl); library(yaml)
})

args     <- commandArgs(trailingOnly = TRUE)
dir_over <- if ('--dir' %in% args) args[which(args == '--dir') + 1] else NULL

root <- if (!is.null(dir_over)) dir_over else {
  file.path(read_yaml('./config/interfaces/output_roots.yaml')$production, 'raw_data')
}
message('raw data root: ', root, '\n')

#-----------------------------------------------------------------------------
# Control totals, verified against the publications 2026-08-19.
#
# OASDI-SC "Table 2" = Number of beneficiaries in current-payment status by
# state, type of benefit, and sex of beneficiaries aged 65 or older. The last
# two measures (65+ men, 65+ women) ARE the D6 age margin.
#
# EEDATA-SC "Table 1" = Number of persons with Social-Security-covered
# earnings by state and sex, split total / wage-and-salary / self-employed,
# plus taxable earnings in $thousands. `wage_salary` is the covered-worker
# margin (the W-2 universe regardless of filing).
#-----------------------------------------------------------------------------

CONTROLS <- list(
  `SSA-OASDI-SC` = list(
    sheet  = 'Table 2',
    glob   = 'oasdi_sc*',
    fields = c('total', 'ret_workers', 'ret_spouses', 'ret_children',
               'surv_widows_parents', 'surv_children', 'di_workers',
               'di_spouses', 'di_children', 'aged65_men', 'aged65_women'),
    expect = list(
      `2017` = c(61903360, 42446992, 2375575, 675261, 4090523, 1903757,
                 8695475, 126154, 1589623, 20385750, 25423026),
      `2022` = c(65994457, 48587883, 2022892, 682295, 3840827, 2019827,
                 7604098, 90972, 1145663, 23302580, 28750227))),
  `SSA-EEDATA-SC` = list(
    sheet  = 'Table 1',
    glob   = 'eedata_sc*',
    fields = c('total', 'wage_salary', 'self_employed', 'taxable_earnings_k'),
    expect = list(
      `2017` = c(173010000, 161986000, 19615000, 6997733976),
      `2022` = c(180675999, 168525999, 21203000, 9201500022)))
)

ANCHOR_YEARS <- c(2017, 2022)

# The national row is labelled "All areas" in both publications' state tables
# (older OASDI vintages use "United States" in Table 1 -- not the sheet we read,
# but match both so a vintage change surfaces as a mismatch, not a crash).
NATIONAL_LABELS <- c('All areas', 'United States')

find_workbook <- function(fdir, glob, year) {
  cand <- list.files(fdir, pattern = '\\.xlsx?$', full.names = TRUE)
  if (!length(cand)) return(NA_character_)
  yr2 <- substr(as.character(year), 3, 4)
  # prefer a filename naming the 4-digit year, then the 2-digit SSA suffix
  hit <- cand[grepl(as.character(year), basename(cand), fixed = TRUE)]
  if (!length(hit)) hit <- cand[grepl(paste0(sub('\\*$', '', glob), yr2, '\\.xlsx?$'),
                                      basename(cand))]
  # never accept a per-state extract (two-letter stem) as the all-tables book
  hit <- hit[!grepl('^[a-z]{2}\\.xlsx?$', basename(hit))]
  if (!length(hit)) NA_character_ else hit[1]
}

national_numbers <- function(path, sheet, n) {
  x <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE,
                                   .name_repair = 'minimal'))
  x <- as.data.table(lapply(x, as.character))
  lab <- apply(x, 1, function(r) any(trimws(r) %in% NATIONAL_LABELS, na.rm = TRUE))
  i <- which(lab)[1]
  if (is.na(i)) return(NULL)
  v <- suppressWarnings(as.numeric(unlist(x[i])))
  v <- v[!is.na(v)]
  if (length(v) < n) return(NULL)
  v[seq_len(n)]
}

pass_all <- TRUE
report  <- list()

for (fam in names(CONTROLS)) {
  spec <- CONTROLS[[fam]]
  fdir <- file.path(root, fam)
  message('=== ', fam)
  if (!dir.exists(fdir)) {
    message('  MISSING DIRECTORY: ', fdir); pass_all <- FALSE; next
  }
  files <- setdiff(list.files(fdir), c('manifest.csv', 'README_MANUAL_DOWNLOAD.md'))
  message('  files present: ', if (length(files)) paste(files, collapse = ', ') else '(none)')

  for (yr in ANCHOR_YEARS) {
    wb <- find_workbook(fdir, spec$glob, yr)
    if (is.na(wb)) {
      message('  TY', yr, ': NO WORKBOOK FOUND'); pass_all <- FALSE
      report[[length(report) + 1]] <- data.table(family = fam, year = yr,
        file = NA_character_, field = NA_character_, expected = NA_real_,
        found = NA_real_, ok = FALSE)
      next
    }
    exp <- spec$expect[[as.character(yr)]]
    got <- tryCatch(national_numbers(wb, spec$sheet, length(exp)),
                    error = function(e) { message('  read error: ',
                                                  conditionMessage(e)); NULL })
    if (is.null(got)) {
      message('  TY', yr, ': could not read a national row from sheet "',
              spec$sheet, '" in ', basename(wb)); pass_all <- FALSE
      next
    }
    ok <- got == exp
    report[[length(report) + 1]] <- data.table(
      family = fam, year = yr, file = basename(wb), field = spec$fields,
      expected = exp, found = got, ok = ok)
    if (all(ok)) {
      message('  TY', yr, ': PASS  (', basename(wb), ')')
    } else {
      pass_all <- FALSE
      message('  TY', yr, ': FAIL  (', basename(wb), ') -- ', sum(!ok),
              ' of ', length(ok), ' control totals differ')
      bad <- data.table(field = spec$fields, expected = exp, found = got)[!ok]
      print(bad)
    }
  }
  message('')
}

rep <- rbindlist(report, fill = TRUE)
out <- file.path('other/state_tax_research/nonfiler_residual/results',
                 'ssa_input_verification.csv')
if (dir.exists(dirname(out))) { fwrite(rep, out); message('wrote ', out) }

#-----------------------------------------------------------------------------
# Derived margins, printed so the numbers that matter are visible at a glance
#-----------------------------------------------------------------------------

o <- rep[family == 'SSA-OASDI-SC' & field %in% c('aged65_men', 'aged65_women')]
if (nrow(o)) {
  b <- o[, .(beneficiaries_65p = sum(found)), by = year]
  message('\nOASDI beneficiaries aged 65+ (national, from the file):')
  print(b)
}
e <- rep[family == 'SSA-EEDATA-SC' & field == 'wage_salary']
if (nrow(e)) {
  message('\nPersons with covered wage-and-salary earnings (national):')
  print(e[, .(year, wage_salary = found)])
}

if (!pass_all) {
  message('\nVERIFICATION FAILED -- do not build anchors on these files.')
  quit(status = 1)
}
message('\nAll SSA control totals match. Files are the expected publications.')

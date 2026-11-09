#-------------------------------------------------------------------------------
# lint_estate_module.R
#
# Static runscript contract check for the estate reporting module (the
# activation bug of 2026-07-16: estate-only rows never loaded the module that
# owns the Kopczuk-Slemrod response and the evasion->estate link, because it
# lived inside wealth/avoidance and activation was a per-row choice).
#
# For every scenario row in config/runscripts/top_tax/*.csv (and every row
# anywhere that loads wealth/avoidance):
#   (1) a behavior stack containing evasion/ or wealth/ must also contain
#       estate/ (after them);
#   (2) rows carrying estate/ must register the estate MTR in mtr_vars, and
#       the runscript's baseline row must register it too (the baseline leg of
#       the own-rate response is load-bearing).
#
# Run via sbatch other/top_tax/tests/test_hidden_ledger_guards.sbatch (which
# chains this lint after the unit tests), or standalone with Rscript.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(readr); library(dplyr) })

`%||%` = function(a, b) if (is.null(a) || is.na(a)) b else a


files = c(Sys.glob('./config/runscripts/top_tax/*.csv'),
          Sys.glob('./config/runscripts/*.csv'),
          Sys.glob('./config/runscripts/tests/*.csv'))

n_fail = 0
fail = function(...) { cat('[FAIL]', ..., '\n'); n_fail <<- n_fail + 1 }

for (f in files) {
  rs = tryCatch(suppressWarnings(read_csv(f, show_col_types = FALSE,
                                          col_types = cols(.default = 'c'))),
                error = function(e) NULL)
  if (is.null(rs) || !all(c('ID', 'behavior') %in% names(rs))) next
  is_toptax = grepl('runscripts/top_tax/', f)

  base_row = rs %>% filter(ID == 'baseline')
  base_mtrs = if (nrow(base_row) > 0 && 'mtr_vars' %in% names(rs))
                strsplit(base_row$mtr_vars[1] %||% '', ' ')[[1]] else character()

  for (i in seq_len(nrow(rs))) {
    id  = rs$ID[i]
    if (identical(id, 'baseline')) next
    beh = rs$behavior[i]
    if (is.na(beh) || !nzchar(trimws(beh))) next
    mods = strsplit(trimws(beh), '\\s+')[[1]]
    has_wealth  = any(startsWith(mods, 'wealth/'))
    has_evasion = any(startsWith(mods, 'evasion/'))
    has_estate  = any(startsWith(mods, 'estate/'))

    # (1) activation contract
    if (has_wealth && !has_estate)
      fail(f, id, ': wealth/ module without estate/avoidance')
    if (is_toptax && has_evasion && !has_estate)
      fail(f, id, ': top_tax row with evasion/ but no estate/avoidance ',
           '(evaded income would stay visible to the estate tax)')
    if (has_estate) {
      est_pos = min(which(startsWith(mods, 'estate/')))
      for (up in c('evasion/', 'wealth/')) {
        up_pos = which(startsWith(mods, up))
        if (length(up_pos) > 0 && min(up_pos) > est_pos)
          fail(f, id, ': estate/ module ordered before ', up)
      }
    }

    # (2) MTR registration
    if (has_estate && 'mtr_vars' %in% names(rs)) {
      row_mtrs = strsplit(rs$mtr_vars[i] %||% '', ' ')[[1]]
      if (!('estate' %in% row_mtrs))
        fail(f, id, ': estate/ module without "estate" in mtr_vars')
      if (nrow(base_row) > 0 && !('estate' %in% base_mtrs))
        fail(f, id, ': baseline row does not register "estate" in mtr_vars')
    }
  }
}

if (n_fail == 0) {
  cat('LINT PASSED: estate-module activation contract holds across runscripts\n')
} else {
  cat(sprintf('\nLINT FAILED: %d violation(s)\n', n_fail))
  quit(status = 1)
}

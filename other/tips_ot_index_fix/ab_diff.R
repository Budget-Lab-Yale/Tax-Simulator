#-------------------------------------------------------------------------------
# A/B diff for the calc_mtrs extensive-margin tips/OT aggregate fix.
#
# Compares two full (5% sample) runs of tests/mtr_extensive_tips_ot, which asks
# for extensive-margin MTRs on tips1/tips2/ot1/ot2 (plus wages1/wages2 as a
# control -- those branches were already maintaining the aggregates):
#
#   mtr_ext_pre  : HEAD 9c3158142, pre-fix (tips/ot aggregates left stale)
#   mtr_ext_post : working tree, post-fix (aggregates decremented)
#
# EXPECTATION: bit-identical MTRs. No calculator reads the bare tips/ot
# aggregates, so maintaining them cannot move a number. A nonzero diff would
# mean some consumer DOES read them, i.e. the pre-fix extensive tips/OT MTRs
# were biased after all -- which is the finding the 2026-07-02 review doc
# flagged as "needs verification, highest stakes".
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

root = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
pre  = file.path(root, 'mtr_ext_pre')
post = file.path(root, 'mtr_ext_post')

n_fail = 0
check = function(name, pass, note = '') {
  cat(sprintf('  %-64s %s%s\n', name, if (isTRUE(pass)) 'PASS' else 'FAIL',
              if (nzchar(note)) paste0('  [', note, ']') else ''))
  if (!isTRUE(pass)) n_fail <<- n_fail + 1
}

scenarios = c('baseline', 'repeal_tips')
years     = 2026:2027
passes    = c('static', 'conventional')

cat('\n== detail-file MTR comparison ==\n')

for (s in scenarios) {
  for (p in passes) {
    for (y in years) {
      f_pre  = file.path(pre,  s, p, 'detail', paste0(y, '.csv'))
      f_post = file.path(post, s, p, 'detail', paste0(y, '.csv'))
      if (!file.exists(f_pre) || !file.exists(f_post)) {
        # baseline has no conventional pass; skip quietly
        next
      }
      d_pre  = fread(f_pre)
      d_post = fread(f_post)

      lbl = sprintf('%s/%s/%d', s, p, y)

      if (!identical(dim(d_pre), dim(d_post)) ||
          !identical(sort(names(d_pre)), sort(names(d_post)))) {
        check(paste(lbl, 'shape'), FALSE,
              sprintf('%dx%d vs %dx%d', nrow(d_pre), ncol(d_pre),
                      nrow(d_post), ncol(d_post)))
        next
      }
      setorder(d_pre, id); setorder(d_post, id)
      check(paste(lbl, 'record universe'), identical(d_pre$id, d_post$id))

      mtr_cols = grep('^mtr_', names(d_pre), value = TRUE)
      if (length(mtr_cols) == 0) {
        cat(sprintf('  %-64s (no mtr_ columns)\n', lbl))
        next
      }
      worst = mtr_cols %>%
        map_dfr(~ tibble(
          col      = .x,
          max_abs  = max(abs(d_pre[[.x]] - d_post[[.x]]), na.rm = TRUE),
          n_diff   = sum(d_pre[[.x]] != d_post[[.x]], na.rm = TRUE),
          na_mismatch = sum(is.na(d_pre[[.x]]) != is.na(d_post[[.x]]))
        ))
      check(sprintf('%s MTRs identical (%d cols)', lbl, length(mtr_cols)),
            all(worst$max_abs == 0) && all(worst$n_diff == 0) &&
              all(worst$na_mismatch == 0),
            sprintf('max |diff| %.3e', max(worst$max_abs)))
      if (any(worst$n_diff > 0 | worst$na_mismatch > 0)) {
        worst %>% filter(n_diff > 0 | na_mismatch > 0) %>% print(n = 50)
      }

      # Liabilities must also be untouched (the fix only alters the perturbed
      # frames inside calc_mtrs, never the frame that produces liability)
      liab_cols = grep('^liab_', names(d_pre), value = TRUE)
      liab_bad = liab_cols %>%
        keep(~ !isTRUE(all.equal(d_pre[[.x]], d_post[[.x]])))
      check(sprintf('%s liabilities identical (%d cols)', lbl, length(liab_cols)),
            length(liab_bad) == 0,
            paste(liab_bad, collapse = ', '))
    }
  }
}

cat('\n== totals comparison ==\n')
for (s in scenarios) {
  for (p in passes) {
    d_pre  = file.path(pre,  s, p, 'totals')
    d_post = file.path(post, s, p, 'totals')
    if (!dir.exists(d_pre) || !dir.exists(d_post)) next
    files = intersect(list.files(d_pre), list.files(d_post))
    bad = files %>% keep(function(f) {
      a = read_csv(file.path(d_pre, f), show_col_types = FALSE)
      b = read_csv(file.path(d_post, f), show_col_types = FALSE)
      !isTRUE(all.equal(a, b))
    })
    check(sprintf('%s/%s totals identical (%d files)', s, p, length(files)),
          length(bad) == 0, paste(bad, collapse = ', '))
  }
}

cat(sprintf('\n== %d failure(s) ==\n', n_fail))
if (n_fail > 0) quit(status = 1)
cat('A/B IDENTICAL -- fix is inert on current law, as claimed\n')

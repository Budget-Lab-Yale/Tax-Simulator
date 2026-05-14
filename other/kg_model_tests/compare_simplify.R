# Compare kg_dyn_simplify_check (post-simplify) against wealth_30yr
# (pre-simplify HEAD). Same Tax-Data vintage, same 2025:2055 year range,
# same 4 scenarios — diffs should be 0 (or floating-point noise) for the
# refactor to count as behavior-preserving.

suppressPackageStartupMessages({ library(tidyverse) })

REF = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/wealth_30yr'
NEW = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_dyn_simplify_check'

SCENARIOS = c('baseline', 'baseline_check', 'rate_up_5pp', 'carryover', 'deemed')

FILES = list(
  totals      = 'conventional/totals/1040.csv',
  receipts    = 'conventional/totals/receipts.csv',
  summary     = 'conventional/supplemental/kg_dynamics_summary.csv',
  revenue     = 'conventional/supplemental/revenue_estimates.csv'
)

# A small diff helper: read both files, align by all non-numeric columns,
# compute max absolute and max relative diff over numeric columns.
diff_file = function(ref_path, new_path, tol = 1e-9) {
  if (!file.exists(ref_path)) return(tibble(status = 'ref missing'))
  if (!file.exists(new_path)) return(tibble(status = 'new missing'))

  ref = read_csv(ref_path, show_col_types = FALSE)
  new = read_csv(new_path, show_col_types = FALSE)

  if (!identical(dim(ref), dim(new))) {
    return(tibble(status = sprintf('shape mismatch: ref=%dx%d new=%dx%d',
                                   nrow(ref), ncol(ref),
                                   nrow(new), ncol(new))))
  }
  if (!identical(names(ref), names(new))) {
    return(tibble(status = paste0('column mismatch: ',
                                  paste(setdiff(names(ref), names(new)),
                                        collapse = ','))))
  }

  num_cols = names(ref)[map_lgl(ref, is.numeric)]
  diffs = map_dfr(num_cols, function(col) {
    a = ref[[col]]; b = new[[col]]
    abs_d = abs(a - b)
    rel_d = abs_d / pmax(abs(a), 1e-30)
    tibble(column   = col,
           max_abs  = max(abs_d, na.rm = TRUE),
           max_rel  = max(rel_d[is.finite(rel_d)], na.rm = TRUE),
           n_diff   = sum(abs_d > tol, na.rm = TRUE))
  })
  diffs %>% filter(max_abs > tol) %>%
    arrange(desc(max_abs))
}

summarize_diffs = function(diffs, kind, scenario) {
  if (nrow(diffs) == 0) {
    cat(sprintf('  [%s/%s] OK\n', scenario, kind))
    return(invisible())
  }
  if ('status' %in% names(diffs)) {
    cat(sprintf('  [%s/%s] %s\n', scenario, kind, diffs$status[1]))
    return(invisible())
  }
  cat(sprintf('  [%s/%s] %d columns differ\n', scenario, kind, nrow(diffs)))
  diffs %>% head(5) %>% as.data.frame() %>%
    walk2(seq_len(nrow(.)), function(.x, .y) NULL)
  print(head(diffs, 5))
}

cat('==================================================================\n')
cat('Regression: kg_dyn_simplify_check vs wealth_30yr (HEAD reference)\n')
cat('==================================================================\n')

for (scen in SCENARIOS) {
  cat(sprintf('\n--- scenario: %s ---\n', scen))
  for (kind in names(FILES)) {
    ref_path = file.path(REF, scen, FILES[[kind]])
    new_path = file.path(NEW, scen, FILES[[kind]])
    summarize_diffs(diff_file(ref_path, new_path), kind, scen)
  }
}

cat('\n==================================================================\n')
cat('Regression complete.\n')

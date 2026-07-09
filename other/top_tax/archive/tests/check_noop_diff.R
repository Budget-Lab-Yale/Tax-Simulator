#-------------------------------------------------------------------------------
# check_noop_diff.R
#
# No-op regression (plan Verification item 2): with sigma absent from the
# behavior column, the pre-change (HEAD worktree) and post-change (working
# tree) runs of tests/kg_item1_regression_v2 must be byte-identical EXCEPT
# the additive tau_eq/conv_inflow columns in the kg supplemental files
# (kg_dynamics_age_profile.csv and kg_dynamics_state/*.rds), whose
# pre-existing columns must still match value-for-value.
#
# Usage: Rscript other/top_tax/tests/check_noop_diff.R <pre_root> <post_root>
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tools)
})

args = commandArgs(trailingOnly = TRUE)
pre_root = args[1]; post_root = args[2]

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}

list_rel = function(root) {
  f = list.files(root, recursive = TRUE, all.files = FALSE)
  f[!grepl('^_slurm_staging/', f) & !grepl('\\.xlsx$', f)]
}

pre_files  = list_rel(pre_root)
post_files = list_rel(post_root)

check(setequal(pre_files, post_files) ||
        length(setdiff(post_files, pre_files)) == 0 &&
        length(setdiff(pre_files, post_files)) == 0,
      sprintf('file trees match (%d pre vs %d post files)',
              length(pre_files), length(post_files)))
if (length(setdiff(pre_files, post_files)) > 0)
  cat('  only in pre: ', paste(head(setdiff(pre_files, post_files), 10),
                               collapse = ', '), '\n')
if (length(setdiff(post_files, pre_files)) > 0)
  cat('  only in post:', paste(head(setdiff(post_files, pre_files), 10),
                               collapse = ', '), '\n')

common = intersect(pre_files, post_files)
is_age_profile = grepl('kg_dynamics_age_profile\\.csv$', common)
is_state_rds   = grepl('kg_dynamics_state/\\d{4}\\.rds$', common)
exact_files    = common[!is_age_profile & !is_state_rds]

# 1. Byte-identical everywhere except the two additive-column surfaces
differing = character(0)
for (f in exact_files) {
  a = md5sum(file.path(pre_root, f)); b = md5sum(file.path(post_root, f))
  if (a != b) differing = c(differing, f)
}
check(length(differing) == 0,
      sprintf('%d files byte-identical outside kg supplemental surfaces',
              length(exact_files)))
if (length(differing) > 0)
  cat('  differing: ', paste(head(differing, 20), collapse = '\n             '), '\n')

# 2. Age profile: pre columns unchanged, post adds only the expected columns
for (f in common[is_age_profile]) {
  a = fread(file.path(pre_root, f), showProgress = FALSE)
  b = fread(file.path(post_root, f), showProgress = FALSE)
  new_cols = setdiff(names(b), names(a))
  check(setequal(new_cols, c('tau_eq_B', 'tau_eq_S', 'conv_inflow')),
        sprintf('%s: additive columns are exactly tau_eq_B/tau_eq_S/conv_inflow (got: %s)',
                f, paste(new_cols, collapse = ', ')))
  shared = intersect(names(a), names(b))
  same = isTRUE(all.equal(as.data.frame(a[, ..shared]),
                          as.data.frame(b[, ..shared]),
                          tolerance = 0, check.attributes = FALSE))
  check(same, sprintf('%s: pre-existing columns value-identical', f))
  check(all(b$conv_inflow == 0),
        sprintf('%s: conv_inflow all zero (sigma absent)', f))
}

# 3. State RDS: same contract check on cell_table + regime
for (f in common[is_state_rds]) {
  a = readRDS(file.path(pre_root, f)); b = readRDS(file.path(post_root, f))
  check(identical(a$regime, b$regime), sprintf('%s: regime identical', f))
  new_cols = setdiff(names(b$cell_table), names(a$cell_table))
  ok_cols = setequal(new_cols, c('tau_eq_B', 'tau_eq_S', 'conv_inflow'))
  shared = intersect(names(a$cell_table), names(b$cell_table))
  same = isTRUE(all.equal(as.data.frame(a$cell_table[, shared]),
                          as.data.frame(b$cell_table[, shared]),
                          tolerance = 0, check.attributes = FALSE))
  check(ok_cols && same && is.null(b$sigma),
        sprintf('%s: cell_table additive-only, values identical, no sigma tracker', f))
}

cat(sprintf('\n%s\n', if (n_fail == 0) 'NO-OP REGRESSION PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

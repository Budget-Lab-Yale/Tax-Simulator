#-------------------------------------------------------------------------------
# check_sigma_zero.R
#
# SIGMA_CONV=0 identity check (plan Verification item 3c): a sigma_smoke run
# with SIGMA_CONV=0 (root A) must produce outputs identical to a run whose
# behavior column omits conversion/sigma entirely (root B, sigma_smoke_noconv)
# — byte-identical everywhere except (i) .xlsx (embedded timestamps),
# (ii) kg_dynamics_state/*.rds (A carries a sigma tracker with zero inflow;
# cell tables must still be value-identical), and (iii) A's optional sigma
# dump dir and behavioral_assumptions.csv / tax_law.csv paths listing the
# module set (behavior columns differ by construction).
#
# Usage: Rscript check_sigma_zero.R <root_A> <root_B>
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(tools); library(dplyr)
})

args = commandArgs(trailingOnly = TRUE)
root_a = args[1]; root_b = args[2]

n_fail = 0
check = function(ok, label) {
  status = if (isTRUE(all(ok))) 'PASS' else 'FAIL'
  if (!isTRUE(all(ok))) n_fail <<- n_fail + 1
  cat(sprintf('[%s] %s\n', status, label))
}

list_rel = function(root) {
  f = list.files(root, recursive = TRUE)
  f[!grepl('^_slurm_staging/', f) & !grepl('\\.xlsx$', f) &
    !grepl('sigma_conversion_dump/', f) &
    !grepl('behavioral_assumptions\\.csv$', f)]
}

fa = list_rel(root_a); fb = list_rel(root_b)
check(setequal(fa, fb), sprintf('file trees match (%d vs %d)', length(fa), length(fb)))
if (!setequal(fa, fb)) {
  cat('  only in A:', paste(head(setdiff(fa, fb), 10), collapse = ', '), '\n')
  cat('  only in B:', paste(head(setdiff(fb, fa), 10), collapse = ', '), '\n')
}

common = intersect(fa, fb)
is_state = grepl('kg_dynamics_state/\\d{4}\\.rds$', common)

differing = character(0)
for (f in common[!is_state]) {
  if (md5sum(file.path(root_a, f)) != md5sum(file.path(root_b, f)))
    differing = c(differing, f)
}
check(length(differing) == 0,
      sprintf('%d files byte-identical outside kg state', sum(!is_state)))
if (length(differing) > 0)
  cat('  differing:', paste(head(differing, 20), collapse = '\n            '), '\n')

for (f in common[is_state]) {
  a = readRDS(file.path(root_a, f)); b = readRDS(file.path(root_b, f))
  same_ct = isTRUE(all.equal(as.data.frame(a$cell_table),
                             as.data.frame(b$cell_table),
                             tolerance = 0, check.attributes = FALSE))
  zero_tracker = !is.null(a$sigma) && a$sigma$conv_total == 0 &&
                 all(a$sigma$conv_inflow == 0)
  check(same_ct && identical(a$regime, b$regime) && zero_tracker &&
          is.null(b$sigma),
        sprintf('%s: cell tables identical; A tracker all-zero; B trackerless', f))
}

cat(sprintf('\n%s\n', if (n_fail == 0) 'SIGMA-ZERO IDENTITY PASSED' else
            paste0(n_fail, ' CHECK(S) FAILED')))
if (n_fail > 0) quit(status = 1)

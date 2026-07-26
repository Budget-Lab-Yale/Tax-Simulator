#-------------------------------------------------------------------------------
# spot_check_eta19.R
#
# The Phase 5 proving spot check. The eta_logs measurement reproduces its pinned
# value exactly from the vintages already on scratch, which proves the ARITHMETIC
# of the inversion but not the SIMULATION behind it -- the detail files those
# vintages hold were produced under the old environment-variable path, before the
# config rebuild moved the trial value into a bound calibration file. This script
# closes that gap for one grid point (the author ruled one of three): re-simulate
# eta_dial_logs_19 as eta_dial_logs_19_spot through the new sweep machinery, and
# compare.
#
# It reports rather than asserts, because the interesting outcome is not
# pass/fail but the SIZE of any difference. What matters is whether the
# measurement moves, and the measurement is one weighted sum.
#
# WHAT IT IS NOT. It is not a byte-identity test, and expecting one would have
# been a mistake. eta_dial_logs_19 was written on 2026-07-19; the model has moved
# since, in ways that have nothing to do with the config rebuild -- the calc_mtrs
# tips/OT aggregate fix, the QBI and bracket-schedule rewrites, the corporate
# statutory rate going on-model, the Off-Model-Estimates two-stream change. The
# rebuild's own six-scenario gate is byte-exact, but it compares against goldens
# taken at the branch point, not against a week-old vintage.
#
# RESULT, 2026-07-26. Records align exactly. 47 of 95 numeric columns differ, at
# relative magnitudes of 1e-6 and below (the two columns reported as relative 1
# and 2 are sign noise on values of absolute size 5e-6 and 1e-10) -- consistent
# with the model changes above, mtr_cap_bundle being the worst offender and
# calc_mtrs being one of the things that changed. And the measured moment, which
# is the only thing the calibration consumes, reproduces to 1.4e-13 relative:
# E_full agrees to ten decimals with the value shipped in efull_logs.csv, moving
# eta_tilde by about 3e-12 against a value carried to four decimals.
#
# So the inference the plan wanted converted into a measurement is now measured:
# re-simulating through the sweep machinery -- a bound calibration file in place
# of an exported environment variable -- gives back the pinned eta_logs.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
YEAR = 2055
LEG  = file.path('s_cg_r25', 'conventional_no_wealth', 'detail', paste0(YEAR, '.csv'))

old = file.path(ROOT, 'eta_dial_logs_19',      LEG)
new = file.path(ROOT, 'eta_dial_logs_19_spot', LEG)

a = fread(old, showProgress = FALSE)
b = fread(new, showProgress = FALSE)

cat(sprintf('old : %s\nnew : %s\n', old, new))
cat(sprintf('rows %d vs %d   cols %d vs %d\n',
            nrow(a), nrow(b), ncol(a), ncol(b)))
stopifnot(identical(names(a), names(b)), nrow(a) == nrow(b))

#-------------------------------------------------------------------------------
# 1. Record alignment. If the two runs put different records in different rows,
#    nothing below means anything.
#-------------------------------------------------------------------------------

cat(sprintf('\nrecord ids identical, in order : %s\n',
            identical(a$id, b$id)))

#-------------------------------------------------------------------------------
# 2. Per-column agreement. Reported as the largest RELATIVE difference in each
#    column, so a column that is off in its tenth significant digit is legible
#    as such rather than just "different".
#-------------------------------------------------------------------------------

num_cols = names(a)[map_lgl(a, is.numeric)]

rel = map_dfr(num_cols, function(nm) {
  x = a[[nm]]; y = b[[nm]]
  d = abs(x - y)
  s = pmax(abs(x), abs(y))
  r = ifelse(s > 0, d / s, 0)
  tibble(column      = nm,
         n_differing = sum(d > 0, na.rm = TRUE),
         max_abs     = max(d, na.rm = TRUE),
         max_rel     = max(r, na.rm = TRUE))
}) %>%
  arrange(desc(max_rel))

cat(sprintf('\nnumeric columns          : %d\n', length(num_cols)))
cat(sprintf('columns with any difference: %d\n', sum(rel$n_differing > 0)))
cat(sprintf('largest relative difference anywhere: %.3e\n', max(rel$max_rel)))

cat('\nworst ten columns by relative difference:\n')
print(as.data.frame(head(rel, 10)))

#-------------------------------------------------------------------------------
# 3. The thing the calibration actually consumes. E_full is measured from one
#    weighted sum, R = sum(w * pmax(kg_lt, 0)), so the question is not whether
#    the files match byte for byte but whether R does to the precision the
#    inversion needs. Four decimals on eta_tilde needs roughly six significant
#    figures on R.
#-------------------------------------------------------------------------------

R_of = function(d) sum(d$weight * pmax(d$kg_lt, 0), na.rm = TRUE)
Ra   = R_of(a)
Rb   = R_of(b)

cat('\n--- the measured moment ------------------------------------------\n')
cat(sprintf('R (old, $B)  : %.10f\n', Ra / 1e9))
cat(sprintf('R (new, $B)  : %.10f\n', Rb / 1e9))
cat(sprintf('relative diff: %.3e\n', abs(Rb - Ra) / abs(Ra)))

# E_full = log(R_shock / R_base) / dtau, so a relative wobble in R_shock moves
# E_full by that wobble divided by (dtau * log-ratio). Read dtau and the base R
# off the shipped fit rather than recomputing them: both are form-invariant and
# come from the levels central vintage, unchanged by this exercise.
fit = read_csv('other/kg_model_tests/form_ab/efull_logs.csv', show_col_types = FALSE) %>%
  filter(eta_tilde == 1.9)

if (nrow(fit) == 1) {
  E_old = log((Ra / 1e9) / fit$R_base_B) / fit$dtau
  E_new = log((Rb / 1e9) / fit$R_base_B) / fit$dtau
  cat(sprintf('\nE_full recomputed, old detail : %.10f\n', E_old))
  cat(sprintf('E_full recomputed, new detail : %.10f\n', E_new))
  cat(sprintf('E_full as shipped in efull_logs.csv : %.10f\n', fit$E_full))
  cat(sprintf('\nE_full shift from re-simulation: %.3e (in absolute E_full units)\n',
              abs(E_new - E_old)))
  cat(sprintf('At the shipped slope that is about %.3e on eta_tilde,\n',
              abs(E_new - E_old) / (abs(fit$E_full) / 1.9)))
  cat('against a shipped value carried to four decimals (1.6625).\n')
}

cat('-------------------------------------------------------------------\n')

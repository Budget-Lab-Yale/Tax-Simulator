#-------------------------------------------------------------------------------
# Cheap pre-flight check for the on-model corporate statutory-rate module:
#   (1) parse-check every edited/new R file (catches syntax errors)
#   (2) unit-test corp_rate_delta() Form A math against hand values
# Runs on a compute node (module load R/4.4.1-foss-2022b). No sim, single core.
#-------------------------------------------------------------------------------

suppressMessages({library(dplyr); library(tidyr); library(readr)})

repo = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'

cat('== (1) parse-check ==\n')
files = c('src/sim/corp_rate.R',
          'src/sim/corp_incidence.R',
          'src/data/post_processing/revenue.R',
          'src/data/post_processing/distribution.R',
          'src/data/post_processing/distribution_etrs.R')
ok = TRUE
for (f in files) {
  res = tryCatch({ parse(file = file.path(repo, f)); 'OK' },
                 error = function(e) paste('PARSE ERROR:', conditionMessage(e)))
  if (res != 'OK') ok = FALSE
  cat(sprintf('  %-52s %s\n', f, res))
}
if (!ok) stop('parse-check failed')

cat('\n== (2) corp_rate_delta unit math ==\n')
source(file.path(repo, 'src/sim/corp_rate.R'))
cat('  CORP_RATE_ETI =', CORP_RATE_ETI, '\n')

e   = CORP_RATE_ETI
t0  = 0.21; t = 0.28; R0 = 100
rate_series = tibble(year = 2027L, t0 = t0, t = t)
rev_corp    = tibble(year = 2027L, rev_corp = R0)

exp_static = R0 * (t / t0 - 1)
exp_conv   = R0 * ((t / t0) * ((1 - t) / (1 - t0))^e - 1)

got_static = corp_rate_delta(rate_series, rev_corp, static = TRUE)$delta
got_conv   = corp_rate_delta(rate_series, rev_corp, static = FALSE)$delta

chk = function(label, got, exp) {
  pass = isTRUE(abs(got - exp) < 1e-9)
  cat(sprintf('  %-28s got %.6f  expect %.6f  %s\n',
              label, got, exp, if (pass) 'PASS' else 'FAIL'))
  pass
}
p1 = chk('static (mechanical)', got_static, exp_static)
p2 = chk('conventional (Form A)', got_conv, exp_conv)
cat(sprintf('  conv < static (base erosion): %s\n',
            if (got_conv < got_static) 'PASS' else 'FAIL'))

# no change -> 0
z_series = tibble(year = 2027L, t0 = 0.21, t = 0.21)
p3 = chk('no-change static', corp_rate_delta(z_series, rev_corp, TRUE)$delta,  0)
p4 = chk('no-change conv',   corp_rate_delta(z_series, rev_corp, FALSE)$delta, 0)

# NULL series -> 0
p5 = chk('NULL series', corp_rate_delta(NULL, rev_corp, FALSE)$delta, 0)

# revenue-max statutory rate t* = 1/(1+e) (report)
cat(sprintf('  Form A revenue-max t* = 1/(1+e) = %.4f\n', 1 / (1 + e)))

if (all(c(p1, p2, p3, p4, p5, got_conv < got_static))) {
  cat('\nALL CHECKS PASS\n')
} else {
  stop('unit math failed')
}

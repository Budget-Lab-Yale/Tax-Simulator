#-------------------------------------------------------------------------------
# realization_elasticity_by_regime.R
#
# Measures the long-run realization elasticity under carryover basis and under
# deemed realization at death, on the convention that pinned the current-law
# value of -0.6
#
# The pinned protocol (other/kg_model_tests/form_ab/measure_efull_logs.R) reads
# a +5pp capital-gains shock against its own base in 2055 and reports
#
#   E_full     = log(R_shock / R_base) / (tau_shock - tau_base)
#   elasticity = E_full * 0.238
#
# where R is weighted positive long-term gains, tau is the realization-weighted
# mean marginal rate on those gains, and 0.238 is the current combined top rate.
# Each death regime is measured against its own no-rate-change run, so the
# regime's own level effect divides out and only the rate response remains.
#
# Run via sbatch (never on the login node):
#   sbatch other/top_tax/report_prep/run_realization_elasticity.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
})

VINTAGE = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/kg_v6_revmax')
YEAR    = 2055
LEG     = 'conventional'
DIVISOR = 0.238   # current combined top rate, the author-locked divisor

pairs = list(
  carryover = c(base = 'cg_00pp_carryover', shock = 'cg_05pp_carryover'),
  deemed    = c(base = 'cg_00pp_deemed',    shock = 'cg_05pp_deemed')
)

# Read the three columns the moment needs
read_year = function(scenario) {
  path = file.path(VINTAGE, scenario, LEG, 'detail', paste0(YEAR, '.csv'))
  fread(path, select = c('weight', 'kg_lt', 'mtr_kg_lt'), showProgress = FALSE)
}

# Weighted positive realizations, and their weighted mean marginal rate. The
# in_range argument drops records whose measured rate falls outside the unit
# interval, the rule the bathtub's cell averages now apply.
moments = function(dt, in_range) {
  d = dt[!is.na(kg_lt) & kg_lt > 0]
  if (in_range) d = d[!is.na(mtr_kg_lt) & mtr_kg_lt >= 0 & mtr_kg_lt <= 1]
  R   = d[, sum(weight * kg_lt)]
  tau = d[, sum(weight * kg_lt * mtr_kg_lt)] / R
  list(R = R, tau = tau)
}

for (in_range in c(FALSE, TRUE)) {
  cat('\n=== marginal rates', if (in_range) 'restricted to the unit interval'
      else 'unrestricted', '===\n')
  for (nm in names(pairs)) {
    p  = pairs[[nm]]
    mb = moments(read_year(p['base']),  in_range)
    ms = moments(read_year(p['shock']), in_range)
    dtau   = ms$tau - mb$tau
    e_full = log(ms$R / mb$R) / dtau
    cat(sprintf(
      '%-10s R_base=%.4g  R_shock=%.4g  tau_base=%.4f  tau_shock=%.4f  dtau=%.4f  E_full=%.3f  elasticity=%.3f\n',
      nm, mb$R, ms$R, mb$tau, ms$tau, dtau, e_full, e_full * DIVISOR))
  }
}

#-------------------------------------------------------------------------------
# realization_curve_by_regime.R
#
# Traces realized long-term gains and their marginal rate across the whole
# capital-gains rate grid, under each treatment of gains at death
#
# The single-point elasticity answers what happens at today's rate. It does not
# say how the response grows as the rate climbs, which is what separates the
# revenue curves. This walks every rate on the grid and reports, between
# adjacent points, the arc elasticity of realizations with respect to the
# net-of-tax share
#
#   e = log(R_next / R) / log((1 - tau_next) / (1 - tau))
#
# Run via sbatch (never on the login node):
#   sbatch other/top_tax/report_prep/run_realization_curve.sbatch
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
})

VINTAGE = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/kg_v6_revmax')
YEAR   = 2055
LEG    = 'conventional'
STEPS  = c(0, 5, 10, 15, 20, 25, 30)
REGIME = c('stepup', 'carryover', 'deemed')

# Weighted positive realizations and their realization-weighted marginal rate,
# dropping rates outside the unit interval as the bathtub's cell averages do
moments = function(scenario) {
  path = file.path(VINTAGE, scenario, LEG, 'detail', paste0(YEAR, '.csv'))
  if (!file.exists(path)) return(NULL)
  d = fread(path, select = c('weight', 'kg_lt', 'mtr_kg_lt'), showProgress = FALSE)
  d = d[!is.na(kg_lt) & kg_lt > 0 & !is.na(mtr_kg_lt) & mtr_kg_lt >= 0 & mtr_kg_lt <= 1]
  R = d[, sum(weight * kg_lt)]
  list(R = R, tau = d[, sum(weight * kg_lt * mtr_kg_lt)] / R)
}

for (rg in REGIME) {
  cat('\n===', rg, '=== (', YEAR, ',', LEG, 'leg )\n')
  cat(sprintf('%5s %14s %8s %10s %10s\n', 'shock', 'realizations', 'tau', 'R/R_first', 'arc e'))
  prev = NULL
  for (st in STEPS) {
    scen = sprintf('cg_%02dpp_%s', st, rg)
    m = moments(scen)
    if (is.null(m)) { cat(sprintf('%4dpp  (no detail: %s)\n', st, scen)); next }
    if (is.null(prev)) { first = m$R; e = NA_real_ } else {
      e = log(m$R / prev$R) / log((1 - m$tau) / (1 - prev$tau))
    }
    cat(sprintf('%4dpp %14.4g %8.4f %10.3f %10s\n',
                st, m$R, m$tau, m$R / first,
                if (is.na(e)) '-' else sprintf('%.3f', e)))
    prev = m
  }
}

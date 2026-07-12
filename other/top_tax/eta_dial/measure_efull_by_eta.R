#-------------------------------------------------------------------------------
# measure_efull_by_eta.R
#
# Full-sim long-run realization semi-elasticity at each KG_ETA of the eta-dial
# batch, on ONE consistent convention (adapted from measure_dilution.R):
#
#   E_full(eta) = log(R_shock / R_base) / dtau_rw   at 2055 (sim-year 30)
#     R       = sum(w * pmax(kg_lt, 0))
#     dtau_rw = realization-weighted mean mtr_kg_lt, shock - base
#
# Differences from measure_dilution.R, both deliberate and applied uniformly
# at every eta (including central), so ratios across eta are clean:
#   - shock scenario = s_cg_r25 (+5pp statutory), the smallest wedge in the
#     eta-dial batch (measure_dilution used a dedicated +2pp run)
#   - shock leg = conventional_no_wealth (behavior on, wealth haircut off) --
#     the only leg whose detail survives delete_detail=1, and a cleaner pure-kg
#     read anyway
#
# Output: other/top_tax/eta_dial/efull_by_eta.csv
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

LOCAL_ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
YEAR       = 2055
SHOCK      = 's_cg_r25'

# 2026-07-12 re-pin grid: 3 fresh vintages on current code (estate offset live),
# Tax-Data 2026070814 (production default). Straddle the ~2.5 expectation.
runs = tribble(
  ~eta,   ~vintage,
  2.0,    'eta_dial_e20_v2',
  2.3992, 'eta_dial_c_v2',
  3.0,    'eta_dial_e30_v2'
)
CENTRAL_VINTAGE = 'eta_dial_c_v2'   # supplies the shared baseline + dtau

agg = function(f, with_mtr = TRUE) {
  cols = c('weight', 'kg_lt', if (with_mtr) 'mtr_kg_lt')
  fread(f, select = cols, showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    summarise(
      R      = sum(weight * kg_pos, na.rm = TRUE),
      tau_rw = if (with_mtr)
                 sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                 sum(weight * kg_pos,             na.rm = TRUE)
               else NA_real_
    ) %>%
    as.list()
}

# Baseline is eta-independent; read the central vintage's own baseline.
base = agg(file.path(LOCAL_ROOT, CENTRAL_VINTAGE, 'baseline', 'static',
                     'detail', paste0(YEAR, '.csv')))

# The conv-no-wealth detail carries only the wealth-channel MTR columns, so
# the wedge is measured ONCE, from the central vintage's conventional leg
# (measure_dilution.R's convention), and shared across etas. dtau is a law
# property to first order; sharing it cancels exactly in the E_full-vs-eta
# linearity test.
s_central = agg(file.path(LOCAL_ROOT, CENTRAL_VINTAGE, SHOCK,
                          'conventional', 'detail', paste0(YEAR, '.csv')))
dtau = s_central$tau_rw - base$tau_rw

results = runs %>%
  pmap_dfr(function(eta, vintage) {
    s = agg(file.path(LOCAL_ROOT, vintage, SHOCK, 'conventional_no_wealth',
                      'detail', paste0(YEAR, '.csv')), with_mtr = FALSE)
    E_full = log(s$R / base$R) / dtau
    tibble(
      eta      = eta,
      vintage  = vintage,
      R_base_B = base$R / 1e9,
      R_shock_B = s$R / 1e9,
      dtau     = dtau,
      E_full   = E_full,
      dilution = -E_full / eta   # E_full is negative; dilution ~ 1/1.128 slope
    )
  })

out = 'other/top_tax/eta_dial/efull_by_eta.csv'
write_csv(results, out)
print(as.data.frame(results))
cat(sprintf('\nwrote %s\n', out))

#-------------------------------------------------------------------------------
# Invert the zero-intercept E_full(eta) line for the re-pin target.
#
# Convention (author-locked 2026-07-12): the literature realization elasticity
# e = -0.6 is evaluated at the top combined LTCG+NIIT statutory rate 0.238, so
# the semi-elasticity target is E_full_target = e / tau = -0.6 / 0.238 = -2.52.
# The single-pool entropy model makes E_full linear-through-origin in eta:
#   -E_full = slope * eta   =>   eta* = |E_full_target| / slope
# (same slope expression as gen_report.py::chart_efull). 3 grid points pin it.
#-------------------------------------------------------------------------------
E_FULL_TARGET = -0.6 / 0.238
slope    = with(results, sum(-E_full * eta) / sum(eta^2))
eta_star = abs(E_FULL_TARGET) / slope

cat('\n--- eta re-pin -------------------------------------------------\n')
cat(sprintf('target E_full         : %.4f   (= -0.6 / 0.238, top-rate divisor)\n',
            E_FULL_TARGET))
cat(sprintf('per-point -E_full/eta  : %s\n',
            paste(sprintf('%.4f', with(results, -E_full / eta)), collapse = ', ')))
cat(sprintf('fitted slope (0-int)   : %.5f  (-E_full = slope * eta)\n', slope))
cat(sprintf('eta*  = |target|/slope : %.4f   (current shipped 2.3992)\n', eta_star))
cat('----------------------------------------------------------------\n')

fit_out = tibble(
  target_E_full = E_FULL_TARGET,
  divisor       = 0.238,
  slope         = slope,
  eta_star      = eta_star,
  grid_eta      = paste(results$eta, collapse = ' '),
  grid_E_full   = paste(sprintf('%.4f', results$E_full), collapse = ' '),
  tax_data_vintage = '2026070814'
)
fit_path = 'other/top_tax/eta_dial/eta_repin_fit.csv'
write_csv(fit_out, fit_path)
cat(sprintf('wrote %s\n', fit_path))

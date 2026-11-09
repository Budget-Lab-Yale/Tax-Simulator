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

source('src/misc/calibration_writer.R')

# The output tree the eta-dial vintages were written to. Overridable, because a
# re-run writes its vintages beside the originals under suffixed names and has to
# be measurable without editing this script -- editing the script being the old
# way a calibration got re-run, and the reason these numbers came loose from their
# provenance in the first place.
LOCAL_ROOT = Sys.getenv('KG_CALIB_OUTPUT_ROOT',
                        '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1')
VINTAGE_SUFFIX = Sys.getenv('VINTAGE_SUFFIX', '')
YEAR       = 2055
SHOCK      = 's_cg_r25'

# Where the measured value lands. Written by this script rather than copied out of
# its log by hand: see src/misc/calibration_writer.R for why that is the point.
CALIB_FILE = 'config/calibrations/kg/bathtub.yaml'

# 2026-07-12 re-pin grid: 3 fresh vintages on current code (estate offset live),
# Tax-Data 2026070814 (production default). Straddle the ~2.5 expectation.
#
# The `_v2` vintages are the ones the 2026-07-12 re-pin wrote by hand. A re-run
# through launch_eta_dial_levels.sh writes `eta_dial_levels_<tag>` instead, so both
# names are accepted and whichever is present is used -- the values are what the
# grid is, not the folder they landed in. The tags, the sweep folders under
# config/calibrations/kg/sweeps/ and the launcher all declare the same three points;
# write_bathtub_sweep.py is where the grid is defined.
runs = tribble(
  ~eta,   ~tag,   ~legacy_vintage,
  2.0,    'e20',  'eta_dial_e20_v2',
  2.3992, 'c',    'eta_dial_c_v2',
  3.0,    'e30',  'eta_dial_e30_v2'
) %>%
  mutate(vintage = map2_chr(tag, legacy_vintage, function(tag, legacy) {
    fresh = paste0('eta_dial_levels_', tag, VINTAGE_SUFFIX)
    if (dir.exists(file.path(LOCAL_ROOT, fresh))) fresh else legacy
  }))

CENTRAL_TAG = 'c'   # its vintage supplies the shared baseline + dtau
CENTRAL_VINTAGE = runs$vintage[runs$tag == CENTRAL_TAG]

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
  select(eta, vintage) %>%
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
cat(sprintf('eta*  = |target|/slope : %.4f\n', eta_star))
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

#-------------------------------------------------------------------------------
# Write the value into the calibration file. This is the last step of the
# calibration, not a separate chore: the number that ships is the number this
# script measured, and nobody transcribes it.
#
# Four decimals, which is the precision the moment supports and the precision the
# shipped value has always carried.
#
# If this re-run does not reproduce the shipped value, the writer leaves the file
# alone and puts its version at bathtub.yaml.proposed with a banner. That is
# deliberate: a calibrated value moving means the model moved, the data moved, or
# the calibration is less identified than it looks, and the author reads the diff.
#-------------------------------------------------------------------------------

calib_write_entry(
  path   = CALIB_FILE,
  entry  = 'eta',
  value  = round(eta_star, 4),
  fields = list(
    kind = 'calibrated',
    set  = format(Sys.Date()),
    target = calib_prose(sprintf(
      'Constant SEMI-elasticity of realizations with respect to the gains rate.
       E_full measured at sim-year %d on the +5pp gains shock across the trial grid
       (%s), inverted through the origin for the eta that hits E_full = %.4f
       (= -0.6/%.3f, the author-locked top-rate divisor). The single-pool entropy
       model makes E_full linear through the origin in eta, which is what licenses
       the through-origin fit here where the net-of-tax form needs a piecewise one.
       Measured E_full at each grid point: %s. Fitted slope %.5f.',
      YEAR, paste(results$eta, collapse = ' '), E_FULL_TARGET, 0.238,
      paste(sprintf('%.4f', results$E_full), collapse = ' '), slope)),
    derived_under = list(tax_data          = fit_out$tax_data_vintage,
                         macro_projections = '2026022522'),
    invalidated_by = c('src/sim/kg/constants.R',
                       'src/sim/kg/bellman.R',
                       'src/sim/kg/recurrence.R',
                       'src/sim/kg/timing.R',
                       'src/sim/kg/apply.R'),
    conditioned_on = list(settings.kg.applier_allocation = '0.5',
                          settings.kg.timing_ref_wedge   = 0.05,
                          settings.kg.timing_window      = 1),
    rederive    = 'other/top_tax/eta_dial/measure_efull_by_eta.R',
    active_when = list(kg.response_form = 'levels'),
    note = calib_prose(sprintf(
      "DORMANT under the shipped configuration: response_form is 'logs' since
       2026-07-22, so the live elasticity is eta_logs and this value is read only by
       a run that flips that setting. Higher than eta_logs because the semi-elastic
       full-sim slope is flatter, so the same E_full needs a larger eta. Response is
       INCREASING in |eta|.
       History: 4.4984 (spec-v2 nested) -> 2.3992 (spec-v3 single pool) -> 2.4825
       (full-sim E_full inversion, 2026-07-12, reproduced 2026-07-26 by this script
       writing its own value for the first time).
       apply.R is listed as a dependency because the applier rule feeds the measured
       E_full -- an applier-rule change once biased every conventional kg estimate by
       about 37%% on a 5pp gains-rate score before it was caught. That is the reason
       the dependency list is wider than the Bellman itself.
       Grid + fit: other/top_tax/eta_dial/eta_repin_fit.csv. The trial values are
       config/calibrations/kg/sweeps/eta_*/bathtub.yaml, and the sweep is launched by
       other/top_tax/eta_dial/launch_eta_dial_levels.sh -- which requires
       response_form flipped to 'levels' first and refuses to run otherwise.
       Measured from vintages: %s.",
      paste(results$vintage, collapse = ' ')))))

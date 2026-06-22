#-------------------------------------------------------------------------------
# measure_dilution.R
#
# Measures the full-sim kg_dynamics realization semi-elasticity under the
# CURRENT applier-allocation rule (KG_APPLIER_ALLOCATION, default 0.5) and
# converts it to the dilution factors calibrate.R needs.
#
# dilution = E_full / E_int, where
#   E_full = full-sim measured semi-elasticity (this script), and
#   E_int  = the bathtub-internal semi-elasticity the calibrator hit at the
#            (psi, planned_share) USED IN THE SIM RUN being measured.
#
# E_full convention (matches other/kg_model_tests/planned_bucket_results.md):
#   E_full = log(R_shock / R_base) / dtau
#     R     = aggregate realized long-term gains  = sum(w * pmax(kg_lt, 0))
#             (matches the bathtub's R_B; positive realizations only)
#     dtau  = realization-weighted EMTR shift
#           = [sum(w*pmax(kg_lt,0)*mtr_kg_lt)/sum(w*pmax(kg_lt,0))]_shock
#           - [same]_base
#   long-run : R/tau from rate_up_2pp vs baseline at sim-year 30 (2055)
#   short-run: R from delayed vs baseline at the ANNOUNCEMENT year (2026),
#              normalized by the t+1 EMTR shift (2027, where the delayed hike
#              lands) -- the short-run moment is dlog(R(t))/dtau(t+1).
#
# Base leg reads baseline/static/detail (baseline writes no conventional pass);
# shock legs read {scn}/conventional/detail (post-behavioral realizations).
#
# CLI:
#   Rscript other/kg_model_tests/measure_dilution.R <vintage> [<local_root>]
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

args       = commandArgs(trailingOnly = TRUE)
VINTAGE    = if (length(args) >= 1) args[1] else 'kg_recal_2pp_05'
LOCAL_ROOT = if (length(args) >= 2) args[2] else
  '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
ROOT = file.path(LOCAL_ROOT, VINTAGE)

# Bathtub-internal semi-elasticities at the (psi, planned_share) the sim ran
# under. Defaults are the pre-recal values (21.2272, 0.3921 -> -3.2075/+5.8293,
# from calibrate_11775725.out). Pass args 3/4 to override -- e.g. for a
# VERIFICATION run at the recalibrated psi (29.3290, 0.2102), pass the new
# internal targets (-2.2358, +3.2758 from calibrate_15779290.out) so the
# printed dilution should reproduce 1.1275/1.5391 if dilution is psi-stable.
# These are allocation-INDEPENDENT (the bathtub never calls the applier).
E_INT_LONG  = if (length(args) >= 3) as.numeric(args[3]) else -3.2075
E_INT_SHORT = if (length(args) >= 4) as.numeric(args[4]) else +5.8293

# Nominal literature targets, for reporting only.
NOMINAL_LONG  = -2.52
NOMINAL_SHORT = +5.04

LONG_ANCHOR        = 2055   # sim-year 30
SHORT_ANCHOR       = 2026   # announcement year
SHORT_DTAU_YEAR    = 2027   # t+1 (where the delayed hike lands)

# --- helpers ----------------------------------------------------------------

# Locate the baseline output dir (baseline may live alongside the CF scenarios
# under the same vintage when baseline_vintage = NULL).
baseline_static = file.path(ROOT, 'baseline', 'static', 'detail')
if (!dir.exists(baseline_static)) {
  stop('baseline static detail not found at ', baseline_static)
}

read_detail = function(scn, pass, yr) {
  f = file.path(ROOT, scn, pass, 'detail', paste0(yr, '.csv'))
  if (!file.exists(f)) stop('missing detail: ', f)
  fread(f, select = c('id', 'weight', 'kg_lt', 'mtr_kg_lt'),
        showProgress = FALSE) %>% as_tibble()
}

# R = sum(w * pmax(kg_lt, 0)); tau_rw = realization-weighted mean mtr_kg_lt
agg = function(df) {
  df %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    summarise(R      = sum(weight * kg_pos, na.rm = TRUE),
              tau_rw = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                       sum(weight * kg_pos,             na.rm = TRUE)) %>%
    as.list()
}

base_at  = function(yr) agg(read_detail('baseline', 'static', yr))
shock_at = function(scn, yr) agg(read_detail(scn, 'conventional', yr))

# --- long-run (permanent +2pp), measured at 2055 ----------------------------

bL = base_at(LONG_ANCHOR)
sL = shock_at('rate_up_2pp', LONG_ANCHOR)
dtau_long   = sL$tau_rw - bL$tau_rw
E_full_long = log(sL$R / bL$R) / dtau_long
dil_long    = E_full_long / E_INT_LONG

# --- short-run (delayed, announced 2026 / effective 2027) -------------------

bS  = base_at(SHORT_ANCHOR)
sS  = shock_at('delayed', SHORT_ANCHOR)
bS2 = base_at(SHORT_DTAU_YEAR)
sS2 = shock_at('delayed', SHORT_DTAU_YEAR)
dtau_short   = sS2$tau_rw - bS2$tau_rw      # t+1 EMTR shift
E_full_short = log(sS$R / bS$R) / dtau_short
dil_short    = E_full_short / E_INT_SHORT

# --- report -----------------------------------------------------------------

cat(sprintf('\nVintage: %s\n', ROOT))
cat('\n== LONG-RUN (rate_up_2pp vs baseline @ %d) ==\n', LONG_ANCHOR)
cat(sprintf('  R_base = %.1f $B   R_shock = %.1f $B   dlog(R) = %+.5f\n',
            bL$R/1e9, sL$R/1e9, log(sL$R/bL$R)))
cat(sprintf('  tau_rw base = %.4f  shock = %.4f   dtau = %+.5f\n',
            bL$tau_rw, sL$tau_rw, dtau_long))
cat(sprintf('  E_full_long  = %+.4f   (E_int = %+.4f, nominal %+.2f)\n',
            E_full_long, E_INT_LONG, NOMINAL_LONG))

cat(sprintf('\n== SHORT-RUN (delayed vs baseline @ %d; dtau @ %d) ==\n',
            SHORT_ANCHOR, SHORT_DTAU_YEAR))
cat(sprintf('  R_base = %.1f $B   R_shock = %.1f $B   dlog(R) = %+.5f\n',
            bS$R/1e9, sS$R/1e9, log(sS$R/bS$R)))
cat(sprintf('  tau_rw base(t+1) = %.4f  shock(t+1) = %.4f   dtau = %+.5f\n',
            bS2$tau_rw, sS2$tau_rw, dtau_short))
cat(sprintf('  E_full_short = %+.4f   (E_int = %+.4f, nominal %+.2f)\n',
            E_full_short, E_INT_SHORT, NOMINAL_SHORT))

cat('\n== NEW DILUTION FACTORS (paste into calibrate.R) ==\n')
cat(sprintf('  KG_DYN_DILUTION_LONG  = %.4f   (was 0.786)\n', dil_long))
cat(sprintf('  KG_DYN_DILUTION_SHORT = %.4f   (was 0.865)\n', dil_short))
cat(sprintf('\n  implied internal targets: long = %+.4f  short = %+.4f\n',
            NOMINAL_LONG / dil_long, NOMINAL_SHORT / dil_short))
cat(sprintf('  measured-from note: rate_up_2pp @ %d full-sim = %+.2f; ',
            LONG_ANCHOR, E_full_long))
cat(sprintf('delayed @ %d full-sim = %+.2f\n', SHORT_ANCHOR, E_full_short))

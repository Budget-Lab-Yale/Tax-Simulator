#-------------------------------------------------------------------------------
# measure_shortrun_logs.R  --  Part B2: measure the logs short-run moment.
#
# Adapts the short-run block of measure_dilution.R for the timeable-share pin of
# the net-of-tax form. For a given full-sim vintage (run under KG_RESPONSE_FORM=
# logs at the pinned eta_tilde and a trial KG_TIMEABLE_SHARE_LOGS), reports:
#   E_full_short = log(R_delayed(2026) / R_base(2026)) / dtau(2027)   [target 5.04]
#   E_full_long  = log(R_rate2pp(2055) / R_base(2055)) / dtau(2055)   [~ -2.52,
#                  timeable-invariant sanity that eta_tilde is still on target]
#     R      = sum(w * pmax(kg_lt, 0));  dtau = realization-weighted mtr_kg_lt shift
# Base leg = baseline/static/detail; shock legs = {scn}/conventional/detail.
#
# Hand-iterate KG_TIMEABLE_SHARE_LOGS (start 0.2542) until E_full_short is within
# ~5% of 5.04 (the v3 pin's own tolerance); the bathtub dilution is unstable in
# the share, so a secant by hand beats the auto loop (single_pool memo).
#
# CLI:  Rscript measure_shortrun_logs.R <vintage> [<local_root>]
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

args       = commandArgs(trailingOnly = TRUE)
VINTAGE    = if (length(args) >= 1) args[1] else stop('need <vintage>')
LOCAL_ROOT = if (length(args) >= 2) args[2] else
  '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
ROOT = file.path(LOCAL_ROOT, VINTAGE)

TARGET_SHORT = 5.04
TARGET_LONG  = -2.52
LONG_ANCHOR     = 2055
SHORT_ANCHOR    = 2026
SHORT_DTAU_YEAR = 2027

read_detail = function(scn, pass, yr) {
  f = file.path(ROOT, scn, pass, 'detail', paste0(yr, '.csv'))
  if (!file.exists(f)) stop('missing detail: ', f)
  fread(f, select = c('id', 'weight', 'kg_lt', 'mtr_kg_lt'),
        showProgress = FALSE) %>% as_tibble()
}
agg = function(df) df %>%
  mutate(kg_pos = pmax(kg_lt, 0)) %>%
  summarise(R      = sum(weight * kg_pos, na.rm = TRUE),
            tau_rw = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                     sum(weight * kg_pos,             na.rm = TRUE)) %>% as.list()
base_at  = function(yr)      agg(read_detail('baseline', 'static', yr))
shock_at = function(scn, yr) agg(read_detail(scn, 'conventional', yr))

# --- short-run (delayed, announced 2026 / effective 2027) -------------------
bS  = base_at(SHORT_ANCHOR);  sS  = shock_at('delayed', SHORT_ANCHOR)
bS2 = base_at(SHORT_DTAU_YEAR); sS2 = shock_at('delayed', SHORT_DTAU_YEAR)
dtau_short   = sS2$tau_rw - bS2$tau_rw
E_full_short = log(sS$R / bS$R) / dtau_short

# --- long-run sanity (permanent +2pp @ 2055) --------------------------------
bL = base_at(LONG_ANCHOR); sL = shock_at('rate_up_2pp', LONG_ANCHOR)
dtau_long   = sL$tau_rw - bL$tau_rw
E_full_long = log(sL$R / bL$R) / dtau_long

cat(sprintf('\nVintage: %s\n', ROOT))
cat(sprintf('\nSHORT-RUN  E_full = %+.4f   (target %+.2f, %+.1f%%)\n',
            E_full_short, TARGET_SHORT, 100 * (E_full_short / TARGET_SHORT - 1)))
cat(sprintf('  R_base(2026) = %.1f $B  R_delayed(2026) = %.1f $B  dtau(2027) = %+.5f\n',
            bS$R/1e9, sS$R/1e9, dtau_short))
cat(sprintf('LONG-RUN   E_full = %+.4f   (target %+.2f, %+.1f%%; timeable-invariant sanity)\n',
            E_full_long, TARGET_LONG, 100 * (E_full_long / TARGET_LONG - 1)))
cat(sprintf('  R_base(2055) = %.1f $B  R_rate2pp(2055) = %.1f $B  dtau(2055) = %+.5f\n',
            bL$R/1e9, sL$R/1e9, dtau_long))
cat('\nIf short-run is off target, adjust KG_TIMEABLE_SHARE_LOGS and re-run\n')
cat('(higher share -> larger short-run response). Long-run should barely move.\n')

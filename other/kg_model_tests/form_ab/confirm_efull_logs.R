#-------------------------------------------------------------------------------
# confirm_efull_logs.R  --  Part B1 confirmation (moment-reproduction check).
#
# Measures full-sim E_full at the PINNED eta_tilde* on the same convention as
# the grid (base R + dtau from the levels central eta_dial_c_v2, R_shock from
# the confirmation vintage's conventional_no_wealth leg). PASS if within +-2%
# of the -2.52 target.
#
# CLI:  Rscript confirm_efull_logs.R <confirm_vintage> <eta_tilde>
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })

args = commandArgs(trailingOnly = TRUE)
CONFIRM_VINTAGE = if (length(args) >= 1) args[1] else 'eta_dial_logs_confirm'
ETA_TILDE       = if (length(args) >= 2) as.numeric(args[2]) else NA_real_

LOCAL_ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
YEAR       = 2055
SHOCK      = 's_cg_r25'
BASE_DTAU_VINTAGE = 'eta_dial_c_v2'   # form-invariant baseline + shared-law dtau
E_FULL_TARGET = -0.6 / 0.238
TOL = 0.02

agg = function(f, with_mtr = TRUE) {
  cols = c('weight', 'kg_lt', if (with_mtr) 'mtr_kg_lt')
  fread(f, select = cols, showProgress = FALSE) %>% as_tibble() %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    summarise(R = sum(weight * kg_pos, na.rm = TRUE),
              tau_rw = if (with_mtr) sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                                     sum(weight * kg_pos, na.rm = TRUE) else NA_real_) %>%
    as.list()
}

base = agg(file.path(LOCAL_ROOT, BASE_DTAU_VINTAGE, 'baseline', 'static',
                     'detail', paste0(YEAR, '.csv')))
s_c  = agg(file.path(LOCAL_ROOT, BASE_DTAU_VINTAGE, SHOCK, 'conventional',
                     'detail', paste0(YEAR, '.csv')))
dtau = s_c$tau_rw - base$tau_rw
s    = agg(file.path(LOCAL_ROOT, CONFIRM_VINTAGE, SHOCK, 'conventional_no_wealth',
                     'detail', paste0(YEAR, '.csv')), with_mtr = FALSE)
E_full = log(s$R / base$R) / dtau
dev    = E_full / E_FULL_TARGET - 1

cat(sprintf('\nCONFIRMATION  eta_tilde* = %s\n', format(ETA_TILDE)))
cat(sprintf('  R_base = %.1f $B  R_shock = %.1f $B  dtau = %+.5f\n',
            base$R/1e9, s$R/1e9, dtau))
cat(sprintf('  E_full = %+.4f   target %+.4f   deviation %+.2f%% (tol +-%.0f%%)\n',
            E_full, E_FULL_TARGET, 100*dev, 100*TOL))
if (abs(dev) <= TOL) cat('  VERDICT: PASS -- moment reproduced\n') else
                     cat('  VERDICT: OUT OF TOLERANCE -- re-pin\n')

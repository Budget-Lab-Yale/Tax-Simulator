#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# sweep_state_weights.R
#
# Phase 1 §4 comparison harness, part 1: hyperparameter sweep of the joint
# gradient fit (counts-backbone IPF prior -> fit_gradient) on the cached 2022
# inputs. One config per SLURM array task; --collect aggregates.
#
# Usage (from the repo root):
#   Rscript other/state_tax_research/sweep_state_weights.R --prior
#       build + cache the shared counts-IPF prior (run once, before the array)
#   Rscript other/state_tax_research/sweep_state_weights.R <config_id>
#       run one sweep config (1..nrow of CONFIGS)
#   Rscript other/state_tax_research/sweep_state_weights.R --collect
#       print the comparison table across all completed configs
#
# Inputs (scratch): hardened_fit_2022.rds  (build_weight_inputs() output;
# see state_weights_fit_issues.md). Outputs land in {SCRATCH}/sweep/.
#
# Metrics per config (spec: state_weights_ml_alternative.md §4):
#   1 target fidelity  - within-2% share and MARD, overall and per series
#   3 weight quality   - Kish ESS by state (min/median across states),
#                        near-degenerate rows (max share > 0.99), share of
#                        near-zero cells, max within-record share
#   4 invariance       - max |rowSums(P) - 1|
#   + take-up covariate: cor of n_returns / eitc_amt signed state errors with
#     IRS TY2022 EITC participation (untargeted-geography diagnostic; the
#     n_returns correlation was -0.45 pre-sweep, see workstream log 2026-07-19)
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml)
})
source('src/data/state_weights.R')

SCRATCH   <- '/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp'
SWEEP_DIR <- file.path(SCRATCH, 'sweep')
PRIOR_RDS <- file.path(SWEEP_DIR, 'prior_counts_2022.rds')

# Laggard series from the 300-step baseline (within-2% 61-77%): SALT/re-tax
# and the EITC/AGI families, plus n_indiv
LAGGARDS <- c('re_tax_amt', 'n_re_tax', 'salt_amt', 'n_salt',
              'eitc_amt', 'n_eitc', 'agi_amt', 'n_indiv')

CONFIGS <- tibble::tribble(
  ~id, ~n_steps, ~lr,  ~schedule,  ~beta, ~lambda_up,
  1L,  1000L,    0.1,  'constant', 1e-3,  1,
  2L,  3000L,    0.1,  'constant', 1e-3,  1,
  3L,  1000L,    0.1,  'cosine',   1e-3,  1,
  4L,  3000L,    0.1,  'cosine',   1e-3,  1,
  5L,  1000L,    0.1,  'cosine',   1e-3,  4,
  6L,  3000L,    0.1,  'cosine',   1e-3,  4,
  7L,  3000L,    0.1,  'cosine',   1e-4,  1,
  8L,  3000L,    0.1,  'cosine',   1e-4,  4,
  9L,  3000L,    0.2,  'cosine',   1e-3,  1,
  10L, 1000L,    0.1,  'constant', 1e-2,  1,
  # round 2: beta was the binding constraint (1e-4 -> 95.3% within-2%);
  # probe the floor and a longer budget at the round-1 winner
  11L, 3000L,    0.1,  'cosine',   1e-5,  1,
  12L, 3000L,    0.1,  'cosine',   1e-5,  4,
  13L, 6000L,    0.1,  'cosine',   1e-4,  4
)

# IRS EITC participation rates by state, TY2022 (ACS-Census linkage;
# irs.gov/tax-professionals/eitc-central/eitc-participation-rate-by-state)
EITC_TAKEUP <- c(
  AL=79.2, AK=75.1, AZ=78.6, AR=80.5, CA=77.4, CO=77.3, CT=82.0, DE=82.4,
  DC=73.6, FL=82.0, GA=82.1, HI=83.3, ID=80.7, IL=81.0, IN=83.3, IA=80.7,
  KS=78.3, KY=82.5, LA=81.6, ME=85.2, MD=82.2, MA=81.7, MI=82.8, MN=81.9,
  MS=80.2, MO=79.3, MT=75.1, NE=83.2, NV=82.2, NH=80.0, NJ=81.1, NM=83.6,
  NY=83.7, NC=79.7, ND=79.6, OH=82.6, OK=79.1, OR=78.4, PA=82.8, RI=82.4,
  SC=80.7, SD=82.3, TN=82.0, TX=80.6, UT=75.4, VT=80.0, VA=82.2, WA=77.4,
  WV=85.1, WI=78.9, WY=78.6)

# Engine jurisdiction order: sort(unique(ht2$state)) (build_weight_inputs)
STATE_ORDER <- sort(c(STATE_JURISDICTIONS, NONTAX_BUCKETS))

load_inputs <- function() {
  h5 <- readRDS(file.path(SCRATCH, 'hardened_fit_2022.rds'))
  p  <- h5$inputs$filers
  rm(h5); invisible(gc())
  p
}

target_errors <- function(p, P) {
  rbindlist(lapply(p$targets, function(t) data.table(
    series = t$series,
    state  = STATE_ORDER[t$state],
    target = t$target,
    that   = sum(p$w[t$rows] * P[t$rows, t$state] * t$x)
  )))[, `:=`(err = abs(that / target - 1), sgn_err = that / target - 1)]
}

weight_quality <- function(w, P) {
  W <- w * P
  ess <- colSums(W)^2 / colSums(W^2)                  # Kish per state
  list(
    ess_min          = min(ess),
    ess_median       = median(ess),
    max_share_p99    = quantile(apply(P, 1, max), 0.99),
    degenerate_share = mean(apply(P, 1, max) > 0.99),
    nearzero_share   = mean(P < 1e-6),
    invariant_maxerr = max(abs(rowSums(P) - 1))
  )
}

takeup_cors <- function(errs) {
  tk <- data.table(state = names(EITC_TAKEUP), takeup = EITC_TAKEUP)
  sapply(c('n_returns', 'eitc_amt'), function(ser) {
    st <- errs[series == ser,
               .(sgn_err = weighted.mean(sgn_err, abs(target)),
                 tw = sum(abs(target))), by = state]
    st <- merge(st, tk, by = 'state')
    cov.wt(cbind(st$sgn_err, st$takeup), wt = st$tw, cor = TRUE)$cor[1, 2]
  })
}

args <- commandArgs(trailingOnly = TRUE)
dir.create(SWEEP_DIR, showWarnings = FALSE, recursive = TRUE)

#--------------------------------------
# Mode 1: build the shared counts prior
#--------------------------------------
if (identical(args[1], '--prior')) {
  p <- load_inputs()
  message('building counts-backbone IPF prior...')
  counts <- Filter(function(t) t$series == 'n_returns', p$targets)
  prior  <- fit_calibration(p$w, p$P0, counts, n_iter = 50)$P
  saveRDS(prior, PRIOR_RDS)
  message('wrote ', PRIOR_RDS)
  quit(save = 'no')
}

#--------------------------------------
# Mode 2: collect results
#--------------------------------------
if (identical(args[1], '--collect')) {
  files <- list.files(SWEEP_DIR, pattern = '^metrics_\\d+\\.rds$', full.names = TRUE)
  if (length(files) == 0) stop('no metrics files in ', SWEEP_DIR)
  res <- rbindlist(lapply(files, function(f) as.data.table(readRDS(f)$row)))
  setorder(res, -within2)
  cat('\n=== Sweep results (sorted by overall within-2%) ===\n')
  print(res[, .(id, n_steps, lr, schedule, beta, lambda_up,
                within2 = round(100 * within2, 1),
                mard = round(100 * mard, 2),
                w2_lagg = round(100 * within2_laggards, 1),
                ess_min = round(ess_min),
                degen = round(100 * degenerate_share, 2),
                nret_tk_cor = round(cor_takeup_n_returns, 2),
                mins = round(runtime_min, 1))],
        row.names = FALSE)
  best <- res$id[1]
  cat('\n=== Per-series table, best config (id ', best, ') ===\n', sep = '')
  print(readRDS(file.path(SWEEP_DIR, paste0('metrics_', best, '.rds')))$per_series)
  quit(save = 'no')
}

#--------------------------------------
# Mode 3: run one config
#--------------------------------------
cfg_id <- as.integer(args[1])
cfg    <- CONFIGS[CONFIGS$id == cfg_id, ]
stopifnot(nrow(cfg) == 1)
message('config ', cfg_id, ': ', paste(names(cfg), unlist(cfg), collapse = ' '))

p     <- load_inputs()
prior <- readRDS(PRIOR_RDS)

targets <- lapply(p$targets, function(t) {
  if (cfg$lambda_up != 1 && t$series %in% LAGGARDS) t$lambda <- t$lambda * cfg$lambda_up
  t
})

t0  <- Sys.time()
fit <- fit_gradient(p$w, prior, targets,
                    beta = cfg$beta, lr = cfg$lr, n_steps = cfg$n_steps,
                    lr_schedule = cfg$schedule, verbose = TRUE)
runtime_min <- as.numeric(difftime(Sys.time(), t0, units = 'mins'))

# Metrics are always computed against the ORIGINAL (un-up-weighted) targets
errs <- target_errors(p, fit$P)
wq   <- weight_quality(p$w, fit$P)
tk   <- takeup_cors(errs)

per_series <- errs[, .(within2 = round(100 * mean(err <= 0.02), 1),
                       mard = round(100 * mean(err), 2)),
                   by = series][order(within2)]

row <- c(as.list(cfg), wq, list(
  within2          = mean(errs$err <= 0.02),
  mard             = mean(errs$err),
  within2_laggards = mean(errs[series %in% LAGGARDS]$err <= 0.02),
  loss_final       = tail(fit$loss_hist, 1),
  cor_takeup_n_returns = unname(tk['n_returns']),
  cor_takeup_eitc_amt  = unname(tk['eitc_amt']),
  runtime_min      = runtime_min
))

saveRDS(list(row = row, per_series = per_series,
             loss_hist = fit$loss_hist),
        file.path(SWEEP_DIR, paste0('metrics_', cfg_id, '.rds')))
saveRDS(fit, file.path(SWEEP_DIR, paste0('fit_', cfg_id, '.rds')))

cat(sprintf('\nconfig %d: within2 %.1f%% | MARD %.2f%% | laggards %.1f%% | %.0f min\n',
            cfg_id, 100 * row$within2, 100 * row$mard,
            100 * row$within2_laggards, runtime_min))
print(per_series)

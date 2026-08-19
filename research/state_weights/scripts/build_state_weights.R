#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/build_state_weights.R
#
# Standalone Phase 1 prototype driver: assembles PUF-side targets from the
# shared IRS-Ind (HT2) and ACS stores, fits split state weights with the
# chosen engine, runs diagnostics, and writes state_weights_{year}.csv in the
# final Tax-Data interface format (plan §2.1, §5.2).
#
# Usage:
#   Rscript research/state_weights/scripts/build_state_weights.R \
#     <year> <method: calibration|gradient> <baseline_detail_dir> [out_dir]
#
# The baseline detail dir must contain {year}.csv with id, agi, eitc (any
# Tax-Simulator baseline static detail output works). Run from the repo root.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr)
  library(stringr); library(yaml)
})
source('src/data/state_weights.R')

args       <- commandArgs(trailingOnly = TRUE)
year       <- as.integer(args[1])
method     <- args[2]
detail_dir <- args[3]
out_dir    <- if (length(args) >= 4) args[4] else '.'

t0 <- Sys.time()

#---------------
# Load the PUF
#---------------

message('Loading PUF + baseline detail for ', year)
input_cols <- c('id','weight','filer','filing_status','n_dep','age1',
                'wages','txbl_int','exempt_int','div_ord','div_pref',
                'kg_st','kg_lt','salt_inc_sales','salt_prop',
                'first_mort_int','second_mort_int','txbl_pens_dist',
                'txbl_ira_dist','gross_ss','sole_prop')
tax_data_root <- read_yaml('./config/interfaces/interface_versions.yaml')
td_path <- file.path(read_yaml('./config/interfaces/output_roots.yaml')$production,
                     'model_data/Tax-Data/v1',
                     tax_data_root$`Tax-Data`$default_vintage, 'baseline',
                     sprintf('tax_units_%d.csv', year))
tu <- fread(td_path, select = input_cols)
detail <- fread(file.path(detail_dir, sprintf('%d.csv', year)),
                select = c('id','agi','eitc'))
tu <- merge(tu, detail, by = 'id', all.x = TRUE)
stopifnot(!anyNA(tu$agi))

#-------------------------
# Assemble and fit
#-------------------------

message('Assembling targets')
inputs <- build_weight_inputs(tu, year)

message('Fitting (', method, ')')
weights <- build_split_weights(tu, year, method = method, inputs = inputs,
                               verbose = TRUE)

#-------------------------
# Diagnostics
#-------------------------

# Split-weight invariant
chk <- weights[, .(w_split = sum(weight)), by = id] |>
  merge(tu[, .(id, weight)], by = 'id')
max_inv_err <- chk[, max(abs(w_split - weight) / pmax(weight, 1e-9))]
message(sprintf('Invariant: max relative row-sum error = %.2e', max_inv_err))

# Target fit (both partitions)
fit_stats <- function(p, P) {
  hit <- vapply(p$targets, function(t) {
    That <- sum(p$w[t$rows] * P[t$rows, t$state] * t$x)
    if (t$target == 0) return(NA_real_)
    abs(That / t$target - 1)
  }, numeric(1))
  hit[is.finite(hit)]
}
P_f <- {
  fit_fn <- if (method == 'gradient') fit_gradient else fit_calibration
  NULL  # refit avoided: recompute achieved fit from weights instead
}
# Recompute achieved relative errors from the assembled weights
W_wide <- dcast(weights, id ~ state, value.var = 'weight', fill = 0)
setkey(W_wide, id)
achieved <- function(part) {
  p   <- inputs[[part]]
  ids <- tu$id[p$idx]
  Wm  <- as.matrix(W_wide[J(ids), inputs$jurisdictions, with = FALSE])
  Wm[is.na(Wm)] <- 0
  errs <- vapply(p$targets, function(t) {
    That <- sum(Wm[t$rows, t$state] * t$x)
    if (t$target == 0) return(NA_real_)
    abs(That / t$target - 1)
  }, numeric(1))
  errs[is.finite(errs)]
}
for (part in c('filers', 'nonfilers')) {
  e <- achieved(part)
  message(sprintf('%s: %d targets | within 2%%: %.1f%% | median err %.3f%% | MARD %.3f%%',
                  part, length(e), 100 * mean(e <= 0.02),
                  100 * median(e), 100 * mean(e)))
}

# Spot state shares
shares <- weights[, .(w = sum(weight)), by = state][order(-w)]
shares[, share := w / sum(w)]
message('Top-5 state shares of national weight: ',
        paste(sprintf('%s %.1f%%', shares$state[1:5], 100 * shares$share[1:5]),
              collapse = ', '))

#-------------------------
# Write
#-------------------------

out_file <- file.path(out_dir, sprintf('state_weights_%d.csv', year))
fwrite(weights[order(id, state)], out_file)
message('Wrote ', out_file, '  (', nrow(weights), ' rows)')
message(sprintf('Total runtime: %.1f min', as.numeric(difftime(Sys.time(), t0, units = 'mins'))))

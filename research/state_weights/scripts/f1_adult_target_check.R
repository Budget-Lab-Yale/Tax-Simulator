#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# F1 / D5 verification: does the non-filer partition now target ADULTS, and how
# well does the fit place them across states?
#
# Compares FITTED non-filer adult SHARES by state against the residual anchors'
# shares. SHARES, deliberately: the national LEVEL gap (PUF 32.4M non-filer
# adults against a ~47M anchor) is what F1's other half (anchors as the primary
# targets) and the Tax-Data rake (D1/D2) exist to close, and neither has
# landed. Comparing levels here would measure those, not this.
#
# Prerequisite: acs_margins_gqdiff_{year}.csv must carry `n_adults`, i.e. it was
# produced by a build_acs_margins() at or after the F1 x-vector landed. Script
# 03 --acs regenerates it (sbatch; the extract read OOMs the login node).
#
# Usage (repo root; login node is fine -- this fits only the non-filer
# partition, ~13k records):
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_weights/scripts/f1_adult_target_check.R [year]
#------------------------------------------------------------------------------
suppressPackageStartupMessages({library(data.table); library(dplyr); library(yaml)})
source('src/data/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2022L
d    <- 'research/state_weights/nonfiler_residual/results'
out  <- 'output'
dir.create(out, showWarnings = FALSE)

roots <- read_yaml('./config/interfaces/output_roots.yaml')
vint  <- read_yaml('./config/interfaces/interface_versions.yaml')$`Tax-Data`$default_vintage
tu <- fread(file.path(roots$production, 'model_data/Tax-Data/v1', vint,
                      'baseline', sprintf('tax_units_%d.csv', year)))
cat('Tax-Data vintage', vint, '|', nrow(tu), 'units\n')

mf <- file.path(d, sprintf('acs_margins_gqdiff_%d.csv', year))
marg <- list(nonfiler_margins = fread(mf))
if (!'n_adults' %in% names(marg$nonfiler_margins))
  stop(mf, ' has no `n_adults` column -- regenerate it with script 03 --acs ',
       '(it predates the F1 x-vector)')

# NOTE the filer partition is deliberately unusable here: the raw Tax-Data file
# carries no baseline-calculated `agi`, so assign_ht2_stub() returns NA and the
# filer loop emits ZERO targets rather than wrong ones. That is the documented
# contract of build_weight_inputs() ("join from baseline detail when running
# standalone") and it is why this script only reads $nonfilers.
inp <- build_weight_inputs(tu, year, ht2 = read_ht2(ht2_path(year), year),
                           acs_margins = marg, verbose = TRUE)
pn <- inp$nonfilers
stopifnot(length(pn$targets) > 0,
          all(vapply(pn$targets, `[[`, character(1), 'series') == 'n_adults'))

fit <- fit_calibration(pn$w, pn$P0, pn$targets, n_iter = 200)
cat(sprintf('\nfit: %d iters, max|rel err| %.3e, %d unfittable\n',
            fit$iters, fit$maxrel, length(fit$unfittable)))
stopifnot(all(fit$P >= 0), max(abs(rowSums(fit$P) - 1)) < 1e-8)

x <- puf_series_x(tu[pn$idx], 'n_adults')
anc <- fread(file.path(d, sprintf('residual_anchors_%d.csv', year)))
z <- merge(data.table(state = inp$jurisdictions,
                      adults = as.vector(crossprod(pn$w * x, fit$P))),
           anc[, .(state, raw = residual_nonfiling_adults,
                   net_dorm = residual_nonfiling_adults_net_dorm)],
           by = 'state')
z[, sh_fit := adults/sum(adults)]

# Both anchor universes, side by side. `net_dorm` is the one that matches this
# partition -- build_acs_margins() removed the dorm students from the margin and
# the PUF non-filer partition carries no dependents at all (0 of 13,204 records
# with dep_status == 1), so the netted anchor is the like-for-like comparison
# and `raw` is kept only to show what the netting bought.
COLLEGE <- c('DC','VT','RI','MA','CT','ND','NH','DE')
panel <- rbindlist(lapply(c('raw','net_dorm'), function(v) {
  if (all(is.na(z[[v]]))) return(NULL)
  r <- z$sh_fit / (z[[v]]/sum(z[[v]])); e <- abs(r - 1)
  data.table(anchor = v, MARD = round(100*mean(e), 2),
             median = round(100*median(e), 2), sd = round(sd(r), 4),
             w5 = sum(e < .05), w10 = sum(e < .10), w20 = sum(e < .20),
             worst_pct = round(100*max(e), 1),
             college8 = round(100*mean(e[z$state %in% COLLEGE]), 2),
             other43  = round(100*mean(e[!z$state %in% COLLEGE]), 2))
}))

cat(sprintf('\nfitted non-filer adults %.3fM | anchor raw %.3fM, net of dorm %.3fM\n',
            sum(z$adults)/1e6, sum(z$raw)/1e6, sum(z$net_dorm)/1e6))
cat('(levels are NOT compared -- the gap is what F1b and the D1 rake close)\n\n')
cat('=== fitted adult SHARES vs anchor SHARES ===\n'); print(panel)

if (!all(is.na(z$net_dorm))) {
  z[, `:=`(r_raw = sh_fit/(raw/sum(raw)), r_net = sh_fit/(net_dorm/sum(net_dorm)))]
  cat('\n=== the states the netting is for ===\n')
  print(z[state %in% COLLEGE, .(state, r_raw = round(r_raw,3),
          r_net = round(r_net,3))][order(r_raw)])
}
f <- file.path(out, sprintf('f1_nonfiler_share_check_%d.csv', year))
fwrite(z, f); cat('\nWrote', f, '\n')

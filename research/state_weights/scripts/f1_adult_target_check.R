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
z <- merge(data.table(state = inp$jurisdictions,
                      adults = as.vector(crossprod(pn$w * x, fit$P))),
           fread(file.path(d, sprintf('residual_anchors_%d.csv', year)))[
             , .(state, anchor = residual_nonfiling_adults)],
           by = 'state')
z[, `:=`(sh_fit = adults/sum(adults), sh_anc = anchor/sum(anchor))]
z[, ratio := sh_fit/sh_anc]

cat(sprintf('\nfitted non-filer adults %.3fM | anchor %.3fM (level gap is F1b/D1, not this)\n',
            sum(z$adults)/1e6, sum(z$anchor)/1e6))
cat(sprintf('SHARE vs anchor: MARD %.2f%%  sd %.4f  min %.3f (%s)  max %.3f (%s)\n',
    100*mean(abs(z$ratio-1)), sd(z$ratio), min(z$ratio), z$state[which.min(z$ratio)],
    max(z$ratio), z$state[which.max(z$ratio)]))
cat(sprintf('within 5%%: %d/%d | within 10%%: %d/%d\n',
    z[abs(ratio-1)<.05,.N], nrow(z), z[abs(ratio-1)<.10,.N], nrow(z)))
cat('\nworst 8 by share error:\n')
print(head(z[order(-abs(ratio-1)),
             .(state, sh_fit = round(100*sh_fit,3), sh_anc = round(100*sh_anc,3),
               ratio = round(ratio,3))], 8))
f <- file.path(out, sprintf('f1_nonfiler_share_check_%d.csv', year))
fwrite(z, f); cat('\nWrote', f, '\n')

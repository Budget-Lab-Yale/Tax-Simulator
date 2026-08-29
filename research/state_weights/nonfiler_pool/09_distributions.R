#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 09_distributions.R
#
# Where the constructed non-filers sit: income, age and family structure, each
# split by whether the unit meets the filing requirement. The split is the
# point -- the two groups are produced by DIFFERENT models (Mok's probits below
# the threshold, the Pub 5785 hazard above it) and they are not the same
# population, so reporting them only in aggregate hides which model is doing
# what.
#
# Counts ADULTS throughout, consistent with every other target in this work: a
# joint unit contributes two adults at the unit's income. Income is the tax
# gross-income concept the threshold test itself uses, so the above/below split
# and the income axis are measured on the same scale.
#
# Writes: results/distributions_{year}.csv (long: dimension, bin, group, adults)
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/09_distributions.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(yaml)
})
source('src/data/state_weights.R')
source('src/data/asec_tax_units.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else c(2017L, 2022L)
RES   <- 'research/state_weights/nonfiler_pool/results'

INC_BREAKS <- c(-Inf, 0, 5e3, 10e3, 15e3, 25e3, 50e3, Inf)
INC_LABELS <- c('none', '$1-5k', '$5-10k', '$10-15k', '$15-25k', '$25-50k', '$50k+')

for (yr in YEARS) {
  message('=== TY', yr)

  u  <- readRDS(file.path(RES, sprintf('calibrated_units_%d.rds', yr)))
  gq <- fread(file.path(RES, sprintf('gq_persons_%d.csv.gz', yr)))

  # Household side: non-filing adults carried by each unit
  hh <- u[unit_type == 'nondependent' & age_head >= 18,
          .(adults  = weight * (1 - p_file_cal) * (1 + (filing_status == 'joint')),
            income  = gross_income,
            age     = age_head,
            married = filing_status == 'joint',
            has_dep = n_dep > 0,
            above   = must_file)]

  # Group-quarters side: one adult each, income on the same concept the GQ
  # threshold test used (total less Social Security)
  g <- gq[!is.na(p_file) & AGE >= 18 & p_file < 1,
          .(adults  = PERWT * (1 - p_file),
            income  = INCTOT - INCSS,
            age     = AGE,
            married = FALSE,
            has_dep = FALSE,
            above   = must_file)]

  d <- rbindlist(list(hh, g))
  d <- d[adults > 0]
  d[, group := fifelse(above, 'above threshold', 'below threshold')]

  d[, inc_bin := cut(income, INC_BREAKS, labels = INC_LABELS, right = FALSE)]
  # `none` should mean exactly zero or a loss, not "under $1"
  d[income <= 0, inc_bin := 'none']
  d[, age_bin := factor(as.character(age_band(age)),
                        levels = c('18_25','26_34','35_44','45_54','55_64','65_74','75p'))]
  d[, fam_bin := factor(fcase(
      married & has_dep,   'married, dependents',
      married & !has_dep,  'married, none',
      !married & has_dep,  'unmarried, dependents',
      default              = 'unmarried, none'),
      levels = c('unmarried, none', 'unmarried, dependents',
                 'married, none', 'married, dependents'))]

  dims <- c(income = 'inc_bin', age = 'age_bin', family = 'fam_bin')
  out <- rbindlist(lapply(dims, function(v) {
    z <- d[, .(adults = sum(adults)), by = c('group', v)]
    setnames(z, v, 'bin')
    z[, .(bin = as.character(bin), group, adults)]
  }), idcol = 'dimension')

  tot <- d[, sum(adults)]
  out[, share_of_all := adults / tot]
  out[, tax_year := yr]
  setorder(out, dimension, group, bin)
  fwrite(out, file.path(RES, sprintf('distributions_%d.csv', yr)))

  #---------------------------------------------------------------------------
  # Report
  #---------------------------------------------------------------------------
  ab <- d[, .(adults = sum(adults)), by = group]
  message(sprintf('  %.2fM non-filing adults: %.2fM below threshold, %.2fM above (%.1f%%)',
                  tot / 1e6, ab[group == 'below threshold', adults] / 1e6,
                  ab[group == 'above threshold', adults] / 1e6,
                  100 * ab[group == 'above threshold', adults] / tot))

  for (dim in c('income', 'age', 'family')) {
    message('  --- ', dim)
    w <- dcast(out[dimension == dim], bin ~ group, value.var = 'adults', fill = 0)
    lev <- switch(dim, income = INC_LABELS,
                  age = c('18_25','26_34','35_44','45_54','55_64','65_74','75p'),
                  family = c('unmarried, none','unmarried, dependents',
                             'married, none','married, dependents'))
    w <- w[match(lev, bin)][!is.na(bin)]
    for (i in seq_len(nrow(w)))
      message(sprintf('    %-22s all %5.2fM (%4.1f%%) | below %5.2fM | above %5.2fM',
                      w$bin[i],
                      (w$`below threshold`[i] + w$`above threshold`[i]) / 1e6,
                      100 * (w$`below threshold`[i] + w$`above threshold`[i]) / tot,
                      w$`below threshold`[i] / 1e6, w$`above threshold`[i] / 1e6))
  }
  message('  wrote distributions_', yr, '.csv')
}

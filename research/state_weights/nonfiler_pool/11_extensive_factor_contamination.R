#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 11_extensive_factor_contamination.R  (group D stage C, proposal 1)
#
# Does swapping the non-filer file move FILER results? The federal validation
# battery's first test (E2) says it must not: every 1040 aggregate is summed
# `* weight * filer`, so `totals/1040.csv` and `cbo_comparison.csv` must be
# identical under current law. This script measures the one channel by which
# a non-filer file can reach filer output anyway.
#
# THE CHANNEL. Tax-Data ages each variable with
#
#     applied(V, y) = income_factor(grow_with(V), y) / extensive_factor(V, y)
#     extensive_factor(V, y) = sum((V != 0) * w_y) / sum((V != 0) * w_2019)
#
# (project_puf.R, build_factor_rows_2020plus). That sum runs over `tax_units`,
# and `main.R` appends the non-filers at line 41 while project_puf runs at
# line 49 -- so non-filer records are INSIDE the mask. The factor is a single
# number per (year, variable) with no filer dimension, so whatever the
# non-filers do to it lands on filers too.
#
# It only bites when the two groups' weights grow at different rates, and they
# do, by construction: compute_weights_for_year gives filers IRS return-count
# growth and non-filers pure demographic growth, and non-filers skew old.
#
# Decomposing the mask into filers F and non-filers N, with s = the non-filer
# share of the 2019 mask weight and r = each group's weight growth:
#
#     extensive_new / extensive_old - 1  =  s * (r_N / r_F - 1)
#
# so the bias is the product of how much of the mask is non-filer and how far
# apart the two growth paths run. Because the extensive factor DIVIDES, a
# positive bias here makes filer values grow too SLOWLY.
#
# Writes: results/extensive_factor_contamination.csv
#
# Login-node safe (reads two ledgers and one pool file).
#   Rscript research/state_weights/nonfiler_pool/11_extensive_factor_contamination.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readr); library(yaml)
})
source('src/data/state_weights.R')

RES     <- 'research/state_weights/nonfiler_pool/results'
# Vintages are pinned deliberately: these numbers describe THIS Tax-Data
# build. Paths come from output_roots.yaml, never typed in.
TAXDATA_VINTAGE <- c(model = 'Tax-Data',            version = 'v1', vintage = '2026081216')
SOI_VINTAGE     <- c(model = 'Compiled-SOI-Tables', version = 'v3', vintage = '20250319')
TAXDATA <- model_data_path(TAXDATA_VINTAGE[['model']], TAXDATA_VINTAGE[['version']],
                           TAXDATA_VINTAGE[['vintage']], 'baseline')
SOI     <- model_data_path(SOI_VINTAGE[['model']], SOI_VINTAGE[['version']],
                           SOI_VINTAGE[['vintage']], 'historical', 'table_1_4.csv')
REPORT_YEARS <- c(2020L, 2025L, 2035L, 2055L)

# id >= 1e6 is the appended non-filer convention (impute_nonfilers.R)
NONFILER_ID_FLOOR <- 1e6

#-------------------------------------------------------------------------------
# How far apart do the two groups' weights grow?
#-------------------------------------------------------------------------------
wl <- as.data.table(readRDS(file.path(TAXDATA, 'weight_ledger.rds')))
wl[, grp := fifelse(id >= NONFILER_ID_FLOOR, 'nonfiler', 'filer')]
stopifnot(uniqueN(wl$grp) == 2)

base19 <- wl[year == 2019L, .(b = sum(weight)), by = grp]
growth <- rbindlist(lapply(REPORT_YEARS, function(y) {
  m <- merge(wl[year == y, .(a = sum(weight)), by = grp], base19, by = 'grp')
  m[, r := a / b]
  data.table(year        = y,
             r_filer     = m[grp == 'filer',    r],
             r_nonfiler  = m[grp == 'nonfiler', r])
}))
growth[, divergence := r_nonfiler / r_filer - 1]

message('=== weight growth from 2019, by group')
for (i in seq_len(nrow(growth))) {
  message(sprintf('  %d: filer x%.4f | non-filer x%.4f | non-filers run %+.2f%% faster',
                  growth$year[i], growth$r_filer[i], growth$r_nonfiler[i],
                  100 * growth$divergence[i]))
}

#-------------------------------------------------------------------------------
# How much of each variable's mask is non-filer?
#
# Filer side from SOI TY2017 return counts; non-filer side from our own pool.
# `dina_in_mask` records whether the file being REPLACED also sat in that
# mask -- the distinction that decides whether the swap introduces a channel
# or merely changes one that is already open.
#-------------------------------------------------------------------------------
t14 <- as.data.table(read_csv(SOI, show_col_types = FALSE))[year == 2017]
pool <- fread(file.path(RES, 'nonfiler_pool_2017.csv.gz'))

vars <- list(
  interest  = list(soi = 'count__txbl_int',        pool = 'txbl_int',        dina = FALSE),
  dividends = list(soi = 'count__div',             pool = 'div_pref',        dina = FALSE),
  wages     = list(soi = 'count__wages',           pool = 'wages',           dina = TRUE),
  pensions  = list(soi = 'count__gross_pens_dist', pool = 'txbl_pens_dist',  dina = TRUE)
)

out <- rbindlist(lapply(names(vars), function(v) {
  spec <- vars[[v]]
  stopifnot(spec$soi %in% names(t14), spec$pool %in% names(pool))
  filer_n <- sum(t14[[spec$soi]])
  nf_n    <- pool[get(spec$pool) != 0, sum(weight)]
  s       <- nf_n / (filer_n + nf_n)
  rbindlist(lapply(seq_len(nrow(growth)), function(i) {
    data.table(variable        = v,
               dina_already_in_mask = spec$dina,
               filer_records   = filer_n,
               nonfiler_records = nf_n,
               nonfiler_share  = s,
               year            = growth$year[i],
               divergence      = growth$divergence[i],
               factor_bias     = s * growth$divergence[i])
  }))
}))

fwrite(out, file.path(RES, 'extensive_factor_contamination.csv'))

message('\n=== bias in the shared growth factor, by variable')
message(sprintf('%-10s %8s %8s %7s  %s', 'variable', 'filer M', 'ours M', 'share',
                paste(sprintf('%8d', REPORT_YEARS), collapse = '')))
for (v in names(vars)) {
  d <- out[variable == v]
  message(sprintf('%-10s %8.1f %8.1f %6.1f%%  %s%s',
                  v, d$filer_records[1] / 1e6, d$nonfiler_records[1] / 1e6,
                  100 * d$nonfiler_share[1],
                  paste(sprintf('%+7.2f%%', 100 * d$factor_bias), collapse = ''),
                  if (d$dina_already_in_mask[1]) '   (channel already open)' else '   (NEW channel)'))
}

message(paste('\n  Positive bias = the extensive factor is too large = filer',
              'values grow too SLOWLY.'))
message(paste('  interest and dividends are NEW: the file being replaced carries',
              'exactly 0.0% receipt'))
message(paste('  on both, so it contributes nothing to those masks. wages and',
              'pensions it does carry,'))
message(paste('  which means E2 does not hold TODAY either -- the swap changes',
              'the size of an open'))
message('  channel rather than opening one.')
message(sprintf('\n  wrote extensive_factor_contamination.csv'))

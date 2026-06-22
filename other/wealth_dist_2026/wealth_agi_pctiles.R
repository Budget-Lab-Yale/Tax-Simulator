#-------------------------------------------------------------------------------
# Weighted percentiles of net worth and AGI by 5-year age bin, 2026 input data.
#
# Net worth  = sum(ESTATE_ASSET_COLS) - sum(ESTATE_DEBT_COLS)   [from input CSV,
#              the same economic-wealth concept the estate model bridges from]
# AGI        = computed `agi` column from a baseline static detail run, joined to
#              the input records by `id` (raw input has no precomputed AGI)
#
# Population-weighted (column `weight`), binned by primary-taxpayer age `age1`.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
})

INPUT_CSV  <- '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2026.csv'
DETAIL_CSV <- '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/niskanen_housing_dr_resid_v3/baseline/static/detail/2026.csv'
OUT_DIR    <- '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dist_2026'

ASSET_COLS <- c('value.cash','value.equities','value.bonds','value.dc','value.db',
                'value.life_ins','value.annuities','value.trusts','value.other_fin',
                'value.pass_throughs','value.primary_home','value.other_home',
                'value.re_fund','value.other_nonfin')
DEBT_COLS  <- c('value.primary_mortgage','value.other_mortgage','value.credit_lines',
                'value.credit_cards','value.installment_debt','value.other_debt')

PCTS <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
PCT_NAMES <- c('p5','p10','p25','p50','p75','p90','p95','p99')

# ---- weighted quantile (linear interpolation on cumulative weight) -----------
wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w)
  # cumulative weight at the *midpoint* of each record's weight mass
  p  <- (cw - 0.5 * w) / sum(w)
  approx(p, x, xout = probs, rule = 2, ties = 'ordered')$y
}

# ---- load --------------------------------------------------------------------
cat('Reading input microdata...\n'); flush.console()
dt <- fread(INPUT_CSV, select = c('id','weight','age1','filing_status','filer',
                                   ASSET_COLS, DEBT_COLS))

dt[, assets   := rowSums(.SD), .SDcols = ASSET_COLS]
dt[, debts    := rowSums(.SD), .SDcols = DEBT_COLS]
dt[, networth := assets - debts]

cat('Reading baseline detail for AGI...\n'); flush.console()
det <- fread(DETAIL_CSV, select = c('id','agi'))

n_in <- nrow(dt)
dt <- merge(dt, det, by = 'id', all.x = TRUE)
match_rate <- mean(!is.na(dt$agi))
cat(sprintf('AGI join match rate: %.4f (%d of %d input records)\n',
            match_rate, sum(!is.na(dt$agi)), n_in)); flush.console()

# ---- age bins (5-year) -------------------------------------------------------
# NOTE: age1 is top-coded at 80 in Tax-Data. The only records with age1 > 80 are
# ~100 unweighted Forbes/donor-clone billionaires (weight ~1). So the top bin is
# "80+": collapsing avoids a pure-clone bin while leaving populous-bin percentiles
# (each represents 13-25M people) unaffected by the weight-1 clones.
brks <- c(-Inf, seq(25, 80, 5), Inf)
labs <- c('<25','25-29','30-34','35-39','40-44','45-49','50-54','55-59',
          '60-64','65-69','70-74','75-79','80+')
dt[, age_bin := cut(age1, breaks = brks, labels = labs, right = FALSE)]

# ---- marital split: MFJ (filing_status == 2) vs everyone else ----------------
dt[, marital := fifelse(filing_status == 2, 'MFJ', 'Non-MFJ')]

# ---- percentile tables (marital group x age bin) -----------------------------
build_table <- function(d, var) {
  d <- d[is.finite(get(var))]
  bins <- c(labs, 'All')
  rows <- list()
  for (mg in c('MFJ', 'Non-MFJ')) {
    dm <- d[marital == mg]
    for (b in bins) {
      sub <- if (b == 'All') dm else dm[age_bin == b]
      qs  <- wtd_quantile(sub[[var]], sub$weight, PCTS)
      out <- as.list(round(qs)); names(out) <- PCT_NAMES
      rows[[length(rows) + 1]] <- c(
        list(marital = mg, age_bin = b, n_records = nrow(sub),
             wtd_pop_mil = round(sum(sub$weight) / 1e6, 3),
             wtd_mean = if (nrow(sub) > 0) round(sum(sub[[var]] * sub$weight) / sum(sub$weight)) else NA_real_),
        out)
    }
  }
  rbindlist(rows)
}

nw_tab  <- build_table(dt, 'networth')
agi_tab <- build_table(dt[!is.na(agi)], 'agi')

fwrite(nw_tab,  file.path(OUT_DIR, 'networth_pctiles_by_age_marital_2026.csv'))
fwrite(agi_tab, file.path(OUT_DIR, 'agi_pctiles_by_age_marital_2026.csv'))

show <- function(tab, title) {
  cat('\n================ ', title, ' ================\n', sep = '')
  for (mg in c('MFJ', 'Non-MFJ')) {
    cat('\n----', mg, '----\n')
    print(tab[marital == mg, !'marital'], nrows = 100)
  }
}
show(nw_tab,  'NET WORTH (assets - debts), 2026, $')
show(agi_tab, 'AGI (baseline computed), 2026, $')

cat('\nWrote:\n  ', file.path(OUT_DIR, 'networth_pctiles_by_age_marital_2026.csv'),
    '\n  ', file.path(OUT_DIR, 'agi_pctiles_by_age_marital_2026.csv'), '\n', sep = '')

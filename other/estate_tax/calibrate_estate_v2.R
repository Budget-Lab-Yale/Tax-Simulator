#-------------------------------------------------------------------------------
# calibrate_estate_v2.R
#
# Calibration of the valuation parameters (r, rho_pt) on the LOCKED-SPEC module
# (estate_module.R), with the cluster cap and the gift add-back ON. Successor to
# the fitting layer of calibrate_estate_tax.R: the forward-receipts scaffolding
# (projected Tax-Data years, chained-CPI exemption paths, receipts = death year
# + 1, CBO/JCT cumulative targets with deadband) carries over; the per-record
# tax spec is the new module's (record-level, explicit debts, f_ded, DSUE
# two-calc blend, pooled gamma), NOT the old cell-collapsed t(gross) spec.
#
# Free parameters (the ONLY fitted quantities):
#   r      : scalar reporting/valuation factor on economic gross
#   rho_pt : pass-through-specific valuation discount
#   reported gross = economic_gross * r * [1 + (rho_pt - 1) * s_pt]
#
# Objective (v2, after the 14569716 corner-solution failure): SOI-anchored with
# a front-window CBO check. The first run showed a flat (r, rho_pt) cannot
# reconcile historical SOI dollars (want r~1), CBO's 10-yr LEVEL PATH (grows
# ~3%/yr vs the model's ~5%/yr — a slope disagreement, not a valuation one),
# and the JCT delta; the 10-yr CBO cumulative dragged rho_pt to an absurd 0.30
# corner and gutted the observed top tail. v2 therefore:
#   - SHAPE (primary): within-year bin shares of taxable counts and net tax
#     across the modelable bins (bottom bin excluded), death years 2018-2022
#     via the FRED net-worth deflator machinery; mean squared share deviation.
#   - SOI LEVEL: the post-TCJA 5-yr aggregate net tax (modelable bins) vs SOI —
#     the OBSERVED level anchor; deadband.
#   - JCT delta FY2026-2034 cumulative (x0.9): the reform-scoring anchor;
#     deadband.
#   - CBO receipts restricted to FY2026-2028 ONLY (x0.9): a level pin in the
#     window before the growth-path disagreement accumulates; deadband. The
#     full 10-yr CBO comparison is REPORTED but not targeted.
#   - rho_pt bounded to economically plausible discounts (>= 0.5).
#   - Pre-TCJA death years 2015-2017 are HELD OUT of the objective and reported
#     as out-of-sample validation.
#
# Fixed inputs (never fitted): f_ded / p_dsue / f_dsue / gamma from SOI;
# absolute cluster death-weight cap = 300; FRED deflators; exemption paths.
#
# Usage: Rscript calibrate_estate_v2.R  (paths configured below)
#-------------------------------------------------------------------------------

source(file.path(dirname(sub('--file=', '', grep('--file=', commandArgs(), value = TRUE))),
                 'estate_module.R'))

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------

TAX_DATA_ROOT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline'
MACRO_ROOT    = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'
SCRIPT_DIR    = dirname(sub('--file=', '', grep('--file=', commandArgs(), value = TRUE)))
SOI_PATH      = file.path(SCRIPT_DIR, 'estate_tax_filed_2016_2023.csv')
SCORE_PATH    = file.path(SCRIPT_DIR, 'score_targets_estate_gift.csv')

CLUSTER_CAP = 300

# FRED TNWBSHNO annual averages (only ratios used); BEA by death year
NW = c('2015' = 89147806,  '2016' = 93433605,  '2017' = 100706809,
       '2018' = 105951254, '2019' = 112549872, '2020' = 120073861,
       '2021' = 144807269, '2022' = 146284336)
BEA_HIST = c('2015' = 5.43e6,  '2016' = 5.45e6,  '2017' = 5.49e6,
             '2018' = 11.18e6, '2019' = 11.40e6, '2020' = 11.58e6,
             '2021' = 11.70e6, '2022' = 12.06e6, '2023' = 12.92e6,
             '2024' = 13.61e6, '2025' = 13.99e6)
WEALTH_BASE_YEAR = 2022

POLICY_BASE_YEAR = 2026   # exemption paths indexed from here (chained CPI)
OBBBA_BEA_2026   = 15e6
SUNSET_BEA_2026  = 7.2e6

GIFT_TAX_HAIRCUT = 0.10   # estate share of CBO/JCT estate+gift targets
LEVEL_TOL    = 0.03
LEVEL_LAMBDA = 200

SHAPE_YEARS    = 2018:2022   # in the objective
HOLDOUT_YEARS  = 2015:2017   # pre-TCJA, validation only
FWD_DEATH_YEARS = 2025:2034  # receipts 2026-2035
CBO_TARGET_YEARS = 2026:2028 # CBO level pin: front window only (see header)
CBO_REPORT_YEARS = 2026:2035 # full path reported, not targeted
JCT_RECEIPT_YEARS = 2026:2034


#-------------------------------------------------------------------------------
# Policy index and exemption paths
#-------------------------------------------------------------------------------

load_policy_index = function(macro_root, base_year) {
  macro = map_dfr(file.path(macro_root, c('historical.csv', 'projections.csv')),
                  ~ fread(.x, showProgress = FALSE) %>% as_tibble())
  index_col = if ('ccpiu_irs' %in% names(macro)) 'ccpiu_irs' else 'cpiu'
  out = macro %>%
    distinct(year, .keep_all = TRUE) %>%
    transmute(year = as.integer(year),
              index_value = as.numeric(.data[[index_col]]))
  base = out$index_value[out$year == base_year]
  out %>% mutate(policy_index = index_value / base)
}

bea_death_year = function(t, policy, policy_index) {
  hist = BEA_HIST[as.character(t)]
  if (!is.na(hist)) return(as.numeric(hist))
  idx = policy_index$policy_index[policy_index$year == t]
  base = switch(policy, obbba = OBBBA_BEA_2026, sunset = SUNSET_BEA_2026,
                stop('Unknown policy: ', policy))
  base * idx
}


#-------------------------------------------------------------------------------
# Load inputs
#-------------------------------------------------------------------------------

cat('Loading inputs...\n')
policy_index = load_policy_index(MACRO_ROOT, POLICY_BASE_YEAR)
soi = load_soi_estate_table(SOI_PATH)

score = fread(SCORE_PATH, showProgress = FALSE) %>%
  as_tibble() %>%
  mutate(value_millions = if_else(tolower(units) %in% c('billion', 'billions'),
                                  value * 1000, value) * (1 - GIFT_TAX_HAIRCUT))
cbo_target_3yr = score %>%
  filter(scenario == 'baseline', year %in% CBO_TARGET_YEARS) %>%
  pull(value_millions) %>% sum()
cbo_target_full = score %>%
  filter(scenario == 'baseline', year %in% CBO_REPORT_YEARS) %>%
  pull(value_millions) %>% sum()
jct_target_cum = score %>%
  filter(scenario == 'obbba_vs_sunset', year %in% JCT_RECEIPT_YEARS) %>%
  pull(value_millions) %>% sum()

# SOI-derived inputs per historical death year; latest year's table reused for
# forward years (with its own exemption-pooled gamma)
hist_years = c(HOLDOUT_YEARS, SHAPE_YEARS)
soi_in = map(set_names(hist_years), ~ soi_inputs(soi, .x, exemption = BEA_HIST[as.character(.x)]))
soi_in_fwd = soi_in[[as.character(WEALTH_BASE_YEAR)]]

# Snapshot records (shape/validation) and forward records (levels), all capped
snapshot = load_estate_records(file.path(TAX_DATA_ROOT, paste0('tax_units_', WEALTH_BASE_YEAR, '.csv'))) %>%
  apply_cluster_abscap_mortality(cap = CLUSTER_CAP, verbose = TRUE)
forward = map(set_names(FWD_DEATH_YEARS), function(t) {
  load_estate_records(file.path(TAX_DATA_ROOT, paste0('tax_units_', t, '.csv'))) %>%
    apply_cluster_abscap_mortality(cap = CLUSTER_CAP, verbose = FALSE)
})
cat(sprintf('Loaded snapshot (%s records) + %d forward death years\n',
            format(nrow(snapshot), big.mark = ','), length(forward)))


#-------------------------------------------------------------------------------
# Objective components
#-------------------------------------------------------------------------------

soi_block = function(valuation) {
  # Returns the shape MSE and the multi-year observed level error together
  # (one pass over the shape years)
  per_year = map(SHAPE_YEARS, function(Y) {
    soi_y = soi_in[[as.character(Y)]]
    out = compute_estate_liability(
      snapshot, exemption = BEA_HIST[as.character(Y)], soi_in = soi_y,
      valuation = valuation, gift_addback = TRUE,
      wealth_scale = NW[as.character(Y)] / NW[as.character(WEALTH_BASE_YEAR)],
      count_mode = 'nodsue'
    )
    smry = summarize_estate_bins(out, soi_y) %>%
      left_join(soi_y %>% select(size_bin, lo), by = 'size_bin') %>%
      filter(lo > 0)   # bottom bin excluded (unmodelable gift-driven payers)
    share_dev = function(m, s) {
      if (sum(m) <= 0 || sum(s) <= 0) return(rep(1, length(m)))
      m / sum(m) - s / sum(s)
    }
    list(
      shape = mean(c(share_dev(smry$model_count, smry$target_count)^2,
                     share_dev(smry$model_tax, smry$target_tax)^2)),
      model_tax = sum(smry$model_tax),
      soi_tax   = sum(smry$target_tax)
    )
  })
  list(
    shape = mean(map_dbl(per_year, 'shape')),
    level_err = sum(map_dbl(per_year, 'model_tax')) /
      sum(map_dbl(per_year, 'soi_tax')) - 1
  )
}

forward_receipts = function(valuation, policy) {
  map_dbl(FWD_DEATH_YEARS, function(t) {
    out = compute_estate_liability(
      forward[[as.character(t)]],
      exemption = bea_death_year(t, policy, policy_index),
      soi_in = soi_in_fwd, valuation = valuation, gift_addback = TRUE,
      wealth_scale = 1, count_mode = 'nodsue'
    )
    sum(out$exp_tax) / 1e6   # $M, booked at receipt year t + 1
  })
}

deadband_penalty = function(rel_err) {
  LEVEL_LAMBDA * pmax(0, abs(rel_err) - LEVEL_TOL)^2
}

evaluate = function(par, detail = FALSE) {
  valuation = list(r = par[1], rho_pt = par[2])
  soi_fit = soi_block(valuation)
  rec_obbba  = forward_receipts(valuation, 'obbba')
  rec_sunset = forward_receipts(valuation, 'sunset')
  cbo3_idx = (FWD_DEATH_YEARS + 1) %in% CBO_TARGET_YEARS
  cbo3_err = sum(rec_obbba[cbo3_idx]) / cbo_target_3yr - 1
  cbo_full_err = sum(rec_obbba) / cbo_target_full - 1
  jct_idx = FWD_DEATH_YEARS + 1 <= max(JCT_RECEIPT_YEARS)
  jct_err = sum(rec_obbba[jct_idx] - rec_sunset[jct_idx]) / jct_target_cum - 1
  value = soi_fit$shape +
    deadband_penalty(soi_fit$level_err) +
    deadband_penalty(jct_err) +
    deadband_penalty(cbo3_err)
  if (detail) {
    return(list(value = value, shape = soi_fit$shape,
                soi_level_err = soi_fit$level_err, cbo3_err = cbo3_err,
                cbo_full_err = cbo_full_err, jct_err = jct_err,
                rec_obbba = rec_obbba, rec_sunset = rec_sunset))
  }
  value
}


#-------------------------------------------------------------------------------
# Fit: coarse grid then L-BFGS-B refinement
#-------------------------------------------------------------------------------

cat('\n--- Uncalibrated reference (r = 1, rho_pt = 1) ---\n')
ref = evaluate(c(1, 1), detail = TRUE)
cat(sprintf(paste0('shape %.5f | SOI level %+.1f%% | CBO FY26-28 %+.1f%% | ',
                   'CBO FY26-35 %+.1f%% (reported) | JCT delta %+.1f%%\n'),
            ref$shape, 100 * ref$soi_level_err, 100 * ref$cbo3_err,
            100 * ref$cbo_full_err, 100 * ref$jct_err))

cat('\nGrid search...\n')
grid = expand_grid(r = seq(0.80, 1.10, 0.05), rho = seq(0.50, 1.10, 0.10))
grid$value = pmap_dbl(grid, function(r, rho) evaluate(c(r, rho)))
best = grid %>% slice_min(value, n = 1)
cat(sprintf('Grid best: r = %.2f, rho_pt = %.2f (obj %.5f)\n',
            best$r, best$rho, best$value))
cat('Top 5 grid points:\n')
print(as.data.frame(grid %>% arrange(value) %>% head(5)), row.names = FALSE)

fit = optim(c(best$r, best$rho), evaluate, method = 'L-BFGS-B',
            lower = c(0.70, 0.50), upper = c(1.20, 1.15),
            control = list(maxit = 100))
r_hat = fit$par[1]; rho_hat = fit$par[2]
final = evaluate(fit$par, detail = TRUE)

cat(sprintf('\n=== FITTED: r = %.3f, rho_pt = %.3f ===\n', r_hat, rho_hat))
cat(sprintf(paste0('objective %.5f = shape %.5f + SOI-level pen %.5f + ',
                   'JCT pen %.5f + CBO-3yr pen %.5f\n'),
            final$value, final$shape, deadband_penalty(final$soi_level_err),
            deadband_penalty(final$jct_err), deadband_penalty(final$cbo3_err)))
cat(sprintf('SOI post-TCJA 5-yr tax level: %+.1f%%\n', 100 * final$soi_level_err))
cat(sprintf('CBO FY2026-28 (targeted): model $%.1fB vs $%.1fB (%+.1f%%)\n',
            sum(final$rec_obbba[(FWD_DEATH_YEARS + 1) %in% CBO_TARGET_YEARS]) / 1e3,
            cbo_target_3yr / 1e3, 100 * final$cbo3_err))
cat(sprintf('CBO FY2026-35 (reported, NOT targeted): model $%.1fB vs $%.1fB (%+.1f%%)\n',
            sum(final$rec_obbba) / 1e3, cbo_target_full / 1e3,
            100 * final$cbo_full_err))
jct_idx = FWD_DEATH_YEARS + 1 <= max(JCT_RECEIPT_YEARS)
cat(sprintf('JCT delta cumulative FY2026-34: model $%.1fB vs target $%.1fB (%+.1f%%)\n',
            sum(final$rec_obbba[jct_idx] - final$rec_sunset[jct_idx]) / 1e3,
            jct_target_cum / 1e3, 100 * final$jct_err))

cat('\nReceipts by fiscal year ($B, estate share = 0.9 x CBO estate+gift):\n')
cat(sprintf('%6s %12s %12s %12s %14s\n', 'FY', 'model obbba', 'CBO target',
            'model sunset', 'delta vs JCT'))
cbo_by_year = score %>% filter(scenario == 'baseline')
jct_by_year = score %>% filter(scenario == 'obbba_vs_sunset')
for (i in seq_along(FWD_DEATH_YEARS)) {
  fy = FWD_DEATH_YEARS[i] + 1
  cbo_t = cbo_by_year$value_millions[cbo_by_year$year == fy]
  jct_t = jct_by_year$value_millions[jct_by_year$year == fy]
  cat(sprintf('%6d %12.1f %12.1f %12.1f %7.1f vs %5.1f\n',
              fy, final$rec_obbba[i] / 1e3,
              if (length(cbo_t)) cbo_t / 1e3 else NA,
              final$rec_sunset[i] / 1e3,
              (final$rec_obbba[i] - final$rec_sunset[i]) / 1e3,
              if (length(jct_t)) jct_t / 1e3 else NA))
}


#-------------------------------------------------------------------------------
# Era tables at fitted parameters (incl. held-out pre-TCJA validation)
#-------------------------------------------------------------------------------

era_table = function(years, label) {
  n = length(years)
  acc = map_dfr(years, function(Y) {
    soi_y = soi_in[[as.character(Y)]]
    out = compute_estate_liability(
      snapshot, exemption = BEA_HIST[as.character(Y)], soi_in = soi_y,
      valuation = list(r = r_hat, rho_pt = rho_hat), gift_addback = TRUE,
      wealth_scale = NW[as.character(Y)] / NW[as.character(WEALTH_BASE_YEAR)],
      count_mode = 'nodsue'
    )
    summarize_estate_bins(out, soi_y) %>% mutate(death_year = Y)
  })
  cat(sprintf('\n%s (%d-yr avg, fitted r/rho_pt, cap + gifts ON):\n', label, n))
  cat(sprintf('%10s | %8s %8s %8s | %9s %9s %8s\n', 'bin', 'mdl_cnt',
              'soi_cnt', 'cnt_err', 'mdl_tax$B', 'soi_tax$B', 'tax_err'))
  cat(strrep('-', 72), '\n')
  by_bin = acc %>%
    group_by(size_bin) %>%
    summarise(mc = sum(model_count) / n, sc = sum(target_count) / n,
              mt = sum(model_tax) / n / 1e9, st = sum(target_tax) / n / 1e9,
              lo = min(map_dbl(size_bin, ~ ESTATE_BIN_BOUNDS[[.x]][1])),
              .groups = 'drop') %>%
    arrange(lo)
  for (i in seq_len(nrow(by_bin))) {
    b = by_bin[i, ]
    cat(sprintf('%10s | %8.0f %8.0f %+7.0f%% | %9.1f %9.1f %+7.0f%%\n',
                b$size_bin, b$mc, b$sc,
                if (b$sc > 0) 100 * (b$mc / b$sc - 1) else 0, b$mt, b$st,
                if (b$st > 0) 100 * (b$mt / b$st - 1) else 0))
  }
  for (Y in years) {
    yr = acc %>% filter(death_year == Y)
    cat(sprintf('  %d totals: cnt %0.0f vs %0.0f (%+.0f%%) | tax $%.1fB vs $%.1fB (%+.0f%%)\n',
                Y, sum(yr$model_count), sum(yr$target_count),
                100 * (sum(yr$model_count) / sum(yr$target_count) - 1),
                sum(yr$model_tax) / 1e9, sum(yr$target_tax) / 1e9,
                100 * (sum(yr$model_tax) / sum(yr$target_tax) - 1)))
  }
}

era_table(HOLDOUT_YEARS, 'PRE-TCJA HELD-OUT VALIDATION (death years 2015-2017)')
era_table(SHAPE_YEARS,   'POST-TCJA (death years 2018-2022, in objective)')

cat('\nDone.\n')

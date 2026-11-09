#-------------------------------------------------------------------------------
# estate_diagnostic.R
#
# Multi-year uncalibrated shape diagnostic on the locked-spec R module --
# the R port of first_order_shape_multiyear.py, extended with two toggles:
#
#   mortality : raw (Tax-Data q_death) vs smooth (clone-robust m_hat(wealth),
#               thoughts doc §10b)
#   gifts     : unified-base gift add-back gamma(bin) on/off
#
# The model has ONE wealth year (the Tax-Data file passed in, death year 2022).
# For each SOI death year Y we deflate that wealth by the FRED household
# net-worth ratio NW_Y / NW_2022, apply Y's actual exemption, estimate
# f_ded / p_dsue / f_dsue / gamma from Y's SOI (filing year Y+1), and compare
# modeled taxable counts and tax to Y's SOI. Reporting factor = 1 throughout.
#
# Death years run automatically over every year present in BOTH the SOI table
# and the NW/BEA tables below -- extending estate_tax_filed_*.csv back to
# pre-TCJA filing years (2014-2018) makes this the §10b raw-vs-smoothed count
# experiment with no code changes.
#
# Usage:
#   Rscript estate_diagnostic.R <tax_units_2022.csv> [soi_csv]
#-------------------------------------------------------------------------------

source(file.path(dirname(sub('--file=', '', grep('--file=', commandArgs(), value = TRUE))),
                 'estate_module.R'))

args = commandArgs(trailingOnly = TRUE)
tax_units_path = if (length(args) >= 1) args[1] else
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2022.csv'
soi_path = if (length(args) >= 2) args[2] else
  file.path(dirname(sub('--file=', '', grep('--file=', commandArgs(), value = TRUE))),
            'estate_tax_filed_2016_2023.csv')

# FRED TNWBSHNO (households & nonprofits net worth, $M), annual average of
# quarterly values, pulled 2026-06-10; 2018-2022 match the python diagnostic's
# hardcoded values exactly. Indexed by DEATH year; only ratios are used.
NW = c(
  '2012' = 70278339,  '2013' = 77611366,  '2014' = 84928638,
  '2015' = 89147806,  '2016' = 93433605,  '2017' = 100706809,
  '2018' = 105951254, '2019' = 112549872, '2020' = 120073861,
  '2021' = 144807269, '2022' = 146284336
)

# Basic exclusion amount by DEATH year
BEA = c(
  '2013' = 5.25e6,  '2014' = 5.34e6,  '2015' = 5.43e6,
  '2016' = 5.45e6,  '2017' = 5.49e6,  '2018' = 11.18e6,
  '2019' = 11.40e6, '2020' = 11.58e6, '2021' = 11.70e6,
  '2022' = 12.06e6
)

WEALTH_BASE_YEAR = 2022


#-------------------------------------------------------------------------------
# Load inputs once
#-------------------------------------------------------------------------------

records = load_estate_records(tax_units_path)
cat(sprintf('Loaded %s Tax-Data %d records (gross assets > 0)\n\n',
            format(nrow(records), big.mark = ','), WEALTH_BASE_YEAR))

soi = load_soi_estate_table(soi_path)

death_years = sort(intersect(
  intersect(as.integer(names(NW)), as.integer(names(BEA))),
  unique(soi$year[soi$tax_status == 'taxable']) - 1
))
cat('Death years in scope:', paste(death_years, collapse = ', '), '\n')

mort_fit = fit_smooth_mortality(records)
cat('\nSmoothed-mortality death-weight preservation (gross > $1M):\n')
print(as.data.frame(mort_fit$diagnostics), row.names = FALSE)
records_smooth = apply_smooth_mortality(records, mort_fit)


#-------------------------------------------------------------------------------
# Diagnostic
#-------------------------------------------------------------------------------

run_variant = function(recs, gift_addback, label) {
  cat('\n', strrep('=', 88), '\n', sep = '')
  cat('VARIANT:', label, ' (r = 1, uncalibrated)\n')
  cat(strrep('=', 88), '\n')
  cat(sprintf('%8s %8s %9s | %8s %8s %8s | %9s %9s %8s\n',
              'death_yr', 'NW_ratio', 'exempt$M', 'mdl_cnt', 'soi_cnt',
              'cnt_err', 'mdl_tax$B', 'soi_tax$B', 'tax_err'))
  cat(strrep('-', 88), '\n')

  acc = list()
  tot = c(mc = 0, sc = 0, mt = 0, st = 0)
  for (Y in death_years) {
    soi_y = soi_inputs(soi, Y, exemption = BEA[as.character(Y)])
    s = NW[as.character(Y)] / NW[as.character(WEALTH_BASE_YEAR)]
    out = compute_estate_liability(
      recs, exemption = BEA[as.character(Y)], soi_in = soi_y,
      gift_addback = gift_addback, wealth_scale = s,
      count_mode = 'nodsue'  # python diagnostic parity
    )
    smry = summarize_estate_bins(out, soi_y) %>% mutate(death_year = Y)
    acc[[as.character(Y)]] = smry

    mc = sum(smry$model_count); sc = sum(smry$target_count)
    mt = sum(smry$model_tax) / 1e9; st = sum(smry$target_tax) / 1e9
    tot = tot + c(mc = mc, sc = sc, mt = mt, st = st)
    cat(sprintf('%8d %8.3f %9.2f | %8.0f %8.0f %+7.0f%% | %9.1f %9.1f %+7.0f%%\n',
                Y, s, BEA[as.character(Y)] / 1e6, mc, sc, 100 * (mc / sc - 1),
                mt, st, 100 * (mt / st - 1)))
  }
  n = length(death_years)
  cat(strrep('-', 88), '\n')
  cat(sprintf('%8s %8s %9s | %8.0f %8.0f %+7.0f%% | %9.1f %9.1f %+7.0f%%\n',
              'AVG', '', '', tot['mc'] / n, tot['sc'] / n,
              100 * (tot['mc'] / tot['sc'] - 1),
              tot['mt'] / n, tot['st'] / n,
              100 * (tot['mt'] / tot['st'] - 1)))

  # Averaged bin panels split by exemption era: averaging counts across the
  # pre-TCJA ($5.4M) and post-TCJA ($11M+) regimes would blend two different
  # taxable universes and hide exactly the low-exemption signal the pre-TCJA
  # extension exists to expose
  all_years = bind_rows(acc)
  for (era in list(list(label = 'pre-TCJA (death years < 2018)',
                        years = death_years[death_years < 2018]),
                   list(label = 'post-TCJA (death years >= 2018)',
                        years = death_years[death_years >= 2018]))) {
    if (length(era$years) == 0) next
    n_era = length(era$years)
    cat(sprintf('\n%d-year-averaged shape by bin, %s:\n', n_era, era$label))
    cat(sprintf('%10s | %8s %8s %8s | %9s %9s %8s\n',
                'bin', 'mdl_cnt', 'soi_cnt', 'cnt_err', 'mdl_tax$B',
                'soi_tax$B', 'tax_err'))
    cat(strrep('-', 72), '\n')
    by_bin = all_years %>%
      filter(death_year %in% era$years) %>%
      group_by(size_bin) %>%
      summarise(
        mc = sum(model_count) / n_era, sc = sum(target_count) / n_era,
        mt = sum(model_tax) / n_era / 1e9, st = sum(target_tax) / n_era / 1e9,
        lo = min(map_dbl(size_bin, ~ ESTATE_BIN_BOUNDS[[.x]][1])),
        .groups = 'drop'
      ) %>%
      arrange(lo)
    for (i in seq_len(nrow(by_bin))) {
      b = by_bin[i, ]
      cat(sprintf('%10s | %8.0f %8.0f %+7.0f%% | %9.1f %9.1f %+7.0f%%\n',
                  b$size_bin, b$mc, b$sc,
                  if (b$sc > 0) 100 * (b$mc / b$sc - 1) else 0,
                  b$mt, b$st,
                  if (b$st > 0) 100 * (b$mt / b$st - 1) else 0))
    }
  }
  invisible(all_years)
}

cat('\n')
records_abs300 = apply_cluster_abscap_mortality(records, cap = 300)
records_abs600 = apply_cluster_abscap_mortality(records, cap = 600)

run_variant(records,        gift_addback = FALSE, 'mortality = RAW,        gifts = OFF  [python parity check]')
run_variant(records_abs300, gift_addback = FALSE, 'mortality = ABS-CAP 300, gifts = OFF')
run_variant(records_abs600, gift_addback = FALSE, 'mortality = ABS-CAP 600, gifts = OFF')
run_variant(records_abs300, gift_addback = TRUE,  'mortality = ABS-CAP 300, gifts = ON')

cat('\nDone.\n')

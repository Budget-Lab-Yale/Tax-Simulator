#------------------------------------------------------------------------------
# research/state_tax/aggregate/02_gap_report.R
#
# Compares modeled state income tax with published benchmarks (plan step 1c):
#
#   A. state x year: modeled liability against Census state tax collections
#      (Annual Survey of State Government Tax Collections, item T40, individual
#      income tax), tax year t against the average of state fiscal years t and
#      t+1. Collections include nonresidents, PTET effects and timing, so this
#      locates gaps; it does not size them precisely.
#   B. state x state-AGI class x measure: modeled resident totals against the
#      agency tax-year tables (NY DTF, CA FTB B-4A, CT DRS), on the coarsest
#      class edges both sides share.
#
# Inputs:
#   - {production}/model_data/Tax-Simulator/v1/{ts_vintage}/baseline/static/
#     totals/state.csv
#   - research/state_tax/aggregate/results/gap_slices_{ts_vintage}.csv.gz
#     (01_gap_slices.R)
#   - {production}/raw_data/State-Ind/aligned/state_pit_benchmarks.csv
#     (State-Ind repo, build_benchmarks.R)
#
# Usage (repo root):
#   Rscript research/state_tax/aggregate/02_gap_report.R <ts_vintage>
#
# Outputs (research/state_tax/aggregate/results/):
#   census_ratio_{ts_vintage}.csv, census_ratio_{ts_vintage}.png,
#   agency_ratio_{ts_vintage}.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
  library(ggplot2)
})

OUT_DIR = 'research/state_tax/aggregate/results'

source('research/state_tax/aggregate/gap_bins.R')

# Agency tables, the resident population each reports, and the model
# liability that matches its liability concept: NY's tax_liability is net of
# refundable credits (it goes negative in the bottom classes); CA's and CT's
# are before them (refundable credits are booked as payments), so the model
# adds them back: liab_pre_ref = liab_st_iit + st_credits_ref
AGENCY_SOURCES = data.table(
  source          = c('ny_dtf_rt8x', 'ca_ftb_b4a', 'ct_drs_9mb8'),
  residency       = c('full_year_resident', 'resident', 'resident'),
  model_liability = c('liab_st_iit', 'liab_pre_ref', 'liab_pre_ref')
)

# Model measure <-> benchmark measure ('liability' resolved per source)
MEASURE_MAP = data.table(
  model_measure = c('returns', 'st_agi', 'st_txbl_inc', 'st_tax_pre_credit',
                    'liability'),
  measure       = c('returns', 'state_agi', 'taxable_income',
                    'tax_before_credits', 'liability')
)

# Years in which a PTET credit can sit inside a source's liability (TY2021+)
PTET_FIRST_YEAR = 2021

# Diverging fill around a ratio of 1 (dataviz reference palette: blue/red
# poles, gray midpoint)
FILL_LOW  = '#2a78d6'
FILL_MID  = '#f0efec'
FILL_HIGH = '#e34948'
RATIO_LIMITS = c(0.6, 1.4)

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop('usage: 02_gap_report.R <ts_vintage>')
ts_vintage = args[1]

production = read_yaml('config/interfaces/output_roots.yaml')$production
ts_root    = file.path(production, 'model_data/Tax-Simulator/v1', ts_vintage,
                       'baseline/static')
bench_path = file.path(production, 'raw_data/State-Ind/aligned',
                       'state_pit_benchmarks.csv')
slice_path = file.path(OUT_DIR, paste0('gap_slices_', ts_vintage, '.csv.gz'))
for (p in c(bench_path, slice_path)) if (!file.exists(p)) stop('missing ', p)

bench  = fread(bench_path)
slices = fread(slice_path)
slices = rbind(slices, slices[
  measure %in% c('liab_st_iit', 'st_credits_ref'),
  .(measure = 'liab_pre_ref', value = sum(value)),
  by = setdiff(names(slices), c('measure', 'value'))], use.names = TRUE)


#------------------------------------
# A. State x year against Census T40
#------------------------------------

# Census T40 counts the NH (and, to 2020, TN) interest-and-dividends taxes as
# individual income tax, so the model side adds the narrow-base liability
state_totals = fread(file.path(ts_root, 'totals/state.csv'))
model_st = state_totals[variable %in% c('liab_st_iit', 'liab_st_narrow_iit'),
                        .(model = sum(value)), by = .(year, state)]

census = bench[source == 'census_stc', .(state, fy = year, collections = value)]
census_ty = merge(census[, .(year = fy, state, fy_t = collections)],
                  census[, .(year = fy - 1L, state, fy_t1 = collections)],
                  by = c('year', 'state'))
census_ty[, census_avg := (fy_t + fy_t1) / 2]

# Universe: tax years with both fiscal years published (FRED marks early
# missing observations '.', read as NA)
n_na = census_ty[is.na(census_avg) & year >= min(model_st$year), .N]
if (n_na > 0) stop(n_na, ' Census state-years in the model window are NA')

census_ratio = merge(model_st, census_ty[, .(year, state, census_avg)],
                     by = c('year', 'state'))
us = census_ratio[, .(model = sum(model), state = 'US (sum)'), by = year]
us = merge(us, census_ty[state == 'US', .(year, census_avg)], by = 'year')
census_ratio = rbind(census_ratio, us, use.names = TRUE)
census_ratio[, ratio := model / census_avg]
census_ratio[, gap_bn := (model - census_avg) / 1e9]

n_unmatched = model_st[!state %in% census_ty$state & model != 0, uniqueN(state)]
cat(sprintf('Census comparison: %d states x years; %d modeled states with liability have no T40 series (e.g. DC)\n',
            nrow(census_ratio), n_unmatched))
fwrite(census_ratio[order(state, year)],
       file.path(OUT_DIR, paste0('census_ratio_', ts_vintage, '.csv')))

# Heatmap: states ordered by modeled liability, largest at top
st_order = census_ratio[state != 'US (sum)', .(m = mean(model)), by = state][
  order(m), state]
plot_dt = census_ratio[, .(state = factor(state, c(st_order, 'US (sum)')),
                           year, ratio)]
heat = ggplot(plot_dt, aes(factor(year), state, fill = pmin(pmax(ratio,
                                                RATIO_LIMITS[1]), RATIO_LIMITS[2]))) +
  geom_tile(color = '#fcfcfb', linewidth = 0.5) +
  geom_text(aes(label = sprintf('%.2f', ratio)), size = 2.4, color = '#1a1a19') +
  scale_fill_gradient2(low = FILL_LOW, mid = FILL_MID, high = FILL_HIGH,
                       midpoint = 1, limits = RATIO_LIMITS,
                       name = 'Model / Census') +
  labs(title = 'Modeled state income tax liability / Census T40 collections',
       subtitle = paste0('Tax year t vs mean of state fiscal years t and t+1\n',
                         'Run ', ts_vintage, '; colors clipped at ',
                         RATIO_LIMITS[1], '-', RATIO_LIMITS[2]),
       x = 'Tax year', y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), legend.position = 'top')
ggsave(file.path(OUT_DIR, paste0('census_ratio_', ts_vintage, '.png')), heat,
       width = 7, height = 10, dpi = 150, bg = '#fcfcfb')


#---------------------------------------------
# B. State-AGI class x measure against agencies
#---------------------------------------------

agency_compare = function(src, res, model_liability) {

  #----------------------------------------------------------------------------
  # Puts one agency table and the model slices on shared class edges: the
  # model's state-AGI edges that fall on a published bin boundary in every
  # year of the source. Totals over filing statuses.
  #----------------------------------------------------------------------------

  b = bench[source == src & residency == res & bin_label != 'total' &
            measure %in% MEASURE_MAP$measure]
  st = unique(b$state)
  bins = unique(b[, .(year, agi_lo, agi_hi)])
  interior = ST_AGI_BREAKS[is.finite(ST_AGI_BREAKS)]
  shared = interior[sapply(interior, function(e)
    bins[agi_lo < e & agi_hi > e, .N] == 0)]
  edges = c(-Inf, shared, Inf)

  b[, class_lo := edges[findInterval(agi_lo, edges)]]
  agency = b[, .(agency = sum(value)), by = .(year, class_lo, measure)]

  m = slices[bin_basis == 'st_agi' & dimension == 'all' & state == st &
             measure %in% c(MEASURE_MAP$model_measure, model_liability) &
             measure != 'liability']
  m[measure == model_liability, measure := 'liability']
  m[, model_lo := ST_AGI_BREAKS[match(agi_group, ST_AGI_LABELS)]]
  if (anyNA(m$model_lo)) stop('unknown state-AGI class labels in slices')
  m[, class_lo := edges[findInterval(model_lo, edges)]]
  m = merge(m, MEASURE_MAP, by.x = 'measure', by.y = 'model_measure')
  model = m[, .(model = sum(value)), by = .(year, class_lo, measure = measure.y)]

  out = merge(model, agency, by = c('year', 'class_lo', 'measure'))
  out[, class_hi := edges[match(class_lo, edges) + 1]]
  out[, `:=`(state = st, source = src, residency = res)]
  out[]
}

agency_ratio = rbindlist(lapply(seq_len(nrow(AGENCY_SOURCES)), function(i)
  agency_compare(AGENCY_SOURCES$source[i], AGENCY_SOURCES$residency[i],
                 AGENCY_SOURCES$model_liability[i])))

# Add each table's all-class total
totals = agency_ratio[, .(model = sum(model), agency = sum(agency),
                          class_lo = NA_real_, class_hi = NA_real_),
                      by = .(year, measure, state, source, residency)]
agency_ratio = rbind(agency_ratio, totals, use.names = TRUE)
agency_ratio[, `:=`(ratio = model / agency,
                    ptet_window = year >= PTET_FIRST_YEAR)]
setcolorder(agency_ratio, c('source', 'state', 'residency', 'year', 'measure',
                            'class_lo', 'class_hi', 'model', 'agency', 'ratio',
                            'ptet_window'))
fwrite(agency_ratio[order(source, measure, year, class_lo)],
       file.path(OUT_DIR, paste0('agency_ratio_', ts_vintage, '.csv')))


#----------
# Summaries
#----------

options(width = 200)
cat('\n=== A. Model / Census, worst 15 state-years by |gap| ($bn), TY2017-2023 ===\n')
print(census_ratio[state != 'US (sum)' & year <= 2023][order(-abs(gap_bn))][
  1:15, .(state, year, model_bn = round(model / 1e9, 1),
          census_bn = round(census_avg / 1e9, 1), ratio = round(ratio, 3),
          gap_bn = round(gap_bn, 1))])
cat('\nUS (sum of T40 states):\n')
print(census_ratio[state == 'US (sum)', .(year, ratio = round(ratio, 3),
                                          gap_bn = round(gap_bn, 1))])

cat('\n=== B. Model / agency, all-class totals ===\n')
print(dcast(agency_ratio[is.na(class_lo), .(state, measure, year,
                                             ratio = round(ratio, 3))],
            state + measure ~ year, value.var = 'ratio'))

cat('\n=== B. Liability ratio by state-AGI class (latest common year per state) ===\n')
for (st in unique(agency_ratio$state)) {
  yr = agency_ratio[state == st & measure == 'liability', max(year)]
  cat(sprintf('\n%s, TY%d\n', st, yr))
  print(agency_ratio[state == st & year == yr & !is.na(class_lo) &
                     measure %in% c('returns', 'state_agi', 'liability'),
                     .(class_lo, class_hi, measure, model_bn = round(model / 1e9, 2),
                       agency_bn = round(agency / 1e9, 2), ratio = round(ratio, 3))][
    order(measure, class_lo)])
}
cat('\nGAP_REPORT_SUCCESS\n')

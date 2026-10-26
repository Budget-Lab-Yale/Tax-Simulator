#------------------------------------------------------------------------------
# research/state_tax/aggregate/01_gap_slices.R
#
# Model side of the state gap diagnostics (plan step 1b): weighted state
# totals by state x federal AGI class x one taxpayer characteristic at a time
# (bin_basis = fed_agi), plus state totals by modeled state-AGI class for the
# agency tables (bin_basis = st_agi), from a state-mode run written with
# state_detail = 2.
#
# Joins three per-record sources on id:
#   - static/detail/state/components/{year}.csv.gz  long (id, state) state
#     weight and liability components
#   - static/detail/{year}.csv                       federal AGI, ages, income
#   - Tax-Data tax_units_{year}.csv                  pension distributions
#     (not in the Tax-Simulator detail file)
#
# Usage (repo root; ~20M component rows a year, so run under sbatch):
#   Rscript research/state_tax/aggregate/01_gap_slices.R <ts_vintage> <years>
#   e.g. ... sw_states_components 2017:2025
#
# Output: research/state_tax/aggregate/results/gap_slices_{ts_vintage}.csv,
#   long in (year, state, bin_basis, agi_group, dimension, level, measure,
#   value)
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
})

source('research/state_tax/aggregate/gap_bins.R')

SENIOR_AGE = 65


MEASURES = c('returns', 'st_agi', 'st_txbl_inc', 'st_tax_pre_credit',
             'st_credits_nonref', 'st_credits_ref', 'st_eitc', 'st_ctc',
             'st_cdctc', 'liab_st_iit', 'liab_st_individual_net', 'fed_agi',
             'weight')

OUT_DIR = 'research/state_tax/aggregate/results'

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop('usage: 01_gap_slices.R <ts_vintage> <years, e.g. 2017:2025>')
}
ts_vintage = args[1]
years      = eval(parse(text = args[2]))

production = read_yaml('config/interfaces/output_roots.yaml')$production
ts_root    = file.path(production, 'model_data/Tax-Simulator/v1', ts_vintage,
                       'baseline/static')

# The Tax-Data vintage the run read, from the run's own dependency record
deps = fread(file.path(production, 'model_data/Tax-Simulator/v1', ts_vintage,
                       'dependencies.csv'))
td_dep = deps[ID == 'baseline' & interface == 'Tax-Data']
if (nrow(td_dep) != 1) stop('expected one Tax-Data row in dependencies.csv')
td_root = file.path(production, 'model_data/Tax-Data',
                    paste0('v', td_dep$version), td_dep$vintage,
                    td_dep$scenario)


slice_year = function(yr) {

  comp = fread(file.path(ts_root, 'detail/state/components',
                         paste0(yr, '.csv.gz')))
  fed  = fread(file.path(ts_root, 'detail', paste0(yr, '.csv')),
               select = c('id', 'filer', 'filing_status', 'age1', 'age2',
                          'n_dep', 'wages', 'gross_ss', 'txbl_kg',
                          'sole_prop', 'part_scorp', 'agi', 'itemizing'))
  pens = fread(file.path(td_root, paste0('tax_units_', yr, '.csv')),
               select = c('id', 'gross_pens_dist', 'txbl_ira_dist'))

  rec = merge(fed, pens, by = 'id', all.x = TRUE)
  n_unmatched = rec[is.na(gross_pens_dist), .N]
  if (n_unmatched > 0) {
    stop(yr, ': ', n_unmatched, ' detail records have no Tax-Data row')
  }

  # Every component row must find its federal record
  dt = merge(comp, rec, by = 'id', all.x = TRUE)
  n_orphan = dt[is.na(agi), .N]
  if (n_orphan > 0) stop(yr, ': ', n_orphan, ' component rows lack detail')

  dt[, `:=`(
    agi_group  = cut(agi, AGI_BREAKS, AGI_LABELS, right = FALSE),
    all        = 'all',
    filer      = fifelse(filer == 1, 'filer', 'nonfiler'),
    senior     = fifelse(pmax(age1, fifelse(filing_status == 2, age2, 0)) >=
                           SENIOR_AGE, 'head_or_spouse_65plus', 'under_65'),
    has_ss     = fifelse(gross_ss > 0, 'ss', 'no_ss'),
    has_pens   = fifelse(gross_pens_dist + txbl_ira_dist > 0, 'pension_ira',
                         'no_pension_ira'),
    has_kg     = fifelse(txbl_kg > 0, 'cap_gains', 'no_cap_gains'),
    has_bus    = fifelse(sole_prop != 0 | part_scorp != 0, 'business',
                         'no_business'),
    has_wages  = fifelse(wages > 0, 'wages', 'no_wages'),
    has_dep    = fifelse(n_dep > 0, 'dependents', 'no_dependents'),
    itemizer   = fifelse(itemizing == 1, 'itemizer', 'standard'),
    fstatus    = as.character(filing_status)
  )]

  dt[, st_agi_group := cut(st_agi, ST_AGI_BREAKS, ST_AGI_LABELS, right = FALSE)]

  dims = c('all', 'filer', 'senior', 'has_ss', 'has_pens', 'has_kg',
           'has_bus', 'has_wages', 'has_dep', 'itemizer', 'fstatus')

  # Every characteristic on federal AGI classes, plus the totals on state-AGI
  # classes for the agency tables
  slice_specs = rbind(
    data.table(bin_basis = 'fed_agi', bin_col = 'agi_group', dim = dims),
    data.table(bin_basis = 'st_agi', bin_col = 'st_agi_group', dim = 'all')
  )

  rbindlist(lapply(seq_len(nrow(slice_specs)), function(i) {
    d       = slice_specs$dim[i]
    bin_col = slice_specs$bin_col[i]
    agg = dt[, .(
      returns                = sum(st_weight * st_tax_filer),
      st_agi                 = sum(st_weight * st_agi),
      st_txbl_inc            = sum(st_weight * st_txbl_inc),
      st_tax_pre_credit      = sum(st_weight * st_tax_pre_credit),
      st_credits_nonref      = sum(st_weight * st_credits_nonref),
      st_credits_ref         = sum(st_weight * st_credits_ref),
      st_eitc                = sum(st_weight * st_eitc),
      st_ctc                 = sum(st_weight * st_ctc),
      st_cdctc               = sum(st_weight * st_cdctc),
      liab_st_iit            = sum(st_weight * liab_st_iit),
      liab_st_individual_net = sum(st_weight * liab_st_individual_net),
      fed_agi                = sum(st_weight * agi),
      weight                 = sum(st_weight)
    ), by = c('state', bin_col, d)]
    setnames(agg, c(bin_col, d), c('agi_group', 'level'))
    agg[, `:=`(dimension = d, bin_basis = slice_specs$bin_basis[i])]
    melt(agg, id.vars = c('state', 'bin_basis', 'agi_group', 'dimension',
                          'level'),
         measure.vars = MEASURES, variable.name = 'measure')
  }))[, year := yr]
}


slices = rbindlist(lapply(years, function(yr) {
  t0 = Sys.time()
  out = slice_year(yr)
  cat(sprintf('%d: %d slice rows (%.0fs)\n', yr, nrow(out),
              as.numeric(Sys.time() - t0, units = 'secs')))
  out
}))
setcolorder(slices, c('year', 'state', 'bin_basis', 'agi_group', 'dimension',
                      'level', 'measure', 'value'))

# Check: the 'all' slice reproduces the run's own state.csv totals, on both
# binnings
state_totals = fread(file.path(ts_root, 'totals/state.csv'))
check = merge(
  slices[dimension == 'all' & measure %in% c('liab_st_iit', 'st_agi'),
         .(sliced = sum(value)), by = .(bin_basis, year, state, measure)],
  state_totals[variable %in% c('liab_st_iit', 'st_agi'),
               .(year, state, measure = variable, run = value)],
  by = c('year', 'state', 'measure')
)
check[, rel_diff := fifelse(run == 0, sliced, sliced / run - 1)]
cat(sprintf('state.csv reproduction: max |rel diff| %.2e over %d cells\n',
            max(abs(check$rel_diff)), nrow(check)))
if (max(abs(check$rel_diff)) > 1e-6) {
  print(check[order(-abs(rel_diff))][1:10])
  stop('slices do not reproduce state.csv')
}

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
out_path = file.path(OUT_DIR, paste0('gap_slices_', ts_vintage, '.csv.gz'))
fwrite(slices, out_path, compress = 'gzip')
cat('Wrote', out_path, '\n')
cat('GAP_SLICES_SUCCESS\n')

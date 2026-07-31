#-------------------------------------------------------------------------------
# above_threshold_income.R
#
# Describes the income sitting above each top group's own income threshold, on
# adjusted gross income and on the same measure with realized gains replaced by
# accrued ones, before and after the tax that falls on it
#
# The after-tax column needs to know how much tax sits on the above-threshold
# dollars. It is measured by removing them: each record's income is scaled down
# until the record sits on the threshold, the record is repriced under current
# law, and the fall in its tax is the tax the removed dollars were carrying.
#
# Scaling is proportional across every income flow, so the record keeps its
# composition and the slice below the threshold is taxed the way that taxpayer
# would be taxed at the threshold. Deductions scale with income for the same
# reason. Leaving them fixed would charge the whole of a taxpayer's deductions
# against the below-threshold slice alone, which for a large enough deduction
# zeroes the tax there and attributes the taxpayer's entire bill to the dollars
# above the line. The standard deduction is a statutory amount rather than a
# feature of the record and does not scale; records whose itemized deductions
# shrink below it fall back onto it in the calculator.
#
# Only positive flows scale. Shrinking a loss would raise income rather than
# lower it, which breaks the monotonicity the solver needs.
#
# Run via sbatch (never on the login node):
#   sbatch other/top_tax/report_prep/run_above_threshold_income.sbatch
#-------------------------------------------------------------------------------

REPO    = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
VINTAGE = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/top_tax_dials_30y_v6')
STAGING = file.path(VINTAGE, '_slurm_staging')
OUT_DIR = file.path(REPO, 'other/top_tax/report_prep')

YEAR    = 2027
GDP_B   = 33651.47   # CY2027 nominal GDP, Macro-Projections 2026022522 gdp
                     # Calendar year, since every income measure here is a
                     # calendar-year concept
TOP_SHARES = c(0.01, 0.001, 0.0001)

setwd(REPO)
source('src/slurm/common.R')
invisible(reconstitute_environment(STAGING))

config = readRDS(file.path(STAGING, 'baseline', 'config.rds'))
config_activate(economy  = config$scenario_info$resolved_economy,
                behavior = config$scenario_info$resolved_behavior)
scenario_info = config$scenario_info
tax_law       = config$tax_law

z0 = function(x) replace_na(as.numeric(x), 0)


#-------------------------------------------------------------------------------
# What scales
#-------------------------------------------------------------------------------

# Every income flow, ordinary and preferentially rated alike. Sub-components
# travel with their totals, and gain basis with the gain
INCOME_COLS = c(
  'wages', 'wages1', 'wages2', 'ot', 'ot1', 'ot2', 'tips', 'tips1', 'tips2',
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref', 'state_ref', 'alimony',
  'sole_prop', 'sole_prop1', 'sole_prop2',
  'kg_st', 'kg_lt', 'kg_lt_basis', 'kg_1250', 'kg_collect', 'other_gains',
  'txbl_ira_dist', 'gross_pens_dist', 'txbl_pens_dist',
  'part_active', 'part_active_loss', 'part_passive', 'part_passive_loss',
  'part_179', 'part_se', 'part_se1', 'part_se2',
  'scorp_active', 'scorp_active_loss', 'scorp_passive', 'scorp_passive_loss',
  'scorp_179', 'rent', 'rent_loss', 'estate', 'estate_loss',
  'farm', 'farm1', 'farm2', 'ui', 'gross_ss', 'nols', 'other_inc'
)

# Deductions and the expenses behind them. Mortgage balances travel with the
# interest they generate
DEDUCTION_COLS = c(
  'ed_exp', 'hsa_contr', 'trad_contr_ira', 'sl_int_ded', 'keogh_contr',
  'se_health', 'early_penalty', 'alimony_exp', 'tuition_ded', 'dpad',
  'char_cash', 'char_noncash', 'other_above_ded', 'auto_int_exp',
  'med_exp', 'salt_inc_sales', 'salt_prop', 'salt_pers',
  'first_mort_bal', 'second_mort_bal', 'first_mort_int', 'second_mort_int',
  'inv_int_exp', 'casualty_exp', 'job_exp', 'tax_prep_exp', 'other_misc_exp',
  'other_item_exp'
)

# Unrealized appreciation, which carries no tax in the year it accrues and enters
# the accrual measure only
ACCRUAL_COLS = c('accruals.equities', 'accruals.pass_throughs',
                 'accruals.primary_home', 'accruals.other_home',
                 'accruals.re_fund', 'accruals.dc', 'accruals.trusts')


#-------------------------------------------------------------------------------
# Frame and pricing, as run_one_year builds them for a baseline static pass
#-------------------------------------------------------------------------------

build_frame = function(year) {

  tax_units = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%
    filter(id %in% globals$sample_ids) %>%
    mutate(weight        = weight / globals$pct_sample,
           year          = year,
           decedent_flag = 0L) %>%
    left_join(globals$random_numbers, by = 'id') %>%
    left_join(tax_law %>% distinct(year, filing.repeal_hoh), by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1, filing_status)) %>%
    left_join(tax_law, by = c('year', 'filing_status')) %>%
    do_salt_workaround_baseline() %>%
    do_ss_cola(year, config$vat_price_offset) %>%
    do_capital_adjustment(year, config$vat_price_offset) %>%
    calc_kg_cpi_ratio(config$indexes, year)

  globals$estate_params <<- get_estate_params(
    scenario_info$interface_paths$`Tax-Data`)
  tax_units$estate_m = calc_estate_mortality(
    tax_units, globals$estate_params$cluster_death_weight_cap)

  tax_units %>%
    mutate(net_worth = rowSums(across(all_of(WEALTH_ASSET_COLS), z0)) -
                       rowSums(across(all_of(WEALTH_DEBT_COLS),  z0)))
}

vars_1040 = return_vars %>%
  remove_by_name('calc_pr') %>%
  unlist() %>%
  set_names(NULL)

price = function(fr) {
  fr %>%
    do_taxes(baseline_pr_er = NULL,
             vars_1040      = vars_1040,
             vars_payroll   = return_vars$calc_pr)
}

cat('Building', YEAR, 'baseline frame\n')
frame = build_frame(YEAR)

missing = setdiff(c(INCOME_COLS, DEDUCTION_COLS, ACCRUAL_COLS), names(frame))
if (length(missing) > 0) {
  stop('Microdata is missing columns: ', paste(missing, collapse = ', '))
}

cat('Pricing current law\n')
base = price(frame)

# Federal tax during life: individual income tax plus payroll tax
tax_of = function(taxed) z0(taxed$liab_iit_net) + z0(taxed$liab_pr)

weight = z0(frame$weight)
tax0   = tax_of(base)
adult  = z0(frame$dep_status) == 0

sum_cols = function(fr, cols) rowSums(as.data.frame(lapply(fr[cols], z0)))

# Accrual income: adjusted gross income with realized gains replaced by accruals,
# less the defined-contribution accrual and withdrawal double count as in
# distribution.R. Defined benefit plans have no accruals counterpart and stay.
#
# The gains that come out are the ones adjusted gross income contains, net of the
# capital loss limitation and any exclusion, rather than the raw realizations
accrual_of = function(fr, taxed) {
  dc_share = if_else(z0(fr$value.dc) + z0(fr$value.db) > 0,
                     z0(fr$value.dc) / (z0(fr$value.dc) + z0(fr$value.db)), 0)
  z0(taxed$agi) -
    (z0(taxed$txbl_kg) + z0(fr$other_gains)) +
    sum_cols(fr, ACCRUAL_COLS) -
    z0(fr$txbl_ira_dist) - z0(fr$gross_pens_dist) * dc_share
}

measure_of = function(fr, taxed, measure) {
  if (measure == 'agi') z0(taxed$agi) else accrual_of(fr, taxed)
}

# Appreciation is part of the accrual measure and so scales with it. It is not in
# adjusted gross income and is left alone there
scale_cols_for = function(measure) {
  if (measure == 'agi') c(INCOME_COLS, DEDUCTION_COLS)
  else                  c(INCOME_COLS, DEDUCTION_COLS, ACCRUAL_COLS)
}

POS = map(unique(c(INCOME_COLS, DEDUCTION_COLS, ACCRUAL_COLS)),
          ~ z0(frame[[.x]]) > 0) %>%
  set_names(unique(c(INCOME_COLS, DEDUCTION_COLS, ACCRUAL_COLS)))


#-------------------------------------------------------------------------------
# One group on one measure
#-------------------------------------------------------------------------------

run_group = function(measure, share, y) {

  cols = scale_cols_for(measure)

  evaluate = function(f) {
    cut = frame
    for (nm in cols) {
      v = z0(cut[[nm]])
      cut[[nm]] = if_else(POS[[nm]], v * f, v)
    }
    taxed = price(cut)
    list(y = measure_of(cut, taxed, measure), taxed = taxed)
  }

  # The group's own threshold, over non-dependent filers as the distribution
  # tables cut them
  ord = order(-y[adult])
  cw  = cumsum(weight[adult][ord])
  thr = y[adult][ord][max(1, sum(cw < sum(weight[adult]) * share))]

  in_group = adult & y >= thr
  hit      = y > thr & weight > 0

  # A record whose income still clears the threshold with every positive flow
  # removed cannot be brought down to it. Measure how much of the above-threshold
  # income sits in those records rather than iterating against them
  at_zero     = evaluate(rep(0, nrow(frame)))
  unreachable = hit & at_zero$y > thr * (1 + 1e-9)
  active      = hit & !unreachable

  # Solve the per-record scale factor by bisection. Every record bisects inside
  # the same calculator call, so the cost is the iteration count and not the
  # record count
  lo = if_else(active, 0, if_else(unreachable, 0, 1))
  hi = if_else(active, 1, if_else(unreachable, 0, 1))
  for (iter in 1:26) {
    mid      = (lo + hi) / 2
    ev       = evaluate(mid)
    too_high = active & ev$y > thr
    hi[too_high]           = mid[too_high]
    lo[active & !too_high] = mid[active & !too_high]
    gap = if (any(active)) max(abs(ev$y[active] - thr)) / thr else 0
    if (gap < 1e-7) break
  }
  f         = (lo + hi) / 2
  final     = evaluate(f)
  taxed_cut = final$taxed
  gap       = if (any(active)) max(abs(final$y[active] - thr)) / thr else 0

  total_B  = sum(weight[in_group] * y[in_group]) / 1e9
  above_B  = sum(weight * pmax(0, y - thr)) / 1e9
  taxon_B  = (sum(weight * tax0) - sum(weight * tax_of(taxed_cut))) / 1e9

  tibble(
    measure   = measure,
    group     = paste0('Top ', share * 100, '%'),
    threshold = thr,
    n_units_M = sum(weight[in_group]) / 1e6,
    total_income_B    = total_B,
    above_B           = above_B,
    tax_on_above_B    = taxon_B,
    after_tax_above_B = above_B - taxon_B,
    rate_on_above     = 100 * taxon_B / above_B,
    total_income_pct_gdp    = 100 * total_B / GDP_B,
    above_pct_gdp           = 100 * above_B / GDP_B,
    after_tax_above_pct_gdp = 100 * (above_B - taxon_B) / GDP_B,
    solver_gap        = gap,
    unreachable_share = sum(weight[unreachable] *
                            pmax(0, y[unreachable] - thr)) /
                        sum(weight * pmax(0, y - thr))
  )
}

rows = list()
for (measure in c('agi', 'accrual')) {
  y = measure_of(frame, base, measure)
  for (share in TOP_SHARES) {
    cat('Case:', measure, 'top', share * 100, '%\n')
    rows[[length(rows) + 1]] = run_group(measure, share, y)
  }
}

results = bind_rows(rows) %>%
  mutate(year = YEAR, gdp_B = GDP_B, .before = everything())

write_csv(results, file.path(OUT_DIR, 'above_threshold_income.csv'))
print(as.data.frame(results))
cat('done\n')

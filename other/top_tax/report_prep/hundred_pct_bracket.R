#-------------------------------------------------------------------------------
# hundred_pct_bracket.R
#
# Computes the revenue raised by taking every dollar of income above a threshold,
# holding tax on income below the threshold at current law, for four income
# concepts
#
# The policy is a base change rather than a rate change, so it is measured by
# running the calculator twice rather than by blending marginal rate columns. A
# unit's tax becomes its current-law tax on income truncated at the threshold,
# plus the whole of the income above it:
#
#   revenue = sum w * (y - T)+  +  sum w * tax(y capped at T)  -  sum w * tax(y)
#
# The tax currently collected on the above-threshold slice is then the difference
# of the two calculator passes, which is what the analytic version of this table
# approximated with a composition-weighted marginal rate.
#
# Run via sbatch (never on the login node):
#   sbatch -p day -c 4 --mem=96G -t 6:00:00 \
#     --wrap "cd <repo> && module load R/4.4.1-foss-2022b && \
#             Rscript other/top_tax/report_prep/hundred_pct_bracket.R"
#-------------------------------------------------------------------------------

REPO    = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator'
VINTAGE = paste0('/nfs/roberts/scratch/pi_nrs36/jar335/model_data/',
                 'Tax-Simulator/v1/top_tax_dials_30y_v6')
STAGING = file.path(VINTAGE, '_slurm_staging')
OUT_DIR = file.path(REPO, 'other/top_tax/report_prep')

YEAR    = 2027
GDP_B   = 33315.19   # FY2027 nominal GDP, Macro-Projections 2026022522 gdp_fy
DEFICIT_B = 1900.0   # FY2027, CBO February 2026

# Round dollar thresholds, and the top shares whose own cutoffs are solved for
# under each income concept
DOLLAR_THRESHOLDS = c(1e6, 5e6, 10e6)
TOP_SHARES        = c(0.10, 0.05, 0.01, 0.001, 0.0001)

setwd(REPO)
source('src/slurm/common.R')
invisible(reconstitute_environment(STAGING))

config = readRDS(file.path(STAGING, 'baseline', 'config.rds'))
config_activate(economy  = config$scenario_info$resolved_economy,
                behavior = config$scenario_info$resolved_behavior)

scenario_info = config$scenario_info
tax_law       = config$tax_law


#-------------------------------------------------------------------------------
# Income columns
#-------------------------------------------------------------------------------

# Every microdata column carrying an income flow. Scaling a unit's income down to
# the threshold scales all of them together, so that no unit's tax falls because
# the truncation happened to take its gains rather than its wages. Sub-components
# travel with their totals, and gain basis with the gain, so that composition and
# the gain-to-basis ratio are held fixed.
INCOME_COLS = c(
  'wages', 'wages1', 'wages2',
  'ot', 'ot1', 'ot2',
  'tips', 'tips1', 'tips2',
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
  'state_ref', 'alimony',
  'sole_prop', 'sole_prop1', 'sole_prop2',
  'kg_st', 'kg_lt', 'kg_lt_basis', 'kg_1250', 'kg_collect', 'other_gains',
  'txbl_ira_dist', 'gross_pens_dist', 'txbl_pens_dist',
  'part_active', 'part_active_loss', 'part_passive', 'part_passive_loss',
  'part_179', 'part_se', 'part_se1', 'part_se2',
  'scorp_active', 'scorp_active_loss', 'scorp_passive', 'scorp_passive_loss',
  'scorp_179',
  'rent', 'rent_loss', 'estate', 'estate_loss',
  'farm', 'farm1', 'farm2',
  'ui', 'gross_ss', 'nols', 'other_inc'
)

# The capital-gain and qualified-dividend flows, held fixed when the concept being
# truncated is ordinary income. Ordinary income subtracts every one of them that
# enters AGI, so that removing a record's ordinary flows can always bring the
# concept to the threshold
PREF_COLS = c('div_pref', 'kg_st', 'kg_lt', 'kg_lt_basis', 'kg_1250',
              'kg_collect', 'other_gains')

# The accrual columns, which carry no tax and so enter the measurement of
# Haig-Simons income only
ACCRUAL_COLS = c('accruals.equities', 'accruals.pass_throughs',
                 'accruals.primary_home', 'accruals.other_home',
                 'accruals.re_fund', 'accruals.dc', 'accruals.trusts')

z0 = function(x) replace_na(as.numeric(x), 0)


#-------------------------------------------------------------------------------
# Build the priced input frame, as run_one_year does for a baseline static pass
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

price = function(frame) {
  frame %>%
    do_taxes(baseline_pr_er = NULL,
             vars_1040      = vars_1040,
             vars_payroll   = return_vars$calc_pr)
}

cat('Building', YEAR, 'baseline frame\n')
frame = build_frame(YEAR)

missing = setdiff(c(INCOME_COLS, ACCRUAL_COLS), names(frame))
if (length(missing) > 0) {
  stop('Microdata is missing income columns: ', paste(missing, collapse = ', '))
}

cat('Pricing current law\n')
base = price(frame)


#-------------------------------------------------------------------------------
# The income concepts
#-------------------------------------------------------------------------------

# Federal tax during life: individual income tax plus both sides of payroll.
# Truncating income cuts payroll liability as well as income tax, so both belong
# in the current-law leg the policy is measured against.
tax_of = function(taxed) z0(taxed$liab_iit_net) + z0(taxed$liab_pr)

# The accrual measure is expanded income with realized gains replaced by
# accruals, less the defined-contribution accrual and withdrawal double count, as
# in distribution.R. Defined benefit plans have no accruals counterpart and stay.
accrual_income = function(frame, taxed) {
  dc_share = if_else(z0(frame$value.dc) + z0(frame$value.db) > 0,
                     z0(frame$value.dc) / (z0(frame$value.dc) + z0(frame$value.db)),
                     0)
  z0(taxed$expanded_inc) -
    (z0(frame$kg_st) + z0(frame$kg_lt) + z0(frame$other_gains)) +
    rowSums(across_cols(frame, ACCRUAL_COLS)) -
    z0(frame$txbl_ira_dist) - z0(frame$gross_pens_dist) * dc_share
}

across_cols = function(df, cols) {
  as.data.frame(lapply(df[cols], z0))
}

concept_value = function(frame, taxed, concept) {
  switch(concept,
    'taxable'  = z0(taxed$txbl_inc),
    'ordinary' = z0(taxed$agi) - (z0(frame$div_pref) + z0(taxed$txbl_kg) +
                                  z0(frame$other_gains)),
    'agi'      = z0(taxed$agi),
    'accrual'  = accrual_income(frame, taxed),
    stop('Unknown concept: ', concept))
}

# The columns a truncation scales. The ordinary concept leaves the
# preferentially taxed flows alone, since the policy does not reach them.
scale_cols_for = function(concept) {
  if (concept == 'ordinary') setdiff(INCOME_COLS, PREF_COLS) else INCOME_COLS
}

# Cash income, used only to report the percentile a dollar threshold lands at
cash_income = z0(base$expanded_inc)

concepts = c('taxable', 'ordinary', 'agi', 'accrual')
y0 = map(concepts, ~ concept_value(frame, base, .x)) %>% set_names(concepts)

tax0   = tax_of(base)
weight = z0(frame$weight)

cat('Current-law federal tax during life: $',
    round(sum(weight * tax0) / 1e9), 'B\n', sep = '')


#-------------------------------------------------------------------------------
# Percentile cutoffs
#-------------------------------------------------------------------------------

# Cutoffs are taken over non-dependent filers, matching the distribution tables,
# while the bracket itself applies to every record
adult = z0(frame$dep_status) == 0

top_cutoff = function(y, share) {
  ord = order(-y[adult])
  w   = weight[adult][ord]
  v   = y[adult][ord]
  cw  = cumsum(w)
  n   = sum(w) * share
  v[max(1, sum(cw < n))]
}

share_above = function(y, thresh) {
  sum(weight[adult & y > thresh]) / sum(weight[adult])
}


#-------------------------------------------------------------------------------
# One case: a concept and a threshold
#-------------------------------------------------------------------------------

run_case = function(concept, thresh, label) {

  cols = scale_cols_for(concept)
  y    = y0[[concept]]
  hit  = y > thresh & weight > 0

  # Solve the per-record scale factor by bisection on [0, 1]. Every record
  # bisects inside the same calculator call, so the cost is the iteration count
  # and not the record count.
  #
  # Only positive flows scale. Shrinking a loss would raise the concept rather
  # than lower it, which breaks the monotonicity bisection needs, and the policy
  # takes income away rather than handing out deductions.
  pos = map(cols, ~ z0(frame[[.x]]) > 0) %>% set_names(cols)

  evaluate = function(f) {
    cut = frame
    for (nm in cols) {
      v = z0(cut[[nm]])
      cut[[nm]] = if_else(pos[[nm]], v * f, v)
    }
    taxed_cut = price(cut)
    y_cut     = concept_value(cut, taxed_cut, concept)
    if (concept == 'accrual') {
      # The accrual columns carry no tax, so they are scaled for measurement here
      # rather than in the frame handed to the calculator
      accr  = rowSums(across_cols(frame, ACCRUAL_COLS))
      y_cut = y_cut - accr + pmax(0, accr) * f
    }
    list(y = y_cut, taxed = taxed_cut)
  }

  # A record whose concept still exceeds the threshold with every positive flow
  # removed cannot be truncated by this construction. Measure how much of the
  # base sits in those records rather than iterating against them.
  at_zero     = evaluate(rep(0, nrow(frame)))
  unreachable = hit & at_zero$y > thresh * (1 + 1e-9)

  # Records the bracket does not reach, and records it cannot truncate, are
  # pinned so that the midpoint below is their own factor and never a bisection
  # step applied by accident
  active = hit & !unreachable
  lo = if_else(active, 0, if_else(unreachable, 0, 1))
  hi = if_else(active, 1, if_else(unreachable, 0, 1))

  for (iter in 1:26) {
    mid = (lo + hi) / 2
    ev  = evaluate(mid)
    too_high = active & ev$y > thresh
    hi[too_high]           = mid[too_high]
    lo[active & !too_high] = mid[active & !too_high]
    gap = if (any(active)) max(abs(ev$y[active] - thresh)) / thresh else 0
    if (iter %% 6 == 0) {
      cat('  iter', iter, 'max relative gap', signif(gap, 3), '\n')
    }
    if (gap < 1e-7) break
  }

  f         = (lo + hi) / 2
  final     = evaluate(f)
  taxed_cut = final$taxed
  gap       = if (any(active)) max(abs(final$y[active] - thresh)) / thresh else 0
  unreachable_base_share = sum(weight[unreachable] *
                               pmax(0, y[unreachable] - thresh)) /
                           sum(weight * pmax(0, y - thresh))

  taken_B   = sum(weight * pmax(0, y - thresh)) / 1e9
  tax_cut_B = sum(weight * tax_of(taxed_cut)) / 1e9
  tax_now_B = sum(weight * tax0) / 1e9
  paid_B    = tax_now_B - tax_cut_B

  tibble(
    concept        = concept,
    threshold_type = label,
    threshold      = thresh,
    n_units_M      = sum(weight[hit]) / 1e6,
    share_above    = share_above(y, thresh),
    taken_B        = taken_B,
    tax_paid_now_B = paid_B,
    implied_rate   = 100 * paid_B / taken_B,
    revenue_B      = taken_B - paid_B,
    revenue_pct_gdp = 100 * (taken_B - paid_B) / GDP_B,
    iter_gap        = gap,
    unreachable_share = unreachable_base_share
  )
}


#-------------------------------------------------------------------------------
# Run every case
#-------------------------------------------------------------------------------

cases = list()

for (concept in concepts) {
  for (thresh in DOLLAR_THRESHOLDS) {
    cat('Case:', concept, 'at $', format(thresh, big.mark = ','), '\n')
    cases[[length(cases) + 1]] = run_case(concept, thresh, 'dollar')
  }
  for (share in TOP_SHARES) {
    thresh = top_cutoff(y0[[concept]], share)
    cat('Case:', concept, 'top', share * 100, '% cutoff $',
        format(round(thresh), big.mark = ','), '\n')
    cases[[length(cases) + 1]] = run_case(concept, thresh,
                                          paste0('top_', share * 100))
  }
}

results = bind_rows(cases) %>%
  mutate(year = YEAR, gdp_B = GDP_B, deficit_B = DEFICIT_B,
         .before = everything())

write_csv(results, file.path(OUT_DIR, 'hundred_pct_bracket.csv'))


#-------------------------------------------------------------------------------
# Percentile a dollar threshold lands at, under each concept and under cash
#-------------------------------------------------------------------------------

placement = expand_grid(threshold = DOLLAR_THRESHOLDS,
                        measure   = c(concepts, 'cash')) %>%
  mutate(share_above = map2_dbl(
    threshold, measure,
    ~ share_above(if (.y == 'cash') cash_income else y0[[.y]], .x)))

write_csv(placement, file.path(OUT_DIR, 'hundred_pct_bracket_placement.csv'))

print(as.data.frame(results))
print(as.data.frame(placement))
cat('done\n')

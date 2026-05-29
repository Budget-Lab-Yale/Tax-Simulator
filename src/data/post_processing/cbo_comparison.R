#-------------------------------------------------------------------------------
# cbo_comparison.R
#
# Post-processing step that benchmarks our current-law baseline against CBO's
# published baseline (the "Individual Income Tax Details" 1040 build-up). Writes
# a line-by-line CBO-vs-ours comparison to the baseline's supplemental folder.
#
# The CBO reference is a committed, pre-processed resource (see
# other/cbo/process_cbo_revenue.py). Because CBO's reference is a current-law
# baseline, this is only run for the baseline scenario (a reform's 1040 vs CBO
# baseline would conflate policy and baseline differences).
#-------------------------------------------------------------------------------

# Path to the committed, pre-processed CBO reference (tidy: cbo_row, cbo_line,
# year, value). Regenerate with other/cbo/process_cbo_revenue.py on a new CBO
# release and update this path / the mapping below if line labels change.
CBO_REFERENCE_PATH = './resources/cbo/cbo_iit_detail_feb2026.csv'

# Mapping from CBO line items to our 1040.csv totals columns.
#   - cbo_line : a distinctive substring of the CBO row label. When multiple CBO
#                rows contain it, the first in sheet order (min cbo_row) is used.
#   - our_expr : '+'-separated 1040.csv columns to sum, or the sentinel
#                '__after_credits__' = liab_iit - liab_niit - liab_surtax.
# Per-bracket CBO lines (taxable income / tax by bracket) are intentionally
# omitted; our totals have no per-bracket breakdown.
cbo_comparison_mapping = function() {
  tibble::tribble(
    ~section, ~line_item,                                   ~cbo_line,                                              ~our_expr,
    'AGI',    'Salaries and wages',                         'Salaries and wages',                                   'wages',
    'AGI',    'Taxable interest and ordinary dividends',    'Taxable interest and ordinary dividends',              'txbl_int + div_ord',
    'AGI',    'Qualified dividends',                         'Qualified dividends',                                  'div_pref',
    'AGI',    'Capital gain or loss',                        'Capital gain or loss',                                 'txbl_kg',
    'AGI',    'Net business income (Sch C, E, F)',           'Net business income',                                  'sole_prop + sch_e + farm',
    'AGI',    'Taxable pensions/annuities + IRA dist.',      'Taxable pensions and annuities and IRA distributions', 'txbl_pens_dist + txbl_ira_dist',
    'AGI',    'Taxable Social Security benefits',            'Taxable Social Security benefits',                     'txbl_ss',
    'AGI',    'Total income',                                'Total income',                                         'gross_inc',
    'AGI',    'Subtract statutory adjustments',              'Subtract statutory adjustments',                       'above_ded',
    'AGI',    'Adjusted gross income',                       'Adjusted gross income',                                'agi',
    'TXBL',   'Subtract personal exemption',                 'Subtract personal exemption',                          'pe_ded',
    'TXBL',   'Subtract standard deduction',                 'Subtract standard deduction',                          'std_ded',
    'TXBL',   'Subtract itemized deductions',                'Subtract total itemized deductions',                   'item_ded',
    'TXBL',   'Subtract QBI deduction',                      'Subtract qualified business income deduction',         'qbi_ded',
    'TXBL',   'Subtract additional deductions',              'Subtract additional deductions',                       'tip_ded + ot_ded + senior_ded',
    'TXBL',   'Total exemptions and deductions',             'Total exemptions and deductions after limits',         'ded',
    'TXBL',   'Taxable income',                              'Taxable income',                                       'txbl_inc',
    'TAX',    'Total income tax before credits (incl AMT)',  'Total income tax (including AMT) before credits',      'liab_bc',
    'TAX',    'Tax at ordinary rates',                       'Tax from taxable income and taxed at ordinary rates',  'liab_ord',
    'TAX',    'Tax at reduced rates (cap gains/div)',        'Tax from taxable income and taxed at reduced rates',   'liab_pref + liab_1250 + liab_collect',
    'TAX',    'Tax from AMT',                                'Tax from AMT',                                         'liab_amt',
    'TAX',    'Total credits (refundable + nonrefundable)',  'Total credits (refundable and nonrefundable)',         'nonref + ref',
    'TAX',    'Income tax after credits',                    'Income tax after credits',                             '__after_credits__',
    'TAX',    'Net investment income tax',                   'Net investment income tax',                            'liab_niit',
    'TAX',    'Individual income tax liability',             'Individual income tax liability',                      'liab_iit',
    'ADD',    'Number of returns (millions)',                'Number of returns',                                    'n_returns',
    'ADD',    'Number with itemized deductions',             'Number with itemized deductions',                      'n_itemizing'
  )
}


build_cbo_comparison = function(id) {

  #----------------------------------------------------------------------------
  # Writes a line-by-line comparison of the run's baseline 1040 totals against
  # the committed CBO baseline reference, for the years the run and CBO overlap.
  #
  # Parameters:
  #   - id (str) : scenario ID (expected to be 'baseline')
  #
  # Returns: void. Writes
  #   <baseline_root>/<id>/static/supplemental/cbo_comparison.csv
  # with columns: section, line_item, our_mapping, year, cbo, ours, diff,
  #   pct_diff   (dollars in billions, returns in millions; pct_diff in points).
  #----------------------------------------------------------------------------

  if (!file.exists(CBO_REFERENCE_PATH)) {
    warning(paste0('CBO reference not found at ', CBO_REFERENCE_PATH,
                   '; skipping CBO comparison.'))
    return(invisible(NULL))
  }

  static_root = file.path(globals$baseline_root, id, 'static')

  totals = file.path(static_root, 'totals', '1040.csv') %>%
    read_csv(show_col_types = FALSE)

  cbo = read_csv(CBO_REFERENCE_PATH, show_col_types = FALSE)

  mapping = cbo_comparison_mapping()

  overlap_years = sort(intersect(totals$year, cbo$year))

  # Our value for one line item in one year (totals is filtered to that year).
  our_value = function(our_expr, yr_row) {
    if (our_expr == '__after_credits__') {
      return(yr_row$liab_iit - yr_row$liab_niit - yr_row$liab_surtax)
    }
    cols = str_trim(str_split(our_expr, fixed('+'))[[1]])
    sum(unlist(yr_row[cols]))
  }

  # CBO values (year -> value) for the first sheet row matching the substring.
  cbo_values = function(cbo_line) {
    matches = cbo %>% filter(str_detect(cbo_line, fixed(!!cbo_line)))
    if (nrow(matches) == 0) return(NULL)
    matches %>% filter(cbo_row == min(cbo_row)) %>% select(year, value)
  }

  comparison = mapping %>%
    pmap(function(section, line_item, cbo_line, our_expr) {
      cbo_vals = cbo_values(cbo_line)
      overlap_years %>%
        map(function(yr) {
          yr_row = totals %>% filter(year == yr)
          ours   = if (nrow(yr_row) == 1) our_value(our_expr, yr_row) else NA_real_
          cbo_v  = NA_real_
          if (!is.null(cbo_vals)) {
            v = cbo_vals %>% filter(year == yr) %>% pull(value)
            if (length(v) == 1) cbo_v = v
          }
          diff = ours - cbo_v
          tibble(
            section     = section,
            line_item   = line_item,
            our_mapping = if (our_expr == '__after_credits__')
                            'liab_iit - liab_niit - liab_surtax' else our_expr,
            year        = yr,
            cbo         = cbo_v,
            ours        = ours,
            diff        = diff,
            pct_diff    = if_else(!is.na(cbo_v) & cbo_v != 0, diff / cbo_v * 100, NA_real_)
          )
        }) %>%
        bind_rows()
    }) %>%
    bind_rows()

  comparison %>%
    mutate(across(c(cbo, ours, diff, pct_diff), ~ round(.x, 1))) %>%
    write_csv(file.path(static_root, 'supplemental', 'cbo_comparison.csv'))
}

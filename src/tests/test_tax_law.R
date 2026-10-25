#---------------------------------------------------------------
# Tests for federal tax law configuration machinery
#
# Defines functions only (this file is sourced by main.R's recursive
# walk; side effects at source time would break model startup).
#
# Run manually:
#   module load R/4.4.2-gfbf-2024a
#   Rscript -e "
#     suppressPackageStartupMessages(invisible(capture.output(
#       lapply(readLines('./requirements.txt'), library, character.only = T))));
#     return_vars = list();
#     list.files('./src', recursive = T, pattern = '\\.[Rr]$') %>%
#       walk(~ if (. != 'main.R' && !startsWith(., 'slurm/')) source(file.path('./src/', .)));
#     test_tax_law()"
#---------------------------------------------------------------


test_tax_law = function() {

  #----------------------------------------------------------------------------
  # Runs all federal tax law configuration tests, stopping on first failure.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  test_unrounded_mapper()
  test_eitc_childless_threshold_indexed()
  test_eitc_mfs_separated_spouse_rule()
  test_eitc_childless_spouse_age()
  test_auto_int_ded_stepped_phaseout()
  test_schedule_1a_in_qbi_and_amt()
  message('test_tax_law: ALL TESTS PASSED')
  invisible(TRUE)
}



test_unrounded_mapper = function() {

  #----------------------------------------------------------------------------
  # A filing status mapper can combine indexed amounts before rounding via
  # {name}__unrounded, as IRC 32(j)(2)(A) requires for the EITC joint phase-out
  # threshold. With 1,003 grown 2% to 1,023.06, rounding each amount to $10
  # first gives 1,020 + 1,020 = 2,040; rounding the combined 2,046.12 gives
  # 2,050. The unrounded twins must not leak into the parsed output, and
  # subparameters used only through them must still be dropped as
  # intermediates.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  yaml_text = "
indexation_defaults:
  i_measure:
    '2017': cpi
  i_base_year: 2020
  i_direction: 0
  i_increment: 10

filing_status_mapper:
  combined:
    '1': thresh
    '2': round((thresh__unrounded + joint_adj__unrounded) / 10) * 10
    '3': thresh
    '4': thresh
  separate:
    '1': thresh
    '2': thresh + joint_adj
    '3': thresh
    '4': thresh

thresh:
  value: 1003
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default

joint_adj:
  value: 1003
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default
"

  # Constant-growth synthetic index series: the 2022 factor is 1.02
  test_indexes = expand_grid(series = 'cpi', year = 2015:2035) %>%
    mutate(growth = 0.02)

  parsed = read_yaml(text = yaml_text) %>%
    parse_param(name = 'test', years = 2017:2035, indexes = test_indexes)

  value_at = function(subparam, status) {
    parsed %>%
      filter(subparameter == subparam, year == 2022, filing_status == status) %>%
      pull(value)
  }

  stopifnot(
    'combined joint amount not rounded once' = value_at('combined', 2) == 2050,
    'separate joint amount changed'          = value_at('separate', 2) == 2040,
    'single amount not rounded'              = value_at('combined', 1) == 1020,
    'unrounded twins or intermediates leaked into output' =
      setequal(unique(parsed$subparameter), c('combined', 'separate'))
  )

  message('test_unrounded_mapper: PASSED')
  invisible(TRUE)
}



test_eitc_childless_threshold_indexed = function() {

  #----------------------------------------------------------------------------
  # Regression test for the unindexed childless EITC phase-out threshold: an
  # empty i_measure parses to null, which parse_subparam() reads as unindexed,
  # so po_thresh_0 stayed at its 1995 base of $5,280. In every current-law
  # configuration it must grow with the index outside 2021 (ARPA).
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  tax_law_ids = c('baseline', 'baseline_2024', 'baseline_2024_tcja_ext',
                  'tests/baseline_2017', 'tests/tcja_2017',
                  'tests/booker_ctc_tcja_ext')

  # Constant-growth synthetic index series for every indexation measure
  test_indexes = expand_grid(series = c('cpi', 'chained_cpi', 'awi'),
                             year   = 1970:2040) %>%
    mutate(growth = 0.02)

  for (tax_law_id in tax_law_ids) {
    po_thresh_0 = build_tax_law_from_id(tax_law_id, years = c(2018, 2023),
                                        indexes = test_indexes) %>%
      filter(filing_status == 1) %>%
      arrange(year) %>%
      pull(eitc.po_thresh_0)

    if (!(po_thresh_0[1] > 5280 && po_thresh_0[2] > po_thresh_0[1])) {
      stop('eitc.po_thresh_0 is not indexed in ', tax_law_id, ': ',
           paste(po_thresh_0, collapse = ', '))
    }
  }

  message('test_eitc_childless_threshold_indexed: PASSED')
  invisible(TRUE)
}



test_eitc_mfs_separated_spouse_rule = function() {

  #----------------------------------------------------------------------------
  # Regression test for the married-filing-separately EITC. mfs_eligible was 0
  # in every year, denying separate filers the credit even after IRC 32(d)(2)
  # (ARPA sec. 9623) allowed it from 2021 to one with a qualifying child who
  # lived with them over half the year and lived apart from the spouse (the
  # living-apart test is unobserved and assumed met). In every current-law
  # configuration a separate filer must get: nothing in 2020, nothing in 2023
  # without a qualifying child, and in 2023 with one the same credit as a
  # head of household with the same income.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  tax_law_ids = c('baseline', 'baseline_2024', 'baseline_2024_tcja_ext',
                  'tests/baseline_2017', 'tests/tcja_2017',
                  'tests/booker_ctc_tcja_ext')

  test_indexes = expand_grid(series = c('cpi', 'chained_cpi', 'awi'),
                             year   = 1970:2040) %>%
    mutate(growth = 0.02)

  # $8,000 of wages: inside every schedule's phase-in or plateau, so any
  # eligible unit gets a positive credit
  cases = tribble(
    ~year, ~filing_status, ~n_dep_eitc, ~should_get,
    2020,  3,              1,           'zero',
    2023,  3,              0,           'zero',
    2023,  3,              1,           'hoh_amount',
    2023,  4,              1,           'hoh_amount'
  )

  for (tax_law_id in tax_law_ids) {
    law = build_tax_law_from_id(tax_law_id, years = c(2020, 2023),
                                indexes = test_indexes)
    result = cases %>%
      mutate(dep_status = 0, age1 = 40, ei1 = 8000, agi = 8000) %>%
      left_join(law, by = c('year', 'filing_status')) %>%
      mutate(eitc = calc_eitc(., fill_missings = T)$eitc)

    hoh = result$eitc[result$filing_status == 4]
    ok = result %>%
      mutate(pass = if_else(should_get == 'zero', eitc == 0,
                            abs(eitc - hoh) < 0.01 & eitc > 0))
    if (!all(ok$pass)) {
      stop('MFS EITC separated-spouse rule fails in ', tax_law_id, ': ',
           paste(sprintf('%d/fs%d/kids%d -> %.2f', ok$year, ok$filing_status,
                         ok$n_dep_eitc, ok$eitc)[!ok$pass], collapse = '; '))
    }
  }

  message('test_eitc_mfs_separated_spouse_rule: PASSED')
  invisible(TRUE)
}



federal_test_law = function(years) {

  #----------------------------------------------------------------------------
  # Baseline federal law for the given years on a flat 2% index path, joined
  # by year and filing status in the tests below.
  #
  # Returns: tax-law tibble (df).
  #----------------------------------------------------------------------------

  test_indexes = expand_grid(series = c('cpi', 'chained_cpi', 'awi'),
                             year   = 1970:2040) %>%
    mutate(growth = 0.02)
  build_tax_law_from_id('baseline', years = years, indexes = test_indexes)
}



test_eitc_childless_spouse_age = function() {

  #----------------------------------------------------------------------------
  # Without a qualifying child, IRC 32(c)(1)(A)(ii) requires that the taxpayer
  # OR the spouse be in the age range, and a joint return's earned income is
  # combined. The credit used to count only the in-range spouse's earnings, so
  # a 22-year-old earner married to a 30-year-old got nothing.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  law = federal_test_law(2023)
  cases = tribble(
    ~age1, ~age2, ~ei1, ~ei2, ~should_get,
    22,    30,    6000, 0,    'positive',   # earner too young, spouse in range
    30,    22,    0,    6000, 'positive',   # the mirror image
    22,    22,    6000, 0,    'zero'        # neither in range
  )
  result = cases %>%
    mutate(year = 2023, filing_status = 2, dep_status = 0, n_dep_eitc = 0,
           agi = ei1 + ei2) %>%
    left_join(law, by = c('year', 'filing_status')) %>%
    mutate(eitc = calc_eitc(., fill_missings = T)$eitc)
  ok = if_else(result$should_get == 'zero', result$eitc == 0, result$eitc > 0)
  if (!all(ok) || abs(result$eitc[1] - result$eitc[2]) > 0.01) {
    stop('Childless joint EITC age test fails: ',
         paste(sprintf('%d/%d -> %.2f', result$age1, result$age2, result$eitc), collapse = '; '))
  }

  message('test_eitc_childless_spouse_age: PASSED')
  invisible(TRUE)
}



test_auto_int_ded_stepped_phaseout = function() {

  #----------------------------------------------------------------------------
  # IRC 163(h)(4)(C)(ii) reduces the vehicle loan interest deduction by $200
  # for each $1,000 "(or fraction thereof)" of MAGI over the threshold. One
  # dollar over costs $200; $1,000 over costs $200; $1,001 over costs $400.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  law = federal_test_law(2026)
  result = tibble(agi = c(100000, 100001, 101000, 101001), expect = c(10000, 9800, 9800, 9600)) %>%
    mutate(year = 2026, filing_status = 1, auto_int_exp = 10000, r.new_car = 0) %>%
    left_join(law, by = c('year', 'filing_status')) %>%
    mutate(auto_int_ded = calc_below_ded(., fill_missings = T)$auto_int_ded)
  if (any(abs(result$auto_int_ded - result$expect) > 0.01)) {
    stop('Auto loan interest phaseout is not stepped: ',
         paste(sprintf('%.0f -> %.2f', result$agi, result$auto_int_ded), collapse = '; '))
  }

  message('test_auto_int_ded_stepped_phaseout: PASSED')
  invisible(TRUE)
}



test_schedule_1a_in_qbi_and_amt = function() {

  #----------------------------------------------------------------------------
  # The Schedule 1-A deductions (tips, overtime, senior, vehicle loan interest)
  # reduce taxable income, so they reduce the QBI deduction's taxable income
  # limit (IRC 199A(e)(1)) and the AMT starting point (Form 6251 line 1).
  # Vehicle interest left both bases when it moved below the line.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  law = federal_test_law(2026)

  # QBI: $50,000 of Schedule C profit, AGI 60,000, a 15,000 standard
  # deduction. The 20%-of-taxable-income limit binds, so $10,000 of vehicle
  # interest cuts the deduction by 2,000; so does $10,000 of tips
  qbi = tibble(id = 1:3, auto_int_ded = c(0, 10000, 0), tip_ded = c(0, 0, 10000)) %>%
    mutate(year = 2026, filing_status = 1, sole_prop = 50000, agi = 60000,
           std_ded = 15000, item_ded = 0, pe_ded = 0, ot_ded = 0, senior_ded = 0,
           part = 0, scorp = 0, farm = 0, div_pref = 0, kg_pref = 0,
           wagebill_sole_prop = 0, wagebill_part = 0, wagebill_scorp = 0, wagebill_farm = 0,
           sstb_sole_prop = 0, sstb_part = 0, sstb_scorp = 0, sstb_farm = 0) %>%
    left_join(law, by = c('year', 'filing_status')) %>%
    mutate(qbi_ded = calc_qbi_ded(., fill_missings = T)$qbi_ded)
  if (abs((qbi$qbi_ded[1] - qbi$qbi_ded[2]) - 2000) > 0.01 ||
      abs(qbi$qbi_ded[2] - qbi$qbi_ded[3]) > 0.01) {
    stop('QBI taxable income omits Schedule 1-A deductions: ',
         paste(sprintf('%.2f', qbi$qbi_ded), collapse = ' / '))
  }

  # AMT: a non-itemizer's AMT income is AGI less the Schedule 1-A deductions
  amt = tibble(id = 1:2, auto_int_ded = c(0, 10000)) %>%
    mutate(year = 2026, filing_status = 1, agi = 300000, std_ded = 15000,
           ded = 15000, itemizing = FALSE,
           # calc_amt() re-runs calc_tax() and drops these first
           liab_ord = 0, liab_pref = 0, liab_1250 = 0, liab_collect = 0, liab = 0) %>%
    left_join(law, by = c('year', 'filing_status')) %>%
    mutate(amt_gross_inc = calc_amt(., fill_missings = T)$amt_gross_inc)
  if (any(abs(amt$amt_gross_inc - (300000 - amt$auto_int_ded)) > 0.01)) {
    stop('AMT income omits vehicle loan interest: ',
         paste(sprintf('%.2f', amt$amt_gross_inc), collapse = ' / '))
  }

  message('test_schedule_1a_in_qbi_and_amt: PASSED')
  invisible(TRUE)
}

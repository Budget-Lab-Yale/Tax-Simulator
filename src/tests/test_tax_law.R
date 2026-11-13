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
  test_ui_exclusion_2020()
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

  tax_law_ids = c('default', 'baseline_2024', 'baseline_2024_tcja_ext', 'baseline_2024_pre_obbba',
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



test_ui_exclusion_2020 = function() {

  #----------------------------------------------------------------------------
  # In 2020 up to $10,200 of unemployment compensation was excluded from gross
  # income for returns with AGI (figured without the exclusion) under $150,000
  # (ARPA section 9042, IRC 85(c)). Applied per return in this model. Checks
  # the amount excluded, the income test, the per-return cap on a joint return,
  # that no other year excludes anything, and that taxable Social Security is
  # figured on income after the exclusion.
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  test_indexes = expand_grid(series = c('cpi', 'chained_cpi', 'awi'),
                             year   = 1970:2040) %>%
    mutate(growth = 0.02)

  tax_law = build_tax_law_from_id('default', years = c(2019, 2020, 2021),
                                  indexes = test_indexes) %>%
    select(year, filing_status, starts_with('agi.'), starts_with('ss.'),
           starts_with('char.'))

  cases = tribble(
    ~case,                                  ~year, ~filing_status, ~wages, ~ui,   ~gross_ss, ~expect_excl,
    '2020, $15,000 of benefits',             2020,  1,              20000,  15000, 0,         10200,
    '2020, $5,000 of benefits',              2020,  1,              20000,  5000,  0,         5000,
    '2020, AGI before exclusion $155,000',   2020,  1,              145000, 10000, 0,         0,
    '2020, AGI before exclusion $149,000',   2020,  1,              139000, 10000, 0,         10000,
    '2020, joint, $25,000 (per-return cap)', 2020,  2,              20000,  25000, 0,         10200,
    '2020, with Social Security',            2020,  1,              20000,  10000, 20000,     10000,
    '2019, $15,000 of benefits',             2019,  1,              20000,  15000, 0,         0,
    '2021, $15,000 of benefits',             2021,  1,              20000,  15000, 0,         0
  ) %>%
    mutate(r.bus_loss = 1) %>%
    left_join(tax_law, by = c('year', 'filing_status'))

  agi = calc_agi(cases, fill_missings = T)
  wrong_excl = cases$case[abs(agi$ui_excl - cases$expect_excl) > 1e-6]
  if (length(wrong_excl) > 0) {
    stop('UI exclusion: wrong amount excluded for ', paste(wrong_excl, collapse = '; '))
  }
  wrong_agi = cases$case[abs(agi$agi - (cases$wages + cases$ui - cases$expect_excl + agi$txbl_ss)) > 1e-6]
  if (length(wrong_agi) > 0) {
    stop('UI exclusion: AGI is not income less the exclusion for ', paste(wrong_agi, collapse = '; '))
  }

  # Taxable Social Security falls when the exclusion lowers income
  ss_case = cases %>% filter(case == '2020, with Social Security')
  no_excl = calc_agi(ss_case %>% mutate(agi.ui_excl_limit = 0), fill_missings = T)
  with_excl = agi[cases$case == '2020, with Social Security', ]
  if (!(with_excl$txbl_ss < no_excl$txbl_ss)) {
    stop('UI exclusion: taxable Social Security does not reflect the exclusion')
  }

  message('test_ui_exclusion_2020: PASSED')
  invisible(TRUE)
}


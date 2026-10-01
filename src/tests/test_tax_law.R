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

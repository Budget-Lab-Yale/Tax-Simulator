#---------------------------------------------------------------
# Characterization tests for do_salt_workaround_baseline()
# (src/calc/do_taxes.R), the federal approximation of pass-through
# entity taxes (PTET). They pin what the function does TODAY, so
# that the PTET rework (research/state_tax/plan.md section 3.2) can
# show its legacy mode is unchanged. They do not assert that the
# behavior is right.
#
# Defines functions only (this file is sourced by main.R's recursive
# walk; side effects at source time would break model startup).
#
# Run:
#   module load R/4.4.2-gfbf-2024a
#   Rscript research/state_tax/scripts/run_federal_tests.R        # fixture
#   Rscript research/state_tax/scripts/run_federal_tests.R full   # + microdata (sbatch)
#---------------------------------------------------------------


# Pinned full-sample totals, $ billions, from
# research/state_tax/ptet/results/workaround_national.csv (written by
# research/state_tax/ptet/01_quantify_workaround.R, 2026-10-26) on Tax-Data
# vintage 2026102315_rebuild: SALT moved off individual returns by entity
# type, and the federal individual income tax cost of the workaround
SALT_WORKAROUND_PINNED = tibble::tribble(
  ~year, ~moved_part_bn,     ~moved_scorp_bn,    ~fed_cost_bn,
  2017,  16.78342868246665,  19.288940372075896, 3.232073363643132,
  2024,  23.956494416787503, 27.858051147784003, 15.830879976810994
)
SALT_WORKAROUND_PINNED_VINTAGE = '2026102315_rebuild'
SALT_WORKAROUND_PINNED_RUNSCRIPT = 'tests/states_all_components'
SALT_WORKAROUND_REL_TOL = 1e-9


test_salt_workaround = function() {

  #----------------------------------------------------------------------------
  # Pins do_salt_workaround_baseline() on hand-built records under baseline
  # law. Each record has $1,000,000 of the function's income concept and
  # $100,000 of state income/sales tax unless noted, so the SALT attributed
  # to an entity type is $100,000 times its income share.
  #
  # Pinned behaviors:
  #   - participation is r.salt_workaround < 0.75 (baseline law), the same
  #     draw for both entity types
  #   - the amount moved is the attributed SALT, all or nothing
  #   - S-corporation SALT is capped at what partnership SALT left
  #   - a missing SSTB flag, non-positive entity income or non-positive total
  #     income moves nothing
  #   - the workaround is on in 2017, before the SALT cap, because the AMT
  #     exemption is finite
  #   - item.salt_workaround_allowed_part = 0 switches off partnerships only
  #   - salt_inc_sales falls and the active loss rises by the amount moved
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  law = federal_test_law(c(2017, 2024))

  cases = tribble(
    ~case,               ~year, ~wages,  ~part_active, ~scorp_active, ~sstb_part, ~sstb_scorp, ~r.salt_workaround, ~expect_part, ~expect_scorp,
    'part elects',       2024,  600000,  400000,       0,             0,          NA,          0.50,               40000,        0,
    'part above cutoff', 2024,  600000,  400000,       0,             0,          NA,          0.80,               0,            0,
    'scorp sstb elects', 2024,  500000,  0,            500000,        NA,         1,           0.10,               0,            50000,
    'both elect',        2024,  400000,  300000,       300000,        1,          0,           0.10,               30000,        30000,
    'scorp capped',      2024,  0,       600000,       1400000,       0,          0,           0.10,               30000,        70000,
    'sstb flag missing', 2024,  600000,  400000,       0,             NA,         NA,          0.10,               0,            0,
    'part loss',         2024,  1400000, -400000,      0,             0,          NA,          0.10,               0,            0,
    'income negative',   2024,  0,       -400000,      0,             0,          NA,          0.10,               0,            0,
    'on in 2017',        2017,  600000,  400000,       0,             0,          NA,          0.50,               40000,        0
  )

  # 'scorp capped': income is 2,000,000, shares 0.3 and 0.7, so both shares
  # are within the SALT total and the cap min(100,000 - 30,000, 70,000) binds
  # with equality. Kept as the pin of the formula's ordering.
  zero_vars = c('trad_contr_er1', 'trad_contr_er2', 'txbl_int', 'exempt_int',
                'div_ord', 'div_pref', 'state_ref', 'txbl_ira_dist',
                'gross_pens_dist', 'kg_st', 'kg_lt', 'other_gains', 'alimony',
                'sole_prop', 'farm', 'gross_ss', 'ui', 'other_inc',
                'part_passive', 'part_active_loss', 'part_passive_loss',
                'part_179', 'scorp_passive', 'scorp_active_loss',
                'scorp_passive_loss', 'scorp_179')

  inputs = cases %>%
    mutate(id = row_number(), filing_status = 1, salt_inc_sales = 100000) %>%
    bind_cols(as_tibble(setNames(rep(list(0), length(zero_vars)), zero_vars))) %>%
    left_join(law, by = c('year', 'filing_status'))

  if (!all(inputs$item.salt_workaround_allowed_sstb == 0.75,
           inputs$item.salt_workaround_allowed_non_sstb == 0.75,
           inputs$item.salt_workaround_allowed_part == 1)) {
    stop('Baseline workaround parameters are no longer 0.75 / 0.75 / 1; ',
         'this characterization test assumes them')
  }
  if (!is.infinite(inputs$item.salt_limit[inputs$year == 2017][1]) ||
      is.infinite(inputs$amt.exempt[inputs$year == 2017][1])) {
    stop('The 2017 case assumes no SALT cap and a finite AMT exemption')
  }

  result = do_salt_workaround_baseline(inputs)
  bad = which(abs(result$salt_workaround_part  - result$expect_part)  > 0.01 |
              abs(result$salt_workaround_scorp - result$expect_scorp) > 0.01 |
              is.na(result$salt_workaround_part) | is.na(result$salt_workaround_scorp))
  if (length(bad) > 0) {
    stop('do_salt_workaround_baseline() amounts changed: ',
         paste(sprintf('%s -> part %.2f, scorp %.2f', result$case[bad],
                       result$salt_workaround_part[bad],
                       result$salt_workaround_scorp[bad]), collapse = '; '))
  }

  # The amount leaves itemizable SALT and enters as an active loss
  moved = result$expect_part + result$expect_scorp
  if (any(abs(result$salt_inc_sales - (100000 - moved)) > 0.01) ||
      any(abs(result$part_active_loss  - result$expect_part)  > 0.01) ||
      any(abs(result$scorp_active_loss - result$expect_scorp) > 0.01)) {
    stop('do_salt_workaround_baseline() no longer shifts the amount from ',
         'salt_inc_sales to the active loss')
  }

  # The partnership switch turns off partnerships and leaves S corporations
  part_off = inputs %>%
    mutate(item.salt_workaround_allowed_part = 0) %>%
    do_salt_workaround_baseline()
  if (any(part_off$salt_workaround_part != 0) ||
      any(abs(part_off$salt_workaround_scorp - part_off$expect_scorp) > 0.01)) {
    stop('item.salt_workaround_allowed_part = 0 no longer zeroes partnerships only')
  }

  message('test_salt_workaround: PASSED')
  invisible(TRUE)
}



test_calc_ptet = function() {

  #----------------------------------------------------------------------------
  # calc_ptet() (src/calc/ptet.R). In mode 0 it must return exactly what
  # do_salt_workaround_baseline() returns. In mode 1 it moves nothing: the
  # inputs come back unchanged with zero amounts moved, because the microdata
  # are reported amounts (decision S48).
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  law = federal_test_law(c(2017, 2024))
  if (any(law$ptet.mode != PTET_MODE_LEGACY)) {
    stop('Baseline ptet.mode is no longer legacy; update this test and the ',
         'pinned totals together')
  }

  zero_vars = c('trad_contr_er1', 'trad_contr_er2', 'txbl_int', 'exempt_int',
                'div_ord', 'div_pref', 'state_ref', 'txbl_ira_dist',
                'gross_pens_dist', 'kg_st', 'kg_lt', 'other_gains', 'alimony',
                'sole_prop', 'farm', 'gross_ss', 'ui', 'other_inc',
                'part_passive', 'part_active_loss', 'part_passive_loss',
                'part_179', 'scorp_passive', 'scorp_active_loss',
                'scorp_passive_loss', 'scorp_179')
  inputs = tibble(year = c(2024, 2024, 2017), wages = 500000,
                  part_active = c(400000, 0, 400000),
                  scorp_active = c(0, 500000, 100000),
                  sstb_part = 0, sstb_scorp = 0, salt_inc_sales = 100000,
                  r.salt_workaround = 0.5) %>%
    mutate(id = row_number(), filing_status = 1) %>%
    bind_cols(as_tibble(setNames(rep(list(0), length(zero_vars)), zero_vars))) %>%
    left_join(law, by = c('year', 'filing_status'))

  # Mode 0 is the legacy function, exactly, and it does move something here
  legacy = calc_ptet(inputs)
  if (!identical(legacy, do_salt_workaround_baseline(inputs))) {
    stop('calc_ptet() in legacy mode differs from do_salt_workaround_baseline()')
  }
  if (all(legacy$salt_workaround_part + legacy$salt_workaround_scorp == 0)) {
    stop('The legacy cases no longer move anything; the test has lost its bite')
  }

  # Mode 1 changes nothing and reports zero moved
  reported_in = inputs %>% mutate(ptet.mode = PTET_MODE_REPORTED)
  reported    = calc_ptet(reported_in)
  if (!identical(reported %>% select(-salt_workaround_part, -salt_workaround_scorp),
                 reported_in) ||
      any(reported$salt_workaround_part != 0) ||
      any(reported$salt_workaround_scorp != 0)) {
    stop('calc_ptet() in mode 1 changed its input or moved an amount')
  }

  # A mode that is not defined, or that varies within the data, is an error
  for (bad in list(inputs %>% mutate(ptet.mode = 2),
                   inputs %>% mutate(ptet.mode = c(0, 1, 0)))) {
    if (!inherits(try(calc_ptet(bad), silent = T), 'try-error')) {
      stop('calc_ptet() accepted an invalid ptet.mode')
    }
  }

  message('test_calc_ptet: PASSED')
  invisible(TRUE)
}



salt_workaround_year_totals = function(year, scenario_info, tax_law, indexes,
                                       vat_price_offset, excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Full-sample totals of the workaround for one year: the baseline data-prep
  # chain of run_one_year() (src/sim/run.R) with the workaround and with it
  # zeroed, each through do_taxes().
  #
  # Parameters:
  #   - year (int)                : tax year
  #   - scenario_info (list)      : baseline scenario info
  #   - tax_law (df)              : baseline tax law
  #   - indexes (df)              : index series
  #   - vat_price_offset (df)     : VAT price offset series
  #   - excess_growth_offset (df) : excess growth offset series
  #
  # Returns: one-row tibble of year, moved_part_bn, moved_scorp_bn,
  #          fed_cost_bn, expanded_inc_gain_bn (weighted expanded income with
  #          the workaround less without) and max_expanded_inc_residual, the
  #          largest per-record departure of that difference from the amount
  #          absorbed by the Schedule E floor, and reported_matches_off,
  #          whether ptet.mode 1 gives exactly the federal liability, AGI and
  #          expanded income of the workaround switched off (df).
  #----------------------------------------------------------------------------

  finish_and_tax = function(tax_units) {
    tax_units %>%
      do_ss_cola(year, vat_price_offset) %>%
      do_capital_adjustment(year, vat_price_offset) %>%
      do_excess_growth(scenario_info, excess_growth_offset) %>%
      calc_kg_cpi_ratio(indexes, year) %>%
      do_taxes(baseline_pr_er = NULL,
               vars_1040      = fed_calc_vars(incl_payroll = F),
               vars_payroll   = return_vars$calc_pr)
  }

  base = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%
    mutate(year = year) %>%
    (\(d) bind_cols(d, build_random_numbers(d$id))) %>%
    mutate(filing_status_input = filing_status) %>%
    left_join(tax_law %>% distinct(year, filing.repeal_hoh), by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1, filing_status)) %>%
    left_join(tax_law, by = c('year', 'filing_status'))

  with_wa    = calc_ptet(base)
  without_wa = base %>%
    mutate(item.salt_workaround_allowed_part     = 0,
           item.salt_workaround_allowed_sstb     = 0,
           item.salt_workaround_allowed_non_sstb = 0) %>%
    do_salt_workaround_baseline()

  taxed_with    = finish_and_tax(with_wa)
  taxed_without = finish_and_tax(without_wa)
  stopifnot(identical(taxed_with$id, taxed_without$id))

  # Mode 1 must give the federal results of the workaround switched off
  taxed_reported = base %>%
    mutate(ptet.mode = PTET_MODE_REPORTED) %>%
    calc_ptet() %>%
    finish_and_tax()
  reported_matches_off = identical(taxed_reported$liab_iit, taxed_without$liab_iit) &&
                         identical(taxed_reported$agi, taxed_without$agi) &&
                         identical(taxed_reported$expanded_inc, taxed_without$expanded_inc)

  # Expanded income (do_taxes()) adds the amount moved back, but Schedule E
  # income floors combined partnership and S-corporation income at zero. So
  # the add-back is exact only while that combined income stays positive;
  # where the amount moved pushes it below zero, expanded income is higher
  # with the workaround by the part the floor absorbed
  moved      = with_wa$salt_workaround_part + with_wa$salt_workaround_scorp
  pass_thru  = taxed_without$part + taxed_without$scorp
  floor_gain = moved - (pmax(0, pass_thru) - pmax(0, pass_thru - moved))
  expanded_inc_diff = taxed_with$expanded_inc - taxed_without$expanded_inc

  tibble(
    year           = year,
    moved_part_bn  = sum(with_wa$weight * with_wa$salt_workaround_part)  / 1e9,
    moved_scorp_bn = sum(with_wa$weight * with_wa$salt_workaround_scorp) / 1e9,
    fed_cost_bn    = sum(taxed_with$weight *
                         (taxed_without$liab_iit - taxed_with$liab_iit)) / 1e9,
    expanded_inc_gain_bn      = sum(taxed_with$weight * expanded_inc_diff) / 1e9,
    max_expanded_inc_residual = max(abs(expanded_inc_diff - floor_gain)),
    reported_matches_off      = reported_matches_off
  )
}



test_salt_workaround_totals = function() {

  #----------------------------------------------------------------------------
  # Pins the full-sample amounts moved and the federal cost of the workaround
  # for TY2017 and TY2024 at the values 01_quantify_workaround.R reported, and
  # checks that expanded income with the workaround equals the amount without
  # it plus whatever the Schedule E floor absorbed (do_taxes() adds the amount
  # moved back; see salt_workaround_year_totals()). Requires `globals` built from the
  # tests/states_all_components runscript; reads full-sample microdata and
  # runs do_taxes() four times, so it needs a batch job (about 15 GB).
  #
  # Returns: TRUE invisibly if test passes (throws otherwise).
  #----------------------------------------------------------------------------

  scenario_info = get_scenario_info('baseline')
  if (basename(scenario_info$interface_paths$`Tax-Data`) != 'baseline' ||
      !grepl(SALT_WORKAROUND_PINNED_VINTAGE,
             scenario_info$interface_paths$`Tax-Data`, fixed = T)) {
    stop('test_salt_workaround_totals() is pinned to Tax-Data vintage ',
         SALT_WORKAROUND_PINNED_VINTAGE, '; got ',
         scenario_info$interface_paths$`Tax-Data`)
  }

  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`,
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`,
    years      = scenario_info$years
  )
  excess_growth_offset = get_excess_growth_offset(
    excess_growth = scenario_info$excess_growth,
    start_year    = scenario_info$excess_growth_start_year,
    years         = scenario_info$years
  )
  indexes = generate_indexes(
    macro_root           = scenario_info$interface_paths$`Macro-Projections`,
    vat_price_offset     = vat_price_offset,
    excess_growth_offset = excess_growth_offset
  )
  tax_law = build_tax_law(scenario_info, indexes)

  got = map_dfr(SALT_WORKAROUND_PINNED$year, salt_workaround_year_totals,
                scenario_info, tax_law, indexes, vat_price_offset,
                excess_growth_offset)
  print(as.data.frame(got), digits = 15)

  for (v in c('moved_part_bn', 'moved_scorp_bn', 'fed_cost_bn')) {
    rel = abs(got[[v]] / SALT_WORKAROUND_PINNED[[v]] - 1)
    if (any(rel > SALT_WORKAROUND_REL_TOL)) {
      stop('Workaround total ', v, ' moved from its pinned value: ',
           paste(sprintf('%d: %.9f vs %.9f', got$year, got[[v]],
                         SALT_WORKAROUND_PINNED[[v]]), collapse = '; '))
    }
  }
  if (any(got$max_expanded_inc_residual > 0.01)) {
    stop('Expanded income with the workaround is no longer the amount without ',
         'plus what the Schedule E floor absorbs: ',
         paste(sprintf('%d: %.4f', got$year, got$max_expanded_inc_residual),
               collapse = '; '))
  }

  if (!all(got$reported_matches_off)) {
    stop('ptet.mode 1 does not reproduce the federal results of the workaround ',
         'switched off: ', paste(got$year[!got$reported_matches_off], collapse = ', '))
  }

  message('test_salt_workaround_totals: PASSED')
  invisible(TRUE)
}

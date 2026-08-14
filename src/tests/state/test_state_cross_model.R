#-----------------------------------------------------------------------------
# test_state_cross_model.R
#
# Cross-model validation harness for the state income tax calculator:
# per-record comparison against NBER TAXSIM-35 (via usincometaxes, local
# WASM) and PolicyEngine US (via a python driver, see
# other/state_tax_research/cross_model/pe_state_tax.py).
#
# Design: record x state x year. Sampled PUF records are run counterfactually
# through EACH state's calculator and through the external model with that
# state code. State weights are NOT used -- this validates tax law encoding,
# not geography. Aggregate validation remains weights-blocked (plan §4).
#
# Year split (design of record, state_tax_implementation_plan.md Phase 5):
#   2017-2020 vs TAXSIM (its state law is actually coded through ~2020)
#   2021+     vs PolicyEngine (TAXSIM 2021+ params are inflated prior law)
#
# Defines functions only (sourced by main.R's recursive walk). Run via
# other/state_tax_research/cross_model/run_cross_model.R
#-----------------------------------------------------------------------------


# States by structural class (baseline configs under tax_law_state/baseline/)
cross_model_states = function() {
  list(
    broad  = c('AZ', 'CA', 'CO', 'CT', 'DE', 'GA', 'ID', 'IL', 'IN', 'KS',
               'KY', 'MD', 'MI', 'MN', 'NC', 'ND', 'NM', 'NY', 'OH', 'PA',
               'RI', 'SC', 'UT', 'VA', 'VT', 'WI', 'WV'),
    narrow = c('NH', 'TN'),           # interest/dividend (Hall-type) taxes
    excise = c('WA'),                 # LTCG excise + WFTC; not in TAXSIM
    stub   = c('AK', 'FL', 'NV', 'SD', 'TX', 'WY')  # no individual income tax
  )
}


cross_model_prepare_year = function(year, cache_dir, force = FALSE) {

  #----------------------------------------------------------------------------
  # Runs the federal pre-pass for one historical year and caches the result.
  # Mirrors the data-prep block of run_one_year() (src/sim/run.R) for the
  # baseline scenario, minus state/behavior/MTR/output steps. Requires
  # `globals` built from the tests/cross_model runscript.
  #
  # Parameters:
  #   - year (int)      : simulation year (2017-2024 usable window)
  #   - cache_dir (str) : directory for fed_calc_{year}.rds caches
  #   - force (bool)    : recompute even if a cache exists
  #
  # Returns: list of
  #   - tax_units (df) : post-federal-calculator tax units
  #   - indexes (df)   : price/wage index series (for build_state_tax_law)
  #----------------------------------------------------------------------------

  cache_file = file.path(cache_dir, paste0('fed_calc_', year, '.rds'))
  if (file.exists(cache_file) && !force) {
    message('Using cached federal pre-pass: ', cache_file)
    return(readRDS(cache_file))
  }

  scenario_info = get_scenario_info('baseline')

  # Price offsets and index series (baseline: VAT offset is a no-op series)
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

  # Load and process microdata (mirrors run_one_year(), src/sim/run.R)
  tax_units = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%
    filter(id %in% globals$sample_ids) %>%
    mutate(weight = weight / globals$pct_sample,
           year   = year) %>%
    bind_cols(globals$random_numbers) %>%
    mutate(filing_status_input = filing_status) %>%
    left_join(tax_law %>%
                distinct(year, filing.repeal_hoh),
              by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1,
                                   filing_status)) %>%
    left_join(tax_law, by = c('year', 'filing_status')) %>%
    do_salt_workaround_baseline() %>%
    do_ss_cola(year, vat_price_offset) %>%
    do_capital_adjustment(year, vat_price_offset) %>%
    do_excess_growth(scenario_info, excess_growth_offset) %>%
    calc_kg_cpi_ratio(indexes, year) %>%

    # Federal calculation (baseline: no employer-side payroll adjustment)
    do_taxes(baseline_pr_er = NULL,
             vars_1040      = fed_calc_vars(incl_payroll = F),
             vars_payroll   = return_vars$calc_pr)

  out = list(tax_units = tax_units, indexes = indexes)
  dir.create(cache_dir, recursive = T, showWarnings = F)
  saveRDS(out, cache_file)
  return(out)
}


cross_model_sample = function(tax_units_calc, n = 20000, seed = 76) {

  #----------------------------------------------------------------------------
  # Draws a stratified sample of calculated tax units for cross-model
  # comparison. Strata: filing status x AGI stratum (nonpositive, positive
  # deciles, top 1%) x has-dependents. The same sampled records are reused
  # for every state (record x state x year design).
  #
  # Parameters:
  #   - tax_units_calc (df) : post-federal-calculator tax units
  #   - n (int)             : target sample size
  #   - seed (int)          : RNG seed (default mirrors globals$random_seed)
  #
  # Returns: tibble of sampled records with stratum labels (df)
  #----------------------------------------------------------------------------

  set.seed(seed)

  # Dependent filers excluded in v1: TAXSIM mstat-8 semantics (dependent
  # standard deduction, kiddie tax) differ enough to swamp state signal
  eligible = tax_units_calc %>%
    filter(dep_status == 0)

  pos_breaks = eligible %>%
    filter(agi > 0) %>%
    pull(agi) %>%
    quantile(probs = c(seq(0.1, 0.9, 0.1), 0.99)) %>%
    unique()

  stratified = eligible %>%
    mutate(
      agi_stratum = cut(agi,
                        breaks = c(-Inf, 0, pos_breaks, Inf),
                        labels = F),
      stratum = paste(filing_status, agi_stratum, as.integer(n_dep > 0),
                      sep = '_')
    )

  # Allocate n proportionally with a floor of 25 records per stratum
  counts = stratified %>%
    count(stratum, name = 'n_stratum') %>%
    mutate(
      n_take = pmax(25, round(n * n_stratum / sum(n_stratum))),
      n_take = pmin(n_take, n_stratum)
    )

  stratified %>%
    left_join(counts, by = 'stratum') %>%
    group_split(stratum) %>%
    map(~ slice_sample(.x, n = .x$n_take[1])) %>%
    bind_rows() %>%
    select(-n_stratum, -n_take) %>%
    return()
}


cross_model_our_leg = function(sampled, states, year, state_law,
                               credit_tables) {

  #----------------------------------------------------------------------------
  # Runs sampled records through our state calculator for each state,
  # counterfactually assigning every record to every state. Exact law-join
  # pattern of test_state_calc.R's run_case()/smoke grid.
  #
  # Parameters:
  #   - sampled (df)       : sampled post-federal tax units
  #   - states (str[])     : 2-letter state codes (upper case)
  #   - year (int)         : tax year
  #   - state_law (df)     : output of build_state_tax_law()
  #   - credit_tables (df) : attr(state_law, 'credit_tables')
  #
  # Returns: long tibble id x state with liability + intermediates (df)
  #----------------------------------------------------------------------------

  map(states, function(st) {
    law_slice = state_law %>%
      filter(state == st, year == .env$year) %>%
      select(-state, -year)
    stopifnot('state law missing for state-year' = nrow(law_slice) > 0)

    sampled %>%
      left_join(law_slice, by = 'filing_status') %>%
      do_state_taxes(
        credit_tables = state_credit_tables_for_year(credit_tables, st, year)
      ) %>%
      bind_cols(sampled %>% select(id), .) %>%
      mutate(state = st, year = .env$year, .after = id) %>%
      return()
  }) %>%
    bind_rows() %>%
    return()
}


cross_model_taxsim_leg = function(sampled, states, year, chunk_size = 10000) {

  #----------------------------------------------------------------------------
  # Runs sampled records through NBER TAXSIM-35 (local WASM via
  # usincometaxes) once per state. Chunked to guard V8/WASM memory.
  #
  # Parameters:
  #   - sampled (df)    : sampled post-federal tax units
  #   - states (str[])  : 2-letter state codes
  #   - year (int)      : tax year
  #   - chunk_size (int): records per taxsim_calculate_taxes() call
  #
  # Returns: long tibble id x state with siitax, staxbc, and v30-v41 state
  #          intermediates (df)
  #----------------------------------------------------------------------------

  map(states, function(st) {
    xw = taxsim_crosswalk(sampled, state = st)
    chunks = split(xw, ceiling(seq_len(nrow(xw)) / chunk_size))

    map(chunks, function(chunk) {
      taxsim_calculate_taxes(chunk, return_all_information = T) %>%
        select(taxsimid, siitax, staxbc,
               any_of(c('v10_federal_agi', 'v25_eitc')),
               starts_with('v3'), any_of('v40_state_total_credits'),
               any_of('v41_state_bracket_rate')) %>%
        return()
    }) %>%
      bind_rows() %>%
      rename(id = taxsimid) %>%
      mutate(state = st, year = .env$year, .after = id) %>%
      return()
  }) %>%
    bind_rows() %>%
    return()
}


cross_model_pe_leg = function(sampled, states, year, venv_python, cache_dir) {

  #----------------------------------------------------------------------------
  # Runs sampled records through PolicyEngine US via the python driver
  # (other/state_tax_research/cross_model/pe_state_tax.py). Inputs use the
  # same concept set as the TAXSIM crosswalk so both external models see
  # identical records. MFS records are modeled as single filers in PE
  # (documented simplification).
  #
  # Parameters:
  #   - sampled (df)     : sampled post-federal tax units
  #   - states (str[])   : 2-letter state codes
  #   - year (int)       : tax year (>= 2021 canonical window)
  #   - venv_python (str): path to the policyengine venv's python
  #   - cache_dir (str)  : directory for the driver's i/o CSVs
  #
  # Returns: long tibble id x state with pe_state_income_tax, WA extras,
  #          and pe_version (df)
  #----------------------------------------------------------------------------

  if (is.null(venv_python)) {
    stop('PolicyEngine leg requires --pe-python (path to the venv python); ',
         'see other/state_tax_research/cross_model/README.md')
  }
  driver = './other/state_tax_research/cross_model/pe_state_tax.py'

  pe_input = map(states, function(st) {
    sampled %>%
      mutate(
        rec_id = id,
        state  = st,
        joint  = as.integer(filing_status == 2),
        page   = age1,
        sage   = age2,
        dep_ages = pmap_chr(
          list(dep_age1, dep_age2, dep_age3, n_dep),
          function(a1, a2, a3, nd) {
            paste(head(na.omit(c(a1, a2, a3)), nd), collapse = ';')
          }
        ),
        pwages = wages1, swages = wages2,
        psemp = se1, ssemp = se2,
        taxable_interest    = txbl_int,
        tax_exempt_interest = exempt_int,
        qualified_dividends = div_pref,
        ordinary_dividends  = div_ord,
        stcg = kg_st, ltcg = kg_lt,
        pension_income  = txbl_pens_dist + txbl_ira_dist,
        social_security = gross_ss,
        unemployment    = ui,
        rental          = rent - rent_loss,
        # Pass-through business income mirroring our AGI's Schedule E
        # concept (sch_e = part_scorp + net_rent + net_estate; net_rent is
        # passed separately as rental), less the SE portion already in
        # psemp/ssemp. other_gains (Form 4797) rides with other_inc --
        # ordinary income, no dedicated PE input. PE floors negative
        # miscellaneous_income (verified empirically), so net ordinary
        # losses route through pass_through, which accepts negatives
        pass_through = part_scorp - part_se +
          pmin(0, other_inc + other_gains),
        estate       = estate - estate_loss,
        misc_income  = pmax(0, other_inc + other_gains),
        real_estate_taxes  = salt_prop,
        mortgage_interest  = first_mort_int + second_mort_int,
        charitable_cash    = char_cash,
        charitable_noncash = char_noncash,
        childcare_expenses = care_exp
      ) %>%
      select(rec_id, state, joint, page, sage, n_dep, dep_ages,
             pwages, swages, psemp, ssemp,
             taxable_interest, tax_exempt_interest,
             qualified_dividends, ordinary_dividends, stcg, ltcg,
             pension_income, social_security, unemployment, rental,
             pass_through, estate, misc_income,
             real_estate_taxes, mortgage_interest,
             charitable_cash, charitable_noncash, childcare_expenses)
  }) %>%
    bind_rows()

  in_csv  = file.path(cache_dir, paste0('pe_in_', year, '.csv'))
  out_csv = file.path(cache_dir, paste0('pe_out_', year, '.csv'))
  write_csv(pe_input, in_csv)

  status = system2(venv_python, c(driver, in_csv, out_csv, year))
  if (status != 0 || !file.exists(out_csv)) {
    stop('PolicyEngine driver failed (exit ', status, '); see ', in_csv)
  }

  read_csv(out_csv, show_col_types = F) %>%
    rename(id = rec_id) %>%
    mutate(year = as.integer(year)) %>%
    return()
}


cross_model_compare = function(ours, theirs, model, known_diffs = NULL,
                               tolerances = c(15, 100)) {

  #----------------------------------------------------------------------------
  # Joins our per-record state results with an external model's and computes
  # per state-year cell summary statistics.
  #
  # Comparison variable by state class:
  #   - broad-IIT states : liab_st_iit
  #   - NH/TN            : liab_st_narrow_iit (vs TAXSIM siitax, 2017-2020)
  #   - stubs            : liab_st_iit (assert both models return 0)
  #   - WA               : liab_st_ltcg_excise (PolicyEngine only; excluded
  #                        vs TAXSIM via known-differences)
  #
  # Parameters:
  #   - ours (df)        : output of cross_model_our_leg()
  #   - theirs (df)      : output of cross_model_taxsim_leg() or _pe_leg(),
  #                        with external liability in column `ext_liab`
  #   - model (str)      : 'taxsim' or 'policyengine'
  #   - known_diffs (df) : known_differences.csv rows (NULL = none)
  #   - tolerances (dbl[]) : dollar tolerances for match rates
  #
  # Returns: list of
  #   - records (df) : per-record joined diffs
  #   - cells (df)   : per state-year summary
  #----------------------------------------------------------------------------

  classes = cross_model_states()

  records = ours %>%
    mutate(
      our_liab = case_when(
        state %in% classes$narrow ~ liab_st_narrow_iit,
        state %in% classes$excise ~ liab_st_ltcg_excise,
        TRUE                      ~ liab_st_iit
      )
    ) %>%
    inner_join(theirs, by = c('id', 'state', 'year')) %>%
    mutate(
      diff     = our_liab - ext_liab,
      abs_diff = abs(diff),
      model    = model
    )

  # Federal-alignment flag: TAXSIM cells inherit federal-side noise (its own
  # federal AGI/EITC and its blindness to tax-exempt interest) that says
  # nothing about our state law encoding. The "clean" subset conditions on
  # federal agreement to isolate state-law signal.
  if (model == 'taxsim' &&
      all(c('v10_federal_agi', 'v25_eitc', 'agi', 'eitc', 'exempt_int',
            'state_ref') %in% names(records))) {
    # state_ref is deliberately omitted from TAXSIM state-mode input
    # (see taxsim_crosswalk), so TAXSIM's federal AGI runs low by state_ref
    records = records %>%
      mutate(fed_aligned = abs(v10_federal_agi - (agi - state_ref)) <= 100 &
                           abs(v25_eitc - eitc) <= 15 &
                           exempt_int == 0)
  } else if (model == 'policyengine' &&
             all(c('pe_fed_agi', 'pe_fed_taxable', 'pe_fed_eitc', 'agi',
                   'txbl_inc', 'eitc', 'state_ref') %in% names(records))) {
    # PE computes its own full federal return; condition on agreement of
    # its federal AGI, taxable income (matters for fed-taxable-start
    # states), and EITC (piggyback credits). PE never sees state_ref
    records = records %>%
      mutate(fed_aligned = abs(pe_fed_agi - (agi - state_ref)) <= 100 &
                           abs(pe_fed_taxable - txbl_inc) <= 100 &
                           abs(pe_fed_eitc - eitc) <= 15)
  } else {
    records$fed_aligned = TRUE
  }

  # Apply known-differences exclusions for this model. An optional
  # `predicate` column narrows an exclusion to records matching an R
  # expression evaluated on the records frame (e.g. "agi > 250000")
  records$excluded = F
  if (!is.null(known_diffs) && nrow(known_diffs) > 0) {
    for (i in seq_len(nrow(known_diffs))) {
      kd = known_diffs[i, ]
      if (kd$action != 'exclude') next
      if (!(kd$model %in% c(model, 'both'))) next
      hit = (kd$state == 'ALL' | records$state == kd$state) &
        records$year >= kd$year_min & records$year <= kd$year_max
      if ('predicate' %in% names(kd) &&
          !is.na(kd$predicate) && nzchar(kd$predicate)) {
        hit = hit & with(records, eval(parse(text = kd$predicate)))
      }
      records$excluded = records$excluded | hit
    }
  }

  cells = records %>%
    filter(!excluded) %>%
    group_by(model, state, year) %>%
    summarise(
      n              = n(),
      match_15       = mean(abs_diff <= tolerances[1]),
      match_100      = mean(abs_diff <= tolerances[2]),
      share_both_zero = mean(our_liab == 0 & ext_liab == 0),
      mean_abs_diff   = mean(abs_diff),
      median_abs_diff = median(abs_diff),
      p90_abs_diff    = quantile(abs_diff, 0.9),
      p99_abs_diff    = quantile(abs_diff, 0.99),
      mean_signed     = mean(diff),
      n_clean         = sum(fed_aligned),
      match_15_clean  = mean(abs_diff[fed_aligned] <= tolerances[1]),
      match_100_clean = mean(abs_diff[fed_aligned] <= tolerances[2]),
      .groups = 'drop'
    ) %>%
    bind_rows(
      records %>%
        filter(excluded) %>%
        group_by(model, state, year) %>%
        summarise(n = n(), .groups = 'drop') %>%
        mutate(match_15 = NA_real_, match_100 = NA_real_)
    )

  # Breakdown by filing status and AGI stratum for weak cells
  weak = cells %>%
    filter(!is.na(match_100), match_100 < 0.95) %>%
    select(state, year)
  breakdowns = NULL
  if (nrow(weak) > 0 && all(c('filing_status', 'agi_stratum') %in%
                            names(records))) {
    breakdowns = records %>%
      inner_join(weak, by = c('state', 'year')) %>%
      group_by(model, state, year, filing_status, agi_stratum) %>%
      summarise(
        n         = n(),
        match_100 = mean(abs_diff <= tolerances[2]),
        median_abs_diff = median(abs_diff),
        .groups = 'drop'
      )
  }

  return(list(records = records, cells = cells, breakdowns = breakdowns))
}


cross_model_stage_diagnosis = function(records) {

  #----------------------------------------------------------------------------
  # Classifies each TAXSIM mismatch (|diff| > $15) by the first calculation
  # stage where the two models diverge, using TAXSIM's state intermediates
  # (v32-v40) against ours. staxbc is not used: TAXSIM leaves it zero for
  # some states (verified for IL).
  #
  # Parameters:
  #   - records (df) : per-record comparison from cross_model_compare()
  #
  # Returns: tibble of counts by model, state, year, stage (df)
  #----------------------------------------------------------------------------

  needed = c('v32_state_agi', 'v33_state_exemption_amount',
             'v34_state_std_deduction_amount', 'v35_state_itemized_deduction',
             'v36_state_taxable_income', 'v39_state_eitc',
             'v40_state_total_credits')
  if (!all(needed %in% names(records))) return(NULL)

  records %>%
    filter(abs_diff > 15) %>%
    mutate(
      stage = case_when(
        abs(v32_state_agi - st_agi) > 5                    ~ '1 state AGI',
        abs(v33_state_exemption_amount - st_exempt) > 5    ~ '2 exemptions',
        abs(v34_state_std_deduction_amount - st_std_ded) > 5 |
          abs(v35_state_itemized_deduction - st_item_ded) > 5 ~ '3 deductions',
        abs(v36_state_taxable_income - st_txbl_inc) > 5    ~ '4 taxable income',
        abs(v39_state_eitc - st_eitc) > 5                  ~ '5 state EITC',
        abs(v40_state_total_credits -
              (st_credits_nonref + st_credits_ref)) > 5    ~ '6 other credits',
        TRUE                                               ~ '7 rate/rounding'
      )
    ) %>%
    count(model, state, year, fed_aligned, stage) %>%
    return()
}


cross_model_load_known_diffs = function(path) {

  #----------------------------------------------------------------------------
  # Loads the known-differences list, returning an empty tibble with the
  # right schema if the file does not exist yet.
  #
  # Parameters:
  #   - path (str) : path to known_differences.csv
  #
  # Returns: tibble of known differences (df)
  #----------------------------------------------------------------------------

  if (!file.exists(path)) {
    return(tibble(
      state = character(), model = character(),
      year_min = integer(), year_max = integer(),
      category = character(), description = character(),
      expected_direction = character(), expected_magnitude = character(),
      action = character(), source = character()
    ))
  }
  read_csv(path, show_col_types = F) %>%
    return()
}


cross_model_report = function(out_dir, known_diffs_path = NULL) {

  #----------------------------------------------------------------------------
  # Writes per-state markdown reports from persisted harness output
  # (results/summary.csv, results/raw/*_stages.csv, known_differences.csv).
  # These reports are the documentation artifact behind flipping a state's
  # cross_model tracker column to done.
  #
  # Parameters:
  #   - out_dir (str)          : results directory
  #   - known_diffs_path (str) : known_differences.csv path (default: sibling
  #                              of out_dir)
  #
  # Returns: vector of report paths written (str[])
  #----------------------------------------------------------------------------

  if (is.null(known_diffs_path)) {
    known_diffs_path = file.path(dirname(out_dir), 'known_differences.csv')
  }
  known_diffs = cross_model_load_known_diffs(known_diffs_path)
  summary_df  = read_csv(file.path(out_dir, 'summary.csv'), show_col_types = F)

  stage_files = list.files(file.path(out_dir, 'raw'),
                           pattern = '_stages\\.csv$', full.names = T)
  stages = if (length(stage_files) > 0) {
    map(stage_files, read_csv, show_col_types = F) %>% bind_rows()
  } else NULL

  classes   = cross_model_states()
  class_of  = function(st) names(classes)[map_lgl(classes, ~ st %in% .x)]
  today     = format(Sys.Date())
  dir.create(file.path(out_dir, 'reports'), recursive = T, showWarnings = F)

  paths = map_chr(sort(unique(summary_df$state)), function(st) {

    cells = summary_df %>%
      filter(state == st) %>%
      arrange(year, model)
    canonical = cells %>%
      filter((model == 'taxsim' & year <= 2020) |
               (model == 'policyengine' & year >= 2021),
             !is.na(match_100))

    # Acceptance: clean rate where defined (TAXSIM), raw rate otherwise
    rate = canonical %>%
      mutate(rate = coalesce(match_100_clean, match_100)) %>%
      pull(rate)
    verdict = if (length(rate) == 0) 'NO CANONICAL CELLS YET'
              else if (min(rate) >= 0.95) 'PASS'
              else 'NEEDS REVIEW'

    lines = c(
      paste0('# Cross-model validation: ', st),
      '',
      paste0('Class: ', class_of(st), ' | Generated: ', today,
             ' | Verdict: **', verdict, '**'),
      '',
      'Acceptance: match@$100 >= 95% in every canonical-window cell',
      '(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where',
      'defined (federally aligned records; see README).',
      '',
      '## Cell summary',
      '',
      knitr::kable(
        cells %>%
          select(year, model, n, any_of(c('n_clean')), match_15, match_100,
                 any_of(c('match_15_clean', 'match_100_clean')),
                 share_both_zero, median_abs_diff, mean_signed) %>%
          mutate(across(where(is.numeric), ~ round(.x, 4)))
      ),
      ''
    )

    st_stages = if (!is.null(stages)) stages %>% filter(state == st) else NULL
    if (!is.null(st_stages) && nrow(st_stages) > 0) {
      lines = c(lines,
        '## Mismatch stage diagnosis (TAXSIM |diff| > $15)',
        '',
        knitr::kable(
          st_stages %>%
            group_by(year, fed_aligned, stage) %>%
            summarise(n = sum(n), .groups = 'drop') %>%
            arrange(year, desc(fed_aligned), stage)
        ),
        '')
    } else if (is.null(stages)) {
      lines = c(lines,
        '## Mismatch stage diagnosis',
        '',
        'Not available: results/raw/*_stages.csv not present on this machine',
        '(raw per-record output is not committed; regenerate with a full',
        'harness run).',
        '')
    }

    kd = known_diffs %>% filter(state %in% c(st, 'ALL'))
    if (nrow(kd) > 0) {
      lines = c(lines,
        '## Known differences applied',
        '',
        knitr::kable(kd %>% select(state, model, year_min, year_max,
                                   category, action, description)),
        '')
    }

    path = file.path(out_dir, 'reports', paste0(tolower(st), '.md'))
    writeLines(lines, path)
    path
  })

  return(paths)
}


cross_model_run = function(states, years, models, n = 20000, n_pe = 1500,
                           out_dir, cache_dir, venv_python = NULL,
                           chunk_size = 10000, force_prepare = FALSE) {

  #----------------------------------------------------------------------------
  # Orchestrates the cross-model validation matrix. Enforces the canonical
  # year split: TAXSIM for years <= 2020, PolicyEngine for years >= 2021.
  #
  # Parameters:
  #   - states (str[])   : 2-letter codes, upper case
  #   - years (int[])    : tax years (2017-2024)
  #   - models (str[])   : subset of c('taxsim', 'policyengine')
  #   - n (int)          : TAXSIM sample size per year
  #   - n_pe (int)       : PolicyEngine sample size per year (nested subset)
  #   - out_dir (str)    : results directory (summary.csv, raw/)
  #   - cache_dir (str)  : federal pre-pass cache directory
  #   - venv_python (str): path to policyengine venv python (PE leg only)
  #   - chunk_size (int) : TAXSIM WASM chunk size
  #   - force_prepare (bool) : recompute federal pre-pass caches
  #
  # Returns: tibble of all cell summaries (df); writes summary.csv and
  #          per-record raw files as a side effect
  #----------------------------------------------------------------------------

  dir.create(file.path(out_dir, 'raw'), recursive = T, showWarnings = F)
  known_diffs = cross_model_load_known_diffs(
    file.path(dirname(out_dir), 'known_differences.csv')
  )

  all_cells = list()

  for (yr in years) {

    yr_models = intersect(
      models,
      c(if (yr <= 2020) 'taxsim', if (yr >= 2021) 'policyengine')
    )
    if (length(yr_models) == 0) {
      message('Year ', yr, ': no canonical model in scope, skipping')
      next
    }

    # Federal pre-pass and sampling
    prep = cross_model_prepare_year(yr, cache_dir, force = force_prepare)
    sampled = cross_model_sample(prep$tax_units, n = n)
    message('Year ', yr, ': sampled ', nrow(sampled), ' records')

    # State law under production indexation
    state_law = build_state_tax_law(
      states  = states,
      years   = yr,
      indexes = prep$indexes
    )
    credit_tables = attr(state_law, 'credit_tables')

    # Our leg (all states at once)
    ours = cross_model_our_leg(sampled %>% mutate(year = yr), states, yr,
                               state_law, credit_tables)

    # Re-attach stratum labels and federal-alignment variables for breakdowns
    # and the clean-subset metrics, plus exposure covariates (age, SS,
    # dependents) so known-difference predicates can key on who a documented
    # external-model bug hits rather than on the outcome it produces
    ours = ours %>%
      left_join(sampled %>%
                  select(id, filing_status, agi_stratum, agi, txbl_inc, eitc,
                         exempt_int, state_ref, age1, age2, gross_ss, n_dep),
                by = 'id')

    for (model in yr_models) {

      if (model == 'taxsim') {
        theirs = cross_model_taxsim_leg(sampled, states, yr,
                                        chunk_size = chunk_size) %>%
          rename(ext_liab = siitax)
      } else {
        pe_sampled = sampled %>%
          group_by(stratum) %>%
          slice_head(n = ceiling(n_pe / n_distinct(sampled$stratum))) %>%
          ungroup()
        theirs = cross_model_pe_leg(pe_sampled, states, yr,
                                    venv_python = venv_python,
                                    cache_dir   = cache_dir) %>%
          rename(ext_liab = pe_state_income_tax)
      }

      comp = cross_model_compare(ours, theirs, model, known_diffs)

      # Persist raw per-record comparisons and collect cells
      write_csv(comp$records,
                file.path(out_dir, 'raw',
                          paste0(model, '_', yr, '.csv')))
      if (!is.null(comp$breakdowns)) {
        write_csv(comp$breakdowns,
                  file.path(out_dir, 'raw',
                            paste0(model, '_', yr, '_breakdowns.csv')))
      }
      stages = cross_model_stage_diagnosis(comp$records)
      if (!is.null(stages)) {
        write_csv(stages,
                  file.path(out_dir, 'raw',
                            paste0(model, '_', yr, '_stages.csv')))
      }

      # No-tax stubs must be zero in BOTH models
      stub_cells = comp$cells %>%
        filter(state %in% cross_model_states()$stub, !is.na(match_100))
      bad_stubs = stub_cells %>% filter(share_both_zero < 1)
      if (nrow(bad_stubs) > 0) {
        warning('No-tax stub state(s) with nonzero liability in ', yr, ': ',
                paste(bad_stubs$state, collapse = ' '), call. = F)
      }

      all_cells[[paste(model, yr)]] = comp$cells
    }
  }

  cells = bind_rows(all_cells) %>%
    arrange(state, year, model)

  # Merge into (or create) the committed summary file
  summary_path = file.path(out_dir, 'summary.csv')
  if (file.exists(summary_path)) {
    existing = read_csv(summary_path, show_col_types = F) %>%
      anti_join(cells, by = c('model', 'state', 'year'))
    cells = bind_rows(existing, cells) %>%
      arrange(state, year, model)
  }
  write_csv(cells, summary_path)

  return(cells)
}

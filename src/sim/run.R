#-----------------------------------------------
# run.R
#
# Contains functions to execute full simulation
#-----------------------------------------------



do_scenario = function(ID, baseline_mtrs) {
  
  #----------------------------------------------------------------------------
  # Executes full simulation for a given scenario. 
  # 
  # Parameters:
  #   - ID (str)           : scenario ID
  #   - baseline_mtrs (df) : tibble of baseline MTRs indexed by year/tax unit 
  #                          ID; NULL if this scenario is the baseline or if 
  #                          no MTR variables were specified 
  #
  # Returns: tibble of baseline MTRs if this scenario is the baseline (df); 
  #          NULL otherwise.
  #----------------------------------------------------------------------------
  
  if (globals$multicore != 'scenario') {
    print(paste0("Running scenario ", "'", ID, "'"))
  }
  
  # Get scenario info
  scenario_info = get_scenario_info(ID)


  #-----------------
  # Initialize data
  #-----------------

  # Calculate VAT price offset 
  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`, 
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`, 
    years      = scenario_info$years
  )
  
  # Calculate excess growth offset
  excess_growth_offset = get_excess_growth_offset(
    excess_growth = scenario_info$excess_growth, 
    start_year    = scenario_info$excess_growth_start_year, 
    years         = scenario_info$years
  )
  
  # Get price and wage index series
  indexes = generate_indexes(
    macro_root           = scenario_info$interface_paths$`Macro-Projections`, 
    vat_price_offset     = vat_price_offset, 
    excess_growth_offset = excess_growth_offset
  )
  
  # Build (and write) tax law
  tax_law = build_tax_law(scenario_info, indexes)


  #----------------
  # Run simulation
  #----------------

  uses_kg     = ID != 'baseline' && scenario_uses_kg_dynamics(scenario_info)
  uses_wealth = ID != 'baseline' && scenario_uses_wealth_dynamics(scenario_info)

  # sigma income conversion is built ON the kg bathtub (gain-state injection,
  # tau_eq wedge): hard-stop early rather than failing later in the
  # conventional pass. (SLURM mode reaches the same stop via the module's
  # own guard in do_conversion().)
  if (ID != 'baseline' && scenario_uses_sigma(scenario_info) && !uses_kg) {
    stop('Scenario "', ID, '" registers a conversion/ behavior module but ',
         'no kg_dynamics/ module. sigma requires kg_dynamics (pinned order ',
         'kg_dynamics -> conversion/sigma -> entity_shifting -> evasion).')
  }

  if (uses_kg || uses_wealth) {

    # Split-pass orchestration for the cohort-dynamics channels (kg and/or the
    # wealth bathtub), computed INDEPENDENTLY (4 combinations, not a 5th
    # branch). Chain:
    #   [frozen (kg)] -> static -> [kg bathtub] -> [conv-no-wealth + wealth
    #   bathtub] -> final conventional.
    #
    # The kg frozen pass needs only Tax-Data cells + tax law and writes the
    # mechanical state the static pass injects. The static pass is always the
    # measurement baseline (the clean law-only counterfactual). The wealth
    # bathtub's forcing ΔT⁰ is CONVENTIONAL (wealth-excluding), so it needs an
    # extra conv-no-wealth pass (behavior on, haircut off) BEFORE the pre-pass;
    # the final conventional pass then applies the haircut (and runs kg behavior
    # on the haircut frame when both channels are active). behavioral =
    # conventional − static.

    if (uses_kg) {
      run_frozen_pass(scenario_info, tax_law,
                      vat_price_offset     = vat_price_offset,
                      excess_growth_offset = excess_growth_offset)
    }

    static_mtrs = run_sim(scenario_info        = scenario_info,
                          tax_law              = tax_law,
                          baseline_mtrs        = baseline_mtrs,
                          indexes              = indexes,
                          vat_price_offset     = vat_price_offset,
                          excess_growth_offset = excess_growth_offset,
                          pass_type            = 'static')

    if (uses_kg) {
      run_bathtub_pass(scenario_info, tax_law,
                       vat_price_offset     = vat_price_offset,
                       excess_growth_offset = excess_growth_offset)
    }

    if (uses_wealth) {
      # Conv-no-wealth pass: produces ΔT⁰ ingredients + mtr_cap_bundle /
      # mtr_net_worth / economic_gross on the un-eroded conventional base.
      run_sim(scenario_info        = scenario_info,
              tax_law              = tax_law,
              baseline_mtrs        = baseline_mtrs,
              indexes              = indexes,
              vat_price_offset     = vat_price_offset,
              excess_growth_offset = excess_growth_offset,
              pass_type            = 'conventional_no_wealth',
              static_mtrs_all      = static_mtrs)

      run_wealth_bathtub_pass(scenario_info, tax_law,
                              vat_price_offset     = vat_price_offset,
                              excess_growth_offset = excess_growth_offset)
    }

    run_sim(scenario_info        = scenario_info,
            tax_law              = tax_law,
            baseline_mtrs        = baseline_mtrs,
            indexes              = indexes,
            vat_price_offset     = vat_price_offset,
            excess_growth_offset = excess_growth_offset,
            pass_type            = 'conventional',
            static_mtrs_all      = static_mtrs)

  } else {

    # Fused static/conventional simulation (current behavior for non-
    # kg_dynamics scenarios and baseline).
    static_mtrs = run_sim(scenario_info        = scenario_info,
                          tax_law              = tax_law,
                          baseline_mtrs        = baseline_mtrs,
                          indexes              = indexes,
                          vat_price_offset     = vat_price_offset,
                          excess_growth_offset = excess_growth_offset)
  }
  
  
  #--------------------
  # Do post-processing
  #--------------------
  
  if (ID != 'baseline') {

    # Formatted 1040 report
    build_1040_report(ID)

    # Revenue estimates
    calc_rev_est(ID)

    # Distribution tables
    build_distribution_tables(ID, baseline_id = 'baseline')

    # ETR-levels distribution supplemental (accrual income defs + stock-based
    # corporate-incidence conventions)
    build_distribution_etrs(ID)

    # Time burden tables
    build_timeburden_table(ID)

    # Horizontal equity
    build_horizontal_table(ID)

    # KG dynamics bathtub diagnostics (no-op for non-kg_dynamics scenarios)
    kg_dyn_build_summary(scenario_info)
  }
  
  # Return MTRs if running baseline
  if (ID == 'baseline') {
    return(static_mtrs)
  }
}



write_pass_outputs = function(output, root, totals_slot,
                              vat_price_offset, excess_growth_offset,
                              scenario_info) {

  #----------------------------------------------------------------------------
  # Writes one pass's (static or conventional) supplemental offsets, totals
  # CSVs, and receipts for a scenario. Shared by run_sim() (in-process) and
  # SLURM aggregate.R Phase 3a, which assemble `output` to the same shape. The
  # two passes differ only in `root`, `totals_slot`, and the offset source, so
  # factoring this here keeps the two call paths in lockstep (see the SLURM
  # sync table in CLAUDE.md).
  #
  # Parameters:
  #   - output (list)             : per-year results; each element carries
  #                                 $static_totals and/or $conventional_totals
  #   - root (str)                : pass output root (…/static or …/conventional)
  #   - totals_slot (str)         : 'static_totals' or 'conventional_totals' —
  #                                 which per-year totals list to aggregate
  #   - vat_price_offset (df)     : VAT price offset series, written to supplemental
  #   - excess_growth_offset (df) : excess-growth offset series, written to supplemental
  #   - scenario_info (list)      : scenario info (interface paths, excess_growth_all_rev)
  #
  # Returns: invisible NULL (writes files as a side effect)
  #----------------------------------------------------------------------------

  vat_price_offset %>%
    write_csv(file.path(root, 'supplemental', 'vat_price_offset.csv'))
  excess_growth_offset %>%
    write_csv(file.path(root, 'supplemental', 'excess_growth_offset.csv'))

  totals_pr = output %>%
    map(.f = ~ .x[[totals_slot]]$pr) %>%
    bind_rows() %>%
    write_csv(file.path(root, 'totals', 'payroll.csv'))

  totals_1040 = output %>%
    map(.f = ~ .x[[totals_slot]]$`1040`) %>%
    bind_rows() %>%
    write_csv(file.path(root, 'totals', '1040.csv'))

  output %>%
    map(.f = ~ .x[[totals_slot]]$`1040_by_agi`) %>%
    bind_rows() %>%
    write_csv(file.path(root, 'totals', '1040_by_agi.csv'))

  totals_estate = output %>%
    map(.f = ~ .x[[totals_slot]]$estate) %>%
    bind_rows() %>%
    write_csv(file.path(root, 'totals', 'estate.csv'))

  totals_wealth = output %>%
    map(.f = ~ .x[[totals_slot]]$wealth) %>%
    bind_rows() %>%
    write_csv(file.path(root, 'totals', 'wealth.csv'))

  totals_pr %>%
    left_join(totals_1040,   by = 'year') %>%
    left_join(totals_estate, by = 'year') %>%
    left_join(totals_wealth, by = 'year') %>%
    calc_receipts(
      scenario_root         = root,
      vat_root              = scenario_info$interface_paths$`Value-Added-Tax-Model`,
      other_root            = scenario_info$interface_paths$`Macro-Projections`,
      cost_recovery_root    = scenario_info$interface_paths$`Cost-Recovery-Simulator`,
      off_model_root        = scenario_info$interface_paths$`Off-Model-Estimates`,
      excess_growth_all_rev = scenario_info$excess_growth_all_rev
    )

  invisible(NULL)
}



run_sim = function(scenario_info, tax_law, baseline_mtrs,
                   indexes, vat_price_offset, excess_growth_offset,
                   pass_type = c('both', 'static', 'conventional',
                                 'conventional_no_wealth'),
                   static_mtrs_all = NULL) {

  #----------------------------------------------------------------------------
  # Runs simulation for all years of a scenario. Three modes (mirroring
  # run_one_year):
  #
  #   pass_type = 'both' (default): static + conventional in one pass per year,
  #     writes both totals files. Returns combined static MTRs.
  #
  #   pass_type = 'static': static-only across all years; writes static totals
  #     and receipts. Returns combined static MTRs.
  #
  #   pass_type = 'conventional': conventional-only across all years; writes
  #     conventional totals and receipts. Caller must supply static_mtrs_all
  #     (combined across years from a prior 'static' run); the loop filters
  #     per year and threads to run_one_year.
  #
  # Used by do_scenario() for the kg_dynamics 3-step (static, bathtub,
  # conventional) and by SLURM aggregate.R Phase 3a indirectly via per-year
  # workers.
  #
  # Parameters:
  #   - scenario_info (list)      : scenario info object; see get_scenario_info()
  #   - tax_law (df)              : tax law tibble; see build_tax_law()
  #   - baseline_mtrs             : tibble of baseline MTRs indexed by year/tax
  #                                 unit ID; NULL if this scenario is the
  #                                 baseline or if no MTR variables were specified
  #   - indexes (df)              : tibble of growth rates for various economic
  #                                 indexes ; see generate_indexes()
  #   - vat_price_offset (df)     : series of price level adjustment factors to
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess
  #                                 real GDP growth scenario
  #   - pass_type (str)           : 'both' (default), 'static', or 'conventional'
  #   - static_mtrs_all (df)      : combined static MTRs across years (required
  #                                 when pass_type='conventional' and the
  #                                 scenario has behavior modules that consume
  #                                 static_mtrs)
  #
  # Returns: tibble of marginal tax rates (only when pass_type %in% c('both',
  #          'static')); invisible NULL otherwise.
  #----------------------------------------------------------------------------

  pass_type = match.arg(pass_type)

  per_year = function(year) {
    smy = NULL
    if (!is.null(static_mtrs_all)) {
      smy = static_mtrs_all %>% filter(year == !!year)
    }
    run_one_year(year                 = year,
                 scenario_info        = scenario_info,
                 tax_law              = tax_law,
                 baseline_mtrs        = baseline_mtrs,
                 indexes              = indexes,
                 vat_price_offset     = vat_price_offset,
                 excess_growth_offset = excess_growth_offset,
                 pass_type            = pass_type,
                 static_mtrs_year     = smy)
  }

  # Run simulation for all years (parallel or sequential depending on settings)
  if (globals$multicore == 'year') {
    output = mclapply(X = scenario_info$years, FUN = per_year,
                      mc.cores = min(32, detectCores(logical = F)))
  } else {
    output = list()
    for (t in seq_along(scenario_info$years)) {
      output[[t]] = per_year(scenario_info$years[t])
    }
  }

  # --- Write static outputs (only when this run actually ran the static pass) ---
  if (pass_type %in% c('both', 'static')) {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'static'),
      totals_slot          = 'static_totals',
      vat_price_offset     = vat_price_offset,
      excess_growth_offset = excess_growth_offset,
      scenario_info        = scenario_info
    )
  }

  # --- Write conventional outputs (skip for baseline; only when conv pass ran) ---
  if (pass_type %in% c('both', 'conventional') && scenario_info$ID != 'baseline') {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'conventional'),
      totals_slot          = 'conventional_totals',
      vat_price_offset     = vat_price_offset,
      excess_growth_offset = excess_growth_offset,
      scenario_info        = scenario_info
    )
  }

  # Return combined MTRs (only meaningful when static pass ran in this call)
  if (pass_type %in% c('both', 'static')) {
    return(output %>% map(.f = ~ .x$mtrs) %>% bind_rows())
  }
  invisible(NULL)
}



kg_dyn_recompute_deemed_tax = function(taxed, input, baseline_pr_er,
                                       vars_1040, vars_payroll, estate_params) {

  #----------------------------------------------------------------------------
  # Expected income tax on mechanical deemed death gains, plus the Sec. 2053
  # estate-deduction reprice. Shared verbatim by the static and conventional
  # passes of run_one_year() (previously copy-pasted in both). Computes the
  # exact decedent/survivor copy-split expectation without row duplication:
  #   liab_deemed = m_household * [T(y + kg_deemed_full) - T(y)]
  # via a second full-frame do_taxes() recompute (the "dead leg") with the full
  # (Sec.121-net, post-avoidance) death gain added to kg_lt, both legs under
  # reform law (so rate reforms flow through automatically). The decedent's
  # deemed-realization tax is then applied as a deductible against the taxable
  # estate (the in-chain estate ran with ded = 0) and estate liabilities are
  # repriced; estate_distributable is unchanged by construction (the deduction
  # enters the base only).
  #
  # The recompute runs on the FULL frame (never a subset): calc functions index
  # globals$random_numbers positionally (e.g. the EITC pre-certification draw),
  # so subsetting rows breaks alignment. Non-holders have kg_deemed_full = 0 and
  # an exactly-zero delta. The dead-leg pass skips the estate calc
  # (calc_estate_flag = FALSE): only its liab_iit_net is read; estate is
  # repriced here separately. The caller folds liab_deemed into reported
  # liability AFTER the MTR block (MTRs anchor on the alive-leg liability).
  #
  # Parameters:
  #   - taxed (df)          : already-taxed alive-leg frame (carries liab_iit_net,
  #                           id, estate inputs); modified and returned
  #   - input (df)          : pre-tax input frame for this pass, same row order
  #                           (carries kg_lt, kg_deemed_full, m_household)
  #   - baseline_pr_er (df) : baseline employer payroll, passed to do_taxes()
  #   - vars_1040 (str[])   : 1040 return vars for do_taxes()
  #   - vars_payroll (str[]): payroll return vars for do_taxes()
  #   - estate_params       : frozen estate measurement params
  #
  # Returns: `taxed` with liab_deemed attached and ESTATE_OUTPUT_COLS repriced.
  #----------------------------------------------------------------------------

  dead_leg = input %>%
    mutate(kg_lt = kg_lt + kg_deemed_full) %>%
    do_taxes(baseline_pr_er   = baseline_pr_er,
             vars_1040        = vars_1040,
             vars_payroll     = vars_payroll,
             calc_estate_flag = FALSE,
             calc_wealth_flag = FALSE)   # only liab_iit_net read; wealth discarded
  stopifnot(identical(dead_leg$id, taxed$id))

  liab_deemed_cond  = dead_leg$liab_iit_net - taxed$liab_iit_net
  taxed$liab_deemed = input$m_household * liab_deemed_cond

  taxed$estate_income_tax_ded = pmax(liab_deemed_cond, 0)
  est = calc_estate(taxed, estate_params)
  taxed[, ESTATE_OUTPUT_COLS] = est[ESTATE_OUTPUT_COLS]

  taxed
}



run_one_year = function(year, scenario_info, tax_law, baseline_mtrs,
                        indexes, vat_price_offset, excess_growth_offset,
                        pass_type = c('both', 'static', 'conventional',
                                      'conventional_no_wealth'),
                        static_mtrs_year = NULL) {

  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation. Three modes:
  #
  #   pass_type = 'both' (default, used by main.R sequential and SLURM Phase 1):
  #     Loads tax_units, runs static pass + conventional pass in one process,
  #     writes both detail files. Returns mtrs + static_totals + conventional_totals.
  #
  #   pass_type = 'static' (SLURM Phase 2A): runs only the static pass,
  #     including MTR calc. Returns mtrs + static_totals.
  #
  #   pass_type = 'conventional' (SLURM Phase 2C): runs only the conventional
  #     pass. Caller must supply `static_mtrs_year` (typically read from the
  #     Phase 2A per-year .rds) so behavioral modules see correct static MTRs.
  #     Returns conventional_totals.
  #
  # Parameters:
  #   - year (int)                : year to run
  #   - scenario_info (list)      : scenario info object; see get_scenario_info()
  #   - tax_law (df)              : tax law tibble; see build_tax_law()
  #   - baseline_mtrs             : tibble of baseline MTRs indexed by year/tax
  #                                 unit ID; NULL if this scenario is the baseline
  #                                 or if no MTR variables were specified
  #   - indexes (df)              : tibble of growth rates for various economic
  #                                 indexes ; see generate_indexes()
  #   - vat_price_offset (df)     : series of price level adjustment factors to
  #                                 reflect introduction of a VAT
  #   - excess_growth_offset (df) : income adjustment factors reflecting excess
  #                                 real GDP growth scenario
  #   - pass_type (str)           : 'both', 'static', or 'conventional'
  #   - static_mtrs_year (df)     : pre-computed static MTRs (only required in
  #                                 pass_type='conventional' when has_behavior)
  #
  # Returns: list with subset of {mtrs, static_totals, conventional_totals}
  # depending on pass_type.
  #----------------------------------------------------------------------------

  pass_type = match.arg(pass_type)

  if (globals$multicore != 'year') {
    print(paste0('Running ', year, ' (', pass_type, ') for scenario ',
                 "'", scenario_info$ID, "'"))
  }



  #--------------------------------
  # Load and process tax unit data
  #--------------------------------

  # Read data
  tax_units = scenario_info$interface_paths$`Tax-Data` %>%
    read_microdata(year) %>%

    # Subset records if running with a sample of the full data
    filter(id %in% globals$sample_ids) %>%
    mutate(weight        = weight / globals$pct_sample,
           year          = year,
           decedent_flag = 0L) %>%

    # Assign random numbers (id-keyed: the per-year id universe varies, so a
    # positional bind would misalign draws and break on years with new ids)
    left_join(globals$random_numbers, by = 'id') %>%

    # Recode filing status if tax law departs from traditional options
    left_join(tax_law %>%
                distinct(year, filing.repeal_hoh),
              by = 'year') %>%
    mutate(filing_status = if_else(filing.repeal_hoh == 1 & filing_status == 4,
                                   1,
                                   filing_status)) %>%

    # Join tax law
    left_join(tax_law, by = c('year', 'filing_status')) %>%

    # Account for tax law changes manifesting as reporting changes
    do_salt_workaround_baseline() %>%

    # Adjust Social Security benefits for VAT-driven price level increase
    do_ss_cola(year, vat_price_offset) %>%

    # Adjust capital income for VAT-drive price level increase
    do_capital_adjustment(year, vat_price_offset) %>%

    # Adjust intensive-margin variables for excess real GDP growth
    do_excess_growth(scenario_info, excess_growth_offset) %>%

    # Compute CPI ratio for capital gains basis indexation
    calc_kg_cpi_ratio(indexes, year)

  # Estate tax setup. Liability itself is computed in-chain by do_taxes()
  # (per pass, so behavioral modules and pass-specific state reprice it);
  # here we (1) load the frozen measurement parameters into globals, where
  # every do_taxes() call -- including MTR-loop recomputes -- can see them,
  # and (2) compute the household death-event probability (the weights side,
  # incl. the donor-clone cluster cap), a population-level operation that
  # stays out of the per-record calculator chain. Wealth stays in raw
  # dollars: the VAT / excess-growth income adjustments don't apply to
  # balance-sheet stocks, so under those scenarios the estate base is
  # intentionally in pre-adjustment units.
  globals$estate_params <<- get_estate_params(scenario_info$interface_paths$`Tax-Data`)
  tax_units$estate_m = calc_estate_mortality(
    tax_units, globals$estate_params$cluster_death_weight_cap)

  # Materialize economic net worth (Sigma assets - Sigma debts, raw dollars,
  # no valuation discount) as a STORED column, computed once here so it can
  # serve three roles downstream: (1) the wealth-tax base read by calc_wealth();
  # (2) the +$1 bump target for the net_worth MTR (calc_mtrs operates on stored
  # columns, and net_worth is derived); (3) the isolation point the conventional
  # avoidance module overwrites with the avoided base. value.* stay intact, so
  # estate and capital income are unaffected. Like the estate base, wealth stays
  # in raw (non-VAT-adjusted) dollars by construction.
  tax_units %<>%
    mutate(net_worth = rowSums(across(all_of(WEALTH_ASSET_COLS), ~ replace_na(., 0))) -
                       rowSums(across(all_of(WEALTH_DEBT_COLS),  ~ replace_na(., 0))))


  #----------
  # Do taxes
  #----------

  # Read baseline payroll taxes
  baseline_pr_er = NULL
  if (scenario_info$ID != 'baseline') {
    baseline_pr_er = globals$baseline_root %>%
      file.path('baseline/static/detail', paste0(year, '.csv')) %>%
      fread() %>%
      tibble() %>%
      select(id, baseline1 = liab_fica_er1, baseline2 = liab_fica_er2)
  }

  # List calculated tax variables
  vars_1040 = return_vars %>%
    remove_by_name('calc_pr') %>%
    unlist() %>%
    set_names(NULL)


  # --- STATIC PASS ---
  static_totals    = NULL
  tax_units_static = NULL
  uses_kg_mech     = scenario_info$ID != 'baseline' &&
                     scenario_uses_kg_dynamics(scenario_info)
  uses_wealth      = scenario_info$ID != 'baseline' &&
                     scenario_uses_wealth_dynamics(scenario_info)
  uses_corp        = scenario_info$ID != 'baseline' &&
                     scenario_uses_corp_incidence(scenario_info)
  if (pass_type %in% c('both', 'static')) {

    # kg_dynamics scenarios: inject the mechanical (frozen-realization)
    # carryover/deemed quantities into records BEFORE tax calculation, so the
    # policy's mechanical content lands in static liabilities, static MTRs
    # (post-injection by design), and the distribution tables. The original
    # tax_units stays unmodified for the conventional pass, whose behavior
    # module applies the full bathtub state itself.
    static_input = tax_units
    if (uses_kg_mech) {
      static_input = kg_dyn_apply_mech_to_records(tax_units, scenario_info,
                                                  year)
    }

    # Use %>% (not %<>%) so original tax_units stays unmodified for conventional pass
    tax_units_static = static_input %>%
      do_taxes(baseline_pr_er = baseline_pr_er,
               vars_1040      = vars_1040,
               vars_payroll   = return_vars$calc_pr)

    # Expected tax on mechanical deemed death gains, preserving record-level
    # nonlinearity without splitting records:
    #   liab_deemed = m * [T(y + kg_deemed_full) - T(y)]
    # where the dead leg is a second recompute with the full (§121-net,
    # post-avoidance) death gain on the return, both legs under reform law
    # (so rate reforms flow through automatically). This is the exact
    # decedent/survivor copy-split expectation, computed with two full-frame
    # passes instead of row duplication. The recompute runs on the FULL
    # frame (never a subset): calc functions index globals$random_numbers
    # positionally (e.g. the EITC pre-certification draw), so subsetting
    # rows breaks alignment. Non-holders have kg_deemed_full = 0, hence
    # identical inputs and an exactly-zero delta. The main frame's kg_lt is
    # alive-leg (no deemed), so MTRs and tau are pure inter-vivos margins;
    # liab_deemed is folded into liab_iit_net AFTER the MTR block below.
    if (uses_kg_mech) {
      tax_units_static = tax_units_static %>% mutate(liab_deemed = 0)
      if (any(static_input$kg_deemed_full > 0)) {
        tax_units_static = kg_dyn_recompute_deemed_tax(
          taxed          = tax_units_static,
          input          = static_input,
          baseline_pr_er = baseline_pr_er,
          vars_1040      = vars_1040,
          vars_payroll   = return_vars$calc_pr,
          estate_params  = globals$estate_params)
      }
    }

    # Calculate static marginal tax rates
    static_mtrs_year = NULL
    if (!is.null(scenario_info$mtr_vars)) {

      # Same-frame expected estate liability (the get_estate_totals DSUE
      # blend) for the estate MTR; consumed by calc_mtrs only when 'estate'
      # is registered. NEVER the baseline's -- the delta must be measured on
      # the frame the recompute runs on.
      actual_liab_estate_static =
        tax_units_static$estate_p_dsue * tax_units_static$liab_estate_dsue +
        (1 - tax_units_static$estate_p_dsue) * tax_units_static$liab_estate_nodsue

      static_mtrs_year = scenario_info$mtr_vars %>%
        map2(.y = scenario_info$mtr_types,
             .f = ~ calc_mtrs(
               tax_units          = tax_units_static %>%
                                      select(-all_of(return_vars %>%
                                      unlist() %>%
                                      set_names(NULL))),
               actual_liab_iit    = tax_units_static$liab_iit_net,
               actual_liab_pr     = tax_units_static$liab_pr,
               actual_liab_wealth = tax_units_static$liab_wealth,
               actual_liab_estate = actual_liab_estate_static,
               actual_estate_p_dsue = tax_units_static$estate_p_dsue,
               # NULL, NOT the pass-level baseline_pr_er: tax_units_static is
               # a POST-do_taxes frame, so its wages already carry the er-
               # payroll rescale (see calc_mtrs parameter doc)
               baseline_pr_er     = NULL,
               var                = .x,
               pr                 = F,
               type               = .y
            )
        ) %>%
        bind_cols() %>%
        mutate(id   = tax_units_static$id,
               year = year) %>%
        relocate(id, year)

      # Add MTRs to static tax units dataframe
      tax_units_static %<>%
        left_join(static_mtrs_year %>%
                    select(-year),
                  by = 'id')

      # Switch-gated estate MTR for the kg death value (part (a) of the
      # estate-margins build): mtr_estate_ded = estate.income_tax_ded x
      # mtr_estate, derived per record while the law column is in the frame
      # -- one perturbation, two emitted columns. mtr_estate stays the raw
      # un-switched base rate (consumed by the wealth-avoidance estate
      # response); mtr_estate_ded is what the kg Bellman/tau_eq exposure
      # aggregator reads (the deductibility interaction must vanish when a
      # reform sets estate.income_tax_ded = 0 while mtr_estate itself is
      # unchanged).
      if ('estate' %in% scenario_info$mtr_vars) {
        tax_units_static %<>%
          mutate(mtr_estate_ded = estate.income_tax_ded * mtr_estate)
      }

      # Law-only kg_lt MTR for the planned-timing wedge: same reform law,
      # computed on the PRE-injection frame. The mech injection above adds
      # mechanically-routed carryover realizations to heir records' kg_lt,
      # which moves their bracket/NIIT/phaseout positions and drifts the
      # cell-average tau by single-digit bp even when the living-side
      # schedule is unchanged -- and the argmin planned-timing rule
      # (kg_dyn_build_planned_timing) would retime ~1-3% of a ~$600B bucket
      # against that composition drift, putting a +/-$2-4B sawtooth on
      # otherwise-smooth annual paths. Only the timing wedge consumes this
      # column; the Bellman keeps the post-injection tau above, where the
      # income effect is real signal. Full-frame recompute (never a subset)
      # for the same positional-random_numbers reason as the deemed dead
      # leg. Cost: two extra full-frame passes per kg scenario-year,
      # accepted for unconditional simplicity (no law-identity gating).
      if (uses_kg_mech) {
        tax_units_raw = tax_units %>%
          do_taxes(baseline_pr_er   = baseline_pr_er,
                   vars_1040        = vars_1040,
                   vars_payroll     = return_vars$calc_pr,
                   calc_estate_flag = FALSE,    # only liab_iit_net/liab_pr read for the law-only MTR
                   calc_wealth_flag = FALSE)
        stopifnot(identical(tax_units_raw$id, tax_units_static$id))
        tax_units_static$mtr_kg_lt_lawonly = calc_mtrs(
          tax_units       = tax_units_raw %>%
                              select(-all_of(return_vars %>%
                              unlist() %>%
                              set_names(NULL))),
          actual_liab_iit = tax_units_raw$liab_iit_net,
          actual_liab_pr  = tax_units_raw$liab_pr,
          # NULL: tax_units_raw is a POST-do_taxes frame (wages already
          # rescaled once; see calc_mtrs parameter doc)
          baseline_pr_er  = NULL,
          var             = 'kg_lt',
          pr              = F,
          type            = 'nextdollar'  # kg_dynamics tau is nextdollar-only
        )$mtr_kg_lt
      }

      # Guaranteed mtr_net_worth for wealth-active kg scenarios: the kg
      # bathtub's wealth-carry aggregator (kg_dyn_aggregate_cell_carry)
      # prices deferral off the record product mtr_net_worth * mtr_kg_lt
      # read from THIS static detail. When the runscript already registers
      # net_worth in mtr_vars (e.g. top_tax dials), the generic loop above
      # wrote it and this branch is skipped — those static legs stay
      # byte-identical. The wealth-law gate (any nonzero wealth.rates* in
      # any year) keeps the detail schema stable across phase-in years.
      if (uses_kg_mech && kg_dyn_wealth_law_active(tax_law) &&
          !('net_worth' %in% scenario_info$mtr_vars)) {
        tax_units_static$mtr_net_worth = calc_mtrs(
          # Drop the just-joined mtr_* columns too: this recovers exactly
          # the frame the generic loop above ran on, so the fallback column
          # matches a runscript-registered mtr_net_worth bit-for-bit.
          tax_units          = tax_units_static %>%
                                 select(-all_of(return_vars %>%
                                 unlist() %>%
                                 set_names(NULL)),
                                 -starts_with('mtr_')),
          actual_liab_iit    = tax_units_static$liab_iit_net,
          actual_liab_pr     = tax_units_static$liab_pr,
          actual_liab_wealth = tax_units_static$liab_wealth,
          # NULL, NOT the pass-level baseline_pr_er: tax_units_static is a
          # POST-do_taxes frame, so its wages already carry the er-payroll
          # rescale (double-rescale trap; see calc_mtrs parameter doc).
          # Deliberately NOT copied from the conv-no-wealth block below,
          # whose frame is PRE-do_taxes and correctly threads baseline_pr_er.
          baseline_pr_er     = NULL,
          var                = 'net_worth',
          pr                 = F,
          type               = 'nextdollar'
        )$mtr_net_worth
      }

      # Guaranteed mtr_estate / mtr_estate_ded for kg scenarios: the kg
      # bathtub's estate-exposure aggregator (kg_dyn_aggregate_cell_estate)
      # prices the death value's estate offset off mtr_estate_ded read from
      # THIS static detail. Same guarantee pattern as mtr_net_worth above,
      # but with NO law gate -- estate law is always active. When the
      # runscript registers 'estate' in mtr_vars the generic loop already
      # wrote both columns (bit-identical to this path) and this branch is
      # skipped. NOTE the fallback exists only on the SCENARIO leg: the
      # BASELINE pass cannot know a kg scenario will consume its detail, so
      # baseline rows of kg/wealth runscripts must register 'estate' in
      # mtr_vars (read_mtr in kg_dynamics.R hard-stops with that message).
      if (uses_kg_mech && !('estate' %in% scenario_info$mtr_vars)) {
        tax_units_static$mtr_estate = calc_mtrs(
          tax_units          = tax_units_static %>%
                                 select(-all_of(return_vars %>%
                                 unlist() %>%
                                 set_names(NULL)),
                                 -starts_with('mtr_')),
          actual_liab_iit    = tax_units_static$liab_iit_net,
          actual_liab_pr     = tax_units_static$liab_pr,
          actual_liab_wealth = tax_units_static$liab_wealth,
          actual_liab_estate = actual_liab_estate_static,
          actual_estate_p_dsue = tax_units_static$estate_p_dsue,
          # NULL: POST-do_taxes frame (see the net_worth fallback above)
          baseline_pr_er     = NULL,
          var                = 'estate',
          pr                 = F,
          type               = 'nextdollar'
        )$mtr_estate
        tax_units_static %<>%
          mutate(mtr_estate_ded = estate.income_tax_ded * mtr_estate)
      }
    }

    # Fold the expected deemed tax into reported liability (after MTRs,
    # which anchor on the alive-leg liability). Receipts are built from the
    # pmt_* payment-timing variables, not liab_iit_net, so fold there too:
    # deemed tax is a final-return capital gains bill, i.e. nonwithheld
    # income tax paid at filing (pmt_iit itself is dropped by remit_taxes).
    if (uses_kg_mech) {
      tax_units_static %<>%
        mutate(liab_iit_net        = liab_iit_net        + liab_deemed,
               liab_iit            = liab_iit            + liab_deemed,
               pmt_iit_nonwithheld = pmt_iit_nonwithheld + liab_deemed)
    }

    # Write static detail (kg_dynamics mechanical columns included when
    # present: kg_lockin, kg_deemed, liab_deemed, estate_income_tax_ded)
    tax_units_static %>%
      select(all_of(globals$detail_vars), starts_with('mtr_'),
             any_of(c('kg_lockin', 'kg_deemed', 'liab_deemed',
                      'estate_income_tax_ded'))) %>%
      write_csv(file.path(scenario_info$output_path, 'static', 'detail',
                          paste0(year, '.csv')))

    # Get static totals
    static_totals = list(pr            = get_pr_totals(tax_units_static, year),
                          `1040`        = get_1040_totals(tax_units_static, year),
                          `1040_by_agi` = get_1040_totals(tax_units_static, year, T),
                          estate        = get_estate_totals(tax_units_static, year),
                          wealth        = get_wealth_totals(tax_units_static, year))
  }


  # --- CONVENTIONAL PASS (and the wealth conv-no-wealth pre-pass) ---
  has_behavior        = length(scenario_info$behavior_modules) > 0
  conventional_totals = NULL

  if (pass_type %in% c('both', 'conventional', 'conventional_no_wealth')) {

    is_convnw     = pass_type == 'conventional_no_wealth'
    # The final conventional pass applies the wealth haircut; the conv-no-wealth
    # pass deliberately does NOT (it measures ΔT⁰ / mtr_cap_bundle on the
    # un-eroded base, the frame independent of the deficit).
    apply_haircut = uses_wealth && !is_convnw
    conv_root     = if (is_convnw) {
                      file.path(scenario_info$output_path, 'conventional_no_wealth')
                    } else {
                      file.path(scenario_info$output_path, 'conventional')
                    }
    conv_detail_path = file.path(conv_root, 'detail', paste0(year, '.csv'))

    # The full do_taxes path runs whenever there is a behavior module OR the
    # wealth channel OR the corporate channel is active: the conv-no-wealth
    # pass needs liabilities + mtr_cap_bundle, the final conv pass needs the
    # haircut applied, and a corporate scenario needs its shocked frame taxed.
    # Only a plain no-behavior, no-wealth, no-corp scenario takes the
    # copy-static shortcut.
    if (has_behavior || uses_wealth || uses_corp) {

      # On-model corporate incidence: a fixed step at the head of EVERY
      # conventional-side pass (incl. conv-no-wealth), BEFORE the wealth
      # haircut and the behavior modules, so the kg/wealth machinery runs on
      # the shocked frame (corp_incidence.R; FORMAL_MODEL section 7). Scales
      # the D16 external-income lines (accumulating the analytic corp_dY_exog
      # the wealth bathtub forcing consumes), marks down exposed value.*
      # stocks and recomputes net_worth (so calc_estate / calc_wealth reprice),
      # and adjusts kg flows in non-kg runs (kg runs route gains through the
      # bathtub state debit + the post-behavior phi term instead). Static side
      # never sees this (D5).
      conv_base = tax_units
      if (uses_corp) {
        corp_check_run_compat(scenario_info, vat_price_offset,
                              excess_growth_offset)
        conv_base = corp_apply_to_records(
          tax_units          = conv_base,
          paths              = corp_get_paths(scenario_info),
          year               = year,
          kg_dynamics_active = uses_kg_mech)

        # Conservation diagnostic (WARN-level reconciliation REPORT; the
        # per-line testable content is analytic-vs-realized, measured here by
        # differencing the pre/post frames). Final conventional pass only.
        if (!is_convnw) {
          corp_write_conservation_diag(
            pre = tax_units, post = conv_base,
            paths = corp_get_paths(scenario_info),
            year = year, conv_root = conv_root)
        }
      }

      # Mechanical wealth haircut: a fixed conventional-pass step BEFORE the
      # behavior modules / do_taxes. Drains each record's (age x net-worth-
      # percentile) cell deficit out of wealth (value.* / capital flows / basis)
      # and recomputes net_worth, so calc_estate sees a smaller estate base and
      # calc_wealth reprices liab_wealth on the eroded stock. Final conv only.
      # Ranking/binning uses the RAW pre-corp net worth (tax_units, row-aligned
      # to conv_base): the pre-pass cutoffs were computed on net_worth_raw, and
      # the corporate markdown must not shift records across cells.
      if (apply_haircut) {
        wealth_state = read_cohort_state(scenario_info, 'wealth_dynamics_state', year)
        conv_base    = wealth_dyn_apply_to_records(conv_base, wealth_state,
                                                   rank_value = tax_units$net_worth)
      }

      # Behavioral feedback (identity passthrough when no behavior module). When
      # both channels are active, kg runs on the post-haircut frame.
      if (has_behavior) {
        conv_input = conv_base %>%
          do_behavioral_feedback(behavior_modules = scenario_info$behavior_modules,
                                 baseline_mtrs    = baseline_mtrs,
                                 static_mtrs      = static_mtrs_year,
                                 scenario_info    = scenario_info,
                                 indexes          = indexes)
      } else {
        conv_input = conv_base
      }

      # Corporate D18 quantity margin for kg_dynamics runs: buyback-forced
      # sale volume tracks after-tax payouts, applied AFTER
      # kg_dyn_apply_to_records (the realization rule knows MTRs and
      # mortality, not payout policy). Mutually exclusive with the record
      # applier's non-kg kg block (skipped there via kg_dynamics_active);
      # the price margin rides the bathtub gain-state debit, never this step.
      if (uses_corp && uses_kg_mech) {
        conv_input = corp_apply_kg_quantity_to_records(
          conv_input, corp_get_paths(scenario_info), year)
      }

      tax_units_conv = conv_input %>%
        do_taxes(baseline_pr_er = baseline_pr_er,
                 vars_1040      = vars_1040,
                 vars_payroll   = return_vars$calc_pr)

      # kg_dynamics: expected tax on deemed death gains via the same two-leg
      # full-frame recompute as the static pass (see comment there); folded
      # into liab_iit_net after the MTR block below
      conv_liab_deemed = NULL
      if (uses_kg_mech && 'kg_deemed_full' %in% names(conv_input) &&
          any(conv_input$kg_deemed_full > 0)) {
        tax_units_conv = kg_dyn_recompute_deemed_tax(
          taxed          = tax_units_conv,
          input          = conv_input,
          baseline_pr_er = baseline_pr_er,
          vars_1040      = vars_1040,
          vars_payroll   = return_vars$calc_pr,
          estate_params  = globals$estate_params)
        conv_liab_deemed = tax_units_conv$liab_deemed
      }

      # Wealth bathtub forcing ingredients (conv-no-wealth pass only): the
      # composition-weighted capital-income bundle MTR + capital total, the
      # marginal wealth-tax rate, and gross assets -- all on this un-eroded
      # frame and BEFORE the deemed fold (so tau is a pure inter-vivos margin,
      # mirroring mtr_kg_lt_lawonly). The wealth pre-pass reads these from this
      # pass's detail.
      if (is_convnw) {
        bundle = calc_cap_bundle_mtr(
          tax_units       = conv_input,
          actual_liab_iit = tax_units_conv$liab_iit_net,
          baseline_pr_er  = baseline_pr_er,
          vars_1040       = vars_1040,
          vars_payroll    = return_vars$calc_pr)
        tax_units_conv$mtr_cap_bundle = bundle$mtr_cap_bundle
        tax_units_conv$cap_bundle_F   = bundle$cap_bundle_F
        tax_units_conv$economic_gross = wealth_dyn_economic_gross(conv_input)
        # Raw (pre-behavior) economic net worth, the cell ranking + denominator
        # variable. A net_worth-overwriting behavior module (e.g. wealth
        # avoidance) mutates conv_input$net_worth on this frame, but the applier
        # ranks on the RAW pre-behavior net_worth (it runs before behavior), so
        # the pre-pass must rank on the same raw stock or cells/conservation
        # break. tax_units is the raw frame, row-aligned to tax_units_conv.
        stopifnot(identical(tax_units_conv$id, tax_units$id))
        tax_units_conv$net_worth_raw  = tax_units$net_worth
        tax_units_conv$mtr_net_worth  = calc_mtrs(
          tax_units          = conv_input %>%
                                 select(-any_of(return_vars %>%
                                 unlist() %>%
                                 set_names(NULL))),
          actual_liab_iit    = tax_units_conv$liab_iit_net,
          actual_liab_pr     = tax_units_conv$liab_pr,
          actual_liab_wealth = tax_units_conv$liab_wealth,
          # conv_input is the PRE-do_taxes frame (un-rescaled wages), so the
          # recompute must apply the same rescale as the actuals run -- thread
          # baseline_pr_er (2026-07-09 fix; see calc_mtrs parameter doc)
          baseline_pr_er     = baseline_pr_er,
          var                = 'net_worth',
          pr                 = F,
          type               = 'nextdollar')$mtr_net_worth
      }

      # Calculate conventional marginal tax rates (skip on the conv-no-wealth
      # pass -- its detail is read only by the wealth pre-pass, which needs only
      # the mtr_cap_bundle / mtr_net_worth computed above)
      if (!is.null(scenario_info$mtr_vars) && !is_convnw) {

        # Same-frame expected estate liability (DSUE blend) for the estate
        # MTR -- see the static-pass comment
        actual_liab_estate_conv =
          tax_units_conv$estate_p_dsue * tax_units_conv$liab_estate_dsue +
          (1 - tax_units_conv$estate_p_dsue) * tax_units_conv$liab_estate_nodsue

        conv_mtrs = scenario_info$mtr_vars %>%
          map2(.y = scenario_info$mtr_types,
               .f = ~ calc_mtrs(
                 tax_units          = tax_units_conv %>%
                                        select(-all_of(return_vars %>%
                                        unlist() %>%
                                        set_names(NULL))),
                 actual_liab_iit    = tax_units_conv$liab_iit_net,
                 actual_liab_pr     = tax_units_conv$liab_pr,
                 actual_liab_wealth = tax_units_conv$liab_wealth,
                 actual_liab_estate = actual_liab_estate_conv,
                 actual_estate_p_dsue = tax_units_conv$estate_p_dsue,
                 # NULL, NOT baseline_pr_er: tax_units_conv is a POST-do_taxes
                 # frame (wages already rescaled; see calc_mtrs parameter doc).
                 # The convnw mtr_net_worth call above differs deliberately:
                 # it passes the PRE-do_taxes conv_input, so it must thread
                 # baseline_pr_er.
                 baseline_pr_er     = NULL,
                 var                = .x,
                 pr                 = F,
                 type               = .y
              )
          ) %>%
          bind_cols() %>%
          mutate(id   = tax_units_conv$id,
                 year = year) %>%
          relocate(id, year)

        tax_units_conv %<>%
          left_join(conv_mtrs %>%
                      select(-year),
                    by = 'id')

        # Switch-gated estate MTR (see the static-pass comment)
        if ('estate' %in% scenario_info$mtr_vars) {
          tax_units_conv %<>%
            mutate(mtr_estate_ded = estate.income_tax_ded * mtr_estate)
        }
      }

      # Fold the expected deemed tax into reported liability (after MTRs,
      # which anchor on the alive-leg liability), including the pmt_*
      # payment-timing variables receipts are built from (nonwithheld
      # income tax paid at filing, like any final-return gains bill)
      if (!is.null(conv_liab_deemed)) {
        tax_units_conv %<>%
          mutate(liab_deemed         = conv_liab_deemed,
                 liab_iit_net        = liab_iit_net        + liab_deemed,
                 liab_iit            = liab_iit            + liab_deemed,
                 pmt_iit_nonwithheld = pmt_iit_nonwithheld + liab_deemed)
      }

      # Write detail to this pass's root (conv-no-wealth detail lives in its own
      # tree so it never clobbers the final conventional detail). The wealth
      # forcing/diagnostic columns ride any_of() -- present only on the relevant
      # pass -- so dormancy is preserved byte-for-byte for non-wealth scenarios.
      dir.create(file.path(conv_root, 'detail'), recursive = TRUE,
                 showWarnings = FALSE)
      tax_units_conv %>%
        select(all_of(globals$detail_vars), starts_with('mtr_'),
               any_of(c('kg_lockin', 'kg_deemed', 'liab_deemed',
                        'estate_income_tax_ded', 'estate_concealed_frac',
                        'economic_gross', 'cap_bundle_F',
                        'net_worth_raw', 'nw_pctile', 'D_alloc', 'wealth_haircut',
                        'corp_dY_exog', 'corp_markdown', 'corp_flow_factor'))) %>%
        write_csv(conv_detail_path)

      # Conventional totals (skip the conv-no-wealth pass -- intermediate, no
      # totals/receipts; run_sim does not aggregate it either)
      if (!is_convnw) {
        conventional_totals = list(pr            = get_pr_totals(tax_units_conv, year),
                                    `1040`        = get_1040_totals(tax_units_conv, year),
                                    `1040_by_agi` = get_1040_totals(tax_units_conv, year, T),
                                    estate        = get_estate_totals(tax_units_conv, year),
                                    wealth        = get_wealth_totals(tax_units_conv, year))
      }

    } else if (scenario_info$ID != 'baseline') {

      # No behavior (and no wealth channel): copy static detail to conventional
      # output. In 'both' mode we have tax_units_static in memory; in
      # 'conventional' mode we copy the already-written static csv directly.
      conv_path = conv_detail_path
      if (!is.null(tax_units_static)) {
        tax_units_static %>%
          select(all_of(globals$detail_vars), starts_with('mtr_')) %>%
          write_csv(conv_path)
      } else {
        static_path = file.path(scenario_info$output_path, 'static', 'detail',
                                paste0(year, '.csv'))
        file.copy(static_path, conv_path, overwrite = TRUE)
      }

      # In 'both' mode static_totals is in scope; in 'conventional' mode the
      # caller (worker) is responsible for substituting the static_totals from
      # Phase 2A's per-year .rds. We return NULL here.
      conventional_totals = static_totals
    }
  }

  # Return required data
  return(list(mtrs                = static_mtrs_year,
              static_totals       = static_totals,
              conventional_totals = conventional_totals))
}



run_bathtub_pass = function(scenario_info, tax_law,
                             vat_price_offset     = NULL,
                             excess_growth_offset = NULL) {

  #----------------------------------------------------------------------------
  # Orchestrates the kg_dynamics bathtub pre-pass for one scenario. Aggregates
  # baseline cells from Tax-Data, builds gain-stock-weighted cell-MTR tau
  # lists from baseline + reform static detail, and runs the sequential
  # year-by-year recurrence via kg_dyn_run_bathtub_pass(). Side effect: writes
  # per-year state files under
  # {scenario_output}/conventional/supplemental/kg_dynamics_state/.
  #
  # Called by do_scenario() for non-baseline scenarios that include any
  # behavior module under kg_dynamics/. The behavior module then reads its
  # year's state file in the conventional pass.
  #
  # Parameters:
  #   - scenario_info (list)        : output of get_scenario_info()
  #   - tax_law (df)                : output of build_tax_law() — reform's
  #                                   joined tax law tibble
  #   - vat_price_offset (df)       : VAT price offset tibble; used only to
  #                                   refuse the run when VAT is active
  #   - excess_growth_offset (df)   : excess-growth offset tibble; used only
  #                                   to refuse the run when growth offset
  #                                   is active
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  kg_dyn_check_run_compat(scenario_info, vat_price_offset,
                          excess_growth_offset)

  # Reuse the frozen pass's Tax-Data sweep when available (same scenario,
  # same pipeline run) instead of re-reading the wide wealth columns.
  cache_path   = kg_dyn_inputs_cache_path(scenario_info)
  cells_inputs = if (file.exists(cache_path)) readRDS(cache_path) else NULL

  inputs = kg_dyn_load_bathtub_inputs(
    scenario_info = scenario_info,
    tax_law       = tax_law,
    baseline_root = globals$baseline_root,
    sample_ids    = globals$sample_ids,
    pct_sample    = globals$pct_sample,
    cells_inputs  = cells_inputs
  )

  # sigma income-conversion context (NULL when the scenario doesn't run the
  # conversion/sigma module): the bathtub pass computes per-record
  # conversions per year, injects the cell inflow into the recurrence, and
  # persists the cell tracker in the state files. Pool legs come from raw
  # Tax-Data; txbl_inc + per-leg MTRs from the baseline/scenario static
  # detail, both available here (the bathtub already requires them).
  sigma_ctx = NULL
  if (scenario_uses_sigma(scenario_info)) {
    sigma_ctx = sigma_build_ctx(
      scenario_info = scenario_info,
      tax_law       = tax_law,
      baseline_root = globals$baseline_root,
      sample_ids    = globals$sample_ids,
      pct_sample    = globals$pct_sample
    )
  }

  kg_dyn_run_bathtub_pass(
    scenario_info     = scenario_info,
    tax_law           = tax_law,
    baseline_cells    = inputs$baseline_cells,
    baseline_tau      = inputs$baseline_tau,
    reform_tau        = inputs$reform_tau,
    reform_tau_timing = inputs$reform_tau_timing,
    heir_dist         = inputs$heir_dist,
    # Corporate gain-state debit (D18 price margin in kg runs): per-year
    # level adjustments D_a(t) = mu_t * V_corp_exposed_a(t), recomputed from
    # the current markdown each year (credit-back automatic). NULL when the
    # corporate channel is inactive -- byte-identical state files then.
    corp_debit_by_year = corp_kg_state_debit_by_year(scenario_info,
                                                     inputs$baseline_cells),
    sigma_ctx          = sigma_ctx,
    # Wealth-tax deferral carrying cost h (per-year cell vectors; all-zero
    # when the scenario levies no wealth tax) -- prices the wealth x CG
    # margin in the Bellman and tau_eq.
    reform_carry       = inputs$reform_carry,
    # Leg-paired estate exposure of the kg death value (per-year cell
    # vectors of switch-gated mtr_estate_ded) -- prices the estate x CG
    # margin: (1 - e) on the Bellman death value F and on the tau_eq
    # death-realize term. e_B rides Pass 1 / prims_B, e_S Pass 2 / prims_S.
    baseline_estate    = inputs$baseline_estate,
    reform_estate      = inputs$reform_estate
  )

  invisible(NULL)
}



kg_dyn_check_run_compat = function(scenario_info, vat_price_offset,
                                   excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Shared preconditions for the kg_dynamics pre-passes (frozen mechanical
  # and conventional bathtub). Both read raw Tax-Data CSVs directly (for
  # value.*/basis.*/q_death*, which aren't in detail_vars), so both refuse
  # VAT and excess-growth scenarios: raw-dollar cell state would mix with
  # adjusted per-record kg_lt and put the carry channels in the wrong unit
  # system. Full sample is required because realization-rate cells are too
  # sparse otherwise; the kg_lt MTR registration is required by the bathtub
  # (checked here too so the pipeline fails before any pass runs).
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  if (is.null(scenario_info$mtr_vars) ||
      !('kg_lt' %in% scenario_info$mtr_vars)) {
    stop('kg_dynamics requires the runscript to register ',
         'mtr_vars = "kg_lt" so the bathtub can read per-cell MTRs from ',
         'static detail. Scenario "', scenario_info$ID, '" does not.')
  }

  if (!isTRUE(all.equal(globals$pct_sample, 1))) {
    stop('kg_dynamics requires pct_sample = 1 (full sample). ',
         'Realization-rate cells are too sparse at smaller samples to ',
         'support the bathtub recurrence; sparse-cell fallbacks would mask ',
         'sampling noise as policy response. Re-run with pct_sample = 1.')
  }

  vat_active = !is.null(vat_price_offset) &&
               'cpi_factor' %in% colnames(vat_price_offset) &&
               any(abs(vat_price_offset$cpi_factor - 1) > 1e-10, na.rm = TRUE)
  if (vat_active) {
    stop('kg_dynamics is not currently compatible with VAT scenarios. ',
         'The pre-passes read raw Tax-Data and would mix raw-dollar lock-in ',
         'carry with VAT-scaled per-record kg_lt. Run the kg_dynamics ',
         'reform without a VAT, or extend the bathtub to read from static ',
         'detail (which is post-VAT).')
  }

  growth_active = isTRUE(scenario_info$excess_growth != 0) &&
                  is.finite(scenario_info$excess_growth_start_year)
  if (growth_active) {
    stop('kg_dynamics is not currently compatible with excess-growth ',
         'scenarios (excess_growth = ', scenario_info$excess_growth, ', ',
         'start_year = ', scenario_info$excess_growth_start_year, '). ',
         'Same reason as VAT: raw cell state would not match growth-',
         'adjusted per-record kg_lt. Either disable excess growth on this ',
         'scenario or extend the bathtub to read from static detail.')
  }

  # Loudly flag a stale calibration (e.g. applier-rule flip or new Tax-Data
  # vintage without recalibration). Warns by default; KG_STRICT_CALIB=1 stops.
  kg_dyn_check_calibration_provenance(scenario_info)

  invisible(TRUE)
}



run_frozen_pass = function(scenario_info, tax_law,
                            vat_price_offset     = NULL,
                            excess_growth_offset = NULL) {

  #----------------------------------------------------------------------------
  # Orchestrates the kg_dynamics frozen mechanical pre-pass for one scenario.
  # Runs BEFORE the static pass (it needs only Tax-Data cell aggregates and
  # the joined tax law — no Bellman, no MTRs). Side effects:
  #   - per-year mechanical state files under
  #     {scenario_output}/static/supplemental/kg_dynamics_mech_state/
  #   - inputs_cache.rds in the same directory (baseline cells + slim
  #     per-record frames), reused by run_bathtub_pass to skip its own
  #     Tax-Data sweep.
  #
  # Called by do_scenario() for non-baseline kg_dynamics scenarios (main.R
  # sequential mode) and by src/slurm/frozen.R (SLURM Phase 1B).
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  kg_dyn_check_run_compat(scenario_info, vat_price_offset,
                          excess_growth_offset)

  cells_inputs = kg_dyn_load_cells_inputs(
    scenario_info = scenario_info,
    tax_law       = tax_law,
    sample_ids    = globals$sample_ids,
    pct_sample    = globals$pct_sample
  )

  dir.create(kg_dyn_mech_state_dir(scenario_info), recursive = TRUE,
             showWarnings = FALSE)
  saveRDS(cells_inputs, kg_dyn_inputs_cache_path(scenario_info))

  kg_dyn_run_frozen_pass(
    scenario_info  = scenario_info,
    tax_law        = tax_law,
    baseline_cells = cells_inputs$baseline_cells,
    heir_dist      = cells_inputs$heir_dist
  )

  invisible(NULL)
}

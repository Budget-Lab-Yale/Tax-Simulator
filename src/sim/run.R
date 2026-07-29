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
  
  # Get scenario info and create the scenario's output directory tree
  scenario_info = get_scenario_info(ID)
  ensure_scenario_dirs(scenario_info)

  # Install this scenario's resolved configuration, before any calculation:
  # economy_param() reads whatever config_activate() installed and errors if
  # nothing is active. Staleness was checked at parse time, in parse_globals.
  config_activate(economy  = scenario_info$resolved_economy,
                  behavior = scenario_info$resolved_behavior)


  #-----------------
  # Initialize data
  #-----------------

  # Calculate VAT price offset 
  vat_price_offset = get_vat_price_offset(
    macro_root = scenario_info$interface_paths$`Macro-Projections`, 
    vat_root   = scenario_info$interface_paths$`Value-Added-Tax-Model`, 
    years      = scenario_info$years
  )
  
  # Get price and wage index series
  indexes = generate_indexes(
    macro_root       = scenario_info$interface_paths$`Macro-Projections`, 
    vat_price_offset = vat_price_offset
  )
  
  # Build (and write) tax law
  tax_law = build_tax_law(scenario_info, indexes)


  #----------------
  # Run simulation
  #----------------

  uses_kg     = ID != 'baseline' && scenario_uses_kg_dynamics(scenario_info)
  uses_wealth = ID != 'baseline' && scenario_uses_wealth_dynamics(scenario_info)

  # The mechanical rung is the static one plus the transmission channels, so it
  # runs only where one of them is live. A reform touching employer payroll law
  # counts: the wage adjustment moves the income tax base and is applied on this
  # rung and not on the static one.
  uses_mech = scenario_runs_mechanical(scenario_info)

  if (uses_kg || uses_wealth || uses_mech) {

    # Run the transmission channels and the cohort dynamics as separate passes,
    # each channel switched on independently:
    #
    #   [frozen] -> static -> [mech-no-wealth, mech bathtub] -> [mechanical]
    #     -> [kg bathtub] -> [conv-no-wealth, wealth bathtub] -> conventional
    #
    # The frozen pass needs only Tax-Data cells and tax law, and writes the
    # mechanical state the static pass injects. Each wealth bathtub's forcing
    # excludes the wealth tax, so it takes its own pass with the haircut off; the
    # rung it serves then applies the haircut. The mechanical rung measures its
    # forcing with behavior off, so its drawdown compounds the static tax change.

    if (uses_kg) {
      run_frozen_pass(scenario_info, tax_law,
                      vat_price_offset = vat_price_offset)
    }

    static_mtrs = run_sim(scenario_info        = scenario_info,
                          tax_law              = tax_law,
                          baseline_mtrs        = baseline_mtrs,
                          indexes              = indexes,
                          vat_price_offset     = vat_price_offset,
                          pass_type            = 'static')

    if (uses_mech) {
      if (uses_wealth) {
        run_sim(scenario_info        = scenario_info,
                tax_law              = tax_law,
                baseline_mtrs        = baseline_mtrs,
                indexes              = indexes,
                vat_price_offset     = vat_price_offset,
                pass_type            = 'mechanical_no_wealth')

        run_wealth_bathtub_pass(scenario_info, tax_law,
                                vat_price_offset = vat_price_offset,
                                leg              = 'mechanical')
      }

      run_sim(scenario_info        = scenario_info,
              tax_law              = tax_law,
              baseline_mtrs        = baseline_mtrs,
              indexes              = indexes,
              vat_price_offset     = vat_price_offset,
              pass_type            = 'mechanical')
    }

    if (uses_kg) {
      run_bathtub_pass(scenario_info, tax_law,
                       vat_price_offset = vat_price_offset)
    }

    if (uses_wealth) {
      # Measure the forcing and the capital-bundle and net worth MTRs on the
      # conventional base before any erosion
      run_sim(scenario_info        = scenario_info,
              tax_law              = tax_law,
              baseline_mtrs        = baseline_mtrs,
              indexes              = indexes,
              vat_price_offset     = vat_price_offset,
              pass_type            = 'conventional_no_wealth',
              static_mtrs_all      = static_mtrs)

      run_wealth_bathtub_pass(scenario_info, tax_law,
                              vat_price_offset = vat_price_offset,
                              leg              = 'conventional')
    }

    run_sim(scenario_info        = scenario_info,
            tax_law              = tax_law,
            baseline_mtrs        = baseline_mtrs,
            indexes              = indexes,
            vat_price_offset     = vat_price_offset,
            pass_type            = 'conventional',
            static_mtrs_all      = static_mtrs)

  } else {

    # Run the static and conventional passes together
    static_mtrs = run_sim(scenario_info    = scenario_info,
                          tax_law          = tax_law,
                          baseline_mtrs    = baseline_mtrs,
                          indexes          = indexes,
                          vat_price_offset = vat_price_offset)
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

    # Effective tax rate levels, under accrual income definitions and the
    # stock-based corporate incidence conventions
    build_distribution_etrs(ID)

    # Time burden tables
    build_timeburden_table(ID)

    # Horizontal equity
    build_horizontal_table(ID)

    # Capital gains bathtub diagnostics
    kg_dyn_build_summary(scenario_info)
  }
  
  # Return MTRs if running baseline
  if (ID == 'baseline') {
    return(static_mtrs)
  }
}



scenario_runs_mechanical = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reports whether the mechanical rung differs from the static one for a
  # scenario, which is so where any transmission channel is live: corporate
  # incidence, a wealth financing profile with a nonzero saving share, or a reform
  # changing employer-side payroll law.
  #
  # Parameters:
  #   - scenario_info (list) : output of get_scenario_info()
  #
  # Returns: TRUE if the mechanical pass runs for this scenario (bool).
  #----------------------------------------------------------------------------

  scenario_info$ID != 'baseline' &&
    (scenario_uses_wealth_dynamics(scenario_info) ||
     scenario_uses_corp_incidence(scenario_info) ||
     scenario_uses_er_payroll_reform(scenario_info))
}



write_pass_outputs = function(output, root, totals_slot,
                              vat_price_offset,
                              scenario_info, leg = NULL) {

  #----------------------------------------------------------------------------
  # Writes one pass's supplemental offsets, totals CSVs, and receipts for a
  # scenario. Called by run_sim() and by Phase 3a of the SLURM pipeline, which
  # assemble output to the same shape.
  #
  # Parameters:
  #   - output (list)             : per-year results; each element carries the
  #                                 totals slots the passes it ran filled
  #   - root (str)                : pass output root (…/static, …/mechanical or
  #                                 …/conventional)
  #   - totals_slot (str)         : which per-year totals list to aggregate, one of
  #                                 'static_totals', 'mechanical_totals' or
  #                                 'conventional_totals'
  #   - leg (str)                 : leg calc_receipts books; defaults to the
  #                                 totals_slot's stem. Given explicitly where a
  #                                 rung reports the totals of the rung below
  #   - vat_price_offset (df)     : VAT price offset series, written to supplemental
  #   - scenario_info (list)      : scenario info (interface paths)
  #
  # Returns: invisible NULL (writes files as a side effect)
  #----------------------------------------------------------------------------

  vat_price_offset %>%
    write_csv(file.path(root, 'supplemental', 'vat_price_offset.csv'))

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
      leg                   = leg %||% str_remove(totals_slot, '_totals$')
    )

  invisible(NULL)
}



#-------------------------------------------------------------------------------
# Helpers shared by the static and conventional bodies of run_one_year(). The
# two passes differ in their inputs, not in how they compute MTRs, assemble
# totals, or write detail.
#-------------------------------------------------------------------------------

# Channel columns written to a detail file through any_of(), so that a scenario
# not running the channel writes no column. The wealth haircut, the corporate
# markdown and the forcing measurements appear on conventional passes only.
DETAIL_COLS_OPTIONAL_STATIC = c('kg_lockin', 'kg_deemed', 'liab_deemed',
                                'estate_income_tax_ded')

DETAIL_COLS_OPTIONAL_CONV = c(DETAIL_COLS_OPTIONAL_STATIC,
                              'estate_concealed_frac',
                              'economic_gross', 'cap_bundle_F',
                              'net_worth_raw', 'nw_pctile', 'D_alloc',
                              'wealth_haircut',
                              'corp_dY_exog', 'corp_markdown',
                              'corp_flow_factor', 'pr_dY_exog')



#-------------------------------------------------------------------------------
# Specifications for the counterfactual passes run_one_year() executes after the
# static one. All of them run the same body and differ only in their inputs and
# in where their output goes:
#
#   root            : output subfolder under the scenario's output path
#   alias_from      : rungs this pass copies when it cannot differ from the one
#                     below, in order of preference
#   config_pass     : label recorded by config_set_pass, read by the economy
#                     leg's role gate
#   behavior        : run the behavior modules
#   haircut         : apply the wealth haircut where the channel is active
#   state_pass      : leg whose cohort state the haircut reads
#   measurement     : compute the columns the wealth pre-pass reads
#   mtrs            : run the MTR block
#   totals_slot     : totals list this pass fills, NA for an intermediate pass
#   corp_diag       : write the corporate conservation diagnostic
#   detail_optional : channel columns written to detail when present
#-------------------------------------------------------------------------------

PASS_SPECS = list(

  mechanical_no_wealth = list(
    root            = 'mechanical_no_wealth',
    alias_from      = 'static',
    config_pass     = 'mechanical',
    behavior        = FALSE,
    haircut         = FALSE,
    state_pass      = 'mechanical',
    measurement     = TRUE,
    mtrs            = FALSE,
    totals_slot     = NA_character_,
    corp_diag       = FALSE,
    detail_optional = DETAIL_COLS_OPTIONAL_CONV),

  mechanical = list(
    root            = 'mechanical',
    alias_from      = 'static',
    config_pass     = 'mechanical',
    behavior        = FALSE,
    haircut         = TRUE,
    state_pass      = 'mechanical',
    measurement     = FALSE,
    mtrs            = TRUE,
    totals_slot     = 'mechanical_totals',
    corp_diag       = TRUE,
    detail_optional = DETAIL_COLS_OPTIONAL_CONV),

  conventional_no_wealth = list(
    root            = 'conventional_no_wealth',
    alias_from      = c('mechanical', 'static'),
    config_pass     = 'conventional',
    behavior        = TRUE,
    haircut         = FALSE,
    state_pass      = 'conventional',
    measurement     = TRUE,
    mtrs            = FALSE,
    totals_slot     = NA_character_,
    corp_diag       = FALSE,
    detail_optional = DETAIL_COLS_OPTIONAL_CONV),

  conventional = list(
    root            = 'conventional',
    alias_from      = c('mechanical', 'static'),
    config_pass     = 'conventional',
    behavior        = TRUE,
    haircut         = TRUE,
    state_pass      = 'conventional',
    measurement     = FALSE,
    mtrs            = TRUE,
    totals_slot     = 'conventional_totals',
    corp_diag       = TRUE,
    detail_optional = DETAIL_COLS_OPTIONAL_CONV)
)



strip_calc_vars = function(df, drop_mtrs = FALSE, strict = TRUE) {

  #----------------------------------------------------------------------------
  # Drops the calculated tax variables from a frame, recovering the exogenous-
  # variable frame calc_mtrs() expects.
  #
  # Parameters:
  #   - df (df)           : tax unit frame
  #   - drop_mtrs (bool)  : also drop any mtr_* columns already joined on
  #   - strict (bool)     : TRUE requires every calculated variable to be
  #                         present, as on a frame that has been through
  #                         do_taxes; FALSE tolerates their absence
  #
  # Returns: the frame less the calculated variables (df).
  #----------------------------------------------------------------------------

  calc_vars = return_vars %>% unlist() %>% set_names(NULL)

  df = if (strict) {
    df %>% select(-all_of(calc_vars))
  } else {
    df %>% select(-any_of(calc_vars))
  }

  if (drop_mtrs) {
    df = df %>% select(-starts_with('mtr_'))
  }
  df
}



mtr_actuals = function(taxed) {

  #----------------------------------------------------------------------------
  # Assembles the actual-liability vectors calc_mtrs() differences against, read
  # off the same frame the recompute runs on. Estate liability is the expected
  # value over the two DSUE branches:
  #
  #   E[liab] = p_dsue * liab_dsue + (1 - p_dsue) * liab_nodsue
  #
  # The wealth and estate entries are read only by their own MTR variables.
  #
  # Parameters:
  #   - taxed (df) : post-do_taxes frame carrying liabilities and estate output
  #
  # Returns: named list of actuals vectors.
  #----------------------------------------------------------------------------

  # A frame taxed with calc_estate_flag or calc_wealth_flag off carries no estate
  # or wealth output. calc_mtrs defaults those actuals to NULL and refuses a NULL
  # for any MTR variable that reads them.
  has = function(col) col %in% names(taxed)

  list(
    liab_iit      = taxed$liab_iit_net,
    liab_pr       = taxed$liab_pr,
    liab_wealth   = if (has('liab_wealth')) taxed$liab_wealth else NULL,
    liab_estate   = if (has('estate_p_dsue')) {
                      taxed$estate_p_dsue * taxed$liab_estate_dsue +
                        (1 - taxed$estate_p_dsue) * taxed$liab_estate_nodsue
                    } else NULL,
    estate_p_dsue = if (has('estate_p_dsue')) taxed$estate_p_dsue else NULL
  )
}



calc_one_mtr = function(frame, actuals, var, baseline_pr_er = NULL,
                        type = 'nextdollar') {

  #----------------------------------------------------------------------------
  # Calculates one MTR as a bare vector, for the columns computed outside the
  # loop over the scenario's mtr_vars.
  #
  # Parameters:
  #   - frame (df)          : exogenous-variable frame (see strip_calc_vars)
  #   - actuals (list)      : output of mtr_actuals() for the frame the actual
  #                           liabilities were computed on
  #   - var (str)           : variable to perturb
  #   - baseline_pr_er (df) : NULL for a frame that has been through do_taxes,
  #                           the real value otherwise (see calc_mtrs)
  #   - type (str)          : 'nextdollar' (default) or 'extensive'
  #
  # Returns: numeric vector of MTRs.
  #----------------------------------------------------------------------------

  calc_mtrs(
    tax_units            = frame,
    actual_liab_iit      = actuals$liab_iit,
    actual_liab_pr       = actuals$liab_pr,
    actual_liab_wealth   = actuals$liab_wealth,
    actual_liab_estate   = actuals$liab_estate,
    actual_estate_p_dsue = actuals$estate_p_dsue,
    baseline_pr_er       = baseline_pr_er,
    var                  = var,
    pr                   = F,
    type                 = type
  )[[paste0('mtr_', var)]]
}



mtr_worker_count = function(n_tasks) {

  #----------------------------------------------------------------------------
  # Resolves the number of local workers available to the MTR recomputes. SLURM
  # exposes the allocation through SLURM_CPUS_PER_TASK, and TAXSIM_MTR_CORES
  # overrides it for benchmark runs. Returns 1 under main.R's own scenario or
  # year parallelism, to avoid a second layer of forks.
  #
  # Parameters:
  #   - n_tasks (int) : number of MTR recomputes to distribute
  #
  # Returns: number of workers (int).
  #----------------------------------------------------------------------------

  if (.Platform$OS.type == 'windows' || globals$multicore != 'none') {
    return(1L)
  }

  requested = Sys.getenv(
    'TAXSIM_MTR_CORES',
    unset = Sys.getenv('SLURM_CPUS_PER_TASK', unset = '1')
  )
  requested = suppressWarnings(as.integer(requested))
  if (is.na(requested) || requested < 1L) requested = 1L

  min(as.integer(n_tasks), requested)
}



run_mtr_block = function(taxed, scenario_info, year, baseline_pr_er) {

  #----------------------------------------------------------------------------
  # Computes every MTR the runscript registers and joins them onto the frame.
  # Two estate columns come out of the one perturbation, derived here while the
  # law column is still in the frame:
  #
  #   mtr_estate_ded = estate.income_tax_ded * mtr_estate
  #
  # mtr_estate is the base rate, read by the wealth avoidance response.
  # mtr_estate_ded is read by the capital gains Bellman and its equivalent-rate
  # exposure aggregator, and goes to zero when a reform turns off deductibility
  # of the decedent's income tax.
  #
  # Parameters:
  #   - taxed (df)           : post-do_taxes frame for this pass
  #   - scenario_info (list) : supplies mtr_vars / mtr_types
  #   - year (int)           : simulation year, stamped onto the MTR tibble
  #   - baseline_pr_er (df)  : has no default. A frame that has been through
  #                            do_taxes already carries the employer payroll wage
  #                            rescale, so callers passing such a frame must pass
  #                            NULL; the real value rescales a second time. See
  #                            calc_mtrs.
  #
  # Returns: list(taxed = frame with mtr_* joined on, mtrs = the MTR tibble).
  #----------------------------------------------------------------------------

  actuals = mtr_actuals(taxed)
  frame   = strip_calc_vars(taxed)

  calc_one = function(var, type) {
    calc_mtrs(
      tax_units            = frame,
      actual_liab_iit      = actuals$liab_iit,
      actual_liab_pr       = actuals$liab_pr,
      actual_liab_wealth   = actuals$liab_wealth,
      actual_liab_estate   = actuals$liab_estate,
      actual_estate_p_dsue = actuals$estate_p_dsue,
      baseline_pr_er       = baseline_pr_er,
      var                  = var,
      pr                   = F,
      type                 = type
    )
  }

  n_workers = mtr_worker_count(length(scenario_info$mtr_vars))
  if (n_workers == 1L) {
    mtr_parts = map2(scenario_info$mtr_vars, scenario_info$mtr_types, calc_one)
  } else {
    cat(paste0('Calculating ', length(scenario_info$mtr_vars), ' MTRs with ',
               n_workers, ' local workers\n'))

    # Return errors as data so the parent can name the MTR that failed. mclapply
    # otherwise warns and embeds a try-error that is easy to overlook.
    mtr_parts = parallel::mclapply(
      X = seq_along(scenario_info$mtr_vars),
      FUN = function(i) {
        tryCatch(
          list(ok = TRUE,
               value = calc_one(scenario_info$mtr_vars[i],
                                scenario_info$mtr_types[i])),
          error = function(e) list(ok = FALSE, error = conditionMessage(e))
        )
      },
      mc.cores       = n_workers,
      mc.preschedule = TRUE,
      mc.set.seed    = FALSE
    )

    failed = which(vapply(
      mtr_parts,
      function(x) inherits(x, 'try-error') || !is.list(x) || !isTRUE(x$ok),
      logical(1)
    ))
    if (length(failed) > 0) {
      i = failed[1]
      detail = if (is.list(mtr_parts[[i]]) &&
                   !is.null(mtr_parts[[i]]$error)) {
                 mtr_parts[[i]]$error
               } else {
                 as.character(mtr_parts[[i]])
               }
      stop('Parallel MTR worker failed for var=', scenario_info$mtr_vars[i],
           ', type=', scenario_info$mtr_types[i], ': ', detail)
    }
    mtr_parts = map(mtr_parts, 'value')
  }

  # Rebuild each one-column tibble from its bare vector. Fork results cross a
  # serialization boundary and the one-core results do not, so this is what keeps
  # dataframe attributes from differing between the two paths.
  mtr_parts = map(mtr_parts, ~ as_tibble(set_names(list(.x[[1]]), names(.x))))

  mtrs = mtr_parts %>%
    bind_cols() %>%
    mutate(id   = taxed$id,
           year = year) %>%
    relocate(id, year)

  taxed %<>%
    left_join(mtrs %>%
                select(-year),
              by = 'id')

  if ('estate' %in% scenario_info$mtr_vars) {
    taxed %<>%
      mutate(mtr_estate_ded = estate.income_tax_ded * mtr_estate)
  }

  list(taxed = taxed, mtrs = mtrs)
}



collect_totals = function(taxed, year) {

  #----------------------------------------------------------------------------
  # Builds the level aggregations written to a pass's totals directory and fed to
  # calc_receipts().
  #
  # Parameters:
  #   - taxed (df) : post-do_taxes frame for this pass
  #   - year (int) : simulation year
  #
  # Returns: named list of totals tibbles.
  #----------------------------------------------------------------------------

  list(pr            = get_pr_totals(taxed, year),
       `1040`        = get_1040_totals(taxed, year),
       `1040_by_agi` = get_1040_totals(taxed, year, T),
       estate        = get_estate_totals(taxed, year),
       wealth        = get_wealth_totals(taxed, year))
}



write_detail = function(taxed, path, optional = character(0)) {

  #----------------------------------------------------------------------------
  # Writes a pass's tax unit detail file.
  #
  # Parameters:
  #   - taxed (df)       : post-do_taxes frame for this pass
  #   - path (str)       : output CSV path; parent directory created if absent
  #   - optional (str[]) : channel columns to include when present, either
  #                        DETAIL_COLS_OPTIONAL_STATIC or DETAIL_COLS_OPTIONAL_CONV
  #
  # Returns: invisible NULL.
  #----------------------------------------------------------------------------

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  taxed %>%
    select(all_of(globals$detail_vars), starts_with('mtr_'), any_of(optional)) %>%
    write_csv(path)

  invisible(NULL)
}



fold_deemed = function(taxed, liab_deemed = NULL) {

  #----------------------------------------------------------------------------
  # Folds the expected tax on deemed death gains into reported liability. Called
  # after the MTR block, which anchors on the liability of the surviving leg.
  # Receipts are built from the payment-timing variables rather than from
  # liab_iit_net, so the fold lands there too: deemed tax is a capital gains bill
  # on a final return, and so nonwithheld income tax paid at filing.
  #
  # Parameters:
  #   - taxed (df)           : frame carrying a liab_deemed column, or one to
  #                            which liab_deemed will be attached
  #   - liab_deemed (dbl[])  : optional vector to attach first; the conventional
  #                            pass holds it aside across the MTR block
  #
  # Returns: the frame with liab_deemed folded into liability and payments.
  #----------------------------------------------------------------------------

  if (!is.null(liab_deemed)) {
    taxed$liab_deemed = liab_deemed
  }

  taxed %>%
    mutate(liab_iit_net        = liab_iit_net        + liab_deemed,
           liab_iit            = liab_iit            + liab_deemed,
           pmt_iit_nonwithheld = pmt_iit_nonwithheld + liab_deemed)
}



run_sim = function(scenario_info, tax_law, baseline_mtrs,
                   indexes, vat_price_offset,
                   pass_type = c('both', 'static', 'mechanical',
                                 'mechanical_no_wealth', 'conventional',
                                 'conventional_no_wealth'),
                   static_mtrs_all = NULL) {

  #----------------------------------------------------------------------------
  # Runs simulation for all years of a scenario, in one of the pass types
  # run_one_year takes:
  #
  #   'both'         : static and conventional in one pass per year, writing both
  #                    totals files. Returns the combined static MTRs.
  #   'static'       : static across all years, writing static totals and
  #                    receipts. Returns the combined static MTRs.
  #   'mechanical'   : mechanical across all years, writing mechanical totals and
  #                    receipts.
  #   'conventional' : conventional across all years, writing conventional totals
  #                    and receipts. Takes static_mtrs_all from an earlier static
  #                    run and filters it by year.
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
  #   - pass_type (str)           : 'both' (default), 'static', 'mechanical',
  #                                 'mechanical_no_wealth', 'conventional', or
  #                                 'conventional_no_wealth'
  #   - static_mtrs_all (df)      : combined static MTRs across years, required on
  #                                 a conventional pass whose behavior modules
  #                                 read them
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

  # Write static outputs, if this call ran the static pass
  if (pass_type %in% c('both', 'static')) {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'static'),
      totals_slot          = 'static_totals',
      vat_price_offset     = vat_price_offset,
      scenario_info        = scenario_info
    )
  }

  # Where no transmission channel is live the mechanical rung equals the static
  # one. Report it from the static totals anyway, so every counterfactual carries
  # all three rungs and the reporting layer needs no special case
  if (pass_type %in% c('both', 'static') &&
      scenario_info$ID != 'baseline' &&
      !scenario_runs_mechanical(scenario_info)) {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'mechanical'),
      totals_slot          = 'static_totals',
      vat_price_offset     = vat_price_offset,
      scenario_info        = scenario_info,
      leg                  = 'mechanical'
    )
  }

  # Write mechanical outputs, if this call ran the mechanical pass. Baseline has
  # none
  if (pass_type == 'mechanical' && scenario_info$ID != 'baseline') {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'mechanical'),
      totals_slot          = 'mechanical_totals',
      vat_price_offset     = vat_price_offset,
      scenario_info        = scenario_info
    )
  }

  # Write conventional outputs, if this call ran the conventional pass. Baseline
  # has none
  if (pass_type %in% c('both', 'conventional') && scenario_info$ID != 'baseline') {
    write_pass_outputs(
      output               = output,
      root                 = file.path(scenario_info$output_path, 'conventional'),
      totals_slot          = 'conventional_totals',
      vat_price_offset     = vat_price_offset,
      scenario_info        = scenario_info
    )
  }

  # Return combined MTRs, if this call ran the static pass
  if (pass_type %in% c('both', 'static')) {
    return(output %>% map(.f = ~ .x$mtrs) %>% bind_rows())
  }
  invisible(NULL)
}



kg_dyn_recompute_deemed_tax = function(taxed, input, baseline_pr_er,
                                       vars_1040, vars_payroll, estate_params) {

  #----------------------------------------------------------------------------
  # Calculates expected income tax on deemed death gains and reprices the Sec.
  # 2053 estate deduction. Takes the expectation over the death event without
  # duplicating rows:
  #
  #   liab_deemed = m_household * [T(y + kg_deemed_full) - T(y)]
  #
  # The second term comes from a second do_taxes() recompute with the death gain,
  # net of Sec. 121, the death-gain exclusion and avoidance, added to kg_lt. Both
  # legs run under reform law, so rate reforms flow through. The decedent's tax is
  # then deducted against the taxable estate, which the in-chain estate
  # calculation ran with turned off, and estate liabilities are repriced. The
  # deduction enters the base only, so estate_distributable does not move.
  #
  # Under a nonzero death-gain exclusion a single filer's death gain has two
  # branches, at the own and married amounts, and the dead leg runs once per
  # branch. The expectation over whether the filer is widowed blends the two:
  #
  #   liab_deemed_cond = p_widow * dT_2x + (1 - p_widow) * dT_x
  #
  # The recompute runs on the whole frame rather than a subset, because the calc
  # functions index globals$random_numbers positionally and subsetting rows breaks
  # the alignment. Records holding no gains have a delta of exactly zero. Only
  # liab_iit_net is read off the recompute, so it skips the estate calculation.
  #
  # Parameters:
  #   - taxed (df)          : taxed surviving-leg frame, modified and returned
  #   - input (df)          : pre-tax input frame for this pass, same row order,
  #                           carrying kg_lt, kg_deemed_full, its two branches,
  #                           p_widow and m_household
  #   - baseline_pr_er (df) : baseline employer payroll, passed to do_taxes()
  #   - vars_1040 (str[])   : 1040 return vars for do_taxes()
  #   - vars_payroll (str[]): payroll return vars for do_taxes()
  #   - estate_params       : estate measurement parameters
  #
  # Returns: taxed with liab_deemed attached and the estate output repriced (df).
  #----------------------------------------------------------------------------

  dead_leg_delta = function(deemed_gain) {
    dead_leg = input %>%
      mutate(kg_lt = kg_lt + !!deemed_gain) %>%
      do_taxes(baseline_pr_er   = baseline_pr_er,
               vars_1040        = vars_1040,
               vars_payroll     = vars_payroll,
               calc_estate_flag = FALSE,
               calc_wealth_flag = FALSE)   # only liab_iit_net is read
    stopifnot(identical(dead_leg$id, taxed$id))
    dead_leg$liab_iit_net - taxed$liab_iit_net
  }

  two_branch = all(c('kg_deemed_full_x', 'kg_deemed_full_2x', 'p_widow') %in%
                     names(input)) &&
    any(input$kg_deemed_full_2x != input$kg_deemed_full_x)

  if (two_branch) {
    dT_x  = dead_leg_delta(input$kg_deemed_full_x)
    dT_2x = dead_leg_delta(input$kg_deemed_full_2x)
    liab_deemed_cond = input$p_widow * dT_2x + (1 - input$p_widow) * dT_x
  } else {
    liab_deemed_cond = dead_leg_delta(input$kg_deemed_full)
  }
  taxed$liab_deemed = input$m_household * liab_deemed_cond

  taxed$estate_income_tax_ded = pmax(liab_deemed_cond, 0)
  est = calc_estate(taxed, estate_params)
  taxed[, ESTATE_OUTPUT_COLS] = est[ESTATE_OUTPUT_COLS]

  taxed
}



run_one_year = function(year, scenario_info, tax_law, baseline_mtrs,
                        indexes, vat_price_offset,
                        pass_type = c('both', 'static', 'mechanical',
                                      'mechanical_no_wealth', 'conventional',
                                      'conventional_no_wealth'),
                        static_mtrs_year = NULL) {

  #----------------------------------------------------------------------------
  # Runs a single year of tax simulation, in one of six pass types:
  #
  #   'both'         : static and conventional in one process, writing both
  #                    detail files. Returns the MTRs and both sets of totals.
  #   'static'       : the static pass, including MTRs. Returns the MTRs and the
  #                    static totals.
  #   'mechanical'   : the static pass plus the transmission channels -- corporate
  #                    incidence, the wealth haircut and the employer payroll wage
  #                    adjustment -- and no behavior. Returns the mechanical
  #                    totals.
  #   'mechanical_no_wealth' : as mechanical, with the wealth haircut off. Writes
  #                    detail only, for the mechanical wealth pre-pass to read.
  #   'conventional' : the conventional pass, taking static_mtrs_year so the
  #                    behavior modules see this scenario's static MTRs. Returns
  #                    the conventional totals.
  #   'conventional_no_wealth' : as conventional, with the wealth haircut off.
  #                    Writes detail only, for the wealth bathtub to read.
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
  #   - pass_type (str)           : 'both' (default), 'static', 'mechanical',
  #                                 'mechanical_no_wealth', 'conventional', or
  #                                 'conventional_no_wealth'
  #   - static_mtrs_year (df)     : static MTRs for this year, required on a
  #                                 conventional pass with behavior modules
  #
  # Returns: list holding some of mtrs, static_totals, mechanical_totals and
  #          conventional_totals, depending on the pass type.
  #----------------------------------------------------------------------------

  pass_type = match.arg(pass_type)

  # Each pass block below tags itself with config_set_pass, which the economy
  # leg's role gate reads. A single tag here would mislabel one of the two blocks
  # a 'both' run executes. Clear the tag on exit so no label leaks across years.
  on.exit(config_set_pass(NA), add = TRUE)

  # Name the counterfactual pass this call runs after the static one. A 'both' run
  # pairs the static pass with the conventional one; every other pass type names
  # its own pass, and the static pass alone runs no second body.
  pass_name = switch(pass_type,
                     'both'   = 'conventional',
                     'static' = NA_character_,
                     pass_type)

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

    # Assign random numbers, keyed on id: the id universe varies by year, so a
    # positional bind would misalign the draws
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

    # Compute CPI ratio for capital gains basis indexation
    calc_kg_cpi_ratio(indexes, year)

  # Set up the estate tax. Liability is computed in the calculator chain by
  # do_taxes, once per pass, so behavioral modules reprice it. Two things happen
  # here instead: the measurement parameters go into globals, where every
  # do_taxes call including the MTR recomputes can see them, and the household
  # death probability is computed, which is a weights operation and includes the
  # donor-clone cluster cap. Wealth stays in raw dollars, since the VAT price
  # adjustment applies to income and not to balance sheet stocks.
  globals$estate_params <<- get_estate_params(scenario_info$interface_paths$`Tax-Data`)
  tax_units$estate_m = calc_estate_mortality(
    tax_units, globals$estate_params$cluster_death_weight_cap)

  # Store economic net worth, assets less debts in raw dollars with no valuation
  # discount, as a column. It is computed once here because three things read it:
  # calc_wealth, as the wealth tax base; calc_mtrs, which perturbs stored columns
  # and so needs net worth to be one; and the avoidance module, which overwrites
  # it with the avoided base. The asset columns themselves are untouched, so
  # estate and capital income are unaffected.
  tax_units %<>%
    mutate(net_worth = rowSums(across(all_of(WEALTH_ASSET_COLS), ~ replace_na(., 0))) -
                       rowSums(across(all_of(WEALTH_DEBT_COLS),  ~ replace_na(., 0))))


  #----------
  # Do taxes
  #----------

  # Read baseline payroll taxes, for the wage adjustment on the passes that apply
  # it. A static-only run does not, and skips a 150MB read per year.
  baseline_pr_er = NULL
  if (scenario_info$ID != 'baseline' && !is.na(pass_name)) {
    # Read 3 columns of about 98. Detail files run to 150MB and every pass reads
    # this once per year
    baseline_pr_er = globals$baseline_root %>%
      file.path('baseline/static/detail', paste0(year, '.csv')) %>%
      fread(select = c('id', 'liab_fica_er1', 'liab_fica_er2')) %>%
      tibble() %>%
      select(id, baseline1 = liab_fica_er1, baseline2 = liab_fica_er2)
  }

  # List calculated tax variables
  vars_1040 = return_vars %>%
    remove_by_name('calc_pr') %>%
    unlist() %>%
    set_names(NULL)


  #-------------
  # Static pass
  #-------------

  static_totals    = NULL
  tax_units_static = NULL
  uses_kg_mech     = scenario_info$ID != 'baseline' &&
                     scenario_uses_kg_dynamics(scenario_info)
  uses_wealth      = scenario_info$ID != 'baseline' &&
                     scenario_uses_wealth_dynamics(scenario_info)
  uses_corp        = scenario_info$ID != 'baseline' &&
                     scenario_uses_corp_incidence(scenario_info)
  # The employer payroll wage adjustment runs on this pass and not on the static
  # one, so a reform touching employer payroll law makes the two rungs differ even
  # with no other channel live
  uses_er_payroll  = scenario_uses_er_payroll_reform(scenario_info)
  if (pass_type %in% c('both', 'static')) {

    config_set_pass('static')

    # The employer payroll wage adjustment holds total employer cost fixed and
    # lets wages absorb the employer tax change, which moves the income tax base
    # in response to a payroll provision. It runs on the mechanical and
    # conventional passes, so this pass hands do_taxes no baseline employer
    # payroll table. See src/sim/payroll.R.
    static_pr_er = NULL

    # Inject the frozen-realization carryover and deemed quantities into records
    # before tax calculation, so that the policy's mechanical content lands in
    # static liabilities, static MTRs and the distribution tables. tax_units
    # itself is left alone for the conventional pass, whose behavior module
    # applies the full bathtub state.
    static_input = tax_units
    if (uses_kg_mech) {
      static_input = kg_dyn_apply_mech_to_records(tax_units, scenario_info,
                                                  year)
    }

    tax_units_static = static_input %>%
      do_taxes(baseline_pr_er = static_pr_er,
               vars_1040      = vars_1040,
               vars_payroll   = return_vars$calc_pr)

    # Calculate expected tax on deemed death gains. The main frame's kg_lt holds
    # no deemed gain, so MTRs and tau price the inter-vivos margin alone, and
    # liab_deemed is folded into liability after the MTR block below. See
    # kg_dyn_recompute_deemed_tax.
    if (uses_kg_mech) {
      tax_units_static = tax_units_static %>% mutate(liab_deemed = 0)
      if (any(static_input$kg_deemed_full > 0)) {
        tax_units_static = kg_dyn_recompute_deemed_tax(
          taxed          = tax_units_static,
          input          = static_input,
          baseline_pr_er = static_pr_er,
          vars_1040      = vars_1040,
          vars_payroll   = return_vars$calc_pr,
          estate_params  = globals$estate_params)
      }
    }

    # Calculate static marginal tax rates
    static_mtrs_year = NULL
    if (!is.null(scenario_info$mtr_vars)) {

      # This pass applies no employer payroll wage rescale, and the frame has
      # been through do_taxes in any case, so pass NULL. See calc_mtrs.
      static_mtr_out   = run_mtr_block(taxed          = tax_units_static,
                                       scenario_info  = scenario_info,
                                       year           = year,
                                       baseline_pr_er = NULL)
      tax_units_static = static_mtr_out$taxed
      static_mtrs_year = static_mtr_out$mtrs

      # Read the actuals off this frame, for the fallback columns below. The MTR
      # join adds only mtr_ columns, so liabilities are unchanged
      static_actuals = mtr_actuals(tax_units_static)

      # Calculate the kg_lt MTR under reform law on the frame before the
      # mechanical injection, read only by the planned-timing wedge. The
      # injection adds carryover realizations to heirs' kg_lt, which moves their
      # bracket and phaseout positions and drifts the cell average rate by a few
      # basis points; kg_dyn_build_planned_timing takes an argmin over years and
      # would retime a few percent of the bucket against that drift. The Bellman
      # keeps the post-injection rate, where the income effect is signal.
      if (uses_kg_mech) {
        tax_units_raw = tax_units %>%
          do_taxes(baseline_pr_er   = static_pr_er,
                   vars_1040        = vars_1040,
                   vars_payroll     = return_vars$calc_pr,
                   calc_estate_flag = FALSE,    # only liab_iit_net and liab_pr are read
                   calc_wealth_flag = FALSE)
        stopifnot(identical(tax_units_raw$id, tax_units_static$id))
        # This frame has been through do_taxes, so pass no employer payroll table
        tax_units_static$mtr_kg_lt_lawonly = calc_one_mtr(
          frame   = strip_calc_vars(tax_units_raw),
          actuals = mtr_actuals(tax_units_raw),
          var     = 'kg_lt')
      }

      # Supply mtr_net_worth when the scenario's wealth law is active and the
      # runscript did not register it. kg_dyn_aggregate_cell_carry prices
      # deferral off the product of mtr_net_worth and mtr_kg_lt read from this
      # static detail. The gate on wealth law being active in any year keeps the
      # detail schema stable across phase-in years.
      if (uses_kg_mech && kg_dyn_wealth_law_active(tax_law) &&
          !('net_worth' %in% scenario_info$mtr_vars)) {
        # drop_mtrs recovers the frame the loop above ran on, so this column
        # matches a registered mtr_net_worth. The frame has been through
        # do_taxes, so pass no employer payroll table -- unlike the no-wealth
        # block below, which measures on a frame that has not and threads it.
        tax_units_static$mtr_net_worth = calc_one_mtr(
          frame   = strip_calc_vars(tax_units_static, drop_mtrs = TRUE),
          actuals = static_actuals,
          var     = 'net_worth')
      }

      # Supply mtr_estate and mtr_estate_ded when the runscript did not register
      # them. kg_dyn_aggregate_cell_estate prices the death value's estate offset
      # off mtr_estate_ded read from this static detail. There is no law gate:
      # estate law is always active. The fallback runs on the scenario leg only,
      # since a baseline pass cannot know a kg scenario will read its detail, so
      # baseline rows of a kg runscript have to register estate in mtr_vars.
      if (uses_kg_mech && !('estate' %in% scenario_info$mtr_vars)) {
        tax_units_static$mtr_estate = calc_one_mtr(
          frame   = strip_calc_vars(tax_units_static, drop_mtrs = TRUE),
          actuals = static_actuals,
          var     = 'estate')
        tax_units_static %<>%
          mutate(mtr_estate_ded = estate.income_tax_ded * mtr_estate)
      }
    }

    if (uses_kg_mech) {
      tax_units_static = fold_deemed(tax_units_static)
    }

    write_detail(tax_units_static,
                 file.path(scenario_info$output_path, 'static', 'detail',
                           paste0(year, '.csv')),
                 optional = DETAIL_COLS_OPTIONAL_STATIC)

    static_totals = collect_totals(tax_units_static, year)
  }


  #----------------------------------------------
  # Counterfactual passes after the static one
  #----------------------------------------------

  has_behavior = length(scenario_info$behavior_modules) > 0

  # Totals by slot, filled by whichever pass runs below. An absent slot reads
  # NULL, which is what a pass that writes no totals returns.
  pass_totals = list()

  if (!is.na(pass_name)) {

    spec = PASS_SPECS[[pass_name]]

    config_set_pass(spec$config_pass)

    # Only a pass that applies the haircut reads the deficit state. The no-wealth
    # passes measure the forcing on the base before erosion, which is the frame
    # that does not depend on the deficit.
    apply_haircut    = uses_wealth && spec$haircut
    conv_root        = file.path(scenario_info$output_path, spec$root)
    conv_detail_path = file.path(conv_root, 'detail', paste0(year, '.csv'))

    # Run the calculator whenever this pass can differ from the rung below it:
    # behavior on a pass that takes it, either cross-base channel active, or the
    # employer payroll wage adjustment this pass applies and the static pass does
    # not. A scenario with none of them copies the rung below instead.
    if ((spec$behavior && has_behavior) || uses_wealth || uses_corp ||
        uses_er_payroll) {

      # Inject the frozen-realization carryover and deemed quantities, as the
      # static pass does. The mechanical rung is the static one plus the
      # transmission channels, and the frozen realizations are law arithmetic
      # rather than transmission. The conventional-side passes take the full
      # bathtub state through their behavior module instead.
      conv_base = tax_units
      if (spec$config_pass == 'mechanical' && uses_kg_mech) {
        conv_base = kg_dyn_apply_mech_to_records(conv_base, scenario_info, year)
      }

      # Apply corporate incidence at the head of the pass, before the wealth
      # haircut and the behavior modules, so that the gains and wealth machinery
      # runs on the shocked frame. The step scales the external income lines,
      # accumulating the corp_dY_exog the wealth forcing reads, marks down exposed
      # asset stocks and recomputes net worth, and adjusts gains flows on runs
      # without the gains bathtub. See src/sim/corp/.
      if (uses_corp) {
        corp_check_run_compat(scenario_info, vat_price_offset)
        pre_corp  = conv_base
        conv_base = corp_apply_to_records(
          tax_units          = conv_base,
          paths              = corp_get_paths(scenario_info),
          year               = year,
          kg_dynamics_active = uses_kg_mech)

        # Report the conservation diagnostic, comparing the analytic paths
        # against what the applier realized by differencing the two frames
        if (spec$corp_diag) {
          corp_write_conservation_diag(
            pre = pre_corp, post = conv_base,
            paths = corp_get_paths(scenario_info),
            year = year, conv_root = conv_root)
        }
      }

      # Apply the wealth haircut, before the behavior modules and do_taxes. It
      # drains each record's cell deficit out of the asset stocks, the capital
      # flows and basis, and recomputes net worth, so that calc_estate sees a
      # smaller estate base and calc_wealth reprices on the eroded stock. Records
      # are ranked on net worth before the corporate markdown, which is what the
      # pre-pass computed its cutoffs on.
      if (apply_haircut) {
        wealth_state = read_cohort_state(scenario_info, 'wealth_dynamics_state',
                                         year, pass = spec$state_pass)
        conv_base    = wealth_dyn_apply_to_records(conv_base, wealth_state,
                                                   rank_value = tax_units$net_worth)
      }

      # Run behavioral feedback. With both channels active, the gains modules see
      # the frame after the haircut
      if (spec$behavior && has_behavior) {
        conv_input = conv_base %>%
          do_behavioral_feedback(behavior_modules = scenario_info$behavior_modules,
                                 baseline_mtrs    = baseline_mtrs,
                                 static_mtrs      = static_mtrs_year,
                                 scenario_info    = scenario_info,
                                 indexes          = indexes)
      } else {
        conv_input = conv_base
      }

      # Apply the corporate quantity margin, the buyback-forced sale volume that
      # tracks after-tax payouts. It runs after the bathtub applier, whose
      # realization rule reads MTRs and mortality rather than payout policy. On
      # these runs the price margin enters as a gain-state debit instead.
      if (uses_corp && uses_kg_mech) {
        conv_input = corp_apply_kg_quantity_to_records(
          conv_input, corp_get_paths(scenario_info), year)
      }

      tax_units_conv = conv_input %>%
        do_taxes(baseline_pr_er = baseline_pr_er,
                 vars_1040      = vars_1040,
                 vars_payroll   = return_vars$calc_pr)

      # Record the wage change the employer payroll adjustment produced, which the
      # wealth forcing reads as a change in income from outside the tax system,
      # alongside the corporate channel's dividend cut. do_taxes rescales wages in
      # place, so the change is the difference against the frame it was handed.
      # Cash wages only: the fringe share of the adjustment is in kind and the
      # forcing is a cash flow.
      tax_units_conv$pr_dY_exog = tax_units_conv$wages - conv_input$wages

      # Calculate expected tax on deemed death gains, as on the static pass, and
      # fold it into liability after the MTR block below
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

      # Measure what the wealth bathtub forcing needs: the composition-weighted
      # capital income bundle MTR and its capital total, the marginal wealth rate,
      # and gross assets. All are read off this un-eroded frame, before the deemed
      # fold, so the rate prices the inter-vivos margin alone. The wealth pre-pass
      # reads them from this pass's detail.
      if (spec$measurement) {
        bundle = calc_cap_bundle_mtr(
          tax_units       = conv_input,
          actual_liab_iit = tax_units_conv$liab_iit_net,
          baseline_pr_er  = baseline_pr_er,
          vars_1040       = vars_1040,
          vars_payroll    = return_vars$calc_pr)
        tax_units_conv$mtr_cap_bundle = bundle$mtr_cap_bundle
        tax_units_conv$cap_bundle_F   = bundle$cap_bundle_F
        tax_units_conv$economic_gross = wealth_dyn_economic_gross(conv_input)
        # Store economic net worth before behavior, the variable cells are ranked
        # and scaled by. The wealth avoidance module overwrites net_worth on this
        # frame, but the applier runs before behavior and so ranks on the raw
        # stock; the pre-pass has to rank on the same one.
        stopifnot(identical(tax_units_conv$id, tax_units$id))
        tax_units_conv$net_worth_raw  = tax_units$net_worth
        # conv_input has not been through do_taxes: it carries none of the
        # calculated variables and its wages are not rescaled, so thread
        # baseline_pr_er to apply the same rescale the actuals ran under
        tax_units_conv$mtr_net_worth  = calc_one_mtr(
          frame          = strip_calc_vars(conv_input, strict = FALSE),
          actuals        = mtr_actuals(tax_units_conv),
          var            = 'net_worth',
          baseline_pr_er = baseline_pr_er)
      }

      # Calculate this pass's marginal tax rates. The no-wealth passes need none:
      # the wealth pre-pass reads only the two MTRs measured above
      if (!is.null(scenario_info$mtr_vars) && spec$mtrs) {

        # This frame has been through do_taxes and its wages are already rescaled,
        # so pass baseline_pr_er = NULL rather than the pass-level value
        tax_units_conv = run_mtr_block(taxed          = tax_units_conv,
                                       scenario_info  = scenario_info,
                                       year           = year,
                                       baseline_pr_er = NULL)$taxed
      }

      if (!is.null(conv_liab_deemed)) {
        tax_units_conv = fold_deemed(tax_units_conv, conv_liab_deemed)
      }

      # Write detail. Each pass has its own output tree, so an intermediate pass
      # never overwrites the final conventional detail
      write_detail(tax_units_conv, conv_detail_path,
                   optional = spec$detail_optional)

      # Collect totals. An intermediate pass has none
      if (!is.na(spec$totals_slot)) {
        pass_totals[[spec$totals_slot]] = collect_totals(tax_units_conv, year)
      }

    } else if (scenario_info$ID != 'baseline') {

      # With no behavior and no channel active, this pass equals the rung below,
      # so copy that rung's detail. A 'both' pass has the static frame in memory;
      # a single-pass run copies the CSV an earlier phase wrote, taking the
      # nearest rung below that was actually computed.
      conv_path = conv_detail_path
      if (!is.null(tax_units_static)) {
        write_detail(tax_units_static, conv_path)
      } else {
        below = spec$alias_from %>%
          map_chr(.f = ~ file.path(scenario_info$output_path, .x, 'detail',
                                   paste0(year, '.csv'))) %>%
          keep(.p = file.exists)
        if (length(below) == 0) {
          stop('Pass "', pass_name, '" for scenario "', scenario_info$ID,
               '", year ', year, ' is an alias of the rung below, but none of ',
               paste(spec$alias_from, collapse = ', '), ' has detail to copy.')
        }
        file.copy(below[1], conv_path, overwrite = TRUE)
      }

      # On a 'both' pass the static totals are in scope. On a single-pass run they
      # are NULL, and the caller substitutes the ones the static phase wrote.
      if (!is.na(spec$totals_slot)) {
        pass_totals[[spec$totals_slot]] = static_totals
      }
    }
  }

  # Return required data
  return(list(mtrs                = static_mtrs_year,
              static_totals       = static_totals,
              mechanical_totals   = pass_totals$mechanical_totals,
              conventional_totals = pass_totals$conventional_totals))
}



run_bathtub_pass = function(scenario_info, tax_law,
                            vat_price_offset = NULL) {

  #----------------------------------------------------------------------------
  # Runs the capital gains bathtub pre-pass for one scenario. Aggregates baseline
  # cells from Tax-Data, builds gain-stock-weighted cell rates from the baseline
  # and reform static detail, and runs the year-by-year recurrence. Writes
  # per-year state files under the scenario's
  # conventional/supplemental/kg_dynamics_state/, which the behavior module reads
  # on the conventional pass.
  #
  # Parameters:
  #   - scenario_info (list)        : output of get_scenario_info()
  #   - tax_law (df)                : joined tax law tibble; see build_tax_law()
  #   - vat_price_offset (df)       : VAT price offset tibble, read only to refuse
  #                                   the run when a VAT is active
  #
  # Returns: invisible NULL (writes files as a side effect).
  #----------------------------------------------------------------------------

  kg_dyn_check_run_compat(scenario_info, vat_price_offset)

  # Reuse the frozen pass's Tax-Data sweep where it exists, rather than reading
  # the wide wealth columns a second time
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

  # Build the income-conversion context. The bathtub pass computes per-record
  # conversions each year, injects the cell inflow into the recurrence, and writes
  # the cell tracker into the state files. Pool legs come from raw Tax-Data, and
  # taxable income and the per-leg MTRs from the baseline and scenario static
  # detail.
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
    # Corporate price margin, as a per-year debit to the cell gain state:
    #
    #   D(a, t) = mu(t) * V_corp_exposed(a, t)
    #
    # recomputed from the current markdown each year, and NULL when the corporate
    # channel is inactive
    corp_debit_by_year = corp_kg_state_debit_by_year(scenario_info,
                                                     inputs$baseline_cells),
    sigma_ctx          = sigma_ctx,
    # Wealth tax carrying cost of deferral, per-year cell vectors, all zero when
    # the scenario levies no wealth tax
    reform_carry       = inputs$reform_carry,
    # Estate exposure of the death value, per-year cell vectors of mtr_estate_ded
    # by leg. It enters as (1 - e) on the Bellman death value and on the
    # death-realize term of the equivalent rate.
    baseline_estate    = inputs$baseline_estate,
    reform_estate      = inputs$reform_estate
  )

  invisible(NULL)
}



kg_dyn_check_run_compat = function(scenario_info, vat_price_offset) {

  #----------------------------------------------------------------------------
  # Checks preconditions for the capital gains pre-passes. Both read raw Tax-Data
  # CSVs directly, for the asset, basis and death columns detail files do not
  # carry, so both take the shared raw-dollar channel guard. The bathtub also
  # needs the kg_lt MTR registered.
  #
  # Parameters:
  #   - scenario_info (list)  : output of get_scenario_info()
  #   - vat_price_offset (df) : VAT price offset tibble
  #
  # Returns: invisible TRUE, stopping if a precondition fails.
  #----------------------------------------------------------------------------

  if (is.null(scenario_info$mtr_vars) ||
      !('kg_lt' %in% scenario_info$mtr_vars)) {
    stop('kg_dynamics requires the runscript to register ',
         'mtr_vars = "kg_lt" so the bathtub can read per-cell MTRs from ',
         'static detail. Scenario "', scenario_info$ID, '" does not.')
  }

  check_raw_data_channel_compat('kg_dynamics', scenario_info,
                                vat_price_offset)

  invisible(TRUE)
}



run_frozen_pass = function(scenario_info, tax_law,
                           vat_price_offset = NULL) {

  #----------------------------------------------------------------------------
  # Runs the frozen mechanical pre-pass for one scenario, before the static pass.
  # It needs only Tax-Data cell aggregates and the joined tax law. Writes per-year
  # mechanical state files under the scenario's
  # static/supplemental/kg_dynamics_mech_state/, along with the baseline cells and
  # slim per-record frames run_bathtub_pass reuses in place of its own Tax-Data
  # sweep.
  #
  # Parameters:
  #   - scenario_info (list)  : output of get_scenario_info()
  #   - tax_law (df)          : joined tax law tibble; see build_tax_law()
  #   - vat_price_offset (df) : VAT price offset tibble
  #
  # Returns: invisible NULL (writes files as a side effect).
  #----------------------------------------------------------------------------

  kg_dyn_check_run_compat(scenario_info, vat_price_offset)

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

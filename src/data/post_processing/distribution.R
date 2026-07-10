#----------------------------------------------------------------------------
# distribution.R
#
# Post-processing functions to generate distributional tables for a scenario.
#
# The per-record microdata is built once by build_distribution_microdata()
# (shared with distribution_etrs.R): it reads both legs, enforces the record
# universe, allocates estate tax to heirs, and attaches the capital-STOCK
# allocation bases (from corp_alloc.R) and the income-definition cores. The
# corporate incidence is stock-based (Harberger) with no phase-in -- see
# corp_alloc.R.
#----------------------------------------------------------------------------



build_distribution_tables = function(id, baseline_id) {

  #----------------------------------------------------------------------------
  # Generates distribution tables by year and for a given scenario,
  # both by income and age.
  #
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  #
  # Returns: void.
  #----------------------------------------------------------------------------


  # Get info on VAT, corporate rate, and cost recovery changes
  other_taxes = get_other_taxes(id, baseline_id)

  # One entry per cut of the table: the grouping variable (NULL = whole
  # population), the reported dimension label, and NA handling -- 'keep'
  # groups as-is, 'drop' removes the non-member NA group after aggregation
  # (top-X cuts, where NA shares still enter the group's share-of-total
  # denominator), any other string labels the NA group (negative-rank records)
  dist_cuts = list(
    list(var = NULL,           dim = 'Overall',       na = 'keep'),
    list(var = 'age_group',    dim = 'Age',           na = 'keep'),
    list(var = 'quintile',     dim = 'Income',        na = 'Negative income'),
    list(var = 'top_10',       dim = 'Income',        na = 'drop'),
    list(var = 'top_5',        dim = 'Income',        na = 'drop'),
    list(var = 'top_1',        dim = 'Income',        na = 'drop'),
    list(var = 'top_01',       dim = 'Income',        na = 'drop'),
    list(var = 'top_001',      dim = 'Income',        na = 'drop'),
    list(var = 'parent_group', dim = 'Parent status', na = 'keep'),
    list(var = 'agi_quintile', dim = 'AGI',           na = 'Negative income'),
    list(var = 'agi_top_10',   dim = 'AGI',           na = 'drop'),
    list(var = 'agi_top_5',    dim = 'AGI',           na = 'drop'),
    list(var = 'agi_top_1',    dim = 'AGI',           na = 'drop'),
    list(var = 'agi_top_01',   dim = 'AGI',           na = 'drop'),
    list(var = 'agi_top_001',  dim = 'AGI',           na = 'drop'),
    list(var = 'nw_quintile',  dim = 'Net worth',     na = 'Negative net worth'),
    list(var = 'nw_top_10',    dim = 'Net worth',     na = 'drop'),
    list(var = 'nw_top_5',     dim = 'Net worth',     na = 'drop'),
    list(var = 'nw_top_1',     dim = 'Net worth',     na = 'drop'),
    list(var = 'nw_top_01',    dim = 'Net worth',     na = 'drop'),
    list(var = 'nw_top_001',   dim = 'Net worth',     na = 'drop')
  )

  # Loop over years
  dist_tables = list()
  for (yr in get_scenario_info(id)$dist_years) {

    # Process microdata
    microdata = process_for_distribution(id, baseline_id, yr, other_taxes)

    # Calculate metrics for each cut
    dist_tables[[as.character(yr)]] = dist_cuts %>%
      map(.f = function(cut) {
        group_vals = if (is.null(cut$var)) 'Overall' else microdata[[cut$var]]
        if (!(cut$na %in% c('keep', 'drop'))) {
          group_vals = replace_na(group_vals, cut$na)
        }
        microdata %>%
          group_by(taxes_included, group = group_vals) %>%
          calc_dist_metrics() %>%
          { if (cut$na == 'drop') filter(., !is.na(group)) else . } %>%
          mutate(group_dimension = cut$dim)
      }) %>%
      bind_rows() %>%

      # Add year indicator
      mutate(year = yr, .before = everything())
  }


  # Combine and write results
  dist_tables %>%
    bind_rows() %>%
    arrange(year, taxes_included) %>%
    select(year, taxes_included, group_dimension, everything()) %>%
    write_csv(file.path(globals$output_root, id, 'static/supplemental', paste0('distribution.csv')))
}



build_distribution_microdata = function(id, baseline_id, yr, other_taxes,
                                        write_supplemental = TRUE,
                                        reform_leg = 'static') {

  #----------------------------------------------------------------------------
  # Reads and cleans input data for a scenario and its "baseline", producing
  # the per-record microdata frame shared by the distribution delta table
  # (process_for_distribution) and the ETR levels table (process_for_etrs).
  #
  # This is everything EXCEPT the tax-inclusion tiers: both legs are read, the
  # record universe is enforced, estate tax is allocated to heirs, the heir
  # copy-split is applied, deemed-realization tax is reattributed to heirs, and
  # reform amounts are expressed in baseline (consumer) dollars. It also
  # attaches the capital-STOCK allocation bases (corp_alloc.R) and the three
  # income-definition cores (AGI / expanded / Haig-Simons accrual).
  #
  # Callers add the tax-inclusion tiers and the corporate-tax allocation on top
  # of the returned per-record frame.
  #
  # Parameters:
  #   - id          (str)  : scenario ID
  #   - baseline_id (str)  : ID of scenario against which metrics are calculated.
  #                          For regular tables, the actual baseline; for stacked
  #                          tables, the preceding scenario.
  #   - yr          (int)  : year to calculate metrics for
  #   - other_taxes (df)   : tibble of CIT/VAT metrics (see get_other_taxes())
  #   - write_supplemental (lgl) : whether to write the per-year estate heir
  #                          liabilities and allocator diagnostics (TRUE for the
  #                          delta table; FALSE for the ETR table, which reuses
  #                          the same allocation but must not re-emit the files)
  #   - reform_leg  (str)  : which run-type leg supplies the REFORM tax
  #                          numerators: 'static' (default, the law-only ask)
  #                          or 'conventional' (realized, with behavior). The
  #                          baseline leg, income cores, rankings, and stock
  #                          bases are baseline-static regardless — swapping
  #                          the leg swaps NUMERATORS only (realized-ETR
  #                          variant, VISION §"welfare vs realized")
  #
  # Returns: ungrouped per-record microdata (df), post heir copy-split.
  #----------------------------------------------------------------------------


  # Read baseline microdata. liab_deemed (tax on deemed realization at death,
  # kg_dynamics scenarios only) is stripped from decedent records here and
  # reattributed to heirs below
  baseline_detail = read_static_detail(baseline_id, yr)
  if (!('liab_deemed' %in% names(baseline_detail))) {
    baseline_detail$liab_deemed = 0
  }
  # Annual wealth tax: a living tax borne by the owner's own record (like income
  # tax), 0 under baseline law. Default for detail predating the wealth column.
  if (!('liab_wealth' %in% names(baseline_detail))) {
    baseline_detail$liab_wealth = 0
  }
  # Economic net worth, the grouping variable for the by-wealth distribution
  # view (baseline stock, like income/AGI cuts use baseline income). Default 0
  # for detail predating the column.
  #
  # Wealth-bathtub schema note (src/sim/wealth_dynamics.R): distribution stays
  # STATIC-sourced (D20), so net_worth here is the UN-ERODED stock. Under the
  # channel (s > 0), CONVENTIONAL detail's net_worth is POST-haircut (~1% lower)
  # while STATIC is un-haircut; the erosion surfaces via receipts, not the
  # distribution tables (the ~1% haircut barely moves rank-order). The by-wealth
  # ranking keeps net_worth >= 0 (zero-NW kept) here, which DIFFERS from the
  # bathtub's cell ranking (net_worth > 0, zero excluded; plan D17) -- the
  # bathtub deliberately drops zero-NW records (no stock to draw, no estate).
  if (!('net_worth' %in% names(baseline_detail))) {
    baseline_detail$net_worth = 0
  }

  # Per-record capital-stock allocation bases + Haig-Simons income pieces, from
  # the BASELINE leg's raw Tax-Data (scenario-invariant balance sheet)
  stock_keys = read_corp_alloc_stock_keys(baseline_id, yr)

  microdata = baseline_detail %>%

    # Remove dependent returns and extraneous variables for distribution calculation
    filter(dep_status == 0) %>%
    mutate(
      year          = yr,
      n_people      = 1 + as.integer(filing_status == 2) + n_dep,
      age           = if_else(filing_status == 2, pmax(age1, age2), age1),
      labor         = pmax(0, wages + (sole_prop + part_scorp + farm) * 0.8),
      capital       = pmax(0, (sole_prop + part_scorp + farm) * 0.2 + txbl_int + exempt_int + div_ord + div_pref + kg_st + kg_lt),
      liab_iit_pr   = liab_iit_net + liab_pr - liab_deemed,
      parent_group  = if_else(
        (!is.na(dep_age1) & dep_age1 < 18) |
        (!is.na(dep_age2) & dep_age2 < 18) |
        (!is.na(dep_age3) & dep_age3 < 18),
        'Parent', 'Non-parent'
      ),
      age_group = case_when(
        age < 30 ~ '29 and under',
        age < 40 ~ '30 - 39',
        age < 50 ~ '40 - 49',
        age < 65 ~ '50 - 64',
        T        ~ '65+'
      )
    )

  # The stock/HS keys must cover every filer: a missing id would flow NA into an
  # allocation base or an income definition and silently poison group sums.
  # Fail loudly (mirror the record-universe guard below).
  missing_keys = setdiff(microdata$id, stock_keys$id)
  if (length(missing_keys) > 0) {
    stop('distribution: ', length(missing_keys), ' records in ', yr,
         ' baseline detail are missing from the baseline Tax-Data file; the ',
         'capital-stock / Haig-Simons allocation bases cannot be built.')
  }

  microdata %<>%
    left_join(stock_keys, by = 'id') %>%
    mutate(
      # Income-definition cores (baseline-sourced; pre-gross-up, pre-inheritance).
      #   AGI      : statutory adjusted gross income
      #   expanded : gross realized income + employer payroll (do_taxes.R:182)
      #   HS       : expanded income with realized gains swapped for accruals and
      #              the DC accrual/withdrawal double-count removed (subtract
      #              txbl_ira_dist in full and the DC share of pension
      #              distributions; DB stays -- it has no accruals.* counterpart)
      inc_agi_core = agi,
      inc_exp_core = expanded_inc,
      inc_hs_core  = expanded_inc - (kg_st + kg_lt + other_gains) + accruals_sum -
                     txbl_ira_dist - gross_pens_dist * dc_share
    ) %>%
    select(year, id, weight, n_people, filing_status, age, age_group, parent_group,
           labor, capital, agi, net_worth,
           income = expanded_inc,
           inc_agi_core, inc_exp_core, inc_hs_core,
           corp_equity, net_capital, net_worth_stock,
           liab_iit_net, liab_pr, liab_deemed, liab_wealth, liab_iit_pr)

  # Read counterfactual reform scenario tax microdata, stripping deemed
  # realization tax from decedents same as the baseline leg above
  reform_detail = read_static_detail(id, yr, leg = reform_leg)
  if (!('liab_deemed' %in% names(reform_detail))) {
    reform_detail$liab_deemed = 0
  }
  if (!('liab_wealth' %in% names(reform_detail))) {
    reform_detail$liab_wealth = 0
  }

  # The two legs must share the same record universe: a baseline id missing
  # from the reform detail would flow NA through liab_delta and silently
  # poison every group aggregate (no na.rm downstream), and a reform-only id
  # would be silently dropped by the join. Mismatches mean the legs come from
  # incompatible vintages (e.g. different sample universes) -- fail loudly
  base_ids   = unique(microdata$id)
  reform_ids = reform_detail$id[reform_detail$dep_status == 0]
  if (length(setdiff(base_ids, reform_ids)) > 0 |
      length(setdiff(reform_ids, base_ids)) > 0) {
    stop('distribution: baseline and scenario ', yr, ' detail files do not ',
         'share the same record universe (',
         length(setdiff(base_ids, reform_ids)), ' baseline-only ids, ',
         length(setdiff(reform_ids, base_ids)), ' scenario-only ids). ',
         'The legs come from incompatible vintages; re-run against a ',
         'consistent baseline.')
  }

  microdata %<>%
    left_join(
      reform_detail %>%
        mutate(liab_iit_pr_reform = liab_iit_net + liab_pr - liab_deemed) %>%
        select(id,
               liab_iit_net_reform = liab_iit_net,
               liab_pr_reform      = liab_pr,
               liab_deemed_reform  = liab_deemed,
               liab_wealth_reform  = liab_wealth,
               liab_iit_pr_reform),
      by = 'id'
    )

  # Estate tax incidence: heir structure (p_inheritance, inheritance) comes
  # from the BASELINE Estate-Tax-Distribution interface; liability comes from
  # the on-model rank-matching allocator (estate_allocator.R), run
  # independently on each leg's detail file so reform estate law flows
  # through to heirs. Inheritance is GROSS of estate tax (scenario-invariant);
  # only the liability column differs across legs. No scenario-specific
  # upstream file is needed anymore
  baseline_estate_path = interface_root('Estate-Tax-Distribution') %>%
    file.path(paste0('estate_tax_detail_', yr, '.csv'))

  have_estate_cols = all(ESTATE_DETAIL_COLS %in% names(baseline_detail)) &
                     all(ESTATE_DETAIL_COLS %in% names(reform_detail))

  if (file.exists(baseline_estate_path) & have_estate_cols) {

    heir_px = baseline_estate_path %>%
      fread() %>%
      tibble() %>%
      select(id, p_inheritance, inheritance)

    alloc_baseline = allocate_estate_to_heirs(baseline_detail, heir_px, yr, baseline_id)
    alloc_reform   = allocate_estate_to_heirs(reform_detail,   heir_px, yr, id)

    # Persist this scenario's heir-level liabilities (4-column upstream
    # schema) and the allocator diagnostics, per year for idempotence. Only the
    # delta-table caller writes these; the ETR table reuses the same allocation
    # but must not re-emit the files (write_supplemental = FALSE).
    if (write_supplemental) {
      supp_root = file.path(globals$output_root, id, 'static/supplemental')
      heir_px %>%
        left_join(alloc_reform$heirs, by = 'id') %>%
        write_csv(file.path(supp_root, paste0('estate_tax_detail_', yr, '.csv')))
      bind_rows(alloc_baseline$diag, alloc_reform$diag) %>%
        write_csv(file.path(supp_root, paste0('estate_allocator_diag_', yr, '.csv')))
    }

    microdata %<>%
      left_join(
        heir_px %>%
          left_join(alloc_baseline$heirs, by = 'id') %>%
          rename(liab_estate = estate_tax_liability),
        by = 'id'
      ) %>%
      left_join(
        alloc_reform$heirs %>%
          rename(liab_estate_reform = estate_tax_liability),
        by = 'id'
      ) %>%
      mutate(inheritance_reform = inheritance)

  } else {
    if (file.exists(baseline_estate_path) & !have_estate_cols) {
      warning('Estate-Tax-Distribution data exists for ', yr, ' but the ',
              'detail files predate the on-model estate columns; estate tax ',
              'is excluded from these distribution tables')
    }
    microdata %<>%
      mutate(
        p_inheritance      = 0,
        inheritance        = 0,
        inheritance_reform = 0,
        liab_estate        = 0,
        liab_estate_reform = 0
      )
  }

  # Records absent from the Estate-Tax-Distribution detail (e.g. ids new to a
  # later Tax-Data vintage) get NA from the join; treat them as non-heirs and
  # keep them in the table (NA weight would silently drop them) but warn
  n_unmatched = n_distinct(microdata$id[is.na(microdata$p_inheritance)])
  if (n_unmatched > 0) {
    warning(n_unmatched, ' records in ', yr, ' detail are missing from the ',
            'Estate-Tax-Distribution detail file; treating as non-heirs')
    microdata %<>%
      mutate(
        across(
          .cols = c(p_inheritance, starts_with('inheritance'), starts_with('liab_estate')),
          .fns  = ~ replace_na(., 0)
        )
      )
  }

  # Heir reattribution of deemed realization tax requires inheritance data:
  # fail loudly rather than silently dropping decedents' stripped liab_deemed
  if (sum((microdata$liab_deemed + microdata$liab_deemed_reform) * microdata$weight, na.rm = T) > 0) {
    if (sum(microdata$inheritance * microdata$p_inheritance * microdata$weight) <= 0) {
      stop('Deemed realization tax present in ', yr, ' but no Estate-Tax-',
           'Distribution inheritance data is available for heir reattribution')
    }
  }

  microdata %<>%

    # Split records based on probability of inheritance
    expand_grid(copy_id = 1:2) %>%
    mutate(
      weight = weight * if_else(copy_id == 1, p_inheritance, 1 - p_inheritance),
      across(
        .cols = c(starts_with('inheritance'), starts_with('liab_estate')),
        .fns  = ~ . * (copy_id == 1)
      )
    ) %>%
    filter(weight > 0) %>%
    select(-p_inheritance) %>%

    # Join other taxes (VAT price offset, corporate deltas)
    left_join(other_taxes, by = 'year') %>%
    mutate(

      # Express counterfactual reform variables in baseline dollars to account
      # for VAT (all reform tax amounts and inheritance are deflated together)
      income_reform = income,
      across(.cols = ends_with('_reform'), .fns  = ~ . / vat_price_offset),

      # Reattribute deemed realization tax from decedents to heir copies in
      # proportion to inheritance, revenue-neutral within year. National
      # aggregate, invariant to the tax-inclusion tier -- computed once here.
      # Like the estate tax it enters only the estate-inclusive presentations;
      # no income is reattributed (the deemed gain accrued to the decedent)
      liab_deemed_heir = if_else(
        inheritance > 0,
        sum(liab_deemed * weight) * inheritance / sum(inheritance * weight),
        0
      ),
      liab_deemed_heir_reform = if_else(
        inheritance > 0,
        sum(liab_deemed_reform * weight) * inheritance / sum(inheritance * weight),
        0
      )
    )

  return(microdata)
}



process_for_distribution = function(id, baseline_id, yr, other_taxes) {

  #----------------------------------------------------------------------------
  # Builds the per-record microdata (build_distribution_microdata) and adds the
  # tax-inclusion tiers, the stock-based corporate-tax allocation, and the
  # rank-group columns for the delta distribution table.
  #
  # Parameters:
  #   - id          (str) : scenario ID
  #   - baseline_id (str) : ID of scenario against which metrics are calculated.
  #                         For regular tables, this is the actual baseline; for
  #                         stacked tables, this is the preceding scenario
  #   - yr          (int) : year to calculate metrics for
  #   - other_taxes (df)  : tibble of metrics for CIT and VAT (see
  #                         get_other_taxes())
  #
  # Returns: microdata with all record-level variables required to calculate
  #          aggregate distributional metrics (df).
  #----------------------------------------------------------------------------

  microdata = build_distribution_microdata(id, baseline_id, yr, other_taxes,
                                           write_supplemental = TRUE)

  # Stock-based corporate incidence (equity_supernormal headline convention),
  # replacing the legacy capital-INCOME allocation and the labor-share phase-in
  # (corp_alloc.R). sigma_N by provision type: the off-model rate delta at the
  # sigma_N knob (0.375 default), the cost-recovery delta at sigma_N = 1 (pure
  # normal: no supernormal leg, 50/50 labor/all-capital). The foreign-borne
  # haircut on the capital legs happens inside allocate_corp_dollars. National
  # aggregate, invariant to the tax-inclusion tier -- computed once, ungrouped.
  sigma_n = corp_env_knobs()$sigma_n

  microdata %>%
    mutate(
      liab_corp =
        allocate_corp_dollars(other_corp_delta,    sigma_n, weight, labor,
                              base_supernormal = corp_equity,
                              base_normal      = net_capital) +
        allocate_corp_dollars(cost_recovery_delta, 1,       weight, labor,
                              base_supernormal = corp_equity,
                              base_normal      = net_capital)
    ) %>%

    # Make 3 copies for tax-type inclusion assumptions
    expand_grid(taxes_included = c('iit_pr_wealth', 'iit_pr_death_wealth', 'iit_pr_death_cit_vat_wealth')) %>%
    group_by(taxes_included) %>%
    mutate(

      # Add inheritance to income for estate tax-inclusive assumption scenarios
      income        = income        + inheritance        * (taxes_included %in% c('iit_pr_death_wealth', 'iit_pr_death_cit_vat_wealth')),
      income_reform = income_reform + inheritance_reform * (taxes_included %in% c('iit_pr_death_wealth', 'iit_pr_death_cit_vat_wealth')),

      # VAT burden is the loss of real income from higher prices. Some components
      # of expanded income will rise with prices (e.g. OASDI or capital income),
      # others won't; compositional differences determine distributional impact
      liab_vat = income - income_reform,

      # Calculate liability under each scenario. The annual wealth tax is a
      # living tax attached to the owner's OWN record (like income tax), so it
      # enters every presentation tier directly -- NOT through the estate heir
      # allocator (whose rank match exists only because estate lands on a
      # decedent != beneficiary). Baseline liab_wealth is 0, so the wealth-tax
      # burden falls out of liab_reform - liab.
      liab = case_when(
        taxes_included == 'iit_pr_wealth'               ~ liab_iit_pr + liab_wealth,
        taxes_included == 'iit_pr_death_wealth'         ~ liab_iit_pr + liab_wealth + liab_estate + liab_deemed_heir,
        taxes_included == 'iit_pr_death_cit_vat_wealth' ~ liab_iit_pr + liab_wealth + liab_estate + liab_deemed_heir
      ),
      liab_reform = case_when(
        taxes_included == 'iit_pr_wealth'               ~ liab_iit_pr_reform + liab_wealth_reform,
        taxes_included == 'iit_pr_death_wealth'         ~ liab_iit_pr_reform + liab_wealth_reform + liab_estate_reform + liab_deemed_heir_reform,
        taxes_included == 'iit_pr_death_cit_vat_wealth' ~ liab_iit_pr_reform + liab_wealth_reform + liab_estate_reform + liab_deemed_heir_reform + liab_corp + liab_vat
      ),

      # Calculate change in tax liability
      liab_delta = liab_reform - liab,

      # Calculate after-tax income in both scenarios
      ati        = income        - liab,
      ati_reform = income_reform - liab_reform,

    ) %>%

    # Add income-based percentile measures (age group already built upstream)
    add_rank_groups('income', 'income_pctile') %>%

    # Add AGI-based income percentile measures
    add_rank_groups('agi', 'agi_pctile', 'agi_') %>%

    # Add net-worth-based percentile measures. This is the natural lens for a
    # wealth tax — it ranks by the balance sheet, not the income statement, so a
    # wealthy-but-low-income unit (e.g. a retiree living off principal) lands in
    # the right group. Computed on baseline economic net worth, mirroring the
    # income/AGI cuts above (only the non-negative-net-worth population is
    # ranked). Produced for every scenario; most informative for wealth reforms.
    add_rank_groups('net_worth', 'nw_pctile', 'nw_') %>%

    ungroup() %>%
    return()
}



calc_dist_metrics = function(grouped_microdata) {

  #----------------------------------------------------------------------------
  # Aggregates record-level tax change microdata into summary stats, grouped
  # by tax inclusion scenario and either income or age.
  #
  # Parameters:
  #  - grouped_microdata (df) : output of  process_for_distribution()
  #
  # Returns: tibble of distributional metrics
  #----------------------------------------------------------------------------

  # Calculate metrics by specified group
  grouped_microdata %>%
    summarise(

      # Group-metric-specific summary stats (lower edge of each group on each
      # ranking variable; net_worth_cutoff is the meaningful one for the
      # 'Net worth' dimension, as income/agi cutoffs are for their dimensions)
      income_cutoff    = round(min(income) / 5) * 5,
      agi_cutoff       = round(min(agi) / 5) * 5,
      net_worth_cutoff = round(min(net_worth)),
      n_tax_units      = sum(weight),

      # Labor/capital/ATI for spending distribution
      labor        = sum(labor * weight) / 1e9,
      capital      = sum(capital * weight) / 1e9,
      ati_baseline = sum(ati * weight) / 1e9,

      # Unconditional and conditional averages
      avg       = round(weighted.mean(liab_delta, weight) / 5) * 5,
      avg_cut   = round(weighted.mean(liab_delta, (weight * (liab_delta <= -5))) / 5) * 5,
      avg_raise = round(weighted.mean(liab_delta, (weight * (liab_delta >= 5)))  / 5) * 5,

      # Relative changes
      pct_chg_ati = sum(ati_reform * weight) / sum(ati * weight) - 1,

      # Counts
      share_cut.5      = sum(weight * (liab_delta <= -5))    / sum(weight),
      share_cut.100    = sum(weight * (liab_delta <= -100))  / sum(weight),
      share_cut.500    = sum(weight * (liab_delta <= -500))  / sum(weight),
      share_cut.1000   = sum(weight * (liab_delta <= -1000)) / sum(weight),
      share_cut.5000   = sum(weight * (liab_delta <= -5000)) / sum(weight),
      share_raise.5    = sum(weight * (liab_delta >= 5))     / sum(weight),
      share_raise.100  = sum(weight * (liab_delta >= 100))   / sum(weight),
      share_raise.500  = sum(weight * (liab_delta >= 500))   / sum(weight),
      share_raise.1000 = sum(weight * (liab_delta >= 1000))  / sum(weight),
      share_raise.5000 = sum(weight * (liab_delta >= 5000))  / sum(weight),

      # Income group's total dollar amount tax change
      net_change = sum(round(liab_delta) * weight) / 1e9,

      .groups = 'drop_last'
    ) %>%

    # Group's share of total change
    mutate(share_net_change = net_change / sum(net_change)) %>%
    ungroup() %>%
    return()
}



get_other_taxes = function(id, baseline_id) {

  #----------------------------------------------------------------------------
  # Gets time series of aggregate effects for VAT changes, corporate tax rate
  # changes, and changes to cost recovery rules.
  #
  # Parameters:
  #   - id (str)          : counterfactual scenario ID
  #   - baseline_id (str) : ID of scenario against which changes are measured
  #
  # Returns: list of three dataframes: VAT price effecvt, corporate rate
  #.         delta, and cost recovery delta (lst).
  #----------------------------------------------------------------------------

  # Get scenario info for counterfactual scenario
  scenario_info = get_scenario_info(id)
  first_year    = min(scenario_info$years)
  last_year     = max(scenario_info$years)


  #-----------------
  # Value added tax
  #-----------------

  # Read VAT price offset for deflating other taxes
  vat_price_offset = read_vat_offset(id) %>%
    select(year, vat_price_offset = cpi_factor)


  #-----------------------
  # Cost recovery changes
  #-----------------------

  # Get corporate tax rate by year
  corp_rate = file.path(globals$output_root, id, 'static/supplemental/tax_law.csv') %>%
    read_csv(show_col_types = F) %>%
    distinct(year, corp.rate)

  # Read recovery ratios by legal form
  cost_recovery_delta = interface_root('Cost-Recovery-Simulator') %>%
    file.path('totals/recovery_ratios_form.csv') %>%
    read_csv(show_col_types = F) %>%
    mutate(policy = 'baseline') %>%
    bind_rows(
      scenario_info$interface_paths$`Cost-Recovery-Simulator` %>%
        file.path('totals/recovery_ratios_form.csv') %>%
        read_csv(show_col_types = F) %>%
        mutate(policy = 'scenario')
    ) %>%
    filter(year >= first_year, year <= last_year) %>%
    pivot_wider(
      names_from  = c(policy, form),
      values_from = c(investment, real, pv)
    ) %>%

    # Calculate implied long-run revenue loss
    left_join(corp_rate, by = 'year') %>%
    mutate(
      cost_recovery_delta = investment_baseline_ccorp * (pv_scenario_ccorp - pv_baseline_ccorp) * -corp.rate
    ) %>%
    select(year, cost_recovery_delta)


  #-----------------------------
  # Other corporate tax changes
  #-----------------------------

  # Read baseline off-model revenue deltas (0 if actual baseline). The corporate
  # allocation (corp_alloc.R) applies a fixed normal/supernormal split with no
  # labor-share phase-in, so only the total delta is needed here.
  other_corp_delta = interface_root('Off-Model-Estimates', baseline_id) %>%
    file.path('revenues.csv') %>%
    read_csv(show_col_types = F) %>%
    select(year, baseline = corporate) %>%
    filter(year >= first_year, year <= last_year) %>%

    # Read counterfactual scenario off-model revenues
    left_join(
      scenario_info$interface_paths$`Off-Model-Estimates` %>%
        file.path('revenues.csv') %>%
        read_csv(show_col_types = F) %>%
        select(year, reform = corporate),
      by = 'year'
    ) %>%

    # Express corporate tax in baseline (consumer) dollars
    left_join(vat_price_offset, by = 'year') %>%
      mutate(
        reform           = reform / vat_price_offset,
        other_corp_delta = reform - baseline
      ) %>%
      select(year, other_corp_delta)

  # Combine and return
  vat_price_offset %>%
    left_join(cost_recovery_delta, by = 'year') %>%
    left_join(other_corp_delta,    by = 'year') %>%
    return()
}

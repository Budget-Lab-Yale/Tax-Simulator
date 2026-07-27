#----------------------------------------------------------------------------
# distribution.R
#
# Post-processing functions to generate distributional tables for a scenario
#
# build_distribution_microdata builds the per-record frame both this file and
# distribution_etrs.R work from: it reads the two legs, enforces the record
# universe, allocates estate tax to heirs, and attaches the capital stock
# allocation bases and the income definition cores. Corporate incidence is
# allocated on stocks, with no phase-in; see corp_alloc.R.
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

  # One entry per cut of the table: the grouping variable, NULL for the whole
  # population; the reported dimension label; and how to handle the NA group.
  # 'keep' groups it as it is, 'drop' removes it after aggregation, so that on a
  # top-X cut its shares still enter the denominator, and any other string labels
  # it, for the records of negative rank.
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
  # Reads and cleans input data for a scenario and its "baseline", producing the
  # per-record microdata frame the delta table (process_for_distribution) and the
  # effective rate levels table (process_for_etrs) share.
  #
  # It reads both legs, enforces the record universe, allocates estate tax to
  # heirs, applies the heir copy-split, reattributes deemed realization tax to
  # heirs, and expresses reform amounts in baseline dollars. It also attaches the
  # capital stock allocation bases and the three income definition cores, being
  # adjusted gross, expanded, and Haig-Simons accrual income. Callers add the
  # tax-inclusion tiers and the corporate tax allocation themselves.
  #
  # Parameters:
  #   - id          (str)  : scenario ID
  #   - baseline_id (str)  : ID of scenario against which metrics are calculated.
  #                          For regular tables, the actual baseline; for stacked
  #                          tables, the preceding scenario.
  #   - yr          (int)  : year to calculate metrics for
  #   - other_taxes (df)   : tibble of CIT/VAT metrics (see get_other_taxes())
  #   - write_supplemental (lgl) : whether to write the per-year estate heir
  #                          liabilities and allocator diagnostics. The delta
  #                          table writes them; the effective rate table reuses
  #                          the same allocation and does not re-emit them
  #   - reform_leg  (str)  : which leg supplies the reform tax numerators,
  #                          'static' for the law-only measure or 'conventional'
  #                          for the realized one. The baseline leg, the income
  #                          cores, the rankings and the stock bases come from the
  #                          baseline static leg either way, so the leg changes
  #                          the numerators alone
  #
  # Returns: ungrouped per-record microdata after the heir copy-split (df).
  #----------------------------------------------------------------------------


  # Read baseline microdata. Tax on deemed realization at death is stripped from
  # decedent records here and reattributed to heirs below
  baseline_detail = read_static_detail(baseline_id, yr)
  if (!('liab_deemed' %in% names(baseline_detail))) {
    baseline_detail$liab_deemed = 0
  }
  # The annual wealth tax is borne by the owner's own record, like income tax, and
  # is zero under baseline law. Default it for detail predating the column
  if (!('liab_wealth' %in% names(baseline_detail))) {
    baseline_detail$liab_wealth = 0
  }
  # Economic net worth is the grouping variable for the net worth cuts, taken from
  # the baseline stock as the income and AGI cuts take baseline income. Default it
  # to zero for detail predating the column.
  #
  # These tables read static detail, so this is the stock before the wealth
  # bathtub's erosion. Conventional detail carries net worth about a percent lower
  # under the channel, which barely moves rank order; the erosion is reported
  # through receipts instead. The ranking here keeps records of zero net worth,
  # unlike the bathtub's own cell ranking, which has no stock to draw on for them.
  if (!('net_worth' %in% names(baseline_detail))) {
    baseline_detail$net_worth = 0
  }

  # Read the capital stock allocation bases and the Haig-Simons income pieces from
  # the baseline leg's raw Tax-Data. The balance sheet does not vary by scenario
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

  # The keys must cover every filer. A missing id would flow NA into an allocation
  # base or an income definition and poison the group sums
  missing_keys = setdiff(microdata$id, stock_keys$id)
  if (length(missing_keys) > 0) {
    stop('distribution: ', length(missing_keys), ' records in ', yr,
         ' baseline detail are missing from the baseline Tax-Data file; the ',
         'capital-stock / Haig-Simons allocation bases cannot be built.')
  }

  microdata %<>%
    left_join(stock_keys, by = 'id') %>%
    mutate(
      # The three income definitions, taken from the baseline leg before the
      # gross-up and before inheritance:
      #
      #   AGI          : statutory adjusted gross income
      #   expanded     : gross realized income plus employer payroll, per do_taxes.R
      #   Haig-Simons  : expanded income with realized gains replaced by accruals,
      #                  less the defined-contribution accrual and withdrawal
      #                  double-count. Defined benefit plans have no accruals
      #                  counterpart and so stay.
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

  # Read counterfactual reform scenario tax microdata, stripping deemed realization
  # tax from decedents as on the baseline leg above
  reform_detail = read_static_detail(id, yr, leg = reform_leg)
  if (!('liab_deemed' %in% names(reform_detail))) {
    reform_detail$liab_deemed = 0
  }
  if (!('liab_wealth' %in% names(reform_detail))) {
    reform_detail$liab_wealth = 0
  }

  # The two legs must share the same record universe. A baseline id missing from
  # the reform detail would flow NA through the liability delta into every group
  # aggregate, and a reform-only id would be dropped by the join. A mismatch means
  # the legs come from incompatible vintages.
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

  # Allocate estate tax to heirs. The heir structure comes from the baseline
  # Estate-Tax-Distribution interface, and liability from the rank-matching
  # allocator in estate_allocator.R, run on each leg's detail file so that reform
  # estate law reaches heirs. Inheritance is gross of estate tax and does not vary
  # by scenario, so only the liability column differs between the legs.
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

    # Write this scenario's heir-level liabilities, in the upstream interface's
    # schema, and the allocator diagnostics, one file per year
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

  # Records absent from the Estate-Tax-Distribution detail, such as ids new to a
  # later Tax-Data vintage, come out of the join with NA. Treat them as non-heirs
  # and keep them in the table, since an NA weight would drop them
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

  # Reattributing deemed realization tax to heirs needs inheritance data. Stop
  # rather than drop the liability stripped from decedents above
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
      # proportion to inheritance, holding the annual total fixed. Like the estate
      # tax it enters only the presentations that include tax at death. No income
      # moves with it: the deemed gain accrued to the decedent.
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
  # Builds the per-record microdata and adds the tax-inclusion tiers, the
  # stock-based corporate tax allocation, and the rank-group columns the delta
  # distribution table needs.
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

  # Allocate the corporate tax on stocks, under the supernormal-equity convention
  # in corp_alloc.R. The normal-return share sigma_N varies by provision type: the
  # off-model rate delta takes the configured value, 0.375 by default, and the
  # cost-recovery delta takes 1, which is pure normal return and so splits half to
  # labor and half across all capital. allocate_corp_dollars applies the
  # foreign-borne haircut to the capital legs.
  sigma_n = corp_env_knobs()$sigma_n

  microdata %>%
    mutate(
      liab_corp =
        allocate_corp_dollars(other_corp_delta,        sigma_n, weight, labor,
                              base_supernormal = corp_equity,
                              base_normal      = net_capital) +
        allocate_corp_dollars(cost_recovery_delta,     1,       weight, labor,
                              base_supernormal = corp_equity,
                              base_normal      = net_capital) +
        # The on-model statutory rate delta hits normal and supernormal returns
        # alike, so it is allocated at the configured sigma_N, as the off-model
        # corporate delta is
        allocate_corp_dollars(corp_rate_static_delta,  sigma_n, weight, labor,
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

      # Calculate liability under each scenario. The annual wealth tax attaches to
      # the owner's own record, like income tax, so it enters every tier directly
      # rather than through the heir allocator, which exists because the estate tax
      # falls on a decedent and not on the beneficiary. Baseline wealth tax is
      # zero, so the burden falls out of the reform less the baseline.
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

    # Add net worth percentile measures, the natural cut for a wealth tax: they
    # rank on the balance sheet rather than the income statement, so a retiree
    # living off principal lands in the right group. Computed on baseline economic
    # net worth, as the income and AGI cuts above are, and ranking the
    # non-negative-net-worth population only.
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

      # Lower edge of each group on each ranking variable. Each dimension reads
      # its own cutoff
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
  # Returns: tibble of the VAT price effect, the cost recovery delta, the
  #          off-model corporate delta, and the statutory rate delta, by year (df).
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

  # Read baseline off-model revenue deltas, zero against the actual baseline. The
  # allocation in corp_alloc.R splits the total between normal and supernormal
  # returns, so only the total delta is needed here.
  #
  # The distribution table is a static concept, so both legs read the static
  # corporate stream through ome_corp_col rather than the conventional column the
  # on-model incidence channel reads.
  other_corp_delta = interface_root('Off-Model-Estimates', baseline_id) %>%
    file.path('revenues.csv') %>%
    read_csv(show_col_types = F) %>%
    ome_corp_col(static = TRUE) %>%
    rename(baseline = corporate) %>%
    filter(year >= first_year, year <= last_year) %>%

    # Read counterfactual scenario off-model revenues
    left_join(
      scenario_info$interface_paths$`Off-Model-Estimates` %>%
        file.path('revenues.csv') %>%
        read_csv(show_col_types = F) %>%
        ome_corp_col(static = TRUE) %>%
        rename(reform = corporate),
      by = 'year'
    ) %>%

    # Express corporate tax in baseline (consumer) dollars
    left_join(vat_price_offset, by = 'year') %>%
      mutate(
        reform           = reform / vat_price_offset,
        other_corp_delta = reform - baseline
      ) %>%
      select(year, other_corp_delta)

  #------------------------------------------
  # On-model corporate statutory rate change
  #------------------------------------------

  # Calculate the static statutory rate revenue delta from the rate series and the
  # CBO corporate receipts level; see src/sim/corp_rate.R. The distribution table
  # is a static concept, so this carries no base erosion.
  # It stands in for the rate portion of the off-model corporate delta, which the
  # Off-Model-Estimates vintage must therefore be generated without.
  corp_rate_static_delta = corp_rate_delta(
    rate_series = corp_rate_read_series(
      file.path(globals$output_root, id, 'static/supplemental/tax_law.csv')),
    rev_corp = read_macro_spliced(scenario_info$interface_paths$`Macro-Projections`) %>%
      distinct(year, .keep_all = TRUE) %>%
      transmute(year, rev_corp),
    static = TRUE
  ) %>%
    filter(year >= first_year, year <= last_year) %>%
    rename(corp_rate_static_delta = delta)

  # Combine and return
  vat_price_offset %>%
    left_join(cost_recovery_delta,     by = 'year') %>%
    left_join(other_corp_delta,        by = 'year') %>%
    left_join(corp_rate_static_delta,  by = 'year') %>%
    return()
}

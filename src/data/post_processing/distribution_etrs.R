#----------------------------------------------------------------------------
# distribution_etrs.R
#
# Contains functions to build the effective tax rate tables
#-------------------------------------------------------------------------------

# The distribution tables report how a policy changes taxes. This reports the level
# of effective tax rates instead, for the baseline and the policy side by side, so
# the question it answers is how progressive the system is rather than how the
# policy changes it.
#
# Rates are reported under four definitions of income, including one that counts
# capital gains as they accrue rather than when they are realized and a variant of
# it that leaves out the accrued gain on a primary residence, and under three
# conventions for who bears the corporate tax.
#
# It follows the same path as the delta tables: the same per-record microdata, the
# same loop over years, the same percentile groupings, and the same estate
# allocation. What differs is that the numerators are levels rather than changes,
# that there are three income denominators with a corporate tax gross-up, and that
# the corporate baseline level enters as well as the reform delta.
#
# The output is long, one row per year, income definition, ranking, set of taxes
# included, corporate convention, and group. Each row carries the number of tax
# units, the income cutoff, and the income, tax and rate for both legs, plus the
# rate broken down by type of tax.
#----------------------------------------------------------------------------


ETR_INCOME_DEFS = c('agi', 'expanded', 'hs', 'hs_ex_home')

# income-definition core column suffixes in the group-sum frame
ETR_CORE_SUFFIX = c(agi = 'agi', expanded = 'exp', hs = 'hs',
                    hs_ex_home = 'hsx')
ETR_CUTOFF_COL  = c(agi = 'cutoff_agi', expanded = 'cutoff_exp', hs = 'cutoff_hs',
                    hs_ex_home = 'cutoff_hsx')
ETR_SELF_RANK_PREFIX = c(agi = 'self_agi_', expanded = 'self_expanded_',
                         hs = 'self_hs_', hs_ex_home = 'self_hsx_')
ETR_SELF_CUTOFF_COL = c(agi = 'cutoff_agi', expanded = 'cutoff_exp_rank',
                        hs = 'cutoff_hs_rank', hs_ex_home = 'cutoff_hsx_rank')

ETR_CONVENTIONS = c('equity_supernormal', 'capital_income', 'uniform_networth')
ETR_CONV_SUFFIX = c(equity_supernormal = 'es', capital_income = 'ci',
                    uniform_networth = 'nw')

# Which taxes are included. The first four nest, each adding a type. The last
# stands alone: state and local taxes plus federal excises, with no federal income
# tax in the numerator, so it reads as a regressive burden on its own. It appears in
# both legs, being outside federal law, unlike the VAT and the wealth tax.
ETR_TIERS = c('iit', 'iit_pr', 'death', 'wealth_cit_vat', 'other')



build_distribution_etrs = function(id) {

  #----------------------------------------------------------------------------
  # Generates the ETR-levels supplemental for a counterfactual scenario.
  #
  # Parameters:
  #   - id (str) : counterfactual scenario ID
  #
  # Returns: void (writes distribution_etrs.csv).
  #----------------------------------------------------------------------------

  baseline_id = 'baseline'

  # VAT/corporate deltas (shared reader with the delta table)
  other_taxes = get_other_taxes(id, baseline_id)

  # The baseline level of corporate receipts. Read the macro projections directly:
  # corp_read_macro() turns this into after-tax profit and does not return it.
  rev_corp = read_macro_spliced(interface_root('Macro-Projections', baseline_id)) %>%
    select(year, rev_corp_level = rev_corp)

  # Loop over years and over which pass supplies the reform numerators. The static
  # pass gives what the law asks for; the conventional pass gives what is actually
  # collected once taxpayers respond. Only the numerator changes between the two:
  # the denominators and the rankings stay on the baseline static frame in both.
  #
  # The baseline columns are the same either way, the baseline having no behavior.
  # A rung whose detail is absent is skipped: the mechanical rung's detail exists
  # only where a transmission channel is live, and the conventional rung's only
  # where the calculator ran on it.
  etr_tables = list()
  for (yr in get_scenario_info(id)$dist_years) {
    for (leg in REPORTING_LEGS) {
      if (leg != 'static' &&
          !file.exists(file.path(globals$output_root, id, leg, 'detail',
                                 paste0(yr, '.csv')))) {
        next
      }
      microdata = process_for_etrs(id, baseline_id, yr, other_taxes, rev_corp,
                                   reform_leg = leg)
      etr_tables[[paste(yr, leg)]] = aggregate_etrs(microdata) %>%
        mutate(year = yr, reform_leg = leg, .before = everything())
    }
  }

  etr_tables %>%
    bind_rows() %>%
    arrange(year, reform_leg, income_definition, ranking, taxes_included,
            corp_convention, group_dimension) %>%
    write_csv(file.path(globals$output_root, id, 'static/supplemental',
                        'distribution_etrs.csv'))
}



process_for_etrs = function(id, baseline_id, yr, other_taxes, rev_corp,
                            reform_leg = 'static') {

  #----------------------------------------------------------------------------
  # Builds the per-record data the tables are aggregated from: the shared
  # distribution microdata, the pieces of each income definition, the tax for every
  # tier and both legs, the three corporate allocations, the state and local taxes,
  # and the grouping columns.
  #
  # Parameters:
  #   - id          (str) : scenario ID
  #   - baseline_id (str) : baseline scenario ID
  #   - yr          (int) : year
  #   - other_taxes (df)  : CIT/VAT metrics (get_other_taxes())
  #   - rev_corp    (df)  : year, rev_corp_level ($B baseline corporate receipts)
  #
  # Returns: ungrouped per-record microdata with all pieces + rank groups (df).
  #----------------------------------------------------------------------------

  # The shared builder does both legs, the estate allocation, the capital stocks and
  # the income definitions. It does not rewrite the estate diagnostics, which the
  # delta tables already wrote.
  md = build_distribution_microdata(id, baseline_id, yr, other_taxes,
                                    write_supplemental = FALSE,
                                    reform_leg = reform_leg) %>%
    left_join(rev_corp, by = 'year')

  if (any(is.na(md$rev_corp_level))) {
    stop('distribution_etrs: baseline corporate receipts (rev_corp) missing ',
         'for ', yr, ' in Macro-Projections.')
  }

  # State and local taxes plus federal excises, as the imputed rate times the income
  # it was measured against. Read from the baseline Tax-Data and identical in both
  # legs, being outside federal law. Where the baseline vintage predates the
  # imputation this is absent and the tier is dropped.
  other_tax_tbl = read_other_taxes_base(baseline_id, yr)
  has_other = !is.null(other_tax_tbl)
  if (has_other) {
    md = md %>% left_join(other_tax_tbl, by = 'id')
    if (any(is.na(md$other_tax))) {
      stop('distribution_etrs: other_tax missing for ',
           sum(is.na(md$other_tax)), ' record(s) in ', yr,
           ' after the id join (id present in detail but absent from baseline ',
           'Tax-Data?).')
    }
  } else {
    md$other_tax = 0   # column must exist for etr_group_sums; tier dropped later
  }

  sigma_rate = corp_env_knobs()$sigma_n   # rate/level split; cost-recovery = 1

  # The corporate allocation under each convention: the baseline level, and the
  # reform's change. The reform total is the sum of the two, the allocation being
  # linear in the amount. The convention chooses what the burden is spread over:
  # corporate equity for the above-normal part and net capital for the normal part;
  # or capital income for both, which is how other agencies do it; or net worth for
  # both.
  cit_alloc = function(super_base, normal_base) {
    list(
      level = allocate_corp_dollars(md$rev_corp_level, sigma_rate, md$weight,
                                    md$labor, super_base, normal_base),
      delta = allocate_corp_dollars(md$other_corp_delta,       sigma_rate, md$weight,
                                    md$labor, super_base, normal_base) +
              allocate_corp_dollars(md$cost_recovery_delta,    1,          md$weight,
                                    md$labor, super_base, normal_base) +
              # the rate change scored on-model, split the same way as the
              # off-model figure it replaces
              allocate_corp_dollars(md$corp_rate_static_delta, sigma_rate, md$weight,
                                    md$labor, super_base, normal_base)
    )
  }
  a_es = cit_alloc(md$corp_equity,     md$net_capital)
  a_ci = cit_alloc(md$capital,         md$capital)
  a_nw = cit_alloc(md$net_worth_stock, md$net_worth_stock)

  md = md %>%
    mutate(

      # Tax by leg. The income tax figure excludes tax on gains deemed realized at
      # death, which re-enters attributed to heirs in the death tier.
      iit_base    = liab_iit_pr - liab_pr,
      iit_reform  = liab_iit_pr_reform - liab_pr_reform,
      pr_base     = liab_pr,
      pr_reform   = liab_pr_reform,
      estate_base = liab_estate,
      estate_reform = liab_estate_reform,
      dh_base     = liab_deemed_heir,
      dh_reform   = liab_deemed_heir_reform,
      wealth_reform = liab_wealth_reform,          # baseline wealth tax is 0

      # The VAT burden is the loss of real income, including inheritance in the tiers
      # that count it. One figure per record, used with every income definition. The
      # baseline has no VAT.
      vat = (income + inheritance) - (income_reform + inheritance_reform),

      # What records are ranked on. The fixed ranking uses expanded income plus
      # inheritance, so heirs are grouped by their income after receiving it. Ranking
      # each definition on itself does the same, except that AGI stays statutory.
      inc_exp_rank = inc_exp_core + inheritance,
      inc_hs_rank  = inc_hs_core  + inheritance,
      inc_hsx_rank = inc_hsx_core + inheritance,

      # --- Corporate allocations (baseline level + reform delta), per convention ---
      citbase_es  = a_es$level, citdelta_es = a_es$delta,
      citbase_ci  = a_ci$level, citdelta_ci = a_ci$delta,
      citbase_nw  = a_nw$level, citdelta_nw = a_nw$delta
    )

  # The grouping columns. Ranks are taken over the whole population, so call this
  # before grouping. Either every definition is ranked on expanded income including
  # inheritance, or each is ranked on its own income.
  out = md %>%
    add_rank_groups('inc_exp_rank', 'fx_pctile',            'fx_')            %>%
    add_rank_groups('inc_agi_core', 'self_agi_pctile',      'self_agi_')      %>%
    add_rank_groups('inc_exp_rank', 'self_expanded_pctile', 'self_expanded_') %>%
    add_rank_groups('inc_hs_rank',  'self_hs_pctile',       'self_hs_')       %>%
    add_rank_groups('inc_hsx_rank', 'self_hsx_pctile',      'self_hsx_')      %>%
    add_rank_groups('net_worth',    'nw_pctile',            'nw_')            %>%
    mutate(nw_billionaire = if_else(net_worth >= 1e9, 'Billionaires', NA_character_)) %>%
    ungroup()
  attr(out, 'has_other') = has_other   # gates the `other` tier in aggregate_etrs
  out
}



etr_group_sums = function(md, group_col) {

  #----------------------------------------------------------------------------
  # Sums every income and tax piece within each group. Done once per grouping, after
  # which the combinations of income definition, tier and convention are assembled
  # from those sums. The full set of about 72 combinations is never built per record.
  #
  # Parameters:
  #   - md        (df)   : per-record ETR microdata (process_for_etrs())
  #   - group_col (vec)  : per-record group labels, or NULL for the whole pop
  #
  # Returns: one row per group with n_tax_units, income cutoffs, and S_* sums.
  #----------------------------------------------------------------------------

  g = if (is.null(group_col)) rep('Overall', nrow(md)) else group_col

  md %>%
    group_by(.grp = g) %>%
    summarise(
      n_tax_units = sum(weight),

      # The income definitions, and the inheritance and gross-up added to them
      S_agi_core = sum(inc_agi_core * weight),
      S_exp_core = sum(inc_exp_core * weight),
      S_hs_core  = sum(inc_hs_core  * weight),
      S_hsx_core = sum(inc_hsx_core * weight),
      S_inh      = sum(inheritance  * weight),

      # Tax numerators (per leg)
      S_iit_base    = sum(iit_base    * weight), S_iit_reform    = sum(iit_reform    * weight),
      S_pr_base     = sum(pr_base     * weight), S_pr_reform     = sum(pr_reform     * weight),
      S_estate_base = sum(estate_base * weight), S_estate_reform = sum(estate_reform * weight),
      S_dh_base     = sum(dh_base     * weight), S_dh_reform     = sum(dh_reform     * weight),
      S_wealth_reform = sum(wealth_reform * weight),
      S_vat           = sum(vat           * weight),
      S_other         = sum(other_tax     * weight),   # state+local+excise

      # The corporate allocations, which enter the numerator as a tax and the
      # denominator as a gross-up
      S_citbase_es = sum(citbase_es * weight), S_citdelta_es = sum(citdelta_es * weight),
      S_citbase_ci = sum(citbase_ci * weight), S_citdelta_ci = sum(citdelta_ci * weight),
      S_citbase_nw = sum(citbase_nw * weight), S_citdelta_nw = sum(citdelta_nw * weight),

      # The bottom of each group, which means something for the percentile cuts
      cutoff_agi = round(min(inc_agi_core) / 5) * 5,
      cutoff_exp = round(min(inc_exp_core) / 5) * 5,
      cutoff_hs  = round(min(inc_hs_core)  / 5) * 5,
      cutoff_hsx = round(min(inc_hsx_core) / 5) * 5,
      cutoff_exp_rank = round(min(inc_exp_rank) / 5) * 5,
      cutoff_hs_rank  = round(min(inc_hs_rank)  / 5) * 5,
      cutoff_hsx_rank = round(min(inc_hsx_rank) / 5) * 5,
      cutoff_nw = round(min(net_worth)),

      .groups = 'drop'
    ) %>%
    rename(group = .grp)
}



compose_etr_rows = function(sums, def, ranking, group_dimension, cutoff_col,
                            include_other = TRUE) {

  #----------------------------------------------------------------------------
  # Assembles the rows for one income definition, across the tax tiers and, in the
  # tier that includes the corporate tax, across the three conventions.
  #
  # On the denominators: AGI is statutory, so it takes neither inheritance nor the
  # corporate gross-up in any tier. The other two definitions add inheritance in the
  # tiers that count death, and add the corporate allocation in the tier that
  # includes it.
  #
  # The numerators follow the nesting of the tiers. The VAT and the wealth tax appear
  # in the reform leg only. The corporate amount in the numerator matches the
  # gross-up in the denominator, so the two are consistent.
  #
  # Returns: long tibble of ETR rows (df).
  #----------------------------------------------------------------------------

  core = sums[[paste0('S_', ETR_CORE_SUFFIX[[def]], '_core')]]

  cit = function(which, conv) sums[[paste0('S_cit', which, '_', ETR_CONV_SUFFIX[[conv]])]]

  rows = list()
  tiers = if (include_other) ETR_TIERS else setdiff(ETR_TIERS, 'other')
  for (tier in tiers) {

    convs = if (tier == 'wealth_cit_vat') ETR_CONVENTIONS else 'n/a'

    for (conv in convs) {

      add_inh   = if (def != 'agi' && tier %in% c('death', 'wealth_cit_vat')) sums$S_inh else 0
      is_cit    = tier == 'wealth_cit_vat'
      is_other  = tier == 'other'         # standalone state+local+excise tier
      cb = if (is_cit) cit('base', conv)  else 0
      cd = if (is_cit) cit('delta', conv) else 0

      # Denominators (AGI takes no add-ons)
      if (def == 'agi') {
        inc_b = core
        inc_r = core
      } else {
        inc_b = core + add_inh + (if (is_cit) cb else 0)
        inc_r = core + add_inh + (if (is_cit) cb + cd else 0)
      }

      # Numerators (tier nesting), kept as separate pieces so each row can emit
      # a same-denominator ETR decomposition. Components not included in a tier
      # are zero; baseline wealth and VAT are zero by construction.
      has_pr    = tier %in% c('iit_pr', 'death', 'wealth_cit_vat')
      has_death = tier %in% c('death', 'wealth_cit_vat')

      # iit is always-on EXCEPT in the standalone `other` tier, which carries no
      # federal tax in the numerator.
      comp_iit_b    = if (is_other) 0 else sums$S_iit_base
      comp_iit_r    = if (is_other) 0 else sums$S_iit_reform
      comp_pr_b     = if (has_pr) sums$S_pr_base else 0
      comp_pr_r     = if (has_pr) sums$S_pr_reform else 0
      comp_estate_b = if (has_death) sums$S_estate_base else 0
      comp_estate_r = if (has_death) sums$S_estate_reform else 0
      comp_deemed_b = if (has_death) sums$S_dh_base else 0
      comp_deemed_r = if (has_death) sums$S_dh_reform else 0
      comp_wealth_b = 0
      comp_wealth_r = if (is_cit) sums$S_wealth_reform else 0
      comp_corp_b   = if (is_cit) cb else 0
      comp_corp_r   = if (is_cit) cb + cd else 0
      comp_vat_b    = 0
      comp_vat_r    = if (is_cit) sums$S_vat else 0
      # Other taxes are exogenous -> present in BOTH legs (not reform-only)
      comp_other_b  = if (is_other) sums$S_other else 0
      comp_other_r  = if (is_other) sums$S_other else 0

      tax_b = comp_iit_b + comp_pr_b + comp_estate_b + comp_deemed_b +
              comp_wealth_b + comp_corp_b + comp_vat_b + comp_other_b
      tax_r = comp_iit_r + comp_pr_r + comp_estate_r + comp_deemed_r +
              comp_wealth_r + comp_corp_r + comp_vat_r + comp_other_r

      rows[[length(rows) + 1]] = tibble(
        income_definition = def,
        ranking           = ranking,
        taxes_included    = tier,
        corp_convention   = conv,
        group_dimension   = group_dimension,
        group             = sums$group,
        n_tax_units       = sums$n_tax_units,
        income_cutoff     = if (is.null(cutoff_col)) NA_real_ else sums[[cutoff_col]],
        income_baseline   = inc_b / 1e9,
        income_reform     = inc_r / 1e9,
        tax_baseline      = tax_b / 1e9,
        tax_reform        = tax_r / 1e9,
        etr_baseline      = tax_b / inc_b,
        etr_reform        = tax_r / inc_r,
        etr_income_tax_baseline = comp_iit_b / inc_b,
        etr_income_tax_reform   = comp_iit_r / inc_r,
        etr_payroll_baseline    = comp_pr_b / inc_b,
        etr_payroll_reform      = comp_pr_r / inc_r,
        etr_estate_baseline     = comp_estate_b / inc_b,
        etr_estate_reform       = comp_estate_r / inc_r,
        etr_deemed_baseline     = comp_deemed_b / inc_b,
        etr_deemed_reform       = comp_deemed_r / inc_r,
        etr_wealth_baseline     = comp_wealth_b / inc_b,
        etr_wealth_reform       = comp_wealth_r / inc_r,
        etr_corp_baseline       = comp_corp_b / inc_b,
        etr_corp_reform         = comp_corp_r / inc_r,
        etr_vat_baseline        = comp_vat_b / inc_b,
        etr_vat_reform          = comp_vat_r / inc_r,
        etr_other_baseline      = comp_other_b / inc_b,
        etr_other_reform        = comp_other_r / inc_r
      )
    }
  }
  bind_rows(rows)
}



aggregate_etrs = function(md) {

  #----------------------------------------------------------------------------
  # Aggregates the per-record ETR microdata into the long output cube: the
  # non-ranked dimensions (Overall / Age / Parent status, ranking = 'n/a') and
  # the income-percentile family under both fixed and self ranking.
  #
  # Parameters:
  #   - md (df) : per-record ETR microdata (process_for_etrs())
  #
  # Returns: long ETR table for the year (df).
  #----------------------------------------------------------------------------

  results = list()
  push = function(x) results[[length(results) + 1]] <<- x

  # Include the standalone `other` tier only when the baseline vintage carried the
  # imputed rate (process_for_etrs stamped this on md).
  has_other = isTRUE(attr(md, 'has_other'))
  co = function(sums, def, ranking, gd, cutoff_col)
    compose_etr_rows(sums, def, ranking, gd, cutoff_col, include_other = has_other)

  # --- Non-ranked dimensions (grouping independent of ranking/definition) ---
  nonranked = list(
    list(dim = 'Overall',       col = NULL),
    list(dim = 'Age',           col = 'age_group'),
    list(dim = 'Parent status', col = 'parent_group')
  )
  for (nr in nonranked) {
    sums = etr_group_sums(md, if (is.null(nr$col)) NULL else md[[nr$col]])
    for (def in ETR_INCOME_DEFS) {
      push(co(sums, def, 'n/a', nr$dim, NULL))
    }
  }

  # --- Income-percentile family: quintile (labels the negative group) + top-X
  # (drop the non-member group) ---
  cuts = list(
    list(col = 'quintile', na = 'Negative income'),
    list(col = 'top_10',   na = 'drop'),
    list(col = 'top_5',    na = 'drop'),
    list(col = 'top_1',    na = 'drop'),
    list(col = 'top_01',   na = 'drop'),
    list(col = 'top_001',  na = 'drop')
  )

  # Fixed ranking: one grouping per cut (by inheritance-inclusive expanded
  # income), reused for all defs.
  for (cut in cuts) {
    gv = md[[paste0('fx_', cut$col)]]
    drop_na = cut$na == 'drop'
    if (!drop_na) gv = replace_na(gv, cut$na)
    sums = etr_group_sums(md, gv)
    if (drop_na) sums = filter(sums, !is.na(group))
    for (def in ETR_INCOME_DEFS) {
      push(co(sums, def, 'fixed', 'Income percentile', 'cutoff_exp_rank'))
    }
  }

  # Self ranking: grouping depends on the definition's own income
  for (def in ETR_INCOME_DEFS) {
    for (cut in cuts) {
      gv = md[[paste0(ETR_SELF_RANK_PREFIX[[def]], cut$col)]]
      drop_na = cut$na == 'drop'
      if (!drop_na) gv = replace_na(gv, cut$na)
      sums = etr_group_sums(md, gv)
      if (drop_na) sums = filter(sums, !is.na(group))
      push(co(sums, def, 'self', 'Income percentile', ETR_SELF_CUTOFF_COL[[def]]))
    }
  }

  # --- Net-worth family: wealth quintiles/top-X plus a billionaire breakout ---
  nw_cuts = list(
    list(col = 'quintile',     na = 'Negative net worth'),
    list(col = 'top_10',       na = 'drop'),
    list(col = 'top_5',        na = 'drop'),
    list(col = 'top_1',        na = 'drop'),
    list(col = 'top_01',       na = 'drop'),
    list(col = 'top_001',      na = 'drop'),
    list(col = 'billionaire',  na = 'drop')
  )

  for (cut in nw_cuts) {
    gv = md[[paste0('nw_', cut$col)]]
    drop_na = cut$na == 'drop'
    if (!drop_na) gv = replace_na(gv, cut$na)
    sums = etr_group_sums(md, gv)
    if (drop_na) sums = filter(sums, !is.na(group))
    for (def in ETR_INCOME_DEFS) {
      push(co(sums, def, 'wealth', 'Net worth', 'cutoff_nw'))
    }
  }

  bind_rows(results)
}

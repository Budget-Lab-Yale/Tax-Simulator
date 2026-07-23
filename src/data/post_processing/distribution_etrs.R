#----------------------------------------------------------------------------
# distribution_etrs.R
#
# Builds distribution_etrs.csv: effective tax rates (ETR = tax / pre-tax income)
# reported as LEVELS, for baseline AND policy side by side, by income group,
# under multiple income definitions and multiple corporate-incidence
# conventions. Answers: how progressive is the system under this policy when
# gains are marked to accrual (Haig-Simons) instead of realization, and under
# different corporate-incidence assumptions?
#
# Mirrors build_distribution_tables: reads static detail via the shared
# build_distribution_microdata(), loops dist_years, reuses add_rank_groups()
# for the percentile families, and the estate allocator path for the death
# tier. It differs in the numerators (ETR levels for BOTH legs, not deltas),
# the income denominators (three definitions, with a corporate-tax gross-up),
# and the corporate allocation (three stock-based conventions from corp_alloc.R,
# plus the corporate BASELINE LEVEL).
#
# Output is long, keyed by:
#   year x income_definition{agi,expanded,hs} x ranking{fixed,self,n/a}
#     x taxes_included{iit, iit_pr, death, wealth_cit_vat, other}
#     x corp_convention{equity_supernormal, capital_income, uniform_networth, n/a}
#     x group_dimension{Overall, Age, Parent status, Income percentile,
#       Net worth} x group
# with, per cell: n_tax_units, income_cutoff, income_/tax_/etr_ for {baseline,
# reform}, and component ETR columns that decompose etr_ into tax types.
# corp_convention varies ONLY the CIT-inclusive tier; ranking is 'n/a' for the
# non-percentile dimensions.
#----------------------------------------------------------------------------


ETR_INCOME_DEFS = c('agi', 'expanded', 'hs')

# income-definition core column suffixes in the group-sum frame
ETR_CORE_SUFFIX = c(agi = 'agi', expanded = 'exp', hs = 'hs')
ETR_CUTOFF_COL  = c(agi = 'cutoff_agi', expanded = 'cutoff_exp', hs = 'cutoff_hs')
ETR_SELF_RANK_PREFIX = c(agi = 'self_agi_', expanded = 'self_expanded_',
                         hs = 'self_hs_')
ETR_SELF_CUTOFF_COL = c(agi = 'cutoff_agi', expanded = 'cutoff_exp_rank',
                        hs = 'cutoff_hs_rank')

ETR_CONVENTIONS = c('equity_supernormal', 'capital_income', 'uniform_networth')
ETR_CONV_SUFFIX = c(equity_supernormal = 'es', capital_income = 'ci',
                    uniform_networth = 'nw')

# Tax-inclusion tiers. The first four are nested (each adds a tax type). `other`
# is a STANDALONE tier -- combined state+local+federal-excise "other taxes"
# (dina_other_taxes_rate) ONLY, no federal tax in the numerator -- so it reads
# as an isolated regressive-burden ETR. Present in BOTH legs (exogenous to
# federal law), unlike VAT/wealth which are reform-only.
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

  # Corporate BASELINE LEVEL (rev_corp, $B, CBO corporate receipts): read the
  # baseline Macro-Projections directly -- do NOT reuse corp_read_macro(), which
  # transmutes rev_corp into pi_at and does not return it.
  rev_corp = read_macro_spliced(interface_root('Macro-Projections', baseline_id)) %>%
    select(year, rev_corp_level = rev_corp)

  # Loop over years x reform leg, build per-record microdata, aggregate the
  # cube. reform_leg keys WHICH leg supplies the reform tax numerators:
  #   static       — the law-only ask (welfare ETR, envelope theorem)
  #   conventional — realized collections with behavior (numerator-only swap;
  #                  denominators/rankings stay baseline-static in both)
  # Baseline columns are identical across the two legs (baseline has no
  # behavior). The conventional rows are skipped when the leg's detail is
  # absent (static-only runs, purged vintages).
  etr_tables = list()
  for (yr in get_scenario_info(id)$dist_years) {
    for (leg in c('static', 'conventional')) {
      if (leg == 'conventional' &&
          !file.exists(file.path(globals$output_root, id, 'conventional/detail',
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
  # Builds the per-record ETR microdata: the shared distribution microdata plus
  # the per-record income-definition pieces, the tax numerators for every tier
  # (both legs), the three corporate-incidence allocations (baseline level +
  # reform delta), the state+local+excise other-tax dollars, and the fixed/self
  # rank-group columns.
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

  # Shared builder: both legs, universe check, estate heir allocation + copy
  # split, stock bases, income-definition cores. Do NOT re-emit the estate
  # supplemental files (the delta table already wrote them).
  md = build_distribution_microdata(id, baseline_id, yr, other_taxes,
                                    write_supplemental = FALSE,
                                    reform_leg = reform_leg) %>%
    left_join(rev_corp, by = 'year')

  if (any(is.na(md$rev_corp_level))) {
    stop('distribution_etrs: baseline corporate receipts (rev_corp) missing ',
         'for ', yr, ' in Macro-Projections.')
  }

  # Combined state+local+federal-excise "other taxes" per record: imputed rate x
  # reconstructed broad income (the base the rate was divided by). Keyed by id
  # from the BASELINE Tax-Data; exogenous to federal law, so it is identical in
  # the baseline and reform legs (the heir copy-split just re-weights the same
  # value). One row per id on the right -> the split rows join cleanly. If the
  # baseline vintage predates the imputation, the reader returns NULL and the
  # `other` tier is dropped downstream (has_other = FALSE).
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

  # Per-convention corporate allocation: the baseline LEVEL (rev_corp, sigma_n)
  # and the reform DELTA (off-model rate delta at sigma_n + cost-recovery delta
  # at sigma_n = 1). Reform total = level + delta (allocation is linear in the
  # amount at fixed sigma_n). The convention picks the capital bases:
  #   equity_supernormal : supernormal = corp-equity stock ; normal = net-capital
  #   capital_income     : both legs = capital INCOME (agency reconciliation)
  #   uniform_networth   : both legs = baseline net worth (floored)
  cit_alloc = function(super_base, normal_base) {
    list(
      level = allocate_corp_dollars(md$rev_corp_level, sigma_rate, md$weight,
                                    md$labor, super_base, normal_base),
      delta = allocate_corp_dollars(md$other_corp_delta,       sigma_rate, md$weight,
                                    md$labor, super_base, normal_base) +
              allocate_corp_dollars(md$cost_recovery_delta,    1,          md$weight,
                                    md$labor, super_base, normal_base) +
              # on-model statutory-rate delta (static), sigma_n like the off-model
              # rate delta it replaces
              allocate_corp_dollars(md$corp_rate_static_delta, sigma_rate, md$weight,
                                    md$labor, super_base, normal_base)
    )
  }
  a_es = cit_alloc(md$corp_equity,     md$net_capital)
  a_ci = cit_alloc(md$capital,         md$capital)
  a_nw = cit_alloc(md$net_worth_stock, md$net_worth_stock)

  md = md %>%
    mutate(

      # --- Tax numerators, per leg (iit strips the deemed-realization tax; it
      # re-enters as the heir-reattributed liab_deemed_heir in the death tier) ---
      iit_base    = liab_iit_pr - liab_pr,
      iit_reform  = liab_iit_pr_reform - liab_pr_reform,
      pr_base     = liab_pr,
      pr_reform   = liab_pr_reform,
      estate_base = liab_estate,
      estate_reform = liab_estate_reform,
      dh_base     = liab_deemed_heir,
      dh_reform   = liab_deemed_heir_reform,
      wealth_reform = liab_wealth_reform,          # baseline wealth tax is 0

      # VAT burden = real income loss on expanded income (incl. inheritance in
      # the death-inclusive top tier); a fixed per-record tax used as a numerator
      # across all income definitions (baseline VAT is 0)
      vat = (income + inheritance) - (income_reform + inheritance_reform),

      # Ranking bases. Fixed income ranking is expanded income plus inheritance,
      # so heirs are grouped by income after inheritance receipt. Self rankings
      # use the same convention for expanded/HS; AGI remains statutory.
      inc_exp_rank = inc_exp_core + inheritance,
      inc_hs_rank  = inc_hs_core  + inheritance,

      # --- Corporate allocations (baseline level + reform delta), per convention ---
      citbase_es  = a_es$level, citdelta_es = a_es$delta,
      citbase_ci  = a_ci$level, citdelta_ci = a_ci$delta,
      citbase_nw  = a_nw$level, citdelta_nw = a_nw$delta
    )

  # Rank-group columns. Ranks are over the whole (ungrouped) population; call on
  # the ungrouped frame. Fixed ranking: all definitions ranked by inheritance-
  # inclusive expanded income. Self ranking: each definition ranked by its own
  # income, with inheritance included for expanded and HS.
  out = md %>%
    add_rank_groups('inc_exp_rank', 'fx_pctile',            'fx_')            %>%
    add_rank_groups('inc_agi_core', 'self_agi_pctile',      'self_agi_')      %>%
    add_rank_groups('inc_exp_rank', 'self_expanded_pctile', 'self_expanded_') %>%
    add_rank_groups('inc_hs_rank',  'self_hs_pctile',       'self_hs_')       %>%
    add_rank_groups('net_worth',    'nw_pctile',            'nw_')            %>%
    mutate(nw_billionaire = if_else(net_worth >= 1e9, 'Billionaires', NA_character_)) %>%
    ungroup()
  attr(out, 'has_other') = has_other   # gates the `other` tier in aggregate_etrs
  out
}



etr_group_sums = function(md, group_col) {

  #----------------------------------------------------------------------------
  # Weighted group sums of every income/tax piece needed to compose the ETR
  # cube. Computed ONCE per grouping; the definition x tier x convention combos
  # are then composed arithmetically (compose_etr_rows), so the ~72-way cube is
  # never materialized at the record level.
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

      # Income-definition cores and the inheritance / gross-up add-ons
      S_agi_core = sum(inc_agi_core * weight),
      S_exp_core = sum(inc_exp_core * weight),
      S_hs_core  = sum(inc_hs_core  * weight),
      S_inh      = sum(inheritance  * weight),

      # Tax numerators (per leg)
      S_iit_base    = sum(iit_base    * weight), S_iit_reform    = sum(iit_reform    * weight),
      S_pr_base     = sum(pr_base     * weight), S_pr_reform     = sum(pr_reform     * weight),
      S_estate_base = sum(estate_base * weight), S_estate_reform = sum(estate_reform * weight),
      S_dh_base     = sum(dh_base     * weight), S_dh_reform     = sum(dh_reform     * weight),
      S_wealth_reform = sum(wealth_reform * weight),
      S_vat           = sum(vat           * weight),
      S_other         = sum(other_tax     * weight),   # state+local+excise

      # Corporate allocations (level + delta), per convention -- these enter both
      # the numerator (as a tax) and the denominator (as a gross-up)
      S_citbase_es = sum(citbase_es * weight), S_citdelta_es = sum(citdelta_es * weight),
      S_citbase_ci = sum(citbase_ci * weight), S_citdelta_ci = sum(citdelta_ci * weight),
      S_citbase_nw = sum(citbase_nw * weight), S_citdelta_nw = sum(citdelta_nw * weight),

      # Lower edge of the group on each income definition (meaningful for the
      # income-percentile cuts; matches distribution.csv's rounding)
      cutoff_agi = round(min(inc_agi_core) / 5) * 5,
      cutoff_exp = round(min(inc_exp_core) / 5) * 5,
      cutoff_hs  = round(min(inc_hs_core)  / 5) * 5,
      cutoff_exp_rank = round(min(inc_exp_rank) / 5) * 5,
      cutoff_hs_rank  = round(min(inc_hs_rank)  / 5) * 5,
      cutoff_nw = round(min(net_worth)),

      .groups = 'drop'
    ) %>%
    rename(group = .grp)
}



compose_etr_rows = function(sums, def, ranking, group_dimension, cutoff_col,
                            include_other = TRUE) {

  #----------------------------------------------------------------------------
  # Composes long ETR rows for one income definition from a group-sum frame,
  # across the tax tiers (four nested federal tiers + the standalone `other`
  # state+local+excise tier) and, for the CIT tier only, the three corporate
  # conventions. All arithmetic is vectorized across the sum frame's groups.
  #
  # Denominator rules:
  #   - AGI is statutory: no inheritance, no CIT gross-up, in every tier.
  #   - expanded / HS: + inheritance in the death-inclusive tiers; + the
  #     convention's CIT allocation (baseline level for the baseline leg; level
  #     + delta for the reform leg) in the CIT tier.
  # Numerator rules mirror the tier nesting; VAT and the reform wealth tax enter
  # the reform leg only; the CIT amount matches the gross-up so the assignment is
  # internally consistent.
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

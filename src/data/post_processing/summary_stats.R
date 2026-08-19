#------------------------------------------------------------------
# summary_stats.R
# 
# Contains functions to calculate various aggregates from microdata
#-------------------------------------------------------------------


get_1040_totals = function(tax_units, yr, by_agi = F) {
  
  #----------------------------------------------------------------------------
  # Aggregates individual income tax microdata. Reports both counts (number of 
  # returns reporting nonzero amounts, in millions) and amounts (in billions).
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units including calculated variables
  #   - yr (int)       : year corresponding to tax unit data
  #   - by_agi (bool)  : whether to group output by AGI
  #
  # Returns: tibble of aggregate tax unit statistics (df).
  #----------------------------------------------------------------------------
  
  # Choose demographic variables to report
  demographic_vars = c(
    'n_tax_units',
    'n_returns',
    'n_returns_dep',  
    'n_dep', 
    'n_simple_filers'
  )
  
  # Choose tax variables to report
  tax_vars = c(
    'wages',
    'wages1',
    'wages2',
    'tips',
    'ot',
    'txbl_int',        
    'exempt_int',      
    'div_ord',         
    'div_pref',        
    'txbl_ira_dist',   
    'txbl_pens_dist',
    'txbl_kg',
    'kg_pref',
    'state_ref',       
    'alimony',    
    'sole_prop',  
    'part_scorp',
    'part_scorp_loss',
    'part_active',
    'part_passive',
    'part_active_loss',
    'part_passive_loss',
    'part_179',
    'scorp_active',
    'scorp_passive',
    'scorp_active_loss',
    'scorp_passive_loss',
    'scorp_179',
    'excess_bus_loss',
    'net_rent',
    'net_estate',
    'sch_e',           
    'farm',    
    'gross_ss',   
    'txbl_ss',
    'ui',
    'other_inc',
    'gross_inc',
    'auto_int_exp',
    'auto_int_ded',
    'sl_int_ded',
    'char_above_ded',
    'above_ded',
    'agi',
    'ded',
    'itemizing',
    'std_ded',
    'med_item_ded', 
    'salt_item_ded', 
    'mort_int_item_ded', 
    'inv_int_item_ded', 
    'int_item_ded', 
    'char_item_ded', 
    'casualty_item_ded', 
    'misc_item_ded', 
    'other_item_ded', 
    'item_ded_ex_limits', 
    'item_ded',
    'pe_ded',
    'qbi_ded',
    'tip_ded', 
    'ot_ded', 
    'senior_ded',
    'txbl_inc',
    'liab_ord', 
    'liab_pref', 
    'liab_1250', 
    'liab_collect',
    'amt_gross_inc',
    'amt_txbl_inc',
    'liab_amt',
    'excess_ptc',
    'liab_bc',
    'ftc',
    'cdctc_nonref',
    'ed_nonref',
    'savers_nonref',
    'old_cred',
    'ctc_nonref',
    'nonref',
    'ctc_ref',
    'ed_ref', 
    'net_ptc', 
    'eitc',
    'wage_subsidy1',
    'wage_subsidy2',
    'rebate',
    'cdctc_ref',
    'savers_ref',
    'ref', 
    'ref_iit', 
    'ref_other', 
    'refund', 
    'liab_niit',
    'liab_surtax',
    'liab_iit', 
    'liab_iit_net', 
    'pmt_iit_nonwithheld', 
    'pmt_iit_withheld', 
    'pmt_refund_nonwithheld', 
    'pmt_refund_withheld', 
    'corp_tax_change'
  )
  
  
  # Derive reporting variables
  tax_units %<>% 
    mutate(n_tax_units     = 1, 
           n_returns       = filer,
           n_returns_dep   = filer * dep_status,
           n_nonfilers     = !filer,
           n_adults        = filer * (1 * (filing_status == 2)),
           n_people        = filer * (n_adults + n_dep),
           n_single        = filer * (filing_status == 1),
           n_joint         = filer * (filing_status == 2),
           n_hoh           = filer * (filing_status == 4),
           n_dep           = filer * n_dep,
           n_with_dep      = (n_dep > 0), 
           n_simple_filers = simple_filer)
  
  # Group data by AGI if specified
  if (by_agi) {
    agi_groups = c('Negative AGI'           = -1e9, 
                   '$1-$10,000'             = 1, 
                   '$10,000-$20,000'        = 1e4,
                   '$20,000-$30,000'        = 2e4,
                   '$30,000-$40,000'        = 3e4,
                   '$40,000-$50,000'        = 4e4,
                   '$50,000-$75,000'        = 5e4,
                   '$75,000-$100,000'       = 7.5e4,
                   '$100,000-$200,000'      = 1e5,
                   '$200,000-$500,000'      = 2e5,
                   '$500,000-$1,000,000'    = 5e5,
                   '$1,000,000-$1,500,000'  = 1e6,
                   '$1,500,000-$2,000,000'  = 1.5e6,
                   '$2,000,000-$5,000,000'  = 2e6,
                   '$5,000,000-$10,000,000' = 5e6,
                   'Over $10,000,000'       = 1e7, 
                   'NA'                     = 1e99)
    
    tax_units %<>% 
      mutate(
        agi_group = cut(x              = agi, 
                        breaks         = agi_groups, 
                        right          = F, 
                        include.lowest = T, 
                        labels         = head(names(agi_groups), -1))
      ) %>% 
      group_by(agi_group)
  }
  
  
  tax_units %>%
    summarise(
      
      # Add up all records for demographic variables
      across(.cols = all_of(demographic_vars), 
             .fns  = ~ sum(. * weight) / 1e6), 
      
      # Get tax variable totals, restricted to filers only
      across(.cols  = all_of(tax_vars), 
             .fns   = list(n      = ~ sum((. != 0) * weight * filer) / 1e6,
                           amount = ~ sum(.        * weight * filer) / 1e9),
             .names = '{fn}_{col}'), 
      
      # MTR vars
      across(.cols  = starts_with('mtr_'), 
             .fns   = ~ weighted.mean(., weight, na.rm = T))
    ) %>%
    
    # Clean up names and return
    rename_with(.cols = starts_with('amount_'), 
                .fn   = ~ str_replace(., 'amount_', '')) %>% 
    mutate(year = yr) %>% 
    select(year, everything()) %>% 
    return()
} 




get_pr_totals = function(tax_units, yr) {
  
  #----------------------------------------------------------------------------
  # Aggregates payroll tax microdata. Reports both counts (number of returns 
  # reporting nonzero amounts, in millions) and amounts (in billions).
  # 
  # Parameters:
  #   - tax_units (df) : tibble of tax units including calculated variables
  #   - yr (int)       : year corresponding to tax unit data
  #
  # Returns: tibble of aggregate tax unit statistics (df).
  #----------------------------------------------------------------------------
  
  
  # Choose demographic variables to report
  demographic_vars = c(
    'n_tax_units'
  )
  
  # Choose tax variables to report
  tax_vars = c(
    'gross_wages1', 
    'gross_wages2', 
    'gross_wages', 
    'se1', 
    'se2', 
    'se', 
    'liab_fica', 
    'liab_seca', 
    'liab_seca_er', 
    'liab_oasdi', 
    'liab_hi', 
    'liab_add_med', 
    'liab_pr_ee', 
    'liab_pr_er', 
    'liab_pr', 
    'pmt_pr_nonwithheld',
    'pmt_pr_withheld'
  )
  
  
  tax_units %>% 
    
    # Derive reporting variables
    mutate(n_tax_units = 1) %>% 
    
    summarise(
      
      # Add up all records for demographic variables
      across(.cols = all_of(demographic_vars), 
             .fns  = ~ sum(. * weight) / 1e6), 
      
      # Get totals for tax variables
      across(.cols  = all_of(tax_vars), 
             .fns   = list(n      = ~ sum((. != 0) * weight) / 1e6,
                           amount = ~ sum(.        * weight) / 1e9), 
             .names = '{fn}_{col}')
    ) %>% 
    
    # Clean up names and return
    rename_with(.cols = starts_with('amount_'), 
                .fn   = ~ str_replace(., 'amount_', '')) %>% 
    mutate(year = yr) %>% 
    relocate(year) %>% 
    return()
} 


get_state_totals = function(tax_units_calc, state_tax_law, state_weights, yr,
                            detail_path = NULL, state_tax_contexts = list(),
                            conformity_groups = load_state_conformity_groups()) {

  #----------------------------------------------------------------------------
  # Calculates state-level aggregates: runs the state calculator per
  # jurisdiction on federally-calculated tax units and aggregates with the
  # split state weights (plan §2.4). Optionally writes the compact per-year
  # state detail matrix -- id plus one liability column per state (plan §5.3)
  # -- accumulated in the same pass, no recomputation.
  #
  # Parameters:
  #   - tax_units_calc (df) : tax units post-federal calculation
  #   - state_tax_law (df)  : state tax law tibble; see build_state_tax_law()
  #   - state_weights (df)  : long (id, state, weight) split weights
  #   - yr (int)            : year
  #   - detail_path (str)   : when non-NULL, path for the per-year state
  #                           detail matrix CSV
  #
  # Returns: tibble long in (year, state, variable) with weighted totals, or
  #          NULL when no state law exists for the year (df | NULL).
  #----------------------------------------------------------------------------

  if (is.null(state_tax_law) || nrow(state_tax_law) == 0) {
    return(NULL)
  }
  credit_tables = attr(state_tax_law, 'credit_tables')
  law_yr = state_tax_law %>%
    filter(year == yr)
  if (nrow(law_yr) == 0) {
    return(NULL)
  }
  state_groups = state_conformity_groups_for_law(law_yr, conformity_groups)

  detail = list()

  totals = unique(law_yr$state) %>%
    map(.f = function(st) {

      group = state_groups %>% filter(state == st)
      state_tax_context = state_tax_context_for_group(
        tax_units_calc     = tax_units_calc,
        conformity_group   = group$conformity_group,
        group_ready        = group$ready,
        state_tax_contexts = state_tax_contexts
      )

      # Join this state's law to its rolling or reference federal context,
      # calculate, and reattach ids and weights.
      st_results = state_tax_context %>%
        left_join(law_yr %>%
                    filter(state == st) %>%
                    select(-state),
                  by = c('year', 'filing_status')) %>%
        do_state_taxes(
          credit_tables = state_credit_tables_for_year(credit_tables, st, yr),
          # Married-separate law row, for states offering the split election
          law_mfs = law_yr %>%
                      filter(state == st, filing_status == 3) %>%
                      select(-state, -year, -filing_status)
        ) %>%
        mutate(id = state_tax_context$id) %>%
        left_join(state_weights %>%
                    filter(state == st) %>%
                    select(id, st_weight = weight),
                  by = 'id') %>%
        mutate(st_weight = replace_na(st_weight, 0))

        # Accumulate the per-record net individual fiscal amount for detail
      if (!is.null(detail_path)) {
          detail[[st]] <<- st_results %>%
            select(id, !!st := liab_st_individual_net)
      }

      st_results %>%

        # Weighted aggregates
        summarise(
            returns           = sum(st_weight * st_tax_filer),
            liab_st_iit       = sum(st_weight * liab_st_iit),
            liab_st_narrow_iit = sum(st_weight * liab_st_narrow_iit),
            liab_st_ltcg_excise = sum(st_weight * liab_st_ltcg_excise),
            st_refund_wftc    = sum(st_weight * st_refund_wftc),
            liab_st_individual_net = sum(st_weight * liab_st_individual_net),
            st_agi            = sum(st_weight * st_agi),
          st_txbl_inc       = sum(st_weight * st_txbl_inc),
          st_tax_pre_credit = sum(st_weight * st_tax_pre_credit),
          st_eitc           = sum(st_weight * st_eitc),
          st_ctc            = sum(st_weight * st_ctc),
          st_yctc           = sum(st_weight * st_yctc),
          st_cdctc          = sum(st_weight * st_cdctc),
          st_credits_nonref = sum(st_weight * st_credits_nonref),
          st_credits_ref    = sum(st_weight * st_credits_ref)
        ) %>%
        mutate(year = yr, state = st) %>%
        pivot_longer(cols      = -c(year, state),
                     names_to  = 'variable',
                     values_to = 'value')
    }) %>%
    bind_rows()

  # Write the compact detail matrix: id + one liability column per state
  if (!is.null(detail_path) && length(detail) > 0) {
    dir.create(dirname(detail_path), recursive = T, showWarnings = F)
    detail %>%
      reduce(.f = ~ left_join(.x, .y, by = 'id')) %>%
      write_csv(detail_path)
  }

  return(totals)
}

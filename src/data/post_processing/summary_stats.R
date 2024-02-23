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
    'txbl_inc',
    'liab_ord', 
    'liab_pref', 
    'liab_1250', 
    'liab_collect',
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
    'rebate',
    'cdctc_ref',
    'savers_ref',
    'ref', 
    'ref_iit', 
    'ref_other', 
    'refund', 
    'liab_niit',
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
      
      # Restrict tax variables to 1040 filers
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
      
      # Restrict tax variables to 1040 filers
      across(.cols  = all_of(tax_vars), 
             .fns   = list(n     = ~ sum((. != 0) * weight * filer) / 1e6,
                           amount = ~ sum(.        * weight * filer) / 1e9), 
             .names = '{fn}_{col}')
    ) %>% 
    
    # Clean up names and return
    rename_with(.cols = starts_with('amount_'), 
                .fn   = ~ str_replace(., 'amount_', '')) %>% 
    mutate(year = yr) %>% 
    relocate(year) %>% 
    return()
} 
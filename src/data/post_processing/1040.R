#---------------------------------------------------
# 1040.R
# 
# Post-processing functions to generate 1040 report 
#---------------------------------------------------


create_1040_reports = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Builds human-readable 1040 output Excel files for any counterfactual 
  # policies specified. 
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : scenario IDs for which to generate report  
  #
  # Returns: void (writes the Excel file)
  #----------------------------------------------------------------------------
  
  if (length(counterfactual_ids) == 0) { 
    return()  
  }
  
  # Read and process baseline 
  baseline = file.path(globals$baseline_root, 
                       'baseline', 
                       'static',
                       'totals', 
                       '1040.csv') %>%
          read_csv(show_col_types = F) %>%  
    
    # Pivot long in variable 
    rename_with(.cols = starts_with('n_'), 
                .fn   = ~ str_replace(., 'n_', 'count.')) %>% 
    rename_with(.cols = -c(year, starts_with('count.')), 
                .fn   = ~ paste0('amount.', .)) %>%
    pivot_longer(cols      = -year, 
                 names_to  = c('series', 'Variable'), 
                 names_sep = '[.]',
                 values_to = 'value') %>% 
    pivot_wider(names_from  = series, 
                values_from = value) 
  
  
  for (scenario_id in counterfactual_ids) {
    
    # Read in counterfactual scenario 1040 totals 
    scenario = c('static', 'conventional') %>% 
      map(.f = ~ file.path(globals$output_root, 
                           scenario_id, 
                           .x,
                           'totals', 
                           '1040.csv') %>%
            read_csv(show_col_types = F) %>%
            mutate(run_type = .x)
      ) %>% 
      bind_rows() %>% 
      
      # Pivot long in variable 
      rename_with(.cols = starts_with('n_'), 
                  .fn   = ~ str_replace(., 'n_', 'count.')) %>% 
      rename_with(.cols = -c(year, run_type, starts_with('count.')), 
                  .fn   = ~ paste0('amount.', .)) %>%
      pivot_longer(cols      = -c(year, run_type), 
                   names_to  = c('series', 'Variable'), 
                   names_sep = '[.]',
                   values_to = 'value') %>% 
      pivot_wider(names_from  = series, 
                  values_from = value) %>% 
      
      # Join baseline numbers 
      left_join(baseline %>% 
                  rename(baseline_count  = count, 
                         baseline_amount = amount), 
                by = c('year', 'Variable')) %>% 
      relocate(starts_with('baseline'), .after = Variable) %>% 
      
      # Calculate differences from baseline
      mutate(diff_count  = count  - baseline_count, 
             diff_amount = amount - baseline_amount) %>% 
      pivot_wider(names_from  = run_type, 
                  values_from = c(count, diff_count, amount, diff_amount)) %>% 
      
      # Rename variables 
      recode_1040_vars(scenario_id) %>% 
      filter(!is.na(Variable)) %>% 
      select(year, Variable, baseline_count, baseline_amount, count_static, amount_static, 
             diff_count_static, diff_amount_static, count_conventional, amount_conventional, 
             diff_count_conventional, diff_amount_conventional)
    
    
    # Convert to year-indexed list
    scenario = unique(scenario$year) %>% 
      map(.f = ~ scenario %>% 
            filter(year == .x) %>% 
            select(-year)) %>% 
      set_names(unique(scenario$year))

    
    # Build Excel workbook
    wb = createWorkbook()
    for (i in 1:length(scenario)) {
    
      yr = names(scenario)[i]
      
      # Create worksheet
      addWorksheet(wb = wb, sheetName = as.character(yr))
      
      # Write data
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = scenario[[i]] %>% 
                             select(Variable, baseline_count, baseline_amount), 
                startRow = 3)
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = scenario[[i]] %>%
                             select(count_static, amount_static, 
                                    diff_count_static, diff_amount_static),
                startRow = 3,
                startCol = 5)
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = scenario[[i]] %>%
                             select(count_conventional, amount_conventional, 
                                    diff_count_conventional, diff_amount_conventional),
                startRow = 3,
                startCol = 10)
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Baseline', 
                xy       = c(2, 2))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Policy reform, static', 
                xy       = c(5, 2))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Policy reform, conventional', 
                xy       = c(10, 2))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Number of returns', 
                xy       = c(2, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Amount', 
                xy       = c(3, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Number of returns', 
                xy       = c(5, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Amount', 
                xy       = c(6, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Number of returns, difference from baseline', 
                xy       = c(7, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Amount, difference from baseline', 
                xy       = c(8, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Number of returns', 
                xy       = c(10, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Amount', 
                xy       = c(11, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Number of returns, difference from baseline', 
                xy       = c(12, 3))
      writeData(wb       = wb, 
                sheet    = as.character(yr), 
                x        = 'Amount, difference from baseline', 
                xy       = c(13, 3))

      # Add title
      writeData(wb = wb, sheet = as.character(yr), startRow = 1, 
                x = paste0('Form 1040 line items, ', yr))
      
      
      # Format numbers and cells 
      addStyle(wb         = wb,
               sheet      = as.character(yr),
               rows       = 4:(nrow(scenario[[i]]) + 4),
               cols       = 2:13,
               gridExpand = T,
               style      = createStyle(numFmt = 'NUMBER'),
               stack      = T)
      mergeCells(wb    = wb,
                 sheet = as.character(yr),
                 rows  = 2,
                 cols  = 2:3)
      mergeCells(wb    = wb,
                 sheet = as.character(yr),
                 rows  = 2,
                 cols  = 5:8)
      mergeCells(wb    = wb,
                 sheet = as.character(yr),
                 rows  = 2,
                 cols  = 10:13)
      addStyle(wb         = wb,
               sheet      = as.character(yr),
               rows       = 2:(nrow(scenario[[i]]) + 4),
               cols       = 2:13,
               gridExpand = T,
               style      = createStyle(halign = 'center'),
               stack      = T)
      addStyle(wb         = wb,
               sheet      = as.character(yr),
               rows       = 1:100,
               cols       = 1:13,
               gridExpand = T,
               style      = createStyle(fontSize = 10,
                                        wrapText = T), 
               stack      = T)
      addStyle(wb         = wb,
               sheet      = as.character(yr),
               rows       = 2,
               cols       = c(2:3, 5:8, 10:13),
               gridExpand = T,
               style      = createStyle(border = 'bottom'),
               stack      = T)
      setColWidths(wb     = wb,
                   sheet  = as.character(yr),
                   cols   = 1:13,
                   widths = c(47, 10, 10, 2, 10, 10, 10, 10, 2, 10, 10, 10, 10))

    }
    
    # Write workbook 
    saveWorkbook(wb   = wb, 
                 file = file.path(globals$output_root, 
                                  scenario_id,
                                  'conventional',
                                  'supplemental', 
                                  '1040.xlsx'), 
                 overwrite = T)
  }
  
  # Create baseline workbook by removing non-baseline info
  if('baseline' %in% globals$runtime_args$ID){
    for (i in 1:length(scenario)) {
      deleteData(wb         = wb, 
                 sheet      = i, 
                 rows       = 1:(nrow(scenario[[i]]) + 4),
                 cols       = 4:13, 
                 gridExpand = T)
      addStyle(wb         = wb, 
               sheet      = i, 
               rows       = 1:(nrow(scenario[[i]]) + 4),
               cols       = 4:13,
               style      = createStyle(), 
               gridExpand = T)
    }
    
    # Write baseline workbook
    saveWorkbook(wb   = wb, 
                 file = file.path(globals$output_root, 
                                  'baseline',
                                  'static',
                                  'supplemental', 
                                  '1040.xlsx'), 
                 overwrite = T)
    }
  }
  



create_stacked_1040_reports = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Builds stacked 1040 report.
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : scenario IDs for which to generate report  
  #
  # Returns: void (writes the Excel file)
  #----------------------------------------------------------------------------
  
  if (length(counterfactual_ids) == 0) {
    return()
  }
  
  # Read data
  stacked_1040_data = list()
  for (behavior in c('static', 'conventional')) { 
    stacked_1040_data[[behavior]] = c('baseline', counterfactual_ids) %>% 
      map(.f = ~ file.path(if_else(.x == 'baseline', globals$baseline_root, globals$output_root), 
                           .x, 
                           if_else(.x == 'baseline', 'static', behavior),
                           'totals', 
                           '1040.csv') %>%
            read_csv(show_col_types = F) %>%
            mutate(scenario_id = .x,
                   run_type    = behavior)
      ) %>% 
      bind_rows() 
  }  
  
  
  # Join data together and reshape 
  stacked_1040_data %<>% 
    bind_rows() %>% 
    relocate(run_type, scenario_id) %>% 
    
    # Calculate stacked deltas, leaving baseline in levels 
    arrange(run_type, year) %>% 
    group_by(run_type, year) %>% 
    mutate(across(.cols = -scenario_id, 
                  .fns  = ~ if_else(scenario_id == 'baseline', ., . - lag(.)))) %>% 
    ungroup() %>%  
  
    # Reshape wide in scenario 
    rename_with(.cols = starts_with('n_'), 
                .fn   = ~ str_replace(., 'n_', 'count.')) %>% 
    rename_with(.cols = -c(scenario_id, year, run_type, starts_with('count.')), 
                .fn   = ~ paste0('amount.', .)) %>%
    pivot_longer(cols      = -c(scenario_id, year, run_type), 
                 names_to  = c('Series', 'Variable'), 
                 names_sep = '[.]',
                 values_to = 'value') %>% 
    pivot_wider(names_from  = scenario_id, 
                values_from = value) %>%
    
    # Clean up 
    relocate(run_type, Variable, Series) %>% 
    recode_1040_vars('baseline') %>% 
    #recode_1040_vars(globals$runtime_args$ID) %>%
    filter(!is.na(Variable)) %>% 
    mutate(Series = if_else(Series == 'amount', 'Amount', 'Number of returns'))
  
  
  # Convert to year-indexed list
  stacked_1040_data = unique(stacked_1040_data$year) %>% 
    map(.f = ~ stacked_1040_data %>% 
          filter(year == .x) %>% 
          select(-year)) %>% 
    set_names(unique(stacked_1040_data$year))
  

  # Build Excel workbook
  wb = createWorkbook()
  for (i in 1:length(stacked_1040_data)) {
    
    yr = names(stacked_1040_data)[i]
    
    # Create worksheet
    addWorksheet(wb = wb, sheetName = as.character(yr))
    
    # Precalculate some key coordinates
    n_rows = stacked_1040_data[[i]] %>% 
      filter(run_type == 'static') %>%
      nrow()
    conventional_row = 5 + n_rows
    
    # Write data
    writeData(wb, i, 
              x = stacked_1040_data[[i]] %>% 
                filter(run_type == 'static') %>% 
                select(-run_type), 
              startRow = 2)
    writeData(wb, i, 
              x  = stacked_1040_data[[i]] %>% 
                filter(run_type == 'conventional') %>% 
                select(-run_type),
              startRow = conventional_row)
    writeData(wb, i, 
              x  = 'Without behavioral feedback (static)', 
              xy = c(1, 1))
    writeData(wb, i, 
              x  = 'With behavioral feedback (conventional)', 
              xy = c(1, conventional_row - 1))
    
    
    # Format numbers and cells 
    addStyle(wb, i, 
             rows       = c(2:(2 + n_rows), conventional_row + (1:n_rows)),
             cols       = 3:(3 + length(counterfactual_ids)),
             gridExpand = T,
             style      = createStyle(numFmt = 'NUMBER', 
                                      halign = 'center'),
             stack      = T)
    addStyle(wb, i, 
             rows       = 1:(2 * (n_rows + 10)),
             cols       = 1:(ncol(stacked_1040_data[[i]]) - 1),
             gridExpand = T,
             style      = createStyle(fontSize = 10), 
             stack      = T)
    addStyle(wb, i, 
             rows       = 1:(2 * (n_rows + 10)),
             cols       = 3:(ncol(stacked_1040_data[[i]]) - 1),
             gridExpand = T,
             style      = createStyle(wrapText = T), 
             stack      = T)
    addStyle(wb, i, 
             rows       = c(1:2, conventional_row - 1, conventional_row),
             cols       = 1:(ncol(stacked_1040_data[[i]]) - 1),
             gridExpand = T,
             style      = createStyle(border = 'bottom'),
             stack      = T)
    setColWidths(wb, i,
                 cols   = 1:2,
                 widths = c(45, 15))
    
  }
  
  saveWorkbook(wb   = wb, 
               file = file.path(globals$output_root, 
                                counterfactual_ids[length(counterfactual_ids)],
                                'conventional',
                                'supplemental', 
                                'stacked_1040.xlsx'), 
               overwrite = T)

}




recode_1040_vars = function(df, scenario_id) {
  
  #----------------------------------------------------------------------------
  # Converts subset of variable names to human-readable descriptions. 
  # Unspecified mappings are returned as NA.  
  # 
  # Parameters:
  #   - df (df)           : dataframe long in Variable with 1040.csv names 
  #   - scenario_id (str) : scenario ID
  #
  # Returns: dataframe with recoded Variable column (df).
  #----------------------------------------------------------------------------

  # Create labels for tax rate variables
  runtime_args = globals$runtime_args %>% 
    slice(1)
    #filter(ID == scenario_id)
  
  if (is.na(runtime_args$mtr_types)) {
    tax_rate_types = c()  
  } else {
    tax_rate_types = ifelse(str_split_1(runtime_args$mtr_types, ' ') == 'nextdollar', 
                            'Marginal', 
                            'Average') %>% 
                       set_names(str_split_1(runtime_args$mtr_vars, ' '))
  }
  
  # Rename tax rate variables
  for (tax_rate_var in names(tax_rate_types)) {
    df %<>%
      mutate(
        Variable = if_else(
          Variable == paste0('mtr_', tax_rate_var),
          paste0(tax_rate_types[tax_rate_var], ' tax rate on ', tax_rate_var),
          Variable
        )
      )    
  }

  # Other variables
  df %>% 
    mutate(Variable = case_when(
      
      str_sub(Variable, 1, 8) == 'Marginal' ~ Variable, 
      str_sub(Variable, 1, 7) == 'Average'  ~ Variable, 
      
      Variable == 'tax_units'             ~ 'Number of tax units', 
      Variable == 'returns'               ~ 'Number of returns filed', 
      Variable == 'returns_dep'           ~ 'Number of dependent returns filed', 
      Variable == 'dep'                   ~ 'Number of dependents claimed', 
      Variable == 'wages'                 ~ 'Wages and salaries', 
      Variable == 'wages1'                ~ 'Wages and salaries, primary earner', 
      Variable == 'wages2'                ~ 'Wages and salaries, secondary earner', 
      Variable == 'exempt_int'            ~ 'Exempt interest income',
      Variable == 'div_ord'               ~ 'Ordinary-rate dividends',
      Variable == 'div_pref'              ~ 'Preferred-rate dividends',
      Variable == 'txbl_ira_dist'         ~ 'Taxable IRA distributions',
      Variable == 'txbl_pens_dist'        ~ 'Taxable pension distributions',
      Variable == 'txbl_kg'               ~ 'Taxable net capital gain',
      Variable == 'kg_pref'               ~ 'Preferred capital gains',
      Variable == 'sole_prop'             ~ 'Schedule C net income',
      Variable == 'part_scorp'            ~ 'Partnership and S corporation income',
      Variable == 'net_rent'              ~ 'Net rental income',
      Variable == 'net_estate'            ~ 'Net income from estates and trusts',
      Variable == 'sch_e'                 ~ 'Schedule E net income',
      Variable == 'farm'                  ~ 'Farm income',
      Variable == 'gross_ss'              ~ 'Gross Social Security benefits',
      Variable == 'txbl_ss'               ~ 'Taxable Social Security benefits',
      Variable == 'ui'                    ~ 'Unemployment compensation',
      Variable == 'sl_int_ded'            ~ 'Student loan interest deduction',
      Variable == 'char_above_ded'        ~ 'Charitable contributions deduction, above-the-line',
      Variable == 'above_ded'             ~ 'Above-the-line deductions',
      Variable == 'agi'                   ~ 'Adjusted gross income',
      Variable == 'std_ded'               ~ 'Standard deduction',
      Variable == 'med_item_ded'          ~ 'Medical expense itemized deduction',
      Variable == 'salt_item_ded'         ~ 'State and local taxes itemized deduction',
      Variable == 'mort_int_item_ded'     ~ 'Mortgage interest itemized deduction',
      Variable == 'inv_int_item_ded'      ~ 'Investment interest itemized deduction',
      Variable == 'int_item_ded'          ~ 'Total interest itemized deduction',
      Variable == 'char_item_ded'         ~ 'Charitable contributions itemized deduction',
      Variable == 'casualty_item_ded'     ~ 'Casualty and theft losses itemized deduction',
      Variable == 'misc_item_ded'         ~ 'Miscellaneous AGI-limited itemized deductions',
      Variable == 'other_item_ded'        ~ 'Other itemized deductions',
      Variable == 'item_ded_ex_limits'    ~ 'Itemized deductions excluding limitations',
      Variable == 'item_ded'              ~ 'Total itemized deductions',
      Variable == 'pe_ded'                ~ 'Personal exemptions deduction',
      Variable == 'qbi_ded'               ~ 'Qualified Business Income deduction',
      Variable == 'txbl_inc'              ~ 'Taxable income',
      Variable == 'liab_ord'              ~ 'Ordinary-rate tax liability',
      Variable == 'liab_pref'             ~ 'Preferred-rate tax liability',
      Variable == 'liab_amt'              ~ 'Alternative Minimum Tax liability',
      Variable == 'excess_ptc'            ~ 'Excess Premium Tax Credit repayment',
      Variable == 'liab_bc'               ~ 'Tax liability before credits',
      Variable == 'ftc'                   ~ 'Foreign tax credit',
      Variable == 'cdctc_nonref'          ~ 'Non-refundable Child and Dependent Care Tax Credit',
      Variable == 'ed_nonref'             ~ 'Non-refundable education credits',
      Variable == 'savers_nonref'         ~ 'Non-refundable Saver\'s credit',
      Variable == 'old_cred'              ~ 'Elderly tax credit',
      Variable == 'ctc_nonref'            ~ 'Non-refundable Child Tax Credit',
      Variable == 'nonref'                ~ 'Total non-refundable credits',
      Variable == 'ctc_ref'               ~ 'Refundable Child Tax Credit (Additional Child Tax Credit)',
      Variable == 'ed_ref'                ~ 'Refundable education credits',
      Variable == 'net_ptc'               ~ 'Net Premium Tax Credit',
      Variable == 'eitc'                  ~ 'Earned Income Tax Credit',
      Variable == 'rebate'                ~ 'Recovery rebate ("stimulus check")',
      Variable == 'cdctc_ref'             ~ 'Refundable Child and Dependent Care Tax Credit',
      Variable == 'savers_ref'            ~ 'Refundable Saver\'s credit',
      Variable == 'ref'                   ~ 'Total refundable credits',
      Variable == 'ref_iit'               ~ 'Refundable credits used to offset income tax',
      Variable == 'ref_other'             ~ 'Refundable credits used to offset other taxes',
      Variable == 'refund'                ~ 'Refundable credits in excess of all tax liability',
      Variable == 'liab_niit'             ~ 'Net Investment Income Tax liability',
      Variable == 'liab_iit'              ~ 'Income tax liability', 
      Variable == 'corp_tax_change'       ~ 'Change in corporate tax attributable to income shifting'
    )) %>% 
    return()
}


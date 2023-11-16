#---------------------------------------------------
# 1040.R
# 
# Post-processing functions to generate 1040 report 
#---------------------------------------------------



create_1040_reports = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Builds human-readable 1040 output Excel files for baseline and 
  # any counterfactual policies specified. 
  # 
  # Parameters:
  #   - counterfactual_ids (str[]) : scenario IDs for which to generate report  
  #
  # Returns: void (writes the Excel file)
  #----------------------------------------------------------------------------
  
  # Read and process baseline 
  baseline = file.path(globals$output_root, 
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
      mutate(Variable = case_when(
        Variable == 'tax_units'             ~ 'Number of tax units', 
        Variable == 'returns'               ~ 'Number of returns filed', 
        Variable == 'returns_dep'           ~ 'Number of dependent returns filed', 
        Variable == 'wages'                 ~ 'Wages and salaries', 
        Variable == 'exempt_int'            ~ 'Exempt interest income',
        Variable == 'div_ord'               ~ 'Ordinary-rate dividends',
        Variable == 'div_pref'              ~ 'Preferred-rate dividends',
        Variable == 'txbl_ira_dist'         ~ 'Taxable IRA distributions',
        Variable == 'txbl_pens_dist'        ~ 'Taxable pension distributions',
        Variable == 'txbl_kg'               ~ 'Taxable net capital gain',
        Variable == 'kg_pref'               ~ 'Preferred capital gains',
        Variable == 'sole_prop'             ~ 'Schedule C net income',
        Variable == 'part_scorp'            ~ 'Partnership and S corporation income',
        Variable == 'part_scorp_loss'       ~ 'Partnership and S corporation loss',
        Variable == 'part_active'           ~ 'Partnership income (non-active)',
        Variable == 'part_passive'          ~ 'Partnership income (passive)',
        Variable == 'part_active_loss'      ~ 'Partnership losses (non-active)',
        Variable == 'part_passive_loss'     ~ 'Partnership losses (passive)',
        Variable == 'part_179'              ~ 'Partnership Section 179 deduction',
        Variable == 'scorp_active'          ~ 'S Corporation income (active)',
        Variable == 'scorp_passive'         ~ 'S Corporation income (passive)',
        Variable == 'scorp_active_loss'     ~ 'S Corporation losses (active)',
        Variable == 'scorp_passive_loss'    ~ 'S Corporation losses (passive)',
        Variable == 'scorp_179'             ~ 'S Corporation Section 179 deduction',
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
        Variable == 'liab_1250'             ~ 'Tax liability on Section 1250 gains',
        Variable == 'liab_collect'          ~ 'Tax liability on collectibles',
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
        Variable == 'liab_iit'              ~ 'Income tax liability'
      )) %>% 
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


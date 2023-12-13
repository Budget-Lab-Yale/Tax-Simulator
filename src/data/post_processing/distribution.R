#----------------------------------------------------------------------------
# distribution.R
# 
# Post-processing functions to generate distributional tables for a scenario
#----------------------------------------------------------------------------



calc_distribution = function(id, baseline_id, file_name = 'distribution.xlsx') {
  
  #----------------------------------------------------------------------------
  # Calculates distribution metrics relative to a specified baseline 
  # and writes the results to an Excel file.
  # 
  # Parameters:
  #   - id          (str) : scenario ID
  #   - baseline_id (str) : ID of scenario against which metrics are calculated. 
  #                         For regular tables, this is the actual baseline; 
  #                         for stacked tables, this is the precedeing scenario
  #   - file_name (str)   : output file name
  # 
  # Returns: void. Writes a dataframe for the scenario containing values, 
  #          grouped by pcts, for: average tax change, share with tax cut, 
  #          average tax cut, share with tax increase, average tax increase, 
  #          percent change in after tax income, share of total tax change.
  #----------------------------------------------------------------------------

  # Create new Excel workbook
  wb = createWorkbook()
  
  for (year in get_scenario_info(id)$years) {

    # Read microdata output
    if (baseline_id == 'baseline') {
      baseline_root = file.path(globals$baseline_root, 'baseline')
    } else {
      baseline_root = file.path(globals$output_root, baseline_id)
    }
    
    baseline = file.path(baseline_root,  
                         'static', 
                         'detail', 
                         paste0(year, '.csv')) %>% 
      fread() %>% 
      tibble()
    
    scenario = file.path(globals$output_root, 
                         id, 
                         'static', 
                         'detail', 
                         paste0(year, '.csv')) %>% 
      fread() %>% 
      tibble()
    
    # Join data together
    microdata = baseline %>% 
      
      # Remove dependent returns
      filter(dep_status == 0) %>% 
      
      # Pare down dataframe and join scenario liability 
      mutate(liab_baseline = liab_iit_net + liab_pr) %>%
      select(id, weight, filing_status, expanded_inc, liab_baseline) %>% 
      left_join(scenario %>% 
                  mutate(liab = liab_iit_net + liab_pr) %>% 
                  select(id, liab), 
                by = 'id') %>%
      
      mutate(
        
        # Round deltas to the nearest $10 increment
        delta = round(liab - liab_baseline, -1),
        
        # Binary dummies for if a tax unit received a meaningful raise or cut
        cut   = delta <= -5,
        raise = delta >= 5,
        
        # Create new person level weight for more representative income groups
        weight_person = weight * (1 + (filing_status == 2))
        
      )
      
    # Calculate income thresholds
    income_groups = wtd.quantile(
      x       = microdata %>% 
                  filter(expanded_inc >= 0) %>%
                  get_vector('expanded_inc'), 
      probs   = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999),
      weights = microdata %>% 
                  filter(expanded_inc >= 0) %>%
                  get_vector('weight_person')
    )

    
    dist_table = microdata %>% 
      
      # Assign income groups
      mutate(
        `Income group` = cut(
           x              = expanded_inc, 
           breaks         = c(-Inf, 0, income_groups, Inf), 
           labels         = c('Negative income', 'Bottom quintile', 'Second quintile', 
                              'Middle quintile', 'Fourth quintile', '80% - 90%', 
                              '90% - 99%', '99% - 99.9%', 'Top 0.1%'), 
           right          = F, 
           include.lowest = T
        )
      ) %>% 
    
      # Calculate metrics by group
      group_by(`Income group`) %>%
      summarise(
   
        `Income cutoff` = round(min(expanded_inc) / 5) * 5,
         
        group_delta = sum(delta * weight),
      
        `Average tax change`   = round(weighted.mean(delta, weight) / 5) * 5,
        `Average tax cut`      = round(weighted.mean(delta, (weight * cut)) / 5) * 5,
        `Average tax increase` = round(weighted.mean(delta, (weight * raise)) / 5) * 5,
      
        `Share with tax cut`      = sum(weight * cut) / sum(weight),
        `Share with tax increase` = sum(weight * raise) / sum(weight),
      
        `Percent change in after-tax income` = sum((expanded_inc - liab) * weight) / sum((expanded_inc - liab_baseline) * weight) - 1 
      ) %>%
      
      mutate(`Share of total tax change` = group_delta / sum(group_delta)) %>% 
       
      # Clean up
      mutate(
        `Income cutoff` = if_else(row_number() == 1, NA, `Income cutoff`), 
        `Percent change in after-tax income` = if_else(row_number() == 1, 
                                                       NA,
                                                       `Percent change in after-tax income`), 
        `Average tax cut` = if_else(is.nan(`Average tax cut`) | round(`Share with tax cut`, 4) == 0, 
                                    NA,
                                    `Average tax cut`),
        `Average tax increase` = if_else(is.nan(`Average tax increase`) | round(`Share with tax increase`, 4) == 0, 
                                         NA,
                                         `Average tax increase`),
        `Share of total tax change` = if_else(is.nan(`Share of total tax change`), 
                                         NA,
                                         `Share of total tax change`)
      ) %>% 
      select(`Income group`, `Income cutoff`, `Average tax change`, `Share with tax cut`, 
             `Average tax cut`, `Share with tax increase`, `Average tax increase`,
             `Percent change in after-tax income`, `Share of total tax change`)
  
    # Add worksheet and table to workbook
    addWorksheet(wb, year)
    writeData(wb = wb, sheet = as.character(year), x = dist_table, startRow = 2)
    
    # Add titles and notes 
    writeData(wb = wb, sheet = as.character(year), startRow = 1, 
              x = paste0('Distributional impact of policy change, ', year))
    writeData(wb = wb, sheet = as.character(year), startRow = 12, 
              x = paste0('Estimate universe is nondependent tax units, including nonfilers.', 
                         '"Income" is measured as AGI plus: above-the-line deductions, ', 
                         'nontaxable interest, nontaxable pension income (including OASI ',
                         'benefits), and employer-side payroll taxes. Income percentile ', 
                         'thresholds are calculated with respect to positive income only ', 
                         'and are adult-weighted.' 
                         )
              )
    
    # Format numbers and cells 
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = 3:11, 
             cols       = c(4, 6, 8, 9), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'PERCENTAGE'))
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = 3:11, 
             cols       = c(2, 3, 5, 7), 
             gridExpand = T, 
             style      = createStyle(numFmt = 'COMMA'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = c(1, 2, 11), 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(border = 'bottom'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = 2, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(textDecoration = 'bold', 
                                      wrapText       = T), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = 2:11, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(halign = 'center'), 
             stack      = T)
    addStyle(wb         = wb, 
             sheet      = as.character(year), 
             rows       = 12, 
             cols       = 1:9, 
             gridExpand = T, 
             style      = createStyle(fontSize       = 8, 
                                      textDecoration = 'italic',
                                      valign         = 'center',
                                      wrapText       = T), 
             stack      = T)
    mergeCells(wb    = wb, 
               sheet = as.character(year), 
               rows  = 12:13, 
               cols  = 1:9)
    setColWidths(wb    = wb, 
                 sheet  = as.character(year), 
                 cols   = 1:9, 
                 widths = c(15, 8, 11, 11, 11, 11, 11, 15, 12))
    
  }
  
  
  # Write workbook 
  saveWorkbook(wb   = wb, 
               file = file.path(globals$output_root, 
                                id,
                                'static',
                                'supplemental', 
                                file_name), 
               overwrite = T)
}




calc_distribution_tables = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # Calculates standard distribution tables for all scenarios. 
  # 
  # Parameters:
  #   - counterfactual_ids : (str) list of non-baseline scenario IDs
  # 
  # Returns: void. 
  #----------------------------------------------------------------------------
  
  counterfactual_ids %>% 
    walk(.f = ~ calc_distribution(.x, 'baseline'))
}



calc_stacked_distribution_tables = function(counterfactual_ids) {
  
  #----------------------------------------------------------------------------
  # For all non-baseline scenarios, calculates distribution table relative to
  # prior scenario in stacking order. 
  # 
  # Parameters:
  #   - counterfactual_ids : (str) list of non-baseline scenario IDs
  # 
  # Returns: void.
  #----------------------------------------------------------------------------
  
  for (i in 2:length(counterfactual_ids)) { 
    calc_distribution(id          = counterfactual_ids[i], 
                      baseline_id = counterfactual_ids[i - 1], 
                      file_name   = 'stacked_distribution.xlsx')
  }
}

#-----------------------------------------------------------------------
# Post-processing functions to gather revenue estimates for fiscal years
#-----------------------------------------------------------------------

calc_receipts = function(totals) {
  #----------------------------------------------------------------------------
  # Calculates a scenario's receipts 
  # 
  # Parameters:
  #   - totals (df) :  a dataframe containing columns for calendar year totals of
  #       - withheld_pr        : Payroll taxes withheld
  #       - withheld_iit       : Individual income taxes withheld
  #       - nonwithheld_iit    : Individual income taxes paid at time of filing
  #       - nonwithheld_refund : Refunds issued
  #
  # Returns:  void, writes a dataframe for the scenario containing values for:
  #   - Fiscal Year
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  #DELETE IF THE DIRECTORY ALREADY EXISTS AND OUT_DIR IS IN MEMORY
  t = Sys.time()
  out_dir = file.path(data_root,
                      'model_data',
                      'Tax-Simulator',
                      paste0('v', version),
                      paste0(year(t), month(t), day(t), hour(t)), #CHECK W/JOHN
                      runscript$ID
                      )
  dir.create(out_dir, recursive = T)
  
  totals %>%
    mutate(
      
      # 75% of current year + 25% of previous year
      PayrollTax = (.75 * withheld_pr) + (.25 * lag(withheld_pr)),
      IndividualIncomeTax = (.75 * withheld_iit) + (.25 * lag(withheld_iit)) + nonwithheld_iit,
      
      #Simple rename
      RefundableCreditOutlays = nonwithheld_refund
    ) %>%
    #Drop incomplete year
    filter(Year!=min(Year)) %>%
    
    select(Year, PayrollTax, IndividualIncomeTax, RefundableCreditOutlays) %>%
    write.csv(., 
              file=file.path(out_dir, paste0("receipts_",ID,".csv")), 
              row.names=F)  

}

calc_rev_est = function() {
  #----------------------------------------------------------------------------
  # Calculates all scenario revenue estimate deltas when compared to the baseline
  # 
  # Parameters: Void, reads in receipts from all scenarios including the baseline
  #
  # Returns: Void, writes dataframes containing, for each scenario, fiscal year deltas for
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  
  #read in base
  base = read.csv(file.path("/gpfs/gibbs/project/sarin/shared/model_data/v", 
                                   version, 
                                   VINTAGE,
                                   "baseline/receipts_baseline.csv"))
  
  base <- base %>% mutate(total = PayrollTax + IndividualIncomeTax - RefundableCreditOutlays)
  
  scenarios <- runscript$id
  
  for(run in scenarios) {
    path = file.path("/gpfs/gibbs/project/sarin/shared/model_data/v", 
                            version, 
                            VINTAGE, 
                            run)
    sim = read.csv(paste0(path,"receipts_", run, ".csv"))
    
    write.csv(
      calc_rev_delta(base, sim),
      paste0(path,"revenue_estimates",run,".csv"),
      row.names = F
    )
  }
}

calc_rev_delta = function(base, sim) {
  #----------------------------------------------------------------------------
  # Calculates a single scenario's revenue estimate delta when compared to the baseline
  # 
  # Parameters: 
  #   - base    (df) : dataframe containing baseline receipts
  #   - sim     (df) : dataframe containing one scenario's receipts
  #
  # Returns: dataframe containing, for each fiscal year, deltas for
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  #Sim total
  sim <- sim %>% mutate(total = PayrollTax + IndividualIncomeTax - RefundableCreditOutlays)
  
  # merge, variables have the same name so are split x/y
  base <- left_join(base, sim, by="Year")

  # mutate deltas
  base %>%
    mutate(
      Total = total.y - total.x,
      PayrollTax = PayrollTax.y - PayrollTax.x,
      IndividualIncomeTax = IndividualIncomeTax.y - IndividualIncomeTax.x,
      RefundableCreditOutlays = RefundableCreditOutlays.y - RefundableCreditOutlays.x
    ) %>%
    
    select(Year, Total, PayrollTax, IndividualIncomeTax, RefundableCreditOutlays) %>%
    return()
}

calc_stacked = function() {
  #----------------------------------------------------------------------------
  # Calculates stacked revenue deltas. Usable if scenarios build off of one another
  # 
  # Parameters: Void, reads in receipts from all scenarios including the baseline
  #
  # Returns: Void, writes dataframe with fiscal year columns stacking 
  #          scenario revenue deltas for
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  
  # Initialize output
  stack <- data.frame(matrix(ncol=4, nrow=1))
  colnames(stack) = c("...1", "Year", "Series", "receipts")
  
  # Collect scenario names
  scenarios <- runscript$id
  
  # Loop through scenarios including baseline (makes it tidier)
  for(run in scenarios) {
    sim = read.csv(file.path(
      "/gpfs/gibbs/project/sarin/shared/model_data/v", 
      version, 
      VINTAGE, 
      run,
      paste0("revenue_estimates_", run, ".csv"))
    )
    
    # Organize into year -> revenue format
    sim <- sim %>%
      pivot_longer(
        cols = !Year,
        names_to = "Series",
        values_to = "receipts"
      )
    
    # Add column with scenario name
    sim <- bind_cols(rep(run, nrow(sim)), sim)
    stack <- bind_rows(stack, sim)
  }
  
  # Drop initial row used to merge tidily, rename scenario column
  stack <- stack[-1,]
  colnames(stack)[1] <- "Scenario" 
  
  
  stack <- stack %>%
    group_by(Year, Series) %>%
    mutate(delta= receipts - lag(receipts)) %>%
    select(Year, Scenario, Series, delta) %>%
    pivot_wider(
      names_from = Year,
      values_from = delta
    ) %>%
    
    # Drop baseline full of zeros or NA
    filter(Scenario!="baseline")
  
  # Unnecessary for analysis, but it made the loop tidier
  unlink("revenue_estimates_base.csv")
  
  # Don't know what path to send this down
  write.csv(stack, "stacked_revenue_estimates.csv", row.names = F)
}



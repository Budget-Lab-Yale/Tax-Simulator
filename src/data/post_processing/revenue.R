#-----------------------------------------------------------------------
# Post-processing functions to gather revenue estimates for fiscal years
#-----------------------------------------------------------------------

calc_receipts = function(totals) {
  
  #----------------------------------------------------------------------------
  # Calculates a scenario's receipts 
  # 
  # Parameters:
  #   - totals (df) : dataframe containing columns for calendar year totals of
  #        - pmt_iit_nonwithheld (dbl)    : income tax paid at time of filing
  #        - pmt_iit_withheld (dbl)       : income tax withheld or paid 
  #                                         quarterly
  #        - pmt_refund_nonwithheld (dbl) : payments for refundable credits 
  #                                         paid during filing season
  #        - pmt_refund_withheld (dbl)    : advance credits paid throughout 
  #                                         year
  #        - pmt_pr_nonwithheld (dbl)     : payroll tax paid at time of filing
  #        - pmt_pr_withheld (dbl)        : payroll tax withheld (FICA) or paid 
  #                                         quarterly (SECA) 
  #
  # Returns:  void, writes a dataframe for the scenario containing values for:
  #   - Fiscal Year
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  totals %>%
    mutate(
      
      # FY receipts: nonwithheld tax plus 75% of current CY withheld tax plus 
      # 25% of previous CY withheld 
      RefundableCreditOutlays = 0.75 * pmt_iit_withheld + 
                                0.25 * lag(pmt_iit_withheld) + 
                                pmt_refund_nonwithheld,
      
      IndividualIncomeTax = 0.75 * pmt_iit_withheld + 
                            0.25 * lag(pmt_iit_withheld) + 
                            pmt_iit_nonwithheld,
      
      PayrollTax = 0.75 * pmt_pr_withheld + 
                   0.25 * lag(pmt_pr_withheld) + 
                   pmt_pr_nonwithheld
    
    ) %>%
    
    # Drop incomplete year
    filter(Year != min(Year)) %>%
    
    select(Year, PayrollTax, IndividualIncomeTax, RefundableCreditOutlays) %>%
    write.csv(file = file.path(out_dir, paste0("receipts_",ID,".csv")), 
              row.names = F)  

}



calc_rev_est = function() {
  
  #----------------------------------------------------------------------------
  # Calculates all scenario revenue estimate deltas when compared to the 
  # baseline.
  # 
  # Parameters: Void; reads in receipts from all scenarios including the 
  #             baseline.
  #
  # Returns: Void, writes dataframes containing, for each scenario, fiscal year 
  # deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  # Read in base
  base = read.csv(file.path("/gpfs/gibbs/project/sarin/shared/model_data/v", 
                                   version, 
                                   VINTAGE,
                                   "baseline/receipts_baseline.csv"))
  
  base <- base %>% 
    
    # Create and rename variables with b for baseline
    mutate(total_b = PayrollTax + IndividualIncomeTax - RefundableCreditOutlays) %>%
    rename(
      PayrollTax_b = PayrollTax,
      IndividualIncomeTax_b = IndividualIncomeTax,
      RefundableCreditOutlays_b = RefundableCreditOutlays
    )
  
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
  # Calculates a single scenario's revenue estimate delta when compared to the 
  # baseline.
  # 
  # Parameters: 
  #   - base (df) : dataframe containing baseline receipts
  #   - sim  (df) : dataframe containing one scenario's receipts
  #
  # Returns: dataframe containing, for each fiscal year, deltas for
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  # Sim total
  sim <- sim %>% 
    
    # Create and rename variables with s for simulation
    mutate(total_s = PayrollTax + IndividualIncomeTax - RefundableCreditOutlays) %>%
    rename(
      PayrollTax_s = PayrollTax,
      IndividualIncomeTax_s = IndividualIncomeTax,
      RefundableCreditOutlays_s = RefundableCreditOutlays
    )
  
  # Merge, variables have the same name so are split x/y
  base <- left_join(base, sim, by = "Year")

  # Mutate deltas
  base %>%
    mutate(
      Total = total_s - total_b,
      PayrollTax = PayrollTax_s - PayrollTax_b,
      IndividualIncomeTax = IndividualIncomeTax_s - IndividualIncomeTax_b,
      RefundableCreditOutlays = RefundableCreditOutlays_s - RefundableCreditOutlays_b
    ) %>%
    
    select(Year, Total, PayrollTax, IndividualIncomeTax, RefundableCreditOutlays) %>%
    return()
}



calc_stacked = function() {
  
  #----------------------------------------------------------------------------
  # Calculates stacked revenue deltas. Usable if scenarios build off of one 
  # another.
  # 
  # Parameters: Void, reads in receipts from all scenarios including the 
  # baseline.
  #
  # Returns: Void, writes dataframe with fiscal year columns stacking 
  #          scenario revenue deltas for:
  #   - Total revenue
  #   - Payroll Tax
  #   - Individual Income Tax
  #   - Refundable Credit Outlays
  #   
  #----------------------------------------------------------------------------
  
  
  # Initialize output
  stack <- data.frame(matrix(ncol=4, nrow=1))
  colnames(stack) = c("Scenario", "Year", "Series", "receipts")
  
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
      ) %>%
      mutate(Scenario = run)
    
  }
  
  # Drop initial row used to merge tidily, rename scenario column
  stack <- stack[-1,]
  
  
  stack <- stack %>%
    group_by(Year, Series) %>%
    mutate(delta = receipts - lag(receipts)) %>%
    select(Year, Scenario, Series, delta) %>%
    pivot_wider(
      names_from = Year,
      values_from = delta
    ) %>%
    
    # Drop baseline full of zeros or NA
    filter(Scenario != "baseline")
  
  # Unnecessary for analysis, but it made the loop tidier
  unlink("revenue_estimates_base.csv")
  
  # Don't know what path to send this down
  write.csv(stack, "stacked_revenue_estimates.csv", row.names = F)
}



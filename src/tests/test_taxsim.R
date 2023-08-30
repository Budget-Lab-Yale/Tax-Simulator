#---------------------------------------------------------------
# Functions to compare YBL Tax Simulator's Output to NBER's Taxsim
#---------------------------------------------------------------


taxsim_check = function(tax_units) {
  
  #----------------------------------------------------------------------------
  # Compares results from YBL tax microsimulator to NBER's Taxsim.
  #
  # ONLY USEFUL FOR HISTORICAL COMPARISON, NOT AGAINST FUTURE FORECASTS
  #
  # Parameters:
  #   - tax_units (df) : dataframe of tax units after passing through calculator
  #
  # Returns: dataframe populated with the dollar differences between simulators.
  # Variables consist of:
  #   - liab_fed_dif      (dbl) : Federal Income Tax liability
  #   - liab_pr_dif       (dbl) : Payroll Tax liability
  #   - liab_taxable_dif  (dbl) : Tax on Taxable Income
  #   - liab_amt_dif      (dbl) : Alternative Minimum Tax liability
  #   - agi_dif           (dbl) : AGI
  #   - ui_dif            (dbl) : Unemployment Insurance Income
  #   - ss_dif            (dbl) : Social Security Income
  #   - txbl_dif          (dbl) : Taxable Income
  #   - amt_dif           (dbl) : Alternative Minimum Tax Income
  #   - std_ded_dif       (dbl) : Standard Deduction
  #   - pe_dif            (dbl) : Personal Exemptions
  #   - item_ded_dif      (dbl) : Itemized Deduction
  #   - ctc_dif           (dbl) : Child Tax Credit benefit
  #   - actc_dif          (dbl) : Refundable Component of Child Tax Credit
  #   - cdctc_dif         (dbl) : Child and Dependent Care Tax Credit
  #   - eitc_dif          (dbl) : Earned Income Tax Credit
  #   - se_dif            (dbl) : Self Employment Income
  #   - liab_add_med_dif  (dbl) : Medicare Tax on Earned Income
  #----------------------------------------------------------------------------
  
  # Install and load library as needed
  if(system.file(package='usincometaxes')=="") {
    install.packages("usincometaxes")
  }
  
  library(usincometaxes)
  
  # Convert to Taxsim readable
  taxsim_crosswalk(tax_units) %>%
 
    # Run Taxsim
    taxsim_calculate_taxes(
      .data = .,
      marginal_tax_rates = 'Wages',
      return_all_information = T
    ) %>%
    
      # Compare the two simulators
      taxsim_check_against(., tax_units) %>%
      
      # Return the differences
      return()
}

taxsim_crosswalk = function(tax_units) {
  
  #----------------------------------------------------------------------------
  # Converts YBL Tax Simulator inputs and outputs into NBER Taxsim readable
  # format. 
  # 
  # Parameters:
  #   - tax_units (df) : dataframe of tax units after passing through calculator
  #
  # Returns: dataframe populated with the variables converted to Taxsim input
  #----------------------------------------------------------------------------
  
  tax_units %>% 
    
    # Rename existing variables
    rename(
      
      # Demographics
      page = age1,
      sage = age2,
      depx = n_dep,
      age1 = dep_age_1,
      age2 = dep_age_2,
      age3 = dep_age_3,
      
      # Earnings
      pwages = wages1,
      swages = wages2, 
      psemp  = se1, 
      ssemp  = se2,
      
      # Capital income
      dividends = qual_div,
      stcg      = kg_st, 
      ltcg      = lg_lt, 
      
      # OASDI
      ggsi = gross_ss,
      
      # Real estate SALT
      proptax = salt_prop,
      
      # Childcare expenses 
      childcare = care_exp
      
    ) %>% 
    
    # Derive and recode variables
    mutate(
      
      # Filing status
      mstat = case_when(
        filing_status %in% c(1, 4) ~ "single",
        filing_status == 2         ~ "married, jointly",
        filing_status == 3         ~ "married, seperately",
        T                          ~ NA
      ),
      mstat = if_else(dep_status == 1, 8, mstat),
      
      # State
      state = 'No state',
      
      # Taxable interest and ordinary dividends 
      intrec = txbl_int + div - qual_div,
      
      # Taxable retirement income distributions
      pensions = txbl_ira_dist + txbl_pens_dist,
      
      # Imputation for ui benefits split
      pui = ui * (wages1 / wages),
      sui = ui - pui,
      
      # Other income 
      otherprop = sch_e - (part_passive - part_passive_loss + scorp_passive - scorp_active_loss), 
      nonprop   = state_ref + alimony + nols + other_inc - ed_exp - hsa_contr - keogh_contr + 
        se_health - early_penalty - alimony_exp - trad_contr_ira - sl_int_ded,
      
      # Feenberg's medical deduction allocation (https://taxsim.nber.org/taxsim-calc9/medical_deduction.html)
      med_pref    = pmin(med_item_ded, pmax(0, agi) * 0.025),
      med_nonpref = med_item_ded - med_pref,
      
      # Itemized deductions
      otheritem = salt_inc_sales + salt_pers + med_pref + misc_item_ded,
      mortgage  = mort_int_item_ded + med_nonpref + char_item_ded + casualty_item_ded, 
      
      # QBI deduction variables...not sure what our setup will be. Will add after 
      # completing QBI calc function.
      scorp    = 0,
      pbusinc  = 0, 
      pprofin  = 0, 
      sbusinc  = 0, 
      sprofinc = 0,
      
      # taxsim vars we don't care about
      transfers = 0,
      rentpaid  = 0
      
    ) %>% 
    
    # Final selection in order of taxsim 3.5 list
    select(
      taxsimid, 
      year, 
      state, 
      mstat, 
      page, sage,
      depx, age1, age2, age3,
      pwages, swages,
      psemp, ssemp,
      dividends, 
      intrec,
      stcg, ltcg,
      otherprop, nonprop,
      pensions, 
      ggsi, 
      pui, sui,
      transfers, 
      rentpaid,
      proptax, 
      otheritem, 
      childcare, 
      mortgage,
      scorp, pbusinc, pprofin, sbusinc, sprofinc
    ) %>%
    return()
}

taxsim_check_against = function(test_cases, tax_units) {
  
  #----------------------------------------------------------------------------
  # Produces dollar difference between YBL Tax Simulator and NBER Taxsim
  #
  # Parameters:
  #   - tax_units (df) : dataframe of tax units after passing through the calculator
  #                      and taxsim_check.
  #
  # Returns: dataframe with dollar differences on comparable variables.
  # Variables consist of:
  #   - liab_fed_dif      (dbl) : Federal Income Tax liability
  #   - liab_pr_dif       (dbl) : Payroll Tax liability
  #   - liab_taxable_dif  (dbl) : Tax on Taxable Income
  #   - liab_amt_dif      (dbl) : Alternative Minimum Tax liability
  #   - agi_dif           (dbl) : AGI
  #   - ui_dif            (dbl) : Unemployment Insurance Income
  #   - ss_dif            (dbl) : Social Security Income
  #   - txbl_dif          (dbl) : Taxable Income
  #   - amt_dif           (dbl) : Alternative Minimum Tax Income
  #   - std_ded_dif       (dbl) : Standard Deduction
  #   - pe_dif            (dbl) : Personal Exemptions
  #   - item_ded_dif      (dbl) : Itemized Deduction
  #   - ctc_dif           (dbl) : Child Tax Credit benefit
  #   - actc_dif          (dbl) : Refundable Component of Child Tax Credit
  #   - cdctc_dif         (dbl) : Child and Dependent Care Tax Credit
  #   - eitc_dif          (dbl) : Earned Income Tax Credit
  #   - se_dif            (dbl) : Self Employment Income
  #   - liab_add_med_dif  (dbl) : Medicare Tax on Earned Income
  #----------------------------------------------------------------------------
  #CHECK VARIABLE NAMES AGAINST COMPLETED CALCULATOR
  
  test_cases %>%
    mutate(
      
      #CORE PAYMENTS
      #liab_fed_dif = fiitax - tax_units$liab_fed,
      liab_pr_dif = tfica - tax_units$liab_pr, #FLAG, FICA RATES/LIAB GET STRANGE WITH HIGH INCOME
      liab_taxable_dif = v19_tax_on_taxable_income - tax_units$liab_bc - tax_units$liab_amt,
      
      #INCOME TOTALS
      agi_dif = v10_federal_agi - tax_units$agi,
      ui_dif = v11_ui_agi - tax_units$ui,
      ss_dif = v12_soc_sec_agi - tax_units$txbl_ss, # CHECK
      #txbl_dif = v18_federal_taxable_income - tax_units$,
      #FEDERAL INCOME TAX BEFORE TAX CREDITS
      
      #DEDUCTIONS AND EXEMPTIONS
      std_ded_dif = v13_zero_bracket_amount - tax_units$std_ded,
      pe_dif = v14_personal_exemptions - tax_units$pe,
      #exemption, deduction phaseout works strangely TIME MATTERS HERE
      item_ded_dif = v17_itemized_deductions - tax_units$item_ded,
      
      #TAX CREDITS
      ctc_dif = v22_child_tax_credit_adjusted - tax_units$ctc,
      actc_dif = v23_child_tax_credit_refundable - tax_units$actc,
      cdctc_dif = v24_child_care_credit - tax_units$cdctc,
      eitc_dif = v25_eitc - tax_units$eitc,
      
      #AMT
      #amt_dif = v26_amt_income - tax_units$amt,
      liab_amt_dif = v27_amt_liability - liab_amt,
      
      #Additional Federal
      #se_dif = v42_self_emp_income - tax_units$se,
      #medicare tax unearned income  capital income (niit) NOT INCLUDED IN FICA
      liab_add_med_dif = v44_medicare_tax_earned_income - liab_add_med
    ) %>%
    
    #Select differences to return
    select(
      #ADD: liab_fed_dif, txbl_dif, amt_dif, se_dif
      liab_pr_dif, liab_taxable_dif,
      agi_dif, ui_dif, ss_dif,
      std_ded_dif, pe_dif, item_ded_dif,
      ctc_dif, actc_dif, cdctc_dif, eitc_dif,
      liab_amt_dif,
      liab_add_med_dif
    ) %>%
    return()
}

taxsim_pct_dif = function(tax_units, tol = .05) {
  
  #----------------------------------------------------------------------------
  # Checks differences to see if they are within an acceptable error tolerance
  #
  # Parameters:
  #   - tax_units (df) : dataframe of tax units after passing through the calculator
  #                      and taxsim_check.
  #   - tol      (dbl) : Error tolerance expressed in decimal form. Default to .05
  #
  # Returns: dataframe populated with true or false if a difference exceeds error
  #          tolerance.
  # Variables consist of:
  #   - liab_fed_off      (dbl) : Federal Income Tax liability
  #   - liab_pr_off       (dbl) : Payroll Tax liability
  #   - liab_taxable_off  (dbl) : Tax on Taxable Income
  #   - liab_amt_off      (dbl) : Alternative Minimum Tax liability
  #   - agi_off           (dbl) : AGI
  #   - ui_off            (dbl) : Unemployment Insurance Income
  #   - ss_off            (dbl) : Social Security Income
  #   - txbl_off          (dbl) : Taxable Income
  #   - amt_off           (dbl) : Alternative Minimum Tax Income
  #   - std_ded_off       (dbl) : Standard Deduction
  #   - pe_off            (dbl) : Personal Exemptions
  #   - item_ded_off      (dbl) : Itemized Deduction
  #   - ctc_off           (dbl) : Child Tax Credit benefit
  #   - actc_off          (dbl) : Refundable Component of Child Tax Credit
  #   - cdctc_off         (dbl) : Child and Dependent Care Tax Credit
  #   - eitc_off          (dbl) : Earned Income Tax Credit
  #   - se_off            (dbl) : Self Employment Income
  #   - liab_add_med_off  (dbl) : Medicare Tax on Earned Income
  #----------------------------------------------------------------------------
  
  tax_units %>%
      mutate(
      #LIABILITIES
      #liab_fed_off = abs(liab_fed_dif)/liab_fed>=tol,
      liab_pr_off = abs(liab_pr_dif)/liab_pr>=tol,
      liab_taxable_off = abs(liab_taxable_dif)/(liab_iit+liab_niit)>=tol,
      liab_amt_off = abs(liab_amt_dif)/liab_amt>=tol,
      
      #INCOME
      agi_off = abs(agi_dif)/agi>=tol,
      ui_off = abs(ui_dif)/ugi>=tol,
      ss_off = abs(ss_dif)/txbl_ss>=tol,
      #txbl_off = abs(txbl_dif)/ >=tol,
      #amt_off = abs(amt_dif)/amt>=tol,
      
      #DEDUCTIONS
      std_ded_off = abs(std_ded_dif)/std_ded>=tol,
      pe_off = abs(pe_dif)/pe>=tol,
      item_ded_off = abs(item_ded_dif)/item_ded>=tol,
      
      #CREDITS
      ctc_off = abs(ctc_dif)/ctc>=tol,
      actc_off = abs(actc_dif)/actc>=tol,
      cdctc_off = abs(cdctc_dif)/cdctc>=tol,
      eitc_off = abs(eitc_dif)/eitc>=tol,
      
      #OTHER FEDERAL
      #se_off = abs(se_dif)/se>=tol,
      liab_add_med_off = abs(liab_add_med_diff)/liab_add_med>=tol
    ) %>%
    
    #Select percent changes to return
    select(
      #ADD: liab_fed_off, txbl_off, amt_off, se_off
      liab_pr_off,liab_taxable_off,liab_amt_off,
      agi_off, ui_off, ss_off,
      std_ded_off, pe_off, item_ded_off,
      ctc_off, actc_off, cdctc_off, eitc_off,
      liab_add_med_off
    ) %>%
    return()
}

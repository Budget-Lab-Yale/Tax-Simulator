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

taxsim_crosswalk = function(tax_units, state = 'No state',
                            independent_item = FALSE,
                            state_subtracts_ref = TRUE) {

  #----------------------------------------------------------------------------
  # Converts YBL Tax Simulator inputs and outputs into NBER Taxsim readable
  # format.
  #
  # Parameters:
  #   - tax_units (df) : dataframe of tax units after passing through calculator
  #   - state (str)    : two-letter postal code for TAXSIM's state calculation,
  #                      or 'No state' to disable it (default; federal-only)
  #   - independent_item (bool) : whether this state's itemization election is
  #                      independent of the federal one (or it computes an
  #                      itemized credit regardless of election, WI-style).
  #                      Must mirror the state's encoded law -- see
  #                      cross_model_taxsim_leg()
  #
  # Returns: dataframe populated with the variables converted to Taxsim input
  #----------------------------------------------------------------------------

  # Ensure a unique TAXSIM record id exists (PUF id when available)
  if (!'taxsimid' %in% names(tax_units)) {
    tax_units$taxsimid = if ('id' %in% names(tax_units)) tax_units$id
                         else seq_len(nrow(tax_units))
  }

  # Independent-election state mode hands the as-if-itemizing Schedule A
  # amounts, because those states let federal standard-deduction takers
  # itemize on the state return and both our calculator and TAXSIM's state
  # logic elect independently. Coupled and federal-gated states keep the
  # zeroed as-claimed components: handing expenses there lets TAXSIM
  # itemize the state return where the law pins the election to the federal
  # standard deduction (verified regression: VA 2019, 805 records flipped).
  # Federal-only mode also keeps as-claimed amounts (established baseline)
  if (state != 'No state' && independent_item) {
    tax_units = tax_units %>%
      mutate(med_item_ded      = med_item_ded_potential,
             misc_item_ded     = misc_item_ded_potential,
             mort_int_item_ded = mort_int_item_ded_potential,
             char_item_ded     = char_item_ded_potential,
             casualty_item_ded = casualty_item_ded_potential)
  }

  tax_units %>%
    
    # Rename existing variables
    rename(
      
      # Demographics
      page = age1,
      sage = age2,
      depx = n_dep,
      age1 = dep_age1,
      age2 = dep_age2,
      age3 = dep_age3,
      
      # Earnings
      pwages = wages1,
      swages = wages2, 
      psemp  = se1, 
      ssemp  = se2,
      
      # Capital income
      dividends = div_pref,
      stcg      = kg_st, 
      ltcg      = kg_lt,
      
      # OASDI (TAXSIM-35 name is gssi; other spellings are silently dropped)
      gssi = gross_ss,
      
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
        filing_status == 3         ~ "married, separately",
        T                          ~ NA
      ),
      mstat = if_else(dep_status == 1, 'dependent child', mstat),

      # State
      state = .env$state,

      # TAXSIM rejects spouse variables on non-joint returns: fold spouse
      # amounts into primary and zero the spouse fields (no-op when already 0)
      joint  = mstat == 'married, jointly',
      pwages = if_else(joint, pwages, pwages + swages),
      swages = if_else(joint, swages, 0),
      psemp  = if_else(joint, psemp, psemp + ssemp),
      ssemp  = if_else(joint, ssemp, 0),
      sage   = if_else(joint, sage, 0),

      # Taxable interest and ordinary dividends
      intrec = txbl_int + div_ord,

      # Taxable retirement income distributions
      pensions = txbl_ira_dist + txbl_pens_dist,

      # Imputation for ui benefits split (all to primary when no wages
      # or on a non-joint return)
      pui = if_else(joint & wages > 0, ui * pwages / wages, ui),
      sui = pmax(0, ui - pui),   # pmax guards float residuals; TAXSIM rejects negatives

      # QBI input reallocation (Section 199A). Mirrors calc_qbi_ded()'s
      # business-type aggregation: SE income (= se1/se2 = sole_prop + farm +
      # part_se, currently in psemp/ssemp) moves to pbusinc (non-SSTB) or
      # pprofinc (SSTB) -- both SECA-subject in TAXSIM, so the payroll base
      # is unchanged. Non-SE QBI income (S corp + non-SE partnership,
      # non-SSTB only) moves from otherprop into TAXSIM's scorp input
      # (QBI-eligible, no SECA); SSTB non-SE income stays in otherprop
      # because TAXSIM has no QBI-no-SECA SSTB slot (pprofinc would wrongly
      # add SECA). Totals are preserved by construction. Remaining
      # approximation: TAXSIM assumes a sufficient wage bill, so its QBID
      # can exceed ours above the phaseout for low-wagebill businesses.
      sstb_sp = coalesce(sstb_sole_prop, 0),
      sstb_f  = coalesce(sstb_farm, 0),
      sstb_p  = coalesce(sstb_part, 0),
      sstb_s  = coalesce(sstb_scorp, 0),
      qbi_scorp_input = scorp * (1 - sstb_s) + (part - part_se) * (1 - sstb_p),
      pbusinc  = sole_prop1 * (1 - sstb_sp) + farm1 * (1 - sstb_f) +
                 part_se1 * (1 - sstb_p),
      pprofinc = sole_prop1 * sstb_sp + farm1 * sstb_f + part_se1 * sstb_p,
      sbusinc  = sole_prop2 * (1 - sstb_sp) + farm2 * (1 - sstb_f) +
                 part_se2 * (1 - sstb_p),
      sprofinc = sole_prop2 * sstb_sp + farm2 * sstb_f + part_se2 * sstb_p,

      # Fold spouse QBI income into primary on non-joint returns (TAXSIM
      # rejects spouse fields there), and zero psemp/ssemp: all SE income
      # is now carried by the QBI inputs
      pbusinc  = if_else(joint, pbusinc, pbusinc + sbusinc),
      pprofinc = if_else(joint, pprofinc, pprofinc + sprofinc),
      sbusinc  = if_else(joint, sbusinc, 0),
      sprofinc = if_else(joint, sprofinc, 0),
      psemp = 0,
      ssemp = 0,
      
      # Other property income, mirroring our AGI's Schedule E concept
      # (calc_agi(): sch_e = part_scorp + net_rent + net_estate, pass-through
      # losses clamped), less the SE portion of partnership income already
      # counted in the SECA-subject inputs, less non-SE QBI income moved to
      # the scorp input, plus Form 4797 gains (no TAXSIM input of their own;
      # other_gains is in our AGI)
      otherprop = sch_e - part_se + other_gains - qbi_scorp_input,

      # Non-property income less above-the-line deductions, mirroring
      # calc_agi(). Notes: nols is NOT in our AGI (AMT only); alimony is
      # gated on pre-repeal divorces; TAXSIM computes its own 1/2 SECA
      # deduction from psemp/ssemp so liab_seca_er is not subtracted here.
      # State refunds: TAXSIM has no state-refund input, so a refund handed
      # inside nonprop is invisible to it as a refund. Which way that cuts
      # depends on the state. Where the state SUBTRACTS its own refund from
      # federal AGI (st_agi.sub_state_ref = 1, 22 of the 24 states that set
      # it), TAXSIM could not apply the subtraction and would over-tax, so
      # omitting the refund keeps the state calculation right at the cost of
      # a small federal AGI gap. Where the state does NOT subtract it (RI and
      # ND, and the schema default), the refund belongs in the state base and
      # omitting it makes TAXSIM's state AGI low by exactly state_ref --
      # which is a state-tax difference, not just a federal one, and at RI's
      # 5.99% top rate it broke the $100 tolerance on the whole top of the
      # income distribution. Hand it over in that case
      alimony_qualifies = !is.na(divorce_year) &
        (divorce_year < agi.alimony_repeal_year),
      nonprop = state_ref * (.env$state == 'No state' ||
                             !.env$state_subtracts_ref) +
        alimony * alimony_qualifies + other_inc -
        char_above_ded - other_above_ded - ed_exp - hsa_contr - keogh_contr -
        se_health - early_penalty - alimony_exp * alimony_qualifies -
        trad_contr_ira - pmin(tuition_ded, agi.tuition_ded_limit) -
        pmin(dpad, agi.dpad_limit) - sl_int_ded,

      # TAXSIM requires nonprop >= 0; fold any negative remainder into
      # otherprop (accepts negatives; both feed AGI identically)
      otherprop = otherprop + pmin(0, nonprop),
      nonprop   = pmax(0, nonprop),
      
      # Feenberg's medical deduction allocation (https://taxsim.nber.org/taxsim-calc9/medical_deduction.html)
      med_pref    = pmin(med_item_ded, pmax(0, agi) * 0.025),
      med_nonpref = med_item_ded - med_pref,
      
      # Itemized deductions
      otheritem = salt_inc_sales + salt_pers + med_pref + misc_item_ded,
      mortgage  = mort_int_item_ded + med_nonpref + char_item_ded + casualty_item_ded, 
      
      # Non-SE QBI-eligible income (computed in the reallocation block above;
      # assigned here because `scorp` overwrites the derived frame column)
      scorp = qbi_scorp_input,

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
      gssi,
      pui, sui,
      transfers,
      rentpaid,
      proptax,
      otheritem,
      childcare,
      mortgage,
      scorp, pbusinc, pprofinc, sbusinc, sprofinc
    ) %>%
    return()
}

taxsim_check_against = function(test_cases, tax_units) {
  
  #----------------------------------------------------------------------------
  # Produces dollar difference between YBL Tax Simulator and NBER Taxsim
  #
  # Parameters:
  #   - test_cases (df) : test cases with NBER taxsim output variables   
  #   - tax_units (df)  : dataframe of tax units after passing through the calculator
  #                       and taxsim_check.
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
      liab_amt_dif = v27_amt_liability - tax_units$liab_amt,
      
      #Additional Federal
      #se_dif = v42_self_emp_income - tax_units$se,
      #medicare tax unearned income  capital income (niit) NOT INCLUDED IN FICA
      liab_add_med_dif = v44_medicare_tax_earned_income - tax_units$liab_add_med
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
      ui_off = abs(ui_dif)/ui>=tol,
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
      liab_add_med_off = abs(liab_add_med_dif)/liab_add_med>=tol
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

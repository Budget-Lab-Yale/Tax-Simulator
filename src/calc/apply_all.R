#------------------
# TODO
#------------------


tax_units %>% 
  
  # Employment taxes
  bind_cols(calc_pr(.)) %>%
  
  # Net capital gain includable in AGI
  bind_cols(calc_kg(.)) %>% 
  
  # AGI, including taxable OASI benefits
  bind_cols(calc_agi(.)) %>% 
  
  # Standard deduction
  bind_cols(calc_std_ded(.)) %>% 
  
  # Itemized deductions
  bind_cols(calc_item_ded(.)) %>% 
  
  # Charity deduction determination
  bind_cols(calc_char(.)) %>% 
  
  # Personal exemptions
  bind_cols(calc_pe(.)) %>% 
  
  # QBI deduction
  bind_cols(calc_qbi_ded(.)) %>% 
  
  # Taxable income
  bind_cols(calc_ti(.)) %>% 
  
  # Tax liability
  bind_cols(calc_tax(.)) %>%
  
  # Alternative minimum tax
  bind_cols(calc_amt(.)) %>% 
  
  # CTC
  bind_cols(calc_ctc(.)) %>% 
  
  # CDCTC
  bind_cols(calc_cdctc(.)) %>% 
  
  # Education credits
  bind_cols(calc_ed_cred(.)) %>% 
  
  # Savers credit
  bind_cols(calc_savers(.)) %>% 
  
  # Energy credits
  bind_cols(calc_energy_cred(.)) %>% 
  
  # Other nonrefundable credits 
  bind_cols(calc_other_cred(.)) %>% 
  
  # EITC
  bind_cols(calc_eitc(.)) %>% 

  # Rebates / UBI
  bind_cols(calc_rebate(.)) %>% 

  # NIIT
  bind_cols(calc_niit(.)) %>% 

  # Liability
  bind_cols(calc_liab(.))  

  
  
  
  
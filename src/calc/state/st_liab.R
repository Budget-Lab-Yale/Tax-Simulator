#------------------------------------------------------------
# Function to calculate state liability and state-filer flag
#------------------------------------------------------------

# Set return variables for function. NOTE: st_taxable_income_surtax is exposed
# for reporting but is ALREADY included in liab_st_iit below -- do not add the
# two together in any downstream aggregation.
return_vars$calc_st_liab = c('st_taxable_income_surtax', 'liab_st_iit',
                              'st_filer')


calc_st_liab = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates net state income tax liability (refundable credits may drive
  # it negative, matching the federal liab_iit convention), including a
  # taxable-income surtax that is imposed after nonrefundable credits, and the
  # state-filer flag per the state's filing requirement (plan §6: federal
  # filers overcount state filers by 8-35% if unmodeled).
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - st_taxable_income_surtax (dbl) : post-credit taxable-income surtax
  #   - liab_st_iit (dbl) : net state individual income tax liability
  #   - st_filer (bool)   : whether the unit files a state return
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'filer',              # (bool) whether unit files a federal return
    'dep_status',         # (bool) whether filer is a dependent
    'filing_status',      # (int)  1 single, 2 MFJ, 3 MFS, 4 HoH
    'blind1',             # (bool) whether primary filer is blind
    'blind2',             # (bool) whether secondary filer is blind
    'txbl_int',           # (dbl)  taxable interest (investment-income tax base)
    'div_ord',            # (dbl)  ordinary dividends
    'div_pref',           # (dbl)  qualified dividends
    'kg_lt',              # (dbl)  long-term capital gains
    'kg_st',              # (dbl)  short-term capital gains
    'other_gains',        # (dbl)  other gains (Form 4797)
    'rent',               # (dbl)  rental/royalty income
    'st_agi',             # (dbl)  state income base
    'st_exempt',          # (dbl)  state exemption allowance
    'st_txbl_inc',        # (dbl)  state taxable income
    'st_tax_pre_credit',  # (dbl)  state tax before credits
    'st_credits_nonref',  # (dbl)  nonrefundable state credits
    'st_credits_ref',     # (dbl)  refundable state credits

    # State tax law
    'st_filing.req_type',            # (int) filing requirement type (see filing.yaml)
    'st_filing.req_income_thresh',   # (dbl) fixed income filing threshold
    'st_filing.req_income_thresh_dep', # (dbl) dependent-filer threshold
    'st_filing.req_if_fed_filer',    # (int) whether federal filers must file
    'st_filing.no_tax_below_thresh', # (int) zero pre-credit tax below the threshold (VA)
    'st_programs.broad_iit',         # (int) broad individual income tax active
    'st_surtax.taxable_income_threshold', # (dbl) taxable-income surtax trigger
    'st_surtax.taxable_income_rate', # (dbl) taxable-income surtax rate
    'st_surtax.taxable_income_round', # (int) whether to round the base to dollars
    'st_surtax.per_return_amount',   # (dbl) flat per-return excise on required filers
    'st_surtax.per_return_blind_exempt', # (int) blind filers exempt from the excise
    'st_surtax.inv_income_rate',     # (dbl) net-investment-income add-on rate (MN 1%)
    'st_surtax.inv_income_thresh',   # (dbl) net-investment-income threshold (MN $1M)
    'st_surtax.kg_rate',             # (dbl) capital-gains surtax rate (MD 2%)
    'st_surtax.kg_agi_thresh',       # (dbl) federal-AGI gate for the surtax (MD $350k)
    'agi'                            # (dbl) federal AGI (surtax gate)
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      # Filing requirement income test (also gates the VA-style no-tax floor
      # below). Federally-required filers must file where req_if_fed_filer =
      # 1, OR the state income test is met, OR the unit has nonzero state
      # liability. Income test by type:
      #  1 (IL): base income above the exemption allowance
      #  2 (NY): state base above the fixed threshold
      #  3 (CO): no separate income test (federal requirement or liability)
      #  0     : no encoding; fall back to the federal filer flag
      meets_income_test = case_when(
        st_filing.req_type == 1 ~ st_agi > st_exempt,
        st_filing.req_type == 2 ~ st_agi > if_else(dep_status == 1,
                                                   st_filing.req_income_thresh_dep,
                                                   st_filing.req_income_thresh),
        TRUE                    ~ FALSE
      ),

      # No-tax floor (VA Form 760 Line 9): where flagged, income at or below
      # the filing threshold owes zero pre-credit tax outright (a cliff, not
      # an exemption). Refundable credits still pay out
      st_tax_floored = if_else(
        st_filing.no_tax_below_thresh == 1 & !meets_income_test,
        0, st_tax_pre_credit
      ),

      st_surtax_taxable_income = if_else(st_surtax.taxable_income_round == 1,
                                          round(st_txbl_inc), st_txbl_inc),
      st_taxable_income_surtax = pmax(
        0,
        st_surtax_taxable_income - st_surtax.taxable_income_threshold
      ) * st_surtax.taxable_income_rate,

      # Flat per-return excise on units required to file (ID Permanent
      # Building Fund tax, 63-3082): like the taxable-income surtax it sits
      # outside the nonrefundable credit stack (Form 40 "other taxes").
      # Legally blind filers are exempt where flagged; the public-assistance
      # exemption is unobserved (known-difference)
      st_per_return_blind = st_surtax.per_return_blind_exempt == 1 & (
        coalesce(blind1, 0) == 1 |
          (filing_status == 2 & coalesce(blind2, 0) == 1)
      ),
      st_per_return_tax = st_surtax.per_return_amount *
        ((filer == 1 & st_filing.req_if_fed_filer == 1) | meets_income_test) *
        (1 - st_per_return_blind),

      # Net-investment-income add-on tax (MN Schedule NIIT, 2024+): rate on
      # investment income above the threshold. Base proxied by interest +
      # dividends + positive net gains + positive rents (royalty/annuity
      # detail and the agricultural-land carve-out are unobserved;
      # known-difference)
      st_nii = txbl_int + div_ord + div_pref +
               pmax(0, kg_lt + kg_st + other_gains) + pmax(0, rent),
      st_inv_income_tax = st_surtax.inv_income_rate *
        pmax(0, st_nii - st_surtax.inv_income_thresh),

      # Capital-gains surtax gated on federal AGI (MD Form 502CG, 2025+:
      # 2% of net capital gain when FAGI > $350k; the 502CG retirement-
      # account/primary-residence exceptions are unobserved)
      st_kg_surtax = st_surtax.kg_rate * pmax(0, kg_lt + kg_st) *
                     (agi > st_surtax.kg_agi_thresh),
      liab_st_iit = if_else(
        st_programs.broad_iit == 1,
        pmax(0, st_tax_floored - st_credits_nonref) +
          st_taxable_income_surtax + st_per_return_tax +
          st_inv_income_tax + st_kg_surtax - st_credits_ref,
        0
      ),
      st_filer = st_programs.broad_iit == 1 & (
        (filer == 1 & st_filing.req_if_fed_filer == 1) |
          (filer == 1 & st_filing.req_type == 0) |
          meets_income_test |
          liab_st_iit != 0
      )
    ) %>%
    select(all_of(return_vars$calc_st_liab)) %>%
    return()
}

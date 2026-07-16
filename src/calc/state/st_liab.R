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
    'st_programs.broad_iit',         # (int) broad individual income tax active
    'st_surtax.taxable_income_threshold', # (dbl) taxable-income surtax trigger
    'st_surtax.taxable_income_rate', # (dbl) taxable-income surtax rate
    'st_surtax.taxable_income_round' # (int) whether to round the base to dollars
  )

  tax_unit %>%
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      st_surtax_taxable_income = if_else(st_surtax.taxable_income_round == 1,
                                          round(st_txbl_inc), st_txbl_inc),
      st_taxable_income_surtax = pmax(
        0,
        st_surtax_taxable_income - st_surtax.taxable_income_threshold
      ) * st_surtax.taxable_income_rate,
      liab_st_iit = if_else(
        st_programs.broad_iit == 1,
        pmax(0, st_tax_pre_credit - st_credits_nonref) +
          st_taxable_income_surtax - st_credits_ref,
        0
      ),

      # Filing requirement: federally-required filers must file where
      # req_if_fed_filer = 1, OR the state income test is met, OR the unit
      # has nonzero state liability. Income test by type:
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

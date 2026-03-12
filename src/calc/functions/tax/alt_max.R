#----------------------------------------------------------------------
# Function to calculate Alternative Maximum Tax for low/middle income
# (IRC Sec. 1A, Working Americans' Tax Cut Act)
#----------------------------------------------------------------------

# Set return variables for function
return_vars$calc_alt_max = c('alt_max_cap_binds', 'alt_max_liab')

calc_alt_max = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Determines whether the Sec. 1A alternative maximum tax cap binds for each
  # tax unit. Qualified individuals (MAGI < qualify_mult * exempt, not described
  # in Sec. 63(c)(6)) have their Section 1 tax capped at a flat rate applied
  # to (MAGI - cost-of-living exemption). Must be applied after calc_tax().
  #
  # MAGI = AGI + foreign income exclusions (Sec. 911/931/933) + non-included
  # SS benefits. Foreign exclusions are not tracked in the model; non-included
  # SS is approximated as gross_ss - txbl_ss.
  #
  # Returns cap metadata only; the caller applies the cap to liab components
  # via mutate to avoid column-name collisions in the pipeline.
  #
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of the following variables:
  #   - alt_max_cap_binds (bool) : whether the cap binds for this tax unit
  #   - alt_max_liab      (dbl)  : capped liability (meaningful only when
  #                                 alt_max_cap_binds is TRUE)
  #----------------------------------------------------------------------------

  req_vars = c(

    # Tax unit attributes
    'agi',            # (dbl)  Adjusted Gross Income
    'gross_ss',       # (dbl)  gross Social Security benefits
    'txbl_ss',        # (dbl)  taxable Social Security benefits (included in AGI)
    'filing_status',  # (int)  filing status (3 = MFS)
    'itemizing',      # (bool) whether filer itemizes deductions
    'liab',           # (dbl)  Section 1 income tax liability from calc_tax()

    # Tax law attributes
    'alt_max.rate',         # (dbl) cap rate (e.g. 0.255 for 25.5%)
    'alt_max.qualify_mult', # (dbl) qualification multiplier (e.g. 1.75)
    'alt_max.exempt'        # (dbl) cost-of-living exemption (filing-status mapped)
  )

  tax_unit %>%

    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>%
    mutate(

      # MAGI: AGI + non-included SS benefits (foreign exclusions not modeled)
      alt_max_magi = agi + pmax(0, gross_ss - txbl_ss),

      # Alternative maximum tax: flat rate on MAGI above exemption
      alt_max_liab = alt_max.rate * pmax(0, alt_max_magi - alt_max.exempt),

      # Sec. 63(c)(6) exclusion: MFS filers where spouse itemizes (approximated
      # as MFS & itemizing, since forced itemization is the 63(c)(6) mechanism).
      # Nonresident aliens and short-year filers are not tracked in the model.
      is_63c6 = filing_status == 3 & itemizing,

      # Cap binds if qualified AND capped liability is lower than regular
      alt_max_cap_binds = (alt_max_magi < alt_max.qualify_mult * alt_max.exempt) &
                          !is_63c6 &
                          (alt_max_liab < liab)

    ) %>%

    # Keep variables to return
    select(all_of(return_vars$calc_alt_max)) %>%
    return()
}

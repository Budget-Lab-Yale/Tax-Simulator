#-----------------------------------------------------------------
# Special state individual-tax programs outside a broad income tax
#-----------------------------------------------------------------

return_vars$calc_st_special = c(
  'liab_st_narrow_iit', 'liab_st_ltcg_excise', 'st_refund_wftc',
  'liab_st_individual_net', 'st_narrow_iit_filer',
  'st_ltcg_excise_filer', 'st_wftc_eligible', 'st_tax_filer'
)


calc_st_special = function(tax_unit, fill_missings = F) {

  #----------------------------------------------------------------------------
  # Calculates state programs that are not a broad-based income tax: NH/TN's
  # investment-income taxes, Washington's long-term-capital-gains excise tax,
  # and Washington's Working Families Tax Credit. The latter is a refundable
  # transfer, so it enters the state fiscal ledger with a negative sign.
  #
  # State-specific limitations are recorded in each source packet. In
  # particular, this implementation uses available PUF income concepts and is
  # not a substitute for an asset-level Washington capital-gains return.
  #----------------------------------------------------------------------------

  req_vars = c(
    'filer', 'dep_status', 'filing_status', 'age1', 'age2', 'blind1',
    'blind2', 'agi', 'gross_ss', 'txbl_ss', 'txbl_int', 'div_ord',
    'div_pref', 'kg_lt', 'char_cash', 'char_noncash', 'n_dep_eitc',
    'ei1', 'ei2', 'exempt_int', 'txbl_kg', 'sch_e', 'part_scorp',
    'liab_st_iit', 'st_filer',
    'st_programs.narrow_iit', 'st_programs.ltcg_excise', 'st_programs.wftc',
    'st_investment_income.interest_share',
    'st_investment_income.ordinary_div_share',
    'st_investment_income.qualified_div_share',
    'st_investment_income.exemption_amount',
    'st_investment_income.filing_threshold',
    'st_investment_income.age_exemption',
    'st_investment_income.blind_exemption', 'st_investment_income.rate',
    'st_investment_income.full_age_min_age',
    'st_investment_income.full_age_income_limit',
    'st_investment_income.age_100_full_exempt',
    'st_investment_income.blind_full_exempt',
    'st_investment_income.blind_mfj_exempt_share',
    'st_capital_gains.model_coverage_share',
    'st_capital_gains.standard_deduction',
    'st_capital_gains.charitable_threshold',
    'st_capital_gains.charitable_max_deduction',
    'st_capital_gains.base_rate', 'st_capital_gains.surtax_rate',
    'st_capital_gains.surtax_threshold',
    'st_transfers.wftc_min_age', 'st_transfers.wftc_max_age',
    'st_transfers.wftc_mfs_eligible',
    'st_transfers.wftc_inv_inc_limit',
    paste0('st_transfers.wftc_phaseout_width', 1:4),
    paste0('st_transfers.wftc_max_amount', 1:4),
    'st_transfers.wftc_min_amount',
    paste0('st_transfers.wftc_max_income_single', 1:4),
    paste0('st_transfers.wftc_max_income_joint', 1:4)
  )

  tax_unit = parse_calc_fn_input(tax_unit, req_vars, fill_missings)
  n = nrow(tax_unit)
  wftc_max_amounts = as.matrix(
    tax_unit[paste0('st_transfers.wftc_max_amount', 1:4)]
  )
  wftc_phaseout_widths = as.matrix(
    tax_unit[paste0('st_transfers.wftc_phaseout_width', 1:4)]
  )
  wftc_income_single = as.matrix(
    tax_unit[paste0('st_transfers.wftc_max_income_single', 1:4)]
  )
  wftc_income_joint = as.matrix(
    tax_unit[paste0('st_transfers.wftc_max_income_joint', 1:4)]
  )

  tax_unit %>%
    mutate(

      # NH/TN tax a limited interest/dividend base. Annual all-source income
      # is the closest available proxy for Tennessee's senior exemption test.
      st_narrow_income = txbl_int * st_investment_income.interest_share +
        div_ord * st_investment_income.ordinary_div_share +
        div_pref * st_investment_income.qualified_div_share,
      st_narrow_all_income = agi + pmax(0, gross_ss - txbl_ss),
      st_narrow_age_exemption = st_investment_income.age_exemption *
        ((age1 >= 65) + (filing_status == 2 & !is.na(age2) & age2 >= 65)),
      st_narrow_blind_exemption = st_investment_income.blind_exemption *
        (coalesce(blind1, 0) == 1) + st_investment_income.blind_exemption *
        (filing_status == 2 & coalesce(blind2, 0) == 1),
      st_narrow_exemption = st_investment_income.exemption_amount +
        st_narrow_age_exemption + st_narrow_blind_exemption,
      st_narrow_blind_mfj_share = case_when(
        filing_status != 2 ~ 0,
        coalesce(blind1, 0) + coalesce(blind2, 0) >= 2 ~ 1,
        coalesce(blind1, 0) + coalesce(blind2, 0) == 1 ~
          st_investment_income.blind_mfj_exempt_share,
        TRUE ~ 0
      ),
      # Age-based full exemptions qualify when EITHER spouse meets the age test
      # on a joint return (TN Hall Income Tax Manual: only one spouse need be
      # 65+/100+). The income limit is already the filing-status-specific
      # threshold ($68k joint) via the investment_income filing_status_mapper.
      st_narrow_full_exempt =
        ((age1 >= st_investment_income.full_age_min_age |
            (filing_status == 2 & !is.na(age2) &
               age2 >= st_investment_income.full_age_min_age)) &
           st_narrow_all_income <= st_investment_income.full_age_income_limit) |
        (st_investment_income.age_100_full_exempt == 1 &
           (age1 >= 100 |
              (filing_status == 2 & !is.na(age2) & age2 >= 100))) |
        (st_investment_income.blind_full_exempt == 1 & filing_status != 2 &
           coalesce(blind1, 0) == 1) |
        (st_investment_income.blind_full_exempt == 1 & filing_status == 2 &
           st_narrow_blind_mfj_share >= 1),
      st_narrow_taxable = pmax(0, st_narrow_income - st_narrow_exemption) *
        (!st_narrow_full_exempt) *
        (1 - st_investment_income.blind_full_exempt * st_narrow_blind_mfj_share),
      liab_st_narrow_iit = st_programs.narrow_iit *
        st_investment_income.rate * st_narrow_taxable,
      st_narrow_iit_filer = st_programs.narrow_iit == 1 &
        st_narrow_income > st_investment_income.filing_threshold &
        !st_narrow_full_exempt,

      # Washington's excise tax is modeled from net long-term capital gains,
      # with documented PUF coverage limitations in the WA source packet.
      st_cg_charitable_deduction = pmin(
        st_capital_gains.charitable_max_deduction,
        pmax(0, char_cash + char_noncash - st_capital_gains.charitable_threshold)
      ),
      st_ltcg_excise_base = pmax(
        0,
        kg_lt * st_capital_gains.model_coverage_share -
          st_capital_gains.standard_deduction - st_cg_charitable_deduction
      ),
      liab_st_ltcg_excise = st_programs.ltcg_excise * (
        st_ltcg_excise_base * st_capital_gains.base_rate +
          pmax(0, st_ltcg_excise_base - st_capital_gains.surtax_threshold) *
          st_capital_gains.surtax_rate
      ),
      st_ltcg_excise_filer = st_programs.ltcg_excise == 1 &
        liab_st_ltcg_excise != 0,

      # WFTC uses the federal EITC's broad eligibility concepts, but not the
      # federal credit amount: Washington permits ITIN filers and, from 2023,
      # MFS filers. Residence and SSN/ITIN status are unobserved.
      st_wftc_child_bin = pmin(3, pmax(0, n_dep_eitc)) + 1,
      st_wftc_earned_income = pmax(0, ei1) +
        if_else(filing_status == 2, pmax(0, ei2), 0),
      st_wftc_investment_income = txbl_int + exempt_int + div_ord + div_pref +
        pmax(0, txbl_kg) + pmax(0, sch_e - part_scorp),
      st_wftc_age_ok = n_dep_eitc > 0 |
        (age1 >= st_transfers.wftc_min_age & age1 <= st_transfers.wftc_max_age) |
        (filing_status == 2 & !is.na(age2) &
           age2 >= st_transfers.wftc_min_age & age2 <= st_transfers.wftc_max_age),
      st_wftc_mfs_ok = filing_status != 3 | st_transfers.wftc_mfs_eligible == 1,
      st_wftc_max_amount = wftc_max_amounts[
        cbind(seq_len(n), st_wftc_child_bin)
      ],
      st_wftc_phaseout_width = wftc_phaseout_widths[
        cbind(seq_len(n), st_wftc_child_bin)
      ],
      st_wftc_income_limit = if_else(
        filing_status == 2,
        wftc_income_joint[
          cbind(seq_len(n), st_wftc_child_bin)
        ],
        wftc_income_single[
          cbind(seq_len(n), st_wftc_child_bin)
        ]
      ),
      st_wftc_eligible = st_programs.wftc == 1 & filer == 1 & dep_status != 1 &
        st_wftc_age_ok & st_wftc_mfs_ok & st_wftc_earned_income > 0 &
        pmax(agi, st_wftc_earned_income) < st_wftc_income_limit &
        st_wftc_investment_income <= st_transfers.wftc_inv_inc_limit,
      st_wftc_raw = st_wftc_max_amount - pmax(
        0,
        pmax(agi, st_wftc_earned_income) -
          (st_wftc_income_limit - st_wftc_phaseout_width)
      ) * st_wftc_max_amount / st_wftc_phaseout_width,
      st_refund_wftc = if_else(
        st_wftc_eligible,
        pmax(st_transfers.wftc_min_amount, round(st_wftc_raw)),
        0
      ),

      liab_st_individual_net = liab_st_iit + liab_st_narrow_iit +
        liab_st_ltcg_excise - st_refund_wftc,
      st_tax_filer = st_filer | st_narrow_iit_filer | st_ltcg_excise_filer |
        st_wftc_eligible
    ) %>%
    select(all_of(return_vars$calc_st_special)) %>%
    return()
}

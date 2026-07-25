#----------------------------------------------------------------------------
# corp_alloc.R
#
# Shared, stock-based corporate-incidence allocator for the distribution
# post-processing (distribution.R delta table and distribution_etrs.R levels
# table). Single source of truth for:
#   - which capital STOCKS bear the corporate tax (the Harberger base: the tax
#     lowers the net return by Dr, so the burden is Dr*K, proportional to the
#     STOCK; agencies use capital INCOME only because tax data lack a wealth
#     measure -- we have one, so we use it),
#   - the normal/supernormal split by provision type (sigma_N),
#   - the foreign-borne haircut on the capital legs,
#   - the owner-occupied-housing land/structure split.
#
# Reuses the src/sim/corp/ constants (CORP_ASSET_EXPOSURE, sigma_N via
# corp_env_knobs()) and the estate/wealth balance-sheet column vectors
# (ESTATE_ASSET_COLS / ESTATE_DEBT_COLS, src/calc/functions/tax/estate.R) so
# the net-capital base stays in lockstep with what estate + wealth call
# "net worth". No phase-in: the split is fixed for both level and delta.
#----------------------------------------------------------------------------


# Foreign-borne share of the CAPITAL leg of corporate tax changes, excluded
# from distribution tables rather than allocated to US households (the JCT
# convention: JCX-14-13 excludes 20.6% as foreign-borne; CBO/OTA/TPC instead
# allocate 100% to US households). Value = foreign share of total US corporate
# equity, FDI + portfolio, publicly traded + closely held: 42% at end-2022
# (Rosenthal & Mucciolo, Tax Notes Federal 183, 2024-04-01), rounded down --
# note the denominator includes S-corp equity that foreigners are statutorily
# barred from holding, biasing the C-corp share DOWN. The labor leg is NOT
# haircut: wage incidence lands on US workers regardless of who owns the
# equity. Conceptually distinct from CORP_THETA_RES = 0.40 (src/sim/corp/:
# foreign + nonprofit + DB residual, conservation diagnostic only) -- equal by
# coincidence; do not merge. Tables no longer sum to the corporate revenue
# line by construction (the remainder is foreign-borne). Moved here from
# distribution.R so the delta and levels tables share one definition.
DIST_CORP_FOREIGN_SHARE = 0.40


# Owner-occupied-housing structure share. Under Harberger, capital migrates
# across sectors until after-tax returns equalize; the mobile-capital margin is
# reproducible STRUCTURES, not residential LAND (a fixed factor whose incidence
# runs through capitalization/asset prices, not through capital flowing "into"
# land). So owner-occupied housing enters the all-capital base only to the
# extent of its structure component: structure_share * net home equity.
# Residential land (1 - structure_share) is excluded. 0.70 is a national-average
# simplification (BEA current-cost net stock of residential structures is
# roughly two-thirds to low-70s of the Fed's owner-occupied real estate
# aggregate; the Fed measure is broader, including vacant land and mobile
# homes). PLACEHOLDER pending a per-record or geography-varying split.
# Env override DIST_HOUSING_STRUCTURE_SHARE sweeps the 0.60 / 1.00 sensitivity
# cases (1.00 = no land split, full net home equity).
DIST_HOUSING_STRUCTURE_SHARE_DEFAULT = 0.70


# Owner-occupied residential real estate on the asset side, and the mortgages
# secured against it on the debt side. The structure share applies to net home
# equity = (homes) - (mortgages); value.re_fund is deliberately NOT here (it is
# investment/pooled real estate, reproducible mobile capital, kept at full
# weight). Derived from the estate/wealth balance-sheet vectors so there is one
# definition of the household balance sheet.
DIST_HOUSING_ASSET_COLS = c('value.primary_home', 'value.other_home')
DIST_HOUSING_DEBT_COLS  = c('value.primary_mortgage', 'value.other_mortgage')



dist_housing_structure_share = function() {

  #----------------------------------------------------------------------------
  # Structure share of owner-occupied housing, with env override for the
  # land-split sensitivity sweep (0.60 conservative / 0.70 central / 1.00 no
  # split). Mirrors corp_env_knobs(): the ONLY runtime knob; everything else is
  # a code edit.
  #
  # Returns: structure share in [0, 1] (dbl).
  #----------------------------------------------------------------------------

  v = Sys.getenv('DIST_HOUSING_STRUCTURE_SHARE', unset = NA)
  if (is.na(v) || !nzchar(v)) return(DIST_HOUSING_STRUCTURE_SHARE_DEFAULT)
  x = suppressWarnings(as.numeric(v))
  if (!is.finite(x) || x < 0 || x > 1) {
    stop('corp_alloc: env override DIST_HOUSING_STRUCTURE_SHARE = "', v,
         '" is not a number in [0, 1].')
  }
  message(sprintf('corp_alloc: env override DIST_HOUSING_STRUCTURE_SHARE = %s (default %s)',
                  x, DIST_HOUSING_STRUCTURE_SHARE_DEFAULT))
  x
}



read_corp_alloc_stock_keys = function(baseline_id, yr) {

  #----------------------------------------------------------------------------
  # Reads the per-record capital-stock allocation bases from the BASELINE leg's
  # raw Tax-Data (scenario-invariant balance sheet; a static ETR-levels table
  # wants the un-marked-down baseline stocks). Rejoined by id per year, mirroring
  # the per-year Estate-interface reads in distribution.R. Also returns the
  # accrual/retirement pieces the Haig-Simons income definition needs, so the
  # single Tax-Data read serves both the allocator and the income defs.
  #
  # Parameters:
  #   - baseline_id (str) : baseline scenario ID (its Tax-Data interface path is
  #                         used even under a dep.Tax-Data.ID override, so the
  #                         base is a fixed reference balance sheet)
  #   - yr          (int) : year of the tax_units_<yr>.csv file
  #
  # Returns: tibble keyed by id with
  #   - corp_equity     : equity-exposed stock (supernormal base), CORP_ASSET_
  #                       EXPOSURE-weighted
  #   - net_capital     : net-equity all-capital base (normal-leg base) -- assets
  #                       less debts, owner-occ housing entered as
  #                       structure_share * net home equity, floored at 0
  #   - net_worth_stock : gross assets less all debts, floored at 0 (uniform_
  #                       networth convention base)
  #   - accruals_sum    : sum of the 7 accruals.* columns (HS income)
  #   - other_gains, txbl_ira_dist, gross_pens_dist : HS realized-gain removal /
  #                       DC-distribution double-count fix pieces
  #   - dc_share        : value.dc / (value.dc + value.db), zero-guarded (HS
  #                       pension-distribution split; same as corp_incidence P7)
  #----------------------------------------------------------------------------

  accrual_cols = c('accruals.equities', 'accruals.pass_throughs',
                   'accruals.primary_home', 'accruals.other_home',
                   'accruals.re_fund', 'accruals.dc', 'accruals.trusts')

  hs_cols   = c('other_gains', 'txbl_ira_dist', 'gross_pens_dist')
  stock_cols = union(ESTATE_ASSET_COLS, ESTATE_DEBT_COLS)
  read_cols = c('id', hs_cols, accrual_cols, stock_cols)

  path = interface_root('Tax-Data', baseline_id) %>%
    file.path(paste0('tax_units_', yr, '.csv'))
  if (!file.exists(path)) {
    stop('corp_alloc: baseline Tax-Data file not found for ', yr, ' at ', path)
  }

  td = fread(path, select = read_cols) %>% tibble()

  structure_share = dist_housing_structure_share()

  # Sub-vectors: the 12 non-housing assets and 4 non-mortgage debts net normally;
  # the 2 housing assets and 2 mortgages net inside the structure term
  nonhousing_assets = setdiff(ESTATE_ASSET_COLS, DIST_HOUSING_ASSET_COLS)
  nonmort_debts     = setdiff(ESTATE_DEBT_COLS,  DIST_HOUSING_DEBT_COLS)

  # Zero-fill any missing balance-sheet cell before summing (a genuine NA in an
  # allocation base would poison group sums; Tax-Data carries these dense, but
  # be defensive). rowSums(across(...)) mirrors run.R:546 / src/sim/corp/apply.R (was src/sim/corp/:1151).
  z0 = function(x) replace_na(x, 0)

  td %>%
    mutate(

      # Supernormal base: equity-exposed stock (CORP_ASSET_EXPOSURE-weighted)
      corp_equity = z0(value.equities) * unname(CORP_ASSET_EXPOSURE['value.equities']) +
                    z0(value.dc)       * unname(CORP_ASSET_EXPOSURE['value.dc'])       +
                    z0(value.trusts)   * unname(CORP_ASSET_EXPOSURE['value.trusts'])   +
                    z0(value.re_fund)  * unname(CORP_ASSET_EXPOSURE['value.re_fund']),

      # Net home equity, then its structure component (residential land excluded)
      net_home_equity   = (z0(value.primary_home) + z0(value.other_home)) -
                          (z0(value.primary_mortgage) + z0(value.other_mortgage)),
      housing_structure = structure_share * net_home_equity,

      # Net-capital (normal-leg) base: structure equity + non-housing net equity
      # (mortgages already netted inside the structure term), floored at 0 (a
      # proportional base cannot go negative)
      net_capital = pmax(0,
        housing_structure +
        rowSums(across(all_of(nonhousing_assets), z0)) -
        rowSums(across(all_of(nonmort_debts),     z0))
      ),

      # Uniform net worth: gross assets less ALL debts, floored at 0
      net_worth_stock = pmax(0,
        rowSums(across(all_of(ESTATE_ASSET_COLS), z0)) -
        rowSums(across(all_of(ESTATE_DEBT_COLS),  z0))
      ),

      # Haig-Simons pieces
      accruals_sum = rowSums(across(all_of(accrual_cols), z0)),
      dc_share     = if_else(z0(value.dc) + z0(value.db) > 0,
                             z0(value.dc) / (z0(value.dc) + z0(value.db)),
                             0),
      other_gains     = z0(other_gains),
      txbl_ira_dist   = z0(txbl_ira_dist),
      gross_pens_dist = z0(gross_pens_dist)
    ) %>%
    select(id, corp_equity, net_capital, net_worth_stock,
           accruals_sum, dc_share, other_gains, txbl_ira_dist, gross_pens_dist)
}



# Broad-income component set, ported VERBATIM from the Tax-Data build
# (Tax-Data/src/imputations/helpers.R: broad_income_components) -- the exact
# denominator dina_other_taxes_rate was divided by. Keep in lockstep with that
# source: the rate is (state income + sales/excise + property tax) / broad
# income, so recovering DOLLARS requires multiplying by THIS concept, not AGI /
# expanded / HS. Sign convention (helpers.R:189): losses subtracted, §179
# subtracted, no floor at zero.
BROAD_INCOME_COMPONENTS = c(
  'wages',
  'sole_prop', 'farm',
  'scorp_active', 'scorp_active_loss', 'scorp_179',
  'scorp_passive', 'scorp_passive_loss',
  'part_active', 'part_active_loss', 'part_179',
  'part_passive', 'part_passive_loss',
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
  'kg_lt', 'kg_st',
  'gross_ss', 'gross_pens_dist',
  'ui',
  'rent', 'rent_loss', 'estate', 'estate_loss'
)



read_other_taxes_base = function(baseline_id, yr) {

  #----------------------------------------------------------------------------
  # Reads the imputed combined state+local+federal-excise effective tax RATE
  # (dina_other_taxes_rate, Tax-Data vintage >= 2026070814) from the BASELINE
  # leg's raw Tax-Data and converts it to per-record DOLLARS by multiplying by
  # the SAME base it was divided by: compute_broad_income() (DINA fiscal income
  # + gross SS + UI), reconstructed here from BROAD_INCOME_COMPONENTS.
  #
  # The rate is scale-free and stored held-constant (grow_with = NA); the
  # Tax-Data author's design intent (Tax-Data/src/imputations/state_local_tax.R
  # lines 20-30, 149-152) is that Tax-Simulator multiplies it by the same broad-
  # income concept, i.e. "ratio-to-income aging" -- the constant rate times the
  # year-aged base. Rates can exceed 1 (state/local + excise legitimately exceed
  # tiny incomes at the bottom); floored/winsorized upstream. Reading the RAW
  # baseline Tax-Data (not the sim detail) is required because the granular
  # components are not in detail_vars, and it takes estate/estate_loss with
  # their Tax-Data meaning (estate/trust INCOME, not the estate TAX that
  # Tax-Simulator's `estate` column carries post-do_taxes).
  #
  # Used ONLY by the ETR-levels table (distribution_etrs.R); the delta table
  # does not read it, so the vintage requirement is scoped to the ETR path.
  #
  # Parameters:
  #   - baseline_id (str) : baseline scenario ID (fixed reference Tax-Data path,
  #                         mirrors read_corp_alloc_stock_keys)
  #   - yr          (int) : year of tax_units_<yr>.csv
  #
  # Returns: tibble keyed by id with
  #   - dina_other_taxes_rate : the imputed rate
  #   - broad_income          : reconstructed compute_broad_income ($)
  #   - other_tax             : dina_other_taxes_rate * pmax(broad_income, 0) ($)
  #----------------------------------------------------------------------------

  path = interface_root('Tax-Data', baseline_id) %>%
    file.path(paste0('tax_units_', yr, '.csv'))
  if (!file.exists(path)) {
    stop('read_other_taxes_base: baseline Tax-Data file not found for ', yr,
         ' at ', path)
  }

  # Graceful degradation: a baseline vintage predating the imputation (< 2026070814)
  # has no rate column. Return NULL and let the caller OMIT the `other` tier rather
  # than crash -- many historical runscripts pin such vintages. (Mirrors the
  # estate-column "predates" handling in distribution.R.) A vintage that DOES carry
  # the rate but is missing a broad-income component is a genuinely broken build
  # and still hard-stops below.
  hdr = names(fread(path, nrows = 0))
  if (!('dina_other_taxes_rate' %in% hdr)) {
    warning('read_other_taxes_base: `dina_other_taxes_rate` absent from baseline ',
            'Tax-Data (', path, '); the state+local+excise `other` ETR tier is ',
            'omitted. Requires Tax-Data vintage >= 2026070814.')
    return(NULL)
  }
  miss = setdiff(BROAD_INCOME_COMPONENTS, hdr)
  if (length(miss) > 0) {
    stop('read_other_taxes_base: broad-income components missing from ',
         'baseline Tax-Data (', path, '): ', paste(miss, collapse = ', '))
  }

  z0 = function(x) replace_na(x, 0)

  fread(path, select = c('id', 'dina_other_taxes_rate', BROAD_INCOME_COMPONENTS)) %>%
    tibble() %>%
    mutate(
      # compute_broad_income(), ported verbatim (helpers.R:223-235)
      broad_income =
        z0(wages) +
        z0(sole_prop) + z0(farm) +
        z0(scorp_active)  - z0(scorp_active_loss)  - z0(scorp_179) +
        z0(scorp_passive) - z0(scorp_passive_loss) +
        z0(part_active)   - z0(part_active_loss)   - z0(part_179) +
        z0(part_passive)  - z0(part_passive_loss) +
        z0(txbl_int) + z0(exempt_int) + z0(div_ord) + z0(div_pref) +
        z0(kg_lt) + z0(kg_st) +
        z0(gross_ss) + z0(gross_pens_dist) +
        z0(ui) +
        z0(rent) - z0(rent_loss) + z0(estate) - z0(estate_loss),
      dina_other_taxes_rate = z0(dina_other_taxes_rate),

      # Floor the base at 0 for the dollar conversion: the rate was trained on
      # income>0 units (floored/winsorized upstream), so a negative base
      # (business losses) would otherwise flip the sign of a positive burden.
      # Rare, small; no effect on the group gradient.
      other_tax = dina_other_taxes_rate * pmax(broad_income, 0)
    ) %>%
    select(id, dina_other_taxes_rate, broad_income, other_tax)
}



allocate_corp_dollars = function(amount_billion, sigma_n, weight, labor,
                                 base_supernormal, base_normal) {

  #----------------------------------------------------------------------------
  # Allocates a national corporate-tax dollar amount to records by capital
  # STOCK, split into a supernormal (rent) leg and a normal-return leg.
  #
  #   supernormal = (1 - sigma_n) * A * (1 - foreign)      proportional to the
  #                 supernormal base (corp-equity stock in the headline
  #                 convention; the capital-income / net-worth base in the
  #                 reconciliation conventions)
  #   normal      =      sigma_n  * A, split 50/50 labor / capital:
  #     labor leg   = 0.5 * sigma_n * A                    proportional to labor
  #                   (NO foreign haircut -- wage incidence is domestic)
  #     capital leg = 0.5 * sigma_n * A * (1 - foreign)    proportional to the
  #                   normal-capital base
  #
  # The foreign-borne portion of the capital legs is dropped (not reallocated),
  # so Sum(allocated * weight) = A * [ (1-sigma_n)*(1-foreign) + sigma_n*0.5 +
  # sigma_n*0.5*(1-foreign) ], NOT A. Denominators are population aggregates
  # Sum(base * weight); pass the FULL (ungrouped) record frame.
  #
  # For sigma_n = 1 (cost-recovery / pure normal) the supernormal leg vanishes.
  # For amount 0 the result is an all-zero vector (no division).
  #
  # Parameters:
  #   - amount_billion   (dbl) : national corporate $ to allocate, in $B
  #   - sigma_n          (dbl) : normal-return share in [0, 1]
  #   - weight           (dbl[]): record weights
  #   - labor            (dbl[]): per-record labor base (income)
  #   - base_supernormal (dbl[]): per-record supernormal base
  #   - base_normal      (dbl[]): per-record normal-capital base
  #
  # Returns: per-record allocated dollars (dbl[]).
  #----------------------------------------------------------------------------

  n = length(weight)
  if (length(amount_billion) != 1) amount_billion = amount_billion[1]
  if (is.na(amount_billion) || amount_billion == 0) return(rep(0, n))

  A       = amount_billion * 1e9
  foreign = DIST_CORP_FOREIGN_SHARE

  # A weighted-share leg: amount * base / sum(base * weight), guarding an empty
  # (all-zero) base so the leg contributes nothing rather than NaN
  leg = function(amount, base) {
    denom = sum(base * weight)
    if (!is.finite(denom) || denom <= 0) return(rep(0, n))
    amount * base / denom
  }

  supernormal = leg((1 - sigma_n) * A * (1 - foreign), base_supernormal)
  normal_lab  = leg(      sigma_n  * A * 0.5,           labor)
  normal_cap  = leg(      sigma_n  * A * 0.5 * (1 - foreign), base_normal)

  supernormal + normal_lab + normal_cap
}

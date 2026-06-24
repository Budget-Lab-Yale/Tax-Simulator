#-------------------------------------------------------------------------------
# wealth_dynamics.R
#
# The wealth bathtub: a MECHANICAL, conventional-side saving-financing channel.
# A share s = 1 - MPC of the net above-baseline DURING-LIFE tax (income +
# payroll - deemed + wealth) is financed out of wealth rather than consumption.
# That deficit compounds over time and drains into the estate (and capital-
# income) base at death, so the model can quantify interactions like
# capital-gains-during-life <-> estate tax and wealth-tax <-> capital-income tax.
#
# This is NOT a behavior module: there is no do_wealth_dynamics() hook. The
# applier (wealth_dyn_apply_to_records) is invoked DIRECTLY as a built-in step
# at the head of the final conventional pass (src/sim/run.R), before the
# behavior modules and before do_taxes. `s > 0` (a runscript COLUMN) activates
# the channel; absent/0 leaves it dormant (byte-identical output).
#
# Longitudinal dynamics live at the COHORT level (synthetic cohorts), exactly
# like kg_dynamics: cells = (age x within-age net-worth percentile), a per-year
# forward recurrence over those cells (run_wealth_bathtub_pass), per-year .rds
# state, and a per-record applier. Generic cohort primitives live in
# src/sim/cohort_bathtub.R.
#
# See the plan at .claude/plans/purrfect-weaving-toucan.md and the decision log
# at other/wealth_dynamics/plan_review_decisions.md (D1-D33).
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Constants and provenance
#-------------------------------------------------------------------------------

# Cohort age grid. Single 80+ topcode, matching kg and the Tax-Data age1
# topcode. Declared independently of kg's constants (do NOT assume they match).
WEALTH_DYN_AGE_MIN = 18L
WEALTH_DYN_AGE_MAX = 80L

WEALTH_DYN_SPEC_VERSION = 1L

# Proportional bump used to MEASURE the capital-income bundle MTR through the
# calculator (the directional derivative of tax along the haircut direction).
WEALTH_DYN_MTR_BUMP = 0.01

# |F| floor as a fraction of gross assets: records whose taxable capital income
# is negligible relative to their balance sheet get mtr_cap_bundle = 0 (no
# meaningful capital-income margin to measure). Scales across the $10k-$10B
# range (D7).
WEALTH_DYN_F_FLOOR = 1e-4

# Upper bound on the cell capital-income yield y = ΣF/Σgross in the kernel
# feedback. y is meant to be a recurring taxable YIELD (~r), but realized gains
# (kg_lt) are a one-time stock-depleting flow that can dwarf gross assets in
# sparse low-net-worth cells, spiking y >> 1. Such cells carry negligible
# deficit (∝ their tiny NW), so capping y here keeps the headline (high-NW
# cells, y ~ r) exact while preventing an unphysical feedback. PLACEHOLDER: a
# portfolio-resolved recurring yield would not need this.
WEALTH_DYN_Y_MAX = 1.0

# Physical floor for the kernel growth factor G. Feedback can only damp growth;
# G must stay positive (a cell's deficit cannot shrink to <=0 in one year from
# tax feedback alone). Clamping binds only in the sparse low-NW lumpy-realization
# cells described above.
WEALTH_DYN_G_FLOOR = 1e-6

# Numerical floor for denominators (cell weighted-NW sums, gross assets).
WEALTH_DYN_EPS = 1e-8

# WEALTH_CAP_FLOWS -- the SINGLE SOURCE OF TRUTH for which capital-income flows
# the channel touches and at what weight (plan D6). The MTR bump and the applier
# haircut MUST scale exactly these columns by these weights, or the measured
# yield/MTR is inconsistent with the erosion.
#
# Pure-capital (weight 1.0): interest, dividends, and the four taxed-gain
# classes, plus the rental and estate/trust NET pairs. The loss leg of each pair
# is scaled together with its gain leg so the NET scales proportionally and a
# haircut unambiguously shrinks taxable capital income. (rent/rent_loss and
# estate/estate_loss are raw Tax-Data columns; derive_vars() recombines them
# into sch_e inside every do_taxes() call, so scaling the raw legs propagates.)
WEALTH_CAP_FLOWS_PURE = c(
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
  'kg_st', 'kg_lt', 'kg_1250', 'kg_collect',
  'rent', 'rent_loss', 'estate', 'estate_loss'
)

# kg_lt's basis is scaled with kg_lt (same pure-capital factor) so the TAXABLE
# gain scales proportionally. Scale kg_lt_basis, NOT the derived kg_lt_infl_adj
# (which derive/calc recompute -- scaling it no-ops and can drive kg_lt_adj
# negative under indexed-basis law).
WEALTH_CAP_FLOWS_PURE_BASIS = c('kg_lt_basis')

# Pass-through capital slice (weight 0.2): the economy.R:287-289 raw
# disaggregated business-income list, under the model's 20%-capital/80%-labor
# split. derive_vars() recombines these into part/scorp/pt/sch_e inside
# do_taxes(), so scaling the raw legs propagates. The loss legs ride along with
# their income legs (same 0.2 factor) so the net pass-through scales.
WEALTH_CAP_FLOWS_PT = c(
  'sole_prop', 'part_active', 'part_passive', 'part_active_loss',
  'part_passive_loss', 'part_179', 'scorp_active', 'scorp_passive',
  'scorp_active_loss', 'scorp_passive_loss', 'scorp_179', 'farm'
)
WEALTH_CAP_FLOWS_PT_WEIGHT = 0.2

# SECA/NIIT earner-split companions of the pass-through aggregates. Co-scaled
# with WEALTH_CAP_FLOWS_PT (same 0.2 factor) so the NIIT/SECA active-vs-passive
# frame stays consistent under the bump/haircut (D7). Caveat (F18/D23): with
# pr = F in the bundle MTR, the SECA response of this slice is omitted (<1% at
# the top tail), accepted as O(curvature).
WEALTH_CAP_FLOWS_SE_COMPANIONS = c(
  'part_se1', 'part_se2', 'sole_prop1', 'sole_prop2', 'farm1', 'farm2'
)

# Staleness stamp, mirroring KG_DYN_CALIB_PROVENANCE. Records the conditions the
# channel's defaults were pinned under; wealth_dyn_check_provenance() compares
# the live Macro-Projections vintage (r_total source) and the operational params
# and warns (WEALTH_STRICT_CALIB=1 -> stops). The default Macro vintage is
# pinned, so the guard mainly catches per-runscript dep.Macro-Projections.vintage
# overrides.
WEALTH_DYN_PROVENANCE = list(
  date           = '2026-06-24',
  spec_version   = 1L,
  macro_vintage  = '2026022522',  # vintage r_total (nominal GDP/capita) is derived from
  fmax           = 0.9,           # expected applier clamp
  n_pctiles      = 100L,          # expected within-age bin count
  transition     = 'identity'     # M placeholder (full persistence)
)



#-------------------------------------------------------------------------------
# Scenario gate and params
#-------------------------------------------------------------------------------

scenario_uses_wealth_dynamics = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The channel is keyed off the runscript `s` column (saving share), NOT the
  # behavior column. s > 0 activates it; absent/0 leaves it dormant.
  #----------------------------------------------------------------------------

  s = scenario_info$s
  isTRUE(is.numeric(s) && length(s) == 1 && !is.na(s) && s > 0)
}



wealth_dyn_params_path = function() {
  file.path('./config/wealth/wealth_financing_params.yaml')
}

wealth_dyn_load_params = function() {

  #----------------------------------------------------------------------------
  # Loads the operational params (NOT reform tax law; never scenario-overridden).
  # Returns a list with n_pctiles, fmax, r_total (source + additive_delta), and
  # transition_matrix_file.
  #----------------------------------------------------------------------------

  p = read_yaml(wealth_dyn_params_path())
  # Coerce/default the fields the code relies on.
  p$n_pctiles = as.integer(p$n_pctiles %||% 100L)
  p$fmax      = as.numeric(p$fmax %||% 0.9)
  if (is.null(p$r_total)) p$r_total = list()
  p$r_total$additive_delta = as.numeric(p$r_total$additive_delta %||% 0)
  p
}



#-------------------------------------------------------------------------------
# Refusal gate and provenance (mirror kg_dyn_check_run_compat /
# kg_dyn_check_calibration_provenance)
#-------------------------------------------------------------------------------

wealth_dyn_check_provenance = function(scenario_info, params = NULL) {

  #----------------------------------------------------------------------------
  # Warns (loudly) when the live configuration no longer matches the conditions
  # the channel's defaults were pinned under -- chiefly a Macro-Projections
  # vintage override (r_total is derived from it) or a hand-edited param file.
  # Warning by default; WEALTH_STRICT_CALIB=1 hard-stops (production scoring).
  # Returns TRUE iff everything matches.
  #----------------------------------------------------------------------------

  if (is.null(params)) params = wealth_dyn_load_params()
  p    = WEALTH_DYN_PROVENANCE
  msgs = character(0)
  num_mismatch = function(live, cal) !isTRUE(all.equal(as.numeric(live),
                                                       as.numeric(cal)))

  if (!identical(as.integer(WEALTH_DYN_SPEC_VERSION), as.integer(p$spec_version)))
    msgs = c(msgs, sprintf('spec_version: live %d vs pinned %d (channel logic changed?)',
                           WEALTH_DYN_SPEC_VERSION, p$spec_version))
  if (num_mismatch(params$fmax, p$fmax))
    msgs = c(msgs, sprintf('fmax: live %s vs pinned %s', params$fmax, p$fmax))
  if (num_mismatch(params$n_pctiles, p$n_pctiles))
    msgs = c(msgs, sprintf('n_pctiles: live %s vs pinned %s',
                           params$n_pctiles, p$n_pctiles))

  macro = scenario_info$interface_paths$`Macro-Projections`
  if (!is.null(macro) && !grepl(p$macro_vintage, macro, fixed = TRUE))
    msgs = c(msgs, sprintf(paste0("Macro-Projections vintage: run uses '%s', ",
                                  "r_total pinned on '%s'"),
                           macro, p$macro_vintage))

  if (length(msgs) > 0) {
    banner = paste0(
      '\n=======================================================================\n',
      'wealth_dynamics PROVENANCE STALE -- conventional estimates may be off-target\n',
      'The wealth bathtub defaults were pinned under conditions that no longer\n',
      'match this run:\n  - ', paste(msgs, collapse = '\n  - '), '\n',
      'Fix: re-pin WEALTH_DYN_PROVENANCE / wealth_financing_params.yaml, or (for a\n',
      'deliberate sensitivity test) ignore. Set WEALTH_STRICT_CALIB=1 to hard-stop.\n',
      '=======================================================================')
    if (identical(Sys.getenv('WEALTH_STRICT_CALIB'), '1')) stop(banner)
    message(banner)
    warning('wealth_dynamics provenance is stale (see banner); estimates may ',
            'be off-target.')
    return(invisible(FALSE))
  }
  invisible(TRUE)
}



wealth_dyn_check_run_compat = function(scenario_info, vat_price_offset,
                                       excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Shared preconditions for the wealth bathtub pre-pass and the applier. The
  # pre-pass forms cell state in raw wealth dollars (net_worth, economic_gross)
  # while ΔT⁰ is in adjusted tax dollars; mixing raw-dollar wealth with VAT/
  # excess-growth-adjusted bases would put the channels in inconsistent units.
  # Full sample is required because the 63x100 cells are sparse (sparser than
  # kg's 63 age cells), so subsample noise is worst in the top cells. Mirrors
  # kg_dyn_check_run_compat (minus the kg_lt mtr_vars requirement). Stops on
  # violation; ends with the provenance check.
  #
  # Returns: invisibly TRUE.
  #----------------------------------------------------------------------------

  if (!isTRUE(all.equal(globals$pct_sample, 1))) {
    stop('wealth_dynamics (s > 0) requires pct_sample = 1 (full sample). The ',
         '63x100 (age x within-age percentile) cells are too sparse at smaller ',
         'samples; sparse-cell noise would masquerade as policy response. ',
         'Re-run with pct_sample = 1.')
  }

  vat_active = !is.null(vat_price_offset) &&
               'cpi_factor' %in% colnames(vat_price_offset) &&
               any(abs(vat_price_offset$cpi_factor - 1) > 1e-10, na.rm = TRUE)
  if (vat_active) {
    stop('wealth_dynamics is not currently compatible with VAT scenarios: raw-',
         'dollar wealth cell state would mix with VAT-scaled bases. Run without ',
         'a VAT.')
  }

  growth_active = isTRUE(scenario_info$excess_growth != 0) &&
                  is.finite(scenario_info$excess_growth_start_year)
  if (growth_active) {
    stop('wealth_dynamics is not currently compatible with excess-growth ',
         'scenarios (excess_growth = ', scenario_info$excess_growth, '). Raw ',
         'wealth cell state would not match growth-adjusted bases. Disable ',
         'excess growth on this scenario.')
  }

  wealth_dyn_check_provenance(scenario_info)
  invisible(TRUE)
}



#-------------------------------------------------------------------------------
# Cohort key and capital-income bundle helpers
#-------------------------------------------------------------------------------

wealth_dyn_age_cohort = function(tax_units) {

  #----------------------------------------------------------------------------
  # The (age x percentile) cell's age key. Joint records use max(age1, age2)
  # (the both-die event the couple's estate_m already carries), applied BEFORE
  # the 80+ topcode -- identical to kg_dynamics.R:404-407 and distribution.R:173
  # (plan D16). The pre-pass and the applier MUST compute this identically.
  #----------------------------------------------------------------------------

  a = if_else(tax_units$filing_status == 2,
              pmax(tax_units$age1, tax_units$age2, na.rm = TRUE),
              tax_units$age1)
  pmax(WEALTH_DYN_AGE_MIN, pmin(WEALTH_DYN_AGE_MAX, a))
}



wealth_dyn_safe_col = function(df, col) {

  # Returns df[[col]] with NAs -> 0, or a 0 vector if the column is absent.
  if (col %in% names(df)) replace_na(df[[col]], 0) else rep(0, nrow(df))
}



wealth_dyn_capital_total = function(df) {

  #----------------------------------------------------------------------------
  # F = the taxable capital-income content of the WEALTH_CAP_FLOWS bundle, in
  # native dollars: F = sum_c w_c * flow_c. Pure-capital flows enter at weight 1
  # (rental and estate/trust as NET pairs); the pass-through slice enters at
  # weight 0.2 (capital share of business income), as the signed net. Robust to
  # missing columns (treated as 0).
  #----------------------------------------------------------------------------

  g = function(col) wealth_dyn_safe_col(df, col)

  f_pure = g('txbl_int') + g('exempt_int') + g('div_ord') + g('div_pref') +
           g('kg_st') + g('kg_lt') + g('kg_1250') + g('kg_collect') +
           (g('rent')   - g('rent_loss')) +
           (g('estate') - g('estate_loss'))

  f_pt_net = g('sole_prop') +
             (g('part_active') + g('part_passive') -
              g('part_active_loss') - g('part_passive_loss') - g('part_179')) +
             (g('scorp_active') + g('scorp_passive') -
              g('scorp_active_loss') - g('scorp_passive_loss') - g('scorp_179')) +
             g('farm')

  f_pure + WEALTH_CAP_FLOWS_PT_WEIGHT * f_pt_net
}



wealth_dyn_economic_gross = function(df) {

  # Gross assets = sum of the 14 value.* asset columns (= ESTATE_ASSET_COLS),
  # computed in-memory. Same object calc_estate() forms inline.
  rowSums(cols_matrix(df, ESTATE_ASSET_COLS))
}

cols_matrix = function(df, cols) {
  # A numeric matrix of the requested columns with NAs -> 0 and missing columns
  # dropped (suitable for rowSums). Always returns a matrix with nrow(df) rows.
  present = intersect(cols, names(df))
  if (length(present) == 0) return(matrix(0, nrow(df), 1))
  m = as.matrix(df[present])
  m[is.na(m)] = 0
  m
}



calc_cap_bundle_mtr = function(tax_units, actual_liab_iit, baseline_pr_er,
                               vars_1040, vars_payroll) {

  #----------------------------------------------------------------------------
  # Composition-weighted bundle MTR: the marginal income-tax response to scaling
  # the record's entire WEALTH_CAP_FLOWS bundle along the EXACT direction the
  # haircut moves it (pure-capital and kg_lt_basis by the same factor;
  # pass-through + SE companions by the 0.2-weighted factor). MEASURED through
  # the calculator (not assigned from statutory classes), so reforms to QBI /
  # muni exclusion / gain rates are reflected automatically.
  #
  #   tau_i = dT_i / (bump * F_i),   dT = liab_iit_net(bumped) - actual
  #
  # which equals the plan's sum_c (w_c flow_c / F) MTR_c (the directional
  # derivative). Income tax ONLY (pr = F): capital-income-triggered spillovers
  # (SS taxability, AGI phaseouts, NIIT, QBI) are correctly included; the
  # pass-through SECA slice is omitted (<1%, documented).
  #
  # Frame: must be the same conv-no-wealth pre-do_taxes frame ΔT⁰ is measured
  # on; run BEFORE the deemed fold. Mirrors mtr_kg_lt_lawonly (run.R:626-645):
  # full-frame recompute (never a subset -- positional random_numbers),
  # calc_estate_flag = calc_wealth_flag = FALSE.
  #
  # Parameters:
  #   - tax_units (df)         : pre-do_taxes frame (raw + behavioral feedback)
  #   - actual_liab_iit (dbl[]): liab_iit_net on the un-bumped frame (alive-leg,
  #                              pre-deemed-fold)
  #   - baseline_pr_er (df)    : baseline employer payroll (passed to do_taxes)
  #   - vars_1040 / vars_payroll : do_taxes variable lists
  #
  # Returns: tibble with mtr_cap_bundle and cap_bundle_F (one row per record,
  #          aligned to tax_units$id).
  #----------------------------------------------------------------------------

  eps   = WEALTH_DYN_MTR_BUMP
  F     = wealth_dyn_capital_total(tax_units)
  gross = wealth_dyn_economic_gross(tax_units)

  pt_cols = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                      names(tax_units))
  pure_cols = intersect(c(WEALTH_CAP_FLOWS_PURE, WEALTH_CAP_FLOWS_PURE_BASIS),
                        names(tax_units))

  bumped = tax_units %>%
    mutate(across(all_of(pure_cols), ~ . * (1 + eps)),
           across(all_of(pt_cols),   ~ . * (1 + WEALTH_CAP_FLOWS_PT_WEIGHT * eps)))

  taxed_bumped = bumped %>%
    do_taxes(baseline_pr_er   = baseline_pr_er,
             vars_1040        = vars_1040,
             vars_payroll     = vars_payroll,
             calc_estate_flag = FALSE,
             calc_wealth_flag = FALSE)

  stopifnot(identical(taxed_bumped$id, tax_units$id))

  dT  = taxed_bumped$liab_iit_net - actual_liab_iit
  # F floor as a fraction of gross; net-capital-loss (F < 0) records get 0 (zero
  # feedback, per D7 -- y is clamped >= 0 at the cell level too).
  ok  = F >= pmax(WEALTH_DYN_F_FLOOR * gross, WEALTH_DYN_EPS)
  mtr = if_else(ok, dT / (eps * F), 0)

  tibble(mtr_cap_bundle = mtr, cap_bundle_F = F)
}



#-------------------------------------------------------------------------------
# r_total(t) and transition operator
#-------------------------------------------------------------------------------

wealth_dyn_read_rtotal = function(scenario_info, params) {

  #----------------------------------------------------------------------------
  # r_total(t) = nominal GDP-per-capita growth, per year, spliced across the
  # Macro-Projections historical.csv + projections.csv series. NOMINAL (matches
  # the nominal wealth stock/flows):
  #   r_total(t) = (gdp_t / gdp_{t-1}) / (pop_t / pop_{t-1}) - 1
  # where `gdp` is nominal GDP and population is the sum of the per-age
  # unmarried_* + married_* tax-unit-count columns. (NOTE: do NOT use gdp_c --
  # that is the CONSUMPTION component of GDP, not GDP per capita.)
  #
  # We splice historical+projections rather than reading projections.csv alone:
  # projections.csv begins in the first projection year, so the YoY growth of
  # that boundary year (and of any pre-projection lead-in year, e.g. a sim that
  # starts a year before the policy to capture FY revenue) has no t-1 predecessor
  # and is undefined. Splicing differences the boundary growth off the real prior
  # actual year, mirroring kg_dynamics.R's cpiu/tsy loaders. Matches the
  # calibration diagnostic other/wealth_dynamics/cohort_wealth_growth.R. Plus the
  # optional additive path-delta knob (default 0, a one-time sensitivity test).
  #
  # Returns: a named numeric vector r_total[as.character(year)] over
  #          scenario_info$years.
  #----------------------------------------------------------------------------

  macro_root = scenario_info$interface_paths$`Macro-Projections`
  read_gdp_pop = function(f) {
    raw = read_csv(file.path(macro_root, f), show_col_types = FALSE)
    pop_cols = grep('^(unmarried|married)_[0-9]+$', names(raw), value = TRUE)
    if (length(pop_cols) == 0) {
      stop('wealth_dynamics: no unmarried_*/married_* population columns in ',
           'Macro-Projections ', f, '; cannot form GDP per capita.')
    }
    raw %>% transmute(year, gdp,
                      pop = rowSums(across(all_of(pop_cols), ~ replace_na(., 0))))
  }

  # Splice historical (through the last actual year) ahead of projections so
  # every requested year -- including a lead-in or the projection boundary -- has
  # a real t-1 predecessor for the YoY growth difference. distinct() keeps the
  # historical (actual) row if the two series ever overlap on a year.
  macro = bind_rows(read_gdp_pop('historical.csv'),
                    read_gdp_pop('projections.csv')) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year) %>%
    mutate(r = (gdp / lag(gdp)) / (pop / lag(pop)) - 1)

  delta = params$r_total$additive_delta %||% 0
  yrs   = scenario_info$years
  r     = macro$r[match(yrs, macro$year)] + delta

  if (anyNA(r)) {
    stop('wealth_dynamics: nominal GDP/capita growth missing for year(s) ',
         paste(yrs[is.na(r)], collapse = ', '),
         ' in spliced Macro-Projections historical.csv + projections.csv. ',
         'r_total(t) is undefined.')
  }
  setNames(r, as.character(yrs))
}



build_within_age_transition = function(params, ages, n_bins) {

  #----------------------------------------------------------------------------
  # The within-age percentile transition M. v1 PLACEHOLDER: 100x100 identity
  # (full persistence) for every age when transition_matrix_file is null/absent.
  # Otherwise loads an .rds (either a single n_bins x n_bins matrix applied to
  # all ages, or a per-age named list) and rakes each to doubly-stochastic.
  #
  # Returns: either a single n_bins x n_bins matrix (applied to every age) or a
  #          named-by-age list of such matrices, consumable by
  #          apply_percentile_transition().
  #----------------------------------------------------------------------------

  f = params$transition_matrix_file
  if (is.null(f) || identical(f, '') || identical(tolower(as.character(f)), 'null')) {
    return(diag(n_bins))            # # PLACEHOLDER: full persistence (identity)
  }
  M = readRDS(f)
  if (is.list(M) && !is.matrix(M)) {
    return(lapply(M, sinkhorn_rake))
  }
  sinkhorn_rake(M)
}



#-------------------------------------------------------------------------------
# Detail IO for the conv-no-wealth pass
#-------------------------------------------------------------------------------

wealth_dyn_convnw_detail_dir = function(scenario_info) {
  # Distinct pass root so the conv-no-wealth detail never clobbers the final
  # conventional detail (which distribution and receipts read). No totals /
  # receipts are written for this intermediate pass.
  file.path(scenario_info$output_path, 'conventional_no_wealth', 'detail')
}

wealth_dyn_convnw_detail_path = function(scenario_info, year) {
  file.path(wealth_dyn_convnw_detail_dir(scenario_info), paste0(year, '.csv'))
}



#-------------------------------------------------------------------------------
# Pre-pass: per-living-record deficit P over (age, percentile) cells
#-------------------------------------------------------------------------------

run_wealth_bathtub_pass = function(scenario_info, tax_law,
                                   vat_price_offset     = NULL,
                                   excess_growth_offset = NULL) {

  #----------------------------------------------------------------------------
  # Orchestrates the wealth bathtub pre-pass for one scenario. For each year:
  # reads the scenario CONV-NO-WEALTH detail and the baseline static detail,
  # assigns (age x net-worth-percentile) cells, builds the CONVENTIONAL forcing
  # ΔT⁰ = Δ(liab_iit_pr + liab_wealth), forms the cell-aggregate yield y, the
  # bundle MTR tau, and the wealth-tax MTR tau_w, and runs the per-living-record
  # recurrence
  #     P(a,p,t) = G(a,p,t) * [aged + percentile-transitioned P(t-1)]
  #                + s * ΔT⁰(a,p,t)
  # with the feedback growth kernel
  #     G(a,p,t) = (1 + r_total(t)) - s*(tau(a,p,t)*y(a,p) + tau_w(a,p,t)).
  # There is NO (1-m) survival factor (deaths handled at aggregation via each
  # record's estate_m; D1). Writes P + the per-age percentile cutoffs to
  # {scenario}/conventional/supplemental/wealth_dynamics_state/{year}.rds.
  #
  # Depends on the conv-no-wealth scenario detail (this scenario) AND the
  # baseline static detail (Phase 1) being present.
  #
  # Returns: invisibly NULL.
  #----------------------------------------------------------------------------

  wealth_dyn_check_run_compat(scenario_info, vat_price_offset, excess_growth_offset)

  params  = wealth_dyn_load_params()
  s       = scenario_info$s
  ages    = WEALTH_DYN_AGE_MIN:WEALTH_DYN_AGE_MAX
  n_ages  = length(ages)
  n_bins  = params$n_pctiles

  A       = build_aging_matrix(ages)
  M       = build_within_age_transition(params, ages, n_bins)
  r_total = wealth_dyn_read_rtotal(scenario_info, params)
  years   = scenario_info$years

  has_baseline = !is.null(globals$baseline_root)

  # Deficit state P[age, percentile], in dollars (cell total). Boundary: 0
  # before any reform year (ΔT⁰ = 0 there since scenario law = baseline law).
  P = matrix(0, n_ages, n_bins, dimnames = list(ages, NULL))

  for (t in years) {

    scen = wealth_dyn_read_convnw_detail(scenario_info, t)
    base = wealth_dyn_read_baseline_detail(t, has_baseline)

    # Match distribution.R's liab_iit_pr forcing population: dependent returns
    # are excluded (distribution.R filters dep_status == 0 before forming
    # liab_iit_pr). The applier excludes them too (no cell), so the forced and
    # drained populations agree.
    scen = scen %>% filter(dep_status == 0)

    # Cohort key + within-age percentile cutoffs/bins on the RAW (pre-behavior)
    # net worth, positive-NW only (D17). net_worth_raw, not the detail's
    # net_worth column, which a behavior module (e.g. wealth avoidance) may have
    # overwritten -- the applier ranks on the raw stock, so the pre-pass must
    # too (else cells/conservation break; review HIGH finding).
    scen$age_cohort = wealth_dyn_age_cohort(scen)
    cutoffs = compute_within_age_cutoffs(scen$net_worth_raw, scen$weight,
                                         scen$age_cohort, ages, n_bins,
                                         positive_only = TRUE)
    scen$bin = assign_within_age_bin(scen$net_worth_raw, scen$age_cohort, cutoffs,
                                     n_bins, positive_only = TRUE)

    # Per-record during-life tax delta ΔT⁰ (CONVENTIONAL, wealth-excluding):
    #   liab_iit_pr = liab_iit_net + liab_pr - liab_deemed   (distribution.R:176)
    #   forcing leg = liab_iit_pr + liab_wealth
    scen = scen %>%
      mutate(liab_iit_pr_scen = liab_iit_net + liab_pr - coalesce(liab_deemed, 0)) %>%
      left_join(base %>% select(id, liab_iit_pr_base, liab_wealth_base),
                by = 'id') %>%
      mutate(liab_iit_pr_base = coalesce(liab_iit_pr_base, 0),
             liab_wealth_base = coalesce(liab_wealth_base, 0),
             dT0 = weight * ((liab_iit_pr_scen + liab_wealth) -
                             (liab_iit_pr_base + liab_wealth_base)))

    # Cell aggregates (drop records with no cell: NA bin = neg/zero NW).
    cells = scen %>%
      filter(!is.na(bin)) %>%
      group_by(age_cohort, bin) %>%
      summarise(
        dT0_cell = sum(dT0,                                     na.rm = TRUE),
        F_signed = sum(weight * cap_bundle_F,                   na.rm = TRUE),
        F_pos    = sum(weight * pmax(cap_bundle_F, 0),          na.rm = TRUE),
        Fmtr_pos = sum(weight * pmax(cap_bundle_F, 0) * mtr_cap_bundle, na.rm = TRUE),
        gross    = sum(weight * economic_gross,                 na.rm = TRUE),
        nw_pos   = sum(weight * pmax(net_worth_raw, 0),         na.rm = TRUE),
        nwmtr    = sum(weight * pmax(net_worth_raw, 0) * mtr_net_worth, na.rm = TRUE),
        .groups  = 'drop') %>%
      mutate(
        # y_cell = capital income per $ gross, clamped to [0, Y_MAX] (D13 + the
        # realized-gains cap above); tau_cell = F-weighted bundle MTR; tau_w_cell
        # = NW-weighted marginal wealth rate.
        y    = pmin(pmax(if_else(gross > WEALTH_DYN_EPS, F_signed / gross, 0), 0),
                    WEALTH_DYN_Y_MAX),
        tau  = if_else(F_pos  > WEALTH_DYN_EPS, Fmtr_pos / F_pos,  0),
        tau_w= if_else(nw_pos > WEALTH_DYN_EPS, nwmtr    / nw_pos, 0))

    # Scatter cell quantities into [n_ages x n_bins] matrices.
    dT0_mat = matrix(0, n_ages, n_bins, dimnames = list(ages, NULL))
    y_mat   = matrix(0, n_ages, n_bins)
    tau_mat = matrix(0, n_ages, n_bins)
    tw_mat  = matrix(0, n_ages, n_bins)
    ri = match(cells$age_cohort, ages)
    ci = cells$bin
    idx = cbind(ri, ci)
    dT0_mat[idx] = cells$dT0_cell
    y_mat[idx]   = cells$y
    tau_mat[idx] = cells$tau
    tw_mat[idx]  = cells$tau_w

    # Feedback growth kernel. The income term needs the yield conversion
    # tau*y; the wealth tax is already a rate on wealth so tau_w enters with
    # coefficient 1 (NOT routed through y). Both feedback terms are clamped >= 0:
    # the feedback is "tax foregone as wealth erodes", which cannot be negative
    # (a cell whose F-weighted bundle MTR is negative -- refundable-credit /
    # phase-in interactions -- would otherwise push G above 1+r_total).
    rt = unname(r_total[as.character(t)])
    G  = (1 + rt) - s * (pmax(tau_mat * y_mat, 0) + pmax(tw_mat, 0))
    # A non-finite G means a genuinely mis-scaled input (NaN/Inf): abort loudly.
    if (any(!is.finite(G))) {
      bad = which(!is.finite(G), arr.ind = TRUE)
      stop(sprintf(paste0('wealth_dynamics: non-finite kernel G in %d cell(s) ',
                          'for year %d (e.g. age %s, pctile %d). Mis-scaled ',
                          'tau/y/tau_w input.'),
                   nrow(bad), t, ages[bad[1, 1]], bad[1, 2]))
    }
    # Clamp G to the physical range (0, 1+r_total]. With the y-cap and the >=0
    # feedback clamps this binds only in sparse low-NW cells with lumpy one-time
    # realizations (y spikes), whose deficit is negligible (∝ tiny NW) and which
    # cannot move the headline (top-NW cells have y ~ r and never bind). Logged,
    # not silent (no silent truncation); the count flags a systematic problem if
    # it is ever large.
    n_clamp = sum(G < WEALTH_DYN_G_FLOOR)
    if (n_clamp > 0) {
      warning(sprintf(paste0('wealth_dynamics: clamped kernel G to >= %.0e in ',
                            '%d/%d cell(s), year %d (min raw G=%.4f). Expected ',
                            'only in sparse low-net-worth lumpy-realization ',
                            'cells (negligible dollar impact); a large count ',
                            'would indicate a scaling problem.'),
                    WEALTH_DYN_G_FLOOR, n_clamp, length(G), t, min(G)))
    }
    G = pmin(pmax(G, WEALTH_DYN_G_FLOOR), 1 + rt)

    # Recurrence: carried deficit (aged + percentile-transitioned) grows by G;
    # fresh inflow s*ΔT⁰ enters at face value (end-of-year saving, D24).
    P = cohort_recurrence_step(P_prev = P, growth = G, inflow = s * dT0_mat,
                               A = A, M_by_age = M)

    write_cohort_state(
      state = list(P            = P,
                   cutoffs      = cutoffs,
                   ages         = ages,
                   n_bins       = n_bins,
                   year         = t,
                   r_total      = rt,
                   s            = s,
                   spec_version = WEALTH_DYN_SPEC_VERSION,
                   # diagnostics for verification (closed-form kernel check, etc.)
                   diag         = list(y = y_mat, tau = tau_mat, tau_w = tw_mat,
                                       G = G, dT0 = dT0_mat)),
      scenario_info = scenario_info,
      subdir        = 'wealth_dynamics_state',
      year          = t)
  }

  invisible(NULL)
}



wealth_dyn_read_convnw_detail = function(scenario_info, year) {

  # Reads one year of the scenario's conv-no-wealth detail and the columns the
  # pre-pass needs. Defensive: hard-error on a missing file or missing columns
  # (a partial/old CSV would silently zero the forcing).
  path = wealth_dyn_convnw_detail_path(scenario_info, year)
  if (!file.exists(path)) {
    stop('wealth_dynamics pre-pass: conv-no-wealth detail missing: ', path,
         ' (was Phase 2N run for this scenario-year?)')
  }
  d = path %>% fread() %>% tibble()
  need = c('id', 'weight', 'dep_status', 'filing_status', 'age1', 'age2',
           'net_worth', 'net_worth_raw', 'liab_iit_net', 'liab_pr', 'liab_wealth',
           'estate_m', 'economic_gross', 'cap_bundle_F', 'mtr_cap_bundle',
           'mtr_net_worth')
  missing = setdiff(need, names(d))
  if (length(missing) > 0) {
    stop('wealth_dynamics pre-pass: conv-no-wealth detail ', path,
         ' is missing required column(s): ', paste(missing, collapse = ', '))
  }
  if (!('liab_deemed' %in% names(d))) d$liab_deemed = 0
  d
}



wealth_dyn_read_baseline_detail = function(year, has_baseline) {

  # Reads the baseline static detail's during-life tax for the forcing's
  # baseline leg. Baseline has no behavior/no wealth, so baseline static =
  # baseline conv-no-wealth. liab_deemed / liab_wealth default to 0.
  if (!has_baseline) {
    stop('wealth_dynamics pre-pass: no baseline_root available, but ΔT⁰ is ',
         'measured as (scenario - baseline). Supply a baseline.')
  }
  path = globals$baseline_root %>%
    file.path('baseline/static/detail', paste0(year, '.csv'))
  if (!file.exists(path)) {
    stop('wealth_dynamics pre-pass: baseline static detail missing: ', path)
  }
  d = path %>% fread() %>% tibble()
  need = c('id', 'liab_iit_net', 'liab_pr')
  missing = setdiff(need, names(d))
  if (length(missing) > 0) {
    stop('wealth_dynamics pre-pass: baseline detail ', path,
         ' is missing required column(s): ', paste(missing, collapse = ', '))
  }
  if (!('liab_deemed' %in% names(d))) d$liab_deemed = 0
  if (!('liab_wealth' %in% names(d))) d$liab_wealth = 0
  d %>%
    transmute(id,
              liab_iit_pr_base = liab_iit_net + liab_pr - coalesce(liab_deemed, 0),
              liab_wealth_base = liab_wealth)
}



#-------------------------------------------------------------------------------
# Applier (built-in conventional-pass step)
#-------------------------------------------------------------------------------

wealth_dyn_apply_to_records = function(tax_units, state, params = NULL) {

  #----------------------------------------------------------------------------
  # The mechanical haircut: drains each record's share of its cell's deficit
  # P(a,p,t) out of wealth, before the behavior modules and do_taxes run on the
  # conventional frame. Per record i in cell (a_i, p_i):
  #
  #   D_alloc_i = P[a,p] * NW_i / sum_cell(w * max(NW, 0))       (proportional to NW)
  #   f_i       = clamp(D_alloc_i / economic_gross_i, -fmax, fmax)
  #
  # then scale the 14 value.* asset columns UNIFORMLY by (1 - f) (so s_pt and the
  # frozen rho_pt valuation discount stay invariant -- the WHOLE balance sheet
  # shrinks), scale the WEALTH_CAP_FLOWS income flows (pure by (1-f),
  # pass-through + SE companions by (1 - 0.2 f)) and kg_lt_basis by (1-f), and
  # recompute net_worth (debts untouched). The eroded value.* flow into
  # calc_estate (estate base falls) and calc_wealth (wealth tax reprices on the
  # eroded net_worth). Records with no cell (neg/zero NW) are untouched (f = 0).
  #
  # Parameters:
  #   - tax_units (df)   : the conventional-pass base frame (pre-behavior)
  #   - state (list)     : the year's wealth_dynamics_state (P + cutoffs)
  #   - params (list)    : wealth params (for fmax); loaded if NULL
  #
  # Returns: tax_units with eroded value.*/flows/basis, recomputed net_worth,
  #          and the diagnostic columns nw_pctile, D_alloc, wealth_haircut.
  #----------------------------------------------------------------------------

  if (is.null(params)) params = wealth_dyn_load_params()
  fmax   = params$fmax
  n_bins = state$n_bins
  ages   = state$ages
  P      = state$P

  n = nrow(tax_units)
  age_cohort = wealth_dyn_age_cohort(tax_units)
  # Rank on the RAW pre-behavior net worth -- the applier runs at the head of the
  # conventional pass (before behavior), so tax_units$net_worth IS the raw
  # materialized stock, matching the pre-pass's net_worth_raw cutoffs.
  bin = assign_within_age_bin(tax_units$net_worth, age_cohort, state$cutoffs,
                              n_bins, positive_only = TRUE)
  # Exclude dependent returns (no cell), matching the pre-pass forcing
  # population (distribution.R filters dep_status == 0).
  if ('dep_status' %in% names(tax_units)) {
    bin[tax_units$dep_status != 0] = NA_integer_
  }

  gross = wealth_dyn_economic_gross(tax_units)
  nw_pos = pmax(tax_units$net_worth, 0)

  # Cell deficit per record (P[a,p]) and cell denominator sum_cell(w * NW+).
  P_i        = rep(0, n)
  cell_denom = rep(0, n)
  has_cell   = !is.na(bin)
  if (any(has_cell)) {
    ri = match(age_cohort[has_cell], ages)
    ci = bin[has_cell]
    P_i[has_cell] = P[cbind(ri, ci)]
    # Weighted positive-NW sum within each (age, bin) cell.
    denom_tbl = tibble(age_cohort = age_cohort[has_cell],
                       bin        = ci,
                       wnw        = tax_units$weight[has_cell] * nw_pos[has_cell]) %>%
      group_by(age_cohort, bin) %>%
      summarise(denom = sum(wnw, na.rm = TRUE), .groups = 'drop')
    key = tibble(age_cohort = age_cohort[has_cell], bin = ci) %>%
      left_join(denom_tbl, by = c('age_cohort', 'bin'))
    cell_denom[has_cell] = key$denom
  }

  # D_alloc proportional to net worth; guard zero/tiny cell denominator and
  # zero gross. f symmetric clamp.
  D_alloc = if_else(cell_denom > WEALTH_DYN_EPS & has_cell,
                    P_i * nw_pos / cell_denom, 0)
  f = if_else(gross > WEALTH_DYN_EPS, D_alloc / gross, 0)
  f = pmax(-fmax, pmin(fmax, f))

  pure_cols = intersect(c(WEALTH_CAP_FLOWS_PURE, WEALTH_CAP_FLOWS_PURE_BASIS),
                        names(tax_units))
  pt_cols   = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                        names(tax_units))
  asset_cols = intersect(ESTATE_ASSET_COLS, names(tax_units))

  f_pure = 1 - f
  f_pt   = 1 - WEALTH_CAP_FLOWS_PT_WEIGHT * f

  # Debts are untouched by the haircut: compute the stock once from the original
  # frame, subtract it after the assets are eroded.
  debts = rowSums(cols_matrix(tax_units, WEALTH_DEBT_COLS))

  out = tax_units %>%
    mutate(
      across(all_of(asset_cols), ~ . * (1 - f)),
      across(all_of(pure_cols),  ~ . * f_pure),
      across(all_of(pt_cols),    ~ . * f_pt)) %>%
    # Recompute the stored net-worth stock from the (now eroded) balance sheet,
    # mirroring run_one_year:505-507 / avoidance.R:90-92, so calc_wealth
    # reprices liab_wealth and calc_estate the estate base on the eroded stock.
    mutate(
      net_worth      = rowSums(across(all_of(asset_cols), ~ replace_na(., 0))) - debts,
      # Diagnostics (non-baseline detail only; dormant when s = 0).
      nw_pctile      = bin,
      D_alloc        = D_alloc,
      wealth_haircut = f)

  out
}

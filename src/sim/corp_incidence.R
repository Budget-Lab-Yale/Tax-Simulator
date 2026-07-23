#-------------------------------------------------------------------------------
# corp_incidence.R
#
# On-model corporate incidence: a MECHANICAL, conventional-side, REVENUE-side
# channel that takes the gross corporate receipts delta from Off-Model-Estimates
# and produces (a) record-level flow cuts (dividends / interest / rent /
# pass-through), (b) an equity markdown on value.* stocks (basis never scales),
# (c) a kg gain-state adjustment, (d) bathtub dissaving via the generalized
# forcing F = dT - dY_exog, and (e) the endogenous individual-tax offset, which
# simply materializes in conventional receipts deltas. Static stays the clean
# law-only counterfactual (D5); distribution tables and the smear are untouched
# (D4); the channel contributes reform DELTAS only (P1/F4).
#
# Design documents (single source of truth for the economics):
#   - other/corporate_incidence/CONSIDERATIONS.md  (rulings D1-D18)
#   - other/corporate_incidence/FORMAL_MODEL.md    (primitives/propositions P1-P14)
#
# Like the wealth bathtub, this is NOT a behavior module: the record applier
# (corp_apply_to_records, built on top of the paths computed here) runs as a
# fixed step at the head of every conventional-side pass in run_one_year(),
# BEFORE the wealth haircut applier and the behavior modules, so the kg/wealth
# machinery runs on the shocked frame (FORMAL_MODEL section 7).
#
# ACTIVATION IS FAIL-CLOSED (D13/section 8.14): the channel turns on only when
# the scenario's Off-Model-Estimates vintage carries an explicit metadata
# declaration (corporate_meta.yaml next to revenues.csv: gross_of_offset = true,
# provision_type = rate) AND the receipts path passes a mechanical seesaw guard
# (depreciation-signature rejection; the old-capital revaluation of timing
# provisions is SIGN-FLIPPED relative to the rate case, D13/P13'). Absent
# metadata -> channel OFF, status quo (off-model receipts line + distribution
# smear), one loud warning. Metadata PRESENT but invalid or contradicted by the
# receipts path -> hard stop (a false declaration is an input error, not an
# opt-out).
#
# Everything here is analytic and cheap: paths are deterministic functions of
# the OME wedge, Macro-Projections aggregates, and hardcoded CORP_* constants.
# No serialized state, no new SLURM phase -- any worker can recompute the paths.
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Constants and provenance (hardcoded, WEALTH_CAP_FLOWS style; sweep corners via
# env overrides ONLY for CORP_SIGMA_N / CORP_KAPPA / CORP_PRICED_AS_PERMANENT)
#-------------------------------------------------------------------------------

CORP_SPEC_VERSION = 1L

# --- The D16 column contract: what enters the generalized forcing ------------
#
# IN (external income; the applied dollar deltas accumulate into the per-record
# detail column `corp_dY_exog`, consumed by the wealth bathtub forcing
# F = dT - dY_exog):
#   - dividends (div_ord, div_pref), at the CORP_OMEGA_DIV exposure
#   - interest (txbl_int, exempt_int), on the debt rollover ramp
#   - rent (rent, rent_loss -- the NET pair scales together)
#   - pass-through lines at the 0.2 capital weight; the column list is
#     WEALTH_CAP_FLOWS_PT + WEALTH_CAP_FLOWS_SE_COMPANIONS and the weight is
#     WEALTH_CAP_FLOWS_PT_WEIGHT (src/sim/wealth_dynamics.R) -- referenced at
#     RUNTIME (not here) because source order is alphabetical.
#
# OUT (internal conversions -- tax leg only, automatically via dT; adding any
# of these to dY_exog double-counts the balance-sheet/gain-state markdown, the
# two-pocket lemma P7/P9):
#   - realized gains kg_st / kg_lt / kg_1250 / kg_collect (and kg_lt_basis)
#   - retirement distributions txbl_ira_dist / txbl_pens_dist / gross_pens_dist
#   - sale proceeds of any marked-down asset
# Do NOT add these to the dY_exog accumulation later. This list exists so the
# contract is greppable; the applier never scales-and-accumulates them.
CORP_FLOWS_DIV  = c('div_ord', 'div_pref')
CORP_FLOWS_INT  = c('txbl_int', 'exempt_int')
CORP_FLOWS_RENT = c('rent', 'rent_loss')
CORP_FLOWS_INTERNAL = c('kg_st', 'kg_lt', 'kg_1250', 'kg_collect',
                        'kg_lt_basis', 'txbl_ira_dist', 'txbl_pens_dist',
                        'gross_pens_dist')

# --- Exposure vector omega_a: equity share of each value.* column ------------
# Only these columns take the markdown; everything else (incl. ALL pass-through
# value.* columns) is deliberately 0 -- the migration leg is flows-only (P14),
# which keeps the frozen estate-valuation bridge (rho_pt, s_pt) invariant.
# value.db is NEVER debited: DB shortfalls land on plan sponsors and join the
# unallocated residual (D10).
# PLACEHOLDER CENTRALS (Phase 0c status table, PHASE0_NOTES.md): equities 1.0
# by construction; dc/trusts/re_fund from SCF + ICI equity-share imputations,
# pending external measurement. Update in place when measured.
CORP_ASSET_EXPOSURE = c(
  'value.equities' = 1.00,
  'value.dc'       = 0.55,
  'value.trusts'   = 0.50,
  'value.re_fund'  = 0.30
)

# C-corp share of dividends (excludes REIT / bond-fund distributions).
# PLACEHOLDER pending ICI/SOI measurement (PHASE0_NOTES.md).
CORP_OMEGA_DIV = 0.85

# C-corp equity share of realized LTCG (stock + fund shares vs pass-through
# sales / real estate / other). PLACEHOLDER ~0.5 prior pending SOI
# sale-of-capital-assets measurement (PHASE0_NOTES.md).
CORP_OMEGA_KG = 0.50

# Normal-return share sigma_N of the corporate wedge ("taxes on margins get
# shifted; taxes on rents get capitalized", D14/D15). Central 0.375 from OTA
# 63% / TPC 60% supernormal; corners {0, 0.5} (house VAT convention = upper).
# Env override CORP_SIGMA_N for sweeps.
CORP_SIGMA_N_DEFAULT = 0.375

# kappa: C-corp share of the economy-wide normal-capital stock (D15). The
# migrated normal burden splits (1-kappa) to noncorporate lines and kappa
# retained on corporate flows. PLACEHOLDER 0.40 prior pending the Fed Z.1 pull;
# the owner-occupied-housing definitional fork sets the sweep corners
# {~0.25, ~0.4, ~0.5}. Env override CORP_KAPPA for sweeps.
CORP_KAPPA_DEFAULT = 0.40

# theta: US-taxable exposure scale on the flow factor phi = -theta * h_c / pi.
# Absorbs the NIPA-economic vs US-taxable profit wedge (Phase 0c). PLACEHOLDER
# 1.0 (pro-rata: every distribution scales by the aggregate after-tax-profit
# hit share) pending Rosenthal-Austin / Z.1 measurement.
CORP_THETA = 1.0

# theta_res: foreign / nonprofit / DB residual share of the wedge, used ONLY by
# the conservation diagnostic's B_res line (D3/D10 -- the honest unallocated
# remainder; no gross-up forces household hits to sum to the revenue line).
# PLACEHOLDER 0.40 (Rosenthal-Austin: ~26% foreign + nonprofits/insurers + the
# DB slice) pending Phase 0c measurement.
CORP_THETA_RES = 0.40

# Vintaging: NIPA economic depreciation rate; the reallocation clock IS the
# replacement clock (D14), same 0.057 as do_capital_adjustment
# (src/data/economy.R). eta(t) = 1 - (1 - 0.057)^(t - t0).
CORP_DELTA_NIPA = 0.057

# Equity discount rate r = nominal tsy_10y (Macro-Projections, enactment year)
# + this fixed equity risk premium. Distributions are nominal, so r is nominal
# (the house Fisher-deflation convention applies to real-utility discounting,
# not nominal-flow PV -- plan note). mu is r-free in the permanent central
# case; r shapes temporary-shock annuities and the migration PV only.
CORP_EQUITY_PREMIUM = 0.05

# PV grid: paths are built through max(sim years) + this many tail years, with
# a Gordon growing-perpetuity terminal beyond (guarded r > g).
CORP_PV_TAIL_YEARS = 80L

# Seesaw guard (D13/P13' depreciation signature): reject when the smaller-signed
# mass of the wedge path exceeds this fraction of the larger-signed mass (and
# the absolute floor, $B, so rounding wobble on a rate path never trips it).
CORP_SEESAW_RETRACE_MAX = 0.10
CORP_SEESAW_ABS_FLOOR   = 0.5

# Sanity bound on the proportional equity markdown |mu|. The naive ceiling for
# a 21->28 hike is ~8.9% (Delta-tau/(1-tau)); anything approaching 50% means
# mis-scaled inputs (units, wrong baseline leg), not policy.
CORP_MU_MAX = 0.5

CORP_EPS = 1e-9



#-------------------------------------------------------------------------------
# Input contract: wedge + metadata + guards (fail-closed gate)
#-------------------------------------------------------------------------------

# Per-process memo of gate decisions and resolved paths (keyed by scenario ID).
# Each SLURM worker is its own process and rebuilds its own cache; everything
# cached here is a cheap deterministic function of on-disk inputs.
.corp_cache = new.env(parent = emptyenv())


corp_ome_roots = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The two Off-Model-Estimates legs: the scenario's own vintage/ID (reform)
  # and the baseline leg (house convention: interface_root() defaults resolve
  # the baseline scenario's interface versions; mirror distribution.R's
  # other_corp_delta reader).
  #
  # Returns: list(scenario = path, baseline = path).
  #----------------------------------------------------------------------------

  scen = scenario_info$interface_paths$`Off-Model-Estimates`
  base = interface_root('Off-Model-Estimates', 'baseline')
  if (length(base) == 0) {
    base = interface_root('Off-Model-Estimates')   # first-row default
  }
  if (length(base) == 0 || is.null(scen)) {
    stop('corp_incidence: cannot resolve Off-Model-Estimates interface paths ',
         'for scenario "', scenario_info$ID, '".')
  }
  list(scenario = scen, baseline = base)
}



corp_read_ome_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The RESIDUAL OME corporate wedge w_t = corporate_reform - corporate_baseline
  # ($B, CY), over the FULL horizon of the OME files (not just sim years -- the
  # seesaw guard and the enactment clock need the whole declared path). Mirrors
  # distribution.R's other_corp_delta reader minus the VAT deflation (the
  # run-compat guard refuses VAT scenarios outright).
  #
  # NOTE: as of the on-model statutory-rate module, this is the "ex rate, ex
  # depreciation" corporate residual (international, credits, base-broadeners).
  # Statutory-rate revenue is now on-model (corp_rate_incidence_wedge below);
  # OME vintages must be regenerated to EXCLUDE the rate portion, or the rate
  # wedge is double-counted. (Pure-rate scenarios read an all-zeros OME, so no
  # double-count in the interim.)
  #
  # Returns: tibble(year, w) over the union of file years.
  #----------------------------------------------------------------------------

  roots = corp_ome_roots(scenario_info)

  read_leg = function(root, nm) {
    f = file.path(root, 'revenues.csv')
    if (!file.exists(f)) {
      stop('corp_incidence: Off-Model-Estimates revenues.csv missing: ', f)
    }
    read_csv(f, show_col_types = FALSE) %>%
      transmute(year, !!nm := corporate)
  }

  read_leg(roots$scenario, 'reform') %>%
    full_join(read_leg(roots$baseline, 'baseline'), by = 'year') %>%
    arrange(year) %>%
    transmute(year,
              w = replace_na(reform, 0) - replace_na(baseline, 0))
}



corp_rate_incidence_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The on-model corporate statutory-rate incidence wedge: the CONVENTIONAL
  # (Form A base-eroded) rate revenue delta (src/sim/corp_rate.R), $B, over the
  # FULL Macro-Projections horizon. This is the on-model replacement for the
  # rate portion of the OME `corporate` wedge that corp_incidence used to read;
  # the entire downstream paths machinery (markdown recursion, kappa split, kg
  # glue, bathtub forcing) consumes it unchanged.
  #
  # The scenario rate comes from this scenario's tax_law.csv sidecar (sim years);
  # it is extended across the full macro horizon by forward-filling the last
  # in-window (t0, t) -- 'extend'/permanent semantics. Pre-policy years (leading
  # NA) carry no change (delta 0). The CY rate delta is booked on the FY rev_corp
  # level, matching corp_incidence's existing CY/FY convention (pi_at = gdp_corp
  # [CY] - rev_corp [FY]).
  #
  # Returns: tibble(year, w) over the macro horizon, or NULL when there is no
  #          rate change / the tax_law sidecars are unavailable.
  #----------------------------------------------------------------------------

  rate_series = corp_rate_read_series(
    file.path(globals$output_root, scenario_info$ID,
              'static/supplemental/tax_law.csv'))
  if (is.null(rate_series)) return(NULL)

  rev_corp = read_macro_spliced(scenario_info$interface_paths$`Macro-Projections`) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year) %>%
    transmute(year, rev_corp)

  # Extend the rate series across the full macro horizon (permanence forward;
  # leading pre-policy years stay NA -> corp_rate_delta returns 0 there).
  full_rate = rev_corp %>%
    select(year) %>%
    left_join(rate_series, by = 'year') %>%
    arrange(year) %>%
    fill(t0, t, .direction = 'down')

  w = corp_rate_delta(full_rate, rev_corp, static = FALSE) %>%
    rename(w = delta)

  if (!any(abs(w$w) > CORP_EPS)) return(NULL)
  w
}



corp_read_wedge = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The total corporate incidence wedge = on-model statutory-rate delta
  # (corp_rate_incidence_wedge) + residual OME corporate wedge
  # (corp_read_ome_wedge), summed over the union of years ($B, CY). For a
  # pure-rate scenario the OME residual is all-zeros and this is just the rate
  # wedge; for a residual-only OME scenario it is just the OME wedge (the
  # pre-existing behavior).
  #
  # Returns: tibble(year, w) over the union of years.
  #----------------------------------------------------------------------------

  ome  = corp_read_ome_wedge(scenario_info)
  rate = corp_rate_incidence_wedge(scenario_info)

  if (is.null(rate)) return(ome)

  ome %>%
    rename(w_ome = w) %>%
    full_join(rate %>% rename(w_rate = w), by = 'year') %>%
    arrange(year) %>%
    transmute(year, w = replace_na(w_ome, 0) + replace_na(w_rate, 0))
}



corp_meta_path = function(ome_root) {
  file.path(ome_root, 'corporate_meta.yaml')
}


corp_read_meta = function(ome_root) {

  #----------------------------------------------------------------------------
  # Reads and validates the fail-closed input declaration corporate_meta.yaml
  # sitting NEXT TO the OME vintage's revenues.csv (additive to interface v4;
  # no version bump). Two distinct failure behaviors (section 8.14):
  #   - file ABSENT           -> returns NULL (caller: channel OFF, loud warning)
  #   - file PRESENT, invalid -> hard stop (a false declaration is an input
  #                              error, not an opt-out)
  #
  # Required fields:
  #   gross_of_offset : must be TRUE (D1 -- the input is gross by construction;
  #                     a JCT-benchmark-net input double-counts the endogenous
  #                     offset and must be re-derived/grossed up first)
  #   provision_type  : must be 'rate' (D13 -- the receipts path is a valid
  #                     year-by-year proxy for the after-tax profit path;
  #                     depreciation/transition provisions fail gate (G))
  #   beyond_horizon  : 'extend' or 'zero' -- permanence past the file horizon
  #                     (needed for the perfect-foresight PV)
  # Optional fields:
  #   delta_tau       : named year->Delta-tau map for the w ~= Delta-tau * Pi
  #                     cross-check (WARN-level)
  #   produced_by, date : provenance strings (not validated)
  #
  # Returns: validated metadata list, or NULL when the file is absent.
  #----------------------------------------------------------------------------

  path = corp_meta_path(ome_root)
  if (!file.exists(path)) return(NULL)

  meta = read_yaml(path)

  problems = character(0)
  if (!isTRUE(meta$gross_of_offset)) {
    problems = c(problems, paste0(
      'gross_of_offset must be true. A JCT-benchmarked (offset-embedded, ',
      'Nunns) corporate line is NOT gross-of-offset; booking the on-model ',
      'endogenous offset on top of it double-counts (combined revenue ',
      'understated, bounded by the embedded offset -- D1 / section 8.4). ',
      'Re-derive or gross up the input, or remove corporate_meta.yaml to ',
      'run status quo.'))
  }
  if (!identical(as.character(meta$provision_type %||% ''), 'rate')) {
    problems = c(problems, paste0(
      "provision_type must be 'rate' (got '", meta$provision_type %||% '<missing>',
      "'). Only rate/rent-heavy provisions satisfy the D13 eligibility gate ",
      '(receipts path == after-tax profit path proxy). Depreciation-type ',
      'provisions are PERMANENTLY ineligible (old-capital revaluation is ',
      'sign-flipped, P13\'); keep them on their own interface.'))
  }
  bh = as.character(meta$beyond_horizon %||% '')
  if (!bh %in% c('extend', 'zero')) {
    problems = c(problems, paste0(
      "beyond_horizon must be 'extend' or 'zero' (got '",
      if (nzchar(bh)) bh else '<missing>',
      "'). The perfect-foresight markdown needs the wedge's permanence past ",
      'the file horizon.'))
  }

  if (length(problems) > 0) {
    stop('corp_incidence: INVALID corporate_meta.yaml at ', path, ':\n  - ',
         paste(problems, collapse = '\n  - '),
         '\nA present-but-invalid declaration is a hard input error (fail-',
         'closed contract, CONSIDERATIONS section 8.14).')
  }

  meta$beyond_horizon = bh
  meta
}



corp_seesaw_check = function(w_path) {

  #----------------------------------------------------------------------------
  # Mechanical eligibility guard (D13 gate (G), enforced): reject receipts
  # paths whose signed mass materially runs BOTH ways -- the timing-seesaw
  # signature of depreciation-type provisions (e.g. bonus: -$61B in 2026
  # reversing to +$35B by 2030), whose old-capital revaluation is SIGN-FLIPPED
  # (P13'). A rate change on a fixed base moves receipts one way (plus rounding
  # wobble); the absolute floor keeps tiny wobble from tripping the guard.
  # This also catches cumulative-delta sign reversals: a path that reverses
  # sign necessarily carries offsetting signed mass.
  #
  # Returns: list(ok, retrace, msg).
  #----------------------------------------------------------------------------

  pos = sum(pmax(w_path, 0), na.rm = TRUE)
  neg = sum(pmax(-w_path, 0), na.rm = TRUE)
  small = min(pos, neg)
  big   = max(pos, neg)

  retrace = if (big > 0) small / big else 0
  ok = !(small > CORP_SEESAW_ABS_FLOOR && retrace > CORP_SEESAW_RETRACE_MAX)

  msg = if (ok) NA_character_ else sprintf(
    paste0('corporate wedge path runs both ways: $%.1fB one direction vs ',
           '$%.1fB the other (retrace %.0f%%, guard %.0f%%). This is the ',
           'depreciation/timing-provision receipts signature, which is NOT a ',
           'valid after-tax profit path (gate (G), D13) -- its old-capital ',
           'revaluation is sign-flipped (P13\'). The rate-type declaration in ',
           'corporate_meta.yaml contradicts the data.'),
    big, small, 100 * retrace, 100 * CORP_SEESAW_RETRACE_MAX)

  list(ok = ok, retrace = retrace, msg = msg)
}



scenario_uses_corp_incidence = function(scenario_info) {

  #----------------------------------------------------------------------------
  # The activation rule (auto-on from input metadata; no runscript column):
  # a non-baseline scenario with (i) a nonzero corporate wedge somewhere,
  # (ii) valid corporate_meta.yaml present on the scenario's OME vintage, and
  # (iii) the seesaw guard passing, runs the channel. Absent metadata with a
  # nonzero wedge -> OFF with ONE loud warning naming the reason (status quo:
  # off-model receipts line + distribution smear -- the smear is a
  # distribution fallback; the revenue status quo is simply "no on-model
  # offset"). Present-but-contradicting metadata -> hard stop (inside
  # corp_read_meta / here).
  #
  # Memoized per scenario ID (per process); A/B runs use the existing
  # dep.Off-Model-Estimates.vintage/.ID runscript overrides.
  #
  # Returns: TRUE/FALSE.
  #----------------------------------------------------------------------------

  if (scenario_info$ID == 'baseline') return(FALSE)

  key = paste0('gate|', scenario_info$ID)
  hit = .corp_cache[[key]]
  if (!is.null(hit)) return(hit$active)

  wedge  = corp_read_wedge(scenario_info)
  has_w  = any(abs(wedge$w) > CORP_EPS)
  roots  = corp_ome_roots(scenario_info)

  # A nonzero on-model statutory-rate change SELF-DECLARES the eligibility
  # contract: it is a rate provision by construction (provision_type 'rate'),
  # gross of the endogenous individual offset (a pure corporate receipts number),
  # and permanent past the horizon (beyond_horizon 'extend'). No
  # corporate_meta.yaml is required for it -- the file governs only a residual
  # OME corporate wedge. When only an OME residual is present (no rate change),
  # the original OME meta contract applies unchanged.
  rate_wedge  = corp_rate_incidence_wedge(scenario_info)
  rate_active = !is.null(rate_wedge)
  if (rate_active) {
    meta = list(gross_of_offset = TRUE, provision_type = 'rate',
                beyond_horizon = 'extend', produced_by = 'on-model corp_rate')
  } else {
    meta = corp_read_meta(roots$scenario)   # hard-stops if present-but-invalid
  }

  active = FALSE
  if (!has_w) {
    # No corporate delta: dormant regardless of metadata (delta-only doctrine).
    active = FALSE
  } else if (is.null(meta)) {
    warning(paste0(
      'corp_incidence: scenario "', scenario_info$ID, '" has a nonzero ',
      'corporate receipts delta but its Off-Model-Estimates vintage (',
      roots$scenario, ') carries no corporate_meta.yaml declaration. The ',
      'on-model corporate channel stays OFF (fail-closed, CONSIDERATIONS ',
      'section 8.14): status quo off-model corporate receipts line + ',
      'distribution smear, and NO on-model individual offset / estate / ',
      'wealth interaction. If this input is a gross-of-offset RATE provision, ',
      'add corporate_meta.yaml next to revenues.csv to activate.'),
      call. = FALSE)
    active = FALSE
  } else {
    seesaw = corp_seesaw_check(wedge$w)
    if (!seesaw$ok) {
      stop('corp_incidence: scenario "', scenario_info$ID, '": ', seesaw$msg)
    }

    # Enactment must not predate the sim window: the enactment-year markdown
    # (the announcement capital loss, D7) would be silently missed.
    t0 = min(wedge$year[abs(wedge$w) > CORP_EPS])
    if (t0 < min(scenario_info$years)) {
      stop('corp_incidence: scenario "', scenario_info$ID, '": the corporate ',
           'wedge begins in ', t0, ' but the simulation starts in ',
           min(scenario_info$years), '. The enactment-year equity markdown ',
           'would be missed. Start `years` at or before ', t0,
           ' (house convention: one year before the policy).')
    }

    if (t0 > max(scenario_info$years)) {
      message('corp_incidence: scenario "', scenario_info$ID, '": corporate ',
              'wedge begins in ', t0, ', after the sim window ends (',
              max(scenario_info$years), '); channel dormant this run.')
      active = FALSE
    } else {
      active = TRUE
    }
  }

  .corp_cache[[key]] = list(active = active, wedge = wedge, meta = meta)
  active
}



corp_check_run_compat = function(scenario_info, vat_price_offset,
                                 excess_growth_offset) {

  #----------------------------------------------------------------------------
  # Refusal gate for an ACTIVE corporate channel, mirroring
  # wealth_dyn_check_run_compat: the paths are formed from raw-dollar national
  # aggregates and the record hits land on raw-dollar flows/stocks, so VAT
  # price offsets and excess-growth scaling would put the channel in an
  # inconsistent unit system; the conservation diagnostic and the bathtub
  # composition additionally assume full-population aggregates. Stops on
  # violation.
  #
  # Returns: invisibly TRUE.
  #----------------------------------------------------------------------------

  if (!isTRUE(all.equal(globals$pct_sample, 1))) {
    stop('corp_incidence requires pct_sample = 1 (full sample): the analytic ',
         'aggregate-to-record mapping and the conservation diagnostic assume ',
         'full-population weights. Re-run with pct_sample = 1.')
  }

  vat_active = !is.null(vat_price_offset) &&
               'cpi_factor' %in% colnames(vat_price_offset) &&
               any(abs(vat_price_offset$cpi_factor - 1) > 1e-10, na.rm = TRUE)
  if (vat_active) {
    stop('corp_incidence is not compatible with VAT scenarios: raw-dollar ',
         'corporate paths would mix with VAT-scaled record incomes. Run ',
         'without a VAT.')
  }

  growth_active = isTRUE(scenario_info$excess_growth != 0) &&
                  is.finite(scenario_info$excess_growth_start_year)
  if (growth_active) {
    stop('corp_incidence is not compatible with excess-growth scenarios ',
         '(excess_growth = ', scenario_info$excess_growth, '): raw-dollar ',
         'corporate paths would not match growth-adjusted record incomes.')
  }

  invisible(TRUE)
}



#-------------------------------------------------------------------------------
# Aggregate series readers (Macro-Projections)
#-------------------------------------------------------------------------------

corp_read_macro = function(scenario_info) {

  #----------------------------------------------------------------------------
  # Reads the Macro-Projections series the paths need, spliced across
  # historical.csv + projections.csv (helpers.R read_macro_spliced pattern;
  # gdp_corp exists only in projections, which is fine -- pre-enactment years
  # never need pi):
  #   - gdp_corp (NIPA pre-tax corporate profits, $B, CY)
  #   - rev_corp (CBO corporate receipts, $B, FY)   => pi = gdp_corp - rev_corp
  #   - tsy_10y  (percent)                          => r = tsy_10y/100 + ERP
  #   - gdp_interest / gdp_rent / gdp_proprietors   => noncorporate line bases
  #
  # Returns: tibble(year, pi_at, tsy_10y, gdp_interest, gdp_rent,
  #          gdp_proprietors). Named pi_at (after-tax profits), never `pi`:
  #          a missing `pi` column would silently resolve to base::pi inside
  #          dplyr verbs.
  #----------------------------------------------------------------------------

  macro_root = scenario_info$interface_paths$`Macro-Projections`
  if (is.null(macro_root)) {
    stop('corp_incidence: scenario_info$interface_paths$`Macro-Projections` ',
         'is NULL; cannot form the after-tax profit path.')
  }

  raw = read_macro_spliced(macro_root) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year)
  need = c('rev_corp', 'tsy_10y', 'gdp_interest', 'gdp_rent', 'gdp_proprietors')
  missing = setdiff(need, names(raw))
  if (length(missing) > 0 || !('gdp_corp' %in% names(raw))) {
    stop('corp_incidence: Macro-Projections vintage at ', macro_root,
         ' lacks required column(s): ',
         paste(c(missing, setdiff('gdp_corp', names(raw))), collapse = ', '),
         '. gdp_corp (NIPA pre-tax corporate profits) ships in projections.csv',
         ' of current vintages; check dep.Macro-Projections.vintage.')
  }

  raw %>%
    transmute(year,
              pi_at = gdp_corp - rev_corp,
              tsy_10y,
              gdp_interest, gdp_rent, gdp_proprietors)
}



corp_rollover_ramp = function() {

  #----------------------------------------------------------------------------
  # Cumulative share of debt rolled over `tenor` years after enactment, from
  # resources/debt_maturities.csv (same source do_capital_adjustment uses;
  # ~fully rolled by year 10). Returns a function roll(t_since) with
  # roll(<=0) = 0 and roll(beyond table) = 1.
  #----------------------------------------------------------------------------

  sched = read_csv('./resources/debt_maturities.csv', show_col_types = FALSE) %>%
    arrange(tenor) %>%
    mutate(cum = pmin(cumsum(share), 1))

  function(t_since) {
    out = rep(0, length(t_since))
    pos = t_since >= 1
    idx = pmin(t_since[pos], max(sched$tenor))
    out[pos] = sched$cum[match(idx, sched$tenor)]
    # Beyond the table (or missing tenors) debt is fully rolled: exactly 1,
    # matching do_capital_adjustment's replace_na(1) convention (and avoiding
    # the cumsum's terminal float dust).
    out[pos][is.na(out[pos]) | t_since[pos] > max(sched$tenor)] = 1
    out
  }
}



#-------------------------------------------------------------------------------
# Path computation (pure core + file-backed wrapper)
#-------------------------------------------------------------------------------

corp_env_knobs = function() {

  #----------------------------------------------------------------------------
  # Sweep-corner env overrides (the ONLY runtime knobs; everything else is a
  # code edit by design): CORP_SIGMA_N, CORP_KAPPA, CORP_PRICED_AS_PERMANENT.
  # Returns list(sigma_n, kappa, priced_as_permanent) with messages when
  # overridden.
  #----------------------------------------------------------------------------

  read_num = function(env, default, lo, hi) {
    v = Sys.getenv(env, unset = NA)
    if (is.na(v) || !nzchar(v)) return(default)
    x = suppressWarnings(as.numeric(v))
    if (!is.finite(x) || x < lo || x > hi) {
      stop('corp_incidence: env override ', env, ' = "', v,
           '" is not a number in [', lo, ', ', hi, '].')
    }
    message(sprintf('corp_incidence: env override %s = %s (default %s)',
                    env, x, default))
    x
  }

  list(
    sigma_n = read_num('CORP_SIGMA_N', CORP_SIGMA_N_DEFAULT, 0, 1),
    kappa   = read_num('CORP_KAPPA',   CORP_KAPPA_DEFAULT,   0, 1),
    priced_as_permanent = identical(Sys.getenv('CORP_PRICED_AS_PERMANENT'), '1')
  )
}



corp_build_paths_core = function(wedge, macro, sim_years, beyond_horizon,
                                 sigma_n, kappa, theta = CORP_THETA,
                                 omega_div = CORP_OMEGA_DIV,
                                 delta_nipa = CORP_DELTA_NIPA,
                                 erp = CORP_EQUITY_PREMIUM,
                                 priced_as_permanent = FALSE,
                                 roll_fn = NULL,
                                 pt_weight = NULL) {

  #----------------------------------------------------------------------------
  # The analytic shock pipeline (FORMAL_MODEL sections 6.2 and 7), pure in its
  # inputs so the self-checks can drive it with synthetic series.
  #
  #   wedge  : tibble(year, w) -- the OME corporate wedge, $B CY (full horizon)
  #   macro  : tibble(year, pi_at, tsy_10y, gdp_interest, gdp_rent,
  #            gdp_proprietors)
  #   sim_years, beyond_horizon ('extend'|'zero'), sigma_n, kappa, theta, ...
  #   roll_fn: function(t_since) -> cumulative debt-rollover share
  #   pt_weight: pass-through capital weight (WEALTH_CAP_FLOWS_PT_WEIGHT at
  #            runtime; parameterized for the self-checks)
  #
  # Grid: min(sim_years) .. max(sim_years) + CORP_PV_TAIL_YEARS, with Gordon
  # growing-perpetuity terminals beyond (guarded r > g). Beyond each series'
  # own horizon:
  #   - w  : 'extend' -> grows with pi from its last file value (a rate change
  #          on a growing base); 'zero' -> 0 (legislated sunset)
  #   - pi : grows at its trailing 5-year average rate
  #
  # Per year t (t0 = enactment = first nonzero-w year; all pre-t0 rows inert):
  #   eta(t)   = 1 - (1 - delta_nipa)^(t - t0)             [vintaging ramp]
  #   w_rent   = (1 - sigma_n) w ;  w_norm = sigma_n w     [wedge split]
  #   h_c      = w_rent + w_norm ((1 - eta) + eta kappa)   [corporate flow hit]
  #   phi      = -theta h_c / pi                           [flow factor]
  #   price    = w_rent + (1 - kappa)(1 - eta) w_norm      [price-relevant hit;
  #              excludes the kappa-retained slice, P14; under
  #              priced_as_permanent the sunset is ignored here]
  #   M_t      = PV_t[price_{s>t}] (backward recursion, constant r)
  #   V_t      = PV_t[pi_{s>t}]  ;  mu_t = M_t / V_t
  #   N_t      = (1 - kappa) eta w_norm                    [noncorporate hit]
  #     split across interest/rent/pt in proportion to gdp_interest / gdp_rent
  #     / pt_weight*gdp_proprietors (Macro aggregates -- the documented
  #     implementation choice); the interest slice DELIVERS only roll(t) of
  #     its share as flow (contract rigidity), the rest is the named delta-rho
  #     revaluation line in the conservation residual (D15/P14)
  #
  # Record-applier factors (per sim year):
  #   fac_div  = 1 + omega_div * phi
  #   fac_int  = 1 - H_int_delivered / gdp_interest
  #   fac_rent = 1 - H_rent / gdp_rent
  #   g_ptcap  = H_pt / (pt_weight * gdp_proprietors)  [proportional hit to the
  #              pass-through CAPITAL slice; lines scale by 1 - pt_weight*g_ptcap]
  #   mu_ret   = omega_dc * mu  (retirement markdown; dc exposure)
  #
  # Returns: list(by_year = full-grid tibble, sim = sim-year slice, r, t0,
  #               g_tail, knobs).
  #----------------------------------------------------------------------------

  if (is.null(pt_weight)) pt_weight = WEALTH_CAP_FLOWS_PT_WEIGHT
  if (is.null(roll_fn))   roll_fn   = corp_rollover_ramp()

  w_by_year = setNames(wedge$w, as.character(wedge$year))
  t0 = min(wedge$year[abs(wedge$w) > CORP_EPS])
  if (!is.finite(t0)) stop('corp_incidence: wedge path is identically zero.')

  grid = min(sim_years):(max(sim_years) + CORP_PV_TAIL_YEARS)

  # --- pi on the grid (extend at trailing growth beyond the macro horizon) ---
  pi_known = macro %>% filter(is.finite(pi_at))
  if (nrow(pi_known) < 6) {
    stop('corp_incidence: fewer than 6 years of pi = gdp_corp - rev_corp in ',
         'Macro-Projections; cannot form the profit path.')
  }
  g_tail = pi_known %>%
    slice_tail(n = 6) %>%
    summarise(g = (pi_at[n()] / pi_at[1])^(1 / (n() - 1)) - 1) %>%
    pull(g)

  pi_grid = pi_known$pi_at[match(grid, pi_known$year)]
  last_known = max(pi_known$year)
  beyond = which(grid > last_known)
  if (length(beyond) > 0) {
    pi_last = pi_known$pi_at[pi_known$year == last_known]
    pi_grid[beyond] = pi_last * (1 + g_tail)^(grid[beyond] - last_known)
  }

  # --- w on the grid ---------------------------------------------------------
  w_grid = w_by_year[as.character(grid)]
  file_last = max(wedge$year)
  # Years on the grid before the file horizon but absent from the file are 0.
  w_grid[is.na(w_grid) & grid <= file_last] = 0
  if (beyond_horizon == 'zero') {
    w_grid[is.na(w_grid)] = 0
  } else {
    # 'extend': continue the last file value, growing with pi.
    w_last = w_by_year[as.character(file_last)]
    ext = which(is.na(w_grid))
    if (length(ext) > 0) {
      pi_at_last = pi_grid[match(file_last, grid)]
      if (is.na(pi_at_last)) {
        stop('corp_incidence: pi undefined at the OME file horizon (',
             file_last, '); cannot extend the wedge.')
      }
      w_grid[ext] = w_last * pi_grid[ext] / pi_at_last
    }
  }
  w_grid = unname(w_grid)

  # pi must exist wherever the wedge is live.
  need_pi = abs(w_grid) > CORP_EPS
  if (any(need_pi & !is.finite(pi_grid))) {
    bad = grid[need_pi & !is.finite(pi_grid)][1]
    stop('corp_incidence: pi = gdp_corp - rev_corp unavailable for year ', bad,
         ' but the corporate wedge is nonzero there. gdp_corp exists only in ',
         'Macro-Projections projections.csv; check the vintage.')
  }

  # --- clocks and split ------------------------------------------------------
  t_since = pmax(grid - t0, 0)
  eta  = if_else(grid < t0, 0, 1 - (1 - delta_nipa)^t_since)
  roll = roll_fn(pmax(grid - t0, 0))

  w_rent = (1 - sigma_n) * w_grid
  w_norm = sigma_n * w_grid

  h_c = w_rent + w_norm * ((1 - eta) + eta * kappa)
  phi = if_else(abs(w_grid) > CORP_EPS & is.finite(pi_grid) & pi_grid > 0,
                -theta * h_c / pi_grid, 0)

  # --- price-relevant hit and the PV objects (constant r from enactment) ----
  tsy_t0 = macro$tsy_10y[match(t0, macro$year)]
  if (!is.finite(tsy_t0)) {
    # Fall back to the first sim year with a rate (t0 can precede the macro
    # projection start only in exotic vintages; pre-t0 rows are inert anyway).
    tsy_t0 = macro$tsy_10y[match(min(sim_years), macro$year)]
  }
  if (!is.finite(tsy_t0)) {
    stop('corp_incidence: tsy_10y unavailable at enactment year ', t0, '.')
  }
  r = tsy_t0 / 100 + erp
  if (r <= g_tail + 0.005) {
    stop(sprintf(paste0('corp_incidence: discount rate r = %.3f is not ',
                        'safely above the tail growth rate g = %.3f; the ',
                        'Gordon terminal PV diverges. Check tsy_10y / ',
                        'CORP_EQUITY_PREMIUM / the pi series.'),
                 r, g_tail))
  }

  price_w = w_grid
  if (priced_as_permanent) {
    # Sunset-disbelief corner: for the PRICE path only, markets ignore the
    # legislated sunset -- the wedge continues from its last nonzero level,
    # growing with pi. Flow factors still follow the statute.
    live = which(abs(w_grid) > CORP_EPS)
    if (length(live) > 0) {
      last_live = max(live)
      if (last_live < length(grid)) {
        idx = (last_live + 1):length(grid)
        price_w[idx] = w_grid[last_live] * pi_grid[idx] / pi_grid[last_live]
      }
    }
  }
  price_rent = (1 - sigma_n) * price_w
  price_norm = sigma_n * price_w
  price_hit  = price_rent + (1 - kappa) * (1 - eta) * price_norm

  n = length(grid)
  M = numeric(n)
  V = numeric(n)
  # Gordon terminals: value of the growing tail beyond the grid.
  M[n] = price_hit[n] * (1 + g_tail) / (r - g_tail)
  V[n] = pi_grid[n]   * (1 + g_tail) / (r - g_tail)
  for (i in (n - 1):1) {
    M[i] = (price_hit[i + 1] + M[i + 1]) / (1 + r)
    V[i] = (pi_grid[i + 1]   + V[i + 1]) / (1 + r)
  }
  # Enactment-year surprise (D7): no pre-announcement capitalization.
  M[grid < t0] = 0

  mu = if_else(V > CORP_EPS, M / V, 0)
  if (any(abs(mu) > CORP_MU_MAX, na.rm = TRUE)) {
    bad = grid[which.max(abs(mu))]
    stop(sprintf(paste0('corp_incidence: |mu| = %.3f at year %d exceeds the ',
                        'sanity bound %.2f. The naive ceiling for a 21->28 ',
                        'hike is ~0.09; a markdown this large means ',
                        'mis-scaled inputs (units? wrong baseline OME leg?), ',
                        'not policy.'),
                 max(abs(mu)), bad, CORP_MU_MAX))
  }

  # --- noncorporate allocation ----------------------------------------------
  base_int  = macro$gdp_interest    [match(grid, macro$year)]
  base_rent = macro$gdp_rent        [match(grid, macro$year)]
  base_pt   = macro$gdp_proprietors [match(grid, macro$year)] * pt_weight

  N = (1 - kappa) * eta * w_norm
  need_bases = abs(N) > CORP_EPS
  if (any(need_bases & (!is.finite(base_int) | !is.finite(base_rent) |
                        !is.finite(base_pt)))) {
    # Only sim years matter for record factors; the PV tail never uses bases.
    in_sim = need_bases & grid %in% sim_years
    if (any(in_sim & (!is.finite(base_int) | !is.finite(base_rent) |
                      !is.finite(base_pt)))) {
      bad = grid[in_sim & (!is.finite(base_int) | !is.finite(base_rent) |
                           !is.finite(base_pt))][1]
      stop('corp_incidence: Macro-Projections gdp_interest/gdp_rent/',
           'gdp_proprietors unavailable for sim year ', bad, '.')
    }
  }
  denom   = base_int + base_rent + base_pt
  sh_int  = if_else(is.finite(denom) & denom > 0, base_int  / denom, 0)
  sh_rent = if_else(is.finite(denom) & denom > 0, base_rent / denom, 0)
  sh_pt   = if_else(is.finite(denom) & denom > 0, base_pt   / denom, 0)

  H_int_pot = N * sh_int
  H_int     = H_int_pot * roll        # delivered as flow (rolled paper only)
  drho_int  = H_int_pot * (1 - roll)  # named delta-rho revaluation line
  H_rent    = N * sh_rent
  H_pt      = N * sh_pt

  # --- record-applier factors ------------------------------------------------
  fac_div  = 1 + omega_div * phi
  g_int    = if_else(is.finite(base_int)  & base_int  > 0, H_int  / base_int,  0)
  g_rent   = if_else(is.finite(base_rent) & base_rent > 0, H_rent / base_rent, 0)
  g_ptcap  = if_else(is.finite(base_pt)   & base_pt   > 0, H_pt   / base_pt,   0)
  fac_int  = 1 - g_int
  fac_rent = 1 - g_rent
  fac_pt   = 1 - pt_weight * g_ptcap

  facs = c(fac_div, fac_int, fac_rent, fac_pt)
  if (any(!is.finite(facs)) || any(facs < 0)) {
    stop('corp_incidence: a record scaling factor is non-finite or negative ',
         '(min = ', min(facs), '). The wedge is implausibly large relative ',
         'to the aggregate income bases; refusing to clamp silently.')
  }

  omega_dc = unname(CORP_ASSET_EXPOSURE['value.dc'])
  by_year = tibble(
    year = grid, w = w_grid, pi_at = pi_grid, eta = eta, roll = roll,
    w_rent = w_rent, w_norm = w_norm, h_c = h_c, phi = phi,
    price_hit = price_hit, M = M, V = V, mu = mu,
    N_noncorp = N, H_int = H_int, drho_int = drho_int,
    H_rent = H_rent, H_pt = H_pt,
    fac_div = fac_div, fac_int = fac_int, fac_rent = fac_rent,
    g_ptcap = g_ptcap, fac_pt = fac_pt,
    mu_ret = omega_dc * mu
  )

  list(by_year = by_year,
       sim     = by_year %>% filter(year %in% sim_years),
       r       = r,
       t0      = t0,
       g_tail  = g_tail,
       knobs   = list(sigma_n = sigma_n, kappa = kappa, theta = theta,
                      omega_div = omega_div, pt_weight = pt_weight,
                      beyond_horizon = beyond_horizon,
                      priced_as_permanent = priced_as_permanent))
}



corp_assert_paths = function(paths) {

  #----------------------------------------------------------------------------
  # Model-internal hard invariants on a built path set (Invariant 2 of the
  # conservation section; runs on every resolve, cheap):
  #
  #  1. Markdown telescoping: M_t (1 + r) = price_hit_{t+1} + M_{t+1} on every
  #     interior grid year (P4) -- plus an INDEPENDENT direct-PV-sum spot
  #     check from several live years, so a vector-alignment bug in the
  #     recursion cannot self-certify.
  #  2. Inertness before enactment: mu = 0 and all factors = 1 for t < t0.
  # (The behavioral properties -- permanent-constant mu, rent-share floor
  # decay, windowed expiry -- are asserted on synthetic inputs by
  # corp_selfcheck_paths, where the truth values are known in closed form.)
  #
  # Stops on violation; returns invisibly TRUE.
  #----------------------------------------------------------------------------

  b = paths$by_year
  r = paths$r
  n = nrow(b)

  # (1) telescoping, interior years
  lhs = b$M[-n] * (1 + r)
  rhs = b$price_hit[-1] + b$M[-1]
  live = b$year[-n] >= paths$t0
  if (!isTRUE(all.equal(lhs[live], rhs[live], tolerance = 1e-8))) {
    stop('corp_incidence: markdown telescoping M_t(1+r) = hit_{t+1} + M_{t+1} ',
         'FAILED (max abs dev ', max(abs(lhs[live] - rhs[live])), '). ',
         'Internal path-construction bug.')
  }
  # direct-sum spot check from up to 5 evenly spaced live years
  live_idx = which(b$year >= paths$t0 & b$year < max(b$year))
  if (length(live_idx) > 0) {
    for (i in unique(round(seq(min(live_idx), max(live_idx), length.out = 5)))) {
      s = (i + 1):n
      direct = sum(b$price_hit[s] / (1 + r)^(s - i)) +
               b$M[n] / (1 + r)^(n - i)
      if (!isTRUE(all.equal(direct, b$M[i],
                            tolerance = 1e-8, scale = max(1, abs(direct))))) {
        stop('corp_incidence: markdown backward recursion disagrees with the ',
             'direct PV sum at year ', b$year[i], ' (', b$M[i], ' vs ', direct,
             '). Internal path-construction bug.')
      }
    }
  }

  # (2) pre-enactment inertness
  pre = b$year < paths$t0
  if (any(pre)) {
    inert = all(abs(b$mu[pre]) < 1e-12) &&
            all(abs(b$fac_div[pre] - 1) < 1e-12) &&
            all(abs(b$fac_int[pre] - 1) < 1e-12) &&
            all(abs(b$fac_rent[pre] - 1) < 1e-12) &&
            all(abs(b$fac_pt[pre] - 1) < 1e-12)
    if (!inert) {
      stop('corp_incidence: pre-enactment years are not inert (mu or a flow ',
           'factor differs from its identity value before ', paths$t0, ').')
    }
  }

  invisible(TRUE)
}



corp_resolve_paths = function(scenario_info) {

  #----------------------------------------------------------------------------
  # File-backed wrapper: reads the wedge + metadata (from the memoized gate
  # entry when available), the Macro-Projections aggregates, and the debt
  # rollover schedule; resolves the env sweep knobs; builds and hard-checks
  # the paths. Deterministic and cheap -- callable from any worker with no
  # serialized state (the design's no-new-SLURM-phase property).
  #
  # Returns: the corp_build_paths_core() list, plus $meta and $wedge.
  #----------------------------------------------------------------------------

  gate_key = paste0('gate|', scenario_info$ID)
  gate = .corp_cache[[gate_key]]
  if (is.null(gate)) {
    if (!scenario_uses_corp_incidence(scenario_info)) {
      stop('corp_incidence: corp_resolve_paths called for scenario "',
           scenario_info$ID, '" but the channel is not active for it.')
    }
    gate = .corp_cache[[gate_key]]
  }
  if (!isTRUE(gate$active)) {
    stop('corp_incidence: corp_resolve_paths called for scenario "',
         scenario_info$ID, '" but the channel is not active for it.')
  }

  knobs = corp_env_knobs()
  paths = corp_build_paths_core(
    wedge               = gate$wedge,
    macro               = corp_read_macro(scenario_info),
    sim_years           = scenario_info$years,
    beyond_horizon      = gate$meta$beyond_horizon,
    sigma_n             = knobs$sigma_n,
    kappa               = knobs$kappa,
    priced_as_permanent = knobs$priced_as_permanent,
    roll_fn             = corp_rollover_ramp(),
    pt_weight           = WEALTH_CAP_FLOWS_PT_WEIGHT
  )
  corp_assert_paths(paths)

  # Optional WARN-level cross-check: w_t ~= Delta-tau_t * Pi_t when the
  # metadata declares the legislated rate path. Pi = pi / (1 - tau) needs the
  # baseline rate; both must be supplied to check.
  if (!is.null(gate$meta$delta_tau) && !is.null(gate$meta$tau_baseline)) {
    dt   = gate$meta$delta_tau
    tau0 = as.numeric(gate$meta$tau_baseline)
    yrs  = intersect(as.integer(names(dt)), paths$sim$year)
    if (length(yrs) > 0 && is.finite(tau0) && tau0 > 0 && tau0 < 1) {
      row = paths$sim[match(yrs, paths$sim$year), ]
      implied = as.numeric(dt[as.character(yrs)]) * row$pi_at / (1 - tau0)
      ratio = ifelse(abs(implied) > CORP_EPS, row$w / implied, NA)
      off = is.finite(ratio) & (ratio < 0.5 | ratio > 2)
      if (any(off)) {
        warning('corp_incidence: wedge vs legislated-rate cross-check is off ',
                'by more than 2x in year(s) ',
                paste(yrs[off], collapse = ', '),
                ' (w / [Delta-tau * Pi] = ',
                paste(sprintf('%.2f', ratio[off]), collapse = ', '),
                '). The receipts path may not be a clean rate-change proxy, ',
                'or theta/US-taxable scaling differs from the declaration.',
                call. = FALSE)
      }
    }
  }

  paths$meta  = gate$meta
  paths$wedge = gate$wedge
  paths
}



corp_get_paths = function(scenario_info) {

  # Memoized corp_resolve_paths (per process, keyed by scenario ID). Env knobs
  # are process-constant, so caching across years/passes is safe.
  key = paste0('paths|', scenario_info$ID)
  hit = .corp_cache[[key]]
  if (!is.null(hit)) return(hit)
  paths = corp_resolve_paths(scenario_info)
  .corp_cache[[key]] = paths
  paths
}



#-------------------------------------------------------------------------------
# Record applier (built-in conventional-pass step, run_one_year)
#-------------------------------------------------------------------------------

corp_apply_to_records = function(tax_units, paths, year,
                                 kg_dynamics_active = FALSE) {

  #----------------------------------------------------------------------------
  # Applies the year's corporate shock to the record frame, at the head of a
  # conventional-side pass (BEFORE the wealth haircut applier and the behavior
  # modules -- FORMAL_MODEL section 7; static side never sees this, D5).
  #
  # Flows (the D16 IN-list; every applied dollar accumulates analytically into
  # the detail column corp_dY_exog, NEVER by differencing files):
  #   - div_ord/div_pref            x fac_div  = 1 + omega_div * phi_t
  #   - txbl_int/exempt_int         x fac_int  (rollover-ramped)
  #   - rent/rent_loss (net pair)   x fac_rent
  #   - pass-through lines + SE companions (WEALTH_CAP_FLOWS_PT[_WEIGHT])
  #                                 x fac_pt   = 1 - 0.2 * g_ptcap
  #
  # Stocks: exposed value.* columns x (1 - omega_a * mu_t) -- column-specific,
  # NOT the wealth channel's uniform (1 - f) (different design goal: the
  # haircut must keep s_pt/rho_pt invariant; the markdown is an equity-price
  # event). net_worth is recomputed from the marked-down balance sheet (debts
  # untouched) so calc_wealth reprices liab_wealth and calc_estate the estate
  # base. BASIS NEVER SCALES (P5).
  #
  # Gains (D18, one rule, two entry points):
  #   - non-kg runs (kg_dynamics_active = FALSE): the exact per-record form
  #       kg_lt'       = kg_lt + omega_kg * [phi_t * kg_lt
  #                                          - mu_t * max(kg_lt + kg_lt_basis, 0)]
  #       kg_lt_basis' = kg_lt_basis * (1 + omega_kg * phi_t)
  #       kg_st'       = kg_st * (1 + omega_kg * phi_t)
  #     (quantity margin phi co-scales basis -- fewer buyback-forced sales;
  #      the price margin mu hits the SALE VALUE, basis fixed).
  #   - kg runs (kg_dynamics_active = TRUE): kg columns are NOT touched here.
  #     The price margin enters as the bathtub gain-state debit and the phi
  #     quantity term is applied AFTER kg_dyn_apply_to_records in
  #     run_one_year -- applying either here too would double-count.
  #   kg deltas stay OUT of corp_dY_exog (internal conversions, P9).
  #
  # Retirement (P7 two-pocket lemma; OUT of corp_dY_exog). P7 as stated:
  # every cash flow sourced from a MARKED-DOWN stock must scale with the
  # markdown -- so the scaling conditions on the record's OBSERVED source
  # balance (a distribution with no marked-down balance behind it gets no
  # phantom cut; the markdown is proportional, so balance SIZE never matters,
  # only the source mix):
  #   - txbl_ira_dist x (1 - omega_dc * mu_t * 1{value.dc > 0}) -- IRA/DC
  #     draws are definitionally dc-type; scale iff a dc balance exists;
  #   - txbl_pens_dist/gross_pens_dist x (1 - omega_dc * mu_t * dc_share_i),
  #     dc_share_i = value.dc / (value.dc + value.db) on the PRE-markdown
  #     balance sheet: DB-sourced pensions are defined benefits whose balance
  #     is never debited (D10), so scaling them would create a phantom income
  #     cut with no booked resource loss (the reverse P7 violation).
  #
  # Diagnostics (conventional detail only): corp_dY_exog (per-record UNWEIGHTED
  # dollars, negative for a hike), corp_markdown (record-effective markdown
  # fraction of gross assets), corp_flow_factor (phi_t).
  #
  # Pre-enactment years return the frame UNTOUCHED (byte-exact dormancy).
  #
  # Parameters:
  #   - tax_units (df)       : conventional-pass base frame (pre-behavior)
  #   - paths (list)         : corp_get_paths(scenario_info)
  #   - year (int)           : simulation year
  #   - kg_dynamics_active   : TRUE when the scenario runs kg_dynamics (the
  #                            gain adjustments then route through the bathtub)
  #
  # Returns: transformed tax_units (+ diagnostic columns).
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) {
    stop('corp_incidence: no path row for year ', year,
         ' (sim paths cover ', min(paths$sim$year), ':', max(paths$sim$year), ').')
  }
  p = paths$sim[i, ]

  # Byte-exact dormancy before enactment (and for any inert year).
  inert = abs(p$mu) < CORP_EPS &&
          abs(p$fac_div - 1) < CORP_EPS && abs(p$fac_int - 1) < CORP_EPS &&
          abs(p$fac_rent - 1) < CORP_EPS && abs(p$fac_pt - 1) < CORP_EPS &&
          abs(p$phi) < CORP_EPS
  if (inert) return(tax_units)

  g = function(col) wealth_dyn_safe_col(tax_units, col)

  # --- everything reads PRE values first --------------------------------------
  # Analytic dY_exog from the applied scalings (D16 rider). The pass-through
  # net mirrors wealth_dyn_capital_total's f_pt_net (income legs only; the SE
  # companions are payroll-base bookkeeping, not cash income).
  pt_net = g('sole_prop') +
           (g('part_active') + g('part_passive') -
            g('part_active_loss') - g('part_passive_loss') - g('part_179')) +
           (g('scorp_active') + g('scorp_passive') -
            g('scorp_active_loss') - g('scorp_passive_loss') - g('scorp_179')) +
           g('farm')

  dY_exog = (p$fac_div  - 1) * (g('div_ord') + g('div_pref')) +
            (p$fac_int  - 1) * (g('txbl_int') + g('exempt_int')) +
            (p$fac_rent - 1) * (g('rent') - g('rent_loss')) +
            (p$fac_pt   - 1) * pt_net

  # Record-effective markdown (diagnostic) and the retirement source split,
  # both on the PRE-markdown balance sheet.
  exposure_cols = intersect(names(CORP_ASSET_EXPOSURE), names(tax_units))
  markdown_amt  = rep(0, nrow(tax_units))
  for (a in exposure_cols) {
    markdown_amt = markdown_amt + CORP_ASSET_EXPOSURE[[a]] * p$mu * g(a)
  }
  gross_pre = wealth_dyn_economic_gross(tax_units)

  omega_dc = unname(CORP_ASSET_EXPOSURE['value.dc'])
  dc  = g('value.dc')
  db  = g('value.db')
  dc_share = if_else(dc + db > CORP_EPS, dc / (dc + db), 0)
  fac_ira  = 1 - omega_dc * p$mu * as.numeric(dc > CORP_EPS)
  fac_pens = 1 - omega_dc * p$mu * dc_share

  # kg adjustments (non-kg runs only; see docstring)
  omega_kg = CORP_OMEGA_KG
  kg_quantity_fac = 1 + omega_kg * p$phi
  kg_lt_delta = omega_kg * (p$phi * g('kg_lt') -
                            p$mu * pmax(g('kg_lt') + g('kg_lt_basis'), 0))

  # --- column lists (intersect for robustness, wealth-applier style) ----------
  div_cols  = intersect(CORP_FLOWS_DIV,  names(tax_units))
  int_cols  = intersect(CORP_FLOWS_INT,  names(tax_units))
  rent_cols = intersect(CORP_FLOWS_RENT, names(tax_units))
  pt_cols   = intersect(c(WEALTH_CAP_FLOWS_PT, WEALTH_CAP_FLOWS_SE_COMPANIONS),
                        names(tax_units))
  ira_cols  = intersect('txbl_ira_dist', names(tax_units))
  pens_cols = intersect(c('txbl_pens_dist', 'gross_pens_dist'), names(tax_units))
  asset_cols = intersect(ESTATE_ASSET_COLS, names(tax_units))

  # Debts untouched: compute once from the original frame.
  debts = rowSums(cols_matrix(tax_units, WEALTH_DEBT_COLS))

  out = tax_units %>%
    mutate(
      across(all_of(div_cols),  ~ . * p$fac_div),
      across(all_of(int_cols),  ~ . * p$fac_int),
      across(all_of(rent_cols), ~ . * p$fac_rent),
      across(all_of(pt_cols),   ~ . * p$fac_pt),
      across(all_of(ira_cols),  ~ . * fac_ira),
      across(all_of(pens_cols), ~ . * fac_pens))

  # Exposed stocks: column-specific markdown.
  for (a in exposure_cols) {
    out[[a]] = out[[a]] * (1 - CORP_ASSET_EXPOSURE[[a]] * p$mu)
  }

  # Gains (non-kg runs).
  if (!kg_dynamics_active) {
    if ('kg_lt' %in% names(out))       out$kg_lt = out$kg_lt + kg_lt_delta
    if ('kg_lt_basis' %in% names(out)) out$kg_lt_basis = out$kg_lt_basis * kg_quantity_fac
    if ('kg_st' %in% names(out))       out$kg_st = out$kg_st * kg_quantity_fac
  }

  out %>%
    mutate(
      # Recompute the stored net-worth stock from the marked-down balance
      # sheet (same recipe as wealth_dyn_apply_to_records / run_one_year), so
      # calc_wealth and calc_estate reprice on the post-markdown stock.
      net_worth = rowSums(across(all_of(asset_cols), ~ replace_na(., 0))) - debts,
      corp_dY_exog     = dY_exog,
      corp_markdown    = if_else(gross_pre > CORP_EPS, markdown_amt / gross_pre, 0),
      corp_flow_factor = p$phi)
}



#-------------------------------------------------------------------------------
# Conservation diagnostic (FORMAL_MODEL section 4, as amended by the external
# review: the three-way identity w = B_flow + B_accr + B_res is a REPORT --
# B_res is residually defined -- while the TESTABLE content is the per-line
# analytic-intended vs record-realized reconciliation. WARN-level; promote to
# hard-error only after the permanent + windowed test scenarios pin sign
# behavior and tolerances. Invariant 2 (markdown telescoping) is already a
# hard assert inside corp_resolve_paths.)
#-------------------------------------------------------------------------------

corp_write_conservation_diag = function(pre, post, paths, year, conv_root) {

  #----------------------------------------------------------------------------
  # Writes conventional/supplemental/corp_conservation_diag_{t}.csv (estate-
  # allocator-diag precedent) for one CY year, from the pre- and post-applier
  # frames (row-aligned; final conventional pass only).
  #
  # Columns:
  #   - inputs/paths: w, phi, mu, eta, roll, sigma_n/kappa/theta_res knobs
  #   - per-line reconciliation ($B): dY_{div,int,rent,pt}_realized (measured
  #     by differencing the frames -- INDEPENDENT of the applier's analytic
  #     accumulation) vs their sum dY_total_analytic (= sum w*corp_dY_exog,
  #     the applier's accumulated column). A gap flags a weights bug, a
  #     missed/overwritten line, or clamping. WARN beyond tolerance.
  #   - B_flow_hh = -sum(w * corp_dY_exog): the household external-income
  #     burden flow (positive under a hike);
  #   - markdown_position_hh = sum(w * per-record markdown dollars): the
  #     household PV markdown POSITION at t (a stock; its year-over-year
  #     movement is the B_accr flow -- difference the per-year files, or see
  #     the bathtub state's corp_gain_debit for the kg slice);
  #   - B_res_theta = theta_res * w (D3/D10 foreign/nonprofit/DB slice) and
  #     drho_int (the named delta-rho revaluation line: undelivered unrolled-
  #     interest compression, D15/P14);
  #   - residual_unallocated = w - B_flow_hh - B_res_theta - drho_int: the
  #     honest unallocated remainder REPORT (no gross-up forces this to zero;
  #     it also absorbs the accrual flow not measured here).
  #
  # Returns: invisibly the one-row diag tibble.
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) return(invisible(NULL))
  p = paths$sim[i, ]

  w8   = pre$weight
  toB  = 1e-9
  line = function(cols_pos, cols_neg = character(0)) {
    d = rep(0, nrow(pre))
    for (cc in intersect(cols_pos, names(pre))) {
      d = d + (replace_na(post[[cc]], 0) - replace_na(pre[[cc]], 0))
    }
    for (cc in intersect(cols_neg, names(pre))) {
      d = d - (replace_na(post[[cc]], 0) - replace_na(pre[[cc]], 0))
    }
    sum(w8 * d) * toB
  }

  pt_cols_pos = c('sole_prop', 'part_active', 'part_passive',
                  'scorp_active', 'scorp_passive', 'farm')
  pt_cols_neg = c('part_active_loss', 'part_passive_loss', 'part_179',
                  'scorp_active_loss', 'scorp_passive_loss', 'scorp_179')

  dY_div_realized  = line(CORP_FLOWS_DIV)
  dY_int_realized  = line(CORP_FLOWS_INT)
  dY_rent_realized = line('rent', 'rent_loss')
  dY_pt_realized   = line(pt_cols_pos, pt_cols_neg)
  dY_total_realized = dY_div_realized + dY_int_realized +
                      dY_rent_realized + dY_pt_realized

  dY_total_analytic = sum(w8 * replace_na(post$corp_dY_exog, 0)) * toB

  # Household markdown position, re-measured from the exposed value.* deltas
  # (independent of the applier's internal markdown_amt).
  md = rep(0, nrow(pre))
  for (a in intersect(names(CORP_ASSET_EXPOSURE), names(pre))) {
    md = md + (replace_na(pre[[a]], 0) - replace_na(post[[a]], 0))
  }
  markdown_position_hh = sum(w8 * md) * toB

  knobs = paths$knobs
  diag = tibble(
    year  = year,
    w     = p$w,
    phi   = p$phi,
    mu    = p$mu,
    eta   = p$eta,
    roll  = p$roll,
    sigma_n   = knobs$sigma_n,
    kappa     = knobs$kappa,
    theta_res = CORP_THETA_RES,
    dY_div_realized   = dY_div_realized,
    dY_int_realized   = dY_int_realized,
    dY_rent_realized  = dY_rent_realized,
    dY_pt_realized    = dY_pt_realized,
    dY_total_realized = dY_total_realized,
    dY_total_analytic = dY_total_analytic,
    B_flow_hh            = -dY_total_analytic,
    markdown_position_hh = markdown_position_hh,
    B_res_theta          = CORP_THETA_RES * p$w,
    drho_int             = p$drho_int,
    residual_unallocated = p$w + dY_total_analytic -
                           CORP_THETA_RES * p$w - p$drho_int
  )

  # The testable content: analytic accumulation vs frame-measured realization.
  gap = abs(dY_total_realized - dY_total_analytic)
  if (gap > max(0.05, 0.005 * abs(dY_total_analytic))) {
    warning(sprintf(paste0(
      'corp_incidence conservation diag, year %d: record-realized external-',
      'income delta ($%.2fB) differs from the analytic corp_dY_exog ',
      'accumulation ($%.2fB) by $%.2fB. A weights bug, a missed/overwritten ',
      'line, or clamping is likely.'),
      year, dY_total_realized, dY_total_analytic, gap), call. = FALSE)
  }

  dir.create(file.path(conv_root, 'supplemental'), recursive = TRUE,
             showWarnings = FALSE)
  write_csv(diag, file.path(conv_root, 'supplemental',
                            sprintf('corp_conservation_diag_%d.csv', year)))
  invisible(diag)
}



#-------------------------------------------------------------------------------
# kg_dynamics glue (D18: one rule, two entry points)
#-------------------------------------------------------------------------------

corp_kg_state_exposed_value = function(tax_units) {

  #----------------------------------------------------------------------------
  # Per-record omega-weighted C-corp equity VALUE underlying the kg gain
  # state: only the kg asset classes with corporate exposure (the
  # CORP_ASSET_EXPOSURE names intersected with value.{KG_DYN_ASSET_CLASSES} --
  # equities and re_fund; dc/trusts are exposed assets but NOT kg classes, so
  # their markdown never enters the kg state). kg_dyn_aggregate_cells sums
  # this to cells; the corporate gain-state debit is then
  #     D_a(t) = mu_t * V_corp_exposed_a(t)
  # -- the dollar value markdown, which debits the gain state dollar-for-
  # dollar (P5: basis fixed, the gain absorbs the entire price hit).
  #
  # Returns: numeric vector, one row per record.
  #----------------------------------------------------------------------------

  kg_value_cols = intersect(names(CORP_ASSET_EXPOSURE),
                            paste0('value.', KG_DYN_ASSET_CLASSES))
  v = rep(0, nrow(tax_units))
  for (a in kg_value_cols) {
    v = v + CORP_ASSET_EXPOSURE[[a]] * wealth_dyn_safe_col(tax_units, a)
  }
  v
}



corp_kg_state_debit_by_year = function(scenario_info, baseline_cells) {

  #----------------------------------------------------------------------------
  # The per-year corporate gain-state debit vectors for the kg bathtub
  # (kg_dyn_run_bathtub_pass): for each sim year t, a vector over the bathtub
  # ages of D_a(t) = mu_t * V_corp_exposed_a(t), in gain dollars (>= 0 for a
  # hike). RECOMPUTED FROM THE CURRENT mu_t EACH YEAR, never accumulated
  # through the recurrence -- the credit-back as the markdown shrinks (P3's
  # recovery appreciation) is automatic. Returns NULL when the corporate
  # channel is not active for the scenario.
  #----------------------------------------------------------------------------

  if (scenario_info$ID == 'baseline' ||
      !scenario_uses_corp_incidence(scenario_info)) {
    return(NULL)
  }
  paths = corp_get_paths(scenario_info)

  out = list()
  for (t in scenario_info$years) {
    bt = baseline_cells[[as.character(t)]]
    if (is.null(bt) || is.null(bt$V_corp_exposed)) {
      stop('corp_incidence: baseline kg cells for year ', t, ' lack the ',
           'V_corp_exposed column. The kg cell aggregation predates the ',
           'corporate channel -- re-run the kg frozen pass (a stale ',
           'inputs_cache.rds is the usual cause).')
    }
    mu_t = paths$sim$mu[match(t, paths$sim$year)]
    if (is.na(mu_t)) mu_t = 0
    out[[as.character(t)]] = setNames(mu_t * bt$V_corp_exposed,
                                      as.character(bt$age))
  }
  out
}



corp_apply_kg_quantity_to_records = function(tax_units, paths, year) {

  #----------------------------------------------------------------------------
  # The D18 QUANTITY margin in kg_dynamics runs: buyback-forced sale volume
  # tracks after-tax payouts -- a margin the kg_dynamics realization rule
  # (which knows MTRs and mortality, not payout policy) cannot produce. Scales
  # the realization flow kg_lt / kg_st by (1 + omega_kg * phi_t) and co-scales
  # kg_lt_basis (fewer lots sold; the taxable gain ratio is preserved). The
  # PRICE margin deliberately does NOT appear here: in kg runs it enters as
  # the bathtub gain-state debit (corp_kg_state_debit_by_year), which is exact
  # because the state is gain-denominated.
  #
  # ENTRY-POINT EXCLUSIVITY: applied in run_one_year AFTER
  # kg_dyn_apply_to_records, and ONLY when the scenario runs kg_dynamics; the
  # non-kg entry point is corp_apply_to_records' kg block (skipped there via
  # kg_dynamics_active = TRUE). Applying both double-counts the phi term.
  # Deemed death gains (kg_deemed_full / kg_deemed) are left untouched: death
  # is not a buyback-driven sale.
  #
  # Returns: tax_units with scaled kg_lt / kg_st / kg_lt_basis.
  #----------------------------------------------------------------------------

  i = match(year, paths$sim$year)
  if (is.na(i)) {
    stop('corp_incidence: no path row for year ', year, ' (kg quantity term).')
  }
  p = paths$sim[i, ]
  fac = 1 + CORP_OMEGA_KG * p$phi
  if (abs(fac - 1) < CORP_EPS) return(tax_units)

  for (col in intersect(c('kg_lt', 'kg_st', 'kg_lt_basis'), names(tax_units))) {
    tax_units[[col]] = tax_units[[col]] * fac
  }
  tax_units
}



#-------------------------------------------------------------------------------
# Self-checks on synthetic inputs (callable from the test harness / sbatch
# verification; NOT run at source time)
#-------------------------------------------------------------------------------

corp_selfcheck_paths = function() {

  #----------------------------------------------------------------------------
  # Drives corp_build_paths_core with synthetic series and asserts the plan's
  # unit properties:
  #   1. PERMANENT, sigma_n = 0 (rent-only corner): mu_t constant across the
  #      live window and equal to theta * (w/pi share) -- the Delta-tau/(1-tau)
  #      equivalent (P1); dividend factor mirrors it via omega_div.
  #   2. PERMANENT, sigma_n > 0: mu_t decays MONOTONICALLY toward the
  #      rent-share floor sigma-split (P14/D14): late-horizon mu ->
  #      (1 - sigma_n) * (w/pi constant share).
  #   3. WINDOWED (zero beyond horizon): M_t = 0 (and mu_t = 0) for all years
  #      at/after expiry (P3); under priced_as_permanent the same input keeps
  #      mu > 0 through the window's end.
  #   4. Telescoping + pre-enactment inertness via corp_assert_paths.
  #
  # Returns: TRUE invisibly; stops with a message on any violation.
  #----------------------------------------------------------------------------

  years_all = 2024:2200
  pi0 = 4000; g = 0.035
  macro = tibble(
    year = years_all,
    pi_at = pi0 * (1 + g)^(years_all - min(years_all)),
    tsy_10y = 4.2,
    gdp_interest = 2000 * (1 + g)^(years_all - min(years_all)),
    gdp_rent     = 1100 * (1 + g)^(years_all - min(years_all)),
    gdp_proprietors = 2200 * (1 + g)^(years_all - min(years_all))
  )
  roll_fn = function(t_since) pmin(pmax(t_since, 0) / 10, 1)
  sim_years = 2025:2036
  w_share = 0.05   # wedge = 5% of after-tax profits, permanent

  perm_wedge = tibble(year = 2024:2040,
                      w = if_else(year >= 2026,
                                  w_share * macro$pi_at[match(year, macro$year)],
                                  0))

  # --- 1. permanent, rent-only ------------------------------------------------
  p1 = corp_build_paths_core(perm_wedge, macro, sim_years, 'extend',
                             sigma_n = 0, kappa = 0.4, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p1)
  live = p1$sim %>% filter(year >= p1$t0)
  if ((max(live$mu) - min(live$mu)) > 1e-6) {
    stop('corp_selfcheck: permanent rent-only mu is not constant (range ',
         min(live$mu), ' .. ', max(live$mu), ').')
  }
  if (abs(mean(live$mu) - w_share * CORP_THETA) > 1e-3) {
    stop('corp_selfcheck: permanent rent-only mu = ', mean(live$mu),
         ' differs from the flow share ', w_share,
         ' (the Delta-tau/(1-tau) equivalent).')
  }

  # --- 2. permanent, migrating ------------------------------------------------
  sig = 0.375; kap = 0.4
  p2 = corp_build_paths_core(perm_wedge, macro, sim_years, 'extend',
                             sigma_n = sig, kappa = kap, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p2)
  live2 = p2$sim %>% filter(year >= p2$t0)
  if (any(diff(live2$mu) > 1e-9)) {
    stop('corp_selfcheck: permanent migrating mu is not weakly decaying.')
  }
  floor_share = (1 - sig) * w_share
  # far tail: eta ~ 1 -> mu -> rent floor (+ small residual from the ramp)
  tail_mu = p2$by_year %>% filter(year == max(year)) %>% pull(mu)
  if (tail_mu < floor_share - 1e-3 || tail_mu > floor_share + 0.3 * sig * w_share) {
    stop('corp_selfcheck: far-tail mu = ', tail_mu,
         ' is not at the rent-share floor ', floor_share, '.')
  }

  # --- 3. windowed ------------------------------------------------------------
  win_wedge = tibble(year = 2024:2040,
                     w = if_else(year >= 2026 & year <= 2031,
                                 w_share * macro$pi_at[match(year, macro$year)],
                                 0))
  p3 = corp_build_paths_core(win_wedge, macro, sim_years, 'zero',
                             sigma_n = 0, kappa = kap, roll_fn = roll_fn,
                             pt_weight = 0.2)
  corp_assert_paths(p3)
  post = p3$by_year %>% filter(year >= 2031)   # M_t = PV of hits AFTER t
  if (any(abs(post$M) > 1e-6) || any(abs(post$mu) > 1e-9)) {
    stop('corp_selfcheck: windowed markdown does not vanish at expiry ',
         '(max |M| after window = ', max(abs(post$M)), ').')
  }
  p3b = corp_build_paths_core(win_wedge, macro, sim_years, 'zero',
                              sigma_n = 0, kappa = kap, roll_fn = roll_fn,
                              pt_weight = 0.2, priced_as_permanent = TRUE)
  mu_at_2030 = p3b$by_year %>% filter(year == 2030) %>% pull(mu)
  if (mu_at_2030 < 0.5 * w_share) {
    stop('corp_selfcheck: priced-as-permanent corner did not keep the ',
         'markdown alive near the window end (mu(2030) = ', mu_at_2030, ').')
  }

  message('corp_selfcheck_paths: all path-property checks passed ',
          '(permanent-constant mu, rent-share floor decay, windowed expiry, ',
          'priced-as-permanent corner, telescoping).')
  invisible(TRUE)
}

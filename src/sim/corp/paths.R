#-------------------------------------------------------------------------------
# corp/paths.R -- module overview + analytic path construction
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
# scenario overrides via assumption.corp.sigma_n / .kappa / .priced_as_permanent)
#-------------------------------------------------------------------------------

CORP_SPEC_VERSION = 1L

# --- The D16 column contract: what enters the generalized forcing ------------
#
# IN (external income; the applied dollar deltas accumulate into the per-record
# detail column `corp_dY_exog`, consumed by the wealth bathtub forcing
# F = dT - dY_exog):
#   - dividends (div_ord, div_pref), at the corp.omega_div exposure
#   - interest (txbl_int, exempt_int), on the debt rollover ramp
#   - rent (rent, rent_loss -- the NET pair scales together)
#   - pass-through lines at the 0.2 capital weight; the column list is
#     WEALTH_CAP_FLOWS_PT + WEALTH_CAP_FLOWS_SE_COMPANIONS and the weight is
#     wealth.cap_flows_pt_weight (src/sim/wealth_dynamics.R) -- referenced at
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
# pending external measurement.
# Values and provenance: config/assumptions/corp.yaml (corp.asset_exposure_*),
# assembled by corp_asset_exposure().
corp_asset_exposure = function() {
  c('value.equities' = assumption('corp', 'asset_exposure_equities'),
    'value.dc'       = assumption('corp', 'asset_exposure_dc'),
    'value.trusts'   = assumption('corp', 'asset_exposure_trusts'),
    'value.re_fund'  = assumption('corp', 'asset_exposure_re_fund'))
}

# C-corp share of dividends: config/assumptions/corp.yaml (corp.omega_div).

# C-corp equity share of realized LTCG (stock + fund shares vs pass-through
# sales / real estate / other). PLACEHOLDER ~0.5 prior pending SOI
# sale-of-capital-assets measurement: config/assumptions/corp.yaml (corp.omega_kg).

# Normal-return share sigma_N of the corporate wedge ("taxes on margins get
# shifted; taxes on rents get capitalized", D14/D15). Central 0.375 from OTA
# 63% / TPC 60% supernormal; corners {0, 0.5} (house VAT convention = upper).
# Value and provenance: config/assumptions/corp.yaml (corp.sigma_n).

# kappa: C-corp share of the economy-wide normal-capital stock (D15). The
# migrated normal burden splits (1-kappa) to noncorporate lines and kappa
# retained on corporate flows. PLACEHOLDER 0.40 prior pending the Fed Z.1 pull;
# the owner-occupied-housing definitional fork sets the sweep corners
# {~0.25, ~0.4, ~0.5}. Value and provenance: config/assumptions/corp.yaml
# (corp.kappa).

# theta: US-taxable exposure scale on the flow factor phi = -theta * h_c / pi.
# Absorbs the NIPA-economic vs US-taxable profit wedge (Phase 0c). PLACEHOLDER
# 1.0 (pro-rata: every distribution scales by the aggregate after-tax-profit
# hit share) pending Rosenthal-Austin / Z.1 measurement.
# Value and provenance: config/assumptions/corp.yaml (corp.theta).

# theta_res: foreign / nonprofit / DB residual share of the wedge, used ONLY by
# the conservation diagnostic's B_res line (D3/D10 -- the honest unallocated
# remainder; no gross-up forces household hits to sum to the revenue line).
# PLACEHOLDER 0.40 (Rosenthal-Austin: ~26% foreign + nonprofits/insurers + the
# DB slice) pending Phase 0c measurement.
# Value and provenance: config/assumptions/corp.yaml (corp.theta_res).

# Vintaging: NIPA economic depreciation rate; the reallocation clock IS the
# replacement clock (D14), same 0.057 as do_capital_adjustment
# (src/data/economy.R). eta(t) = 1 - (1 - 0.057)^(t - t0).
# Value and provenance: config/assumptions/corp.yaml (corp.delta_nipa).

# Equity discount rate r = nominal tsy_10y (Macro-Projections, enactment year)
# + this fixed equity risk premium. Distributions are nominal, so r is nominal
# (the house Fisher-deflation convention applies to real-utility discounting,
# not nominal-flow PV -- plan note). mu is r-free in the permanent central
# case; r shapes temporary-shock annuities and the migration PV only.
# Value and provenance: config/assumptions/corp.yaml (corp.equity_premium).

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
  # rate portion of the OME `corporate` wedge;
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



corp_check_run_compat = function(scenario_info, vat_price_offset) {

  #----------------------------------------------------------------------------
  # Refusal gate for an ACTIVE corporate channel. The paths are formed from
  # raw-dollar national aggregates and the record hits land on raw-dollar
  # flows/stocks; the conservation diagnostic additionally assumes
  # full-population aggregates. No channel-specific conditions beyond the
  # shared raw-dollar guard.
  #
  # Returns: invisibly TRUE; stops on violation.
  #----------------------------------------------------------------------------

  check_raw_data_channel_compat('corp_incidence', scenario_info,
                                vat_price_offset)
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
  # Sweep corners are now scenario assumptions rather than env knobs: override
  # assumption.corp.sigma_n / .kappa / .priced_as_permanent in the runscript, so
  # the corner is recorded in the vintage's assumptions.csv instead of vanishing
  # with the shell that launched the run.
  # Returns list(sigma_n, kappa, priced_as_permanent).
  #----------------------------------------------------------------------------

  read_num = function(name, lo, hi) {
    x = suppressWarnings(as.numeric(assumption('corp', name)))
    if (!is.finite(x) || x < lo || x > hi) {
      stop('corp_incidence: assumption corp.', name, ' = "', x,
           '" is not a number in [', lo, ', ', hi, '].')
    }
    x
  }

  list(
    sigma_n = read_num('sigma_n', 0, 1),
    kappa   = read_num('kappa',   0, 1),
    priced_as_permanent = isTRUE(as.logical(assumption('corp', 'priced_as_permanent')))
  )
}



corp_build_paths_core = function(wedge, macro, sim_years, beyond_horizon,
                                 sigma_n, kappa, theta = assumption('corp', 'theta'),
                                 omega_div = assumption('corp', 'omega_div'),
                                 delta_nipa = assumption('corp', 'delta_nipa'),
                                 erp = assumption('corp', 'equity_premium'),
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
  #   pt_weight: pass-through capital weight (wealth.cap_flows_pt_weight at
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

  if (is.null(pt_weight)) pt_weight = wealth_cap_flows_pt_weight()
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
                        'corp.equity_premium / the pi series.'),
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

  omega_dc = unname(corp_asset_exposure()['value.dc'])
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
    pt_weight           = wealth_cap_flows_pt_weight()
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




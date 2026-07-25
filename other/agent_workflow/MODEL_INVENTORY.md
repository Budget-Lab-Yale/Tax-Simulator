# What the model is currently made of

**GENERATED FILE — do not hand-edit.** Rebuild with
`python3 other/agent_workflow/build_inventory.py` after changing anything
under `config/assumptions/`. The YAML is the source of truth; this page is
a rendering of it.

Every fixed economic number the model relies on, and the input data
versions they were set against. Machinery: `src/misc/assumptions.R`;
conventions: the "Model Assumptions" section of CLAUDE.md.

**45 assumptions** across 7 channels: 5 calibrated, 9 sourced, 18 judgment, 13 structural.

## Input data versions

| Interface | Version | Default vintage |
|---|---|---|
| Tax-Data | 1 | `2026070814` |
| Macro-Projections | 3 | `2026022522` |
| Value-Added-Tax-Model | 1 | `2024050121` |
| Off-Model-Estimates | 5 | `20260722` |
| Cost-Recovery-Simulator | 1 | `202509251222` |
| Estate-Tax-Distribution | 1 | `2025092512` |

## What each kind means

- **calibrated** (5) — Output of a procedure, so it can go stale. Checked on every run against the data vintages it was derived under and the content of the files listed as invalidating it. A mismatch stops the run.
- **sourced** (9) — Taken from a paper or an agency convention. Cannot go stale; can be superseded by better evidence.
- **judgment** (18) — Somebody chose it. No derivation recorded. These are the entries worth ranking for sensitivity analysis.
- **structural** (13) — A model-form switch or a conditioning rule rather than a measured magnitude.

## corp

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `corp.sigma_n` | `0.375` | sourced | — | Normal-return share of the corporate wedge (D14/D15: taxes on margins get shifted, taxes on rents get capitalized). Central 0.375 implied by OTA's 63% and TPC's 60% supernormal shares. Sweep corners… |
| `corp.delta_nipa` | `0.057` | sourced | — | NIPA economic depreciation rate. The reallocation clock IS the replacement clock (D14), so eta(t) = 1 - (1 - 0.057)^(t - t0). Same 0.057 used by do_capital_adjustment in src/data/economy.R -- change… |
| `corp.rate_eti` | `0.367` | sourced | — | Corporate AVOIDANCE-only semi-elasticity from Coles, Patel, Seegert and Smith (2022, JAR 60(3)). Deliberately NOT the effective-rate CETI (0.91 = 8.93/9.76) and NOT the total statutory elasticity (0.… |
| `corp.omega_div` | `0.85` | judgment | — | C-corp share of dividends, excluding REIT and bond-fund distributions. PLACEHOLDER pending ICI/SOI measurement (PHASE0_NOTES.md). |
| `corp.omega_kg` | `0.5` | judgment | — | C-corp equity share of realized long-term capital gains (stock and fund shares, versus pass-through sales, real estate and other). PLACEHOLDER ~0.5 prior pending SOI sale-of-capital-assets measuremen… |
| `corp.kappa` | `0.4` | judgment | — | C-corp share of the economy-wide normal-capital stock (D15). The migrated normal burden splits (1 - kappa) to noncorporate lines and retains kappa on corporate flows. PLACEHOLDER 0.40 prior pending t… |
| `corp.theta` | `1.0` | judgment | — | US-taxable exposure scale on the flow factor phi = -theta * h_c / pi. Absorbs the NIPA-economic versus US-taxable profit wedge. PLACEHOLDER 1.0 (pro-rata: every distribution scales by the aggregate a… |
| `corp.theta_res` | `0.4` | judgment | — | Foreign / nonprofit / DB residual share of the wedge. Used ONLY by the conservation diagnostic's B_res line (D3/D10 -- the honest unallocated remainder; nothing grosses up to force household hits to… |
| `corp.equity_premium` | `0.05` | judgment | — | Fixed equity risk premium added to nominal tsy_10y (Macro-Projections, enactment year) to form the equity discount rate. Distributions are nominal, so r is nominal -- the house Fisher-deflation conve… |
| `corp.asset_exposure_equities` | `1.0` | judgment | — | Corporate-equity exposure of value.equities, used for the balance-sheet markdown. PLACEHOLDER pending external measurement. |
| `corp.asset_exposure_dc` | `0.55` | judgment | — | Corporate-equity exposure of value.dc. PLACEHOLDER pending external measurement. |
| `corp.asset_exposure_trusts` | `0.5` | judgment | — | Corporate-equity exposure of value.trusts. PLACEHOLDER pending external measurement. |
| `corp.asset_exposure_re_fund` | `0.3` | judgment | — | Corporate-equity exposure of value.re_fund. PLACEHOLDER pending external measurement. |
| `corp.priced_as_permanent` | `False` | structural | — | Sweep corner: when true, a temporary wedge is priced by the equity market as if permanent. Default false (the market sees the sunset). |

## distribution

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `distribution.corp_foreign_share` | `0.4` | sourced | — | Foreign-borne share of the corporate burden, excluded from the distribution tables under the JCT convention (JCX-14-13 excludes 20.6% as foreign-borne; CBO, OTA and TPC instead allocate 100% to US ho… |
| `distribution.housing_structure_share` | `0.7` | judgment | — | Structure share of owner-occupied housing. Under Harberger the mobile-capital margin is reproducible STRUCTURES, not residential LAND (a fixed factor whose incidence runs through capitalization, not… |

## estate

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `estate.report_eps` | `0.16` | sourced | — | Kopczuk and Slemrod, "Dying to Save Taxes": elasticity of the REPORTED estate with respect to the net-of-estate-tax rate, central ~0.16 (pooled estimates 0.10-0.22). Applied as retained = ((1 - tau_S… |

## evasion

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `evasion.e_schc` | `0.046` | sourced | — | DeBacker, Heim and Yuskavage (NTA 2025), pooled cross-section, sole-proprietor subsample, federal MTR. Applies to Schedule C/F (sole prop and farm). Alternative anchors for sweeps, not centrals: the… |
| `evasion.e_pt` | `0.052` | sourced | — | DeBacker, Heim and Yuskavage (NTA 2025), pooled cross-section, partnership subsample, federal MTR. Applies to partnership and S-corp income. |
| `evasion.e_rent` | `0.04` | sourced | — | DeBacker, Heim and Yuskavage, Kansas DiD, Schedule E excluding pass-through. Not statistically significant -- the weakest-identified value in this file. |
| `evasion.topend_mult` | `1` | structural | — | Underdetection multiplier applied to every elasticity above. NRP random audits underdetect sophisticated top-end evasion (offshore structures, tiered partnerships -- Guyton, Langetieg, Reck, Risch an… |

## kg

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `kg.eta` | `2.4825` | calibrated | 2026-07-12 | Long-run realization semi-elasticity. Pinned on the FULL SIMULATOR: the eta_dial grid measures E_full(eta) = dlog(R)/dtau at sim-year 30 across a few eta values and inverts the linear-through-origin… <br> _derived under: tax_data `2026070814`, macro_projections `2026022522`_ <br> _re-derive: `other/top_tax/eta_dial/measure_efull_by_eta.R`_ |
| `kg.eta_logs` | `1.6625` | calibrated | 2026-07-19 | Constant net-of-tax elasticity dlog(r_D)/dlog(1 - MC). Same protocol and same local moment as the levels eta. Internal long-run semi at the shipped value: -2.2393. <br> _derived under: tax_data `2026070814`, macro_projections `2026022522`_ <br> _re-derive: `other/top_tax/eta_dial/measure_efull_by_eta.R`_ |
| `kg.timeable_share` | `0.2542` | calibrated | 2026-07-09 | Short-run announcement moment: full-sim short-run semi-elasticity of +5.04 (twice the long-run magnitude, sign flipped -- future-tax-up implies realize-today), measured at the announcement year under… <br> _derived under: tax_data `2026050315`, macro_projections `2026022522`_ <br> _re-derive: `other/kg_model_tests/calibrate.R`_ |
| `kg.timeable_share_logs` | `0.2542` | calibrated | 2026-07-19 | Same short-run announcement moment as the levels share (+5.04), calibrated independently for the net-of-tax form given eta_logs. The timing overlay is a mechanical fraction rather than a Bellman resp… <br> _derived under: tax_data `2026070814`, macro_projections `2026022522`_ <br> _re-derive: `other/kg_model_tests/form_ab/confirm_efull_logs.R`_ |
| `kg.deemed_avoidance` | `0.25` | judgment | — | Applier-only valuation/noncompliance haircut on deemed realization at death: the per-record deemed amount is scaled by (1 - this). Chosen for consistency with the estate-side pass-through valuation d… |
| `kg.char_extensive_intercept` | `-2.415` | judgment | — | Logit intercept for the probability that a decedent makes ANY charitable bequest, on log(gross estate in char_base_year dollars, millions). NO SOURCE RECORDED anywhere in the repository. The four cha… |
| `kg.char_extensive_ln_slope` | `0.458` | judgment | — | Log-estate slope of the extensive charitable-bequest logit. See char_extensive_intercept -- no source recorded. |
| `kg.char_intensive_intercept` | `-1.872` | judgment | — | Logit intercept for the charitable share conditional on giving. See char_extensive_intercept -- no source recorded. |
| `kg.char_intensive_ln_slope` | `0.468` | judgment | — | Log-estate slope of the intensive charitable-bequest logit. See char_extensive_intercept -- no source recorded. |
| `kg.response_form` | `logs` | structural | — | Which cost primitive the scenario-side FOC uses to extrapolate away from the calibrated local moment. 'logs' (default since 2026-07-22) is a constant NET-OF-TAX elasticity, the Agersnap-Zidar preferr… |
| `kg.applier_allocation` | `0.5` | structural | — | Within-cell rule for distributing the lock-in/carryover stock realization across records: 'R' proportional to positive kg_lt (targets active realizers), 'G' proportional to gain stock (targets holder… |
| `kg.dg_allocation` | `G` | structural | — | Within-cell allocation of policy-induced dG, controlling the effective cell mortality m_eff in the death/survivor channels. 'G' is the inheritance-flow story (m_eff gain-weighted); 'R' is the lock-in… |
| `kg.timing_window` | `1` | structural | — | Half-width in years over which timeable realizations may retime. Part of the conditioning set for the timeable-share calibration. |
| `kg.timing_ref_wedge` | `0.05` | structural | — | Reference wedge for the timing overlay: the fraction of planned dollars that move toward the best year in the window is clamp(rate change / ref_wedge, 0, 1), so 5pp moves the full bucket and 1pp move… |
| `kg.wealth_carry_scale` | `1` | structural | — | Multiplier on the wealth-tax deferral carrying cost h before it enters the Bellman, tau_eq, the guard slack and the state file. Default 1 (no scaling). A DISCLOSED, uncalibrated sensitivity dial, not… |
| `kg.beta_fallback` | `0.978` | structural | — | Fallback annual discount factor used ONLY when no year-varying series is supplied, i.e. isolated solver unit tests. Production paths build the series from tsy_10y Fisher-deflated by year-t YoY CPI-U… |
| `kg.char_base_year` | `2026` | structural | — | CPI-U base year the gross estate is deflated to before entering the charitable logits. Must match the year the coefficients were fitted in; since that fit is undocumented, this cannot currently be ve… |

## sigma

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `sigma.conv` | `0.16` | calibrated | 2026-07-12 | Top-subset ETI of 0.25 (Saez-Slemrod-Giertz central; taxable income EXCLUDING net capital gains, after deductions) measured on the +5pp top-ordinary validation leg (tests/topord_plus5, 2025:2035) wit… <br> _derived under: tax_data `2026070814`, macro_projections `2026022522`_ <br> _re-derive: `other/top_tax/archive/tests/compute_top_eti.R`_ |
| `sigma.pt_labor_share` | `0.75` | sourced | — | Smith, Yagan, Zidar and Zwick labor-content share of active pass-through income; applied to the active pass-through legs of the conversion pool. |

## wealth

| Assumption | Value | Kind | Set | Provenance |
|---|---|---|---|---|
| `wealth.avoid_public_e` | `-7` | judgment | — | Semi-elasticity of REPORTED marketable (publicly valued) wealth with respect to the marginal wealth rate: reported stock scales by exp(mtr_net_worth * e). Author-accepted, seeded from the standalone… |
| `wealth.avoid_private_e` | `-17` | judgment | — | Same, for closely held (privately valued) wealth, where valuation discretion is greater. Author-accepted, uncalibrated, and the largest single behavioral magnitude in the model. See avoid_public_e. |
| `wealth.chi_pub` | `1.0` | structural | — | Share of the public-wealth avoidance response that is CONCEALMENT (and so shrinks the income and estate bases too) rather than pure valuation. Default 1.0: marketable wealth that disappears from the… |
| `wealth.chi_priv` | `0.5` | structural | — | Same for closely held wealth. Default 0.5, i.e. half of the private-wealth response is concealment and half is valuation discretion that does not move the income base. Sweep 0.25 / 0.5 / 0.75 for the… |
| `wealth.cap_flows_pt_weight` | `0.2` | structural | — | Weight on pass-through flows in the capital-income bundle, for both the MTR bump and the wealth haircut (pure-capital flows carry 1.0). Matches the raw pass-through list in src/data/economy.R -- chan… |

## Entries with no recorded derivation

These are the numbers somebody chose. Nothing is wrong with a
judgment call, but these are where a sensitivity ranking should
start, and where a citation would retire the entry from this list.

- `corp.asset_exposure_dc`
- `corp.asset_exposure_equities`
- `corp.asset_exposure_re_fund`
- `corp.asset_exposure_trusts`
- `corp.equity_premium`
- `corp.kappa`
- `corp.omega_div`
- `corp.omega_kg`
- `corp.theta`
- `corp.theta_res`
- `distribution.housing_structure_share`
- `kg.char_extensive_intercept`
- `kg.char_extensive_ln_slope`
- `kg.char_intensive_intercept`
- `kg.char_intensive_ln_slope`
- `kg.deemed_avoidance`
- `wealth.avoid_private_e`
- `wealth.avoid_public_e`

## Not covered here

- **Numerical plumbing** — epsilons, convergence tolerances, guard caps
  (`fmax`, `CORP_MU_MAX`), structural bounds (age topcodes). Not economics.
- **The estate measurement bridge** — `r`, `rho_pt`, the SOI per-bin
  fractions, the gift add-back, the cluster cap. They live in
  `config/estate/estate_valuation_params.yaml`, are generated by
  `other/estate_tax/write_frozen_params.R`, and measure the data rather
  than describe a counterfactual.
- **The wealth saving profile** `s` and transition matrix `M` — a table,
  not a scalar, selected per scenario by the `wealth_financing` runscript
  column from `config/wealth/profiles/`.
- **Behavior-module parameters** that only affect their own module (the
  entity-shifting `alpha`, the Bastian labor elasticities, and so on).
  A scenario varies those by pointing at a different module, which is the
  existing mechanism. Where another channel's calibration depends on one
  — evasion feeds sigma, for instance — the module file is listed in that
  calibration's `invalidated_by`, so a change to it still trips the check.

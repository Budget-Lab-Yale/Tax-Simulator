# Integrating a net-worth tax onto Tax-Simulator — review & migration plan

_Generated 2026-06-22. Reviews the standalone **Wealth-Tax-Simulator** (`/nfs/roberts/project/pi_nrs36/jar335/Repositories/Wealth-Tax-Simulator`, last commit `8c00e14`, 2026-04-01) against the **Tax-Simulator** `wealth` branch, in service of deprecating the standalone model and rebuilding a net-worth tax as a first-class tax base here._

Method: 9 deep-read agents over both repos → 4 synthesis dimensions (feature map, blast radius, interactions, gaps + migration) → 14 adversarial verifications of the load-bearing premises against the actual code. Every file:line below was either read directly during the review or confirmed by a skeptic agent. Where the obvious approach is **wrong**, it's flagged with ⚠.

---

## 0. Bottom line

**Deprecating the standalone model is the right call, and most of it is `DROP`, not `port`.** The original reason for a separate model — no wealth information on the main model — is gone. Three facts make integration tractable:

1. **The data already exists, per-record, every year, death-agnostic.** Tax-Data carries the full balance sheet (`value.*` — 14 asset + 6 debt columns, `tax_units_2026.csv` cols 146–165, plus `forbes_net_worth` col 187), loaded onto every record by `read_microdata()`'s unrestricted `fread()` (`src/data/economy.R:477-494`, `src/sim/run.R:437-438`). `calc_estate()` already computes `economic_gross = rowSums(assets)` and `estate_debts = rowSums(debts)` for **every living record** (`src/calc/functions/tax/estate.R:140-141`). Net worth is one subtraction away — no new data plumbing. **(Verified, high confidence.)**

2. **A net-worth tax is structurally "the estate tax minus death."** Estate is the working proof that a balance-sheet/stock tax can live in `do_taxes()`, aggregate with weights, book receipts, and feed distribution. A wealth tax is estate **minus mortality, minus DSUE/portability/both-die, minus the gift/§2053/valuation death-time adjustments, minus the FY death-year+1 lag** — so the wiring path is well-trodden.

3. **The standalone already contains a primitive version of the "second bathtub."** WTS's `track_deccumulation()` + `age_scf()` (`Wealth-Tax-Simulator/src/sim.R:242-284, 135-203`) is a real cross-year saving-financed wealth-erosion feedback, and its `dissaving_rate` / `mobility_rate` levers map directly onto the planned bathtub's saving-response `s` and within-age transition matrix `M`. **The wealth tax and the second bathtub are inseparable**: without the bathtub, a wealth tax on this model implicitly assumes a fixed wealth stock (the "MPC = 1" absence) and can *never* erode the future estate/capital-income base it logically should.

**Where the work concentrates:** (a) the calculator + YAML + `do_taxes` + totals + receipts + distribution wiring — medium effort, well-templated by estate, with a mandatory SLURM Phase 3a/3b mirror; (b) the within-year avoidance behavioral module — low-medium, but the standalone's elasticities must be recalibrated, not copied; (c) **the second bathtub — high effort, currently UNBUILT, and the real intellectual core** of "composing" a wealth tax with the rest of the system.

---

## 1. The standalone model, factually (and what's a phantom)

WTS is a thin, single-base, household-level microsimulator: 6 source files (`data.R` 584L, `sim.R` 284L, `calc.R` 120L, + `main.R`/`config.R`/scenarios). Its pipeline is `process_scf() → age_scf_historical() → add_forbes_data()` (`sim.R:29-31`), then a sequential year loop doing static `calc_taxes` + `calc_mtr`, conventional `do_avoidance`, and `track_deccumulation` feeding next year's aging.

Two capabilities that were assumed to exist **do not**, and this matters for expectations:

- ⚠ **There is no SZZ / top-0.1% wealth-share calibration.** `resources/szz_wealth_shares.csv` and `resources/top_01_shares_historical.csv` are **never read anywhere** in the repo. Top-1%/0.1% shares appear only as diagnostic *output* in `get_inequality()` (`data.R:399-423`). WTS's top tail is driven **solely** by the Forbes splice + the SCF≥$500M composition assumption. **If anyone believes the standalone matched published Saez-Zucman / Smith top shares, that is false.** Consequence: a revenue cross-check between the standalone and the integrated tax will **not** reconcile (different SCF/Forbes universe, no top-share calibration), and main-model top-tail fidelity must be validated independently.

- ⚠ **B101 is not an "aggregate control."** The Financial Accounts B101 file supplies only 2022:Q3→2024:Q3 per-capita **growth rates** for aging (`data.R:102-168`); SCF totals are never benchmarked to B101 levels. There is no level-control logic to port.

Two real bugs/inconsistencies to **not** carry over:
- ⚠ `calc.R:53-54` — `deductions = primary_mortgage + ... + installment_debt, other_debt` — the comma makes `other_debt` a dangling expression, so **`other_debt` is silently never subtracted** from net worth.
- `annuities` is in WTS's `net_worth` and `do_avoidance` but **absent from the `calc_taxes` asset list** — internally inconsistent.
- `retirement` is a single WTS class but maps to **two** Tax-Data columns (`value.dc` + `value.db`); any 1:1 port mis-aggregates retirement wealth.

---

## 2. Feature map — WTS → Tax-Simulator

Disposition legend: **REUSE** existing TS machinery · **ADAPT** onto TS · **NET-NEW** · **DROP** (superseded).

| WTS capability | Where (WTS) | Disposition | Target home on Tax-Simulator |
|---|---|---|---|
| SCF microdata build (14 assets / 6 debts) | `data.R:9-84` | **DROP** | Tax-Data `value.*` (`ESTATE_ASSET_COLS`/`ESTATE_DEBT_COLS`, `estate.R:23-33`) |
| Historical aging 2022→24 (B101 growth) | `data.R:88-212` | **DROP** | Tax-Data already projected by vintage |
| Forbes-400 billionaire splice | `data.R:216-294` | **DROP** | Estate's Forbes-clone splice (one shared top tail) — ⚠ see §4 |
| "B101 aggregate control" | — | **DROP (phantom)** | does not exist |
| "SZZ / top-share calibration" | — | **DROP (phantom)** | does not exist; optionally repurpose CSVs as *validation* targets |
| Bracketed net-worth calculator | `calc.R:9-90` | **ADAPT** | new `calc_wealth()` + `integrate_rates_brackets()` (`src/calc/utils.R:105`) |
| Bracket inflation indexing (`ccpiu_irs`, round to $1M) | `calc.R:30-39` | **REUSE** | YAML `i_measure`/`i_base_year`/`i_increment` engine |
| Exemption / asset-class exclusions | `calc.R:57-59` (TODO) | **NET-NEW** | real indexed `exemption` subparam in `wealth.yaml` |
| MTR (`+$1` to cash) | `calc.R:94-120` | **DROP** | redundant w/ `nextdollar` machinery — ⚠ see §3 |
| Within-year avoidance (`do_avoidance`) | `sim.R:207-238` | **ADAPT** | `do_wealth_avoidance()` behavior module (recalibrate elasticities) |
| Cross-year decumulation + mobility | `sim.R:242-284, 135-203` | **ADAPT → 2nd bathtub** | `run_wealth_bathtub_pass()` (see §5) |
| `consumption_finance` lever (`dissaving_rate=0`) | scenario config | **ADAPT** | bathtub saving-response `s` (0 ≈ today's implicit fixed-wealth) |
| `perfect_mobility` lever (`mobility_rate=1`) | scenario config | **ADAPT** | bathtub transition matrix `M` (`M=I` = full persistence = mobility 0) |
| Totals / inequality / detail / distribution writers | `data.R:345-583` | **DROP** | standard `write_pass_outputs` + `process_for_distribution` |
| by-wealth-percentile distribution metrics | `data.R:484-583` | **ADAPT (mostly net-new flavor)** | extend `distribution.R` with wealth-percentile grouping + `share_with_hike`, `pct_chg_wealth` at 99/99.9 cuts |
| `why_not_analysis.R` (manual wealth+income surtax composition) | `analysis_scripts/private/` | **DROP → reconstitute** | one first-class composed scenario — ⚠ deprecation hazard, see §6 P4 |
| Sequential year loop / orchestration | `main.R`, `sim.R:9-131` | **DROP** | runscript / `parse_globals` / `do_scenario` |

The genuinely **net-new** items are few: (1) a real indexed exemption; (2) wealth-specific distribution grouping; (3) the entire second bathtub.

---

## 3. MTR / behavior for a *stock* tax — the conceptual subtlety

⚠ **Correction to the intuitive framing.** It is *not* true that "a net-worth tax has no clean next-dollar MTR." WTS's `calc_mtr()` adds $1 to net worth and re-runs the calc (`calc.R:94-120`) — the identical mechanism Tax-Simulator uses for `type='nextdollar'` (`do_taxes.R:521-557`). A piecewise-linear bracket schedule has a perfectly well-defined marginal rate on the next dollar of wealth (arguably *cleaner* than an income MTR — no credit/phase-out kink interactions).

The real reasons the standard `apply_mtr_elasticity` pathway doesn't directly apply:
- The response variables are wealth **stock** components (`value.cash`, `value.equities`, …), and the `mtr_vars`/`calc_mtrs` machinery is built for income/flow composites — it self-documents that it is "not generalized to every variable."
- A wealth MTR is a wedge on **saving/accumulation**, not on a flow already in the base — economically a different object.
- `apply_mtr_elasticity` (`semi`/`arc`/`netoftax`/`taxprice`) adjusts flows; WTS's avoidance shrinks the **base** (`exp(mtr·e)` on stocks). That's a **new contract**: a wealth-avoidance module must mutate component columns and then **force a `calc_wealth` recompute**.
- Note WTS's `do_avoidance` `exp(mtr·e)` *is* exactly `e_type='semi'` (`exp((mtr−mtr_baseline)·e)−1`) with baseline MTR = 0.

**Recommendation:** register **no** standard MTR for the base initially. The credible behavioral response is the multi-year bathtub, not a one-line elasticity. If a wealth MTR is later wanted, it needs (a) `net_worth` composite handling in `calc_mtrs` and (b) a carve-out from the `calc_wealth_flag=FALSE` gate — precedent is the `kg_lt` law-only MTR special case (`run.R:606-624`).

⚠ The standalone elasticities are **extreme** (`public_e=-7`, `private_e=-17`, Warren single `e=-13`). Applied as `exp(mtr·e)`, at high wealth-tax MTRs they imply near-total base disappearance. They **must be re-justified/recalibrated on Tax-Data, never copied**.

---

## 4. The two bathtubs — and why wealth is the most-coupled base on the model

There are two distinct cohort "bathtubs" in play. **Don't conflate them.**

**Bathtub #1 (built): `kg_dynamics`** — a capital-gains realization-timing recurrence on age cells [18,80]+tail (`src/sim/kg_dynamics.R`). It is the *template*: aging matrix `A` (`:560`), heir matrix (`:520`), per-year `.rds` state + `match()`-applier (`:1311-1320`), one-step recurrence (`:1116`), a frozen-mechanical (static-side) vs behavioral (conventional-side) split, SLURM phases 1B/2A/2B/2C, and a calibration-provenance staleness guard `KG_DYN_CALIB_PROVENANCE` (`:216`, built after a 2026-06 applier flip silently biased estimates ~37%). **Load-bearing reuse:** effective cell mortality `m_eff = Σw·m·X / Σw·X`, *not* a cell-mean (`:1140-1155`) — the wealth-mortality gradient makes a cell-mean overstate the death/bequest flow ~2.7×. A wealth bathtub **must** replicate this. The full Bellman is realization-specific — do **not** copy it.

**Bathtub #2 (UNBUILT): wealth-dynamics saving incidence** — plan at `/home/jar335/.claude/plans/purrfect-weaving-toucan.md`; only two calibration diagnostics exist (`other/wealth_dynamics/cohort_wealth_growth.R`, `id_persistence_check.R`, both git-untracked). It overturns the implicit fixed-wealth assumption: a share `s = 1 − MPC` of above-baseline **during-life** tax is financed out of wealth, compounds at `r_total`, and drains into the estate base at death — opening the income/CG → wealth → estate channel. Cells = (age × within-age net-worth percentile); present/survivor recurrence; placeholders `M = 100×100 identity`, flat `s = 0.5`, `r_total =` nominal GDP/capita ≈ 4.0%; a `(1−f)` gross-asset haircut on `value.*`/`basis.*` + capital-income flows, reusing the 20%-capital / 80%-labor pass-through split (`distribution.R:147-148`).

**The standalone's behavior is bifurcated and maps cleanly onto these:**
- `do_avoidance` (within-year base erosion) → a **behavior module** (Phase 2).
- `track_deccumulation` + `age_scf` (cross-year saving feedback) → **the second bathtub** (Phase 3). `dissaving_rate ↔ s`; `mobility_rate ↔ M` (`M=I` = `mobility_rate=0` = full persistence). ⚠ The `mobility_rate ↔ M` mapping is an **analogy, not a structural port**: WTS blends own-ETR vs a single economy-wide ETR (a scalar); the bathtub's `M` is a 100×100 within-age percentile transition matrix that adds the *targeting* dimension WTS lacks (required so a top-wealth tax deficits only the people who paid it).

### ⚠ The design is in flux — plan vs. latest decisions disagree
The plan doc as written says **"inflow = STATIC"** and **"conventional-side only / behavior module."** A later working session (recorded in memory `wealth-dynamics-design`, 2026-06-22b) **overturned both**:
- Placement should be **MECHANICAL / static-side** (mirroring the kg *frozen* pre-pass), **not** a conventional behavior module — so the erosion lands in static detail and the heir allocator + distribution see it for free.
- The ΔT inflow should be **CONVENTIONAL** (the tax actually paid, post-behavioral; static overstates ~1.9× for CG), read from a *separate* wealth-excluding conventional pass to avoid a fixed point.
- The "consistency is automatic (doubly-stochastic)" claim was found **wrong as stated** — it conflates the rank-label marginal with the deficit-mass marginal; differential mortality breaks it. Replace with an explicit, unit-tested **mass-conservation** invariant.

This must be reconciled before Phase 3.

### Integration obligation (the edit the plan does not yet contain)
The bathtub's locked during-life inflow is `Δ(liab_iit_net + liab_pr)` **excluding** estate/deemed death-time legs. Wiring a wealth tax requires **extending the during-life inflow to add the new wealth-tax liability component** while still excluding death-time legs — and building it from **raw components**, not the post-proc `liab_iit_pr` (which subtracts `liab_deemed`, `distribution.R:149`).

---

## 5. Blast radius — subsystems that must change

In dependency order. "SLURM" notes the obligation under the CLAUDE.md sync table.

1. **Shared asset/debt constants.** Promote `ESTATE_ASSET_COLS`/`ESTATE_DEBT_COLS` (`estate.R:23-33`, also duplicated `kg_dynamics.R:159`) to one shared module so estate + wealth + bathtub import **one** net-worth definition. _SLURM: if a new source file, add to `common.R` `reconstitute_environment()`._
2. **`calc_wealth()`** — NEW `src/calc/functions/tax/wealth.R`, mirror `calc_estate()`; strip DSUE/portability/both-die/mortality. ⚠ The estate two-branch calc exists because of the *nonlinear unified-credit kink* — a graduated wealth schedule is **linear** through `integrate_rates_brackets`, so there is **one** calc per record and **no** separate exemption-kink rationale. ⚠ A separate `wealth.exemption` param is optional: the standalone models the exemption *as the zero-rated bottom bracket*; decide explicitly. **Do NOT register in `return_vars`** (follow estate, not the 1040). _SLURM: inside `run_one_year` → safe._
3. **`do_taxes.R` chain** — add a "Wealth tax" section after the estate block (`do_taxes.R:127-144`), gated by a new `calc_wealth_flag` (default TRUE; FALSE in the MTR loop and kg dead-leg recomputes), drop/rebind `WEALTH_OUTPUT_COLS` for idempotency. _SLURM: inside `run_one_year` → safe._
4. **`wealth.yaml` tax_law** — NEW `config/scenarios/tax_law/baseline/wealth.yaml`, baseline = no tax (rate 0 / exemption Inf). Auto-joins onto the frame via `left_join(tax_law, by=c('year','filing_status'))` (`run.R:459`) and the `*.yaml` glob loader (`tax_law.R:98-100`) — no parser change. ⚠ MFJ doubling is **not** done via a mapper in estate (estate.yaml is filing-blind; 2× is hardcoded in the calculator `estate.R:60-61`); the YAML-mapper precedent is `ord.yaml:13-18`. ⚠ WTS rounds to nearest $1M = `i_direction:0`, **not** estate's floor (`-1`) — pick deliberately, it shifts revenue at kinks.
5. **`detail_vars`** — append `liab_wealth` (+ `net_worth`) at `config_parser.R:271-288` (where estate cols sit, `:286-287`). Raw `value.*`/`basis.*` need not persist. _SLURM: parser change is consumed at setup → confirm._
6. **Runscript columns** — **none** required for the base; the `tax_law` path routes the reform. Only MTR registration (if ever added) touches `mtr_vars`/`mtr_types`.
7. **Totals** — NEW `get_wealth_totals()` = `Σ(weight·liab_wealth)/1e9` (**drop** `estate_m`). Wire into the per-pass totals lists (`run.R:649-652`, `741-744`) + `totals/wealth.csv` in the shared `write_pass_outputs()`. _SLURM: `aggregate.R` Phase 3a — required._
8. **Receipts** — ⚠ writing `totals/wealth.csv` alone books **no** revenue. `calc_receipts` (`revenue.R`) must be **extended**: a `revenues_wealth_tax` line as a **pure on-model LEVEL** (no CBO anchor, unlike estate's level+delta), booked **FY = CY, income-style 75/25** (`revenue.R:131-137`) — **NOT** the estate `t+1` death-lag (`revenue.R:146-147`), which would mis-time receipts a full FY. Must be added to the final `select` (`:184-185`), every `total=` sum in `calc_rev_est`, and the stacked report selects/labels — or it silently drops from the headline. _SLURM: `aggregate.R` Phase 3a + Phase 4 — required._
9. **MTR registration** — see §3; recommend none initially. If added, carve-out per `kg_lt` law-only (`run.R:606-624`), and ⚠ the `calc_mtrs` `delta_taxes` numerator (`run.R:661`) reads **only** `liab_iit_net + liab_pr` — wealth liability never enters any MTR numerator without editing that formula too.
10. **Behavioral module** — `do_wealth_avoidance()` (base-erosion, new contract). _SLURM: inside `run_one_year` → safe._
11. **Distribution / incidence** — fold `liab_wealth` into the `taxes_included` `case_when` (`distribution.R:322-332`), attach to the **owner's own record** like income tax. ⚠ Do **NOT** route through `allocate_estate_to_heirs()` — that rank-match allocator exists only because estate lands on a decedent ≠ beneficiary (`estate_allocator.R:21-25`); its hard heir-ladder-exhaustion error would fire spuriously. Reuse the 20%-capital split for incidence. _SLURM: `aggregate.R` Phase 3b — required._
12. **Valuation params** — ⚠ see §4 hazard below; a wealth tax must **not** share `estate_valuation_params.yaml`.
13. **SLURM sync** — Phases 3a (totals/receipts), 3b (distribution), 4 (stacked) confirmed required. ⚠ `setup.R`/`common.R` are **conditional**, not automatic: estate sets `globals$estate_params` **inside** `run_one_year` (`run.R:486`) and is *never* serialized in `setup.R` nor reconstituted in `common.R`; CLAUDE.md lists "anything inside `run_one_year()`" as a **safe** change. `setup.R` triggers only on a `parse_globals` change or `do_scenario` pre-sim setup; `common.R` only for new globals consumed by **post-processing**.
14. **Provenance guard** — any calibrated wealth response (avoidance elasticities; bathtub `s`/`r_total`/`M`) needs a `WEALTH_DYN_PROVENANCE`-style stamp + strict env gate, pinned to Tax-Data/Macro vintages, mirroring `KG_DYN_CALIB_PROVENANCE`.

---

## 6. Interactions, complexities & blind spots

A wealth tax is the **most-coupled base the model has** — it lands on the exact same balance sheet that drives estate, kg-deemed-at-death, and the planned 2nd bathtub. Once four taxes/feedbacks sit on one balance sheet, every shared object is a place to double-count or silently couple calibrations.

1. **Double-tax-at-death.** In a decedent's final year, the annual wealth tax and the estate tax both hit the same stock. (a) The calculator must decide whether estate's taxable base is net of the year-of-death wealth tax paid. (b) The deeper channel: paying a wealth tax shrinks wealth, which *should* shrink the future estate base — but with the fixed-stock model (`ati = income − liab`, `distribution.R:338-339`, no asset decrement) it never does. **Only the 2nd bathtub overturns this.** Without it, wealth composes with estate only mechanically, **overstating** long-run wealth/estate bases.

2. ⚠ **The valuation-bridge sharing hazard.** `estate_valuation_params.yaml` is MEASUREMENT — pinned to a Tax-Data vintage (`:50`), structurally non-overridable (loaded into `globals$estate_params` and passed as an argument, `run.R:486`; lives outside the `tax_law/` tree so reforms physically can't reach it), with a staleness warning (`src/sim/estate.R:50-61`). Two compounding risks if a wealth tax reuses it: **(coupling)** sharing `r`/`rho_pt` means a future *estate* recalibration silently moves the *wealth* base; **(wrong regime)** `r=0.951`/`rho_pt=0.612`/`f_ded`/`gamma` were fit to SOI estate-**return** reporting (death-time §2031 valuation, §2053 debts, gift add-back) — a different regime than annual assessment. ⚠ **Correction:** the estate *module* does **not** itself do the Forbes splice or wealth-share calibration — Tax-Data does that upstream; the estate code adds only the measurement bridge + the donor-clone cap. So the reusable asset is the **shared Tax-Data `value.*` columns + the cluster-detection idea**, **not** the `r/rho_pt/gamma/bin` bridge (a net-worth tax bases on *economic* wealth directly; the standalone uses **no** discount). **Design fork (gates P0):** economic net worth vs. a reported-valuation bridge — and if a bridge is wanted, it belongs in its **own** frozen `config/wealth/` file.

3. **Donor-clone / Forbes — worse for a wealth tax.** The estate cluster cap (≈300) is a **death-weight** cap (`src/sim/estate.R:67-119`): it lets billionaire clones "barely die" out of the base. An annual tax weights **living** clones every year, exposing the full duplicated top-tail mass — exactly where a wealth tax concentrates. The death-weight cap does **not** transfer; a **living-weight** cap must be calibrated fresh. Compounding: the $100M–$1B band is under-sampled upstream and the estate model is biased low on levels — reusing its top tail inherits that bias unless retargeted.

4. **Wealth × kg-deemed.** Both read `value.{cls}`/`basis.{cls}` (only **5 of 14** asset classes carry basis). Order wealth **before** kg; the `(1−f)`-haircut `kg_lt` feeds kg's scale-invariant applier so the kg pre-pass is untouched. ⚠ But the plan's "consistency confirmed" is aspirational (the feature is unbuilt): kg lock-in (`extra_R`) and deemed (`kg_deemed`) must read the haircut `value.*`/`basis.*` consistently with the frozen-pass `G_B`, or stock-flow consistency breaks (a verification item, not yet confirmed). A wealth-tax-at-death, being threshold'd, would need rank-matching like estate — **not** the proportional-to-inheritance smear the kg-deemed allocator uses (kg has no threshold).

5. **Static-vs-conventional consistency.** Estate books FY `t+1`; wealth must book in-year. And the bathtub's static-inflow approximation is flagged by its own plan as accepted-but-open and "may not be second-order" at wealth-tax magnitudes (now compounded by the overturn to a conventional inflow — §4).

6. **Tax-Data artifacts propagate into the base and the inflow.** (a) The **wages residual** (`wages1+wages2 ≠ wages`) contaminates the bathtub's during-life income inflow — net it out. (b) The **sample-universe** fix (union of ids over sim years, id-keyed RNG) is *more* critical for a top-tail-concentrated tax; like kg, this demands `pct_sample=1`. (c) **age1 top-code at 80** (only Forbes clones above) — the top age bin must be 80+, exactly where the wealth base concentrates. (d) Joint records keyed on `age1` vs `max(age1,age2)` is an accepted v1 approximation that mis-times the both-die drain.

7. **One canonical net-worth definition.** Four consumers each compute their own today (`calc_estate`, `cohort_wealth_growth.R`, `kg_dynamics`, WTS — the last with the `other_debt` bug). Adopt one shared definition and decide explicitly: economic vs reported; whether retirement (dc+db)/life-ins/annuities are in/out; basis-aware variant or not.

8. **Filing-status / MFJ doubling** is an undesigned LAW choice. The standalone is filing-blind; estate uses a joint 2× exemption (in the calculator). Decide whether `wealth.yaml` needs a `filing_status_mapper`.

9. **VAT / excess-growth refusal must be inherited.** Wealth stocks are raw (non-VAT-adjusted) dollars (`run.R:482-485`); kg refuses VAT/excess-growth scenarios for exactly this unit-mismatch reason (`run.R:869-889`). The wealth base must refuse or guard explicitly — and the bathtub mixing raw-$ net worth with adjusted ΔT is a live unit-mismatch risk.

---

## 7. Phased migration path

| Phase | Goal | Key deliverables | Gating decisions |
|---|---|---|---|
| **P0** | Confirm top-tail data quality; decide valuation & file boundary | Living-population top-share diagnostic (`pct_sample=1`) vs. repurposed SZZ/Smith series as *targets*; written decision **economic vs reported** base; decision that any wealth valuation factors live in a **new frozen `config/wealth/`** file, never shared with estate | economic vs reported; is a measurement file needed at all; retirement/life-ins/annuities in or out |
| **P1** | Static first-class wealth tax | `calc_wealth()` + `wealth.yaml` + `detail_vars` + `get_wealth_totals` + **`calc_receipts` extension** (pure level, FY=CY) + SLURM Phase 3a mirror | filing-status MFJ doubling; payment timing; VAT/excess-growth refuse-vs-guard; separate `liab_wealth` (recommended) vs folded |
| **P2** | Within-year behavioral avoidance | `do_wealth_avoidance()` base-erosion module (**recalibrated** elasticities + provenance guard); optional `net_worth` MTR carve-out; **living-weight** donor-clone cap | authoritative elasticity set; stock-MTR vs ATR anchor; avoidance as module vs folded into bathtub |
| **P3** | Dynamic saving incidence (the credibility phase) | **Part A:** build the 2nd bathtub (reuse kg scaffolding, `m_eff` gradient correction, SLURM 2B-sibling with `2C afterok BOTH`). **Part B:** extend the during-life inflow to add `liab_wealth` (exclude death-time legs; raw components) | reconcile plan-vs-overturned design (static/behavioral → conventional/mechanical); mass-conservation invariant; joint keying; `fmax` clamp binding under a concentrated top tax |
| **P4** | Deprecate the standalone | Reconstitute `why_not_analysis.R`'s wealth+income-surtax composition as **one** first-class scenario; port wealth-percentile distribution metrics (adapt, not port); document why standalone revenue won't reconcile; archive repo | own `taxes_included` distribution tier vs folded; migrate vs drop SZZ/Smith; archive vs delete |

⚠ **P3 depends on the bathtub being built**, which is currently planning-only. P1+P2 deliver a usable static + within-year-behavioral wealth tax on their own; P3 is what makes it *credible* (dynamic composition with estate/capital-income), not optional polish. Validate at `pct_sample=1`; run **one year past** the reporting window (estate delta is FY-lagged to death-year+1 — same trap logged in the estate/kg memory).

---

## 8. Open decisions (consolidated)

1. **Economic net worth vs. reported valuation** for the wealth base. (Gates P0/P1; determines whether a `config/wealth/` measurement file exists.)
2. **Base composition:** retirement (dc+db), life-ins, annuities, trusts in or out; basis-aware variant?
3. **Filing status:** MFJ doubling (mapper or calculator) vs filing-blind.
4. **Exemption** as a separate indexed subparam vs zero-rated bottom bracket; indexation rounding convention.
5. **Receipts timing:** income-style 75/25 vs full nonwithheld-at-filing (both in-year, not estate's lag).
6. **MTR:** register a `net_worth` MTR or not (recommend not initially).
7. **2nd bathtub design reconciliation:** static/behavioral (plan) vs conventional-ΔT/mechanical-application (latest session); the mass-conservation invariant.
8. **Year-of-death interaction:** is the estate base net of the wealth tax paid that year?
9. **VAT/excess-growth:** refuse (like kg) or guard.
10. **Standalone disposition:** archive vs delete; migrate SZZ/Smith as validation diagnostics or drop.

---

## Appendix — adversarial verification summary

14 load-bearing claims were checked by skeptic agents against the code. **2 confirmed outright, 12 partial** (correct core, important nuance). The nuances are folded into the body above; the most consequential:
- The data crux **(confirmed)**: the 20 `value.*` columns are physically present and populated population-wide, death-agnostic.
- The "MTR doesn't exist" framing is **wrong** — a clean next-dollar wealth MTR exists; the real issue is stock-vs-flow behavior and machinery.
- The estate **module** doesn't splice Forbes — Tax-Data does upstream; reuse the data + cluster idea, not the `r/rho_pt` bridge.
- `setup.R`/`common.R` SLURM sync is **conditional**, not automatic (the estate precedent sets its params *inside* `run_one_year`).
- Writing `totals/wealth.csv` books **no** revenue — `calc_receipts` must be extended too.
- The 2nd bathtub is **unbuilt**, and its design is **actively being revised** (static→conventional inflow, behavioral→mechanical placement).

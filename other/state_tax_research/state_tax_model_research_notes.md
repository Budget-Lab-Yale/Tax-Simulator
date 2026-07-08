# State Individual Income Tax Modeling — Research Notes

**Date:** 2026-07-07/08
**Purpose:** Planning input for a Tax-Simulator branch that adds state individual income
tax models mirroring the federal design. Parameters are to be built from primary
governmental sources (state forms, instructions, statutes); existing models are used to
validate our results and inform structure.

**Provenance:** Findings below come from (a) a code-level review of this repo and (b) a
fan-out web research run with adversarial fact-checking. The verification stage was
partially cut off by a rate limit: 38 verification votes confirmed claims, 1 refuted a
claim (corrected below, §2.1), and a subset of claims (mostly PolicyEngine-related) went
unverified — those are marked ⚠. Raw agent outputs are preserved in
`raw/all_agent_results.txt` and `raw/deep_research_journal.jsonl` in this directory.

---

## 1. How Tax-Simulator models federal income taxes today (repo review)

- Everything is a tibble of tax units. Tax law becomes columns named
  `{parameter}.{subparameter}` (e.g. `item.salt_limit`, `ord.rates1`), joined onto the
  microdata by `(year, filing_status)` at `src/sim/run.R:315`.
- Calculation is a pipeline of pure, vectorized `calc_*()` functions orchestrated by
  `do_1040()` in `src/calc/do_taxes.R:248-385`: capital gains → AGI → standard/itemized/
  PE/QBI/below-line deductions → taxable income (incl. itemizer choice) → ordinary +
  preferred rates → alternative-maximum cap → AMT → credits (CDCTC, education, saver's,
  CTC, EITC, rebate, wage subsidy) → NIIT/AGI surtax → final liability (`calc_liab`).
- Each `calc_X()` follows a strict contract: registers outputs in `return_vars$calc_X`,
  declares `req_vars` (tax-unit attributes + tax-law columns, `[]` suffix for vectors),
  validates via `parse_calc_fn_input()` (`src/calc/utils.R:39`), does one big `mutate`,
  returns only its registered columns. Bracket schedules use the reusable
  `integrate_rates_brackets()` (`utils.R:105`).
- YAML parameter machinery (`src/data/tax_law.R`) is fully generic: per-subparameter
  time series, indexation (`i_measure`/`i_base_year`/`i_direction`/`i_increment`),
  `filing_status_mapper` expressions, and subparameter-level reform overrides. New
  `state_*.yaml` files would work **with no changes to this code**.
- MTRs: `calc_mtrs()` (`do_taxes.R:458`) perturbs a variable by $1 and re-runs
  `do_taxes()`. Behavioral feedback consumes baseline vs static MTRs.
- SLURM pipeline mirrors orchestration; changes inside `do_taxes`/calc functions need no
  SLURM edits, new post-processing does (see CLAUDE.md sync table).

**Constraints that define the state problem:**

1. **No state identifier exists in the input microdata.** The Tax-Data output
   (`tax_units_{year}.csv`, 138 vars per `variable_guide.csv`) has no state/FIPS/ZIP
   field. State law would need a third join key the current `(year, filing_status)`
   join lacks.
2. **State income tax paid is not separately observed.** `salt_inc_sales` (PUF E18400)
   is the *greater of* state/local income OR sales tax, itemizers only. Also present:
   `salt_prop` (E18500), `salt_pers` (imputed), `state_ref` (E00700, state refunds —
   already flows into AGI at `functions/income/agi.R:96` and is an AMT add-back).
3. SALT deduction calc lives at `functions/deductions/item_ded.R:112-118`
   (`salt_paid = salt_inc_sales + salt_prop + salt_pers`, capped by the effective
   limit). The PTE workaround (`do_salt_workaround_baseline`, `do_taxes.R:650-720`)
   and AMT add-backs both interact with anything that changes measured SALT.
4. `src/tests/test_taxsim.R` already cross-validates the federal calculator against
   NBER TAXSIM with `state = 'No state'` — natural seed for a state validation harness.

---

## 2. Existing state income tax models

### 2.1 NBER TAXSIM (v35) — the validation benchmark

- Single Fortran program computing federal + state liability; `state` is an input coded
  with **SOI state codes 1 (AL) – 51 (WY)** (not FIPS/Census); `state = 0` disables the
  state calculation entirely. [taxsim.nber.org/taxsim35]
- Coverage: federal law 1960–2023. State law is coded from 1977 through ~2020/2021;
  later years use the last coded year's parameters inflated (~2.5%/yr).
  **Correction from fact-check:** an earlier extraction said state law stops in 2018 —
  refuted; the NBER page's own adjacent sentence says 2021–2023 use "an inflated 2020
  law as an estimate," and the taxsim35 page says state law is incorporated through
  2021. Either way: *state law encoding materially lags federal*, and recent-year
  TAXSIM state results are approximations — relevant when using it for validation.
- **Circularity handling:** deducts *calculated liability* rather than amount paid
  (both for federal SALT and for states deducting federal tax) and resolves the
  simultaneity by **iterating federal↔state for 3 rounds**. Feenberg (1987) gives a
  closed-form expression for the combined marginal rate under mutual deductibility.
- Optimizes on the taxpayer's behalf: itemization decision uses TAXSIM's own computed
  state income tax; sales-tax deduction imputed from IRS Pub. 600 regressions (2002+);
  where states allow optional separate spousal filing it computes both and takes the
  lower.
- Outputs 11 state intermediate variables (v30–v41: state AGI, exemptions, standard/
  itemized deductions, taxable income, property-tax credit, child-care credit, state
  EITC, bracket rate).
- History (Feenberg & Coutts 1993): state IDs appeared on the PUF starting tax year
  1974; high-income records (AGI > $200k) lacked state IDs and were **randomly assigned
  to states to match JCT counts**; Helen Ladd criticized the model's failure to
  reproduce aggregate state revenue totals.
- **Primary-source philosophy matches ours:** Feenberg & Coutts state they code state
  law "chiefly from the tax forms distributed by the states," using CCH/ACIR/Tax
  Foundation only secondarily, because secondary summaries are "rarely sufficiently
  detailed" and sometimes describe laws that never took effect.
- R access: `usincometaxes` CRAN package runs TAXSIM-35 locally via WebAssembly (no
  data leaves the machine) — the practical path for an automated validation harness.

### 2.2 Tax Policy Center — the PUF-reweighting precedent

Source: Urban/TPC methodology paper, *Incorporating State Analysis into the TPC's
Microsimulation Model* (2016).

- **Splits each record's national weight into 52 state weights** (50 states + DC +
  other areas) using Schirm & Zaslavsky's (1997) constrained parametric Poisson-
  regression small-area method. Guarantees (i) state weights sum to the national
  weight and (ii) state-weighted totals match published SOI state targets.
- Estimation stratified into **nine AGI groups**, targeting 39–51 variables per group
  (return counts + dollar amounts: AGI, wages, deductions, EITC) from SOI state tables
  (TY2011). Of 22,308 targets, only 32 (0.1%) missed adjusted targets by >2%.
- **TPC did not write its own state calculators** — it runs Jon Bakija's state income
  tax calculators on the reweighted database.
- Validation vs 11 state revenue-agency models: overestimates state filer counts by
  8–35% (federal filers who need not file state returns), state taxable income within
  5% for all but two states (OH, OR); a DC top-rate-cut simulation gave −$8.0M vs the
  DC revenue office's −$8.4M.
- Quantifies interactions: 7 states allowed full/partial deduction of federal income
  tax; 27 states based state EITC on the federal credit; 9 states set standard
  deduction equal to federal.

### 2.3 Treasury OTA — Technical Paper 6 (Fisher & Lin, 2015)

The "2015 OTA working paper": *Re-weighting to Produce State-Level Tax Microsimulation
Estimates* (https://home.treasury.gov/system/files/131/TP-6.pdf).

- Same structural idea as TPC: split each INSOLE record's national weight into 52
  state weights, `w_{i,st} = p̂(st | x_i) · w_i`, so state statistics mechanically sum
  to national totals.
- `p(st | x)` estimated from the *population* Individual Returns Transaction File
  (IRTF) under a conditional-independence assumption (state ⊥ outcome | X), via a
  decomposable log-linear graphical model (R package `gRim`, forward/backward search)
  with closed-form ML estimates — no iterative proportional fitting needed.
- Conditioning variables: nine discretized return characteristics — AGI (14 classes),
  exemptions, filer age, family type, Schedule A and Schedule B indicators, and
  grouped SALT income deduction, real-estate tax deduction, and mortgage interest.
- Validation vs SOI Historic Table 2 (2008): correlations 0.986–0.999 and mean
  absolute relative differences 0.02–0.09 for most variables; real-estate tax the weak
  spot (MARD 0.22).
- **Key caveat (their own):** the conditional-independence assumption breaks when the
  outcome depends on state-specific policy or conditions not captured by X — which is
  precisely the situation when simulating state tax law. Mitigation: include state-
  policy-relevant variables (they included the SALT deduction fields for this reason).
  Also a fit-vs-variance tradeoff limits how many X variables the national sample can
  support.

### 2.4 PolicyEngine US — the open-source structural analogue (⚠ partially unverified)

- Open-source (AGPL) Python rules engine covering federal + all 50 states + DC;
  parameter tree `gov.states.{state}.…` with one directory per jurisdiction. ⚠
- Parameters are YAML with **date-indexed value histories** (step-function semantics,
  handles mid-year changes) and a `reference` metadata field carrying label+URL
  citations to statutes, forms, and instructions (e.g., NY EITC file cites NY Tax Law
  §606(d) and Form IT-201/IT-215 instructions). State EITCs encoded as a match
  percentage of federal with full history (NY: 7.5% 1994 → 30% 2003). ⚠
- Contributor conventions (from their repo docs): cite specific statute/regulation
  sections on every parameter/variable; follow the exact order of operations on the
  state form; state programs self-contained with state-prefixed variable names
  (`il_…`, `ny_…`); per-state unit tests from form worksheets; never hardcode to pass
  tests; a machine-readable `programs.yaml` registry tracks per-program
  `verified_years`. ⚠
- **Validation against TAXSIM is institutionalized:** tests run against TAXSIM-35 on
  every release ($100/tax-unit tolerance on CPS records, federal `fiitax` and state
  `siitax` compared separately). A dedicated `policyengine-taxsim` emulator maps
  TAXSIM I/O with explicit per-state mappings for state intermediate outputs, uses a
  $15 match tolerance with per-state/per-year match-rate dashboards (2021–2024), and —
  under a Sept 2025 MOU with NBER — routes historical years to TAXSIM and 2021+ to
  PolicyEngine. PolicyEngine's usable state coverage starts at tax year 2021. ⚠
- **Enhanced CPS** (their data pipeline, March 2024 beta): impute PUF tax detail onto
  the CPS ASEC with quantile regression forests, then gradient-descent reweighting of
  household weights against ~90–93 administrative targets (hit within ~1%). State-level
  calibration was announced as planned future work at that time. This is the main
  *alternative* to PUF reweighting: switch to a state-identified survey base and pull
  tax detail in.

### 2.5 Jon Bakija's IncTaxCalc

- Comprehensive federal + all-state calculator, 1900–present, explicitly
  parameterizing: state starting point (federal AGI vs federal taxable income vs own
  base), state deductibility of federal taxes, federal SALT deductibility, and
  dependent (federal-linked) itemization rules. Documentation:
  https://web.williams.edu/Economics/papers/bakijaDocumentation_IncTaxCalc.pdf
- Used by TPC as its state calculation engine (§2.2) — strong endorsement of its
  parameterization as a design reference. The web sweep surfaced its existence and
  role but not internal file-format detail; **read the PDF directly when designing the
  state YAML schema** (it is effectively a catalog of every structural switch a state
  parameter system needs).

### 2.6 US Census tax model — gap

The fetch agents for the Census CPS ASEC tax model failed (server errors) before
extraction, so this run produced no verified detail on it. Known context: it simulates
taxes on the CPS for SPM/after-tax income statistics. Revisit directly at
https://www.census.gov/topics/income-poverty/income/guidance/tax-model.html if needed;
it is the least architecturally relevant of the six (aggregate-purpose, not reform
analysis).

---

## 3. Federal–state interaction mechanics (what the model must encode)

**Starting point / conformity (as of ~2023–2024; counts shift over time and must be
parameterized per year):**
- ~31 states + DC start from **federal AGI**; ~5–7 start from **federal taxable
  income** (lists vary by year/source: CO, ID, MT, ND, OR, SC, ± IA/MN/NM); ~5 build
  their **own base** (AL, AR, MS, NJ, PA) while still referencing IRS definitions.
- Conformity regime per state: **rolling** (≈18 + DC), **fixed-date/static** (≈18–19,
  requires pinning the federal-law vintage a state conforms to), or **selective**;
  Maryland has a revenue-triggered hybrid (auto-conform unless cost > $5M, then 1-year
  hold). A state parameter system needs a per-state conformity flag and, for static
  states, the ability to evaluate *a different federal law vintage* for the state base.
- Several states require the **same filing status** on state and federal returns.

**Itemization coupling:**
- In some states the ability to itemize on the state return is contingent on having
  itemized federally — so federal reforms that shift filers to the standard deduction
  mechanically change state itemization. Concrete primary-source example (Nebraska
  Form 1040N instructions, TY2023): federal standard-deduction users *must* take the
  NE standard deduction; federal itemizers take max(NE standard, federal itemized
  minus state/local income tax) — and the back-out uses the **uncapped Schedule A line
  5a** amount, not the $10k-capped 5e, income taxes only. Sub-line federal detail must
  therefore be exposed to the state layer.
- Federal side (CRS RL32781 / 2024 update): SALT deductible only for itemizers; sales
  tax in lieu of income tax is an election; TCJA cut itemization from 31% (2017) to 9%
  (2021) of filers, 99% of remaining itemizers claim SALT; PTE workarounds enacted in
  36 of 41 states with PTE taxes (already partially modeled in this repo).

**Federal tax deductibility (the reverse circularity):**
- As of Jan 2024 (ITEP): **Alabama** (unlimited), **Missouri**, **Oregon**
  (capped/phased out). Iowa, Louisiana, Montana had such deductions historically —
  time-series parameterization required. (TPC 2016 counted 7 states; the set shrinks
  over time.)
- Resolution options: TAXSIM iterates 3 rounds on liability; Feenberg (1987) closed
  form for MTRs; Bakija parameterizes the structural switches. Only a handful of
  states trigger true simultaneity.

**Federal-linked state credits:**
- ~27 states set state EITC as a % of federal EITC (NE: flat 10% TY2023, residents and
  prorated part-year residents only); some states similarly link CTC and CDCTC. The
  2021 ARP EITC expansion automatically expanded conforming state EITCs — federal
  credit amounts must be passed into the state layer.
- ~9 states set their standard deduction equal to the federal amount.

**Rates/brackets landscape (2026, Tax Foundation):** 42 states levy an individual
income tax (8 none); 15 flat, 26 + DC graduated; top rates 2.5% (AZ, ND) to 13.3% (CA).
Multi-year statutory phase-downs are common (NE: 6.64% → 3.99% over 2023–2027 under
LB 873/LB 754) — full time series, not point values.

**Indexation heterogeneity (Tax Foundation primer):** only 17 of 32 graduated-rate
states index brackets. Measures vary: traditional CPI (most), chained CPI (ME, MN, ND,
SC), Metro Phoenix MSA CPI (AZ), California CPI (CA), GDP deflator (IA, OH). Base years
vary (IA 1988, OR 1992Q2, ID 1998, WI 2008, ME 2015); rounding varies (AR nearest $100;
some next-lowest $50). **Implication:** the existing `i_measure` architecture fits, but
the index-series catalog in `generate_indexes()` must grow (state CPI variants, GDP
deflator), and per-state measurement windows may need support.

**Local income taxes:** NYC and Philadelphia are separately administered (separate FTA
entries); Maryland counties and Ohio municipalities exist as a later-phase scope
decision.

---

## 4. Making the PUF state-representative (reweighting)

**The consensus design — and the one matching our proposal — is split weights:** each
national record gets 52 state weights with `Σ_st w_{i,st} = w_i`. Used by both OTA
(TP-6) and TPC. Properties:

- Federal aggregates are *mechanically identical* between with-state and without-state
  runs — the with/without-state mode switch is clean.
- Every state's tax calculation has full record support (no thin-sample states, unlike
  discrete imputation).
- Two estimation flavors documented: TPC's constrained-optimization / Schirm-Zaslavsky
  Poisson regression per AGI stratum vs OTA's log-linear graphical model for
  `p(st|x)`. Both calibrate to **IRS SOI Historic Table 2** (state × AGI-class return
  counts and line-item amounts; CSVs, 1996–2022, ~2–3 year publication lag; DC, PR,
  "Other areas," and a US-total file for reconciliation). Deville & Särndal (1992)
  calibration is the general framework if we roll our own in R.
- **Known weaknesses to design around:** (i) OTA's conditional-independence caveat —
  state weights built on X can't capture outcome variation driven by state policy not
  in X; include SALT-related and state-policy-correlated variables in the conditioning
  set, as OTA did; (ii) real-estate-tax amounts calibrated worst in OTA validation;
  (iii) TPC overestimates state filer counts (federal filers ≠ state filers — model
  state filing thresholds explicitly); (iv) fixed record attributes mean a record "in"
  two states has identical SALT paid, muni interest, etc. — acceptable for weights,
  but state-attribute imputation (e.g., replacing `salt_inc_sales` with computed state
  tax) is where state-specificity actually enters.
- **The alternative** (PolicyEngine): abandon PUF-only, fuse PUF detail onto the CPS
  (which has state codes) via QRF, gradient-descent reweight to targets. More radical
  data surgery, weaker high-income tail, but state IDs are native. For a PUF-based
  shop with an upstream Tax-Data model, split weights are the lower-risk path; the
  PolicyEngine work is the reference for *target construction and loss-based
  calibration* if we want a modern optimizer.

**Where it lives:** state weighting is most naturally an upstream **Tax-Data** product
(a `tax_units_{year}` companion file of state weight columns or a long
state-weight file keyed by `id`), versioned via `interface_versions.yaml` like
everything else.

---

## 5. Primary-source workflow for building state parameters

1. **Encode from forms + instructions first** (TAXSIM's stated practice since 1993 and
   PolicyEngine's contributor rule): the form is a snapshot of law actually in effect;
   follow the form's order of operations when writing `calc_state_*()` functions.
2. **Archives:** NBER maintains a historical state tax forms archive at
   `taxsim.nber.org/historical_state_tax_forms/{STATE}/{YEAR}/` — the source for
   multi-year time series. The FTA directory (taxadmin.org/state-tax-forms/) indexes
   every state DOR's *current* forms (plus NYC, Philadelphia, PR) but has no history.
   State DOR "prior year forms" pages fill gaps.
3. **Statutes** for indexation formulas, phase-down schedules, and conformity language
   (forms show the resulting numbers, statutes show the rule — both needed since our
   YAML stores rules via `i_measure` etc.).
4. **Cross-checks (secondary, never primary):** Tax Foundation annual "State
   Individual Income Tax Rates and Brackets" (itself compiled from statutes/forms —
   good transcription check), their inflation-indexing primer and conformity reports;
   ITEP conformity tables; TPC briefing-book conformity taxonomy.
5. **Citation discipline:** adopt PolicyEngine's rule — every parameter carries a
   reference to the specific form line/statute section. Our YAML has no `reference`
   field convention yet; add one (ignored by the parser, invaluable for maintenance).
6. **Validation:** per-state unit tests built from form worksheets/examples;
   cross-model checks against TAXSIM-35 via `usincometaxes` in R (extending
   `src/tests/test_taxsim.R`), with tolerance bands and known-difference lists
   (PolicyEngine uses $15–$100 tolerances); aggregate checks against SOI Historic
   Table 2 state totals and state revenue-agency estimates where published.

---

## 6. Evaluation of the proposed architecture (the two held questions)

**Proposed design (JI):** a with-state / without-state run option; with-state runs on
the PUF carrying state-specific weights (state identifier constant within each
reweighted copy), matching state income/demographic targets.

**Q1 — How much federal work is done once, not 51×?**
Nearly all of it, by construction. Per-record federal liability is weight-independent,
so the entire federal static pass (and federal MTRs) runs once per record-year; states
differ only in aggregation. The exceptions, in increasing order of cost:

1. *Federal quantities states consume* — AGI, taxable income, itemization status,
   uncapped Schedule A line 5a detail, federal EITC/CTC/CDCTC — are already computed;
   they just need to be exposed to the state layer (return-var plumbing, not
   recomputation).
2. *State→federal feedback via SALT:* under baseline, keep reported `salt_inc_sales`
   in the federal calc (federal pass unchanged and run once); computed state liability
   feeds back only for reforms that change state tax or for SALT-focused analysis.
   When needed, TAXSIM's precedent is 3 fixed federal↔state iterations over liability
   (not amounts paid) — and post-TCJA only ~9% of filers itemize, bounding the
   population where iteration matters.
3. *True simultaneity* (states deducting federal tax) affects only AL/MO/OR currently
   — handle inside the same iteration loop.
4. *Combined MTRs and behavioral feedback* are inherently state-specific; that's per-
   state work by nature, and a scope decision (state-static first).

**Q2 — Is reweighted-PUF-with-state-weights the right structure?**
Yes — it is exactly what both Treasury OTA (TP-6, 2015) and TPC (2016) do, and the
`Σ_st w_{i,st} = w_i` constraint gives the clean with/without-state switch for free.
Alternatives considered and why not:
- *Discrete state imputation per record:* thin samples in small states; breaks the
  exact federal-total identity; TAXSIM's random assignment of high-income records is a
  cautionary tale.
- *CPS fusion (PolicyEngine):* native state IDs but a different microdata foundation
  than the rest of TBL's model stack; weaker top of the distribution; much larger
  data-engineering lift.
Design-arounds to carry into implementation: include state-policy-relevant variables
in the weighting model's conditioning set (OTA caveat); model state filing
requirements explicitly (TPC's filer overcount); treat state-varying *attributes*
(computed state tax replacing `salt_inc_sales`; possibly muni interest) as part of the
state layer, not the weighting layer.

---

## 7. Sketch of the implementation shape (for the plan doc)

1. **Data (upstream/Tax-Data):** state weight matrix per record-year calibrated to SOI
   Historic Table 2 within AGI strata (split-weight method), delivered as an interface
   versioned like other dependencies. Without-state mode ignores it.
2. **Tax law:** `config/scenarios/tax_law/baseline/states/{st}/*.yaml` (or
   `state_{st}_*.yaml`) using existing subparameter/indexation machinery; add new
   index series (state CPIs, GDP deflator) to `generate_indexes()`; adopt a
   `reference:` citation convention. Per-state conformity flags (starting point,
   rolling/static + vintage, itemization coupling, federal-tax deductibility,
   federal-credit match rates).
3. **Calculator:** `src/calc/functions/state/` with `calc_state_*()` functions obeying
   the existing contract; a `do_state_taxes()` called after `do_1040()`; a bounded
   iteration loop (≤3 rounds, TAXSIM-style) engaged only when reforms require the
   SALT/federal-deductibility feedback. Reuse `integrate_rates_brackets()`.
4. **Orchestration:** with-state runs join tax law on `(year, filing_status, state)`;
   either long-format (records × states where weight > threshold) or a loop over
   states reusing the single federal pass. New state outputs added to
   `globals$detail_vars`, totals, and the SLURM aggregate files per the sync table.
5. **Validation:** per-state form-worksheet unit tests; TAXSIM-35 harness in R
   (`usincometaxes`) extending `src/tests/test_taxsim.R`; aggregate benchmarks vs SOI
   HT2 and state revenue agencies. Sequence states: start with a no-tax state + a flat
   AGI-coupled state + one complex state (e.g., CA or NY) to prove the architecture.

---

## 8. Source list

- NBER TAXSIM: https://www.nber.org/research/data/taxsim ; https://taxsim.nber.org/taxsim35/ ;
  Feenberg & Coutts (1993) JPAM 12(1): http://users.nber.org/~taxsim/feenberg-coutts.pdf
- NBER historical state forms archive: https://taxsim.nber.org/historical_state_tax_forms/
- `usincometaxes` R package: https://www.shaneorr.io/r/usincometaxes/
- TPC state model methodology (Urban, 2016):
  https://www.urban.org/sites/default/files/publication/79096/2000697-Incorporating-State-Analysis-into-the-TPCs-Microsimulation-Model.pdf
- TPC briefing book, state conformity:
  https://taxpolicycenter.org/briefing-book/how-do-state-individual-income-taxes-conform-federal-income-taxes
- OTA Technical Paper 6 (Fisher & Lin, 2015): https://home.treasury.gov/system/files/131/TP-6.pdf
- IRS SOI Historic Table 2: https://www.irs.gov/statistics/soi-tax-stats-historic-table-2
- PolicyEngine US repo: https://github.com/PolicyEngine/policyengine-us ;
  TAXSIM validation docs: https://policyengine.github.io/policyengine-us/validation/taxsim.html ;
  TAXSIM emulator: https://github.com/PolicyEngine/policyengine-taxsim ;
  NBER MOU: https://www.policyengine.org/us/research/policyengine-nber-mou-taxsim ;
  Enhanced CPS: https://www.policyengine.org/us/research/enhanced-cps-beta ;
  data docs: https://policyengine.github.io/policyengine-us-data/
- Bakija IncTaxCalc documentation:
  https://web.williams.edu/Economics/papers/bakijaDocumentation_IncTaxCalc.pdf
- ITEP: federal deduction for federal taxes paid: https://itep.org/federal-income-tax-deduction-state-income-tax/ ;
  conformity explainer: https://itep.org/how-does-federal-state-tax-conformity-work/
- Tax Foundation: 2026 rates/brackets: https://taxfoundation.org/data/all/state/state-income-tax-rates-2026/ ;
  inflation-indexing primer: https://taxfoundation.org/research/all/state/inflation-adjusting-state-tax-codes/ ;
  TCJA conformity: https://taxfoundation.org/research/all/state/state-conformity-federal-tax-reform/
- CRS RL32781, Federal Deductibility of State and Local Taxes: https://www.congress.gov/crs-product/RL32781
- FTA state forms directory: https://taxadmin.org/state-tax-forms/
- Nebraska Form 1040N booklet TY2023 (primary-source worked example):
  https://taxsim.nber.org/historical_state_tax_forms/NE/2023/
- Deville & Särndal (1992), Calibration Estimators in Survey Sampling, JASA 87(418)

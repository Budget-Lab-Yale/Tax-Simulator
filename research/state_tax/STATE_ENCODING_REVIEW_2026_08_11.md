# State encoding review: holes, archetypes, and the path to completion

**Date:** 2026-08-11
**Scope:** the 30 coded jurisdictions, as of `ab45a6661` (state-tax branch);
§2.1 and R6 (added 2026-08-11) map the 21 remaining jurisdictions into the
same class structure and sequence their rollout
**Sources:** research/STATUS.md, research/state_tax/state_parameter_rollout.csv, research/state_tax/state_parameter_workflow.md,
research/state_tax/notes/state_data_imputation_plan.md, research/state_tax/notes/elderly_retirement_provisions.md,
research/state_tax/CODE_REVIEW_2026_07_17.md, jurisdictions.yaml, conformity_groups.yaml, all 30
source packets, all `documented_not_modeled:` YAML blocks,
src/tests/state/test_state_calc.R (160 worksheet cases),
test_state_cross_model.R, and cross_model/results (summary.csv, 27 triage
reports, known_differences.csv).

**The bar we set for ourselves** (research/state_tax/state_parameter_workflow.md): a state is not
production-ready "merely because its directory parses" — it must clear source
packet → YAML → worksheet tests → known-differences notes → record-level
cross-model → aggregate validation. By that bar **no state is complete**: all
30 sit at `aggregate = blocked_weights`, and only IL has cleared every
weights-independent gate. That is not a criticism of the work — the gating is
deliberate — but it frames everything below.

Headline counts: 30 encoded (18 broad-IIT + NH/TN narrow + WA excise + 6
zero-tax stubs + CA/SC/VA encoded-but-disabled), 27 enabled for `states=all`,
21 jurisdictions untouched. 160 worksheet cases across 24 states. Cross-model
runs exist for all 30; 9 states at `cross_model = done`, 21 `in_progress`.

---

## 1. Biggest holes

Ranked by how much they distort or block results, not by effort to fix.

### 1.1 State weights are still the uniform placeholder

Every state total the model emits is currently meaningless at the state level
(federal results are unaffected). Phase 1 is close — engines and data done,
2022 full fit at 82.9%-within-2% (MARD 1.43%) against a ≥99% bar — but the §4
comparison harness, the β sweep, the `state_weights_{year}.csv` writer, the
2013/2015 HT2 gap decision, and the dispatcher swap all remain. This blocks
the aggregate gate for **all 30 states**, plus every downstream benchmark
(HT2 total-tax comparisons, revenue-agency checks) and the Tier-1 imputations
sequenced behind it. Single biggest lever in the whole workstream.

### 1.2 Cross-model validation is far from done, and its bookkeeping has bugs

Only IL among the 18 broad-IIT states has cleared the record-level gate
(TAXSIM 1.000 all four years; PE 0.992–0.995). Thirteen broad states sit in a
27–86% clean-match@$100 band. Worst: KY 0.273 (2017), GA 0.504–0.540,
SC 0.476–0.556, MN 0.491–0.673 (TAXSIM window). Beyond the raw rates:

- **18 of 24 broad/narrow states have zero state-specific rows in
  known_differences.csv** (AZ CA CT GA IN KY MD MI MN NC ND NH NY SC TN UT VA
  WI). The acceptance criterion requires every residual cluster mapped to a
  KD row or a filed bug, so these states *cannot* pass as documented even
  where their rates are good.
- **`cross_model_states()` in test_state_cross_model.R omits MD, MN, and WI
  from every class** — `class_of()` returns empty, so the report writer
  silently skipped them. Their summary.csv rows exist but
  `results/reports/{md,mn,wi}.md` were never generated; the triage narrative
  lives only in tracker notes. A two-line fix plus `--report-only`.
- **Undiagnosed PE-window collapses:** NY 2023 at 0.160 (vs 0.833 in 2022) and
  VA 2023–24 at 0.345/0.349 (vs 0.940 in 2022) are discrete breaks, not
  noise, and no report or KD row explains them.
- **ID's PE window (0.431–0.582) is scored against a known-bad benchmark** —
  PE doesn't net the grocery credit; the packet flags a whole-window
  exclusion as the fix but the KD row is still only `annotate`.
- **Single-benchmark verdicts:** CO's entire PE window is excluded (TABOR
  netting) so it rests on a 0.575–0.656 TAXSIM window; WA's entire TAXSIM
  window is excluded so it rests on four PE cells at 0.809–0.863.
- **AZ PE 2021 has a mean-abs-diff of ~$819k** (p99 $14.3M) — one or a few
  pathological records, untriaged.
- PE clean subsets are small (~270–365 records/cell); single records move a
  cell ~0.3pp. Fine for triage, worth remembering when reading rates.

### 1.3 Conformity bridges don't exist

All four positive conformity groups are `ready: false` with empty
`reference_tax_law_id` — no reference-law bridge has been built for any of
them. Consequences:

- **CA, SC, VA are excluded from `states=all`** and hard-stop any
  federal-reform run (by design — the guard works). These are three of the
  largest remaining IIT bases; CA alone dwarfs most of what's encoded.
- The quieter risk: **ID, MN, OH, WI (and KY de jure) are fixed-date states
  modeled as rolling** by documented judgment call. Each call is defensible
  for baseline law, but in a large federal reform these states will
  *silently* apply scenario federal law rather than hard-stopping. Worth a
  standing caveat on any federal-reform output that includes them.

### 1.4 A systematic one-sided retiree bias

Source-restricted retirement subtractions (military / public / railroad /
police-fire pensions) are omitted in roughly a dozen states because pension
*source* is unobservable in the PUF: CT, ID, MD, MI (public share), MN, NC
(Bailey + military), ND, SC, UT, VA (the ramping Military Benefits
Subtraction, material and growing from TY2022), WI, NY (`govt_pension_full_sub`).
Every one of these omissions biases modeled liability **up** for affected
retirees — the errors do not offset. Named open items from the elderly
survey: **SC's $15k age-65 deduction** (offset $1:$1 by the retirement
deduction — the survey's "second-largest gap", needs a pension-offset
coupling on the VA-style age machinery) and **IN's income-tested $500
elderly exemption**. Note the survey itself covers only 21 jurisdictions and
predates MD/MN/PA/ID/WI — it should be refreshed against all 30.

### 1.5 Enacted law that isn't encoded yet

- **SC TY2026 (H.4216 / Act 110) is a structural rewrite** — federal-AGI
  start, the SC Income Adjusted Deduction, a 1.99%/5.21% schedule, a $200
  EITC cap. Our 2026+ SC simulations project 2025 law and are wrong in
  *structure*, not just detail.
- ND TY2026 rate schedule (ND-1ES published) not encoded; 2026 projects 2025.
- WI 2025 Act 15 $24k/$48k retirement election with credit forfeiture —
  deferred because it needs generic min-liability election machinery.
- CT TY2026 family child care home credit ($500, PA 25-168 §372).
- OH 2026 $500,000 JFC/exemption boundary semantics TBD pending guidance.

### 1.6 Input-data gaps (the imputation plan's inventory)

The 2026-07-24 imputation plan already triages these well; the headline items
that bind hardest on already-coded states:

- **Tenure/rent/property-tax family (Tier 1):** WI and MI homestead credits,
  the renter side of WI's school property tax credit, property tax for
  non-itemizers (IL/CT credits, MD/MN deduction bases) — and the **MN renter
  credit (2024+), which is ON the M1 and therefore a structural liability
  gap**, not a side program. PE includes it; we can't.
- **Pension source split (Tier 1):** unlocks the entire §1.4 list.
- **Household-resources income (Tier 1):** PA Tax Forgiveness eligibility
  income currently **overstates forgiveness**; also MD/VA poverty credits,
  ID grocery proration.
- **Elective deferrals (Tier 2):** PA taxes 401(k)-type deferrals when made;
  PUF Box-1 wages understate the PA base — the single largest PA
  known-difference (shared by TAXSIM/PE, so invisible to cross-model).
- **Everywhere:** `sub_us_int` is carried as a flag in every state and
  subtracted nowhere; own-state muni shares run on a 75% convention; all
  state credits assume 100% take-up.

### 1.7 Test coverage is skewed and the coverage layer was never built

Worksheet cases per state range from VA 13 / MN 11 / CA 11 down to **KY 1 and
IN 1** — and KY has the repo's worst cross-model rate with a
deductions-dominant mismatch, while IN's dominant mismatch stage
(exemptions) is the one thing its single test covers. Code-review item #9 —
a coverage assertion (every modeled parameter family exercised by ≥1
worksheet case) and continuity/cliff sweeps — is the **only open item from
the 2026-07-17 review**, and its cost grows with every state added. The
112-unit smoke grid asserts structure only (finite, non-NA) and **excludes
NH, TN, and WA entirely**.

### 1.8 Locality taxes are out of scope but the consequence should be stated

Deliberate Phase-7 deferrals, but for four states the omission is first-order
for anyone reading "state" liability as tax burden: MD county piggyback
(2.25–3.3% — the flagship), OH school-district + municipal taxes, IN county
tax, PA Act 32 EITs + Philadelphia wage tax (~1–4% of wages), NY
NYC/Yonkers/MCTMT. `liab_st_iit` for these states structurally understates
the state-local income-tax burden.

### 1.9 Bookkeeping drift

Small but worth clearing so the trackers stay trustworthy: research/STATUS.md header
says 2026-07-13 with body content through 07-24 and stale jurisdiction counts
("25 → 27", "28th" — actual is 30); `oh.md` and `ut.md` packets still say
"encoding pending" though both are encoded, tested, and enabled; the six
zero-tax stubs sit at `source_packet = in_progress` though their packets
exist on disk; NC and IN encode through TY2026
while other states stop at TY2025; the CA worksheet numbering skips CA-7.

---

## 2. Archetypes

Seven structural families cover all 30 jurisdictions. The families matter
because they predict encoding cost: the machinery is built per-family, and
the marginal state within a family is mostly parameter transcription.

| Family | States | Core structure |
|---|---|---|
| A. Zero-tax stub | AK FL NV SD TX WY | No broad IIT; registry entry + zero assertions |
| B. Narrow / special base | NH, TN, WA | NH interest+dividends (repealed 2025), TN Hall (repealed 2021), WA LTCG excise + WFTC |
| C. Federal-AGI start, flat rate | IL IN MI KY NC AZ(2023+) GA(2024+) | Thin adjustment layers over FAGI; exemptions/credits do the distributional work |
| D. Federal-AGI start, graduated | NY CA MD MN WI CT VA | Where nearly all exotic machinery lives |
| E. Federal taxable-income start | CO ND SC ID (+MN 2017) | Federal deductions flow through; SALT addback family |
| F. Own base | PA | Eight gross income classes, class loss floors, Tax Forgiveness |
| G. Credit-in-lieu-of-deductions | UT | Deductions/exemptions enter only via the phased Taxpayer Tax Credit |

Notes per family:

- **A** is free. **B** cost one profile flag and per-state base proxies; both
  NH and TN carried repeal paths, which exercised year-keyed machinery.
- **C** looks easy and mostly is — but three of its members are
  graduated→flat converts (AZ 2023, GA 2024, plus ID's 2025 flattening over
  in family E), so full year-keyed schedule machinery is required anyway; and
  the family's cross-model rates are among the *worst* (KY, GA, IN), because
  simple structure pushed the complexity into credits and adjustments that
  external models handle inconsistently.
- **D** drove most generic components: CT stepped recapture + Table E
  percentage credit + AGI-band factor tables; NY benefit recapture +
  independently-elected pre-TCJA itemization; MD dual-EITC election +
  capital-gains surtax + 15%-of-AGI standard deduction; MN year-keyed
  `start_point` + the TY2018 nonconformity rebuild + two-tier Pease; VA
  single-schedule + Spouse Tax Adjustment + age-package/EITC exclusivity; WI
  sliding standard deduction + itemized-deduction-as-*credit*.
- **E** is the family where federal reforms hit twice (base + own
  parameters); its states share the SALT-addback and exclusion-share
  machinery (`cap_gains_excl_share`, `div_excl_share`).
- **F (PA)** validated that `start_point = 0` own-base accounting works, at
  the cost of the module's biggest data known-differences (deferrals,
  eligibility income). NJ will reuse this.
- **G (UT)** proved the credit-in-lieu pattern plus return-level credit
  exclusivity (SS vs retirement credit, take-the-larger).
- **Conformity is a second, orthogonal axis:** rolling (group 0, 27 states) /
  fixed-date-annual (SC; VA through 2022; KY de jure) / fixed-selective (CA,
  groups 1–2) / re-fixed (VA 2026+, group 4) / rolling-by-judgment (ID MN OH
  WI). Only group 0 is `ready: true`.

**The encouraging takeaway:** each new family forced 1–4 reusable components
into `st_utils.R` / `credit_tables.csv` / the schema (band/step primitives,
income-base enum, election machinery, dense-table lookups), and the marginal
state now mostly reuses them — evidenced by PA/ID/MN/MD/WI landing in a
single collaborator pass in July.

### 2.1 The remaining 21 jurisdictions, mapped (preliminary)

Classifications below are for sequencing only — from secondary knowledge
(reform-pivotal cases spot-verified 2026-08-11: IA federal-deduction
flow-through from TY2023 + flat 3.8% from TY2025 per SF 2442; MT federal
taxable-income start + two brackets from TY2024 per SB 399; LA flat 3% from
TY2025). Every state still gets its primary-source packet before encoding;
the form remains the operational truth. Fast-moving items to re-verify at
packet time are flagged ⚑.

One genuinely new mechanism appears: **federal income-tax deductibility**
(deducting federal liability from the state base) — full in AL
(constitutional), capped/AGI-phased in MO and OR, and in the pre-reform
years of IA (through 2022), LA (through 2021), and MT (through 2023). It is
a feature overlaying families rather than an eighth family, and one generic
component (`fed_tax_ded` with cap + phase-out params) covers all six. It is
mechanically clean for us — federal liability is already computed upstream.

| St | Family (prelim) | Structure and encoding notes |
|---|---|---|
| NJ | F own-base | Gross income tax: income classes (no cross-class netting, no carryovers) + graduated to 10.75%; reuses PA `ob_*` but with a rate schedule. Pension exclusion to $100k with the income cliff (banded machinery exists); property-tax deduction/credit election (renters 18%-of-rent — Tier 1 data); NJ EITC 40%. ⚑ NJ taxes 403(b)/457 employee deferrals but not 401(k) — a different deferral wrinkle from PA's |
| MA | F own-base (schedular) | Part A/B/C classes at different rates: 5% wages/interest, short-term gains at their own rate, LTCG 5%; 4% millionaire surtax on taxable > $1M from 2023 (CA/MD/MN surtax machinery reuses); no-tax status + limited income credit (VA `no_tax_below` precedent). Extends PA `ob_*` to per-class rates |
| AL | F own-base + full fed-tax ded | Own income concept, graduated 2/4/5%, FULL federal income-tax deductibility (constitutional); sliding standard deduction and dependent exemption by AGI tier |
| AR | F own-base | Own income concept, graduated (top cut repeatedly, 3.9% by 2024 ⚑); low-income alternative tax tables → credit_tables.csv dense-lookup |
| MS | F own-base (flat) | Simple own base; 4% bracket phased out; 4.7% (2024) → 4.4% (2025) → 4.0% (2026); ⚑ 2025 legislation sets a trigger path toward elimination — encode enacted rates, triggers document-only (NC precedent) |
| IA | E (2023+) / own-base + full fed ded (2017–22) | Multi-regime: pre-2023 own AGI with full federal deductibility and own deductions; 2023+ federal standard/itemized flow through (taxable-income conformity); flat 3.8% from 2025. Retirement income fully excluded 55+/disabled from 2023. MN's year-keyed `start_point` precedent |
| MT | E (2024+) / own-base + capped fed ded (2017–23) | SB 399: federal taxable-income start, two brackets (4.7/5.9%) from TY2024, state filing status must match federal (convenient — PUF units are federal); pre-2024 seven brackets + $5k/$10k capped federal deduction + own std. 65+ $5,500 subtraction 2024+ |
| LA | C (2025+) / D (2022–24) / +fed ded (2017–21) | Federal-AGI start throughout; full federal deductibility through 2021 (constitutional; repealed with the 2022 reform ⚑ verify mechanics), graduated 1.85/3.5/4.25% 2022–24, flat 3% + $12.5k/$25k std from 2025; excess-federal-itemized deduction pre-2022 |
| MO | D + capped fed ded | Micro-bracket schedule, top ~4.7% 2025 (SB 3 cuts + triggers ⚑); federal deduction capped $5k/$10k and AGI-phased; public-pension exemption income-capped; MO std = federal |
| OR | D + capped fed ded | Federal-AGI start, graduated 4.75–9.9%; federal-tax subtraction capped (~$8k) and phased out at high AGI; own std; EITC 9/12% by child age. ⚑ Kicker credit is a % of PRIOR-year liability (44.28% in 2024) — prior-year liability is unobservable per-record: likely document-only known difference in kicker years (material!) |
| DC | D | Graduated to 10.75%; EITC match stepping 70% → 100% by 2026 incl. childless ⚑ (schedule); federal-coupled std/itemized |
| DE | D | To 6.6%; pension exclusion; EITC election: 20% nonrefundable vs 4.5% refundable — VA's `eitc_match_alt` greater-of machinery is exactly this |
| HI | D | Twelve brackets to 11%; ⚑ 2024 Act 46 standard-deduction ramp through 2031; employer-funded pension exclusion (funding source unobservable — share proxy, joins the §1.4 list); refundable EITC 20% (2023+); food/excise credit |
| KS | D | Two brackets from TY2024 (was three); ⚑ TY2017 hazard: SB 30 raised rates RETROACTIVELY mid-2017 and repealed the pass-through exemption — encode final enacted 2017 law |
| ME | D | 5.8/6.75/7.15%; itemized cap with phase-out; large pension deduction (growing schedule ⚑); property tax fairness + sales tax fairness credits are refundable and rent/property-linked → Tier 1 imputation |
| NE | D | Top rate ramp to 3.99% by 2027 (LB 754); SS exemption ramp to 100% by 2025; refundable EITC 10%; ⚑ school-district property-tax income-tax credits (LB 34) are property-linked → Tier 1 or document |
| NM | D | 2025 bracket restructure (HB 252) to six brackets 1.5–5.9% ⚑; refundable WFTC 25%; SS exemption income-capped 2022+; tiered refundable child credit (banded machinery exists) |
| OK | D | Micro-brackets top 4.75%; ⚑ 2025 act cuts to 4.5% top from TY2026 + trigger path; ⚑ OK's refundable EITC is computed on FROZEN ~2020 federal EITC law — a mini fixed-conformity case; candidate reuse of the group reference-law machinery rather than a one-off |
| RI | D | Three brackets to 5.99%; std/exemption phase-out at high AGI; EITC 15% refundable; SS/pension exclusions income-capped |
| VT | D (2018+) / E (2017) | Switched from federal taxable-income start to AGI start in TY2018 (Act 11) — year-keyed `start_point`, MN precedent; own std/exemptions post-2018; EITC 38%; capital-gains exclusion (flat or 40%, capped — ND/SC/WI machinery) |
| WV | D | Five brackets; 2023 HB 2526 −21.25% + annual trigger cuts ⚑ (verify each year's certified rates); SS exemption phase-in; low-income earned-income exclusion; family tax credit |

Machinery inventory for the remainder, in build order of value: (1) the
`fed_tax_ded` component (unlocks AL/MO/OR + the IA/LA/MT pre-reform years);
(2) per-class rates on the PA `ob_*` base (MA, and NJ's schedule-on-classes);
(3) nothing else new — every other feature maps to an existing component,
and four more states (ME, NE, NJ renters, WV) queue behind the Tier-1
homestead-family imputation for one credit each. The OR kicker and the OK
frozen-EITC are the two oddities to decide deliberately rather than
discover mid-encoding.

---

## 3. Completion roadmap for the coded states

Sequenced by dependency. R0–R2 are weights-independent and can run in
parallel with R3.

### R0 — Hygiene (hours, do first)

1. Add MD/MN/WI to `cross_model_states()` and re-run `--report-only` →
   generates the three missing triage reports.
2. Refresh research/STATUS.md (header date, jurisdiction counts, MD/WI/OH/UT/VA in the
   rollout narrative); fix `oh.md`/`ut.md` "encoding pending" lines; flip the
   six stub `source_packet` cells to `done`; note the NC/IN
   TY2026-vs-TY2025 horizon inconsistency (either extend everyone or
   document the two). (Resolved during this review: the OH/IL "duplicate"
   summary.csv rows are by design — each cell with `exclude`-type known
   differences gets a second all-NA row counting the excluded records.)

### R1 — Validation close-out (the bulk of remaining effort)

1. **Per-state triage to the acceptance bar** for the 13 below-bar broad
   states: every residual cluster becomes a known_differences.csv row
   (`exclude`/`annotate`) or a filed bug. Suggested order (worst-first,
   weakest tests first): **KY** (write worksheet cases before triaging — the
   dominant mismatch is the deductions stage and KY has one test), GA, SC,
   MN, CO, PA, NY, VA, MI, MD, IN, ID, AZ.
2. Diagnose the discrete PE breaks: NY 2023 (0.160), VA 2023–24
   (0.345/0.349), GA 2021 (0.448); triage the AZ 2021 $819k outlier records.
3. Promote ID's grocery-credit KD row from `annotate` to a PE-window
   `exclude` (or net the credit out in `pe_state_tax.py`).
4. **Review item #9:** the parameter-family coverage assertion (every family
   in the registry exercised by ≥1 worksheet case — KY/IN fail it today) and
   per-state continuity sweeps (monotonicity except declared cliffs; the
   declared-cliff list already exists in OH/CT packets). Add NH/TN/WA to the
   smoke grid.

### R2 — Law-gap closure within coded states (weights-independent)

Priority order by materiality:

1. **SC age-65 $15k deduction** with the retirement-deduction offset
   (12-6-1170(B)) — largest open elderly item; extends VA-style age
   machinery with an offset coupling.
2. **SC TY2026 restructure (H.4216/Act 110)** — without it every 2026+ SC
   number is structurally wrong. Consider gating SC 2026+ (error) until done.
3. **ND TY2026 schedule; CT TY2026 child-care-home credit; OH 2026 boundary
   semantics** (when guidance publishes).
4. **IN income-tested elderly/blind exemptions** (needs the generic
   income-tested-exemption feature already sketched in the survey).
5. **CO child-care expenses credit (DR 0347)** — the oldest carried TODO —
   plus the CO 2026 TABOR rate revisit (~Sept 2026 certification).
6. **WI Act 15 retirement election** — build the generic min-liability
   election pass (also needed later for sales-tax electors).
7. Verification flags: MN childless WFC phase-out rate (unverified 9%), ND
   2019 SS-cap enacting bill, KY 2017 Schedule ITC Table C transcription,
   OH SB 18 UI mechanics primary source.

### R3 — Weights (Phase 1 close-out)

§4 comparison harness → joint-fit tuning to the ≥99%-within-2% bar (SALT and
EITC/AGI families carry the residual) → writer + projection carry-forward
(decide 2013/2015 HT2 gaps) → dispatcher swap off `placeholder`. Unlocks the
aggregate gate for all 30 states and the HT2/revenue-agency benchmark layer,
which is the real test of the encodings at scale.

### R4 — Conformity bridges

In order of value-per-effort: **group 3** (fixed-date annual — unblocks SC
and VA-through-2022 with one bridge), then **group 4** (VA 2026+), then **CA
groups 1+2** (fixed-2015/2025 selective — largest payoff, hardest build).
Each bridge = a reference-law overlay under `tax_law/` + validation cases
proving form-line fidelity, per the conformity-groups contract ("federal
reform runs must fail rather than use scenario federal outputs" until then).
Also decide whether ID/MN/OH/WI/KY keep their rolling-by-judgment treatment
under large federal reforms or get a warning surface.

### R5 — Data imputations (post-weights, per the plan's own sequencing)

Tier 1 first: tenure/rent/property-tax family (unblocks WI+MI homestead, MN
renter credit — the one structural liability gap in the set), pension source
split (clears the §1.4 retiree bias across ~12 states), household-resources
income (fixes PA forgiveness overstatement). Then Tier 2 alongside the states
that need it: elective deferrals (PA base), dependent detail (MN CTC/M1CWFC,
MD), disability status, interest-composition shares. Every imputation stays
switchable-off so the cross-model harness keeps comparing law-only.

### R6 — Remaining-state rollout (sequenced batches; see §2.1 for the map)

Weights-independent like R1/R2; batches ordered so each new component is
built once and immediately reused, largest revenue first within a batch:

1. **Batch C-transcription (no new machinery):** the eleven plain
   graduated-federal-AGI states — DC, DE, HI, KS, ME, NE, NM, OK, RI, VT,
   WV. Pure packet-and-parameter work; VT needs only the year-keyed
   `start_point` for its 2017 taxable-income year. Decide the OK
   frozen-federal-EITC treatment (group machinery vs one-off) before OK.
   ME/NE property-linked credits: encode the rest, document the credits as
   Tier-1-blocked (WI/MI homestead precedent).
2. **Batch fed-ded (build `fed_tax_ded` once):** MO and OR (capped/phased
   variants), then AL (full deductibility, own-base). Decide the OR kicker
   treatment (document-only vs same-year approximation) up front.
3. **Batch own-base (reuse PA `ob_*`):** NJ first (largest un-started IIT;
   needs per-class rate support), then MA (schedular + millionaire surtax),
   then AR and MS (simple own-base).
4. **Batch multi-regime:** IA, LA, MT — each spans a structural reform
   inside the 2017+ window and consumes both the `fed_tax_ded` component
   (pre-reform years) and the year-keyed `start_point` (MN/VT precedent).

R3–R5 remain shared prerequisites for meaningful aggregates in all of them;
none of this batch order depends on weights.

---

## Appendix — per-state status table

Worksheet = hand-computed cases in test_state_calc.R. TAXSIM = clean
match@$100 range 2017–2020; PE = clean match@$100 range 2021–2024 (small
samples, ~270–365/cell). KD = state-specific known_differences.csv rows.

| St | Family | Tests | TAXSIM | PE | Report | KD | Top open items |
|---|---|---|---|---|---|---|---|
| AK | A stub | 0* | 1.00 | 1.00† | yes | 1 | — (†PFD imputation breaks PE clean subset; raw rate binds) |
| FL | A stub | 0* | 1.00 | 1.00 | yes | 0 | — |
| NV | A stub | 0* | 1.00 | 1.00 | yes | 0 | — |
| SD | A stub | 0* | 1.00 | 1.00 | yes | 0 | — |
| TX | A stub | 0* | 1.00 | 1.00 | yes | 0 | — |
| WY | A stub | 0* | 1.00 | 1.00 | yes | 0 | — |
| NH | B narrow | 3 | 1.00 | .958–.982 | yes | 0 | annuity/distribution base proxy; disability exemption |
| TN | B narrow | 8 | .995–.998 | 1.00 | yes | 0 | source/ownership exclusions; senior total-income measure |
| WA | B excise | 5 | excluded | .809–.863 | yes | 1 | kg_lt proxy; family-business ded; WFTC take-up; not in smoke grid |
| IL | C flat | 4 | **1.000** | .992–.995 | yes | 2 | **PASS.** K-12 credit; property-tax credit eligibility; all-muni addback proxy |
| IN | C flat | **1** | .669–.675 | .849–.876 | yes | 0 | county tax; elderly/blind + first-year-dep exemptions; renter ded; exemptions-stage mismatch |
| KY | C flat | **1** | **.273–.476** | .725–.859 | yes | 0 | Table C MGI base mismatch; pension source; $40 age/blind credits; worst state in module |
| MI | C flat | 10 | .639–.647 | .563–.684 | yes | 0 | homestead credit; public-pension source; +$386 TAXSIM point mass open |
| NC | C flat | 4 | .535–.750 | .934–.970 | yes | 0 | Bailey + military pensions; credits.yaml is a placeholder (no credits); 2018 outlier |
| AZ | C flat | 3 | .549–.801 | .693–.883 | yes | 0 | low-income family/excise/property credits; PE-2021 $819k outlier untriaged |
| GA | C flat | 3 | .504–.540 | .448–.920 | yes | 0 | IND-CR credits; state-income-tax itemization adjustment; retirement eligibility detail |
| NY | D grad | 4 | .532–.631 | .160–.833 | yes | 0 | NYC/Yonkers; tuition credit; govt-pension sub; dep-care table anchors; **2023 PE collapse undiagnosed** |
| CA | D grad | 11 | .610–.728 | .843–.908 | yes | 0 | **disabled** (groups 1/2); FYTC/renter/AMT; Schedule CA incomplete; tracker yaml+tests `in_progress` |
| MD | D grad | 9 | .582–.700 | .839–.889 | **no** | 0 | county piggyback deferred; military/public-safety pensions; poverty credit; century club |
| MN | D grad | 11 | .491–.673 | .752–.822 | **no** | 0 | **renter credit 2024+ (structural)**; MN AMT doc-only; QPEN/military; 2021 CDC overstated; WFC rate unverified |
| WI | D grad | 7 | .758–.923 | .720–.777 | **no** | 0 | homestead credit; military/pre-1964 pensions; $500 cap-loss limit; Act 15 election |
| CT | D grad | 8 | .859–.901 | .773–.885 | yes | 0 | teacher/military/RR pensions; motor-vehicle property tax; TY2026 credit |
| VA | D grad | 13 | .597–.714 | .345–.940 | yes | 0 | **disabled** (groups 3/4); military subtractions (ramping); disability sub; **2023–24 PE collapse undiagnosed** |
| CO | E fed-txbl | 5 | .575–.656 | excluded | yes | 1 | child-care credits (DR 0347); FATC linear proxy; single-benchmark verdict; 2026 TABOR revisit |
| ND | E fed-txbl | 8 | .733–.788 | .464–.874 | yes | 0 | Marriage Penalty + Family Member Care credits; US-obligation sub; **TY2026 schedule not encoded** |
| SC | E fed-txbl | 7 | .476–.556 | .524–.816 | yes | 0 | **disabled** (group 3); age-65 deduction; **TY2026 H.4216 restructure not encoded**; under-6 doubling |
| ID | E fed-txbl | 7 | .590–.708 | .431–.582 | yes | 5 | retirement-benefits + cap-gains deductions; 529/MSA; grocery proration; PE benchmark known-bad |
| PA | F own-base | 9 | .601–.626 | .812–.877 | yes | 1 | elective deferrals (base); forgiveness eligibility income; Schedule O; local EIT out of scope |
| UT | G credit-in-lieu | 9 | .789–.869 | .958–.981 | yes | 0 | MAGI proxy; qualified-exempt floor; military retirement credit; my529 |
| OH | C/D hybrid | 10 | .909–.913 | .966–.987 | yes | 4 | SDIT/municipal deferred; uniformed-services retirement; BID definition; 2026 boundary TBD; closest to PASS |

\* stubs are covered by the zero-assertion loop (liab = 0, st_filer = FALSE,
all grid units × years), not hand-computed cases.

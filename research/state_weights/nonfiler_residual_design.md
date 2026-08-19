---
title: "Non-Filer Estimation — Residual-Methodology Redesign (Design Memo)"
role: method
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Non-Filer Estimation — Residual-Methodology Redesign (Design Memo)

**Date:** 2026-08-16. Consolidated 2026-08-19 — amendments folded into the body,
so the text below says what is true now rather than what was thought when drafted.
Revision history at the end.

**Status.** **Stage D executed** (`4783dc3e9`) — anchors built for TY2017/2022,
diagnostic tables T1–T7 run, findings in `research/state_weights/nonfiler_residual/04_findings.md`,
decisions D1–D6 resolved. **Pre-flight is closed** (2026-08-19): the SSA inputs
are in the store and documented, the age bands and anchor tolerance are settled
in code, and the dependents/MFS and covered-worker-universe decisions are taken.
Sections 5–7 remain design; the only code that exists is the Stage-D harness,
`08_residual_tolerance.R`, and the band functions in `state_weights.R`.

**What this memo is for.** It re-considers how non-filers are estimated in (1)
the national production PUF built by Tax-Data and (2) the state split-weights fit
on this branch, and designs how the income memo's residual methodology —
non-filing adults as a population-minus-filers residual, disciplined by SSA age
and earnings margins, imposed jointly with weight calibration — applies to both.
It is the **method of record**: the *why*, and the design decisions with their
evidence.

**It is not the task list.** For what is decided, what is next and what is
blocked, see **`research/state_weights/plan.md`**, which is the single
operational entry point for this workstream.

**Prompted by:** the Affordability-Index income memo
(`research/docx_sources/income_memo_affordability.docx`, "Filing status",
"Calibration A — the universe mismatch", and "Aligning the code" sections), which
specifies a residual non-filer methodology and explicitly asks that its upgraded
filing model replace the v0 rule the state weights currently rest on. That file is
a **copy taken into this repo as an input**, not the canonical document — the
Affordability-Index repo owns it; this copy exists so the citation resolves.

**Companions:** `research/state_weights/plan.md` (the plan),
`research/state_weights/nonfiler_residual/04_findings.md` (Stage D evidence),
`research/state_weights/nonfiler_residual/05_filing_model_literature.md` (the literature pass),
`raw_data/SSA-{OASDI,EEDATA}-SC/NOTES.md` (what the SSA margins mean),
`research/state_weights/state_weights_phase1_summary.md` (bake-off record),
`research/state_weights/notes/state_weights_fit_issues.md` (v0 filing-model bias note),
`research/state_tax/plan.md` §2.1 (weights design).

**Code discussed:** `src/data/state_weights.R` (Tax-Simulator, this branch);
`src/impute_nonfilers.R`, `src/project_puf.R` (Tax-Data, main @ d5f1f51,
production vintage `2026070814`).

## 1. Purpose and scope

Two systems currently carry a non-filer population, and neither count is
anchored to anything:

- **Tax-Data** appends ~27.6M (TY2022) non-filing tax units from PSZ/DINA to
  the PUF. Their weights are DINA's own, never calibrated; every target in
  `config/target_info/baseline.csv` is a filer concept.
- **Tax-Simulator's state weights** place those units across states using
  ACS margins built with a v0 filing rule known to over-assign filers by ~7%
  nationally, with a 20pp state spread.

The income memo's residual methodology gives both systems the anchor they
lack: non-filing adults by state and age = Census PEP resident population
minus filing adults derived from HT2 by filing-status identities, age-shaped
by OASDI beneficiary counts and earnings-shaped by SSA covered-worker tables.
(The income memo subtracts group quarters at this step; **we do not** — §3.0
explains why the PUF universe makes that subtraction wrong here.) This memo
translates the method into these two codebases.

## 2. Current state

### 2.1 Tax-Data: the PSZ append

`src/impute_nonfilers.R` (148 lines) reads `usdina2017.dta`, filters to
DINA's `filer == 0`, collapses person rows to 13,204 tax units, and
`bind_rows`-appends them to the 2017 PUF (`impute_nonfilers.R:145-146`).
Pipeline position: `main.R:41`, after the LP reweight
(`create_2017_puf.R`), before `impute_variables.R`. Weights are DINA's tax-
unit weights, `dweghttaxu / 1e5` (`impute_nonfilers.R:22`), unadjusted.

Resulting non-filer mass (production vintage `model_data/Tax-Data/v1/
2026070814/baseline`, verified 2026-08-16): 26.18M units in 2017, 27.62M in
2022, 32.54M in 2035. Filing-status mix (2022): 10,989 single / 2,037 MFJ /
178 HoH; `dep_status = 0` on every record (no non-filer is ever a
dependent; no MFS).

Six defects, in rough order of downstream damage:

1. **Investment income is identically zero.** `fiint`, `fidiv`, `fikgi` are
   DINA *fiscal-income* variables — income as reported on returns — and are
   zero for non-filers by construction. Verified in the 2022 output:
   weighted non-filer `txbl_int`, `div_ord`, `div_pref`, `kg_lt` are all
   $0.000B, against `wages` $147.3B and `gross_ss` $244.7B. Consequences:
   (a) the state fit's non-filer `income_tier()` cells are assigned on an
   understated income proxy; (b) in Tax-Data's new wealth stack, non-filers
   are mis-placed by `compute_broad_income()`
   (`src/imputations/helpers.R:192-241`), which drives SCF wealth-cell
   assignment and DFA aging buckets.
2. **Dividends are silently dropped even where DINA has them.**
   `impute_nonfilers.R:46` writes `qual_div`, a column that does not exist
   in the output schema (`config/variable_guide/baseline.csv` defines
   `div_ord`/`div_pref`); `write_outputs.R`'s `out_cols` intersection
   discards it.
3. **Ages are a three-point band.** DINA `ageprim` takes three values, so
   `impute_nonfilers.R:92-96` draws age groups uniformly within coarse
   bands: 2017 output has bands 1/2/3 at 1,514/1,456/1,489 records and
   4/5 at 2,341/2,433 — flat by construction — with 3,971 records (10.5M
   weighted) piled into band 6 (65+). The state fit's non-filer cells are
   `age_band × income_tier`, so this smear propagates directly into state
   placement.
4. **No dependent non-filers, no MFS** (`impute_nonfilers.R:25-26`) —
   acceptable v1 approximations, but undocumented ones.
5. **`filer = 0` is set only implicitly**, via the zero-fill of
   `remaining_vars` (`impute_nonfilers.R:136-142`). Nothing asserts it. The
   flag is load-bearing for both the CTC/rebate become-filer logic
   (Tax-Simulator `src/calc/do_taxes.R:126-127`) and the entire state-
   weights partition.
6. **The aging path is unmoored.** `project_puf.R`'s
   `compute_weights_for_year()` grows non-filer weights by married-only
   population factors for 2018-19 (`:333-341`, the only filer-aware branch)
   and by pure married×age demographics from 2020 on (`:344-365`) — no
   filer term at all. The filer/non-filer split therefore drifts with
   demographics regardless of what return counts do, exactly the
   universe-mismatch failure the income memo's worked example describes.

The stated rationale for the whole approach is one sentence in
`docs/model_documentation.md:84-87` ("synthetic non-filer records appended
from PSZ… so reforms affecting non-filers are scored on a full-population
base"). No limitation is documented anywhere.

### 2.2 Tax-Simulator: the v0 non-filer treatment in the state weights

The split-weights fit partitions records on the Tax-Data `filer` flag
(`src/data/state_weights.R:704` filers, `:763` non-filers; line numbers as of
2026-08-19). The filer partition targets 22 HT2 series × 10 stubs × 52 areas
(10,229 share-normalized targets). The non-filer partition gets **count-only**
targets on 1,390 `state × age_band(7) × income_tier(5)` cells (`:763-806`,
x ≡ 1 at `:788`), with priors and cell totals from `build_acs_margins()`
(`:289`) — the **v0 filing rule**: pointer-built tax units, filer iff unit
`sum(pmax(INCTOT, 0)) ≥` a hardcoded standard-deduction table
(`filing_threshold()`, `:185`). Because each non-filer cell has single
membership and count-only targets, the calibration engine reproduces the
ACS margin *exactly* in one pass (`research/state_weights/state_weights_phase1_summary.md` §2) —
the non-filer "fit" is pure prior reproduction of the v0 margin, errors
included.

Known problems, two previously documented and one new:

- **−7% filer bias.** v0 produces 148.2M ACS filer units vs HT2's 159.7M
  TY2022 returns (`research/state_weights/notes/state_weights_fit_issues.md:84-91`), from the omitted
  age-65 standard-deduction bump, the $400 SE rule, dependent filers, and
  above/below-threshold filing behavior.
- **Filing-propensity leak.** The postmortem found state `n_returns` errors
  correlate −0.61 with EITC take-up (`research/state_weights/state_weights_phase1_summary.md`
  §5.3) and pre-registered "a filing-rate covariate for the non-filer
  partition" as the fix (§7).
- **NEW — group-quarters persons are untreated.** `build_acs_margins()`
  never reads or filters `GQ` (verified 2026-08-16: the variable appears
  nowhere in `state_weights.R`): the ~8M group-quarters persons enter the
  unit builder as mostly income-less single-person "units" and land
  overwhelmingly in the non-filer margins. Whether that is a level error
  depends on the universe (§3.0): unlike the Affordability spine, the PUF
  universe INCLUDES GQ residents, so their presence in the margins is
  correct in principle — but the v0 treatment is wrong in composition
  (dorm students counted as independent non-filer units in the college's
  state, when they are mostly dependents claimed on parents' returns, often
  in another state), and the income memo's GQ warning bites wherever the
  anchors and the margins disagree about who is in the universe.

The ACS common extract
(`shared/raw_data/ACS/acs_common/us2022a/variables.csv`) carries `GQ`,
`SCHOOL`, `EDUC`, `RELATE`, `EMPSTAT`, `LABFORCE`, `SEX`, `RACE`, `HISPAN`,
`INCWAGE`, `INCBUS00`, `INCSS`, `INCSUPP`, `INCWELFR`, `FOODSTMP`,
`INCRETIR`, `INCINVST`, `HINSCAID`, `HINSCARE` — nearly the full Cilke
(1998) covariate set, and most of Mok's (2017). **No new IPUMS *USA* pull is
needed for the ACS-side margins**, though "nearly" and "most" are doing work:
§6.1 requires an explicit covariate-mapping check before the transfer, since
any regressor that cannot be reproduced on the ACS is one the transfer
silently drops. The **ASEC** is a separate requirement, sourced through the
shared extract machinery (§4.1). (The stale `state_weights.R:166` comment
claiming the extract lacks `SCHOOL` has since been corrected.)

## 3. The residual methodology, restated for these codebases

### 3.0 The universe question (why GQ treatment differs from the Affordability spine)

The income memo subtracts group quarters from PEP because its ACS spine
*excludes* GQ records — universe matching for a household-only file. The
PUF universe is different: DINA targets the full resident population aged
20+. Verified 2026-08-16 against the pinned vintage: `usdina2017.dta`
adults sum to 241.96M, matching PEP's 2017 resident 20+ population (~242M),
not the household-only population (~235M). The Tax-Data non-filer mass
(31.6M adults / 26.18M units in 2017) therefore already contains GQ
non-filing adults — prisoners, most nursing-home residents, non-filing
students. Three consequences:

- **Anchors use the full resident population.** Non-filing adults by state
  = PEP resident adults (NO blanket GQ subtraction) − HT2 filing adults. A
  GQ-subtracted anchor would rake genuine non-filer mass *out of* exactly
  the prison/college/nursing-home states the income memo worries about —
  the same error with the opposite sign.
- **The invariant is universe consistency, not exclusion.** The same GQ
  population must sit inside (or outside) all three objects: the Tax-Data
  non-filer partition, the PEP-side anchor, and the ACS margins. Blanket
  exclusion could only be made consistent by also carving GQ adults out of
  the DINA append, which is not possible — DINA carries no GQ flag.
- **An adult-dependent netting refinement follows** from the same
  adult-space accounting: the residual includes adult dependents claimed on
  filed returns, who are neither filing adults nor non-filer-unit heads —
  they ride filer records whose state placement the filer partition already
  determines. The non-filer-partition target is therefore PEP adults −
  filing adults − adult dependents claimed on returns, the last estimated
  from the HT2 dependents identity net of the under-18 population (T2/T5
  material; carried in the tolerance if too noisy to estimate directly).

### 3.1 The anchors

For each HT2 year (2014, 2016-2022; 2017 and 2022 first):

1. **Filing adults by state** from HT2 filing-status identities, implemented as
   `ht2_filing_persons()` (`state_weights.R:382`), with the QSS/MFS and
   adult-dependent caveats documented alongside it:
   `married = 2·n_joint + (n_returns − n_single − n_joint − n_hoh)`;
   `single = n_single + n_hoh`; `dependents = n_indiv − (n_returns +
   n_joint)`. TY2022 coverage: married 85.6% of ACS married adults, single
   77.6%, children 109.2%.
2. **National filing structure from Pub 1304** (IRS-Ind
   `national/by_size/`, already in the shared store, TY2011-2023): Table
   1.6 (`returns_marital_age_{year}.xls`) tabulates returns by AGI class ×
   **marital status × age of taxpayer** — an age dimension HT2 lacks
   entirely — giving national filing adults by age band directly rather
   than inferred. Table 1.7 (`dependent_returns_{year}.xls`) counts
   dependent filers, directly measuring both the dependent-filer
   double-count caveat in the identities above and the §3.0
   adult-dependent netting. The by-size tables also carry fine AGI classes
   to $10M+ and a no-AGI class where HT2 stops at $1M+. Principle:
   **national levels come from Pub 1304; HT2 supplies only the state
   shares** — the same share-normalization discipline the filer targets
   already use.
3. **Population** = Census PEP resident state × age — no GQ subtraction,
   per §3.0. (GQ composition by type × state is still tabulated from the
   on-disk IPUMS extract, with a national cross-check against ACS table
   B26001 — but as the T7 diagnostic and the dorm-student dependent share,
   not as a subtraction.)
4. **Residual non-filing adults** = (3) − (1) by state, with the national
   age × marital structure of the filing side taken from (2) and netted of
   the adult-dependent adjustment in §3.0 (now directly estimable from
   Table 1.7). An anchor with an explicit tolerance, not an exact count
   (return-state vs residence and facility-state vs tax-residence wedges,
   status-mapping wedges, vintage differences — income memo fn. 8).
5. **State age shape**: the *national* non-filer age shape is anchored by
   construction (PEP age minus Table 1.6 filing adults by age), and nationally
   the 65+ band is further splittable — SOI's **IRA study Table 4, column (1)**
   publishes Form 1040 filers by five-year age band to 80+ (TY2000-2023), whose
   `65 under 70`+`70 under 75` and `75 under 80`+`80 and over` aggregate exactly
   to `65_74`/`75p`. Use it for the **share, not the level**: its 65+ total runs
   4.5-5.7% below T1.6's, consistently, because it assigns each taxpayer their
   own age where T1.6 assigns joint filers the primary's.

   **State** variation is disciplined by **OASDI beneficiaries** at 65+ (100%
   data) and by the **covered-worker margin** through the working ages. The
   covered-worker universe is **HI (Medicare)**, not OASDI — EEDATA publishes
   both, and HI is the closer analogue to the W-2 universe because it includes
   the state and local government employment outside OASDI coverage (~4.1M
   persons). On dollars it is not a close call: HI is **uncapped** and matches
   QCEW to ~1% (1.007x in 2017, 1.013x in 2022), where OASDI's
   taxable-maximum-capped earnings run ~17% low. Read **Table 4** for persons and
   dollars and **Table 5** for state x age; tag every margin `covered_worker_hi`.

   Table 5 replaces the national persons-vs-returns ratio the design would
   otherwise have inferred the working-age layer from: covered workers are published
   **by state × age**, so the layer is measured rather than imputed from a national
   figure. Keep the ratio as a cross-check only.

   But note what that buys and what it does not. The bands are cut around Social
   Security eligibility (`<20/20-29/…/60-61/62-64/65-69/70+`) and align with the
   anchor bands at exactly one point, `65-69`+`70+` = `65+`. So Table 5 cannot supply
   a band-by-band target; what it supplies is a **residence-based, age-bounded 20-64
   count per state**, which is still a real improvement on a national ratio because
   the age bound and the geography are both observed. The upgraded ACS non-filer age
   shape smooths within it.

   **⚠ Sum the 51 jurisdictions, never the published `All areas` row.** Both SSA
   publications carry `All areas`, which includes beneficiaries and workers in the
   **territories and abroad** — neither of which is in the residual's universe.
   Reading it would overstate the 65+ margin by **2.5-2.6%**: the anchor values are
   **44,635,968 (2017)** and **50,766,317 (2022)**, not the 45,808,776 /
   52,052,807 the `All areas` rows show. The geography partition reconciles
   exactly (51 jurisdictions + 5 territories + foreign + unknown = `All areas`), so
   this is a choice the reader must make deliberately, not an approximation.
   `06_verify_ssa_inputs.R` checks the `All areas` row on purpose — that is a
   file-identity check, not the anchor.

   **Two further limits to carry.** OASDI publishes 65+ **by sex only**, so no
   state source can split it — the split lives in the national shape and the ACS
   prior, never in a state target. And EEDATA is a **1% sample** (Continuous Work
   History Sample) where OASDI-SC is 100% data, so its margins are soft targets
   with explicit small-state tolerance. Sources:
   `raw_data/SSA-{OASDI,EEDATA}-SC/NOTES.md`.

6. **Cross-checks, never targets**: QCEW state wage totals on the dollar
   side; the QWI/LODES fetchers already in `state_weights.R` stay
   diagnostics.

### 3.2 The filing model

This section is the authority on the filing model. It rests on the literature pass
recorded in `research/state_weights/nonfiler_residual/05_filing_model_literature.md`.

#### 3.2.0 Why a survey filing model at all — the honest framing

Treasury, the IRS and JCT have all **abandoned** survey-based filing models. Their
replacements are not better survey methods; they are administrative microdata:

| Institution | Current method | Requires |
|---|---|---|
| Treasury OTA (TP-12, 2023) | Non-filer units built directly from information returns, with **Form 1095** health-coverage rosters supplying household structure | Admin microdata |
| IRS (Pub 5785, Hertz et al.) | The "**Administrative Data Method**", explicitly replacing what they call the "Census Method" | Admin microdata |
| JCT (Cilke 2014) | Information-return sample; marital and parental structure filled with imputed "automatons" | Admin microdata |
| CBO | The exception — a statistical match of CPS to SOI, still survey-based | Survey + published SOI |

Cilke's own trajectory is the compact version: he wrote WP-78 at Treasury using the
CPS, then wrote the 2014 paper at JCT using administrative data, with no probit in it.

**This is a data-access story, not a verdict on the method**, and the distinction
matters for what we do next. Two senses of "administrative data" are easy to conflate:

- **Admin *microdata*** (linked CPS-IRS records, information returns). Closed to us,
  and closed to every model outside Treasury/IRS/JCT.
- **Admin *published tabulations*** (HT2, Pub 1304, Pub 5785, PEP, SSA). Fully open,
  and already the entire basis of this memo's anchors, targets and ceilings.

No peer model in our data position has anything structurally better than *threshold
rules + a behavioral layer + calibration to published totals*: TPC uses Cilke probits
with recalibrated constants; PolicyEngine uses a hand-set 16-cell voluntary-filing
table; Census uses threshold rules plus an admitted fudge factor (`FED105 = $2,000`,
"got us closer to the IRS targets"); PWBM uses statutory rules plus an unpublished
elective-filing module; PSL taxdata effectively trusts the CPS recode. **The design
below is at or above the state of the art for anyone without admin microdata**, and
the abandonment finding is not a reason to change course.

There is, however, a third option none of the peer models exploit, and it is the most
important thing this section adds — see §3.2.2.

#### 3.2.1 v1a — deterministic upgrades to `build_acs_margins()`

**Differentiated GQ treatment** in place of the v0
non-treatment: keep institutional residents (`GQ == 3`) as own-state non-filer units
unless income makes them filers; reclassify college-age dorm residents (`GQ == 4`, in
school, age < 24) as dependents rather than unit heads (they are claimed on parents'
returns, generally elsewhere, and HT2 already counts them in N2); leave military
barracks residents to the income test (most have wages and classify as filers); report
GQ weight by type and state. Then extend `filing_threshold()` to add the age-65
additional standard deduction; add the $400 SE rule via `INCBUS00`; let dependents with
own income above the dependent filing floor form filing units; use `SCHOOL` to keep
19-24 household students dependent.

#### 3.2.2 v1b — the probabilistic layer, on borrowed coefficients

Behind a `filing_model` argument, now `c("v1a", "mok", "cilke")`.

**Below the threshold — use Mok (2017), not Cilke (1998).** CBO Working Paper 2017-06,
Table 14, gives fourteen group-specific filing probits — coefficients, standard errors,
cell Ns and weighted filing rates — estimated on the **2007 CPS ASEC linked via PIK to
the IRS Individual Master File for TY2006**, on a covariate set Mok describes as
"similar to the set of covariates used by Cilke." Groups: unmarried &lt;65 and 65+, and
married &lt;65 and 65+, each × {0, 1, 2+ dependents}, plus dependent filers × {&lt;65, 65+}.

**The reason this works for us despite being identified on linked data is the point
worth internalizing: every regressor is CPS-native** — log gross income, a
negative-income indicator, presence indicators for wages / interest / dividends /
self-employment / rent / retirement / Social Security, means-tested transfer receipt,
count of household members on Medicaid, education, race and ethnicity. The
administrative linkage bought the *identification*; scoring the model needs only survey
variables. **We get the benefit of the linkage without the linkage.** That is the
non-admin option the framing question in §3.2.0 asks for, and it is exactly the logic
under which Cilke's coefficients were used for 25 years — the upgrade is simply that
Mok's were identified on far better data, 16 years later.

Mok's published per-cell weighted filing rates are the calibration targets, and come
free with the coefficients (all verified against the PDF, 2026-08-18): unmarried &lt;65
0.82 / 0.77 / 0.79; unmarried 65+ 0.62 / 0.60 / 0.63; married &lt;65 0.92 / 0.92 / 0.92;
married 65+ 0.83 / 0.77 / 0.80; dependents 65+ 0.23; **dependents &lt;65 0.10**. Her
constructed file is 141.7M non-dependent tax units = 117.9M filers + **23.7M
non-filers** (40.7M individuals), against TY2006 returns.

> **⚠ Transcription trap, found on inspection.** In Panel E (dependent filers) the
> columns run **"Age 65 or Older" first, then "Under Age 65"** — the reverse of the
> intuitive order, and the reverse of how the panel reads if you trust automated text
> extraction, which returns the two headers in the wrong sequence. The sample sizes
> disambiguate it (909 vs 62,438), and the page was checked visually. Panel E also has
> `.` rather than a coefficient for self-employment income in the 65+ column. Transcribe
> Table 14 against a rendered image, not a text dump.

> **⚠ Frame mismatch with our universe — the most consequential thing the verification
> turned up, and §3.2.4's companion problem.** Mok states that the CPS sampling frame
> means *"the characteristics of filers and nonfilers outside of the CPS sampling frame,
> such as people who are institutionalized or living outside the United States, are not
> considered,"* and elsewhere names *"individuals who were institutionalized in March
> 2007, died before the survey date, or were members of the military living in
> barracks"* as filers the linked data cannot see. **Her coefficients are therefore
> estimated on a household-population frame.** Our PUF universe includes group quarters
> (§3.0, verified against DINA totals), and §3.2.1 deliberately keeps institutional
> residents as non-filer units and routes military-barracks residents through the income
> test. Scoring Mok's equations on those records extrapolates outside her estimation
> frame. This is exactly the universe-consistency failure §7.3 exists to prevent, and it
> is not a reason to reject the model — no survey-estimated model will cover GQ — but the
> GQ population must be scored under an explicitly stated assumption and reported
> separately in the T7 diagnostic, not silently swept in. The same caveat applies to
> Cilke, whose CPS frame has the same exclusion.

**Fit Cilke alongside as the comparison.** The two use overlapping CPS-native
covariates, so scoring the same ASEC file under both costs one extra pass over a file
we have to build anyway, and it converts "keep the 1990s slopes?" into an empirical
question with a documented answer. If only one is fit, fit Mok.

**Test Mok's assignment rule too — but do not overstate its advantage.** She evaluates
two statistical-match ranking rules: **predicted income** (CBO's production method) and
**predicted probability of filing**. Under the latter, units are ranked within group by
predicted filing probability and the lowest-probability units are cut to match the
group's observed non-filer share — rank-and-cut, rather than intercept calibration plus
a uniform draw, which is the natural default. Her verdict is measured: *"The
share of constructed tax units that is correctly simulated … is similar under both
methods."* What the probability method buys is that the demographic composition of
simulated filers matches the linked data **by construction**, and that simulated
non-filers' average income sits closer to the truth. Worth testing, not worth assuming a
large gain.

**Above the threshold — unchanged in structure.** An IRS Pub 5785 non-filing hazard, a
national scalar for v1 (~11.19M above-threshold units, TY2014-16 average) allocated by
the publication's relative risks. Pub 5785 stands: no successor edition exists. Two
additions: **Erard et al. (2020)** is the only modern published model of this hazard
with full coefficients (pooled TY2001-2013), worth reading before fixing the allocation;
and its `MARRIED` coefficient **flips sign across vintages of the same model by the
same authors** over nearly identical years, so no single vintage should be treated as
settled — which matters because Pub 5785 reports &lt;20% of above-threshold non-filers
are married and the scalar has to allocate across marital status somehow.

#### 3.2.3 Survey, sequence, and the transfer to the ACS

The models are estimated on the **CPS ASEC** and transferred to the ACS. Cilke and Mok
both estimated on the ASEC, so it is the models' native environment; the ACS is the
destination, because that is where the state margins are consumed. The ordering:

1. Build ASEC tax units and income concepts (design per §8 research pass A — the ASEC
   is *not* the ACS with better income detail).
2. Compute the threshold each unit faces (§3.2.1 rules).
3. Score the below-threshold probits and the above-threshold hazard.
4. **Calibrate jointly** — group constants and the above-threshold scalar chosen
   together against the population, filer-count and SSA margins, not sequentially.
5. Re-calibrate the constants on the ACS.

**The ASEC→ACS national filing-rate comparison is a gate, not a footnote.** If the two
surveys imply materially different national filing rates, the transfer needs rethinking
before the state fit rests on it.

#### 3.2.4 The bias we inherit, and what to do about it

The reason the IRS dropped the Census Method applies to us regardless of which
coefficients we use, and it is the most consequential caveat in this section. Hertz et
al. state the IRS *"abandoned the Census Method because even with the income
imputations, the income reported on the CPS-ASEC by nonfilers still fell short of the
income reported to the IRS by third parties"* — and quantify it: against a **50.7M**
administrative target, the reweighted ASEC reaches **42.0M**, a **~17% undercount**.

This is not a probit problem; it is a survey-income problem, and it enters our design at
a specific, identifiable point. **The threshold test is a function of reported income.**
If ASEC income is understated, units are misclassified as below-threshold, and both the
threshold split and the voluntary-filing model inherit the error. The direction is
knowable: an ASEC-based file will tend to **undercount** non-filers — the same direction
as the F1 shortfall the rework exists to fix, so the rework must not be credited with
closing more of that gap than it does.

Three mitigations, now load-bearing rather than optional:

- **Erard et al. (2014)** publishes a bivariate probit for **ASEC under-reporting of
  Social Security and pension income**. That is precisely this correction, and it
  matters doubly because the state OASDI margin (§3.1 step 5) assumes ASEC Social
  Security receipt is right.
- **Pub 5785's receipt rates as ceilings** (already in the design, §5.1) discipline the
  imputation in the direction the survey errs.
- **Carry the ~17% figure explicitly where it bites — the filing model, not the
  anchor.** The anchor tolerance is computed from population and filing-adult
  wedges alone (`08_residual_tolerance.R`); this bias enters through the ASEC
  threshold test, so it belongs in the filing model's own error budget. It is a
  published magnitude for a bias we would otherwise be guessing at.

A second diagnostic from Mok, worth stating because it sets expectations: her
predicted-income match reproduces filing behavior for 83% of units — **94% of filers but
only 27% of non-filers**. Whatever we build will be far more accurate on the margin we
care least about.

#### 3.2.5 Dependents and MFS are in scope

The original design put dependent non-filers and MFS out of scope for v1 (§5.1).
The literature pass argued for revisiting that on evidence, and **JI decided
2026-08-19 that both are in scope.**

The brief for that pass assumed TCJA's larger standard deduction sharply cut
filing requirements. **For the main statuses that is wrong** — the §6012(a)(1)
threshold is the standard deduction *plus* exemptions pre-TCJA, so zeroing
exemptions offset most of the increase (single $10,400 → $12,000, +15.4%; MFJ
$20,800 → $24,000, +15.4%), and returns filed actually rose over the period.
**But the two groups that moved sharply are precisely the two that were
deferred:**

| Status | TY2017 | TY2018 | Change |
|---|---|---|---|
| Single dependent (earned income) | $6,350 | $12,000 | **+89%** |
| Married filing separately | $4,050 | $5 | **collapse** |

Dependents are **Cilke's largest group** — 31.1% of his below-threshold
population and 36.4% of his non-filers — and Mok estimates a dependent-filer
equation directly, with a published filing rate of **0.10** for dependents under
65. So the coefficients exist, the population is large, and the threshold
governing it nearly doubled inside our window. Deferring them was a bigger
approximation than the original draft treated it as.

Four consequences, because this is not a free decision:

- **The ASEC unit builder must form and retain dependent and MFS units from the
  start** (§3.2.3 step 1, research pass A). Retrofitting either later means
  rebuilding the file, which is why this was decided before pass A rather than
  after.
- **Mok's Panel E (dependent filers) is now load-bearing**, which makes its
  reversed column order (65+ first) a correctness risk rather than a
  transcription nicety.
- **MFS has no borrowed coefficients.** Neither Mok nor Cilke estimates an MFS
  equation, so MFS enters through the *threshold* rule (the $5 floor from TY2018),
  not a probit — a deterministic v1a treatment, with the voluntary-filing layer
  left to the married equations. State that limitation where the model is
  documented rather than implying MFS is modelled to the same standard.
- **`impute_nonfilers.R`'s `dep_status == 0` assertion (§5.1) becomes a
  reconciliation question, not an invariant.** Tax-Data currently guarantees no
  non-filer is a dependent; with dependents in scope on the ACS side the two
  universes disagree unless that is deliberately reconciled. The wage evidence in
  §5.4 sharpens this: the non-filer wage residual demonstrably contains
  wage-earning dependents.

#### 3.2.6 Acceptance metric

v1 ACS filer units vs HT2 `n_returns` by state: the −7% national bias and its 20pp
state spread should collapse to within the anchor tolerance. Stated on the ACS side
because that is where the margins are consumed. Plus one more, per §3.2.3: **the ASEC
and ACS implied national filing rates, reported side by side.**

### 3.3 "Impose it jointly" — what that means here

The income memo's worked example (7% filer over-assignment → raking drains
the non-filer pool → state income-per-adult biased up 7% while every
targeted diagnostic looks perfect) does **not** apply mechanically inside
`build_split_weights()`: the filer and non-filer partitions are fit
separately, and the row constraint `Σ_s W[i,s] = w_i` holds within each
partition, so weight cannot drain between filers and non-filers *inside the
state fit*. The failure mode lives in two other places, and the joint-fit
requirement translates accordingly:

1. **The national filer/non-filer split** is set upstream in Tax-Data and
   is currently unmoored (§2.1, defects 3 and 6). Fix: a national residual
   calibration of non-filer weights in Tax-Data (§5.2), and an aging path
   that keeps the split consistent with projected return counts (§5.3).
2. **The state placement of the non-filer partition** currently reproduces
   a biased margin exactly. Fix: compute the non-filer state targets as the
   arithmetic residual of the *same HT2 vintage* the filer partition is fit
   to (§6.2). Then, if both partitions hit their targets, fitted state
   adult populations reproduce PEP resident adults **by construction** — the population
   identity is enforced through target self-consistency, not through a
   stacked optimization.
3. **A population-identity diagnostic** in `research/state_weights/scripts/validate_state_weights.R`:
   fitted filer adults + fitted non-filer adults per state (× age band
   where supported) vs PEP resident adults (no GQ subtraction, §3.0), with the
   tolerance from `residual_tolerance_{year}.csv`. If soft-target
   trade-offs push this beyond tolerance, the escalation path is a single
   stacked fit — `fit_gradient()` already accepts arbitrary row sets, so
   concatenating `(w, P0)` across partitions and adding identity targets
   spanning both is mechanical — but it should not be built speculatively.

## 4. Diagnostic harness (built and run; Stage D)

Home: `research/state_weights/nonfiler_residual/`, numbered scripts sourcing
`src/data/state_weights.R`, scratch under the existing `state_weights_tmp/`
convention. **Executed 2026-08-16** (`4783dc3e9`); the SSA inputs it was missing
were placed and verified 2026-08-19. The ACS tabulation steps run under
`sbatch` (login node OOM-kills near 5-8 GB; the weights inputs alone are
~1 GB serialized). Everything else is login-node safe.

### 4.1 `01_fetch_residual_inputs.R` — data acquisition

Fetchers follow the `fetch_qwi()` pattern (`state_weights.R:494`): small
functions, source caveats in comments, paths derived from
`raw_data_root()`, never hardcoded. New shared-store families mirror the
existing store layout (each with a manifest):

| Series | Source | Proposed store |
|---|---|---|
| PEP state × single-year-age × sex, 2020-2024 vintage | census.gov popest `sc-est2024-syasex.csv` (verify filename at fetch time; civilian variant as sensitivity) | `raw_data/Census-PEP` |
| PEP intercensal 2010-2020 (back-year anchors) | census.gov popest intercensal state files | same |
| Group quarters by state × age | tabulated from the on-disk IPUMS extract, `GQ ∈ {3,4}` × `STATEFIP` × age band; national check vs ACS B26001 via the Census API | derived (script output), not a raw store |
| OASDI beneficiaries by state (65+ rows) | SSA statcomps `oasdi_sc` — **placed and verified 2026-08-19**, TY2017-2025 plus the 1999-2025 flat series. ssa.gov 403s automated retrieval on TLS fingerprint and no browser engine exists on the cluster, so placement is manual | `raw_data/SSA-OASDI-SC` (with `NOTES.md`) |
| Covered workers: persons and wage dollars by state | SSA statcomps `eedata_sc` — **placed and verified 2026-08-19**, TY2017-**2023** (the series ends there, which bounds forward extension). Read **Tables 4/5 (HI)**, not 1/2 (OASDI) | `raw_data/SSA-EEDATA-SC` (with `NOTES.md`) |
| QCEW state annual wage totals | BLS CEW annual singlefile | `raw_data/BLS-QCEW` |
| Pub 1304 by-size tables (1.6 returns × marital × age; 1.7 dependent filers; 1.1 income sources) | **already in the shared store** — `raw_data/IRS-Ind/national/by_size/`, TY2011-2023, maintained by the IRS-Ind downloader | no fetcher needed; reader only |
| **SOI IRA study Table 4** (added 2026-08-19) — *Taxpayers with IRA Plans, by Age of Taxpayer*. **Column (1) is the find: `Number of taxpayers who filed Form 1040` by five-year age band to 80+**, i.e. filers by age at a resolution Table 1.6 does not publish. `65 under 70` + `70 under 75` and `75 under 80` + `80 and over` aggregate **exactly** to the 65_74 / 75p split. TY2000-2023 (no 2003), so both anchor years and every back year | **already in the shared store** — `raw_data/IRS-Ind/national/ira/ira_t04_{year}.xlsx` | reader only |
| **SOI Form W-2 study** (added 2026-08-19). Universe is W-2 income on **filed returns**, which is what makes it differenceable against SSA (§5.4.1). **Table 5.A** is the published **box-1/box-5 reconciliation** — the wedge the wage constraint needs — and ships inside the all-tables workbook `{yy}inallw2.xls`, a different naming lineage from the per-table `{yy}in{NN}w2all.xlsx` files, which is why it was missed. **TY2014/2016/2017/2018 exist and are placed; 2015 and everything after 2018 do not.** Tables 1.A-4.A for TY2019-2020 are separately in the store | **placed 2026-08-19** — `raw_data/IRS-Ind/national/w2/w2_all_{year}.xls`, registered in the IRS-Ind manifest | reader only. **The IRS-Ind downloader should learn the `{yy}inallw2.xls` lineage**, and its `NOTES.md` gain a W-2 family section |
| Pub 5785 above-threshold composition | hand-transcribed CSV with page citations | repo: `research/state_weights/nonfiler_residual/resources/pub5785_hazard.csv` |
| **Mok (2017) Table 14 probit coefficients** — the primary below-threshold model (§3.2.2) | hand-transcribed CSV, 14 group equations with SEs and per-cell filing rates. **PDF in hand and verified** (JI's copy, Affordability `Literature/Reweighting/53125-nonfilers.pdf`; cbo.gov 403s automated retrieval). Transcribe from a rendered image and mind Panel E's reversed column order (§3.2.2) | repo: `research/state_weights/nonfiler_residual/resources/mok_coefs.csv` |
| Cilke (1998) probit coefficients — retained as the **comparison** fit only (§3.2.2) | hand-transcribed CSV (9 group equations). **Extract with PyMuPDF word positions, not `pdftotext -layout`**, which silently mis-assigns the coefficients | repo: `research/state_weights/nonfiler_residual/resources/cilke_coefs.csv` |
| **CPS ASEC** — filing-model estimation sample (§3.2.3) | **Pulled 2026-08-19** through the shared `common_ipums_download` machinery: ASEC **2015-2025** (income years 2014-2024, so both anchor years and every back year), 72 variables validated against the IPUMS API before pulling, 92 common to all years, no case selection. Carries IPUMS's own `FILESTAT` filer recode, `DEPSTAT`, `ADJGINC` and `TAXINC` — establish what those already do before building a unit builder that duplicates them (§8). **Add variables to the shared request; do not fork an extract** | **`raw_data/CPS-ASEC/cps_asec_common/`** (the family predates this work; the earlier proposed path `CPS/cps_common` was wrong) |

Note: `raw_data/SSA-Demographic/v3` was checked and holds only national
series — not usable for the state margins. The HT2 store duplication is
settled (§7.4), so the new families land under one convention.

### 4.2 `02_build_residual_anchors.R` — the anchor computation

The refactor it required is **done**: the identities were promoted out of
`compare_individuals_acs_irs()` into an exported `ht2_filing_persons(ht2)`
(`state_weights.R:382`) returning `(state, married_filing_adults,
single_filing_adults, dependents)` — one definition per computation, called by
the diagnostic, the target builder, and (per the income memo)
Affordability-Index. Still to add: a reader for the Pub 1304 by-size `.xls` tables (Tables
1.6/1.7 first; multi-row headers and disclosure footnotes per
IRS-Ind `notes/national_bysize.md`, TCJA-2018 IRA/pension combining
caveat). Then compute §3.1 steps 3-5 and emit the cross-repo artifacts:

- `residual_anchors_{year}.csv` — `(state, age_band, nonfiling_adults,
  tolerance)` plus a national row;
- `nonfiler_wage_margin_{year}.csv` — covered-worker minus HT2 wage-return
  counts by state, with the returns-per-person ratio and QCEW dollar check.

### 4.3 `03_diagnose_current_nonfilers.R` — the re-consideration tables

Reads the production `tax_units_{year}.csv` non-filer slice, the v0 margins and the
anchors, and writes `results/` CSVs plus a findings memo. Seven tables: **T1**
national level, **T2** age composition, **T3** income composition, **T4** aging
path, **T5** state margins, **T6** cell support, **T7** GQ composition.

**All seven ran (2026-08-16).** What they found — F1 the mass is 15-25% short,
**F2 the age composition is inverted** and is the single most consequential defect
for the weights, F3 investment income is identically zero, F4 the aging path
drifts, F5 the v0 margins run 0.78x-1.51x of the anchor and are reproduced
*exactly*, F6 group quarters are 16.8% of the national residual but 42% in SD, F7
above-threshold non-filers are 10.6-11.9M and SE-shaped — and how each fed the
decisions **D1-D6**, is the subject of **`research/state_weights/nonfiler_residual/04_findings.md`**. It
is the evidence record and is not restated here.

`08_residual_tolerance.R` was added 2026-08-19 and emits
`residual_tolerance_{year}.csv` from T5 (§3.1, and the tolerance discussion in
`research/state_weights/plan.md`).

## 5. Overall-PUF rework design (Tax-Data)

All in Phase 1 (base construction) plus one Phase 2 fix. Nothing goes
through `module_deltas` — Phase 3 modules cannot add rows or reweight
(`materialize.R:264` matches by id) — and no forbes-splice row-adding is
needed: the PSZ record set stays; values and weights change.

### 5.1 Composition fixes in `impute_nonfilers.R`

- **Dividends**: route `fidiv` to `div_ord`/`div_pref` (minimal: all
  `div_ord`; better: split by the qualified share among bottom-two-stub PUF
  filers). Trivial; do regardless of everything else.
- **Investment income**: first action at implementation is to inspect
  `usdina2017.dta` for DINA's national-income counterparts to the fiscal
  `fiint`/`fidiv`/`fikgi` (DINA carries NI interest/dividend/equity
  returns). Option A: scale those to per-unit amounts. Option B (fallback):
  hot-deck from stub-1/2 PUF filers conditioned on age band × has-wages.
  Either beats identical zeros. Discipline for both: Pub 1304 Table 1.1
  (`income_sources_{year}.xls`, in the IRS-Ind store) gives return counts
  *with* each income type in the bottom AGI classes (including the no-AGI
  class HT2 lacks) — the receipt rates the repaired non-filers should sit
  at or below.
- **Age detail**: replace the flat `runif` draw
  (`impute_nonfilers.R:92-96`) with a within-band distribution from the
  national anchor age shape (committed as
  `resources/nonfiler_age_shape.csv`, produced by the Stage-D harness).
  **This is the single highest-value fix for the state weights**, since the
  non-filer state cells key on `age_band(age1)`.
- **Assertions**: set `filer = 0` explicitly; `stopifnot` on it and on
  `dep_status == 0` rather than relying on the zero-fill.
- **Scope.** An in-Tax-Data filing model stays out: filing belongs where it is
  modelled on survey records (the ACS margins here; later Affordability-Index), and
  Tax-Data's DINA units arrive with a filer flag. **Dependent non-filers and MFS,
  however, are in scope as of 2026-08-19** (§3.2.5) — which is why the
  `dep_status == 0` assertion above is a reconciliation question rather than an
  invariant to enforce.

### 5.2 New `src/calibrate_nonfilers.R` (Phase 1)

Sourced in `main.R` between `impute_nonfilers.R` and `impute_variables.R`:
a post-append rake of **non-filer weights only** to the national residual
anchors by age band × marital status (≤14 cells — closed-form cell-ratio
adjustment; do not touch `reweight.R`'s filer LP). Targets read from a
committed `resources/nonfiler_targets_2017.csv` snapshot with a provenance
header, generated by the Stage-D harness — Tax-Data gains no HT2/PEP
readers and no server paths. Emits a before/after diagnostic table. This is
the national half of the income memo's "force the model and the counts to
agree."

### 5.3 Aging-path fix in `project_puf.R` (Phase 2)

In `compute_weights_for_year()`: make the non-filer path residual-by-
construction — each year's non-filer adult mass = projected adults (the
`demog` table already loaded) − projected filer adults (2018-19 from the
IRS return-count factors as now; 2020+ filer adults grow with the same
married×age demographics they already use), scaled within age×married
cells. Minimal change: one new factor table alongside
`population_factors_2020plus`, applied `if_else(filer == 0, …)` symmetric
with the existing filer branch at `:333`. Filer weights and the ledger
architecture are untouched.

**Priority order for the state weights downstream:** age detail (§5.1c) >
national level + aging (§5.2/§5.3) > investment income (§5.1b) > dividends
bug (trivial, always).

### 5.4 Federal validation of the reworked vintage

The rework changes a national file every federal estimate rests on, so a
validation battery sits between it and any state work. It has a **predicted
signature** worth writing down before running anything.

**Build V1/V2/V3 as separate vintages** — V1 = §5.1 composition fixes at fixed
weights, V2 = V1 + §5.2 calibration, V3 = V2 + §5.3 aging. Two of the three A/Bs
are then *exact-equality* tests, worth the negligible extra cluster time: V1 must
leave `n_tax_units`, `n_returns` and payroll totals untouched, and V3 must be
bit-identical to V2 in 2017.

**What must not move.** Every 1040 dollar aggregate is summed as
`. * weight * filer` (`summary_stats.R:193-195`), so non-filers contribute exactly
zero. Under current law in 2017-2019 and 2022+, `totals/1040.csv` and every line
of `supplemental/cbo_comparison.csv` must be **identical**. They are the tripwire
on the "non-filer only" claim, not a benchmark on the reworked object — CBO's
build-up has no non-filer line.

**The one real external check, and it should run early.** The current-law baseline
already pays refundable credits to non-filers in 2020-2021 (`baseline/rebate.yaml`
sets $1,800/$1,400; `ctc.yaml` sets `min_refund_young/old` 3600/3000 in 2021), so a
plain baseline run over 2020-2022 exercises the non-filer level against published
actuals — **EIP3 ≈ $402-411B, advance CTC ≈ $93B**. A file whose 2021 baseline
overshoots is over-massed whatever the anchors say. This is the **only** place the
non-filer level meets an observed dollar amount, and because §5.2 calibrates to
anchors that §6.2 also targets, it is the only thing standing between a wrong
anchor and a system in which every diagnostic passes. Run it on V2, not at the end.

**A hazard specific to this rework.** `run.R:348-357` binds precomputed random
numbers **positionally** (`bind_cols(globals$random_numbers)`). If
`calibrate_nonfilers.R` reorders rows, every *filer* record is silently
rerandomized. Assert `identical(ids_in, ids_out)` and gate the A/B on ordered id
identity across vintages and years.

#### 5.4.1 The combined-universe wage constraint

**Tax-Data's control totals do not include non-filers.** Every target in
`config/target_info/baseline.csv` comes from `process_targets.R`'s
Compiled-SOI-Tables — returns filed, so filers by construction — and the LP is
solved on the filer file (`create_2017_puf.R:54`) **before** `impute_nonfilers.R`
appends non-filers. So §5.2's choice not to renormalize filers cannot break a
national wage reconciliation: there is none spanning both universes.

**The exposure is the opposite — nothing constrains the combined total.** TY2022:
filer wages $9,615.8B + non-filer $147.3B = $9,763.1B, non-filers 1.51% of it.
Raking non-filer weights +15/+25% adds $22.1B/$36.8B of wages and roughly
**+$3.4B/+$5.6B of baseline payroll receipts** (~0.2-0.35% of payroll), because
`get_pr_totals()` (`summary_stats.R:214-278`) has **no filer gate**. Predict it,
then constrain it.

**Use SSA HI wage-and-salary (EEDATA Table 4).** The candidates are not
interchangeable, because the PUF's `wages` is **E00200 = 1040 line 1 = W-2 box 1**
(verified in `variable_guide.csv`; the PUF carries **no elective-deferral
variable**). SSA HI is the only candidate whose universe is exactly the object in
question — persons with wages irrespective of filing — where QCEW counts
establishments and NIPA `gdp_wages` is a macro aggregate already used as a
*growth* factor (`project_puf.R:90`), not a level. It is also already the chosen
`covered_worker_hi` universe, and the 1% sampling objection does not bite on a
national total. QCEW and NIPA stay cross-checks.

**The box-1/box-5 wedge is measured, not assumed.** SOI's Form W-2 study **Table
5.A** publishes both boxes on one universe — in the store as
`IRS-Ind/national/w2/w2_all_{2014,2016,2017,2018}.xls`:

| TY | box 1 | box 5 | wedge | wedge % |
|---|---|---|---|---|
| 2014 | $6,516.3B | $6,745.0B | $228.7B | 3.51% |
| 2016 | $6,950.5B | $7,216.6B | $266.0B | 3.83% |
| **2017** | **$7,277.1B** | **$7,541.0B** | **$263.9B** | **3.63%** |
| 2018 | $7,525.4B | $7,841.7B | $316.2B | 4.20% |

The wedge is **elective deferrals only**: Section 125 cafeteria contributions are
excluded from box 1 **and** box 5 (pre-tax for income tax and FICA alike), so they
never appear in the difference — nor in SSA's HI taxable earnings.

**This overturns the first reading of the gap.** The $871.6B PUF-vs-SSA gap in
TY2022 was provisionally attributed to deferrals plus cafeteria plans, implying the
PUF's combined total was roughly right. Scaling the measured wedge to 2022 accounts
for only about **$400B**, leaving **~$450B (4.3%) of genuinely missing wage mass**.
The constraint would **not** be satisfied today.

**And a third source turns the gap into a direct measurement of non-filer wages.**
The IRS W-2 study's universe is W-2 income attached to **filed returns** — its
tables are "by Return and Earner Type", carry `Number of returns` columns, and its
footnotes enumerate filing statuses. SSA's EEDATA counts **all** covered workers.
Same box-5 concept, different universes, so differencing them isolates wage earners
who never appear on a return:

| TY | SSA HI (all workers) | IRS W-2 (filed returns) | residual | persons | PUF non-filer wages |
|---|---|---|---|---|---|
| 2017 | $8,021.6B | $7,541.0B | **$480.6B** | 20.3M | $116.2B (**24%**) |
| 2018 | $8,431.9B | $7,841.7B | **$590.3B** | 23.6M | $121.3B (**21%**) |

**The PUF carries about a fifth of the non-filer wage mass two administrative
sources jointly imply** — the first dollar-denominated read on F1, and consistent
with F2: the PUF's non-filers are not merely too few but the wrong *kind*,
disproportionately elderly and wageless where the missing mass is working-age
earners. It should size the V2 calibration rather than be discovered after it.

**Read the residual as an upper bound.** It also contains wage-earning
**dependents**, outside the IRS study's taxpayer count but riding filer records in
the PUF rather than forming non-filer units (see §3.2.5). The implied ~$25,000
average is high for a below-threshold population, which says as much.

**Coverage binds.** Table 5.A exists for **2014, 2016, 2017 and 2018 only** — the
2019/2020 releases publish Tables 1-4 without it, and nothing is published after
2020 (probed 2026-08-19). TY2017 is covered, TY2022 is not, so a 2022 constraint
rests on extrapolating a wedge that is visibly trending up as deferral
participation rises. Treat the constraint as a **bound** for that reason, not
because the gap is explained.

**One trap, recorded.** W-2 Table 2.A's universe is only taxpayers **with**
elective retirement contributions — 62.6M of the 146.2M with wage income (42.8%,
confirmed against Table 3.A's total row). Its **contributions** column is a valid
population total, since non-contributors contribute zero; its **`Medicare wages`
column is not an aggregate** and must never be read as one.

## 6. State-weights rework design (Tax-Simulator)

### 6.1 Filing model in `build_acs_margins()`

Implement §3.2, which is the authority for the model itself; this section covers only
how it lands in the ACS-side code.

Extend `read_acs_extract()`'s default `cols` with `GQ, SCHOOL, EMPSTAT, SEX, EDUC,
INCWAGE, INCBUS00, INCSS, INCSUPP, INCWELFR, FOODSTMP, INCRETIR` (all present in the
common extract). Apply the v1a deterministic upgrades (§3.2.1) unconditionally; put the
probabilistic layer behind the `filing_model = c("v1a", "mok", "cilke")` argument
(§3.2.2). Two things to be precise about:

- **The estimation happens on the ASEC, not here** (§3.2.3). What runs on the ACS side
  is the *transfer*: re-calibrating the group constants against `ht2_filing_persons()`
  totals by state × dependent status. The joint calibration of group constants and the
  above-threshold scalar happens once, on the ASEC, against the population/filer/SSA
  margins; the ACS step re-fits constants only.
- **Mok's covariates must survive the transfer.** Her regressors are CPS-native but not
  all are ACS-native — the means-tested-transfer and Medicaid-count terms map to
  `INCWELFR`/`INCSUPP`/`FOODSTMP` and `HINSCAID`, and education and race map to
  `EDUC`/`RACE`/`HISPAN`. Check the mapping explicitly before fitting; any covariate
  that cannot be reproduced on the ACS is a covariate the transfer silently drops.

### 6.2 Non-filer targets in `build_weight_inputs()` (`state_weights.R:763-806`)

- **Primary targets**: the residual anchors `(state × age_band)`,
  share-normalized like every other target (PUF non-filer national adult
  total × residual state share within age band), with the adult x-vector
  per D5.
- **Additional margins**, each at the status its source can bear:
  - **OASDI 65+ beneficiary counts** by state — 100% data, so this can carry a
    firmer target, but **only as a single `65p` band**: OASDI publishes the 65+
    cut by sex only, and no state-level source splits it. (Nationally the split
    exists — see §3.1 step 5 — and belongs in the Tax-Data age shape and the ACS
    prior, not here.)
  - **The HI covered-worker margin** (EEDATA Tables 4/5) as a coarse **20-64
    working-age count per state** — its bands align with the target space only at
    65+, and it is a **1% sample**, so it enters as a **soft target or prior**,
    never a hard constraint, with the small-state tolerance from
    `residual_tolerance_{year}.csv`.
  - **QCEW** stays a diagnostic, never a target.

  The two SSA margins are **not of equal authority** — OASDI-SC is 100% data,
  EEDATA is a 1% sample — and must not be given the same status.

- **Income tiers move to the prior**: keep `income_tier` in the prior
  (upgraded v1 ACS shares) and demote the current 1,390 count-only cells
  from exact targets to prior-only or soft targets. The anchors own the
  level; the ACS owns the within-state shape. This turns the non-filer
  partition from exact-IPF reproduction of a biased margin into a genuine
  calibration — it should run through `fit_gradient()` (today it only ever
  sees `fit_calibration()`-trivial cells).
- **Hard prerequisite**: the Tax-Data age fix (§5.1c) must land first, or
  `age_band(tu_n$age1)` cell assignment stays smeared across the very
  dimension the anchors discipline.

### 6.3 Validation additions

- The population-identity check of §3.3 (fitted filer + non-filer adults vs
  PEP **resident** adults per state — no GQ subtraction, §3.0) in
  `research/state_weights/scripts/validate_state_weights.R`, against `residual_tolerance_{year}.csv`.
- Re-run the EITC take-up correlation (postmortem §5.3); the −0.61 should
  attenuate.
- A new held-out metric where gains *are* expected: state adults by age
  band vs PEP (currently unscored anywhere).

### 6.4 Expected effects, stated honestly

This rework fixes the state placement of the ~27.6M non-filer units — the
income memo's headline income-per-adult bias — and the pre-registered
filing-propensity leak. It does **not** directly fix the filer-partition
held-out misses (taxable pensions 17.0, Schedule C 30.8, capital gains 61.0
MARD; `research/state_weights/state_weights_phase1_summary.md` §5.2): those are a filer-target
poverty problem, complementary to §7-item-1 demographic target expansion.
Do not promise held-out MARD gains from the non-filer rework alone.

## 7. Cross-repo sequencing and interfaces

### 7.1 Roadmap

Sequencing decision (JI, 2026-08-16): the non-filer rework lands **before**
the production weights swap-in, so the swap-in fit happens once, on
upgraded margins — not fit-on-v0-then-re-fit. One exception this memo still
argues for: the differentiated GQ treatment (D4) is decision-independent
and should ship ahead of the rest, sized by T7 first.

1. ~~**Stage D — diagnostics**~~ — **DONE** (`4783dc3e9`, 2026-08-16): fetchers,
   anchors, tables T1-T7, findings memo, decisions D1-D6. Its one open item, the
   **SSA manual downloads**, closed 2026-08-19. The coefficient transcription
   remains outstanding, with its target changed — transcribe **Mok Table 14**
   (§3.2.2), with Cilke as the comparison fit.
2. ~~**Pre-flight**~~ — **DONE 2026-08-19.** Vintage advanced to `2026070814`
   (F1-F4 verified to stand); age bands settled and implemented as
   `age_band()`/`target_age_band()`/`a16_band()` in `state_weights.R`; anchor
   tolerance computed (`08_residual_tolerance.R`); dependents/MFS and the
   covered-worker universe decided. Details in `research/state_weights/plan.md`.
3. **Research pass A** (~3-5 days, parallel with everything): ASEC
   tax-unit and income construction. §8. *(Pass B — parameter currency — is closed;
   its findings are in §3.2.)*
4. **GQ treatment fix** in `build_acs_margins()` (D4: dorm-student
   reclassification, institutional retention, GQ reporting), as soon as T7
   sizes it. Decision-independent; can ship first.
5. **Tax-Data rework** (§5): composition fixes, national calibration,
   aging fix; full pipeline re-run. **Build V1/V2/V3 as separate vintages**
   (§5.4) so each change can be A/B'd independently. ~1-2 weeks + cluster run.
6. **Federal validation battery** (§5.4) on those vintages,
   before any state work depends on them. ~1 week, mostly diff-reading.
7. **State-weights rework** (§6) on the new Tax-Data vintage: margins v1,
   residual targets, re-fit (config-7 hyperparameters unless the sweep says
   otherwise), validation battery. ~2-3 weeks.
8. **Production swap-in** per the existing checklist
   (`research/state_weights/state_weights_phase1_summary.md:260-263`, `research/STATUS.md` §Phase-1
   close-out): structural-core pruning, `build_split_weights(method =
   'gradient')`, `state_weights_{year}.csv` writer, dispatcher flip at
   `src/sim/run.R:433`.
9. **Cross-validation + handoff**: identity diagnostic, held-out battery,
   pilot-state liability re-check, memo updates (including a pointer from
   the income memo's "Aligning the code" section to this workstream).
   ~1 week.

### 7.2 Interfaces (one definition per computation)

- **`ht2_filing_persons()`**: single home in Tax-Simulator. Recommend
  splitting `state_weights.R` (~1,070 lines) into `src/data/ht2.R` (reader,
  `HT2_TARGET_MAP`, stub logic, identities) and `src/data/filing_model.R`
  (thresholds, Cilke, hazard, `build_acs_margins`), leaving engines and
  assembly in place — this is what makes the income memo's "source the
  shared functions from Tax-Simulator" workable for Affordability-Index
  without dragging in the split-weight scaffolding it explicitly does not
  want.
- **`residual_anchors_{year}.csv`**: one builder (the Stage-D scripts,
  promoted to maintained code), three consumers — the state target
  assembly, Tax-Data (as a committed snapshot), Affordability-Index.
- **`filer` flag contract**: authority stays with Tax-Data; post-rework it
  means "residual-anchored non-filing unit." Document in both repos; add
  the missing assertion (§5.1).
- **`state_weights_{year}.csv`**: schema unchanged `(id, state, weight)`;
  vintage-tag fits (`v0-margins` vs `v1-residual`) so downstream consumers
  can tell them apart. Eventual home of the whole weights model remains
  Tax-Data ("migration changes a path, not the model") — the `ht2.R` /
  `filing_model.R` split makes that move easier, not harder.

### 7.3 Division of labor with the Affordability pipeline

Per the income memo's alignment section: the two state systems solve
inverse problems (this branch splits national weights; the ACS spine
calibrates in place), so what transfers is machinery, not architecture —
`ht2.R`, `filing_model.R`, the anchors, the identities, the diagnostics.
The upgraded filing model built here (§6.1) is exactly what the memo says
should replace the v0 rule; building it once, in this repo, serves both.
One universe caveat for that sharing (§3.0): the two systems legitimately
differ on group quarters — the ACS spine excludes GQ and subtracts it from
its anchors; the PUF includes GQ and must not — so any shared state margin
or anchor file must carry an explicit universe tag (`resident` vs
`household`) before either side reuses the other's numbers.

### 7.4 Housekeeping

The HT2 store question is settled (confirmed 2026-08-16): the mirror was
renamed IRS-GEO → IRS-Ind — `raw_data/IRS-GEO` is now a symlink to
`raw_data/IRS-Ind`, and the maintained repo is
github.com/johniselin-budget-lab/IRS-Ind (relabel commit `2dba645`; the
rename accompanied the addition of the national Pub 1304 by-size family
this memo now leans on). `ht2_path()` has since been repointed to `IRS-Ind`
directly (`state_weights.R:65`), and the new store families (§4.1) sit beside it
under the same manifest conventions.

## 8. Open questions

Closed questions have been folded into the sections they bear on and are listed in
the revision history below; what follows is what is genuinely still open.

**Research pass A — how should tax units and income be built on the CPS ASEC?**
The one substantial unknown, and it gates the filing model. The ASEC is not the ACS
with better income detail: it has its own family/subfamily structure, its own
dependent and filing-related variables, and survey income items that are
differently defined and top-coded. Survey the established approaches —
PolicyEngine's Enhanced CPS, the Census SPM/tax-model unit construction, TAXSIM's
CPS conventions, Urban/TPC's CPS work — record where they agree and diverge, and
choose deliberately. **Start from what the extract already carries**: the shared
`CPS-ASEC/cps_asec_common` pull (ASEC 2015-2025) includes IPUMS's own `FILESTAT`
filer recode, `DEPSTAT`, `ADJGINC` and `TAXINC`, so establish what those already do
before building a unit builder that may duplicate them. Dependents and MFS are in
scope from the start (§3.2.5). Deliverable: a design note in `research/state_weights/nonfiler_residual/`,
without which the ASEC→ACS transfer is a black box.

**Does the ASEC→ACS transfer hold?** Specified as a gate in §3.2.3: if the two
surveys imply materially different national filing rates, the transfer needs
rethinking *before* the state fit rests on it. Unanswerable until pass A is done.

**Above-threshold hazard geography.** If the national scalar (D3) leaves
state-correlated residuals, the cell version needs a defensible state allocation —
and Pub 5785 is national. F7's self-employment signature (~45% with net
business/farm income) is the hook; no allocation is proposed yet.

**Two model-side gaps this rework cannot fix on its own.** Both are decisions, not
bugs, because both change published refundable-credit scores:

- **EITC has no become-filer path.** `grep become_filer src/` returns exactly two
  definitions, `ctc.R:232` and `rebate.R:86`. There is no `become_filer_eitc`, so
  the earnings-bearing non-filers this rework creates — precisely the population
  §5.4.1 shows is missing — still cannot claim EITC, and their computed `eitc` is
  multiplied out of every total by the `* filer` gate. **An EITC reform will
  therefore score identically across vintages.**
- **`become_filer_ctc` requires `qual_ei == 0` exactly**, so a non-filer with $1 of
  earned income who gains a refundable CTC keeps `filer = 0` and has its `ctc_ref`
  dropped from every total. The rework grows that population, so the
  silently-dropped credit mass grows with it. Measure it before changing either.

**Carried forward:**

- **DINA national-income variables**: availability and quality of the NI
  counterparts to `fiint`/`fidiv`/`fikgi` for non-filers — inspect the `.dta` at
  implementation (§5.1b decides A vs B on this).
- **Back-year anchors** (2014, 2016-2019): the SSA and IRA-study inputs now cover
  them (OASDI 2017-2025 plus a 1999-2025 flat series; IRA Table 4 from TY2000), but
  **EEDATA stops at data year 2023**, which bounds forward extension rather than
  back.
- **Adult-dependent netting**: the ~5.5M lower bound is a **bias to remove** from
  the estimate, not a tolerance — and §5.4.1's wage residual gives an independent
  handle on its size.

---

## Revision history

- **2026-08-16** — drafted, after Stage D was specified.
- **2026-08-18** — §3.2 rewritten around the literature pass
  (`05_filing_model_literature.md`): **Mok (2017) replaces Cilke (1998)** as the
  below-threshold model, with Cilke retained as the comparison fit; an explicit
  statement of why a survey model is still right for our data position; the ~17%
  ASEC income-understatement bias and its mitigations; and the dependent/MFS scope
  question reopened. The filing model moved from the ACS to the **CPS ASEC** with
  an explicit transfer step, sourced through the shared extract machinery. A
  federal validation battery (§5.4) was inserted between the Tax-Data and
  state-weights reworks.
- **2026-08-19** — SSA inputs placed, verified and documented; five pre-flight
  decisions taken (dependents/MFS in scope; HI as the covered-worker universe;
  vintage `2026070814`; the two age-band spaces; a computed state-varying
  tolerance); the combined-universe wage constraint sourced from SOI's W-2 study
  Table 5.A, which **corrected** an earlier reading of the PUF-vs-SSA wage gap.
  **Consolidated**: amendments folded into the body, change log replaced by this
  history, and the operational task list moved to
  `research/state_weights/plan.md`.

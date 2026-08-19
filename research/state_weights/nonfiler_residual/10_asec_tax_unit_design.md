---
title: "Building tax units and income concepts on the CPS ASEC"
role: method
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Building tax units and income concepts on the CPS ASEC

**Research pass A (design memo §8, todo A2). Written 2026-08-19.**
**Status: the convention below is the one phase C implements (C2).**

The filing model is estimated on the CPS ASEC and transferred to the ACS. Before
either can happen, one question has to be answered deliberately: *what is a tax
unit on the ASEC, and what income does it have?* The ASEC is not the ACS with
better income detail. It has its own family and subfamily structure, its own
dependency and filing recodes produced by a tax model rather than by
respondents, and income items whose definitions have changed underneath us
inside our own sample window.

This note does three things, in order: it **measures** what the extract already
carries, it **surveys** how the established models answer the question, and it
**chooses**. Every number quoted comes from a CSV written by
`09_asec_tax_unit_diagnostics.R`; nothing here is asserted from memory.

**Companions.** `../nonfiler_residual_design.md` §3.2 (the filing model this
feeds), `05_filing_model_literature.md` (which model, and why Mok), `04_findings.md`
(the residual anchors this must reconcile against).

---

## 1. The headline

**Do not build on `FILESTAT`, `DEPSTAT`, `ADJGINC` or `TAXINC`.** They are
outputs of the Census Bureau's CPS ASEC Tax Model, not survey responses, and
each fails in a way that is disqualifying for our purpose:

| Recode | What kills it for us |
|---|---|
| `FILESTAT` | Its filer *count* is **calibrated to administrative totals**, so agreement with our anchor is by construction, not corroboration. Its *mix* is badly wrong: head-of-household returns are **41.5% short** in TY2022. Married-filing-separately does not exist. TY2020–21 are broken outright. |
| `DEPSTAT` | **10.8%** of dependents point at someone the same model codes as a non-filer. TY2014 sits on the far side of a level break IPUMS itself flags. |
| `ADJGINC` / `TAXINC` | Statistically matched to the **SOI Public Use File** — the same PUF Tax-Data is built from. Using them would make the correction circular. |

Build our own units, following **Mok (2017)'s conventions**, because those are
the units her coefficients were estimated on (A4). Keep the Census recodes as
benchmarks and cross-checks, never as inputs.

---

## 2. What the extract already carries

`raw_data/CPS-ASEC/cps_asec_common`, ASEC 2015–2025 = **income years 2014–2024**,
92 variables, no case selection. Both anchor years (TY2017, TY2022) are in it.

### 2.1 The Census recodes are model output, not data

IPUMS is explicit, in the variable description for every one of them:

> "FILESTAT, like other tax-related variables included in the ASEC CPS (ADJGINC,
> CAPGAIN, CAPLOSS, EITCRED, FEDRETIR, FEDTAX, FICA, MARGTAX, STATETAX, TAXINC…)
> was not determined by direct questioning of respondents. Rather, values for
> these variables come from the Census Bureau's tax model, which simulates
> individual tax returns."

The model is documented in Lin (2022), *Methods and Assumptions of the CPS ASEC
Tax Model*, SEHSD-WP2022-18 — the authority for everything in §3.1 below.

**They do, however, define tax units implicitly, and cleanly**
(`asec_A6_unit_structure_{year}.csv`). For joint filers the model carries
`ADJGINC`, `FEDTAX` and `EITCRED` on **exactly one spouse**. Of 120.34M persons
in joint units in TY2022, 119.87M are in couples where exactly one spouse
carries a positive `ADJGINC` and **none** are in couples where both do. TY2017
is the same picture: 117.96M of 118.12M, with 0.01M carrying both. So a unit
can be reconstructed as
*designated filer + `SPLOC` spouse + persons whose `DEPSTAT` points at the
filer*. That reconstruction is feasible. It is just not one we should use.

### 2.2 `FILESTAT`: calibrated on the total, wrong on the mix

The model's filing test is eight rules, the last of which is "total income above
$2,000" — a floor deliberately **below** the IRS requirement, and Lin's footnote
18 says why: *"As some people file even when they are not required, a minimum
income lower than the IRS requirements was introduced to match the count of
filers (O'Hara 2004)."*

So the total agrees with SOI because it was made to. The composition was not,
and that is where the structural assumptions surface
(`asec_A2_filing_status_{year}.csv`):

| Returns, TY2022 | SOI Pub 1304 T1.6 | ASEC `FILESTAT` | ratio |
|---|---:|---:|---:|
| joint (incl. surviving spouse) | 54.89M | 60.17M | 1.096 |
| **married filing separately** | **3.99M** | **0** | **—** |
| **head of household** | **21.27M** | **12.44M** | **0.585** |
| single | 81.19M | 83.77M | 1.032 |
| **total** | 161.34M | 156.37M | 0.969 |

TY2017 is the same picture: HoH 0.538, MFS zero, total 0.957. **The total is
within 3–4%; head-of-household is short by 8.8M returns and MFS by 4.0M.**

This is not a defect peculiar to the Census model — Mok hit it too, and said so:
*"There are significantly more head-of-household filing units in the tax data
than would be suggested by the information in the CPS… a significant gap remains
even after those adjustments."* It is a property of the survey, and any builder
we write will meet it. What **D-A6** decides is what to do about it.

### 2.3 `FILESTAT` is broken in TY2020–2021

From `asec_A1_filestat_series.csv`, non-filing adults 18+:

| TY | 2017 | 2018 | 2019 | **2020** | **2021** | 2022 | 2023 | 2024 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| M | 45.41 | 44.24 | 42.92 | **11.69** | **11.47** | 43.96 | 43.50 | 46.24 |

Non-filers collapse by three quarters and snap back. Lin documents the cause:
for the pandemic years the model routed Economic Impact Payments through filing
units, with special rules for presumed non-filers. Whatever the mechanism,
**TY2020 and TY2021 are unusable as a filing benchmark**, and the same two years
show implied returns of 181–183M against SOI's ~157–160M.

### 2.4 `DEPSTAT` is a pointer that does not cohere with `FILESTAT`

`DEPSTAT` gives the **Census line number** (`LINENO`) of the person who claimed
the respondent. `LINENO` is not in our extract, so `PERNUM` has to stand in.
That substitution is nearly harmless — but the pointer's *content* is not
(`asec_A3_depstat_{year}.csv`, TY2022):

| | persons | % of dependents |
|---|---:|---:|
| dependents (`DEPSTAT` > 0) | 85.60M | 100% |
| adult dependents (18+) | 13.80M | 16.1% |
| pointer resolves to no `PERNUM` | 0.10M | 0.1% |
| …of which in a household with a `LINENO` gap | 0.10M | **100% of the failures** |
| **pointer lands on a modelled NON-FILER** | **9.27M** | **10.8%** |
| pointer lands on a person under 18 | 2.83M | 3.3% |

Two conclusions, and they point opposite ways. The resolution failures are
**entirely** explained by `LINENO` gaps, so adding `LINENO` fixes them exactly
(§5). But the 10.8% is not a pointer artifact — it is a genuine inconsistency
between two outputs of the same model: you cannot be claimed as a dependent by
someone who does not file. TY2017 shows the same 10.4%.

IPUMS warns about this variable in terms strong enough to quote: *"Data checking
has revealed dramatic shifts across time in the proportion of persons who are
dependent, which may indicate inaccuracies in the data. Researchers should
exercise caution in using DEPSTAT."* The shift is visible in our own window —
adult dependents step from **6.37M (TY2014) to 13.36M (TY2015)** and then sit
flat at 13.2–14.7M. **TY2014, one of the back years, is not comparable.**

The 13.80M adult dependents matter beyond data quality. The design memo carries
adult-dependent netting at a **~5.5M lower bound, 12% of the residual**. If the
ASEC figure is even roughly right, the netting is **2.5× larger** than the bound
now in the memo. That is a bias to size properly in phase C, not a tolerance
(P5 already excludes it from the tolerance budget for exactly this reason).

### 2.5 `ADJGINC` and `TAXINC` are matched to the PUF

Lin §"Statistical Match": the model matches ASEC records to the **SOI PUF** to
fill in what the survey does not collect — self-employed health insurance, HSA
and IRA deductions, and the whole itemized-deduction block. Imputed itemized
deductions are then zeroed by random draw within AGI bins (100% of filers under
$10,000 of AGI, 95% under $15,000, 75% under $30,000, 60% under $50,000). The
match runs on a long lag: the TY2020 model used the **2014** PUF.

We are building a filing model in order to fix the non-filer population in a
PUF-derived file. An income concept imputed *from that PUF* cannot be the
yardstick. `ADJGINC` stays as a benchmark; it is not an input.

### 2.6 Which income items can carry a tax concept

`asec_A4_income_{year}.csv`. The comparison is deliberately three-way, because
the universes differ: SOI HT2 is income **on filed returns**, SSA EEDATA is
wages of **all covered workers**, and the ASEC covers the whole civilian
noninstitutional population.

| TY2022 | ASEC | SOI HT2 | ASEC/SOI | ASEC/SSA |
|---|---:|---:|---:|---:|
| `INCWAGE` | $10,748B | $9,632B | 1.116 | **1.011** |
| `ADJGINC` | $13,278B | $14,676B | 0.905 | — |
| `INCINT` | $344B | $131B | 2.624 | — |
| `INCDIVID` | $180B | $400B | **0.449** | — |
| `EITCRED` | $36B | $59B | **0.606** | — |

**Wages are sound.** ASEC exceeding HT2 is expected and uninformative — HT2 is
filers only. Against SSA HI covered wage-and-salary earnings, which like the
ASEC covers workers regardless of filing, the ASEC total is **1.011 in TY2022
and 1.038 in TY2017**. That is the like-for-like read, and it says the ASEC
wage aggregate is essentially right. Since the filing decision is dominated by
wages, this is the single most reassuring number in the audit.

**Dividends are not**, at 45% of the SOI total on a *wider* universe. Interest
runs the other way, at 2.6×, which is a definitional gap (the ASEC asks about
interest received; SOI reports taxable interest) rather than an error — but it
means neither can be used as a level. The Census model's own EITC reaches only
61% of SOI dollars.

### 2.7 A third break: pensions and annuities vanish from our extract at TY2018

`asec_A1b_income_continuity.csv` shows `INCRETIR` falling **69%** between TY2017
($571B, 22.9M recipients) and TY2018 ($180B, 10.6M). IPUMS states the cause
without ambiguity:

> "Beginning in 2019, income from retirement accounts, pension plans, and
> annuities are split into separate variables… only those respondents age 58 and
> over who received income from retirement accounts last year are included in
> INCRETIR, where previously those 15 and older are included… **INCRETIR is NOT
> comparable before and after 2019.**"

Pensions and annuities moved to `INCPEN1`/`INCPEN2`/`INCRANN`, **none of which
this extract pulls**. `INCTOT` is unaffected ($11.1T → $11.7T), so the mass is
still in the survey and only our decomposition of it is short.

This bites directly. Mok's covariates include the **presence of retirement
income**, and **our two anchor years sit on opposite sides of the break** —
TY2017 pre, TY2022 post. Fixed by the extract change in §5; until then, no
retirement-income covariate is comparable across the two anchors.

### 2.8 Universe: the ASEC has almost no group quarters

Group-quarters residents are **0.27M in TY2022** — the ASEC frame is the
civilian noninstitutional population plus military living with civilians, so
institutional GQ and barracks are out by construction. The PUF universe includes
GQ, and the residual anchor is PEP-based and includes it too.

The arithmetic reconciles cleanly. For TY2022: PEP adults exceed ASEC adults by
2.48M, ASEC filing adults exceed the T1.6 anchor by 0.07M, and the ASEC's
non-filer count falls short of the residual anchor by 2.54M — so the identity
`non-filer gap = population gap − filer gap` predicts 2.55M against 2.54M
observed. TY2017 predicts 1.92M (3.64M − 1.72M) against 1.89M observed. **The
ASEC–anchor non-filer gap is a universe difference, not a modelling
disagreement**, to within 0.03M in both years. That is worth knowing
before the C6 gate reads any discrepancy as a transfer failure.

It also means the ASEC can say **nothing** about GQ filing behaviour — which is
what C7 already assumes, and now has a number for.

### 2.9 How ambiguous are the living arrangements?

`asec_A5_arrangements_{year}.csv`, TY2022 — the cases every approach in §3
handles differently, sized so §4's choices can be weighed:

| | persons | % of population |
|---|---:|---:|
| unmarried partners | 9.27M | 2.80% |
| persons in households containing an unmarried partner | 26.39M | 7.98% |
| children under 18 in those households | 5.66M | 1.71% |
| persons in multi-family households (`FAMUNIT` > 1) | 18.47M | 5.58% |
| other relatives of the householder | 23.33M | 7.05% |
| non-relatives other than partners | 3.54M | 1.07% |
| **union of the above (households, excluding GQ)** | **88.98M** | **26.91%** |

The categories overlap, so the union is the figure that matters: **89.0M people,
26.9% of the population, live in a household containing at least one
relationship the conventions in §3 treat differently.** That is not a claim that
27% of people would be assigned to different units — most of them are the
householder or a spouse or a young child, and every approach agrees about those.
It is the *exposure*: the share of the population living somewhere the choice
could reach. A convention picked casually would be picked over a quarter of the
country.

The 5.66M children in unmarried-partner households are a smaller and sharper
number, and they are precisely the group Mok reallocates to close the
head-of-household gap (§2.2).

---

## 3. How the established approaches do it

### 3.1 Census CPS ASEC Tax Model (Lin 2022)

Partitions each household into three mutually exclusive subsets — **married**
(non-zero spouse pointer), **dependents** (non-zero parent pointer), **others** —
then adjusts toward IRS definitions:

- Dependents restricted to **children aged 18 or under, children under 24 and
  enrolled in school, or adult children with a disability**. Qualifying relatives
  who are not children are *not* identified; elderly non-disabled dependents are
  not modelled. Anyone under 15 in "others" is reassigned to children. **Married
  persons are never dependents.**
- Dependents attach via the parent pointer; under-15s without one attach to the
  household's main unit. Dependents who file their own return keep the parent's
  unit ID but get their own observation.
- Filing status: joint if two spouses, HoH if a single person with dependents,
  single otherwise. **All married couples are assumed to file jointly**; MFS and
  qualifying widow(er) are not modelled, on the reasoning that joint filing is
  almost always advantageous.
- The filing decision **is** modelled — the eight rules of §2.2.

Lin's own summary of the limitation is the cleanest statement of the trade-off:
*"The CPS ASEC Tax Model, unlike other tax models, presorts persons into tax
units. Other tax models, such as TAXSIM and the Bakija Income Tax Calculator,
leave the construction of tax units to the users."*

### 3.2 TRIM3 (Urban Institute)

Units are **all single individuals aged 15 and above and married couples**,
regardless of taxable income. Distinctively, **a filing unit may itself be
claimed as a dependent of another unit** in the household — richer than the
Census model's flat parent-pointer attachment. Dependency runs through five
explicit IRS tests: relationship, married-dependent, citizenship (assumed
satisfied), gross income, and support. Filing status is joint / HoH / single —
**again no MFS**.

Critically, TRIM3 **does not model the filing decision at all**: it computes
taxes for all potential units and aligns to aggregates by adjusting matched
income amounts and credit participation rates. That makes it a good source of
unit-construction rules and no use at all as a filing model.

### 3.3 TAXSIM (NBER)

Leaves unit construction to the user. Its CPS preparation code identifies units,
filing status and qualifying children, but the design point is that the
convention is the analyst's responsibility. Useful as a calculator downstream of
our builder; not a source of the convention.

### 3.4 PolicyEngine Enhanced CPS

Keeps the CPS household structure intact and imputes **67 tax variables from the
PUF onto CPS records using quantile regression forests**, then reweights with
dropout-regularised gradient descent to **2,813 targets** from SOI, Census, CBO
and Treasury. It solves a different problem from ours — enriching the CPS with
tax detail — and it solves it by leaning on the PUF, which is the file we are
trying to correct. The reweighting design is worth borrowing from in phase F;
the imputation is not a route to a filing model.

### 3.5 Census SPM resource units — a deliberate non-answer

SPM units group **all related persons, cohabiting partners, relatives of
cohabiting partners, foster children, and unrelated individuals under 15**. They
are built to capture resource sharing, which is the opposite of what a tax unit
captures. They are the right comparator for asking *how much* the choice matters
— they bound the 8% in §2.9 from the other side — and the wrong unit to adopt.

### 3.6 Mok (2017) — the conventions that actually bind

Mok built units on the March 2007 CPS linked to the IRS Individual Master File,
and **A4 transplants her Table 14 coefficients**. Her rules are therefore not one
option among several; they are the frame those coefficients are only valid in:

- Units organised by **marital status and dependency (age, relationship, and —
  for adults — financial support)**. Spouses in the same household are **always**
  in the same unit; spouses living apart are single or HoH.
- The **householder is the primary taxpayer**, the spouse secondary.
- **Related subfamilies**: if the subfamily head could be a qualifying child or
  relative of the householder *and has no earned income*, the subfamily becomes
  dependents of the householder's unit. An **unrelated** subfamily is its own unit.
- **Dependents can head their own unit**; for most of her analyses, units headed
  by dependents are excluded.
- **Head-of-household reallocation**: qualifying children are moved between
  unmarried partners and across related subfamilies with wages to raise the HoH
  count toward the tax data. She flags this as behavioural, not legal — *"those
  adjustments are made to reflect the behavior of tax filers and may not conform
  to the legal criteria"* — and reports that a significant gap survives it.
- **Income**: only income of the primary and spouse. AGI from wages, net
  self-employment, unemployment compensation, retirement income, interest and
  dividends, rental income, alimony, survivors' and disability income (excluding
  workers' compensation), educational assistance, and taxable Social Security.
  **Capital gains are absent** — not collected in the 2006 CPS.

Her appendix is, uniquely, a *measured* error budget for CPS-constructed units
against real 1040s, from linked data:

| | units | share |
|---|---:|---:|
| all members link to the same 1040 | 101M | 71% |
| matches one 1040, not everyone links | 9M | 7% |
| **cannot be matched to any 1040** (the non-filers) | **24M** | **17%** |

and, for the residual multi-1040 cases, **12% are married couples filing
separately**, 30% have one spouse single and the other joint, 23% one single and
one HoH. Dependents claimed on 1040s exceed CPS dependents by about **11
million** — dependents who live outside the survey household. Wages differ by
20% or more between the CPS and the 1040 for about **40%** of matched units,
usually higher on the 1040.

### 3.7 Where they agree and diverge

| | Census model | TRIM3 | Mok | our choice (§4) |
|---|---|---|---|---|
| spouses in one household | always joint | always joint | always joint | **always joint** |
| married filing separately | not modelled | not modelled | not modelled | **post-step, calibrated** |
| unit may be another unit's dependent | no | **yes** | yes (subfamily rule) | **yes** |
| non-child dependents | not identified | gross income + support tests | age/relationship/support | **support test** |
| dependent heads own unit | yes, same unit ID | yes | yes, usually excluded | **yes, retained and tagged** |
| filing decision | modelled, **calibrated to filer counts** | **not modelled** | **modelled from linked data** | **Mok** |
| HoH shortfall | untreated (0.585 of SOI) | untreated | reallocation, gap remains | **both, reported** |
| income source | ASEC + **PUF match** | ASEC + IRS match | **ASEC components only** | **ASEC components only** |

The agreement is near-universal on the easy part (spouses in one household file
jointly) and near-universal on the hard part too — **nobody models MFS, and
everybody is short on head-of-household.** Those are properties of household
survey data, not of any one model's carelessness.

---

## 4. The convention we adopt

### D-A1. Build our own units; do not adopt `FILESTAT`

Four independent reasons, any one sufficient: MFS is out of scope for the recode
but in scope for us (P3); HoH is 41.5% short; the filer count is calibrated to
the administrative total we are independently trying to reproduce, so adopting it
would make the C6 gate vacuous; and two of our eleven years are broken.

**But keep it.** `FILESTAT` is the best available *benchmark* precisely because
it is calibrated — carry it through the builder as a comparison column and report
our unit counts against it in every year except TY2020–21.

### D-A2. Follow Mok's conventions, rule for rule

Not because they are better in the abstract — TRIM3's dependency tests are more
faithful to the code — but because **A4 transplants Mok's coefficients**. A unit
built differently is a unit outside her estimation frame, and the coefficients
would be silently wrong rather than loudly wrong. Where Mok is ambiguous, prefer
TRIM3's rule and record the choice.

Specifically:
1. Householder (`RELATE == 101`) is the primary; spouse via `SPLOC` is secondary;
   spouses in the same household are always one unit.
2. Children attach via `MOMLOC`/`POPLOC`.
3. Related subfamilies: subfamily head who could be a qualifying child or
   relative of the householder **and has no earned income** → dependents of the
   householder's unit. Unrelated subfamily → its own unit.
4. Adult dependency uses a **support test**, not age alone.
5. Dependents who meet the filing requirement head their own unit.

### D-A3. MFS is a calibrated post-step, never part of the estimation frame

P3 puts MFS in scope, and no CPS-based approach can observe it. Resolve the
tension by **separating the two stages**:

- **Stage 1 — estimation frame.** Spouses in one household are one joint unit,
  exactly as Mok has it. The filing model is scored here. Coefficient validity
  is preserved.
- **Stage 2 — post-scoring split.** Split a calibrated share of joint units into
  MFS pairs, targeted to SOI's MFS return count (3.99M in TY2022, 3.21M in
  TY2017), by state where HT2 supports it.

The split does not change *who files* — both spouses file either way — only the
return count and status mix, which is what the state-weights fit consumes. Mok's
linked data give the one empirical anchor available: **12% of constructed units
that matched multiple 1040s were MFS.** Report the split as a calibration, never
as an observation.

### D-A4. Dependents are constructed, retained and tagged

Mok drops dependent-headed units from most analyses. We cannot: our residual
anchor counts **adults**, and adult dependents sit inside it — 13.80M of them by
`DEPSTAT`, against the memo's ~5.5M netting bound. Construct them, retain them,
tag them, and report them as a separate line in every reconciliation. P3 requires
it and the netting question depends on it.

### D-A5. Income from ASEC components, with `ADJGINC` alongside as a benchmark

Gross income for the filing test is built from the ASEC items on Mok's list, not
from `ADJGINC` (§2.5). Two deliberate deviations, both recorded here rather than
discovered later:

- **Capital gains.** Absent from Mok's 2006 CPS; available as an ASEC item from
  2014 and as `CAPGAIN`/`CAPLOSS` from the Census tax model. Build **two**
  income measures — one **excluding** gains, which is the one the transplanted
  coefficients are scored on, and one **including** them, which is the one
  reported and compared to SOI AGI. Never mix them.
- **Dividends and interest carry known bias as levels** (§2.6: dividends at 0.449
  of SOI, interest at 2.624). Their amounts still have to enter gross income —
  there is no substitute — but two things follow. Mok's *covariates* use
  **presence**, not amount, so the probit is insulated; and the filing-threshold
  test is not, so report the threshold test's gross income **with and without
  investment income** and check the filer count is not turning on the difference.

### D-A6. Implement the legal head-of-household rule first; report the reallocation separately

Every model has this shortfall: the Census recode lands at 0.585 of SOI in
TY2022 and 0.538 in TY2017, and Mok reports it on her own units too, so expect
ours to be short as well — by how much is an output of C2, not an input to it.
Order of work: implement the **legal** rule, measure *our* shortfall against SOI,
then apply a Mok-style reallocation of qualifying children
across unmarried partners and related subfamilies as an **explicitly flagged
behavioural adjustment**. Publish both. The reallocated version must never be the
only number in a table — it is the one place in this design where we knowingly
depart from the tax code.

### D-A7. Group quarters: out of the ASEC, so out of the estimation

The ASEC has 0.27M GQ persons (§2.8), so the filing model is estimated on a
frame that excludes institutional GQ and barracks — as Mok's was. Scoring those
records on the ACS extrapolates outside the estimation frame. That is already
C7's requirement; §2.8 supplies the number and the exact reconciliation showing
the ASEC–anchor non-filer gap *is* the universe difference.

---

## 5. Extract changes this requires

All are additions to the shared `parameters.cps.yaml` — per the memo's rule, add
to the common extract rather than forking one. Every name below was read off the
IPUMS CPS documentation rather than guessed, but **none has yet been validated
against the IPUMS API for all eleven samples**, which is the standard A3 held
itself to and the step that catches a variable that exists but is absent from
some years. Validate first, then pull; the pull re-runs all eleven years.

| Variable(s) | Why | Priority |
|---|---|---|
| `INCPEN1`, `INCPEN2`, `INCRANN` (with `SRCPEN1`/`SRCPEN2`, `INCRET1`/`INCRET2`) | Restores pension and annuity income from TY2018 on. Without them the retirement-income covariate is not comparable across our two anchor years (§2.7). | **blocking for C5** |
| `LINENO` | Resolves `DEPSTAT` exactly. Accounts for **100%** of today's unresolved pointers (§2.4). | **blocking for C2** |
| `CAPGAIN`, `CAPLOSS` | The Census tax model's capital-gains items, survey-collected since 2014. Needed for the reported AGI measure of D-A5. | high |
| `FAMREL`, `FTYPE`, `FAMID` | Census subfamily identification. **IPUMS CPS has no `SUBFAM`/`SFTYPE`/`SFRELATE`** — those are IPUMS USA variables — so subfamilies must come from `MOMLOC`/`POPLOC`/`SPLOC` plus these. Required by D-A2 rule 3. | high |
| `INCALIM` | Alimony, on Mok's AGI list; separated from `INCALOTH` into its own variable in 1988. Not currently pulled. | medium |

**The subfamily asymmetry is a transfer risk, not just an extract gap.** The ACS
side has `SUBFAM` natively; the ASEC side must reconstruct it. The same rule will
be implemented twice against different inputs, which is exactly the situation
where two implementations drift. Write the subfamily assignment once, as a shared
helper with the survey as an argument, and test that both sides agree on a common
case.

---

## 6. What this means for phase C

- **C2** implements §4. The extract changes in §5 come first — two of them are
  blocking.
- **C5** (do Mok's covariates survive the transfer?) has a known casualty
  already: retirement income, until `INCPEN*`/`INCRANN` land. Check that one
  first rather than last.
- **C6** (the ASEC↔ACS filing-rate gate) should be read against §2.8's
  reconciliation. A 4–5% ASEC shortfall against the PEP-based anchor is the
  *expected* universe difference and is not evidence of transfer failure; the
  gate should be specified on a common universe, not on raw rates.
- **Back years.** TY2014 is unusable for anything involving `DEPSTAT`, and
  TY2020–21 for anything involving `FILESTAT`. Since those are benchmark
  variables rather than inputs under §4, this constrains *validation* coverage,
  not estimation — but it should be stated wherever back-year weights are fit.
- **Expected accuracy.** Mok's linked-data appendix is the best available prior
  for how well any of this can work: **71%** of constructed units match a single
  1040 exactly, **17%** match none, and ~11M dependents claimed on returns live
  outside the survey household. A builder that matched better than that on CPS
  data alone would be a reason for suspicion, not celebration.

---

## 7. What this note does not close

1. **The support test needs an operational definition.** Mok says adult
   dependency uses financial support; TRIM3 divides household spending equally
   across family members and compares to the dependent's own income. TRIM3's is
   implementable and Mok's is not, as stated. Adopt TRIM3's, and record it as a
   deviation from Mok — it is the one place where D-A2's "follow Mok" cannot be
   followed literally.
2. **The MFS split has no state-level target yet.** HT2 identities put MFS in a
   residual (`n_returns − n_single − n_joint − n_hoh`) that also absorbs
   surviving spouses. Whether that residual is clean enough to target by state,
   or whether MFS must be split on the national share, is an open question for
   C4.
3. **The adult-dependent count needs its own resolution.** `DEPSTAT` says 13.80M,
   the memo carries ~5.5M as a lower bound, and Mok's linked data say CPS
   dependents *understate* 1040 dependents by ~11M. These are not obviously
   reconcilable and the netting is 12%+ of the residual. It deserves a short
   dedicated pass before C4 calibrates anything to it.
4. **Whether the ACS can support the subfamily rule at the same fidelity.**
   `SUBFAM` exists there, but its definition is IPUMS's rather than Census's, and
   the two differ. Test before relying on it.

---

## Sources

- Daniel Lin, *Methods and Assumptions of the CPS ASEC Tax Model*, U.S. Census
  Bureau SEHSD Working Paper FY-2022-18 (November 2022) —
  [census.gov](https://www.census.gov/library/working-papers/2022/demo/SEHSD-wp2022-18.html)
- Shannon Mok, *Estimating the Characteristics of Filers and Nonfilers*, CBO
  Working Paper 2017-06 — local copy at `resources/mok2017_cbo_wp2017-06.pdf`
- TRIM3 federal income tax module documentation, Urban Institute —
  [boreas.urban.org](https://boreas.urban.org/documentation/federaltax/main.php)
- PolicyEngine US Data methodology —
  [policyengine.github.io](https://policyengine.github.io/policyengine-us-data/)
- Amy O'Hara, *New Methods for Simulating CPS Taxes* (2004) —
  [cps.ipums.org](https://cps.ipums.org/cps/resources/adjginc/oharataxmodel.pdf)
- IPUMS CPS variable documentation: `FILESTAT`, `DEPSTAT`, `ADJGINC`, `TAXINC`,
  [`INCRETIR`](https://cps.ipums.org/cps-action/variables/INCRETIR),
  [`INCALOTH`](https://cps.ipums.org/cps-action/variables/INCALOTH),
  [ASEC 2019 changes](https://cps.ipums.org/cps/asec_2019_changes.shtml)
- NBER TAXSIM CPS preparation code — [taxsim.nber.org](https://taxsim.nber.org/to-taxsim/)

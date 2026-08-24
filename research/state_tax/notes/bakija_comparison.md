---
title: "What Bakija's IncTaxCalc models that we may not"
role: notes
workstream: state_tax
status: open
updated: 2026-08-23
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Bakija's IncTaxCalc, read against our state module

Source: Jon Bakija, *Documentation for a Comprehensive Historical U.S. Federal
and State Income Tax Calculator Program*, Williams College, dated 20 March 2019
(`https://web.williams.edu/Economics/wp/BakijaIncTaxCalcDoc.pdf`). Read
2026-08-22. It is the most complete public taxonomy of state income tax
mechanisms in existence, covering 1900-forward, so its **parameter vocabulary is
a checklist** — anything it needed a variable for is a mechanism some state
actually has.

Two caveats on using it this way. It is a **2019** document, so provisions
repealed since (and enacted since) are not reflected; and its coverage goal is
historical breadth, so some mechanisms it carries are dead in our 2017-forward
window. Every item below is therefore flagged for what still needs checking
against primary sources rather than asserted from the document alone.

---

## 1. Silent gaps — mechanisms we neither model nor document

### 1a. Colorado and Wisconsin state AMTs

Bakija's `mintaxtype = 3` — "Addition to tax = [(mintaxrate/100)*(AMTI -
AMTEX)] - ordinary state income tax liability" — is listed for **CA 1987-present,
CO 1987-present, MN 1985-present, WI 1987-present**, plus Iowa's separately.

We have a **no-state-AMT policy** and Phase 7 owns state AMTs, so not modelling
them is a decision, not an oversight. But the policy is to *document* them, and:

| state | documented? |
|---|---|
| MN | yes — `mn.md` "MN AMT (6.75%) document-only per the no-state-AMT policy" |
| CA | yes — deferred with a written plan (`ca_misc_amt_plan.md`) |
| IA | yes — `ia.md` calls it "the largest unencoded feature" |
| **CO** | **no mention anywhere in `research/`** |
| **WI** | **no mention anywhere in `research/`** |

`params_schema.yaml` carries no `st_*amt*` parameter of any kind, so there is no
machinery either.

**To verify before acting** (Bakija is 2019 vintage): Wisconsin repealed its AMT
for taxable years beginning after 2018, so its exposure is likely TY2017-2018
only; Colorado's (C.R.S. 39-22-105) appears still live. Neither has been checked
against primary sources here.

**Why it matters now:** an AMT bites at the top of the distribution, which is
where these states' residuals sit. CO is 3 cells short (min 0.939), WI 3 short
(min 0.915), MN 8 short (min 0.823).

Bakija also records the state-specific wrinkles, which is the expensive part of
the research if we ever build it: **WI** allows a subtraction from AMTI for its
capital-gains exclusion, **CO** one for its retirement-income exclusion, **MN**
limits charitable contributions to the excess over 1.3% of AGI, and **CA** has
its own AMT exemption and phase-out threshold, different from federal.

### 1b. Spouse-allocation is four dimensions, and we parameterize one

Separate/combined filing needs four independent allocation rules in Bakija's
scheme, coded per state-year:

| his variable | what it decides |
|---|---|
| `itemalloc` | itemized deductions: own-deductions / free choice, **or** proportional to each spouse's AGI share |
| `stdalloc` | standard deduction: half the joint maximum, or the **full** value each |
| `exalloc` | exemptions |
| `credalloc` | credits |

`src/calc/state/st_split.R` divides unobserved-ownership inputs **evenly** (the
VA/KY convention) and its header already records that Arkansas "pools itemized
deductions and prorates them by AGI share" and so "will need that as a per-state
option" — which is exactly `itemalloc = 1`. The other three dimensions we handle
implicitly through per-state parameters rather than as declared allocation
rules, and `stdalloc = 1` (each separate filer gets the FULL joint maximum, not
half) is a live shape — Iowa's per-column standard deduction is already noted in
`STATUS.md` as "NOT a clean fraction of the joint one".

**This is a design observation, not a defect list**: our parameterization may
already cover the same ground. What it argues is that when AL/AR/MS get wired,
the allocation rules should be *declared per state* rather than inherited from
the even-split default.

---

## 2. Corroboration — gaps we already know, with his effort as the price tag

### 2a. Circuit-breaker property tax credits: the biggest section in the document

Bakija spends more space on these than on any other mechanism — **39 `cb*` /
`xcb*` parameters**, a design taxonomy (`cbtype`), separate parameter sets for
above and below an age threshold, a rent-to-property-tax equivalence rate
(`cbrenteq`), its own income definition (`cbincdef`), and an all-caps protocol
for research assistants about which *year's* income each form uses.

We meet this class one state at a time and keep paying for it: RI-1040H (both
legs, the PE-side row added today), MT's property rebate, VT and MN renter
credits, and his own list reaches DC, IL, MI, IN, KS, MD and CT. It is the
single strongest external argument for the **Tier 1 rent / property-tax
imputation** in `state_data_imputation_plan.md`.

Two honest notes from his text. He explicitly **does not** model homestead
exemptions at all ("I do not intend to incorporate it"), and he records that
circuit-breaker "phase-ins and phase-outs that operate in a step function
pattern have been smoothed and approximated" — so even a well-resourced,
decades-long effort approximates here. That is a reasonable ceiling to hold
ourselves to rather than exceed.

### 2b. Local income taxes

Bakija carries a `local` switch with three settings (off / all states with local
taxes / only states where they apply statewide) and a `localtax` output, plus a
warning about its state. Our Phase 7 defers MD counties and NYC/Yonkers. His
treatment confirms the scope is real and that a partial implementation is the
normal compromise.

### 2c. Arkansas's low-income tables are three mechanisms, not one

`STATUS.md` treats AR's Low Income Tax Tables as a single unbuilt thing. Bakija
codes **three**: `lowtab1` (1973-1990), `lowtab2` (1991-2006), `lowtab3`
(2007-). Only `lowtab3` is in our window, which makes the job smaller than the
STATUS wording implies — worth knowing before scoping it.

He also carries **11 `lowtype` designs** for low-income relief generally,
including two shapes worth auditing our coverage against: type 10, "exclude all
labor income up to a threshold if AGI is less than [a limit]", and type 11, "a
credit that is a percentage of labor income, eligible if AGI is below a
threshold".

---

## 3. Negative findings — recorded so nobody re-derives them

Checked against his taxonomy and **already modelled on our side**:

| Bakija mechanism | our implementation |
|---|---|
| `xtaxtype = paratax` — CT's "3% Tax Rate Phase-Out Add-Back" and "Tax Recapture" | `st_ord.step_recap_*` segments plus a dense table; `ct/ord.yaml` cites Tables C and D |
| `xtaxtype = maxtax` — optional maximum tax (IA, UT among others) | Iowa's alternate tax (the MA no-tax-status formula) and Utah's taxpayer tax credit (`st_ded_credit`) |
| `sptx = cgtax` / `cgmax` — special or alternative-maximum capital gains tax | Hawaii's 7.25% alternative CG tax (`st_ord.kg_alt_rate`/`kg_alt_floor`) and Montana's preferential schedule (`st_ord.kg_pref_*`) |
| `sptx = charcred` — non-itemizer charitable credit | Vermont's `st_char_credit` |
| `xtaxtype = liabcred` — credit as a percentage of liability after credits | `st_pct_credit` (CT Table E) and Kentucky's family-size credit |

---

## 4. What I would do with this

1. **Cheap and immediate:** document the CO and WI AMTs in their source packets
   under the existing no-state-AMT policy, after checking WI's repeal date and
   CO's current status against primary sources. This closes a documentation hole
   the policy already requires closing.
2. **Feeds an existing decision:** add Bakija's circuit-breaker taxonomy as
   supporting evidence in `state_data_imputation_plan.md` — it prices the Tier 1
   rent/property work from the outside.
3. **Feeds an existing plan:** when the AL/AR/MS split-election wiring happens,
   declare the four allocation dimensions per state rather than relying on the
   even-split default, and scope AR's tables as `lowtab3` only.
4. **Audit, not urgent:** check our low-income relief coverage against his 11
   `lowtype` designs, particularly types 10 and 11.

## Revision history

- **2026-08-22** — written from a read of the 2019 documentation. Findings are
  flagged for primary-source verification where his vintage matters; nothing
  here has been acted on.

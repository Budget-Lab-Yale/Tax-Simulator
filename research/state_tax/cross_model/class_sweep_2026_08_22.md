---
title: "Cross-state class sweep — crosswalk exposure, T18, P9"
role: evidence
workstream: state_tax
status: current
updated: 2026-08-22
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# Cross-state class sweep, 2026-08-22

Per-state triage had re-derived the same three divergence classes seven, three
and three times respectively. This pass asks each class of **every** state at
once: which states' encodings give the class a route, does the class actually
bite there, and does the evidence survive a control.

Scripts are in `output/probe/` (gitignored, regenerable):
`class_sweep_measure.R`, `class_sweep_control.R`, `class_sweep_add_rows.R`,
`mawi_care_probe.R`, `p9_sweep_probe.R`, `p9_encoding_scan.R`,
`p9_confirm_probe.R`, `p9_sweep_add_rows.R`.

**Method, and the rule that governed what got a row.** Applicability was read
off the *resolved* state law (`build_state_tax_law()`), not off YAML greps, so a
parameter absent from a state's file takes the schema default the calculator
would actually use. A row was then added only where the class was *measured* to
bite and the measurement survived a falsification control. Everything measured
and rejected is recorded below, because a negative result is what stops the next
pass from re-running the same probe.

---

## 1. Crosswalk exposure (the DC/CA class) — 13 rows added, 2 rejected

**Route.** The class needs somewhere for the crosswalk's unstripped SALT
(`salt_inc_sales + salt_pers`, riding to TAXSIM inside `otheritem`) and its
unhanded Schedule A components (investment interest, "other") to land. Of the 42
broad-IIT jurisdictions, 14 already carried a row. The scan found the route in
15 more via a state itemized deduction (`st_ded.item_allowed`), and in 3 more
(ND, SC, VT) only via the federal-itemizing keying that CO and UT use.

**Measured, pooled 2017–2020 on the clean subset.** Exposed-record match@\$100
against unexposed:

| state | exposed share | exposed | unexposed | verdict |
|---|---|---|---|---|
| IA | 13.3% | 0.163 | 0.813 | row added |
| AR | 14.4% | 0.180 | 0.724 | row added |
| LA | 4.6% | 0.185 | 0.944 | row added |
| KS | 3.3% | 0.186 | 0.913 | row added |
| KY | 4.7% | 0.219 | 0.831 | row added |
| AL | 22.4% | 0.225 | 0.885 | row added |
| GA | 4.6% | 0.254 | 0.929 | row added |
| NM | 4.1% | 0.259 | 0.840 | row added (2018+) |
| NE | 4.6% | 0.292 | 0.989 | row added |
| MO | 3.4% | 0.299 | 0.898 | row added |
| MT | 16.2% | 0.338 | 0.940 | row added |
| MS | 21.5% | 0.361 | 0.838 | row added |
| ME | 2.2% | 0.462 | 0.926 | row added |
| **OK** | 1.4% | **0.758** | 0.900 | **rejected** |
| **AZ** | 22.7% | **0.823** | 0.932 | **rejected** |

OK and AZ are rejected because most exposed records still match: exposure is not
the binding cause there, and excluding 22.7% of the AZ subset to buy 2.5 points
would be over-broad in exactly the way the UT row's materiality bound exists to
prevent. AZ's dominant stage remains exemptions; OK already carries the T13
itemized-cap row that covers most of its exposure.

Each row cites its own statutory route — LA's excess-federal-itemized deduction
(R.S. 47:293(3)), ME's Schedule 2 under 36 M.R.S. 5125, NE's
77-2716.01(2) "less state and local income taxes", MT's Worksheet A, and so on.
NM starts at 2018 because `item_allowed` is off in 2017, where the fed-taxable
start carries the deduction instead.

### The control, which is the part that matters

A decisive itemizer/non-itemizer split is *not* by itself evidence of this
mechanism — federal itemizers are higher-income and more complex, so many
unrelated divergences concentrate there. The control is the ten states whose
encoding gives the class **no route at all** (CT IL IN MA MI NJ OH PA RI WV).

- **The state-itemizing predicate fires on ZERO records in all ten.** That is
  what makes variant A mechanism-specific, and it is why 13 rows could be added
  by sweep rather than by thirteen separate digs.
- **The federal-itemizing predicate fires everywhere, and depresses match rates
  in states with no route:** MA 0.393, PA 0.444, NJ 0.466, MI 0.702, CT 0.795
  among federal itemizers. So a federal-itemizing split of that size occurs
  *without* the mechanism.

**Consequence: ND, SC and VT get no swept row.** Their measured exposed rates
(ND 0.793, SC 0.525, VT 0.268) sit inside the range the control reaches without
the mechanism, so the split cannot carry them. Each needs the CO/UT treatment —
name the statutory route (all three are federal-taxable-start; SC and VT also
carry a SALT addback) and probe it — which is per-state triage, not sweep work.

> **ND was probed the same day and the control is vindicated
> ([`nd_marriage_credit_probe.md`](nd_marriage_credit_probe.md)).** The exposure
> class has no route into North Dakota at all — TAXSIM never populates `v35`
> there, and federal itemizers are 1.3–1.5% of the clean subset, so excluding
> every one of them moves the 2019 cell from 0.929 only to 0.937. The real cause
> is the marriage penalty credit (N.D.C.C. 57-38-01.28), it is entirely ours, and
> encoding it takes all four cells to ~0.999. **Had the sweep trusted the split
> and added the row, it would have banked a 0.8pp exclusion and hidden a 7pp
> encoding gap behind it** — which is the concrete cost the control avoided.

---

## 2. T18 (care deduction without the IRC 21(d) limit) — 0 rows added, 2 rejected

Five states encode a care-expense deduction off the federal IRC 21 base. VA and
ID carry T18 rows; MD was probed and rejected on 2026-08-22. The remaining two
were probed here (`mawi_care_probe.R`, 2019, joint returns, two dependents,
$6,000 of care expenses, spouse earnings varied):

| state | both earn | spouse earns nothing | spouse earns \$2,000 | verdict |
|---|---|---|---|---|
| MA | no effect | no effect | no effect | **TAXSIM grants no MA care deduction at all** |
| WI | \$930.60 | **\$0.00** | \$210.20 | **TAXSIM applies the limit correctly** |

WI is the clean negative: the care effect vanishes when the spouse earns
nothing and shrinks when the spouse earns \$2,000, which is the 21(d)(1)
limitation working. MA is a different finding — TAXSIM's Massachusetts liability
is invariant to care expenses entirely (siitax 5,050.00 and implied deduction
22,000 in all four cases), so its Form 1 line 12 deduction is simply not
modelled. That is a coverage gap, not T18, and it points the other way (ours
deducts, TAXSIM does not).

**T18 does not generalize.** It holds in VA and ID and in neither of the other
three states that encode the same deduction.

---

## 3. P9 (PE denies per-dependent benefits at 18+) — 9 rows added, 7 unresolved

The existing P9 section of `external_model_issues.md` had flagged an age effect
in 23 jurisdictions while stating plainly that twenty of them had not been
checked against their own statutes, and that the count was "an upper bound on
the problem, not a claim." This is that check.

**Our side is flat in dependent age by construction, not by measurement.**
`src/calc/state/st_exempt.R` applies the dependent exemption as
`n_dep * st_dep_v`, and `params_schema.yaml` gives `st_exempt.dep_amount` no age
parameter at all — there is no mechanism by which it could vary with a
dependent's age. That is a stronger warrant than a numeric probe.

**Probed** against policyengine-us 1.775.7 on 2023 head-of-household returns
with one dependent, identical but for that dependent's age (17 vs 18), at two
income levels. A state qualifies only where the PE rise resolves **exactly** to
the encoded per-dependent amount times a published state rate at that income:

| state | PE rise at \$60k | encoded amount | implied rate | resolves to |
|---|---|---|---|---|
| SC | 295.04 | 4,610 | 6.400% | SC top rate |
| GA | 172.50 | 3,000 | 5.750% | GA flat rate |
| VT | 162.48 | 4,850 | 3.350% | VT first bracket |
| KS | 128.25 | 2,250 | 5.700% | KS top rate |
| MS | 75.00 | 1,500 | 5.000% | MS flat rate |
| NY | 55.00 | 1,000 | 5.500% | NY bracket at that income |
| MA | 50.00 | 1,000 | 5.000% | MA flat rate |
| LA | 18.50 | 1,000 | 1.850% | **LA BOTTOM bracket** — see below |
| AZ | 25.00 | 25 (credit) | — | the \$25 credit exactly |

**Louisiana is the strongest single confirmation in the sweep.** A \$60,000
Louisiana filer's marginal rate is 4.25%, so a deduction-style reading predicts
\$42.50. The measured rise is \$18.50 — the exemption at the *bottom* bracket,
1.85%. That is `st_ord.exempt_from_bottom` (R.S. 47:32(A)(1)/294/295(B))
reproduced from the outside, by a different model, on a quantity nobody was
testing for.

**Why \$30,000 is not the test.** At \$30,000 the deltas are large and
irregular (MA 1,108.52, NJ 1,084.77, MN 2,006.80) because a dependent turning 18
also stops being an EITC/CTC qualifying child under IRC 32(c)(3)/24, which
cascades into state piggyback credits. That is correct law, not a divergence, so
the \$60,000 level — above the EITC range — is the clean one.

### Unresolved, deliberately left without a row

| state | PE rise at \$60k | why it does not clear |
|---|---|---|
| MN | 288.56 | implies 6.012%, not an MN bracket rate; MN's 2023 child credit is phasing out at this income, so the delta is contaminated |
| NM | 200.00 | implies 8.00%; NM's top rate is 5.9% |
| OK | 72.50 | implies 7.25%; OK's top rate is 4.75% |
| NJ | 36.75 | implies 2.45%, not an NJ bracket rate |
| IN | 47.25 | resolves to the \$1,500 child add-on at 3.15%, and our add-on IS age-gated (`dep_child_max_age` 23) — a different question |
| ME | 300.00 | benefit is not `st_exempt.dep_amount`; likely the dependent exemption *credit*, needs its own read |
| IA | 40.00 | benefit is not `st_exempt.dep_amount`; unidentified |

MO (−3.72), OR (−43.75) and AR (−110.00) move the *other* way — PE treats the
older dependent more favourably — and are a separate open question.

---

## 4. What the confirming rerun showed

Both windows were re-run against the pre-sweep committed summary first, and
reproduced it exactly — 0 of 200 cells moved — so every delta below is
attributable to the rows and not to sampling.

### Crosswalk exposure: every one of the 13 moved, and nothing else did

2018–2020 gains, worst-to-best cell: AL +0.180/+0.181/+0.194, MS
+0.128/+0.124/+0.133, MT +0.118/+0.123/+0.129, AR +0.093/+0.090/+0.116, IA
+0.106/+0.109/+0.098, LA +0.044/+0.042/+0.040, NE +0.041/+0.040/+0.037, GA
+0.041/+0.039/+0.037, KY +0.039/+0.037/+0.034, NM +0.031/+0.029/+0.028, KS
+0.027/+0.029/+0.026, MO +0.025/+0.026/+0.024, ME +0.011/+0.012/+0.010.

2017 barely moves anywhere (exposure ~2–3.6% pre-TCJA against 4–28% after),
and two cells went **down** by ≤0.4pp on denominator composition — KY 2017
−0.004, MS 2017 −0.001 — the same effect recorded in the 2026-08-16 hardening
batch. **The regression check found no cell outside the 13 states that moved at
all**, which is the second half of the specificity claim: the predicate is
inert where the mechanism is absent.

**On exposed share, which is the over-breadth question.** AL removes 28.4% of
its clean subset and MS 27.2% — far above CO's 3.5%, which is why it had to be
checked rather than assumed. It is in line with precedent: the states that
already carried a row remove NC 26.2%, HI 25.0%, DE 20.7%, NY 20.6%, MN 20.2%,
WI 17.8%, CA 17.5%. CO and UT are the small ones (3.5–4.2%) precisely because
they are federal-itemizing-keyed and have no state itemized deduction, so their
exposed population is structurally small. An independent-election state under
the 2026-08-15 state-only-itemization fix has a large one by construction.

### P9: four rows hold, five were wrong as exclusions

The rerun **falsified five of the nine**. Arizona moved DOWN in all four cells
(−0.003 to −0.005) while dropping a tenth of its subset, and Louisiana moved
−0.005 to +0.016. The cause is arithmetic and should have been caught before the
run: the harness tolerance is \$100, and AZ's benefit is \$25, LA's \$18.50,
MA's \$50, NY's \$55, MS's \$75. **A single dependent aged 18 or over cannot
produce a \$100 mismatch in those states**, so an exclusion keyed on
`n_dep_ge18 > 0` removes records that were matching.

Those five were demoted to `annotate` (`output/probe/p9_fix_subhundred.R`),
following the standard set on 2026-08-11 — sub-\$100 bugs are annotated, not
excluded. The divergence is real and still documented; it just carries no
exclusion, and it does still bite on returns with two or more such dependents.

The four whose per-dependent amount exceeds the tolerance keep their exclusions,
and they are where the movement is:

| state | per dependent | cells cleared | movement |
|---|---|---|---|
| GA | 172.50 | 2 of 3 measurable | +0.084 / +0.082 / +0.057 |
| KS | 128.25 | 2 of 4 | +0.065 / +0.077 / +0.073 / +0.075 |
| VT | 162.48 | 2 of 4 | +0.058 / +0.081 / +0.081 / +0.077 |
| SC | 295.04 | — | +0.027 / +0.071 / +0.059 / +0.049 |

**The lesson worth carrying forward:** applicability and a decisive split are
not sufficient. A class also has to clear the tolerance the harness measures at,
and that is checkable from the parameter value alone, before any run.

---

## Revision history

- **2026-08-22** — written, then amended after the confirming rerun. Sweep
  executed: 22 rows added (13 crosswalk exposure, 9 P9), of which 5 P9 rows were
  demoted to `annotate` when the rerun showed a sub-tolerance benefit cannot
  support an exclusion. Net: 13 exposure exclusions + 4 P9 exclusions + 5 P9
  annotations. Rejected outright: OK and AZ (exposure not binding), ND/SC/VT
  (failed the control) for the exposure class; MA and WI for T18. Left
  unresolved for P9: MN, NM, OK, NJ, IN, ME, IA, and the opposite-direction
  MO/OR/AR.

---
title: "Federal-side divergences (for separate review)"
role: evidence
workstream: state_tax
status: current
updated: 2026-08-22
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Federal-side divergences (for separate review)

**Policy (JI, 2026-07-18):** the state cross-model harness documents federal
divergences here and then ignores them — the clean-subset metrics condition
them away so state-law validation is not polluted. They are NOT dismissed:
each item below is a real disagreement between our federal calculator and an
external model's, and someone should review whether any indicates a problem
in OUR federal calculation rather than theirs. This file is the handoff.

**Section 5 is the first item confirmed to be a defect on our side**, with a
statute cite and a one-line fix. It is not a "which model is right" question
like the rest of this file.

How the filter works: a record is "federally aligned" when the external
model's federal AGI is within $100 of ours, federal taxable income within
$100, federal EITC within $15, and (TAXSIM only) `exempt_int == 0`.

**Two corrections on 2026-08-22.** Federal taxable income is now compared on
the TAXSIM leg too; it had been PolicyEngine-only, and
`v18_federal_taxable_income` was available from TAXSIM all along but dropped by
the leg's `select`. That hole mattered most for states whose base IS federal
taxable income, which inherit every federal difference below the AGI line:
Idaho records carrying a section 199A deduction matched at 0.60 against 0.91
without, with the state taxable-income gap equal to -qbi_ded (median ratio
-1.0006). Correcting it lifted 160 of 200 TAXSIM cells. Separately, the
`state_ref` offset on the AGI comparison is now conditional on whether the
crosswalk actually withheld the refund -- it is handed to states that do not
subtract their own, and applying the offset there had been failing almost every
RI and ND record carrying a refund (fed_aligned 0.037 against 0.674). Flags are set
in `cross_model_compare()` (`src/tests/state/test_state_cross_model.R`); the
`fed_aligned` column is on every per-record file in `results/raw/`.

Scale: for IL (a state whose own law matches near-perfectly once federally
aligned), federal-side noise accounted for roughly 25–35 points of raw
match@$100 in both model windows.

## 1. Federal EITC disagreements (TAXSIM and PolicyEngine)

Both external models compute their own federal EITC and assume full take-up;
state EITCs piggyback at 18–20%+, scaling every federal difference into the
state comparison.

Two verified sub-patterns (IL 2019, TAXSIM):

- **Eligibility**: records where we deny EITC and TAXSIM grants it. The
  diagnosed mechanism is the investment-income test — `exempt_int` counts
  toward the limit and TAXSIM has no tax-exempt-interest input, so it cannot
  deny on that ground. Verified on record ids 220652, 210863 (2019 sample,
  seed 76): our `eitc = 0`, TAXSIM granted ~$240 federal → ~$43 IL EITC gap.
  **Review question**: confirm every such denial traces to investment income
  or another modeled test, not a bug in `calc_eitc()`.
- **Amounts**: records where both grant EITC but amounts differ, e.g. id
  296044: ours $403.92 vs TAXSIM $504.88 (gap ×18% → $18 IL state gap).
  Candidate causes: earned-income concept differences (SE income treatment),
  prior-year-EI election, AGI-vs-EI phase-out base. **Not yet root-caused.**

## 2. Residual federal AGI tail vs TAXSIM (~8% of records)

After the 2026-07-18 crosswalk repair (see commit 78bcbf143), the TAXSIM
federal AGI gap is $0 at the median but the p99 is ~+$13.8k (TAXSIM higher)
on 2019 data. Untraced candidates: taxable-SS computation differences
(compare `v12_soc_sec_agi` vs `txbl_ss`), capital-loss limitation
(`txbl_kg` vs TAXSIM's own stcg/ltcg netting), SECA-deduction interplay on
mixed wage/SE records. Raw material: any `results/raw/taxsim_{year}.csv`,
records with `fed_aligned == FALSE` and `exempt_int == 0`.

## 3. QBI in the TAXSIM crosswalk — CLOSED 2026-07-19 (but see the note below)

`taxsim_crosswalk()` now maps QBI inputs (SE income to `pbusinc`/`pprofinc`
by SSTB share, non-SE QBI income to `scorp`, totals preserved; see
`src/tests/test_taxsim.R`). The 2017–2020 rerun moved the
federal-taxable-start states' clean match rates by +3 to +5 points in
2018–2020 (2017 unchanged, as expected pre-TCJA). Remaining approximation:
TAXSIM assumes a sufficient wage bill, so its QBID can exceed ours above
the phase-out for low-wage-bill businesses.

**2026-08-22 addendum.** The residual QBI divergence is larger than "remaining
approximation" suggested, and it was invisible because the TAXSIM leg did not
compare federal taxable income. On Idaho, whose base is federal taxable income,
records with a QBI deduction matched at 0.60 against 0.91 without, and the
state taxable-income gap equalled -qbi_ded almost exactly (median ratio
-1.0006) -- i.e. for the mismatching subset TAXSIM takes no QBID where we take
one. These records are now conditioned out by the corrected filter rather than
polluting state cells, but the underlying federal difference is unexplained and
belongs on this list. Reviewing it means asking why TAXSIM's QBID is zero for
that subset -- the wage/SSTB limit is the obvious candidate, and the direction
is opposite to the wage-bill approximation noted above.

## 4. PolicyEngine's federal return

PE recomputes the entire federal return from raw-ish inputs, so any
modeling difference lands here: its own taxable-SS, QBID, standard/itemized
choice, and full-take-up credits. The driver now exports `pe_fed_agi`,
`pe_fed_taxable`, `pe_fed_eitc` per record for exactly this review. Also
note: PE imputes the Alaska Permanent Fund Dividend into AK households'
federal AGI (verified +$2,622/record constant in 2022), so AK records are
never federally aligned — benign for state validation (AK liability is
0 = 0) but relevant if these outputs are reused.

## 5. MFS filers get the unmarried aged/blind standard deduction — OUR BUG

**Confirmed defect in our federal calculator.** Found 2026-08-21 while
verifying the PolicyEngine filing-status fix (commit 8991c75aa); unrelated to
state law, and it would not have surfaced without forcing PE onto our filing
statuses.

`config/scenarios/tax_law/baseline/std.yaml` maps the aged/blind bonus by
filing status:

```yaml
  bonus:
    '1': bonus_single
    '2': bonus_married
    '3': bonus_single      # <-- married filing separately
    '4': bonus_single
```

Status 3 is married filing separately, and it is drawing `bonus_single`.
IRC 63(f) sets the additional amount at $600 (1987 dollars) and 63(f)(3)
substitutes $750 only "in the case of an individual who is not married and is
not a surviving spouse". An MFS filer **is** married, so status 3 belongs on
`bonus_married`. Status 4 (head of household) is unmarried and is correct as
written. Indexed, the two amounts are $1,700 and $1,350 in 2021
(Rev. Proc. 2020-45), so each affected bonus instance overstates the
deduction by $350.

The mapper carries no time dimension and both amounts index from 1987, so
this is wrong in **every** simulated year, not just the cross-model window.

**Live path:** `std.bonus` is consumed at
`src/calc/functions/deductions/std_ded.R:54`, `bonus_value = std.bonus *
n_bonuses`, where `n_bonuses` counts aged-65+ and blind instances across both
filers. A single MFS filer aged 65+ therefore gets one $1,700 bonus instead of
$1,350; a blind filer who is also 65+ gets two.

**Evidence.** CA 2021, old vs new PE driver over identical records, joined on
`id` (job 23084507). To regenerate: run `run_cross_model.R --states CA
--years 2021:2021 --models policyengine` twice, once with the driver that
predates any filing-status assignment (`git show
8991c75aa~2:src/tests/state/cross_model/pe_state_tax.py` -- note `~2`, not
`~1`: 8991c75aa~1 is the withdrawn MFS-only version) and once with the
current one, keeping `results/raw/policyengine_2021.csv`
from each. Note that file is a whole-file overwrite, so back up the 51-state
version first. Twelve MFS records aged
65+ moved `pe_fed_taxable` by exactly +$350 once PE was told they were
SEPARATE rather than left to derive SINGLE, while their federal AGI was
unchanged and equal to ours to the dollar. Seven sat in the clean subset and
flipped `fed_aligned` TRUE to FALSE; the other five were already excluded.
Worked case, id 121955 (age 76, AGI $186,665):

| | std deduction | AGI - taxable |
|---|---|---|
| PE derived SINGLE (old), matches ours | 12,550 + 1,700 | 14,250 |
| PE told SEPARATE (new), correct | 12,550 + 1,350 | 13,900 |

Our federal AGI matches PE's exactly on all twelve, and only taxable income
moved, so the alignment flip isolates the deduction. That our value tracked
the *old* column is what identifies the bug as ours: the harness had been
agreeing with PE only because PE was making the same mistake for a different
reason.

Ids (all CA 2021, age1 >= 65, no secondary filer): 52557, 86017, 121955,
146605, 163124, 169816, 169919, 194024, 318241, 322107, 374389, 435663.

**Fix.** One line — `'3': bonus_married` in the `bonus` mapper. Deliberately
NOT applied here: it moves federal taxable income for every MFS aged/blind
record in every year, so it belongs in a federal change with its own revenue
check, not inside a state-triage commit.

**Downstream.** Any state starting from federal taxable income, or conforming
to the federal standard deduction, inherits the error — see
`config/scenarios/tax_law_state/conformity_groups.yaml`. State-side impact is
second order next to the federal one.

**Scale.** In the cross-model sample MFS is deliberately oversampled at 26.6%
of records, of which 93 of 408 are 65+ (CA 2021), so the ~6%-of-records figure
there is a property of the sample and **not** a population estimate. MFS is a
low-single-digit share of real returns and aged MFS a fraction of that; the
per-return error is $350 per bonus instance of taxable income. Someone should
size it against production weights before deciding urgency.

**Not explained by this defect.** Three further records flipped `fed_aligned`
in the same probe with deltas that are not $350: MFS ids 136854 (age 32,
+$499) and 218808 (age 48, +$643), and HoH id 141486 (age 61, +$650). None is
aged or has a secondary filer, so the aged/blind bonus cannot be the mechanism;
all three are `excluded` and none touches the clean subset. Single-vs-separate
differences that could plausibly produce non-round deltas at these ages
include the student-loan-interest deduction (MFS ineligible) and IRA-deduction
phase-out bases. Untraced — left open rather than guessed at.


## Operational notes for the reviewer

- Per-record raw files are overwritten per year by the most recent run's
  state set — regenerate with `research/state_tax/cross_model/run_cross_model.R` if a state you need is
  missing from `results/raw/`.
- Federal pre-pass caches: `cache/fed_calc_{year}.rds` (sample seed 76,
  dependent filers excluded — TAXSIM mstat-8 semantics differ).

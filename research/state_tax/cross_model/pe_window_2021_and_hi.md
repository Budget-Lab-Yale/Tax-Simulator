---
title: "PolicyEngine window — the 2021 class and the Hawaii SALT defect"
role: evidence
workstream: state_tax
status: current
updated: 2026-08-23
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# The PolicyEngine window, 2026-08-23

Entered from the board rather than from a state: almost every catastrophic cell
in the set was a **PolicyEngine 2021** cell (HI 0.058, ME 0.143, MA 0.168,
MT 0.300, SC 0.491), and 2021 was systematically the weakest PE year — median
clean match 0.887 against 0.923–0.934 for 2022–2024, q25 0.750 against
0.826–0.849, five cells below 0.60 against nought or one elsewhere. Its clean
subset was also ~20% smaller, which said our FEDERAL calculation diverges from
PE's more often in that year.

Two entirely separate causes came out of it. Scripts in `output/probe/`:
`hi_pe_synthetic.R`, `hi_pe_bracket_probe.R`, `hi_pe_rate_pin.R`,
`pe2021_class_dig.R`, `pe2021_add_rows.R`, `pe2021_amend_rows.R`.

---

## 1. Hawaii: PolicyEngine deducts Hawaii's own income tax (PE-side bug)

**The whole HI PE window, not just 2021.** Dumping PolicyEngine's own Hawaii
intermediates for a pure-wage single filer with no deductible items at all,
2022, \$501,000 of wages:

```
hi_salt_deduction         49,246.60
hi_withheld_income_tax    49,246.60   <- identical to the cent
hi_itemized_deductions    39,220.60   (after Hawaii's own limitation)
hi_standard_deduction      2,200.00
```

Hawaii allows state and local income taxes as an itemized deduction **only
where federal AGI is under \$100,000 single / \$150,000 head of household /
\$200,000 joint** (Worksheet A-2 note, permanent for taxable years after 2010 —
the threshold our `st_ded.salt_addback_agi_thresh` encodes from the booklet).
PolicyEngine runs straight through that cliff with no discontinuity at all:

| federal AGI | PE's HI SALT deduction |
|---|---|
| 95,000 | 6,909.60 |
| 99,000 | 7,239.60 |
| **101,000** | **7,404.60** |
| 105,000 | 7,734.60 |
| 150,000 | 11,447.10 |

**The arithmetic closes exactly.** Each extra \$1,000 of wages raises PE's
withholding, which raises the deduction, so HI taxable income rises only \$920 —
and 0.92 × Hawaii's statutory 11% top rate is **0.1012**, which is the effective
marginal rate measured at \$500k, \$600k and \$1,001k, in both 2019 and 2022.
PE's rate *parameters* are the correct HRS 235-51 ladder; the base is what is
wrong, so the error grows without bound in income.

**The record-level split is as clean as this work gets:**

| year | above the thresholds | below the thresholds |
|---|---|---|
| 2022 | match 0.026, median diff **+3,051** | match 0.786, median diff **0** |
| 2023 | match 0.022, median diff **+3,384** | match 0.767, median diff **0** |
| 2024 | match 0.021, median diff **+2,964** | match 0.878, median diff **0** |

Below the thresholds we agree with PolicyEngine *to the dollar*. Excluded on the
above-threshold population only, keyed on the statutory AGI tests by filing
status.

**It is Hawaii-specific.** Probing the same pure-wage filer in ten states, MT
and NY report a zero `salt_deduction` and ME/MA/SC/CA/VT/AR/IA do not expose the
variable at all. Only Hawaii has `salt_deduction == withheld_income_tax`.

**Worth filing upstream.** It understates Hawaii's top marginal rate by 0.88
points for every high-income filer, which is larger in effect than most of the
P-series already in `external_model_issues.md`.

---

## 2. The 2021 collapse is the P5 rebate class, in five unfiled states

**The UI hypothesis was tested first and refuted.** ARPA's \$10,200
unemployment exclusion was the obvious candidate, but in the collapse states
non-UI records missed just as badly as UI records — MT 0.000 with UI against
0.327 without, ME 0.200 with against 0.138 without — and the large positive
median differences were present on non-UI records too (HI +325, MA +568,
ME +850, MT +762).

The cause is the documented P5 class: rebates paid in a LATER year that
PolicyEngine nets into the eligibility year. Probed at \$80,000 single:

| state | 2021 amount | mechanism |
|---|---|---|
| HI | 300.00 refundable | Act 115 (2022) constitutional tax refund |
| ME | 850.00 refundable | \$850 relief payment, LD 1995 (2022) |
| MA | 516.35 refundable | Chapter 62F taxpayer refund — **proportional**, 14.0312% of TY2021 liability |
| MT | 1,250.00 nonrefundable | HB 192 (2023) individual income tax rebate |
| SC | 800.00 nonrefundable | 2022 income tax rebate, capped at 800 |

Every one behaves like the NM trio already in the accept-list: the variable
computes a nonzero value in **every** year but is inside that state's credit
total only in 2021 (verified per state), so each predicate is year-scoped rather
than trusting the column alone. Six diagnostic exports were added to
`pe_state_tax.py` following the existing `pe_*_rebate` pattern; the harness
needed no change, since the PE leg returns the driver's output wholesale.

### These are DROPPED cells, not passes — and two are not solved

The rebates went to nearly every filer, so `rebate > 0` excludes nearly the
whole cell. This is the IL-2021 / CO-TABOR situation and is recorded as such in
each row, because the resulting match rates are otherwise badly misleading:

| state | 2021 before → after | share of the cell excluded | read |
|---|---|---|---|
| HI | 0.058 → 1.000 | **95.6%** (16 records left) | dropped cell; not a pass |
| ME | 0.143 → 0.881 | 83.8% | effectively dropped |
| MT | 0.300 → **0.287** | 68.3% | **moved DOWN — see below** |
| MA | 0.168 → 0.408 | 65.7% | rebate is only part of it |
| SC | 0.491 → 1.000 | 52.1% | least over-broad of the five |

**Montana went the wrong way.** Excluding 68% of the cell moved it 0.300 →
0.287, which means the excluded records were matching *better* than the
remainder: MT's 2021 residual is dominated by something else entirely. The
rebate divergence is real and far above tolerance (\$1,250), so the row stands
as documentation, but it does not rescue the cell. **MT 2021 is still open.**

**Massachusetts is only partly explained.** Excluding 66% lifts it to 0.408,
so a second 2021 cause remains unidentified — and MA's other PE years are weak
too (0.634 / 0.810 / 0.835). **Open.**

---

## What this did and did not achieve

- HI 2022–2024 moved +0.137 / +0.142 / +0.180, landing on 0.786 / 0.767 / 0.878
  — exactly the below-threshold rates predicted before the run.
- PE cells below 0.60 went from **five to two**; the 2021 median rose 0.887 →
  0.895 (small, because the class is concentrated in a few states).
- **No cell outside the five states moved**, which is the specificity check.
- **No state closed.** Hawaii's verdict now rests on 2022–2024 at 0.767–0.878,
  much better understood but still short of the bar; its remaining wedge is most
  likely the pension source split its TAXSIM triage already identified as a
  Tier 1 data limitation.

## Revision history

- **2026-08-23** — written. Two causes diagnosed and landed: the Hawaii circular
  SALT deduction (PE-side, whole window, above-threshold population) and five
  unfiled P5 rebate instances (TY2021). MT and MA 2021 remain open with a second
  cause each; no state closed.

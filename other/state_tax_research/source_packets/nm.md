# New Mexico State Source Packet

State: `NM`
Status: `ENCODED 2026-08-13 (baseline/nm/, tests NM-1..NM-6b); cross-model not yet run`
Last updated: `2026-08-13`

> Two design decisions in this state are load-bearing and were resolved with
> worked evidence (§Decisions). One found a trap that would have cost **$304 of
> tax on a single test return** — read §Decisions before encoding.

## Scope

- TY2017-2025. Resident individual income tax only (PIT-1; no PIT-B allocation).
- Major features: federal-AGI base with the FEDERAL standard/itemized deduction
  subtracted ON THE RETURN (PIT-1 line 12) rather than inherited via a
  federal-taxable-income start — NM allows NEITHER the federal QBI deduction NOR
  the 2025 OBBBA below-the-line deductions; pro-rata addback of the federal
  income/sales-tax SALT deduction; a $2,500-per-exemption low- and middle-income
  exemption; a $4,000 per-dependent-after-the-first deduction (2019+, MFJ/HoH
  only); an AGI-banded $8,000 65-or-older exemption; three rate regimes
  (2017-2020 four brackets, 2021-2024 five, 2025 six under HB 252); a refundable
  EITC match (Working Families Tax Credit) at 10/17/20/25% by vintage; and a
  refundable seven-tier per-child credit from 2023.

## Primary sources

- NBER mirror `taxsim.nber.org/historical_state_tax_forms/NM/{YEAR}/`, complete
  1996-2025. Used: 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025
  (PIT-1, PIT-1 instructions, PIT-ADJ instructions, PIT-RC instructions,
  PITbook, TRT).
- **TRD statutory rate compilation** "PERSONAL INCOME TAX RATES" (2005-2025),
  in the 2025 directory — reproduces NMSA 7-2-7 verbatim per regime with the
  enacting laws and the contingent-effective-date history.
- Statutes: 7-2-2 (rolling IRC conformity), 7-2-7 (rates), 7-2-5.8 (low/middle
  income exemption), 7-2-39 (dependent deduction), 7-2-5.2 (aged/blind),
  7-2-5.14 (SS, HB 163 of 2022), 7-2-34 (net capital gains), 7-2-18.15 (WFTC),
  7-2-18.34 (child credit), 7-2-37 (medical care expense deduction, sunset
  12/31/2024), 7-2-14 (LICTR), 7-2-14.3 (property tax rebate), 7-2-18.1 (child
  day care credit).
- **Only the child income tax credit is indexed** (7-2-18.34(D)). Rates,
  brackets, the LMI exemption, the aged/blind table, the dependent deduction
  and the SS thresholds are all FIXED in statute.
- Gap: no TY2017 PIT-1 *instruction* booklet on the mirror (the 2017 form and
  PIT-ADJ instructions are present). TY2017 instruction-only items (the WFTC
  percentage, the LMI worksheet) are bracketed by the 2016 and 2018 booklets,
  both identical.

## Verified value tables

### Rate regimes (same rate sequence across statuses; brackets differ)

- **2017-2020:** `1.7 / 3.2 / 4.7 / 4.9%`
- **2021-2024:** adds a `5.9%` top bracket (Laws 2019 ch.270 s.12; contingent
  effective date — DFA certified 18 Dec 2020 that FY20 recurring general fund
  revenue was less than 5% above FY19, triggering it for tax years beginning on
  or after 1 Jan 2021)
- **2025+:** six brackets `1.5 / 3.2 / 4.3 / 4.7 / 4.9 / 5.9%` (HB 252 = Laws
  2024 ch.67 s.5)

### Bracket thresholds

Single: 2017 `0/5,500/11,000/16,000`; 2021 `0/5,500/11,000/16,000/210,000`;
2025 `0/5,500/16,500/33,500/66,500/210,000`.
MFJ/QSS **and HoH** (statutorily grouped, 7-2-7(A)/(B)): 2017
`0/8,000/16,000/24,000`; 2021 `0/8,000/16,000/24,000/315,000`; 2025
`0/8,000/25,000/50,000/100,000/315,000`.
MFS (own schedule, not joint/2): 2017 `0/4,000/8,000/12,000`; 2021
`0/4,000/8,000/12,000/157,500`; 2025 `0/4,000/12,500/25,000/50,000/157,500`.

### Low- and middle-income exemption (7-2-5.8) — per exemption

`amount = $2,500 - rate x max(0, federal AGI - threshold)`:

| Status | rate | threshold | AGI limit |
|---|---|---|---|
| single | 0.15 | $20,000 | $36,667 |
| MFJ / QSS / HoH | 0.10 | $30,000 | $55,000 |
| MFS | 0.20 | $15,000 | $27,500 |

The eligibility LIMIT needs no separate encoding: the slope reaches zero
exactly at the limit in every status (`0.15 x 16,667 = 2,500.05`;
`0.10 x 25,000 = 2,500`; `0.20 x 12,500 = 2,500`). Encode the per-exemption
reduction as a SHARE per $1 step (`po_share_per_step = rate / 2,500` =
`0.00006` / `0.00004` / `0.00008`), because the calculator's flat
`po_reduction_per_step` is per RETURN, not per exemption. **If the $2,500
amount ever changes, all three constants must be recomputed.**

### Social Security (7-2-5.14, TY2022+) — hard CLIFF, no phase-out

Full exemption of federally taxable SS at/below federal AGI `$100,000` single /
`$150,000` MFJ+HoH+QSS / `$75,000` MFS.

### Net capital gains deduction (7-2-34)

Greater of 100% of gain up to a dollar floor, or a share:
2017-2018 **50%**; 2019-2024 **40%**; 2025 `$2,500` flat with the 40% surviving
only for sales of a New Mexico business (unobservable → share 0). The
`$1,000`/`$2,500` floors have no machinery (<= $2,500 of base ~ $118).

### Aged exemption (7-2-5.2) — $8,000 per taxpayer 65+, 9 AGI bands

The band table is bit-identical in the 2017, 2019 and 2025 booklets. The
calculator's aged deduction reduces the POOLED amount $1 per $1 over a
threshold, so thresholds are anchored so the pool reaches zero at the table's
top bound: single `20,500` (zero above 28,500), MFJ `35,000` (two 65+ spouses,
$16,000 pooled, zero above 51,000), MFS `17,500`, HoH `43,000`.
**Documented approximation**, max error ~$1,500 of base (single) / $3,000
(joint) inside the band, zero outside $18,000-$51,000 of AGI.

### Credits

WFTC (EITC match, refundable): **10%** (2017-2018), **17%** (2019-2020),
**20%** (2021-2022), **25%** (2023+). Child income tax credit (TY2023+,
refundable, per qualifying child), published SEVEN AGI tiers:

| AGI band | 2023 | 2024 | 2025 |
|---|---|---|---|
| 0-25,000 | 600 | 622 | 637 |
| 25,001-50,000 | 400 | 414 | 424 |
| 50,001-75,000 | 200 | 207 | 212 |
| 75,001-100,000 | 100 | 103 | 106 |
| 100,001-200,000 | 75 | 77 | 79 |
| 200,001-350,000 | 50 | 51 | 53 |
| over 350,000 | 25 | 25 | 26 |

## Decisions (both resolved with worked evidence)

### 1. Base construction — `start_point 2` for 2017, `start_point 1` for 2018+

PIT-1 line 17 = FAGI + SALT addback + PIT-ADJ additions − **federal Form 1040
line 12** − dependent deduction − LMI exemption − PIT-ADJ subtractions
(− medical deduction through 2024).

- **2017 is exact with `start_point 2`**: PIT-1 subtracts both the federal
  deduction (line 12) AND the federal personal exemption post-PEP (line 13) =
  exactly federal taxable income, and §199A did not yet exist.
- **2018+ must use `start_point 1`** with the federal deduction rebuilt in
  `ded.yaml`. Line 12 is the standard/itemized deduction ONLY: line 13 (QBI)
  and, from 2025, the OBBBA below-the-line deductions (senior, tips, overtime,
  auto-loan interest) reduce federal TAXABLE income but **not** NM's base.
  **A federal-taxable-income start would silently grant all of them.** Test
  NM-3 quantifies it: an MFJ couple both 70 would have had NM taxable income of
  $3,300 and tax $49.50 instead of $15,300 and $353.60 — **a $304 (86%)
  understatement on one return**, and the QBI leg is worth ~$980 for a filer
  with $100k of QBI, across every pass-through owner in the state.
- **Cost, stated plainly:** `std_amount` duplicates a federal computation, so a
  federal STANDARD-deduction reform will not flow into NM (itemized reforms
  will, since components are calculated). Mitigations: mirror `std.yaml`
  parameter-for-parameter INCLUDING indexation so out-years track exactly, and
  add a test asserting `nm st_std_ded == federal std_ded` across year x status
  x aged x blind x dependent-filer. **That test is a required follow-up.**

### 2. The $4,000 dependent deduction (7-2-39) — encode ZERO and document

`$4,000` x (total dependents − 1), TY2019+, MFJ/HoH only. `st_exempt.dep_amount`
already carries the $2,500 LMI exemption and applies to EVERY dependent;
`st_child_ded` is the only other per-dependent slot and has **no count offset**.
Every config-only mapping was wrong by a full $4,000 of base per eligible
return in the same direction — larger than the provision's own average value —
so zero plus documentation was the smaller error at the time.

**RESOLVED 2026-08-12: `st_child_ded.count_offset` (default 0, NM 1) was added
and tested (test MACH-5), applied as `pmax(0, n_qual - count_offset)`.** Encode
the provision live via `child_ded.yaml` with `style` 1 from TY2019,
`amounts` 4,000, `agi_bounds` Inf, and `count_offset` 1 — the "encode ZERO and
document" decision below is superseded, and the ~$196-per-extra-dependent
overstatement ($128 in test NM-2) no longer applies. The one remaining caveat
is that `st_child_ded` has no filing-status gate, so NM's MFJ/HoH-only
restriction still needs a documented note (single and MFS filers would receive
the deduction they are denied).

### 3. Child credit — 3 tiers encodable, 7 published

`st_credits_child.R` hard-codes three tiers (`pick_tier(..., 1:3)`, three-branch
`co_tier`), though the schema's family pattern `^st_credits\.ctc_tier\d+_bound$`
already admits any n. Encoding the top three tiers leaves filers above $75,000
of AGI at $0 instead of $25-$106 per child (~15-20% of the credit's cost).
**Do NOT ship a 7-tier YAML before generalizing the calculator** — the name
validator would accept `ctc_tier4_bound` and the calculator would silently
ignore it, exactly the inert-configuration failure the validator exists to
prevent. Generalization is ~6 lines (`st_family_matrix` over discovered
elements + the existing `st_band_index_upper` helper).

`credit_tables.csv` was considered and REJECTED for both the child credit and
LICTR: `lookup_state_credit_table` is called only for four credit_ids, and
routing the child credit through `independent_earned_income` would impose an
earned-income eligibility gate NM's credit does not have, mislabel the result
`st_earned_credit`, and key the band on earned income rather than AGI.

## Worksheet tests drafted (hand-verified)

- NM-1 TY2019 single $28,000: four-bracket schedule + LMI slope
  (`2,500 - 0.15 x 8,000 = 1,300`) → taxable $14,500 → **$434.00**.
- NM-2 TY2023 MFJ two children $45,000: dependent deduction (n−1 rule), LMI at
  4 exemptions, tier-2 child credit ($800), 25% WFTC ($762.27) → form-true
  **−$1,384.67**; as encoded (dependent deduction at zero) **−$1,256.67**,
  documenting the $128.00 gap.
- NM-3 TY2025 MFJ both 70, FAGI $72,000: HB 252 six-bracket schedule, SS
  exemption cliff (72,000 <= 150,000 → exempt $22,000), aged exemption at zero
  (AGI above the band), federal aged std add-ons allowed, **OBBBA senior
  deduction correctly NOT allowed** → **$353.60**. This is the case that
  justifies Decision 1.

## Known differences

Ours: the duplicated federal standard deduction (Decision 1); SALT addback
pro-rating (the form pro-rates capped SALT between property and income taxes,
we let property fill the cap first — exact whenever total SALT is under the
cap, i.e. most post-2018 itemizers); the linearized aged-exemption phase-out;
the omitted $4,000 dependent deduction; child credit tiers 4-7; **LICTR not
modeled** (up to $819 refundable, keyed to a 25-band MODIFIED GROSS INCOME
table crossed with exemption count — MGI adds TANF/SSI/general assistance/child
support/gifts/inheritances/VA benefits/scholarships and forbids loss netting,
none observable; **expect this to dominate the PE comparison for low-income NM
records**); the medical care expense DEDUCTION (25/15/10% of unreimbursed
expenses, 2017-2024, base unobservable); property tax rebate, child day care
credit, $2,800 65+ medical credit, blind branch of 7-2-5.2, armed-forces
retirement exemption (2022+ $10k/$20k/$30k), 100-year exemption; own-state muni
75% convention; US-obligation interest flagged not taken; MFS halving of the
child credit; capital-gain floors; the 2021/2022/2023 one-time rebates
(mailed checks — P5 class, PE books them, pre-register).

## PolicyEngine disagreements (primary wins in all three; report upstream)

1. **PE applies the 5.9% top bracket from 2008.** Its `main/*.yaml` sets the
   210,000/315,000/157,500 threshold at rate 0.059 effective `2008-01-01`. The
   TRD statutory compilation is explicit that this schedule applies only to tax
   years beginning on or after 1 Jan 2021 (contingent effective date certified
   18 Dec 2020). **PE overstates NM tax by 1.0 pp above those thresholds for
   TY2017-2020.** This is the reason to probe TAXSIM on 2017-2020 first.
2. **PE omits the 2017-2018 50% capital gain exclusion** (its series starts at
   40% in 2019); both the 2017 and 2018 PIT-ADJ instructions read "50%".
3. **PE omits the low- and middle-income exemption before 2021** (all its
   parameters start 2021-01-01); the worksheet is present and numerically
   identical in the 2016, 2018 and 2019 booklets, and 7-2-5.8 is unindexed and
   unamended. **PE overstates NM tax for filers under the AGI limits in
   TY2017-2020** — up to $2,500 x exemptions of base.

Minor: PE's child-credit third threshold is `75_001` where the band is
"50,001-75,000" (a $1 artifact); its inert 66,500/100,000/50,000 bracket splits
for 2008-2024 are harmless (same rate both sides).

## Uncertainties

- TY2017 WFTC = 10% is **bracketed, not directly sourced** (no TY2017
  instruction booklet on the mirror; the 2016 and 2018 booklets both say 10%
  with no intervening amendment to 7-2-18.15). Same reasoning for the TY2017 LMI
  values. Pull the TY2017 booklet from TRD's own prior-year page to close it.
- The joint aged-exemption threshold `$35,000` is a **modal choice**, not a form
  value: it anchors the pooled $16,000 at the table's $51,000 zero point for a
  two-65+ couple. A one-65+ couple is understated by up to $3,000 of base. If
  the NM 65+ married population is materially one-spouse-65, re-anchor to
  $43,000 (which then overstates two-65+ couples by up to $8,000 of base —
  worse on the modal case).
- `ctc_max_child_age = 23` proxies "under 19, or a student under 24" (IN
  precedent). If the harness shows a positive child-credit wedge for families
  with 19-23-year-old dependents, drop to 18.
- Three tracked dependent-age slots cap the child credit at three children.
- `item_component_style 1` completeness for 2018+ has not been record-tested
  against a filled NM return; spot-check on an itemizing record before the
  harness run.
- 2026+ unencoded; the child-credit amounts index annually and need each year's
  PIT-RC Table 4.

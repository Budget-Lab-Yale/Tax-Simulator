# Ohio State Source Packet

State: `OH`
Status: `baseline encoded; record-level worksheet tests complete`
Last updated: `2026-08-11`

## Scope

- Tax years covered: 2017-2035. Published IT 1040 values transcribed through 2025;
  the enacted 2026 flat 2.75 percent schedule (HB 96) carries forward.
- Resident individual income tax only. School-district income taxes, municipal
  income taxes, and nonresident allocation are out of scope (Phase 7 locality work).
- Major features: federal-AGI base, full Social Security subtraction, NO standard
  or itemized deductions, income-tiered personal/dependent exemptions, a rate
  schedule with statutory base-amount cliffs at the zero-bracket top, the business
  income deduction (BID) with a flat 3 percent tax on excess business income, and
  a nonrefundable credit stack (retirement, senior, CDCTC match, $20 exemption
  credit, joint filing credit, EITC) ordered by ORC 5747.98.

## Primary sources

- IT 1040 instruction booklets 2017-2025, Ohio Dept. of Taxation:
  `https://dam.assets.ohio.gov/image/upload/tax.ohio.gov/forms/ohio_individual/individual/{year}/...`
  (2017 `pit_it1040_booklet.pdf` ... 2025 `it1040-booklet.pdf`).
- ORC: [5747.02](https://codes.ohio.gov/ohio-revised-code/section-5747.02) rates;
  [5747.025](https://codes.ohio.gov/ohio-revised-code/section-5747.025) exemptions;
  [5747.022](https://codes.ohio.gov/ohio-revised-code/section-5747.022) $20 credit;
  [5747.05](https://codes.ohio.gov/ohio-revised-code/section-5747.05)(E) joint filing credit;
  [5747.055](https://codes.ohio.gov/ohio-revised-code/section-5747.055) retirement/senior;
  [5747.054](https://codes.ohio.gov/ohio-revised-code/section-5747.054) CDCTC;
  [5747.71](https://codes.ohio.gov/ohio-revised-code/section-5747.71) EITC;
  [5747.98](https://codes.ohio.gov/ohio-revised-code/section-5747.98) credit ordering.
- LSC as-enacted/enrolled bill analyses: HB 166 (2019), HB 110 (2021), HB 33 (2023),
  HB 96 (2025) — indexing suspensions and reform mechanics.

## Structure (IT 1040, stable 2017-2025)

FAGI (line 1) ± Schedule of Adjustments → OAGI (line 3) − exemptions (line 4) →
Ohio income tax base (line 5) − taxable business income (line 6, from IT BUS) →
taxable NONBUSINESS income (line 7) → schedule tax (8a) + flat 3% business tax
(8b) → nonrefundable credits (Schedule of Credits) → refundables. Ohio has NO
standard or itemized deduction.

**Modified adjusted gross income (MAGI, TY2019+, ORC 5747.01(JJ)) = OAGI + BID
claimed.** From 2019 the means tests for exemption tiers, the $20 credit, JFC,
retirement/senior credits, and CDCTC use MAGI (or MAGI less exemptions); 2017-2018
they use OAGI or the tax base (line 5).

## Rate schedules (taxable nonbusiness income; booklet + ORC verified)

Schedule has statutory BASE AMOUNTS: tax = base_j + rate_j × (TI − bracket_j),
with $0 at or below the zero-bracket top — a CLIFF at the boundary (2019+ the
first taxed dollar owes the full base, e.g. $310.47 in 2019). 2025 is internally
discontinuous at $100,000 ($2,375.63 continuation vs $2,394.32 statutory base —
$18.69 jump; model as written). Encode base amounts explicitly (st_ord.base_amounts
family), not as a smooth marginal schedule.

- 2017: 0 ≤ $10,650; then (bracket, base, rate): (10,650, 79.08, 1.980%),
  (16,000, 185.01, 2.476%), (21,350, 317.48, 2.969%), (42,650, 949.88, 3.465%),
  (85,300, 2,427.70, 3.960%), (106,650, 3,273.16, 4.597%), (213,350, 8,178.16, 4.997%).
- 2018 (indexed): 0 ≤ $10,850; (10,850, 80.56, 1.980%), (16,300, 188.47, 2.476%),
  (21,750, 323.41, 2.969%), (43,450, 967.68, 3.465%), (86,900, 2,473.22, 3.960%),
  (108,700, 3,336.50, 4.597%), (217,400, 8,333.44, 4.997%).
- 2019 (HB 166: bottom three brackets merged to 0%; rates −4%; brackets frozen):
  0 ≤ $21,750; (21,750, 310.47, 2.850%), (43,450, 928.92, 3.326%),
  (86,900, 2,374.07, 3.802%), (108,700, 3,202.91, 4.413%), (217,400, 7,999.84, 4.797%).
- 2020 (indexed): 0 ≤ $22,150; (22,150, 316.18, 2.850%), (44,250, 946.03, 3.326%),
  (88,450, 2,416.12, 3.802%), (110,650, 3,260.16, 4.413%), (221,300, 8,143.14, 4.797%).
- 2021 (HB 110: top bracket eliminated → 3.99%; zero-bracket to $25,000; indexing
  suspended): 0 ≤ $25,000; (25,000, 346.16, 2.765%), (44,250, 878.42, 3.226%),
  (88,450, 2,304.31, 3.688%), (110,650, 3,123.05, 3.990%).
- 2022 (indexed): 0 ≤ $26,050; (26,050, 360.69, 2.765%), (46,100, 915.07, 3.226%),
  (92,150, 2,400.64, 3.688%), (115,300, 3,254.41, 3.990%).
- 2023 (HB 33 yr 1): 0 ≤ $26,050; (26,050, 360.69, 2.750%), (100,000, 2,394.32, 3.688%),
  (115,300, 2,958.58, 3.750%).
- 2024 (HB 33 yr 2): 0 ≤ $26,050; (26,050, 360.69, 2.75%), (100,000, 2,394.32, 3.50%).
- 2025 (HB 96 yr 1): 0 ≤ $26,050; (26,050, 342.00, 2.75%), (100,000, 2,394.32, 3.125%).
- 2026+ (HB 96 yr 2, enacted; no booklet yet): 0 ≤ $26,050; (26,050, 332.00, 2.75%) flat.

Business income: taxable business income taxed at FLAT 3% all years (ORC
5747.02(A)(4)(a)); unused exemptions offset taxable business income first
(5747.02(A)(4)(b)).

## Parameter inventory

- `agi.yaml`: federal AGI start; full SS subtraction (5747.01(A)(5), all years);
  state/municipal refund subtraction (5747.01(A)(11)(a)); US-obligation interest
  subtraction (5747.01(A)(3)); non-Ohio municipal interest addition; BID
  (5747.01(A)(28)): 100% of first $250,000 ($125,000 MFS) of business income,
  all years, with MAGI addback for 2019+ means tests.
- `ded.yaml`: none (no standard/itemized deduction; neutral defaults).
- `exempt.yaml`: per-exemption amount (taxpayer+spouse+deps) tiered by OAGI
  (2017-18) / MAGI (2019+): 2017 $2,300/$2,050/$1,800 (≤$40k / $40-80k / >$80k);
  2018-2019 $2,350/$2,100/$1,850; 2020-2025 $2,400/$2,150/$1,900 (single 2020
  indexation, round UP to $50); 2025+ $0 tier at MAGI ≥ $750,000 (HB 96); 2026
  cap tightens to $500,000. Dependent filers get $0 (5747.025(B)).
- `ord.yaml`: rate schedules above with explicit base amounts; flat 3% business
  rate + BID cap (business carve-out params).
- `credits.yaml`:
  - $20/exemption credit (5747.022): tax base (2017-18) or MAGI-less-exemptions
    (2019+) < $30,000.
  - Joint filing credit (5747.05(E)): MFJ, each spouse ≥$500 qualifying income
    (OAGI excl. interest/dividends/capital gains/rents/royalties and Schedule of
    Adjustments deductions); 20/15/10/5% of tax after preceding credits, by tax
    base (2017-18) or MAGI-less-exemptions (2019+) tiers $25k/$50k/$75k; max $650;
    2025+ requires MAGI < $750,000; 2026 ≤/< $500,000 (boundary TBD in 2026 forms).
  - Retirement income credit (5747.055(B)): $25/50/80/130/200 by qualifying
    retirement income bands ($500/$1,500/$3,000/$5,000/$8,000), max $200/return;
    income test < $100,000 (tax base 2017-18; MAGI-less-exemptions 2019+).
  - Senior citizen credit (5747.055(F)): $50/return, 65+, same $100,000 test.
  - EITC (5747.71): 2017-18 10% of federal, nonrefundable, AND if tax base >
    $20,000 limited to 50% of tax remaining after preceding credits (excl. JFC);
    2019+ (HB 62, eff. 7/3/2019): 30%, limitation repealed. Never refundable.
  - CDCTC (5747.054): 100% of federal credit if OAGI (2017-18) / MAGI (2019+)
    < $20,000; 25% if < $40,000; else 0. Nonrefundable.
  - Credit ordering (5747.98): retirement → senior → CDCTC → $20 exemption
    credit → JFC → EITC. JFC multiplies tax net of the earlier credits; EITC
    offsets tax net of everything incl. JFC.
- `filing.yaml`: no filing duty if OAGI ≤ exemptions (booklet three-part test);
  tax is zero whenever OAGI − exemptions ≤ zero-bracket top.

## Indexation

- Brackets (5747.02(A)(5)): GDP deflator, nearest $50, rates never adjusted, no
  downward adjustment. Exemptions (5747.025(B),(C)): GDP deflator, round UP to
  $50, beginning 2020.
- Suspensions: 2019 both (HB 166 §757.160); 2021 brackets + 2021-22 exemptions
  (HB 110 §803.97(B)); 2023-24 both (HB 33 §757.50); 2025-26 both (HB 96
  §757.120(A)). Applied: 2018, 2020, 2022 (brackets). Model years 2027+ carry
  2026 values forward (indexing scheduled to resume under continuing law; treat
  as projection).

## Worksheet tests to add

- Zero-bracket cliff: TI just below vs just above the zero-bracket top (2024:
  $26,050 → $0; $27,050 → $360.69 + 27.50).
- 2025 internal discontinuity at $100,000 (base $2,394.32 vs $2,375.63 continuation).
- Exemption tier boundary + $20 credit cliff at $30,000.
- JFC: MFJ two-earner case with rate tier, $650 cap, and ordering after
  retirement/senior/CDCTC/$20 credits.
- Retirement + senior credit case with the $100,000 MAGI-less-exemptions test.
- BID: business income above/below $250,000; MAGI-based credit denial via addback.
- 2017-18 EITC 50%-of-remaining-tax limitation above $20,000 tax base.

## Known differences

- School-district and municipal income taxes omitted (Phase 7).
- Uniformed-services retirement subtraction, disability/survivor benefits, 529,
  medical-expense subtraction, adoption/campaign/displaced-worker/home-school/
  SGO credits, lump-sum retirement/distribution credits: omitted (data limits /
  materiality).
- BID business-income definition: modeled from PUF Schedule C/E/F components;
  ≥20%-owner compensation reclassification and the 2020+ law/lobbying exclusion
  unobservable.
- JFC qualifying-income test proxied by each spouse's earned income (wages +
  self-employment net of the BID-deducted share is imperfect).
- 2020 ARPA UI exclusion flows through FAGI (Ohio conformed via SB 18 2021);
  no separate Ohio UI subtraction exists (Ohio taxes UI).
- 2026 values from ORC/LSC (no forms yet); $500,000 boundary semantics TBD.
- Secondary-only attributions: 2017 bottom-bracket elimination attributed to
  HB 49; SB 18 UI-conformity mechanics from ODT alert (PDF not read).

## Cross-model validation notes

- TAXSIM years: 2018 (8-bracket), 2019 (post-HB 166), 2021 (post-HB 110), 2023,
  2025. Expect differences on BID (TAXSIM handling differs) and JFC ordering.
- PolicyEngine spot checks 2021+.

## Aggregate validation notes

- HT2 state × AGI-class totals once weights land; ODT annual report individual
  income tax collections (GRF) as the revenue benchmark.

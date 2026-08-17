# Maine State Source Packet

State: `ME`
Status: `ENCODED 2026-08-17 (baseline/me/, tests ME-1..ME-9); cross-model not yet run`
Last updated: `2026-08-17`

## Scope

- TY2017-2025 transcribed from primary forms (1040ME booklets/general
  instructions, Schedules 1/1A/1S/2/A/PTFC-STFC, MRS annual rate
  schedules) plus 36 M.R.S. statute text. Resident Form 1040ME only.
- Structure: three brackets 5.8/6.75/7.15% (all years; the 2016 ballot 3%
  surtax was REPEALED before effect -- P.L. 2017 c. 284 Pt. D; the 2017
  schedule is literally titled "no surcharge"); standard deduction =
  FEDERAL amounts; a WHOLE-DEDUCTION linear phase-out (standard AND
  itemized) in every year; Schedule 2 itemized with an indexed overall cap
  (medical-only exemption); personal exemption with its own phase-out
  (2017 = the federal exemption incl. dependents and the federal PEP);
  the dependent exemption tax credit from 2018; a large per-person
  pension deduction reduced by gross SS; refundable EITC ALL years;
  refundable-to-$500 child care credit; the refundable income-keyed sales
  tax fairness credit (ENCODED as a dense table); the property tax
  fairness credit (documented, Tier 1).

## Corrections to the batch survey (STATE_ENCODING_REVIEW_2026_08_11 §2.1)

1. **The ME EITC has been REFUNDABLE for residents since TY2016**
   (5219-S(4)) -- not "5% nonrefundable through 2019".
2. **The STFC is income-keyed only** (no purchase/rent data needed) -- the
   review's "Tier-1-blocked" applied only to the PTFC. The STFC is encoded.
3. **The 2024+ pension cap is the SS-maximum rule** ($45,864 in 2024,
   $48,216 in 2025, printed on Schedule 1S line P2) -- the P.L. 2021 c.
   635 schedule's $35,000-for-2024 was superseded.
4. The 2017 itemized cap is the INDEXED $28,600, not the statutory base
   $28,350.

## Primary sources / retrieval

- NBER archive `taxsim.nber.org/historical_state_tax_forms/ME/{2017..2024}/`
  via curl (booklets, schedules, worksheets, PTFC/STFC); maine.gov/revenue
  for the 2025 set and the 2024 Schedule 1S; legislature.maine.gov statute
  HTML (5111, 5122, 5124-B, 5125, 5126-A, 5213-A, 5219-KK, 5219-S).
- **TY2019 rate schedule unobtainable** (maine.gov 404; Wayback down at
  retrieval): 2019 lower thresholds and all bases verified against the
  official 2019 TAX TABLE; the HoH/MFJ UPPER thresholds ($77,550/$103,400)
  are DERIVED (verified single upper x statutory 1.5/2.0 x uniform COLA)
  -- flagged in ord.yaml.
- All downloads + extractions preserved in the encoding session's scratch
  dir; per-value citations in the YAML `reference:` keys.

## Encoding decisions

1. **Whole-deduction phase-out**: the line-17 worksheet reduces whichever
   deduction is taken linearly to zero over $75,000/$112,500/$150,000 of
   MAINE AGI above the year's threshold (2017: 70k/105k/140k; 2025:
   100k/150k/200,050; MFS = single). Standard side: the RI stepped
   machinery at a $1 step (share 1/width; within half a basis point of the
   4-decimal worksheet ratio). Itemized side: the NY share machinery with
   share1 = 1 over the same thresholds (both mapped to ONE set of
   dedpo_* subparams).
2. **Cap-then-phase-out order**: Schedule 2 caps at line 5 and the
   phase-out hits line 7's total (INCLUDING the cap-exempt medical). The
   calculator's flat-cap block was MOVED ahead of the share-based
   phase-out to match -- behavior-preserving for OK (no share phase-out)
   and NY (no flat cap), verified by the full suite.
3. **Itemized base**: federal itemized less ALL taxes-paid plus REAL
   ESTATE and PERSONAL PROPERTY taxes PAID -- i.e., property taxes escape
   the federal $10,000 SALT cap (5125(3)(A-1)), which is exactly the
   model's default state itemized base with salt_addback = 1. Cap
   $28,600 -> $36,300 with ONLY medical exempt (the charitable carve-out
   was repealed by P.L. 2015 c. 267); `item_include_medical: 1` declared
   for the cap-exemption sizing (the OK convention). Only federal
   itemizers may itemize, best-of (MD item_fed_gate).
4. **2017 exemption regime**: Maine used the federal line-42 amount --
   $4,050 x exemptions INCLUDING dependents, net of the federal PEP --
   encoded as personal+dep amounts 4,050 with the 2017 federal PEP
   (2%/$2,500 steps at the federal thresholds, federal AGI). 2018+:
   taxpayer/spouse-only ($4,150 -> $5,150), LINEAR phase-out over $125,000
   ($62,500 MFS) of Maine AGI (the $1-step machinery again).
5. **Pension deduction** (Schedule 1S worksheet): per-person cap ($10,000
   2017-2021, $25,000 2022, $30,000 2023, $45,864 2024, $48,216 2025 --
   the SS-max rule floats it after; CPI-indexed forward as an
   approximation of the wage-indexed SS maximum), reduced by GROSS SS/RR
   received (`pension_cap_less_gross_ss`, the MD convention: the
   worksheet nets per person in separate columns, we net total-vs-total --
   interior-equivalent, corner differences documented), IRAs ELIGIBLE
   (`pension_excl_incl_ira = 1`), no age gate. **NEW 2025 phase-out**
   (P.L. 2025 c. 388 Pt. H) = new generic `pension_excl_po_thresh/width`
   (FAGI > 125k/187.5k/250k, $100k width, $50k MFS). Military pensions
   fully exempt with no cap/offset/phase-out -- unobservable subset,
   documented [overstates ME tax for military retirees].
6. **DETC as NY-style ctc_style 2** with the new `ctc_po_step` generic:
   $300/dependent nonref 2018-2023 ($7.50 per $1,000 over $200k/$400k,
   round up, aggregate), REFUNDABLE from 2024, and the 2025 restructure
   ($610 under-6 at year-end / $305 for 6+, $20 per $500 over
   100k/125k/150k/75k of Maine AGI). Dependent ages tracked to 23 --
   older adult dependents missed (documented).
7. **Child care credit**: 25% of the federal CDCC, refundable UP TO $500
   -- the new `cdctc_ref_cap` generic splits the refundable portion at the
   cap (the .inf default preserves every other state). The 50%
   quality-certificate rate is unobservable (documented).
8. **STFC encoded as a dense table** (credit_id `stfc`, new st_stfc output
   in the household module): per-return amount by filing status x capped
   dependent count with the stepped phase-out ($10/$500 single, $15/$750
   HoH, $20/$1,000 MFJ) baked into the printed bands. All NINE vintages
   2017-2025 transcribed programmatically from the printed tables
   (PyMuPDF word coordinates), endpoint- and decrement-verified, worked
   examples reproduced; the 2017 exemption-keyed table is mapped into
   (status x dependents) space. MFS ineligible = no fs-3 rows (stated in
   every year's instructions). Income concept = federal AGI + nontaxable
   SS + exempt interest (`stfc_add_*` flags); the loss and above-the-line
   add-backs of the form's broad "total income" are documented
   approximations [overstate the credit for loss/deduction units].
9. **Conformity**: fixed-date advanced by act (Dec 31 2024 for TY2025 --
   OBBBA NOT conformed; the 2025 forms use pre-OBBBA federal std amounts,
   which is what the transcribed values carry). Modeled rolling (group 0):
   the unconformed items are below-AGI or business-side, and Maine let the
   ARPA TY2020 UI exclusion flow through (no 2020 Schedule 1A add-back
   line -- structural verification; the MRS Tax Alert itself was not
   retrieved).
10. Printed rate-schedule bases are rounded to whole dollars (e.g. 2024
    single base 1,511 vs continuous 1,510.90); worksheet tests pin the
    CONTINUOUS values (VT convention).

## Worksheet tests (ME-1..ME-9, all hand-computed)

ME-1 2024 basic single; ME-2 the 2017 federal-style exemption regime;
ME-3 the deduction phase-out on the standard deduction; ME-4 the itemized
cap (medical exempt) then phase-out, in the worksheet's order, with the
best-of election; ME-5 the 2024 SS-max pension cap less gross SS with
aged standard add-ons; ME-6 the 2025 pension phase-out stacked on the
deduction phase-out; ME-7 the 2022 credit stack (25% EITC, nonrefundable
DETC, capped-refundable care credit, STFC from the table); ME-8 the 2025
DETC restructure ($500 steps); ME-9 the STFC alone mid-phase-out.

## Known differences (documented in the YAML)

Military pension full exemption (decision 5); PTFC (property tax/rent --
Tier 1; PE models it -> expect a one-sided PE divergence at low income
like the WI homestead); STFC broad-income add-backs; per-person pension
netting corners + the always-both-caps MFJ approximation; pre-55
non-SEPP distributions; EITC 18-24 childless extension (2020+) and ITIN
filers (2021+) [understate ME EITC]; quality-care 50%; adult dependent
care credit; student loan repayment credit (5217-E); 529 subtraction
(2023+); MainePERS pick-ups; bonus-depreciation add-back (model-wide);
MFS same-method and no-income-spouse rules; 4-decimal worksheet-ratio
rounding (~$1.50 max).

## Uncertainties

1. The 2019 HoH/MFJ upper bracket thresholds are derived, not
   form-verified (see retrieval note).
2. The 2021 pension worksheet was not directly opened ($10,000 rests on
   statute + the 2022 "Important Changes" narrative).
3. Per-year IRC conformity dates 2017-2019 not transcribed (immaterial:
   the forms embed the conformed amounts).
4. DETC/2025-threshold indexation after 2024/2025: published values
   carried flat pending each year's worksheet.

## Cross-model validation notes

- TAXSIM 2017-2020 / PE 2021+. Expected wedges: military-pension share
  (ours-high on ME tax); PTFC (PE-low at low income); STFC income-concept
  approximations; TAXSIM's ME pension/SS-offset handling worth probing
  (the MD experience: TAXSIM std-ded vintage bugs).
- The 2019 derived thresholds only matter above ~$77k HoH / $103k MFJ in
  2019 TAXSIM cells -- if those cells show a uniform wedge at exactly
  0.40pp of the affected slice, revisit the derivation.

## Aggregate validation notes

- HT2 ME total tax once weights land; MRS annual "Maine Revenue Services
  collections" and the Maine Compendium of State Fiscal Information as
  revenue-agency benchmarks.

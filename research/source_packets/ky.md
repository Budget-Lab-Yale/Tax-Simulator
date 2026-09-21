# Kentucky State Source Packet

State: `KY`  
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-11` (2017 graduated schedule, combined-return

> **Status note (as of 2026-08-11), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete
filing, standard-deduction vintages, and personal tax credits corrected
from the 2017 Form 740 packet + DOR announcements; see below)

## Scope

- Tax years covered: 2017-2035. Published Form 740 values are transcribed through 2025; the enacted 2026 3.5 percent rate carries forward pending review.
- Resident individual income tax only. No nonresident allocation or pass-through entity credit.
- Major features: federal-AGI base, Social Security and pension exclusions, standard/itemized deduction, flat rate, family-size tax credit, and child/dependent-care credit.

## Primary sources

- [Kentucky individual income tax page](https://revenue.ky.gov/Individual/Individual-Income-Tax/Pages/default.aspx).
- [2025 Form 740 packet](https://revenue.ky.gov/Forms/740%20Packet%20Instructions%20%282025%29.pdf).
- [2025 Form 740](https://revenue.ky.gov/Forms/740%20%282025%29.pdf), lines 19-26 for family-size and care credits.
- [2025 Schedule ITC](https://revenue.ky.gov/Forms/Schedule%20ITC%20%282025%29.pdf) and [2018 Schedule ITC](https://revenue.ky.gov/Forms/Schedule%20ITC%202018.pdf) for Table C.

## Parameter inventory

- `agi.yaml`: federal AGI start, municipal/U.S. interest flags, full Social Security subtraction, and the $41,110/$31,110 pension-exclusion history.
- `ded.yaml`: standard deduction history ($2,480 2017 per the packet; $2,530/$2,590/$2,650/$2,690/$2,770 per the DOR annual announcements 2018-2022; packet values 2023-25) and federal-itemized carry-through mode. *(Corrected 2026-08-11: the 2017-2021 values had been shifted one year.)*
- `ord.yaml`: **2017 graduated schedule** (2/3/4/5/5.8/6% at $3k/$4k/$5k/$8k/$75k, "$4,166 plus 6% of amount over $75,000", 2017 packet p.11); flat 5.0% 2018-22 (HB 366), 4.5% in 2023, 4.0% 2024-25, 3.5% from 2026. *(Corrected 2026-08-11: 2017 had been simplified to a flat 5.8%.)* Plus `combined_sep = 1`: married filing separately on a combined return — each spouse's column applies the schedule to own income less own standard deduction (or income-share of itemized, per the Schedule A rule), floored at zero; the unit takes the lower of joint and combined. Column income = own wages + half of non-wage state AGI (ownership unobserved; documented approximation).
- `credits.yaml`: generic four-family-size percentage-of-tax Table C plus nonrefundable 20 percent federal CDCTC match. **Personal tax credits added 2026-08-11**: $10 per regular credit (taxpayer/spouse/dependents) in 2017 (Form 740 Section B), repealed by HB 366 for 2018+; 65+/blind at $40 in ALL years (2017: four $10 boxes; 2018+: Schedule ITC $40). Applied before the family-size credit per the Form 740 line 17 → 21 ordering. The $20 Kentucky National Guard credit is unobservable and omitted.

## Worksheet tests

- KY-1: 2025 single at $16,000 MGI — Table C 90% family-size band applied to preliminary tax.
- KY-2/KY-3: 2017 graduated schedule (5.8% band and 6% top bracket) + the $10 personal credit; KY-2 reproduces TAXSIM-35 to the cent (2,562.16).
- KY-4/KY-11: 2017 combined-return split, standard and itemized (income-share division per the Schedule A rule).
- KY-5: 2019 standard-deduction vintage ($2,590).
- KY-6/KY-7: flat-year combined return — two-earner 2× std vs the one-earner per-column zero floor.
- KY-8: pension exclusion cap ($31,110, 2018).
- KY-9: aged $40 credit (2023).
- KY-10: CDCTC 20% match (2024).

## Known differences

- Table C uses modified gross income with MFS-spouse income, certain municipal interest, and lump-sum adjustments. The baseline uses federal AGI plus observable state additions; affected records are documented approximation cases.
- Combined-return columns: non-wage income split 50/50 (ownership unobserved) and itemized deductions divided by income share; actual returns follow ownership/election.
- The pension exclusion has source, public/private, and special-government pension distinctions not observable in the PUF. The shared cap applies broadly and needs source-specific validation.
- The $20 Kentucky National Guard credit, education credits, PTET, business credits, and most Schedule M additions/subtractions are omitted. *(The $40 age/blind credits and the 2017 $10 personal credits are now encoded, 2026-08-11.)*
- 2017 family-credit bounds use the first available modeled-period table pending a primary 2017 Schedule ITC transcription; 2018-24 annual FPL table updates also remain a review item.
- CDCTC base: the form multiplies federal Form 2441 line 9; we use the limited federal credit (PA-style base approximation).

## Cross-model and aggregate validation

- Pre-fix (2026-07-19 run): TAXSIM clean match@100 was 0.273 (2017) / 0.458-0.476 (2018-20), the worst in the module. Root causes were OURS: the flat-5.8% 2017 simplification, one-per-return standard deduction for married filers (KY combined returns give one per spouse), the shifted 2017-2021 std vintages, and the missing personal credits. All fixed 2026-08-11; a harness rerun is pending.
- TAXSIM differences pre-registered from a direct WASM probe (src/tests/state/cross_model/known_differences.csv, 2026-08-11): TAXSIM doubles each spouse's 2017 std ded (~$287.68/couple), and grants both std deductions to one-earner couples where the form's per-column zero floor wastes one (~$130).
- Aggregate: blocked until weights land; compare HT2 income/returns and Department of Revenue annual net individual income tax collections.

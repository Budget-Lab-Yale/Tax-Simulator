# Kentucky State Source Packet

State: `KY`  
Status: `baseline encoded; record-level worksheet tests complete`  
Last updated: `2026-07-13`

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
- `ded.yaml`: standard deduction history and federal-itemized carry-through mode.
- `ord.yaml`: 5.8 percent in 2017, 5.0 through 2022, 4.5 in 2023, 4.0 in 2024-25, and 3.5 percent from 2026.
- `credits.yaml`: generic four-family-size percentage-of-tax Table C plus nonrefundable 20 percent federal CDCTC match.

## Worksheet tests

- 2025 single filer at $16,000 modified gross income: confirms the Table C 90 percent family-size credit band and its application to preliminary tax.

## Known differences

- Table C uses modified gross income with MFS-spouse income, certain municipal interest, and lump-sum adjustments. The baseline uses federal AGI plus observable state additions; affected records are documented approximation cases.
- The pension exclusion has source, public/private, and special-government pension distinctions not observable in the PUF. The shared cap applies broadly and needs source-specific validation.
- $40 age/blind credits, education credits, PTET, business credits, and most Schedule M additions/subtractions are omitted.
- 2017 family-credit bounds use the first available modeled-period table pending a primary 2017 Schedule ITC transcription; 2018-24 annual FPL table updates also remain a review item.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2018, 2023, 2025, and 2026 cases, especially pension exclusions and Table C boundaries.
- Aggregate: blocked until weights land; compare HT2 income/returns and Department of Revenue annual net individual income tax collections.

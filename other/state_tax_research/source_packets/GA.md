# Georgia State Source Packet

State: `GA`  
Status: `baseline encoded; record-level worksheet tests complete`  
Last updated: `2026-08-11` (married-filing-joint personal exemption
corrected: $7,400 TOTAL per O.C.G.A. 48-7-26 / HB 386 of 2012, had been
transcribed as a $3,700 joint total — $212.75 at 5.75% on every married
return 2018-2023, the dominant GA cross-model wedge; a TAXSIM WASM probe
matches our corrected law EXACTLY on the joint case and on four other
structural cases: wage baseline, 65+ earned-cap exclusion + aged std
deduction, broad unearned exclusion, and the 2017 graduated schedule.
Test GA-4 pins the $7,400.)

## Scope

- Tax years covered: 2017-2035. Published Form 500 values are transcribed through 2025; the final published flat rate carries forward.
- Baseline only. Resident individual income tax; no nonresident apportionment.
- Major features: federal-AGI base, Social Security exclusion, age-62 retirement exclusion, standard/itemized deductions, personal/dependent exemptions through 2023, 2024/25 rate reductions, and the child/dependent-care credit.

## Primary sources

- [Georgia IT-511 instruction booklet landing page](https://dor.georgia.gov/it-511-individual-income-tax-instruction-booklet), including 2017-2025 Form 500 instructions.
- [2025 IT-511 booklet](https://dor.georgia.gov/document/document/2025-it-511-individual-income-tax-booklet/download) for the 5.19 percent rate and 50 percent child/dependent-care credit.
- [2024 IT-511 booklet](https://dor.georgia.gov/document/document/2024-it-511-individual-income-tax-booklet/download) for the 5.39 percent rate, deduction/exemption changes, and 30 percent care credit.
- [Georgia retirees FAQ](https://dor.georgia.gov/retirees-faq) for the Social Security and retirement-income rules.
- O.C.G.A. 48-7-20, 48-7-27, and 48-7-29.10 for rate, retirement, and care-credit law.

## Parameter inventory

- `agi.yaml`: federal AGI start; U.S. obligation flag; full Social Security subtraction; generic per-spouse retirement exclusion with 2017-23 $4,000 and 2024+ $5,000 earned-income caps.
- `ded.yaml`: status-specific standard deduction history, age/blind additions through 2023, federal-election coupling, and generic federal-itemized carry-through.
- `exempt.yaml`: pre-2024 personal/dependent exemptions and the 2024 dependent-exemption increase.
- `ord.yaml`: six-bracket schedules through 2023; flat 5.39 percent in 2024 and 5.19 percent in 2025.
- `credits.yaml`: nonrefundable federal child/dependent-care credit match, 30 percent through 2024 and 50 percent from 2025.
- `filing.yaml`: federal-filer-or-liability rule.

## Worksheet tests

- 2025 age-65 single filer: earned-income-first ordering and $65,000 retirement exclusion.
- 2023 age/blind standard deduction before the 2024 restructuring.
- 2025 child/dependent-care credit match.

## Known differences

- The PUF does not identify pension ownership, disability eligibility under age 62, royalty income, or all qualifying retirement-income types. The generic calculator splits jointly held non-wage amounts equally and treats observable categories as the best available proxy.
- U.S. obligation interest is unobservable in taxable interest and therefore only retained as a law flag.
- Georgia itemization has a state-income-tax adjustment not separable from the aggregate SALT input. The model carries the federal itemized amount and documents this omission rather than applying an incorrect all-state-tax addback.
- Low-income, adoption, education, military, caregiver, and other IND-CR credits require data or eligibility not available in the baseline PUF.
- The unborn-child exemption beginning in 2024 is not observable.
- Post-2025 rate policy requires annual review before a new rate is encoded.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2017, 2023, and 2025 cases, emphasizing retirement-income ordering and itemization.
- Aggregate: blocked until state weights land; inspect Georgia HT2 income strata and compare state-wide liability to DOR annual reports.

# State Source Packet: North Dakota

State: `ND`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-16`

## Scope

- Tax years covered: 2017-2025 (2026 rate schedule available only via ND-1ES; not encoded).
- Baseline only.
- Major structural features: starts from federal TAXABLE income; rolling IRC
  conformity; graduated rate schedule with a distinct MFS column; HB1158 (2023)
  three-tier 0/1.95/2.50% collapse; 40% net-long-term-capital-gain and 40%
  qualified-dividend exclusions; Social Security subtraction phasing to full.

## Primary sources

- Form ND-1 and Individual Income Tax booklets, TY2017-2025:
  `https://www.tax.nd.gov/sites/www/files/documents/forms/individual/{YEAR}-iit/`
  (2017 and 2019 via Wayback/NBER mirror due to 403s; others direct).
- Rate schedules / tax tables: ND-1 booklet rate-schedule pages; 2026 rates from
  Form ND-1ES (SFN 28709, 12-2025) — provisional, not encoded.
- Statute: N.D.C.C. ch. 57-38 (`https://ndlegis.gov/cencode/t57c38.pdf`),
  esp. 57-38-30.1 (rates), 57-38-30.3(2) (modifications), 57-38-01(5),(13)
  (rolling conformity), 57-38-31 (filing requirement).
- Capital gain / dividend exclusion: 57-38-30.3(2)(d); ND-1 worksheet.

## Parameter inventory by file

- `ord.yaml`: graduated `rates` (two regimes) + filing-status-mapped
  `brackets_single/married/mfs/head`, per year 2017-2025. Recapture omitted
  (ND has none; defaults to Inf).
- `agi.yaml`: `start_point=2`; rolling `conformity_year=0`, `conformity_group=0`;
  `add_exempt_int=0` (all muni interest exempt, no addback); `sub_us_int=1`
  (flag; not modeled); `sub_state_ref=0`; SS via `ss_sub_share` (0 -> 1 at 2021)
  and `ss_full_sub_allages` (2019-2020) with `ss_allages_agi_limit_single/joint`
  (50k/100k); `cap_gains_excl_share=0.40`, `div_excl_share=0.40`.
- `ded.yaml`, `exempt.yaml`: all zero (federal deduction flows through; no ND
  standard/itemized deduction, no state income-tax addback, no exemptions).
- `filing.yaml`: `req_type=3`, `req_if_fed_filer=1`.
- `credits.yaml`: `eitc_match=0` (documents that ND has no EITC/CTC).

## Known differences / approximations

- **US-obligation interest subtraction** (57-38-30.3(2)(a)): flagged `sub_us_int=1`
  but NOT subtracted — the US-obligation share of taxable interest is unobserved
  in the PUF (model-wide known-difference).
- **2019-2020 SS AGI cap** ($50k/$100k) applies to ALL ages; modeled with the
  new `ss_full_sub_allages` param. The exact 2019 enacting bill number was not
  primary-source confirmed (session/effective-year verified).
- **Credits not modeled**: ND Marriage Penalty Credit (57-38-01.28; NOT repealed
  — HB1388 repeal failed 2025), Family Member Care Credit (57-38-01.20, elderly/
  disabled care), credit for tax paid to another state, and various investment
  credits — all nonrefundable and not PUF-representable. The 2021-2022 $350/$700
  residency relief credit (HB1515) was one-time, non-earnings-based, expired.
- **Military pay/retirement, College SAVE, peace-officer retirement** subtractions:
  not identifiable in the PUF; omitted.
- **TY2026** schedule (ND-1ES only) not encoded; simulations of 2026 will project
  2025 law.
- Pre-2023 bracket thresholds transcribed per year; 2017 intermediate thresholds
  carry <=$10 uncertainty (top-band constant is primary).

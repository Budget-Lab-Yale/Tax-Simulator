# State Source Packet: South Carolina

State: `SC`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-11` (age-65 deduction 12-6-1170(B) and the Two Wage
Earner Credit encoded — the two largest SC cross-model wedges; 5-case TAXSIM
probe now agrees within $10 everywhere, incl. retirees at $0)

## Scope

- Tax years covered: 2017-2025. TY2026 (H.4216/Act 110) is a structural reform
  (federal-AGI start + SC Income Adjusted Deduction, 1.99%/5.21% schedule,
  $200 EITC cap); forms unpublished; NOT encoded.
- Baseline only.
- Major structural features: starts from federal TAXABLE income; FIXED-DATE IRC
  conformity (annual); ONE rate schedule for all filing statuses (subtraction
  method); 2022 reform collapsing 6 brackets to 3 with a top rate falling
  7% -> 6.5% (2022) -> 6.0% (2025 floor); 44% net-capital-gain deduction; full
  SS exemption; retirement + age-65 deductions; per-dependent exemption (2018+);
  nonrefundable SC EITC phasing to 125% of federal.

## Primary sources

- SCDOR forms at `https://dor.sc.gov/sites/dor/files/forms/`: `SC1040TT_{YEAR}`
  (tax tables + Tax Rate Schedule), `SC1040Instr_{YEAR}` / `IITPacket_{YEAR}`,
  `SC1040TC`, `TC-60` (EITC), `I-319` (tuition).
- Statute: S.C. Code Title 12 ch. 6 (`scstatehouse.gov/code/t12c006.php`), esp.
  12-6-510/520 (rates/indexing), 12-6-40 (conformity), 12-6-1120/1140/1150/1170
  (modifications/deductions), 12-6-3632 (EITC), 12-6-560 (start point).
- Reform: 2022 rate act; contingent reductions in 12-6-510; H.4216/Act 110
  (2026, SCDOR "Information about H.4216").
- Conformity dates: annual conformity Acts + SCDOR IRC Conformity Guides 18-20, 21-23.

## Parameter inventory by file

- `ord.yaml`: single (non-mapped) `rates` + `brackets`, per year 2017-2025.
  Pre-2022 brackets = 1x..5x the indexed 0% ceiling. Reform-year element-3
  boundary calibrated so `(top-0.03)*B1 + 0.03*B0` equals the exact published
  constant C (658/670/659/642), making tax exact above B1.
- `agi.yaml`: `start_point=2`; `conformity_group=3` (fixed_date_annual, ready
  false); year-keyed `conformity_year`; `add_exempt_int=1`/`own_state_exempt=1`
  (out-of-state muni addback); `sub_us_int=1` (flag; not modeled);
  `sub_state_ref=1`; `ss_sub_share=1` (full SS exemption);
  `cap_gains_excl_share=0.44`; retirement deduction via
  `pension_excl_under65=3000`/`pension_excl_65plus=10000`/`pension_excl_min_age=0`.
- `ded.yaml`: `salt_addback=1` (state/local income-or-sales tax addback for
  itemizers); no standard/itemized deduction.
- `exempt.yaml`: per-dependent `dep_amount` (0 in 2017; 4110->4930 for 2018-2025).
- `filing.yaml`: `req_type=3`, `req_if_fed_filer=1`.
- `credits.yaml`: `eitc_match` (0 -> 1.25 over 2018-2023), `eitc_refundable=0`.

## Known differences / approximations

- **Fixed-date conformity** modeled as reference-group 3 (ready=false): SC is
  excluded from `states=all` and blocked for federal-reform runs until a
  reference-law overlay is built (same treatment as CA). Baseline-law runs use
  current-law federal values (SC's ~1-year conformity lag is a known-difference).
- **Age-65 deduction** ($15,000/person, reduced by the retirement deduction;
  12-6-1170(B)): **ENCODED 2026-08-11** via the generic aged-deduction
  machinery + a new `age_ded_less_pension_sub` offset flag (elderly survey
  recommendation #3). Unit-level offset — exact for singles and both-65+
  couples; over-offsets by up to $3,000 when only an under-65 spouse claims
  (A). Tests SC-8/SC-9; TAXSIM matches retiree probe cases within $3.20.
- **Military-retirement deduction** (fully deductible 2022+) not separately
  identified; **44% cap-gain deduction** needs net LT gain over net ST loss
  (PUF has it at return level).
- **Under-6 dependent doubling** (line t) not modeled (needs dependent-age detail).
- **SALT addback** uses the shared mechanism; SC's exact $10k-cap interaction is
  a known-difference vs the CO form.
- **US-obligation subtraction**: flag only, not modeled (PUF share unobserved;
  Fannie/Freddie/Ginnie do not qualify).
- **Two Wage Earner Credit** (12-6-3330): **ENCODED 2026-08-11** on the WI
  married-couple-credit machinery (0.7% of the lesser earner's earned income,
  cap phased $30k→$50k in six Act-266 steps, max $210→$350). TAXSIM's 2019
  value matches exactly (256.67). Tests SC-10/SC-11. SC-source qualified
  earned income proxied by total earned income (known-difference).
- **Credits not modeled**: Child & Dependent Care (7% of federal Sec.21
  EXPENSES, a different base than the model's cdctc match, so omitted rather
  than mis-encoded); Tuition (refundable, needs student data); nursing-home,
  motor-fuel (2018-2022), excess-premium.
- **65+ filing threshold** (federal threshold + $15k/$30k) not modeled (federal-
  filer trigger used).
- **TY2026 H.4216 reform** not encoded (forms unpublished); simulations of 2026
  will project 2025 law and be structurally wrong for SC.
- Reform-year 0% ceiling B0 carries $10-50 uncertainty; the exact constant C is
  preserved so tax above B0 is exact.

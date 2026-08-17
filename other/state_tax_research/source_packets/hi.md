# Hawaii State Source Packet

State: `HI`
Status: `ENCODED 2026-08-17 (baseline/hi/, tests HI-1..HI-8); cross-model not yet run`
Last updated: `2026-08-17`

## Scope

- TY2017-2025 transcribed from primary forms, PLUS the full enacted Act 46
  SLH 2024 schedule (bracket steps TY2025/2027/2029; standard-deduction
  steps TY2024/2026/2028/2030/2031) and the Act 163 SLH 2023 sunset
  reversions for TY2028+. Resident Form N-11 only.
- **The batch survey's Act 46 timing was wrong**: the first BRACKET step is
  TY2025 (already printed in the 2025 booklet, cross-verified
  digit-for-digit against the enacted HB2404 CD1 text), not TY2027; 2031 is
  a standard-deduction step only.
- Structure: 12-bracket schedule to 11% from 2018 (9 brackets to 8.25% in
  2017, Act 107 SLH 2017); $1,144 exemptions with the aged DOUBLING (a 65+
  filer counts two exemptions); own small standard deduction; own itemized
  deductions computed on Hawaii AGI with an INDEPENDENT election, the
  $100k/$150k/$200k FAGI disallowance of the state-income-tax deduction,
  and a retained pre-TCJA-style overall limitation at fixed
  $166,800/$83,400 thresholds on HI AGI; 7.25% alternative tax on net
  capital gain; employer-funded pension and Social Security income fully
  excluded; refundable food/excise credit (dense N-311 table); refundable
  sliding-rate CDCC; EITC 20% nonrefundable 2018-2022 -> refundable 2023 ->
  40% 2023-2027 with the Act 163 sunset back to 20%.

## Primary sources / retrieval

- NBER archive `taxsim.nber.org/historical_state_tax_forms/HI/{yr}/` via
  curl: N-11 form + instruction booklets 2017-2024, N-311 (2017-2020,
  2022-2024), Schedule X, N-356.
- DOTAX `files.hawaii.gov/tax/forms/{yr}/`: 2025 set + prior-year Schedule
  X / N-356 gaps.
- Legislature: Act 107 SLH 2017, Act 114 SLH 2022 (HB2510), Act 163 SLH
  2023 (SLH PDFs); Act 46 SLH 2024 from the enacted **HB2404 CD1 text**
  (the certified SLH PDF is not posted; every TY2024/2025 value
  cross-checks against the printed forms). `capitol.hawaii.gov` needs a
  browser user-agent.
- **Form N-311 (Rev. 2021) unobtainable** (DOTAX 404, not in NBER,
  archive.org unreachable at retrieval): TY2021 food-credit values are
  statute-verified (the pre-2023 HRS 235-55.85(b) table is quoted verbatim
  in Act 163) and bracketed by identical 2020/2022 forms.
- All downloads + page-marked text extractions preserved in the encoding
  session's scratch dir; citations per value in the YAML `reference:` keys.
- **Act-number discrepancy**: the Legislature records HB2404 (2024) as Act
  46; DOTAX booklets print "Act 45, SLH 2024" for the same changes.
  Substance verified identical; cited here as Act 46.

## Encoding decisions

1. **Alternative capital-gains tax = new generic machinery**
   (`st_ord.kg_alt_rate` 7.25% + `st_ord.kg_alt_floor` 24k/36k/48k,
   st_tax.R): the Tax on Capital Gains Worksheet taxes gain above
   max(TI - net capital gain, floor) at 7.25% when that beats the schedule.
   `kg_pref` is exactly the worksheet's base (smaller of HI net LTCG and HI
   net capital gain; **qualified dividends are NOT included** --
   full-text-verified in the 2017/2024/2025 booklets). Omitting it would
   have overstated HI tax by up to 3.75pp x gains on every high-income
   gain-haver (the VT no-new-params lesson). The N-158
   investment-interest offset (line 9) is unobservable and documented.
2. **SALT disallowance = new `st_ded.salt_addback_agi_thresh`** (default
   -Inf preserves every other salt_addback state): income/sales taxes are
   excluded from the itemized base only at FAGI >= 100k/150k/200k
   (Worksheet A-2, permanent since TY2011).
3. **Overall limitation via pease machinery + new `st_ded.pease_agi_base`**
   (enum; HI computes on HAWAII AGI, unlike the federal-AGI default): 3%
   over fixed $166,800/$83,400, 80% cap, medical/investment-interest/
   casualty protected. The worksheet also protects gambling losses (in
   A-6); that slice of other_item is not separable (documented).
4. **Food/excise credit = banded percap table in credit_tables.csv**
   (credit_id `percap_credit`, new banded leg of the ID percap family,
   income base federal AGI): per-PERSON amount by FAGI band, fs-keyed
   (single table ends at $30k/$40k; other statuses at $50k/$60k). Three
   vintages: 2017 (through TY2022), 2023 (Act 163 doubled), 2028 (sunset
   reversion = the 2017 table). Aged extra exemptions do NOT increase it
   (the form counts persons). DHS public-support minor children (flat $110
   + gate exception, pre-2023) and the MFS combined-FAGI rule are
   documented.
5. **CDCC = style 2 with two new generics**: `cdctc_rate_po_step` (the rate
   falls 0.01 per $5,000 band of HI AGI over $25,000, floored at 15% --
   stepped, not the NY continuous slide) and `cdctc_expense_ei_limit` (the
   2441-style earned-income cap HI carries; NY 2026 stays as encoded).
   Caps $2,400/$4,800 -> $10,000/$20,000 (Act 163, TY2023-2027, sunset).
   Refundable (N-11 line 30 sits in the refundable block).
6. **Pension exclusion via `pension_sub_share = 1`** (CT machinery): the
   full txbl_pens_dist pool is subtracted. SOURCE-SPLIT APPROXIMATION
   (imputation plan Tier 1): elective-deferral (401(k)/457/TSP) and
   employee-contribution portions are legally TAXABLE but not separable in
   the PUF [understates HI tax for those distributions; TAXSIM and PE face
   the same input]. IRAs stay taxable (no ira_sub_share).
7. **EITC sunset encoded**: 0 (2017) / 20% nonref (2018-2022, carryforward
   documented-not-modeled) / 40% refundable (2023-2027) / 20% refundable
   (2028+, Act 163 sec. 5 reversion; refundability from Act 114 survives).
8. **Exemptions**: $1,144 personal/dependent + $1,144 aged_addl (the
   oval-count doubling). The $7,000 blind/deaf/disabled IN-LIEU exemption
   is documented-not-modeled (N-172 certification unobservable; the
   in-lieu forfeiture of dependent/aged exemptions makes the federal blind
   flag a bad proxy).
9. **Tax-table rounding**: under $100,000 the printed Tax Table is
   mandatory ($50 bands at the midpoint, rounded); the continuous schedule
   differs by <= ~$3 and printed base amounts round the continuous bases
   by <= $0.40 (e.g. single $48,000 base: printed 3,214 vs continuous
   3,213.60). Worksheet tests pin the CONTINUOUS values (VT convention).

## Verified value tables (transcription summary)

- Rates: 2017 nine brackets 1.4/3.2/5.5/6.4/6.8/7.2/7.6/7.9/8.25; 2018+
  twelve with 9/10/11% over 150k/175k/200k (single; MFJ 300/350/400k; HoH
  225/262.5/300k). Act 46 vintages transcribed for 2025/2027/2029 -- all
  three statuses, cross-checked against printed 2025 schedules. NOT
  indexed. MFS uses the SINGLE schedule; QSS the married one.
- Standard deduction: 2,200/4,400/3,212 (2017-2023) -> 4,400/8,800/6,424
  (2024-2025) -> 8,000/16,000/12,000 (2026) -> 9,000/18,000/13,500 (2028)
  -> 10,000/20,000/15,000 (2030) -> 12,000/24,000/18,000 (2031+).
  Dependent-filer worksheet: greater of $500 or earned income, capped.
- Food/excise per-person amounts: 2017-2022 single 110/100/85/70/55 (bands
  <5k/<10k/<15k/<20k/<30k), others + 45/35 (<40k/<50k); 2023-2027
  220/200/170/140/110 (<15k/<20k/<25k/<30k/<40k), others + 90/70
  (<50k/<60k). MFS combines both spouses' FAGI (documented).
- CDCC percentage table (all years): 25% under $25,001 of HI AGI stepping
  -1pp per $5,000 band to 15% at $50,001+.
- Filing thresholds: std + $1,144/exemption (3,344/6,688/4,356 single/
  MFJ/HoH through 2023; 5,544/11,088/7,568 from 2024; 2026+ derived from
  the Act 46 ramp).

## Worksheet tests (HI-1..HI-8, all hand-computed)

HI-1 2018 basic single; HI-2 2017 nine-bracket + pension/SS exclusions +
aged exemption; HI-3/3b the 7.25% alternative tax (binding gain case and
the $24,000 ordinary-floor case); HI-4/4b refundable-40% vs
nonrefundable-20% EITC vintages + both food-credit tables; HI-5/5b CDCC at
the 15% floor with Act 163 caps and one band into the rate slide with the
pre-2023 caps; HI-6/6b the SALT disallowance and overall limitation above
and below their thresholds; HI-7 the Act 46 TY2025 bracket step; HI-8 the
TY2027 schedule + TY2026 standard deduction (enacted future law).

## Known differences (documented in the YAML)

Pension source split (decision 6 -- the big one); 10% medical floor (the
model's medical component carries the federal floor) [overstates HI
medical deduction]; misc-2% deductions retained by HI but zero in the
federal data from 2018 [understates HI itemized]; home-equity/pre-TCJA
mortgage limits retained [understates]; renters credit ($50/exemption,
rent-gated -- Tier 1; PE models it -> expect a one-sided PE divergence];
EITC carryforward stocks 2019-2025 [understates HI credits]; DHS
public-support children; COLA/ERS wage additions; moving-expense
subtraction; military reserve pay exclusion; individual housing accounts;
blind/deaf/disabled $7,000 exemption; US-obligation interest (flag
carried, VT convention); N-158 election; child passenger restraint $25.

## Cross-model validation notes

- TAXSIM 2017-2020 / PE 2021+; both model HI. Expected wedges: the pension
  full-subtraction is SHARED (clean); the 10% medical floor is ours-high
  for medical itemizers; renters credit PE-only; food-credit
  presence/incarceration tests unobservable everywhere; the tax-table
  midpoint rounding is a +-$3 noise floor below match@15.
- The alternative capital-gains tax matters above ~$200k single with
  gains -- check TAXSIM models it (its HI schedule includes the alt rate?
  probe if the high-gain cells diverge at exactly 7.25% vs 11%).

## Aggregate validation notes

- HT2 HI total tax once weights land; DOTAX annual individual income tax
  collections as the revenue-agency benchmark.

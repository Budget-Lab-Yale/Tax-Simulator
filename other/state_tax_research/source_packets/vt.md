# Vermont State Source Packet

State: `VT`
Status: `research COMPLETE and primary-verified; NOT yet encoded (YAML drafted, tests drafted)`
Last updated: `2026-08-12`

> **The four scalars this packet asked for LANDED on 2026-08-12** (see
> §Machinery gaps), so VT is now encodable exactly on both its
> capital-gains exclusion and its charitable credit. The two large
> one-signed omissions this packet warned about no longer apply.

## Scope

- TY2017-2025 transcribed; indexed series carry forward beyond.
- Resident Form IN-111 only.
- **REGIME CHANGE at TY2018 (Act 11, 2018 special session):** federal TAXABLE
  income start in 2017 (with the IN-155 itemized addback), switching to federal
  ADJUSTED GROSS income from TY2018 with Vermont's OWN standard deduction and
  personal exemption — the year-keyed `start_point` pattern, MN precedent.
- Four-bracket schedule from 2018 (five in 2017), CPI-indexed annually per
  filing status; **NO Vermont itemized deduction in any year 2018+** (standard
  only, replaced by a charitable-contribution CREDIT); income-based SS /
  retirement exemption from TY2018 (full below a threshold, linear phase-out
  over a $10,000 band); greater-of flat-$5,000 / 40%-of-eligible-gain capital
  gains exclusion capped at 40% of federal taxable income; refundable EITC
  (32% → 36% → 38%, 100% for childless filers from 2025); refundable CTC from
  TY2022; CDCC 24% nonrefundable → 72% refundable from TY2022.

## Retrieval note (important for future sessions)

`tax.vermont.gov` returns **HTTP 403** to both WebFetch and curl;
`legislature.vermont.gov` fails TLS verification. The working channel is
`taxsim.nber.org/historical_state_tax_forms/VT/{year}/` **via curl** (WebFetch
403s there too). All year directories 2017-2025 exist. `pdftotext` needs
`module load poppler/25.07.0-GCC-13.3.0`.

## Verified value tables

### Rates

2017 (five brackets): `3.55 / 6.80 / 7.80 / 8.80 / 8.95%`.
2018-2025 (four brackets, unchanged): `3.35 / 6.60 / 7.60 / 8.75%`.

### Bracket thresholds

Single: 2017 `0/37,900/91,850/191,650/416,650`; then (four brackets)
2018 `0/38,700/93,700/195,450`; 2019 `0/39,600/96,000/200,200`;
2020 `0/40,350/97,800/204,000`; 2021 `0/40,950/99,200/206,950`;
2022 `0/42,150/102,200/213,150`; 2023 `0/45,400/110,050/229,550`;
2024 `0/47,900/116,000/242,000`; 2025 `0/49,400/119,700/249,700`.

MFJ/QSS: 2017 `0/63,300/153,100/233,300/416,650`;
2018 `0/64,600/156,150/237,950`; 2019 `0/66,150/159,950/243,750`;
2020 `0/67,450/163,000/248,350`; 2021 `0/68,400/165,350/251,950`;
2022 `0/70,450/170,300/259,500`; 2023 `0/75,850/183,400/279,450`;
2024 `0/79,950/193,300/294,600`; 2025 `0/82,500/199,450/304,000`.

MFS: 2017 `0/31,650/76,550/116,650/208,325`;
2018 `0/32,300/78,075/118,975`; 2019 `0/33,075/79,975/121,875`;
2020 `0/33,725/81,500/124,175`; 2021 `0/34,200/82,675/125,975`;
2022 `0/35,225/85,150/129,750`; 2023 `0/37,925/91,700/139,725`;
2024 `0/39,975/96,650/147,300`; 2025 `0/41,250/99,725/152,000`.

HoH: 2017 `0/50,800/131,200/212,450/416,650`;
2018 `0/51,850/133,850/216,700`; 2019 `0/53,100/137,050/221,950`;
2020 `0/54,100/139,650/226,200`; 2021 `0/54,850/141,700/229,450`;
2022 `0/56,500/145,950/236,350`; 2023 `0/60,850/157,150/254,500`;
2024 `0/64,200/165,700/268,300`; 2025 `0/66,200/171,000/276,850`.

**Transcription trap:** from TY2023 the printed MFJ schedule SPLITS its first
bracket at $75,000 (an artifact of the tax TABLES covering income under
$75,000). The economically distinct second threshold is 75,850 / 79,950 /
82,500 — encode those, NOT 75,000. Printed base-tax amounts are rounded to
whole dollars, so a continuous schedule differs by <= $0.05.

### Standard deduction (2018+; zero in 2017), single / MFJ / HoH

2018 `6,000/12,000/9,000`; 2019 `6,150/12,300/9,200`; 2020 `6,250/12,500/9,400`;
2021 `6,350/12,700/9,500`; 2022 `6,500/13,050/9,800`;
2023 `7,000/14,050/10,550`; 2024 `7,400/14,850/11,100`;
2025 `7,650/15,300/11,450`. MFS = single every year. Aged/blind add-on PER BOX:
`1,000` (2018-2019), `1,050` (2020-2022), `1,150` (2023), `1,200` (2024),
`1,250` (2025).

### Personal exemption (2018+; zero in 2017, per exemption)

2018 `4,150`; 2019 `4,250`; **2020 AND 2021 both `4,350`** (an actual freeze, as
published); 2022 `4,500`; 2023 `4,850`; 2024 `5,100`; 2025 `5,300`.

### SS / retirement exemption thresholds (NOT indexed)

Full exemption below, MFJ | other: 2018-2021 `60,000|45,000`;
2022-2024 `65,000|50,000`; 2025 `70,000|55,000` (S.51/Act 71 of 2025 raised
both by $5,000). Phase-out: exempt share = `round((upper - AGI)/10,000, 2)`,
i.e. 1% per $100 of AGI over the threshold, zero at threshold + $10,000. No
age test — income-tested only.

### Credits

EITC: 32% (2017), 36% (2018-2021), 38% (2022-2024); **from TY2025 the match is
100% for filers with NO qualifying children**, 38% otherwise (Act 71 of 2025).
Refundable all years. CTC (TY2022+): `$1,000` refundable per qualifying child,
reduced `$20` per `$1,000` (or fraction) of AGI over `$125,000`, **PER CHILD**;
qualifying children are ages 0-5 through TY2024, 0-6 in TY2025 (the 2019
birth-year floor was not advanced). CDCC: 24% nonrefundable (2017-2021) → **72%
refundable** from TY2022.

## Machinery gaps — RESOLVED 2026-08-12, all four scalars landed

**Both provisions are now encodable.** The four parameters recommended below
were added and tested on 2026-08-12 (tests MACH-1 through MACH-3):
`st_agi.cap_gains_excl_flat`, `st_agi.cap_gains_excl_txbl_share`,
`st_credits.char_credit_rate` and `st_credits.char_credit_base_cap`. VT can now
be encoded exactly on both, so the two large one-signed residuals described
here will NOT appear. The analysis is kept because it records why a share
parameter alone was the wrong answer.

VT's two largest provisions had no representable parameter, and both are
one-signed, so encoding without them would have guaranteed a large residual:

1. **Capital-gains exclusion.** IN-153 computes `min(40% x federal taxable
   income, max(flat: min(adjusted net capital gain ex-QD, $5,000), percentage:
   min(40% x eligible gain, $350,000)))`. The 40% branch reaches ONLY gain from
   assets held >3 years that is NOT real estate, NOT depreciable personal
   property, and NOT publicly traded securities — an unobservable residual, so
   that branch is correctly ZERO for us (PE reaches the same conclusion). The
   BINDING branch is therefore the **flat $5,000**, and
   `st_agi.cap_gains_excl_share` multiplies a SHARE of gain — no share can
   express `min(gain, 5000)`, and nothing expresses the
   40%-of-federal-taxable ceiling. **Setting share = 0.40 would be a serious
   over-exclusion, not a simplification.** Recommended:
   `st_agi.cap_gains_excl_flat` + `st_agi.cap_gains_excl_txbl_share` (~4 lines
   in `st_agi.R`). Cost of omitting: VT liability HIGH by rate x min(net LT
   gain ex-QD, 5,000, 40% x fed taxable) — max **$437.50**, modal $167-330, on
   every unit with net long-term gain.
2. **Charitable contribution credit** (5822(d)(3), TY2018+): 5% of the first
   $20,000 of contributions, max $1,000, nonrefundable, available to ALL filers
   — it REPLACED itemization in the Act 11 restructure, so it is claimed
   broadly. `st_credits.item_credit_rate` computes rate x (components less the
   state std deduction) (WI mechanic) and would return ~0; `st_ded`'s
   charitable add-on is a DEDUCTION, equivalent only at a 5% marginal rate.
   Recommended: `st_credits.char_credit_rate` + `char_credit_base_cap`. Cost of
   omitting: VT liability HIGH by min(5% x contributions, $1,000) — $150-250
   typical.

## Worksheet tests drafted (hand-verified)

- VT-1 TY2017 single, FAGI $120,000, itemized $21,000 incl. $6,000 state income
  tax → federal taxable $94,950 start, muni addback $250, SALT addback $6,000
  → VT taxable $101,200 → **$5,743.35**. Cross-checks against the printed 2017
  Schedule X ($5,743.30; the $0.05 gap is the schedule's whole-dollar base
  rounding).
- VT-2 TY2019 MFJ 67/64, one dependent, FAGI $58,000 incl. $8,000 taxable SS →
  full SS exemption (AGI under $60,000), std $12,300 + one aged box $1,000,
  exemptions 3 x $4,250 → taxable $23,950 → **$802.33**.
  VT-2b TY2023 single 68, AGI $52,000: share = 0.80 exactly (the excess is a
  multiple of $100) → **$984.90**.
- VT-3 TY2023 HoH, two dependents, wages $24,000 + $8,000 LT gain, federal EITC
  $4,200, charitable $2,000 → model `-$1,364.85` vs form-true `-$1,596.00`:
  **a +$231.15 wedge** ($150.08 capital-gains exclusion where the
  40%-of-federal-taxable cap binds at $4,480, $81.07 charitable credit). This
  is the archetypal VT residual signature.

## Known differences (beyond the two machinery gaps)

3%-of-AGI **minimum tax** over $150,000 AGI (77-2715-analogue 5822(a)(6); no
tax-FLOOR machinery exists — VT liability LOW where it binds; PE models it as
`vt_amt`, so expect a PE-high tail); **medical-expense subtraction** (TY2019+,
federal Schedule A medical less the VT deduction stack — both inputs available
but no parameter; PE models it); **CTC phase-out applied once, not per child**
(overstates the credit by (n-1) x $20 x ceil(excess/1,000), up to (n-1) x
$1,000, for 2+ children at AGI $125-175k); **renter credit** (2022+) /
Renter Rebate (structural, PE models it); **retirement exemption non-SS
branch** (up to $10,000 of non-SS-covered government/military/CSRS pension,
electable in lieu of the SS exemption, TY2022+; PE imputes it); **2017 IN-155
second addback term** (non-protected itemized over 2.5x the federal standard —
2017 only); **low-income 50% refundable CDCC 2017-2021** (accredited-provider
gate unobservable — worst for units with no VT liability, where the 24%
nonrefundable credit is worth ~0 against a 50% refundable one); 2025 childless
100% EITC not extended to childless MFJ by the machinery gate; SS phase-out
step rounding (we ceiling the reduction where the worksheet rounds the share —
<= 0.005 x taxable SS); own-state muni 75% convention; US-obligation interest
flagged not taken; bonus depreciation; veteran credit (TY2025+, $250
refundable, PE models it); 529 credit; elderly/disabled credit (24% of federal
Schedule R, which the model does not compute).

## Uncertainties

1. **The 2017 Schedule IN-155 is SECONDARY-sourced** — the form is absent from
   the NBER 2017 directory and tax.vermont.gov is unreachable. The line
   structure (and hence that the first addback term is `min(itemized - federal
   standard, state/local income tax)`, which is all we encode via
   `salt_addback`) comes from a third-party aggregator's description,
   corroborated only by the 2017 IN-111's own structure. **Retrieve
   `IN-155-2017.pdf` and confirm before encoding**; if the term is the full
   SALT deduction (property tax included), `salt_addback` alone under-adds.
2. **PE disagreement, TY2022 CDCC AGI cap.** PE sets an income limit in effect
   for 2022 (citing Act 138 s.3 as retaining the 5828c cap); the 2022 booklet's
   Schedule IN-112 Part II instructions impose **no** AGI cap and no
   accreditation test. We follow the form. If PE is right, we overstate the
   2022 CDCC for filers above $30,000/$40,000 AGI by up to 72% of the federal
   credit. Worth 15 minutes against Act 138 text.
3. **PE disagreement, minimum-tax formula**: PE computes `3% x AGI - US-oblig
   interest`; the form reads `3% x (AGI - interest)`. Immaterial to us (we
   don't model it) but relevant when reading PE residuals.
4. Statutory pin-cites (32 V.S.A. 5811(21), 5822, 5824, 5828b/c, 5830e/f,
   5825a, 5861, 5823(b)) are anchored on the corresponding **form lines, which
   were verified**, plus PE reference strings — the subdivision letters may be
   imprecise. Fix when the statute site is reachable.
5. **Conformity is FIXED-DATE** (5824 re-set nearly annually; Dec 31 2023
   currently, Dec 31 2024 for TY2025 via H.493 of 2025), modeled as rolling
   (group 0). Justified: VT has never sat out a PUF-material federal change in
   this window — it explicitly conformed to CARES and to the ARPA $10,200 UI
   exclusion for TY2020 (the Department reissued refunds), so no VT-specific UI
   treatment is needed.
6. The SS phase-out could be made EXACT by encoding the limit at
   threshold + $50 (since `ceiling((x-50)/100) = round(x/100)` off exact
   half-steps), at the cost of publishing a non-form figure. The published
   figure is encoded and the ~$15 max effect documented.
7. **Expect VT's first cross-model run to look worse than KY/ND/MN did at the
   same stage** — dominated by the two documented one-signed masses (a
   near-constant rate x $5,000 on gain-havers, and 5% of contributions on
   donors). That is the cost of the no-new-params constraint, not an encoding
   defect, and both masses vanish if the four scalars land.

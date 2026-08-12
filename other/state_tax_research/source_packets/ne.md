# Nebraska State Source Packet

State: `NE`
Status: `research COMPLETE and primary-verified; NOT yet encoded (YAML drafted, tests drafted)`
Last updated: `2026-08-12`

> Encoding is mechanical from the tables below — every value is
> primary-transcribed. Two modeling decisions (§CDCC) need a call before the
> YAML lands.

## Scope

- TY2017-2025 encoded; enacted 2026 rates/brackets and the 2027 statutory rate
  available.
- Resident individual income tax only (Form 1040N lines 5-18 + Schedules I/II).
- Major features: federal-AGI start; NE-specific standard deduction with
  aged/blind add-ons (federal-linked in 2017, decoupled and separately indexed
  by LB 1090 from 2018); itemizing allowed ONLY for federal itemizers, who take
  the greater of NE standard and federal itemized LESS state/local income
  taxes; four-bracket schedule with the **LB 754 top-rate ramp**; a
  nonrefundable per-exemption CREDIT (not an exemption); a **2017-only
  "Additional Tax Rate Schedule"** = graduated-rate-benefit recapture keyed to
  federal AGI; a **Social Security exclusion ramp** (greater of a low-income
  threshold rule and a rising percentage) reaching 100% in 2024; 10%
  refundable EITC; 25% nonrefundable CDCC above $29,000 AGI and a large
  refundable CDCC at/below $29,000.

## Primary sources

- Form 1040N booklets TY2017-2025 (`revenue.nebraska.gov`, per-year paths);
  Form 2441N (2024); Form 1040N-ES (2026).
- **DOR History of Individual Income Tax Rates by Brackets** (Rev. 2-2026) —
  the authoritative per-year bracket transcription source, 2014-2024.
- **DOR Tax Rate Chronologies Table 1** (Rev. 3-2024) — per-year rates by
  bracket, exemption-credit amount, single/joint std deduction, pre-2018
  preferential-rate AGI thresholds, and footnote 12: "The additional Tax Rate
  Schedule is not applicable for tax years 2018 through 2025."
- Statutes: Neb. Rev. Stat. 77-2715.03 (rates, LB 754 ramp incl. (2)(c)(v) for
  2026), 77-2716 ((14)(a) SS ramp; (15) military retirement), 77-2716.01
  (std deduction / base), 77-2715.07 (exemption credit, CDCC, EITC,
  elderly-or-disabled credit), 77-2714 (rolling IRC conformity).
- Session laws: LB 970 (2012), LB 1090 (2018), LB 738 (2018), LB 64 (2021),
  LB 873 (2022), LB 754 (2023), LB 1107 (2020), LB 34 (2024 special session).

## Verified value tables

### Rates by bracket (brackets 1/2 constant at 2.46% / 3.51%)

| TY | b1 | b2 | b3 | b4 |
|---|---|---|---|---|
|2017-2022|2.46|3.51|5.01|6.84|
|2023|2.46|3.51|5.01|**6.64**|
|2024|2.46|3.51|5.01|**5.84**|
|2025|2.46|3.51|5.01|**5.20**|
|2026|2.46|3.51|**4.55**|**4.55**|
|2027|2.46|3.51|**3.99**|**3.99**|

Note 2026 is the first year the THIRD bracket is cut (Form 1040N-ES 2026 p.6
cites 77-2715.03(2)(c)(v)).

### Bracket thresholds

Single (= MFS): 2017 `0/3,090/18,510/29,830`; 2018 `0/3,150/18,880/30,420`;
2019 `0/3,230/19,330/31,160`; 2020 `0/3,290/19,700/31,750`;
2021 `0/3,340/19,990/32,210`; 2022 `0/3,440/20,590/33,180`;
2023 `0/3,700/22,170/35,730`; 2024 `0/3,900/23,370/37,670`;
2025 `0/4,030/24,120/38,870`; 2026 `0/4,130/24,760/39,900`.

MFJ/QSS: 2017 `0/6,170/37,030/59,660`; 2018 `0/6,290/37,770/60,840`;
2019 `0/6,440/38,680/62,320`; 2020 `0/6,570/39,410/63,500`;
2021 `0/6,660/39,990/64,430`; 2022 `0/6,860/41,190/66,360`;
2023 `0/7,390/44,350/71,460`; 2024 `0/7,790/46,760/75,340`;
2025 `0/8,040/48,250/77,730`; 2026 `0/8,250/49,530/79,800`.

HoH: 2017 `0/5,760/29,620/44,230`; 2018 `0/5,870/30,210/45,110`;
2019 `0/6,020/30,940/46,200`; 2020 `0/6,130/31,530/47,080`;
2021 `0/6,220/31,990/47,760`; 2022 `0/6,410/32,950/49,200`;
2023 `0/6,900/35,480/52,980`; 2024 `0/7,270/37,400/55,850`;
2025 `0/7,510/38,590/57,630`; 2026 `0/7,700/39,620/59,160`.

2025 values are PE-sourced but **arithmetic-verified against three published
2025-booklet worksheet constants** ($3,566 single / $3,088 MFJ / $3,276 HoH of
tax at $77,760), which reproduce to the dollar only with those thresholds and
the 5.20% top rate.

### Standard deduction (single / MFJ / HoH; MFS = single)

2017 `6,350/12,700/9,350`; 2018 `6,750/13,500/9,900`; 2019 `6,900/13,800/10,100`;
2020 `7,000/14,000/10,300`; 2021 `7,100/14,200/10,450`; 2022 `7,350/14,700/10,750`;
2023 `7,900/15,800/11,600`; 2024 `8,350/16,700/12,250`; 2025 `8,600/17,200/12,600`.

Aged/blind add-on PER BOX (single/HoH | MFJ/QSS/MFS): 2017 `1,550|1,250`;
2018-2019 `1,600|1,300`; 2020-2021 `1,650|1,350`; 2022 `1,700|1,400`;
2023 `1,850|1,500`; 2024 `1,950|1,600`; 2025 `2,000|1,650`.

### Personal exemption CREDIT (nonrefundable, per exemption)

2017 `$132`; 2018 `$134`; 2019 `$137`; 2020 `$140`; 2021 `$142`; 2022 `$146`;
2023 `$157`; 2024 `$166`; 2025 `$171`; 2026 `$176`.

### Social Security (statute grants the GREATER of the two rules)

Percentage rule: 0 (2017-2020), 5% (2021, LB 64), 40% (2022, LB 873), 60%
(2023), **100% (2024+, LB 754)**.

Threshold rule (full exclusion at/below federal AGI, all ages), MFJ | other:
2017-2019 `58,000|43,000`; 2020 `59,100|43,820`; 2021 `59,960|44,460`;
2022 `61,760|45,790`; 2023 `66,510|49,310`; repealed as unnecessary from 2024.
(The printed 2020 booklet says $43,000 for non-joint; the erratum on its own
cover corrects it to **$43,820** — PE agrees.)

### Other credits

EITC 10% of federal, refundable, all years. CDCC: 25% of federal
nonrefundable above $29,000 AGI; REFUNDABLE at/below $29,000 computed on Form
2441N as capped expenses x federal decimal x **state decimal** (1.00 at/below
$22,000, stepping down 10 pp per $1,000 to 0.30 in the $28,000-29,000 band).

### 2017-only Additional Tax (graduated-rate-benefit recapture)

AGI thresholds single `261,500` / MFJ `313,800` / MFS `156,900` / HoH
`287,650`; full recapture reached at AGI `559,800` / `910,400` / `455,200` /
`729,950`; maxima `$855.99` / `$1,712.02` / `$855.99` / `$1,314.19`.

**Identity check confirming the NY-style recapture is the right machinery:**
single `0.0684 x 29,830 - T(29,830) = 2,040.37 - 1,184.38 = 855.99` — exactly
the published maximum, to the cent. MFJ `0.0684 x 59,660 - T(59,660) =
4,080.74 - 2,368.73 = 1,712.01` vs published `1,712.02` (one-cent rounding).
Encode via `recapture_agi_start` / `recapture_width` (filing-status mapped,
`.inf` from 2018); the published three-segment interior phase-in is
linearized (both endpoints exact, interior deviation ~$100-250, TY2017 only).

## Decisions still open (make before encoding)

**The CDCC needs a call.** A single share table can span both AGI regimes
(bands 1-5 carrying the refundable state decimals, the open top band carrying
the 0.25 nonrefundable match), but `cdctc_refundable` is a single flag, so:

1. **D1 — refundable base.** NE's Form 2441N recomputes from EXPENSES; our
   share table multiplies the LIABILITY-LIMITED federal credit. At AGI
   <= $29,000 federal liability is often zero, so the model returns zero or a
   fraction of a credit worth up to 100% of 35% of $3,000/$6,000 of expenses —
   **understates NE refunds by up to ~$1,050 (one child) / $2,100 (two+)**.
   Worked example NE-3 shows a $295.12 gap on one return.
2. **D2 — refundability above $29,000.** Setting the flag refundable makes
   the >$29,000 25% match refundable where the form makes it nonrefundable,
   **understating liability** by up to ~$260-525 for units whose NE tax falls
   below 25% of the federal credit (a real population, since the
   $166-per-exemption credit zeroes NE tax up to roughly $30-45k of AGI).

Neither is fixable with existing parameters. If the module later exposes a
pre-limitation federal CDCTC or per-band refundability, **NE should be
revisited first among all states**.

## Worksheet tests drafted (all hand-verified)

- NE-1 TY2017 single $50,000: four-bracket schedule + $132 credit →
  `$1,997.67`. **Independently validated against the printed 2017 tax table**
  (row 43,860-43,960 = $2,147; our schedule at the 43,910 midpoint = $2,147.45
  → $2,147, exact agreement).
- NE-2 TY2024 MFJ both 68, SS $28,000 taxable + pension $120,000: 100% SS
  exclusion, 2 aged boxes (std $19,900, matching the printed chart), 5.84% top
  rate → `$4,105.32`. Same unit under TY2022 law → `$5,963.23` (exercises both
  ramps).
- NE-3 TY2019 HoH EITC + refundable CDCC → form-true `-$868.04` vs model
  `-$572.92`, documenting the D1 gap.
- NE-4 TY2022 SS greater-of at AGI $65,000 → `$1,022.42`; at $61,000 the
  threshold rule wins → `$460.82`. **A $4,000 AGI increase raises NE tax by
  $561.60 — a genuine cliff in the law, not an artifact.**
- NE-5 the 2017 Additional Tax identity above.

## Known differences

**Tier 1.** School-district property tax credit (LB 1107, TY2020-2023) and
community-college credit (LB 873, TY2022+), Form PTC — refundable, keyed to
property taxes paid, unobservable. **The largest single omission**: on the
order of $0.5-1.0 billion/year statewide across claimant types at its
2022-2023 peak. **From TY2024 the school-district piece leaves the return
entirely** (LB 34 moved it to the property-tax statement), shrinking the
omission to the community-college credit. **Any NE aggregate benchmark for
2020-2023 must be net-of-PTC or it will look wildly off.** Plus D1/D2 above.

**Tier 2.** Own-state muni share at the model's 75% convention (almost
certainly far too high for a state this small → addback understated); SALT-cap
allocation in NE itemized (income-first assumption, MD precedent); sales-tax
electors enter zero on line 8 and are not modeled; US-obligation interest
flagged not taken; the 2017 Additional Tax interior linearization; unobservable
Schedule I subtractions (military retirement **100% from TY2022**, Railroad
Retirement, CSRS annuities TY2024+, 529/ABLE/LTC, Form 4797N special capital
gains, NE NOL); NE "other tax" (29.6% of federal Form 4972 / early-distribution
taxes) omitted; elderly-or-disabled credit (100% of federal Schedule R, which
the model does not compute).

**Tier 3.** MFS EITC/CDCC ineligibility not gated; QSS takes the joint SS
threshold where the booklet implies the lower one; the "$5,000 of net Nebraska
adjustments" alternative filing test is not representable; MFS aged/blind box
conditions not gated; dependent-filer std floor/bonus duplicate federal
indexed values and will drift from `tax_law/baseline/std.yaml` if that file is
revised.

## Uncertainties

1. TY2025 brackets are not in a DOR chronology PDF (that file stops at 2024);
   the 2025 values are PE-sourced but arithmetic-verified against three
   published worksheet constants. Confidence high; a direct read of the 2025
   Tax Calculation Schedule page would close it.
2. 2026 MFJ thresholds are PE-sourced from the ES page (single and HoH were
   read directly from the ES text). Verify before relying on 2026.
3. 2027 rate 3.99% is statutory but **2027 thresholds are unpublished**.
4. Whether the Additional Tax revives after TY2025: the chronology says "not
   applicable for tax years 2018 through 2025" because it keys on the federal
   personal exemption, and OBBBA (2025) permanently repealed those — so it
   should stay inapplicable, but this is an inference, not a cited DOR
   statement. Encoded `.inf` from 2018 with no reactivation.
5. `recapture_agi_start`/`recapture_width` have not previously been
   filing-status mapped anywhere in the module (NY uses scalars); validate the
   mapped columns reach `calc_st_tax` on a TY2017 slice.
6. Dependent-std floor/bonus values are recalled federal Rev. Proc. figures;
   reconcile against the repo's federal `std.yaml` before writing.

## Cross-model and aggregate validation

- **PE has NO Nebraska parameters before 2021** — a hard limit; 2017-2020
  rests entirely on primaries. PE also **lacks the 2017 Additional Tax
  Schedule**, so expect PE to understate 2017 NE tax by up to $855.99 (single)
  / $1,712.02 (MFJ) for high-AGI filers: a systematic, cleanly identifiable
  residual. On every parameter present in both, PE and the primaries agree.
- Probe TAXSIM on **2017 first** (Additional Tax + pre-LB 1090 std), then
  2021-2023 (SS greater-of), then 2024 (rate + SS ramps).
- Aggregate: HT2 returns/AGI by bracket; NE DOR *Individual Income Tax
  Statistics* and the biennial *Tax Expenditure Report* for provision-level
  benchmarks (SS exclusion, std deduction, exemption credit, EITC, CDCC,
  property-tax credits).

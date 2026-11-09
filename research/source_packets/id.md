# Idaho State Source Packet

State: `ID`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

> **Status note (as of 2026-07-23), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

Full research notes with per-year citations and complete bracket tables:
[research/raw/id_research_core.md](research/raw/id_research_core.md) (all schedules extracted
from the TY2017-TY2025 Form 40/39R instruction packets on tax.idaho.gov, the
STC published rate schedules, and Idaho Code Title 63 ch. 30).

## Scope

- Tax years 2017-2035; parameters transcribed through TY2025. 2026+ carries
  the 2025 flat rate and thresholds (no enacted change as of 2026-07-23;
  thresholds index annually — documented carry-forward) and the enacted CTC
  sunset.
- Resident Form 40/39R only.
- Major features: federal-TAXABLE-income start (Form 40 rebuilds the federal
  deduction stack from AGI — identical result), CO-style SALT addback, four
  rate regimes ending in the 2023+ flat tax with a CPI-INDEXED zero bracket,
  refundable per-person grocery credit, $205 nonrefundable CTC (2018-2025),
  dependent-care deduction, and the $10 Permanent Building Fund excise.

## Primary sources

- Form 40 + 39R instruction packets TY2017-TY2025 (tax.idaho.gov, URLs in the
  research notes); STC Individual Income Tax Rate Schedule page.
- Idaho Code 63-3004 (conformity), 63-3011B/63-3022 (base), 63-3022A/D/H/K/O
  (subtractions), 63-3024 (rates), 63-3024A (grocery credit), 63-3029L (CTC),
  63-3082 (PBF).
- Session laws: 2018 HB463/HB675; 2021 HB380; 2022 HB436, HB509; 2022 1st
  E.S. HB1 (flat tax); 2024 HB521; 2025 HB40 (5.3%), HB231 (grocery $155).

## Parameter inventory

- `agi.yaml`: federal-taxable start (rolling conformity group 0 — Idaho
  re-enacts conformity retroactively every session, unlike SC's frozen
  date); non-Idaho muni addback (75% own-state convention); US-obligation
  flag; state-refund subtraction; full SS subtraction. Documented, not
  modeled: bonus-depreciation decoupling, retirement-benefits deduction
  (63-3022A), Idaho-situs capital-gains deduction (63-3022H), 529/MSA.
- `ded.yaml`: SALT addback (Form 40 line 14 = capped SALT less property
  taxes, exactly the CO mechanics incl. the itemized-over-standard limit,
  which also captures Idaho's itemize-federally/standard-for-Idaho election);
  dependent-care deduction $3,000/person capped at 2 (Idaho did NOT adopt the
  2021 ARPA expansion).
- `exempt.yaml`: none (2017 federal exemptions ride the starting point).
- `ord.yaml`: full per-year schedules — 2017 seven brackets 1.6-7.4%;
  2018-2020 1.125-6.925%; 2021 five brackets 1.0-6.5%; 2022 four brackets
  1.0-6.0%; 2023+ flat 5.8% / 5.695% / 5.3% with indexed zero brackets
  4,489/8,978 → 4,673/9,346 → 4,811/9,622 (do NOT hard-code the statutory
  $2,500/$5,000). Status mapping is unusual: MFS uses the SINGLE schedule,
  HoH uses the MARRIED schedule.
- `credits.yaml`: grocery credit (new generic per-person credit family:
  $100 → $120 (2023) → $155 (2025), aged +$20 through 2024 then eliminated,
  refundable); CTC $205 under the UT-style flat per-child machinery
  (2018-2025, sunset 2026); explicit no-EITC.
- `surtax.yaml`: PBF $10 per required return, all years (NEVER repealed —
  verified on the 2023/2024/2025 forms), blind exemption modeled.
- `filing.yaml`: federal filing requirement passes through.

## Generic components introduced

1. **Per-person credit** (`st_credits.percap_*`): flat amount per taxpayer
   and dependent with an aged add-on and refundability switch.
2. **Per-return excise** (`st_surtax.per_return_amount`, blind exemption):
   sits outside the credit stack like the taxable-income surtax.

## Worksheet tests (src/tests/state/test_state_calc.R, ID-1 .. ID-7)

- 2017 seven-bracket schedule + grocery + PBF; 2020 SS subtraction on the
  married schedule; 2024 SALT addback + flat tax; 2023 care deduction + CTC +
  family grocery; 2025 5.3% + $155 flat grocery; refundable grocery with the
  blind PBF exemption (net refund); 2021 HoH on the married schedule.

## Triage 2026-08-22 — closed; the QBI lead was right, and it was a harness gap

All four cells clear: 0.9997 / 0.9827 / 0.9842 / 0.9833. Our encoding was not
changed.

**Landed first, both superseding earlier annotate rows.** The DC/CA
crosswalk-exposure class (Idaho starts from federal taxable income, so the
federal itemized deduction enters the base directly; federal itemizers matched
at 0.162 against 0.928). And the Idaho instance of T18, probed for Idaho rather
than carried over from Virginia: TAXSIM grants the care deduction at the full
federal cap without the IRC 21(d)(1) earned-income limit -- siitax identical at
4,873.16 whether the spouse earns $40,000, nothing or $2,000, a flat $415.50 =
$6,000 x 6.925%. Those two took 2017 to 0.977 but left 2018-2020 at ~0.86.

**The remainder was section 199A, and the cause was in the harness.** The
residual was entirely post-TCJA, which pointed at QBI, and the measurement is
unambiguous: records with a QBI deduction matched at **0.60 against 0.91**
without, the state taxable-income gap equals **-qbi_ded** (median ratio
-1.0006), and diff divided by that gap is 0.0692, Idaho's rate. So a federal
section 199A difference was landing whole in the Idaho cell.

It should never have reached the cell. `fed_aligned` exists to condition away
federal disagreements, and the PolicyEngine branch has always compared federal
taxable income "for fed-taxable-start states" -- but the TAXSIM branch compared
only federal AGI, EITC and tax-exempt interest. Idaho's base *is* federal
taxable income, so every federal difference below the AGI line passed straight
through. `v18_federal_taxable_income` was available from TAXSIM all along and
was simply dropped by the leg's `select`. Adding it makes the two branches
symmetric.

This is why excluding QBI records outright would have been the wrong fix: it
treats a symptom, and the same hole was letting every other sub-AGI federal
difference through for every federal-taxable-income-base state. The corrected
filter lifted 160 of 200 TAXSIM cells across all states.

## Known differences

- **Retirement-benefits deduction omitted** (CSRS pre-1984/military/ID
  police-fire only, 65+, reduced dollar-for-dollar by SS): pension source
  unobserved; the SS offset keeps the net deduction small for most.
- **Idaho-situs capital-gains deduction omitted** (60%, no stocks): situs
  unobservable — biases liability up for gain-heavy returns.
- **US-obligation interest share** of taxable interest unobserved (flag
  carried, no subtraction — model-wide convention).
- **Non-Idaho muni share** of exempt interest unobserved (75% own-state
  convention).
- **529/MSA deductions omitted** (contributions unobserved).
- **Grocery credit proration** for SNAP/incarceration months and the 2025
  receipts option unobservable; full annual credit overstates it for SNAP
  households.
- **PBF public-assistance exemption** unobservable (blind exemption IS
  modeled); grocery-credit-only refund filers below the federal threshold
  correctly avoid the PBF via the required-filer gate.
- **Bonus-depreciation decoupling** unobservable at the micro level.
- **TY2020 ARPA UI non-conformity needs no addback** here because the
  federal calculator does not model the $10,200 exclusion; add one if that
  changes.
- **Idaho PTE workaround (ABE, 2021+)** out of scope; can distort aggregate
  comparisons for high-income units.

## Cross-model validation notes

- TAXSIM 2017-2020: coverage of the grocery credit, CTC (2018+), and PBF is
  UNVERIFIED — expect signatures of −$100 × persons (+$20 aged), −$205 ×
  kids under 17, and +$10 flat if modeled.
- PolicyEngine 2021+: models the rate schedules/flat tax, grocery credit,
  and CTC (2025 research note covers the 5.3% rate, $155 credit, and CTC
  expiration); PBF/retirement-deduction/capital-gains-deduction coverage
  unverified.
- Triage hints: a 2023+ zero-bracket mismatch usually means the other model
  hard-coded $2,500/$5,000; verify the other model gives HoH the MARRIED
  schedule.

## Aggregate validation notes

- Blocked on Phase 1 weights. Compare with STC annual reports / HT2; note
  the capital-gains-deduction omission (biases up) against the grocery
  proration omission (biases the credit up).

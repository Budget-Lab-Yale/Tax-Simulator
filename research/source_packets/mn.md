# Minnesota State Source Packet

State: `MN`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-11` (TAXSIM triage: clean, 11 exact probe cases;

> **Status note (as of 2026-08-11), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete
childless M1CWFC phase-out corrected to the general 12% per the 2024 form)

Full research notes with per-year tables and citations:
[research/raw/mn_research_core.md](research/raw/mn_research_core.md) (Form M1 booklets and
schedules 2017-2025, DOR inflation memos and algorithm sheets, Minn. Stat.
ch. 290; PolicyEngine corroboration).

## Scope

- Tax years 2017-2035; parameters transcribed through TY2025, carried
  forward beyond (indexed in law; documented).
- Resident Form M1 only.
- Major features: federal-TAXABLE-income start in 2017 (with SALT addback)
  switching to federal-AGI start from TY2018 (year-keyed `start_point`);
  the TY2018 TCJA-nonconformity year encoded as TCJA FAGI + MN's own
  pre-TCJA deduction/exemption stack; four-bracket graduated schedule
  (6.80% second tier from 2019); MN standard/itemized deductions with the
  high-income limitation (two-tier + flat-80% from 2023, applying to BOTH
  deductions); dependent exemptions with a 2%-per-$2,500 phase-out; dual
  Social Security subtraction regimes (sliding 2017-2022; greater-of
  simplified/frozen-sliding 2023+); WFC 2017-2022 and the combined
  CTC+WFC (M1CWFC) 2023+; marriage credit; capped dependent-care credit;
  1% NIIT over $1M (2024+).

## Machinery introduced (all generic)

1. Two-tier Pease with flat-80% override and standard-deduction inclusion
   (`st_ded.pease_thresh2/rate2/flat_thresh/pease_incl_std`).
2. Share-based exemption phase-out (`st_exempt.po_share_per_step`).
3. Sliding partial SS subtraction (`st_agi.ss_partial_*`, provisional
   income = AGI − taxable SS + 50% gross SS + exempt interest) and a
   stepped phase-out for the all-ages full subtraction
   (`st_agi.ss_allages_po_step/_share`); greater-of election automatic.
4. Non-itemizer charitable share (`st_agi.sub_char_nonitem_share`).
5. Combined child + working-family credit (`st_credits.cwfc_*`, joint
   phase-out on max(earned, AGI)).
6. Two-earner marriage credit (`st_credits.mc_*` + `mc_single_brackets`
   family: joint-schedule tax less single-schedule tax on imputed shares).
7. Dependent-care income cap (`st_credits.cdctc_cap_*`).
8. Net-investment-income add-on tax (`st_surtax.inv_income_*`).

## Worksheet tests (src/tests/state/test_state_calc.R, MN-1 .. MN-11)

Basic 2024 return; 2017 taxable-income start + SALT addback; 2018
pre-TCJA stack on FAGI; sliding SS + aged standard add-ons (2021);
simplified SS stepped phase-out beating the frozen alternative (2024);
two-tier deduction limitation + exemption phase-out (2023); WFC
triangular schedule (2021); M1CWFC combined credit (2024); marriage
credit (2022); NIIT + flat-80% limitation (2024); dependent-care cap
(2023).

## Known differences

- **2017 only:** MN's incremental Pease and exemption-phase-out addbacks
  (M1M lines 1-2, thresholds below federal) not modeled — affects
  AGI ~$186-314k itemizers.
- **2018:** M1NC residual items (post-retroactivity: tuition/fees, CARES
  business items, moving expenses, opportunity zones) skipped; the
  pre-TCJA restorations are on MN's own lines and ARE encoded.
- **M1SA components:** medical uses the federal-floor amount (MN floor is
  10% of AGI); misc-2% deductions zero in post-TCJA PUF data (MN allows);
  casualty is federal disaster-only (MN allows non-disaster); the property
  tax cap is applied to property taxes alone rather than the combined line.
- **2021 dependent care:** our federal CDCTC is ARPA-law; MN computed its
  own pre-ARPA credit — overstates the MN credit for 2021.
- **WFC eligibility:** the 2017-18 federal-EIC gate approximated by age
  alone; the childless upper age limit (64) unmodeled; M1CWFC older
  children proxied by dependents aged 18-23 (students/disabled
  unobserved); dependent slots cap tracked children at three (the credit
  has no child limit). *(Resolved 2026-08-11: the childless M1CWFC
  phase-out is the GENERAL 12%, not 9% — 2024 form line 13 gives 9% only
  to older-child-only units; we had applied 9% to childless units.
  Fixed in st_credits_child.R; test MN-12.)*
- **Marriage credit:** lesser earner's share uses earned income only (the
  M1MA lines 1-5 pension/SS elements unobserved); the printed lookup
  table's midpoint rounding ignored.
- **Renter's credit (2024+):** ON the M1 via Schedule M1RENT but requires
  rent data we lack — STRUCTURAL totals difference from TY2024;
  PolicyEngine includes it (expected one-sided divergence).
- **MN AMT (6.75%)** document-only per the no-state-AMT policy (PE models
  it — expect residuals on high-SALT itemizers). NIIT encoded; its
  agricultural-land carve-out unobservable, threshold treated unindexed.
- **Skipped subtractions:** QPEN public-safety pensions (2023+), military
  pensions, K-12 education, 529, M1R elderly/disabled, US-obligation
  share, bonus-depreciation 80%/5-year mechanics.
- **Conformity:** fixed-date (May 1, 2023 currently) modeled as rolling;
  2025 OBBBA nonconformity is below-AGI for the marquee items and does
  not reach MN's own deduction stack.

## Cross-model validation notes

- TAXSIM 2017-2020: **triage 2026-08-11 — eleven probe cases match TAXSIM
  to the cent (or ~$1 indexed rounding) across all three regimes,
  dependents, the sliding SS subtraction, the marriage credit, and the
  WFC at phase-in/phase-out/childless/2-child edges.** No MN encoding
  defects on definable shapes; the clean-match residual concentrates in
  itemizers (M1SA components + TAXSIM SALT circularity) and the
  documented 2017 M1M addbacks (KD row).
- PolicyEngine 2021+: models everything we encode PLUS the MN AMT,
  renter's credit (2024+), and QPEN — three expected one-sided divergence
  sources.

## Aggregate validation notes

- Blocked on Phase 1 weights; compare with MN DOR income tax statistics.
  Note the renter's credit exclusion (2024+) when reading totals.

# Federal-side divergences (for separate review)

**Policy (JI, 2026-07-18):** the state cross-model harness documents federal
divergences here and then ignores them — the clean-subset metrics condition
them away so state-law validation is not polluted. They are NOT dismissed:
each item below is a real disagreement between our federal calculator and an
external model's, and someone should review whether any indicates a problem
in OUR federal calculation rather than theirs. This file is the handoff.

How the filter works: a record is "federally aligned" when the external
model's federal AGI is within $100 of ours (net of `state_ref`, which TAXSIM
never sees), federal taxable income within $100 (PolicyEngine leg only),
federal EITC within $15, and (TAXSIM only) `exempt_int == 0`. Flags are set
in `cross_model_compare()` (`src/tests/state/test_state_cross_model.R`); the
`fed_aligned` column is on every per-record file in `results/raw/`.

Scale: for IL (a state whose own law matches near-perfectly once federally
aligned), federal-side noise accounted for roughly 25–35 points of raw
match@$100 in both model windows.

## 1. Federal EITC disagreements (TAXSIM and PolicyEngine)

Both external models compute their own federal EITC and assume full take-up;
state EITCs piggyback at 18–20%+, scaling every federal difference into the
state comparison.

Two verified sub-patterns (IL 2019, TAXSIM):

- **Eligibility**: records where we deny EITC and TAXSIM grants it. The
  diagnosed mechanism is the investment-income test — `exempt_int` counts
  toward the limit and TAXSIM has no tax-exempt-interest input, so it cannot
  deny on that ground. Verified on record ids 220652, 210863 (2019 sample,
  seed 76): our `eitc = 0`, TAXSIM granted ~$240 federal → ~$43 IL EITC gap.
  **Review question**: confirm every such denial traces to investment income
  or another modeled test, not a bug in `calc_eitc()`.
- **Amounts**: records where both grant EITC but amounts differ, e.g. id
  296044: ours $403.92 vs TAXSIM $504.88 (gap ×18% → $18 IL state gap).
  Candidate causes: earned-income concept differences (SE income treatment),
  prior-year-EI election, AGI-vs-EI phase-out base. **Not yet root-caused.**

## 2. Residual federal AGI tail vs TAXSIM (~8% of records)

After the 2026-07-18 crosswalk repair (see commit 78bcbf143), the TAXSIM
federal AGI gap is $0 at the median but the p99 is ~+$13.8k (TAXSIM higher)
on 2019 data. Untraced candidates: taxable-SS computation differences
(compare `v12_soc_sec_agi` vs `txbl_ss`), capital-loss limitation
(`txbl_kg` vs TAXSIM's own stcg/ltcg netting), SECA-deduction interplay on
mixed wage/SE records. Raw material: any `results/raw/taxsim_{year}.csv`,
records with `fed_aligned == FALSE` and `exempt_int == 0`.

## 3. QBI in the TAXSIM crosswalk — CLOSED 2026-07-19

`taxsim_crosswalk()` now maps QBI inputs (SE income to `pbusinc`/`pprofinc`
by SSTB share, non-SE QBI income to `scorp`, totals preserved; see
`src/tests/test_taxsim.R`). The 2017–2020 rerun moved the
federal-taxable-start states' clean match rates by +3 to +5 points in
2018–2020 (2017 unchanged, as expected pre-TCJA). Remaining approximation:
TAXSIM assumes a sufficient wage bill, so its QBID can exceed ours above
the phase-out for low-wage-bill businesses.

## 4. PolicyEngine's federal return

PE recomputes the entire federal return from raw-ish inputs, so any
modeling difference lands here: its own taxable-SS, QBID, standard/itemized
choice, and full-take-up credits. The driver now exports `pe_fed_agi`,
`pe_fed_taxable`, `pe_fed_eitc` per record for exactly this review. Also
note: PE imputes the Alaska Permanent Fund Dividend into AK households'
federal AGI (verified +$2,622/record constant in 2022), so AK records are
never federally aligned — benign for state validation (AK liability is
0 = 0) but relevant if these outputs are reused.

## Operational notes for the reviewer

- Per-record raw files are overwritten per year by the most recent run's
  state set — regenerate with `research/state_tax/cross_model/run_cross_model.R` if a state you need is
  missing from `results/raw/`.
- Federal pre-pass caches: `cache/fed_calc_{year}.rds` (sample seed 76,
  dependent filers excluded — TAXSIM mstat-8 semantics differ).

# Clausing excise distribution: COLA'd-benefit offset (built 2026-07-06)

Status: built, run, verified. Final run = SLURM job 17167766 on the
clausing_v2_s50 vintage (results near-identical to the initial
clausing_estate run, job 17165384 — differences in the 3rd-4th decimal).

## Headline (10-yr avg, 2026$)

- Offset is ~$13B/yr overall (-$71/unit) against the ~$131B/yr net excise burden.
- Quintile 1: +0.36% of ATI against a -2.67% burden -> net excise effect -2.32%.
- Per-unit dollar offset is flat-ish across quintiles (~$45-90) because Social
  Security is everywhere in the distribution; the progressivity comes through
  the ATI denominator (fades to +0.03% in Q5, ~0 at the top).

## Implementation

- Price effect: pi = (net excise revenue / 0.75) / PCE (gdp_c from
  Macro-Projections, same denominator as the model's VAT machinery).
  pi is ~0.70% and flat over the window. Benefits respond at pi_{t-1},
  so 2030 is zero.
- Social Security: per-record gross_ss * pi_lag. No CBO data needed; correct
  buckets and top tail automatically.
- SNAP + SSI: CBO Feb-2026 baseline outlays (SNAP from the program file; SSI
  from the Spending-Projections workbook, log-linear-smoothed to remove the
  11/12/13-payment-month FY artifacts; both extended to 2039), allocated by
  CBO's 2022 market-income-ranked shares: quintiles 1:1, Q5 sub-split by CBO's
  81-90 / 91-95 / 96-99 / top-1 detail, negative income pooled with Q1.
  Q1 gets 56.8% of SNAP and 62.0% of SSI.
- New rows in both output CSVs: cola_ss, cola_snap, cola_ssi, plus subtotals
  benefit_offset, all_excises_net, all_measures_net. Offset rows carry
  avg < 0 / pct_chg_ati > 0 so all metrics sum cleanly to net.

## Caveats

1. The SS leg gives ALL current beneficiaries the full cumulative uplift.
   New claimants' initial benefits are wage-indexed and shouldn't get the
   price catch-up, so the offset is modestly overstated. The model's
   do_ss_cola() (src/data/economy.R) handles claiming vintage properly if we
   ever want the refinement.
2. One overall pi for all programs, even though SNAP indexes to the Thrifty
   Food Plan (a carbon tax hits food less than energy). Footnote, not a fix.
3. The offset phases in with the COLA lag, so the 10-yr average understates
   the steady state.
4. The Negative-income group's pct flips sign (negative aggregate ATI
   denominator) - same quirk as the existing burden rows.
5. CBO's website now bot-blocks direct downloads; projections were pulled via
   the Wayback Machine (web.archive.org/web/2026id_/<url>).

## Files

- Script (modified):
  other/analysis_scripts/public/clausing_excise_distribution.R
- Outputs (rewritten):
  other/analysis_scripts/public/clausing_excise_distribution_avg_2030_2039.csv
  other/analysis_scripts/public/clausing_excise_distribution_by_year.csv
- New input:
  other/analysis_scripts/public/resources/cbo/households_ranked_by_market_inc_table_11_means_tested_transfer_shares_1979_2022.csv
- Run log:
  other/analysis_scripts/public/clausing_excise_distribution_17165384.out

(All paths relative to /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/)

## Pending

Nothing for this script - TS_VINTAGE already repointed to clausing_v2_s50 and
re-run (2026-07-06). The two chart scripts still need their vintage repoints.

## Sources

- CBO additional data for researchers:
  https://www.cbo.gov/system/files/2026-01/61911-additional-data-for-researchers.zip
- CBO Feb 2026 SNAP baseline (51312-2026-02-snap.xlsx, via Wayback)
- CBO Feb 2026 Spending Projections (51142-2026-02-Spending-Projections.xlsx,
  via Wayback; SSI = TIN 028-0406-0-1-609, mandatory row)

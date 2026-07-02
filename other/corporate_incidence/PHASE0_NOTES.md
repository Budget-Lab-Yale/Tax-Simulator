# Phase 0 notes — provenance recon (0b) + measurement status (0c)

_2026-07-02. Companion to CONSIDERATIONS.md §10 Phase 0 and the implementation plan._

## 0b. Off-Model-Estimates provenance

**Structure (verified on disk):** OME vintages are bare per-scenario `revenues.csv`
files (`year, individual, payroll, corporate, estate, vat`, $B) under
`/nfs/roberts/project/pi_nrs36/shared/model_data/Off-Model-Estimates/v4/{vintage}/{ID}/`.
No metadata, no producer scripts, no profit paths — receipts deltas only (consistent
with D2). No `Off-Model-Estimates` producer repo exists under
`~/Repositories` — vintages appear to be hand-assembled. **Open question for the
author/producer (0c crowdsource):** are corporate lines JCT-benchmarked? If yes, the
individual offset is embedded (Nunns) and the input is NOT gross-of-offset (D1); it
must be re-derived or grossed up before the channel can book its endogenous offset
without double-counting. Until answered: outputs state the bound (combined revenue
UNDERSTATED by the embedded offset, plausibly a few % of the corporate estimate).

**Live corporate scenario found:** `v4/20260609/07_corporate` — corporate deltas
+$336.7B (2030) growing ~3.9%/yr through 2039, zero after: a **windowed 10-year
corporate raise**. Once the channel exists, this is a real-world test of the
temporary-shock machinery (P3 annuity markdown shrinking to zero at expiry, D17
persistent dissaving). Its metadata file must declare `beyond_horizon: zero`.

**`buyback-tax` repo (v2 seed, noted only):** `src/calc.R` carries an explicit
payout-composition parameter `phi` ("share of distributions structured as buybacks")
and METR machinery by asset/financing. This is the seed for the v2 payout-SHIFT
behavior module (D12 deferral) and a source for payout-composition stylized facts.
Nothing needed from it for v1 (composition is data-embedded per D12).

## 0c. Measurement status for the CORP_* constants

| constant | central (placeholder) | source | status |
|---|---|---|---|
| κ (C-corp share of normal-capital stock) | 0.40; corners {0.25, 0.5} | Fed Z.1 (B.101/B.103); owner-occupied-housing fork sets corners | EXTERNAL — needs Z.1 pull |
| ω_kg (C-corp equity share of realized LTCG) | 0.50 | SOI sale-of-capital-assets | EXTERNAL — needs SOI table |
| ω_div (C-corp share of dividends) | 0.85 | exclude REIT/bond-fund distributions; ICI/SOI | EXTERNAL |
| ω_dc / ω_trusts / ω_re_fund (equity shares) | 0.55 / 0.50 / 0.30 | SCF + ICI equity-share imputations | EXTERNAL |
| θ (US-taxable exposure scale) | TBD | Rosenthal–Austin update + Z.1 | EXTERNAL |
| θ_res (foreign/nonprofit/DB residual) | TBD | complements of the above + on-disk value.db aggregate (stakes memo sizes the DB slice) | PARTIAL on-disk |
| σ_N (normal-return share) | 0.375; corners {0, 0.5} | OTA 63%/TPC 60% supernormal; house VAT 50% normal = upper corner | LITERATURE — settled |
| π_t (after-tax profit path) | `gdp_corp − rev_corp` | Macro-Projections (verified both series exist) | ON-DISK — settled |
| r (equity discount) | nominal tsy_10y + ERP constant | Macro-Projections + literature ERP | LITERATURE |
| δ (vintaging) | 0.057 | NIPA, same as `do_capital_adjustment` | SETTLED (house) |

On-disk measurements ride in the stakes memo job (`stakes_memo/out/`): household
equity exposure aggregates, the value.db (D10 residual) slice, taxable-estate
portfolio composition (D15 direction). The EXTERNAL rows need data pulls the author
may already have on hand; the channel builds against the placeholders (hardcoded,
provenance-commented) and the constants are updated in code when measured.

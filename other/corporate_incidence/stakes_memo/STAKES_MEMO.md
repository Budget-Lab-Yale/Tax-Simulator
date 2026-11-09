# Corporate incidence Phase 0a — stakes memo

_2026-07-02. Decision gate for the on-model corporate incidence channel
(CONSIDERATIONS.md §10 Phase 0a; implementation plan). All numbers are the NAIVE
CEILING: permanent 21→28 rate hike, markdown μ = Δτ/(1−τ) = 8.86%, full US-taxable
exposure (θ = 1), no anticipated-migration discount, placeholder equity shares
(see `out/assumptions.csv`). Data: full-sample 30-year baseline `warren_nd_30yr`
(2026–2055, Tax-Data 2026060918; predates the 2026-07 calc fixes — immaterial at
this precision). Producer: `stakes_memo.R`, SLURM job 17001458. CY basis._

## Gate question: are the stock-side effects material? — YES

**1. The capitalization hit is enormous and top-concentrated.** Household equity
exposure (direct + imputed fund/DC/trust shares) is **$62.8T** in 2026; the naive
one-time markdown is **$5.57T**. 86% of the hit lands above the 90th net-worth
percentile (p90–99: 41.5%, p99–99.9: 24.5%, top 0.1%: 20.4%; mean top-0.1% hit
≈ $6.1M/household). The DB slice — sized only for the D10 residual, never debited
on records — is $268B (≈5% of the household hit).

**2. The conventional flow offset is small next to the stock effects.** The
year-one taxable-flow offset ceiling (dividends + realized gains at effective
rates) is **$12.6B against a $134.7B receipts wedge ≈ 9.3¢ per dollar** — and
that is a deliberate overstatement (placeholder ω_div = 0.85, full θ). The honest
JCT-invisible offset is single-digit cents; the interesting economics is elsewhere.

**3. The un-scored cross-base revenue interactions are material.**
- **Estate tax:** the markdown erodes expected estate-tax revenue by **$2.0B/yr
  (2026) rising to $6.8B/yr (2055)** — cumulative **$26.5B over 10y, $127B over
  30y**. Nobody currently scores any of this.
- **Wealth tax (scenario-contingent):** under the warren scenario's law, the same
  hike erodes wealth-tax revenue by **$23B/yr (2026) → $92B/yr (2055)** —
  **$287B/10y, $1.5T/30y**. When a wealth tax is on the table, corporate-rate
  interactions are first-order, not a refinement.
- **Deemed realization (kg):** not computed here (needs unrealized-gain state);
  direction is the same and the base is the equity-heavy top tail. Bound: the
  taxable-estate composition below puts ~41% of the deemed base in the markdown's
  path.

**4. The smear-defect finding is publishable standalone (review item 8d).**
Allocating a fixed $100B corporate capital burden by taxable capital FLOWS
(today's smear) vs by equity HOLDINGS:
- **141k top-1%-net-worth households with below-median capital flows get $0 under
  the smear** and $1.8B under holdings — the founder-with-no-dividends / IRA-rich
  retiree case, exactly as predicted.
- By income percentile: the top 0.1% bears **$44.5B under the smear vs $15.4B
  under holdings** (realized flows are far more top-concentrated than holdings);
  p50–99 bears $33B vs $63B. The smear substantially overstates top-tail burden
  concentration and understates the upper-middle (retirement-account) share.
- By net worth: the bottom half bears $3.1B under the smear vs $0.3B under
  holdings — flow-based allocation hands corporate burden to wealth-poor records
  with lumpy realizations.

**5. D15 estate-composition direction.** Taxable estates hold **41.3% corporate
equity exposure, 30.1% pass-through, 8.1% interest-bearing** — corporate-equity-
heavy, so the κ-split (D15) shifts long-run burden TOWARD the estates that pay
tax; the permanent dividend-compression leg matters for the estate story.

## Recommendation

Proceed to the v1 build. The channel's comparative advantage is confirmed where
predicted: stock-side bases (estate, wealth, deemed), not the near-term flow
offset. The two data items that most move the headline are θ (US-taxable
exposure scale — every number above scales with it) and the ω equity shares
(Phase 0c). The gross-vs-net OME question (0b) affects only the receipts total,
not the interaction story.

Outputs: `other/corporate_incidence/stakes_memo/out/*.csv`
(`series_30yr`, `markdown_by_nw_bin`, `flow_offset_ceiling`,
`smear_vs_holdings_by_{nw,income}`, `smear_defect_cases`,
`taxable_estate_composition`, `assumptions`).

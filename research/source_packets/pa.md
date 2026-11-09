# Pennsylvania State Source Packet

State: `PA`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-23`

> **Status note (as of 2026-07-23), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

Full research notes with per-year citations: [research/raw/pa_research_core.md](research/raw/pa_research_core.md)
(all headline parameters verified against the PA-40 instruction booklets
2017-2025 fetched from pa.gov, plus 72 P.S. 7302/7303/7304).

## Scope

- Tax years 2017-2035; parameters transcribed through TY2025 plus the enacted
  TY2025 Working Pennsylvanians Tax Credit; flat rate carried forward (in
  force unchanged since 2004).
- Resident PIT only. Act 32 local EITs and the Philadelphia wage tax are out
  of scope (documented).
- Major features: PA is the model's first OWN-BASE state (`st_agi.start_point
  = 0`) — eight gross income classes with class-level loss floors, flat
  3.07%, no deductions or exemptions, and Tax Forgiveness (Schedule SP) as
  the low-income mechanism.

## Primary sources

- PA-40 instruction booklets TY2017-TY2025 (stable URL pattern
  `pa.gov/.../pit/documents/{YYYY}/{YYYY}_pa-40in.pdf`), Schedules SP, O, A, DC.
- 72 P.S. 7302 (rate), 7303 (classes), 7304 (Tax Forgiveness).
- PA PIT Guide chapters (Tax Forgiveness, Gross Compensation); DOR CDCTC and
  Working Pennsylvanians Tax Credit pages.

## Parameter inventory

- `agi.yaml`: own-base start (new generic component), class shares for the
  eight classes (retirement/SS/UI/alimony = 0), class-level loss floor, and
  the other-state muni interest addition (25% of exempt interest under the
  model-wide own-state-share convention).
- `ded.yaml`: no deductions; Schedule O (529/MSA/HSA/ABLE, 2025 student loan
  interest) documented, not modeled.
- `exempt.yaml`: none.
- `ord.yaml`: flat 3.07%, all statuses, all years.
- `credits.yaml`: Tax Forgiveness as the new generic poverty-forgiveness
  family ($6,500/$13,000 + $9,500 per dependent, 10pp per $250 step-down,
  eligibility income = PA base + 75% of exempt interest + alimony); CDCTC
  enhancement (30% TY2022, 100% TY2023+, refundable); Working Pennsylvanians
  Tax Credit (10% of federal EITC, refundable, TY2025+).
- `filing.yaml`: file above $33 of PA taxable income, independent of the
  federal requirement.

## Generic components introduced

1. **Own-base income classes** (`st_agi.ob_*`): per-class shares over PUF
   concepts with an optional per-class zero floor (the no-cross-class-offset
   rule). Reusable for NJ/AL/AR/MS.
2. **Poverty-based forgiveness credit** (`st_credits.forgive_*`): share of
   pre-credit tax from a family-size income limit with a stepped decline;
   filing-status-mapped base, per-dependent add-on, and configurable
   nontaxable-income additions (exempt-interest share, alimony share).

## Worksheet tests (src/tests/state/test_state_calc.R, PA-1 .. PA-7b)

- Flat rate base case; class-level loss floors; retiree exclusions with 100%
  forgiveness; forgiveness step-down (80% cell verified against SP Table 1);
  forgiveness at the exact Table 2 limit with exempt-interest additions;
  CDCTC 100% vs 30% years; WPTC on/off across 2025/2024.

## Known differences

- **Wage base**: PA taxes 401(k)-type elective deferrals when made; PUF wages
  are federal Box 1 (excludes them) — PA compensation understated for
  deferring workers. All external models on PUF-type data share this gap.
- **Retirement**: all pension/IRA distributions treated as exempt; early
  distributions above basis are taxable but basis/1099-R codes are
  unobserved (slightly overstates the exemption).
- **Other-state muni interest**: taxable share of exempt interest unobserved;
  25% convention.
- **Capital gains**: PUF net gains embed federal carryovers (PA disallows);
  class floored at zero as an approximation.
- **Class/spousal netting**: PUF gives netted per-return schedule totals;
  class-level unit flooring approximates the per-spouse/per-activity rules
  (understates the base slightly).
- **Schedule O deductions omitted** (529/MSA/HSA/ABLE, 2025 student loan
  interest): contributions unobserved; overstates PA tax modestly.
- **Eligibility income unobservables**: gifts, insurance proceeds,
  inheritances, outside-household support, nontaxable military pay,
  educational assistance — omission OVERSTATES forgiveness. MFS units use
  own rather than combined-spouse income. Dependent-claimant forgiveness
  (only if parents qualify) not linked across units; dependent filers are
  ineligible in-model.
- **CDCTC base**: statute uses the tentative federal credit (Form 2441 line
  9a, pre-liability-cap); model applies the match to the limited federal
  credit — understates the PA credit for low-federal-liability filers.
- **`other_inc` proxy** for estates/trusts + gambling includes some
  non-PA-taxable items.
- **Local taxes out of scope**: PA-40 liability understates the total PA
  income-tax burden by roughly 1-4% of wages depending on locality.

## Cross-model validation notes

- TAXSIM window 2017-2020: whether TAXSIM models Tax Forgiveness is
  UNVERIFIED — probe low-income cases first (single 0-dep wages 6,000 →
  0 with forgiveness vs ~184 without; married 2-dep wages 31,000 → ~0 vs
  ~952). The 401(k) wage-base gap is shared by all models on PUF inputs.
- PolicyEngine 2021+: models the flat rate, exemptions, 529 deduction, Tax
  Forgiveness, the CDCTC enhancement (0.30/1.00 match verified in its
  parameters), and the 2025 WPTC (0.10). Expect divergence in
  eligibility-income construction and per-class loss flooring detail.

## Aggregate validation notes

- Blocked on Phase 1 weights. Compare with PA DOR PIT statistics; note the
  forgiveness overstatement direction when reading low-income stubs, and the
  deferral wage-base gap (biases liability down) in the middle.

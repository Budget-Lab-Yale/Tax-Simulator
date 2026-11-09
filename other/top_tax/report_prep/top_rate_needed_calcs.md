# Top-rate-needed calculations — "what would it take to close the deficit from the top"

*Computed 2026-07-18 for the top-tax report (Table 1 + the AGI-above-$1M envelope).
Source: `top_tax_dials_30y_v3` baseline, FY2027, static. All rates are federal.*

> **REFRAME (2026-07-30).** Table 1 in the report is now the calculator-measured
> version in the **"Taking all income above a threshold"** section below. Both
> framings above it are superseded: they rest on a composition-weighted marginal
> rate blended by hand from two MTR columns, and the new version measures the
> same quantity with two passes of the calculator instead. The rate-needed and
> 100%-ceiling tables are retained as the derivation, and their base columns
> agree with the new ones.
>
> **REFRAME (2026-07-22).** Rather than solving for the rate that closes the
> deficit (which yields impossible >100% rates), impose a 100% total federal rate
> and ask what share of GDP it raises. Same point, cleaner.

## The question

Even under the extreme (counterfactual) assumption that tax bases don't respond,
what tax rate on the top would be required to raise the **FY2027 deficit of $1.9T**
in a single year — holding everyone below the group's threshold harmless?

Two lenses:
- **Effective rate (ETR):** the average rate over the group's *entire* cash income.
  `needed ETR = current cash ETR + deficit / (group cash income)`.
- **Marginal rate (MTR):** a new top bracket at the group's threshold, applied only
  to income *above* it. `needed MTR = current marginal on the above-threshold slice
  + deficit / (base above threshold)`. Shown on two bases: ordinary taxable income
  (narrow) and AGI (broad, includes capital gains).

Groups and thresholds are by **cash (expanded) income**; the bracket sits at that
cash-income line and taxes the chosen base above it. Base above a threshold T =
`Σ weight · max(0, base_i − T)` over all units.

## Table 1 (FY2027, static, deficit = $1.9T)

| Group | Cash threshold | Cash ETR | Needed ETR | Needed MTR — ordinary | Needed MTR — AGI |
|---|---|---|---|---|---|
| Top 10%   | $232k   | 24.0% | 40%  | 93%    | 67%  |
| Top 5%    | $345k   | 24.6% | 46%  | 111%   | 77%  |
| Top 1%    | $911k   | 26.2% | 63%  | 166%   | 103% |
| Top 0.1%  | $4.24M  | 29.2% | 105% | 346%   | 164% |
| Top 0.01% | $21.39M | 30.0% | 189% | 1,086% | 322% |

Supporting detail (the "this much base, this much current tax" pieces):

| Group | Ordinary base > T | Cur. marginal (ord) | AGI base > T | Cur. marginal (AGI) |
|---|---|---|---|---|
| Top 10%   | $3.31T | 35% | $5.40T | 32% |
| Top 5%    | $2.56T | 37% | $4.26T | 32% |
| Top 1%    | $1.47T | 37% | $2.67T | 32% |
| Top 0.1%  | $0.61T | 36% | $1.42T | 30% |
| Top 0.01% | $0.18T | 37% | $0.65T | 28% |

Reading: the narrower the slice, the higher the required rate; ordinary income always
needs far more than AGI (top income is disproportionately capital gains, outside the
ordinary base). A rate above 100% is mathematically impossible.

## The 100% rate ceiling (PREFERRED FRAMING, added 2026-07-22)

The exact dual of Table 1, same machinery and same bases. Table 1 solves for the
rate that closes the deficit (`needed = current_marginal + deficit / base`);
here we fix the rate at **100%** and read off the revenue. A **total federal
marginal rate of 100%, inclusive of current law**, on income **above the group's
threshold** (nothing below), raises over current law

> `(1 − current marginal rate on the slice) × (base above threshold)`,

which is the Table-1 identity with `needed := 100%`. Naive upper bound: no
behavioral response, no state taxes. Two bases, exactly as Table 1: **ordinary
taxable income** (`txbl_inc − txbl_kg`) and **AGI**. Groups self-ranked by cash
income; the bracket sits at that cash floor. Base and current marginal reproduce
Table 1's supporting detail exactly.

**FY2027 GDP = $33.3T; deficit = $1.9T = 5.7% of GDP.**

| Group | Cash threshold | Ordinary base > T | Cur. mtr (ord) | **Ord. revenue %GDP** | AGI base > T | Cur. mtr (AGI) | **AGI revenue %GDP** |
|---|---|---|---|---|---|---|---|
| Top 10%   | $232k   | $3,308B | 35% | **6.4%** | $5,404B | 32% | **11.1%** |
| Top 5%    | $345k   | $2,560B | 37% | **4.9%** | $4,260B | 32% |  **8.6%** |
| Top 1%    | $911k   | $1,468B | 37% | **2.8%** | $2,675B | 32% |  **5.5%** |
| Top 0.1%  | $4.24M  |   $614B | 36% | **1.2%** | $1,416B | 30% |  **3.0%** |
| Top 0.01% | $21.39M |   $181B | 37% | **0.3%** |   $646B | 28% |  **1.4%** |

Reading (against the 5.7%-of-GDP deficit):
- Even a **100% total federal marginal rate — the government taking every dollar
  above the line, inclusive of tax already paid** — falls short of a single
  year's deficit for every group narrower than the top 5%, on either base. On
  the AGI base the top 1% raises 5.5% of GDP (just under one year's deficit); on
  ordinary income only 2.8%.
- The narrow groups are hopeless: 100% on the top 0.01% raises 0.3% of GDP on
  ordinary income, 1.4% on AGI.
- Ordinary income always raises far less than AGI — top income is
  disproportionately capital gains, outside the ordinary base.
- And this is the *static* ceiling; behavioral response (a 100% rate has an
  infinite ETI in the limit) collapses it toward zero.

Reproduce: `hundred_pct_ceiling.py` → `hundred_pct_ceiling.json`. Reads the
baseline static detail (`baseline/static/detail/2027.csv`) exactly as
`mtr_table_data.py` does. GDP from Macro-Projections v3 `2026022522` (`gdp_fy`,
2027).

## Taking all income above a threshold (TABLE 1, 2026-07-30)

The exercise the report now runs. The government takes every dollar of income
above a threshold and leaves tax on income below it exactly as current law sets
it. A unit's tax becomes

    tax_policy = tax(income capped at T) + (income - T)+

so revenue over current law is

    revenue = sum w * (income - T)+  +  sum w * tax(capped) - sum w * tax(current)

No rate is assumed anywhere. The tax currently collected on the above-threshold
slice is the difference between the two calculator passes, which is what the two
framings above approximate with a blended marginal rate. Federal individual
income tax plus payroll tax, FY2027, bases held fixed.

Four income concepts, each entering only through who is above the threshold and
how many dollars are above it:

| concept | definition |
|---|---|
| taxable income | AGI less deductions: what the rate schedule applies to |
| ordinary income | AGI less capital gains and qualified dividends |
| AGI | the broadest measure the current system counts |
| accrual income | expanded income with realized gains replaced by accruals, less the defined-contribution double count |

### Round thresholds

Revenue over current law, $B and % of FY2027 GDP ($33.3T). The FY2027 deficit is
$1.9T, 5.7% of GDP.

| Concept | $1M | | $5M | | $10M | |
|---|---|---|---|---|---|---|
| | $B | %GDP | $B | %GDP | $B | %GDP |
| Taxable income | 1,563 | 4.7% | 820 | 2.5% | 595 | 1.8% |
| Ordinary income | 939 | 2.8% | 358 | 1.1% | 206 | 0.6% |
| AGI | 1,779 | 5.3% | 930 | 2.8% | 668 | 2.0% |
| Accrual income | 5,800 | 17.4% | 2,419 | 7.3% | 1,626 | 4.9% |

Share of filers above each threshold, which is what "roughly the top X%" means
for each measure:

| Threshold | Taxable | Ordinary | AGI | Accrual | Cash |
|---|---|---|---|---|---|
| $1M | 0.51% | 0.45% | 0.59% | 1.93% | 0.75% |
| $5M | 0.063% | 0.045% | 0.070% | 0.165% | 0.076% |
| $10M | 0.021% | 0.014% | 0.023% | 0.062% | 0.025% |

A $1M threshold is roughly the top half-percent on every realized measure and
roughly the top two percent on accrual income.

### Each measure's own top shares

| Group | Taxable | Ordinary | AGI | Accrual |
|---|---|---|---|---|
| Top 10% | 11.2% | 8.9% | 12.2% | 31.3% |
| Top 5% | 8.9% | 6.7% | 9.7% | 24.8% |
| Top 1% | 5.6% | 3.8% | 6.2% | 13.7% |
| Top 0.1% | 3.0% | 1.6% | 3.3% | 5.8% |
| Top 0.01% | 1.4% | 0.5% | 1.5% | 3.0% |

Reading: on AGI, taking everything above the top-1% line raises 6.2% of GDP,
just over one year's deficit; on ordinary income the same line raises 3.8%. The
accrual column is two to three times the AGI column, and the implied current rate
on the slice is about 15% against AGI's 30%, because most of the appreciation the
accrual measure counts carries no current-year tax.

### Method notes

- Truncation scales each above-threshold record's positive income flows until
  that record's income sits on the threshold, holding composition fixed, and
  reprices the record under current law. Only positive flows scale: shrinking a
  loss would raise the concept rather than lower it, and the policy takes income
  away rather than handing out deductions. Solved by bisection, converged to
  within 0.13% on the worst single record and 1e-5 or better on most cases.
- Ordinary income subtracts every gain and qualified-dividend flow that enters
  AGI, not only the preferentially rated slice. Subtracting only the rated slice
  leaves the depreciation-recapture and collectibles gains inside the concept
  while holding them fixed, and then a fifth of the base at a $10M threshold
  cannot be truncated at all.
- On the accrual measure, replacing realized gains with accruals adds realized
  losses back, and losses do not scale. Between 0.5% and 2.6% of the accrual base
  therefore sits in records that cannot be brought down to the threshold; those
  records are held at every positive flow removed. The `unreachable_share` column
  reports it per case.
- Reproduce: `hundred_pct_bracket.R` (via `run_hundred_pct_bracket.sbatch`) ->
  `hundred_pct_bracket.csv` + `hundred_pct_bracket_placement.csv`;
  `build_hundred_pct_table.py` -> `hundred_pct_table.html`. Reads the
  `top_tax_dials_30y_v6` baseline through `reconstitute_environment`, so it runs
  the shipped calculator rather than a reimplementation of it.

## The AGI-above-$1M envelope (report intuition paragraph)

- ~1.12M tax units have AGI > $1M; combined AGI ≈ **$3.68T**.
- AGI **above** $1M (what a $1M-threshold bracket taxes) ≈ **$2.57T** (call it $2.6T).
- That slice is taxed at ~**32%** at the margin today (blend of 37% ordinary and
  ~24% capital gains) → ~**$0.81T** current tax.
- To raise a new **$1.9T**: surtax = 1.9 / 2.57 ≈ **74 points**.
- Total marginal rate = 32% + 74% ≈ **106%** — over 100%, i.e. impossible.
- One-line intuition: the entire taxable base above $1M ($2.6T) is only ~1.35× the
  single-year deficit ($1.9T).

## Threshold sensitivity (AGI base, top-1%-ish lines)

| Threshold | AGI above it | Surtax | Total marginal |
|---|---|---|---|
| $0.68M (top-1% by AGI)  | $3.06T | +62 | ~94%  |
| $0.91M (top-1% by cash, Table 1) | $2.67T | +71 | ~103% |
| $1.00M (round)          | $2.57T | +74 | ~106% |

## Caveats

- **Static** — bases held fixed; behavioral responses would push every rate higher.
- **FY2027 single year.** The 10-year version runs modestly higher because projected
  deficits outgrow incomes.
- Cash ETRs and thresholds are self-ranked baseline values (metrics §1–3); income
  levels/ETRs are unchanged v2→v3 (baseline is reform-invariant).
- "Ordinary taxable income" = `txbl_inc − txbl_kg` (taxable income net of the
  preferential-rate slice). AGI composite marginal = income-weighted blend of
  `mtr_wages1` (ordinary) and `mtr_kg_lt` (gains) by each unit's composition.

## Reproduce

All under `other/top_tax/report_prep/`:
- `mtr_table_data.py` → `mtr_table_data.json` (Table 1 + base/marginal detail)
- `build_mtr_table.py` → `mtr_deficit_table.html` (the published Table 1 artifact)
- `mtr_viz_data.py` → `mtr_viz_data.json` (per-percentile marginal-rate curve; feeds
  the earlier chart `build_mtr_fig.py` → `mtr_marginal_rate_fig.html`)

Reads per-record: `…/top_tax_dials_30y_v3/baseline/static/detail/2027.csv`
(cols: `weight, agi, txbl_inc, txbl_kg, mtr_wages1, mtr_kg_lt`). Deficit constant
`$1,900B` is FY2027 (CBO Feb 2026). Rerun the `.py` files on the login node (pure CSV
read, no R).

## Realization elasticity by death regime (RESOLVED 2026-07-18)

Realization elasticity = |voluntary realization semi-elasticity| × 0.238 (the
current-law top CG+NIIT rate), i.e. a tax-rate elasticity evaluated at the 23.8%
margin. "Voluntary" = during-life realizations only: from `kg_dynamics_summary.csv`,
`R_S_voluntary = R_B_total + rate_channel + lockin_channel` (i.e. R_S_total MINUS
`deemed_realized`), then `semi_elast = mean_yr[ log(R_S_voluntary / R_B_total) / dtau ]`,
dtau = `tau_S_avg_rw − tau_B_avg_rw`. **Excluding `deemed_realized` is the fix** — the
shipped `semi_elast_implied` bakes forced death-realization into R_S_total, which flips
the sign under deemed.

| Regime | Voluntary semi-elast (v3, cg 20→40%) | Realization elasticity (×0.238) |
|---|---|---|
| Current law (step-up) | −2.50 | **0.60** |
| Carryover basis       | −1.47 | **0.35** |
| Gains taxed at death  | −0.98 | **0.23** |

Monotonic fall, correct direction (less deferral value → less rate-responsive).
Report line: "…assume 0.6 under current law, falling to about **0.35** with carryover
basis and **0.23** with gains taxed at death."

Caveat on "at the margin": measured off the 20→40% arc (normalized to 23.8%), which is
the stable estimate. The literal 5-pp margin (cg 20→25) gives step-up 0.56, but
carryover/deemed are not reliably estimable at a 5-pp step — the muted voluntary response
is below the noise floor and the sign flips. Scenarios: `s_cg_r40` (step-up),
`prco_cg_deemed` (carryover at cg40), `pr_cg_deemed` (deemed at cg40). Compute:
`report_prep/` ad-hoc (voluntary semi-elast formula above).

## Vintage check on v5

Both framings were re-run against the `top_tax_dials_30y_v5` baseline on
2026-07-29 (`hundred_pct_ceiling.py` and `mtr_table_data.py` now read that
vintage). Every figure is identical to the v3 numbers above to three decimal
places: the 2027 baseline income distribution, the bases above each threshold
and the current-law marginal rates on those slices did not move between the two
vintages. The kg form change bears on behavioral response, which the table does
not use.

## Published artifacts

- Table 1: https://claude.ai/code/artifact/51070392-59ad-4c31-86f5-39df2860ed64
- Marginal-rate-by-percentile chart: https://claude.ai/code/artifact/bd75db2f-5b7b-4e19-a19b-09e6e34803c9

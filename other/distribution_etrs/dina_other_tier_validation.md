# Validation of the `other` ETR tier against DINA (2026-07-08)

**Question.** Our `distribution_etrs.csv` `other` tier (state+local+federal-excise ETR,
built from Tax-Data's imputed `dina_other_taxes_rate`) shows a steeply regressive
profile — ~24% of expanded income for the bottom quintile, falling to ~8% at the top,
producing a U-shaped all-in ETR. Is that bottom-quintile rate real, or an artifact of
our imputation / denominator?

**Answer.** It is **real and faithful to DINA**. Computed the same way in both datasets
(same taxes, same income concept, same zero/negative handling as `distribution_etrs`),
our imputed rate reproduces DINA's *actual* state+local+excise burden to within ~1pp at
every income group. The high bottom-quintile rate is what these consumption/property
taxes look like as a share of *market* income; it flattens to ~16% only if you switch
the denominator to pre-tax national income (`peinc`) — a legitimate choice of income
concept, not a correction.

## The imputation model (Tax-Data `src/imputations/state_local_tax.R`)

`dina_other_taxes_rate` is a **scale-free RATE** (not dollars), imputed per PUF unit by
a quantile forest:

```
rate = (ditas + salestax + proprestax) / income
     = (state income tax + sales/excise + residential property tax) / broad income
```

The denominator is **broad income = fiscal income (incl. capital gains) + Social
Security + UI**:
- DINA training side: `fiinc + ssinc_oa + ssinc_di + uiinc`
- Tax-Data/PUF side: `compute_broad_income()` (26-component sum; the concept our ETR
  reader reconstructs).

Stored `grow_with = NA`; design intent is to multiply by that same broad-income concept
(ratio-to-income aging). Rates >1 occur at the bottom and are floored at 0 / winsorized
at the weighted 99.5th pct upstream.

## Matched comparison — the closest analogue

State+local+excise ETR by income group, **both = (ditas+salestax+proprestax) ÷ broad
income**, 2019, using the exact `distribution_etrs` handling (`add_rank_groups`,
helpers.R:112): rank only the **nonnegative** population; negatives → a separate
"Negative income" bucket; numerator floors the base at `pmax(broad,0)`. Tax-unit
aggregation follows `state_local_tax.R`: split vars SUM, residential property tax FIRST.

| Group | DINA 2019 (actual) | Our Tax-Data (imputed) |
|---|---|---|
| Negative income | −2.7% | −0.0% |
| **Quintile 1** | **28.6%** | **28.0%** |
| Quintile 2 | 17.2% | 16.8% |
| Quintile 3 | 13.6% | 14.0% |
| Quintile 4 | 12.5% | 12.8% |
| Quintile 5 | 11.7% | 12.3% |
| Top 10% | 11.1% | 11.9% |
| Top 1% | 10.4% | 10.5% |
| Top 0.1% | 10.8% | 10.4% |
| ALL | 12.9% | 13.4% |

→ **Imputation validated.** Our chart's 24% (on `expanded`, a hair broader than `broad`)
is the same result; DINA-on-broad is 28.6% at Q1, we get 28.0%.

## Reference: DINA under other denominators (why the level moves)

Same taxes, different income concept (DINA 2019). The bottom is sane only once the
denominator broadens past market income:

| Denominator | Q1 "other" ETR | ALL "other" ETR |
|---|---|---|
| `fiinc` (fiscal income) | negative (Q1 income ≈ 0/neg) | ~14% |
| `broad` (fiinc + SS + UI) | 28.6% | 12.9% |
| `ptinc` (personal pre-tax) | 15.3% | 9.2% |
| `peinc` (pre-tax national income, PSZ) | 16.1% | 8.9% |

DINA **total** ETR (ALL taxes ÷ `peinc`) reproduces the PSZ macro number — 30.2% overall,
denominator = **$18.2T = US national income** — with a roughly flat profile (Q1 29.9% →
Q5 31.8% → Top 0.1% 36.4%). Composition at the bottom is almost entirely sales/excise +
payroll; at the top it's federal + state income tax.

### What's in each income concept
- **`fiinc`** = wages+pensions + business + rents + interest + dividends + capital gains
  (taxable market income on returns). Excludes SS/UI/transfers, employer payroll,
  tax-exempt interest, imputed rent, retained corporate earnings, in-kind. Near-zero or
  negative at the bottom → any tax ÷ it explodes.
- **`broad`** = `fiinc` + SS + UI. The rate's native base; ≈ our `expanded`.
- **`peinc`** = pre-tax national income (adds social insurance, government/nonprofit
  income allocations, imputed components); sums to national income. PSZ headline.

## Takeaway
- The `other` tier is a faithful on-model reproduction of DINA's own state+local+excise
  ETR by group. The high bottom-quintile rate (~24–28%) is genuine on a market-income
  base, not a bug or a denominator error.
- The choice of chart denominator is presentational: `broad`/`expanded` → ~28/24% bottom
  (matches DINA-on-broad); a `peinc`-style national-income denominator → ~16% bottom
  (matches PSZ). Both answer legitimate but different questions.

## Reproduction
Scripts + SLURM logs under `other/taxdata_interface_2026070814/`:
- `dina_vs_taxdata.R` — the matched validation table above (log `logs/dina_vs_td_17414733.out`, second block).
- `dina_other_rate.R` — DINA "other" rate by denominator (`broad`, `peinc`).
- `dina_etr_check.R` — full DINA ETR by group × denominator with tax-type composition
  (`total`, `other_bundle`, federal income, payroll, sales, property, state income, corp,
  estate); log `logs/dina_etr_17407358.out`.
- DINA source: `/nfs/roberts/project/pi_nrs36/shared/raw_data/DINA/v1/2023082913/historical/usdina2019.dta`.
- Our `other` tier source: `dist_etrs_other_v1/corp_perm/static/supplemental/distribution_etrs.csv`.

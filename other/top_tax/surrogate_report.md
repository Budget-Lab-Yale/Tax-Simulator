# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v5`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±3.0% on conventional DECADE-1 total; decades 2/3 measured+disclosed; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (t_cg_wealth_taxmax)
- `cy`: max |residual| 0.0000 (t_ord_wealth_taxmax)
- `ch`: max |residual| 0.0000 (pr_ord_wealth)
- `st`: max |residual| 0.0000 (t_wealth_qbi_taxmax)
- `sy`: max |residual| 0.0000 (t_ord_wealth_estate)
- `sh`: max |residual| 0.0000 (t_ord_cg_qbi)
- `etr`: max |residual| 0.0020 (t_ord_cg_qbi)
- `etrc`: max |residual| 0.0050 (t_ord_cg_taxmax)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | static d1 err % |
|---|---|---|---|---|---|---|
| pc_cgr50_deemed | 2188.7 | 2137.8 | -2.33 | -3.08 | -3.56 | -0.01 |
| pc_cgr30_wealthr3t500 | 2184.2 | 2166.7 | -0.80 | -0.75 | -0.76 | +0.00 |
| pc_cgr50_estater60e5 | 1232.9 | 1202.3 | -2.48 | -2.60 | -3.60 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4786.0 | 4758.6 | -0.57 | -1.03 | -1.63 | +0.00 |
| pc_wealthr1t1000_deemed | 1356.8 | 1355.4 | -0.11 | -0.00 | +0.14 | +0.00 |
| pc_ordr50_cgr30 | 2364.5 | 2337.8 | -1.13 | -0.98 | -0.88 | -0.00 |
| pc_ordr50_qbi | 2881.6 | 2890.8 | +0.32 | +0.31 | +0.28 | +0.00 |
| pc_corpr35_cgr30 | 2513.6 | 2525.0 | +0.45 | +0.39 | +0.38 | -0.00 |
| pc_cgr50_deemede5 | 1015.4 | 1000.3 | -1.49 | -2.23 | -2.81 | -0.00 |
| pc_wealthr2t50_deemedcoe1 | 3436.0 | 3435.5 | -0.01 | -0.01 | -0.01 | -0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4360.8 | -0.43 | +0.01 |
| q02 | 8533.0 | -0.87 | -0.25 |
| q03 | 1409.8 | +0.95 | +2.37 |
| q04 | 7857.8 | -0.56 | +0.38 |
| q05 | 9505.0 | -3.32 | -3.15 |
| q06 | 7319.1 | -0.26 | +0.48 |
| q08 | 10108.2 | -1.35 | +0.07 |
| q09 | 7687.0 | +1.49 | +2.13 |
| q10 | 7148.2 | -0.84 | -0.68 |
| q11 | 6062.8 | -0.78 | -0.33 |
| q12 | 9241.7 | -3.70 | -2.23 |
| q13 | 4987.8 | -3.15 | -1.96 |
| q14 | 9937.0 | -0.64 | +0.79 |
| q15 | 7826.5 | -0.47 | +0.32 |
| q18 | 6371.7 | -0.90 | -0.89 |
| stack_ref | 11041.8 | -0.52 | +1.28 |

Triples improve 5/16 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4360.8 | 4342.1 | -0.43 | +0.03 | +0.62 | 1.00 | 70.87 | 0.186 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8533.0 | 8459.1 | -0.87 | -0.63 | -0.51 | 1.09 | 86.24 | 0.974 |
| q03 | cg+deemed+estate | 1409.8 | 1423.2 | +0.95 | +1.16 | +1.30 | 1.67 | 36.56 | 0.061 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7857.8 | 7813.6 | -0.56 | -0.11 | +0.28 | 0.74 | 62.27 | 0.464 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 9505.0 | 9189.3 | -3.32 | -3.70 | -4.42 | 5.19 | 368.23 | 1.649 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 7319.1 | 7300.0 | -0.26 | -0.27 | -0.51 | 0.79 | 47.73 | 0.648 |
| q07 | deemed+ord+taxmax | 3424.9 | 3421.8 | -0.09 | -0.08 | -0.08 | 0.11 | 9.33 | 0.057 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10108.2 | 9971.8 | -1.35 | -1.04 | -0.78 | 1.79 | 127.24 | 2.134 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7687.0 | 7801.5 | +1.49 | +1.71 | +1.10 | 1.84 | 98.87 | 0.765 |
| q10 | cg+deemed+ord+taxmax+wealth | 7148.2 | 7087.8 | -0.84 | -0.56 | -0.29 | 1.51 | 79.50 | 0.455 |
| q11 | cg+deemed+taxmax+wealth | 6062.8 | 6015.4 | -0.78 | -0.67 | -0.63 | 1.23 | 76.62 | 0.531 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 9241.7 | 8899.8 | -3.70 | -4.18 | -4.73 | 5.24 | 380.01 | 1.006 |
| q13 | cg+corp+deemed+qbi+wealth | 4987.8 | 4830.7 | -3.15 | -3.14 | -3.12 | 4.42 | 171.62 | 0.780 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 9937.0 | 9873.2 | -0.64 | -2.30 | -4.92 | 5.91 | 689.64 | 3.025 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax | 7826.5 | 7789.9 | -0.47 | -0.42 | -0.48 | 0.86 | 57.78 | 0.682 |
| q16 | cg+corp+deemed | 3265.3 | 3285.6 | +0.62 | +0.79 | +0.83 | 0.92 | 49.23 | 0.601 |
| q17 | deemed+taxmax+wealth | 4874.0 | 4887.1 | +0.27 | +0.26 | +0.17 | 0.94 | 68.50 | 0.122 |
| q18 | deemed+estate+qbi+taxmax+wealth | 6371.7 | 6314.6 | -0.90 | -1.51 | -2.18 | 2.42 | 230.37 | 0.077 |
| q19 | cg+corp+deemed+ord | 3222.3 | 3223.8 | +0.05 | -0.08 | -0.07 | 0.32 | 10.35 | 0.718 |
| q20 | deemed+ord+taxmax | 3949.8 | 3956.8 | +0.18 | +0.19 | +0.21 | 0.52 | 21.22 | 0.034 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 11041.8 | 10984.3 | -0.52 | -0.67 | -0.85 | 1.09 | 115.62 | 0.413 |

**Holdout bounds (max |err|, per decade): conv ±3.7% / ±4.2% / ±5.0% (median d1 quiz 0.64%); static ±1.9% / ±1.1% / ±0.7%**

## Warnings
- triples improve only 5/16 cluster-heavy holdouts
- q01: ETR cell err 0.186pp > 0.1pp
- q02: ETR cell err 0.974pp > 0.1pp
- q04: ETR cell err 0.464pp > 0.1pp
- q05: ETR cell err 1.649pp > 0.1pp
- q06: ETR cell err 0.648pp > 0.1pp
- q08: ETR cell err 2.134pp > 0.1pp
- q09: ETR cell err 0.765pp > 0.1pp
- q10: ETR cell err 0.455pp > 0.1pp
- q11: ETR cell err 0.531pp > 0.1pp
- q12: ETR cell err 1.006pp > 0.1pp
- q13: ETR cell err 0.780pp > 0.1pp
- q14: ETR cell err 3.025pp > 0.1pp
- q15: ETR cell err 0.682pp > 0.1pp
- q16: ETR cell err 0.601pp > 0.1pp
- q17: ETR cell err 0.122pp > 0.1pp
- q19: ETR cell err 0.718pp > 0.1pp
- stack_ref: ETR cell err 0.413pp > 0.1pp

## HARD FAILURES
- quiz: q05 conv d1 err -3.32% (levers cg+corp+deemed+estate+ord+qbi+taxmax+wealth)
- quiz: q12 conv d1 err -3.70% (levers cg+corp+deemed+estate+ord+qbi+taxmax+wealth)
- quiz: q13 conv d1 err -3.15% (levers cg+corp+deemed+qbi+wealth)

Patch loop: add targeted anchors/corners to levers.PATCHES, run build_dial_runs.py --patch, run the patch batch into the same vintage, refit, revalidate.

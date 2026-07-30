# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v6`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±3.0% on conventional DECADE-1 total; decades 2/3 measured+disclosed; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (t_corp_wealth_estate)
- `cy`: max |residual| 0.0000 (t_ord_cg_wealth)
- `ch`: max |residual| 0.0000 (t_ord_wealth_estate)
- `mt`: max |residual| 0.0000 (t_ord_corp_wealth)
- `my`: max |residual| 0.0000 (t_corp_wealth_taxmax)
- `mh`: max |residual| 0.0000 (pr_cg_wealth)
- `st`: max |residual| 0.0000 (pr_ord_wealth)
- `sy`: max |residual| 0.0000 (t_ord_wealth_estate)
- `sh`: max |residual| 0.0000 (t_ord_cg_qbi)
- `etr`: max |residual| 0.0050 (t_cg_qbi_taxmax)
- `etrc`: max |residual| 0.0050 (t_corp_estate_taxmax)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | static d1 err % |
|---|---|---|---|---|---|---|
| pc_cgr50_deemed | 2214.1 | 2164.6 | -2.24 | -2.88 | -3.24 | -0.01 |
| pc_cgr30_wealthr3t500 | 2178.7 | 2166.4 | -0.57 | -0.45 | -0.35 | +0.00 |
| pc_cgr50_estater60e5 | 1279.3 | 1256.9 | -1.75 | -1.65 | -2.23 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4794.1 | 4765.8 | -0.59 | -1.08 | -1.55 | +0.00 |
| pc_wealthr1t1000_deemed | 1349.5 | 1347.7 | -0.13 | -0.04 | +0.07 | +0.00 |
| pc_ordr50_cgr30 | 2378.0 | 2349.2 | -1.21 | -1.09 | -1.06 | -0.00 |
| pc_ordr50_qbi | 2881.0 | 2889.7 | +0.30 | +0.26 | +0.20 | +0.00 |
| pc_corpr35_cgr30 | 2532.8 | 2543.0 | +0.40 | +0.37 | +0.22 | -0.00 |
| pc_cgr50_deemede5 | 1055.6 | 1040.9 | -1.39 | -2.02 | -2.46 | -0.00 |
| pc_wealthr2t50_deemedcoe1 | 3431.8 | 3431.4 | -0.01 | -0.02 | +0.01 | -0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4354.0 | -0.42 | +0.04 |
| q02 | 8521.7 | -0.74 | -0.40 |
| q03 | 1407.5 | +0.90 | +2.36 |
| q04 | 7832.5 | -0.58 | +0.36 |
| q05 | 9419.3 | -2.31 | -2.56 |
| q06 | 7331.8 | +0.23 | +0.27 |
| q08 | 10098.3 | -0.92 | -0.01 |
| q09 | 7670.0 | +1.40 | +1.98 |
| q10 | 7114.5 | -0.84 | -0.74 |
| q11 | 6040.7 | -1.02 | -0.47 |
| q12 | 9222.5 | -3.65 | -2.29 |
| q13 | 5003.2 | -3.31 | -2.03 |
| q14 | 9954.8 | -0.92 | +0.28 |
| q15 | 7819.3 | -0.24 | +0.21 |
| q18 | 6331.4 | -0.87 | -0.91 |
| stack_ref | 11012.7 | -0.23 | +1.32 |

Triples improve 6/16 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4354.0 | 4335.8 | -0.42 | +0.00 | +0.52 | 0.92 | 68.32 | 0.183 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8521.7 | 8458.4 | -0.74 | -0.74 | -0.40 | 1.51 | 111.51 | 0.997 |
| q03 | cg+deemed+estate | 1407.5 | 1420.2 | +0.90 | +1.09 | +1.16 | 1.48 | 33.49 | 0.062 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7832.5 | 7787.3 | -0.58 | -0.34 | +0.06 | 0.72 | 63.15 | 0.479 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 9419.3 | 9202.0 | -2.31 | -3.76 | -4.33 | 5.58 | 444.82 | 0.874 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 7331.8 | 7348.8 | +0.23 | -0.29 | +0.02 | 1.99 | 120.50 | 0.622 |
| q07 | deemed+ord+taxmax | 3398.4 | 3396.0 | -0.07 | -0.07 | -0.16 | 0.75 | 8.54 | 0.053 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10098.3 | 10005.2 | -0.92 | -1.39 | -0.85 | 2.68 | 201.56 | 2.217 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7670.0 | 7777.1 | +1.40 | +1.55 | +1.03 | 1.79 | 98.73 | 0.803 |
| q10 | cg+deemed+ord+taxmax+wealth | 7114.5 | 7054.9 | -0.84 | -0.57 | -0.26 | 1.36 | 75.80 | 0.442 |
| q11 | cg+deemed+taxmax+wealth | 6040.7 | 5979.0 | -1.02 | -0.79 | -0.97 | 1.47 | 101.32 | 0.629 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 9222.5 | 8885.8 | -3.65 | -4.42 | -4.87 | 5.40 | 380.82 | 1.048 |
| q13 | cg+corp+deemed+qbi+wealth | 5003.2 | 4837.4 | -3.31 | -3.03 | -3.03 | 4.63 | 168.72 | 0.780 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 9954.8 | 9862.8 | -0.92 | -3.40 | -3.82 | 4.88 | 574.19 | 3.016 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax | 7819.3 | 7800.4 | -0.24 | -0.66 | -0.46 | 1.67 | 94.18 | 0.725 |
| q16 | cg+corp+deemed | 3268.6 | 3287.1 | +0.57 | +0.73 | +0.69 | 0.95 | 42.05 | 0.601 |
| q17 | deemed+taxmax+wealth | 4854.7 | 4852.4 | -0.05 | +0.02 | -0.28 | 0.74 | 86.64 | 0.122 |
| q18 | deemed+estate+qbi+taxmax+wealth | 6331.4 | 6276.4 | -0.87 | -1.63 | -2.41 | 2.96 | 288.06 | 0.077 |
| q19 | cg+corp+deemed+ord | 3246.3 | 3247.3 | +0.03 | -0.06 | -0.02 | 0.56 | 10.66 | 0.718 |
| q20 | deemed+ord+taxmax | 3923.7 | 3930.6 | +0.18 | +0.21 | +0.18 | 0.38 | 20.21 | 0.032 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 11012.7 | 10987.7 | -0.23 | -0.81 | -1.00 | 1.90 | 128.80 | 0.780 |

**Holdout bounds (max |err|, per decade): conv ±3.7% / ±4.5% / ±4.9% (median d1 quiz 0.74%); static ±1.8% / ±1.1% / ±0.6%**

## Warnings
- triples improve only 6/16 cluster-heavy holdouts
- q01: ETR cell err 0.183pp > 0.1pp
- q02: ETR cell err 0.997pp > 0.1pp
- q04: ETR cell err 0.479pp > 0.1pp
- q05: ETR cell err 0.874pp > 0.1pp
- q06: ETR cell err 0.622pp > 0.1pp
- q08: ETR cell err 2.217pp > 0.1pp
- q09: ETR cell err 0.803pp > 0.1pp
- q10: ETR cell err 0.442pp > 0.1pp
- q11: ETR cell err 0.629pp > 0.1pp
- q12: ETR cell err 1.048pp > 0.1pp
- q13: ETR cell err 0.780pp > 0.1pp
- q14: ETR cell err 3.016pp > 0.1pp
- q15: ETR cell err 0.725pp > 0.1pp
- q16: ETR cell err 0.601pp > 0.1pp
- q17: ETR cell err 0.122pp > 0.1pp
- q19: ETR cell err 0.718pp > 0.1pp
- stack_ref: ETR cell err 0.780pp > 0.1pp

## HARD FAILURES
- quiz: q12 conv d1 err -3.65% (levers cg+corp+deemed+estate+ord+qbi+taxmax+wealth)
- quiz: q13 conv d1 err -3.31% (levers cg+corp+deemed+qbi+wealth)

Patch loop: add targeted anchors/corners to levers.PATCHES, run build_dial_runs.py --patch, run the patch batch into the same vintage, refit, revalidate.

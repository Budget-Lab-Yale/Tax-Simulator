# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±3.0% on conventional DECADE-1 total; decades 2/3 measured+disclosed; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (t_ord_wealth_taxmax)
- `cy`: max |residual| 0.0000 (t_ord_wealth_deemed)
- `ch`: max |residual| 0.0000 (t_ord_wealth_qbi)
- `st`: max |residual| 0.0000 (t_wealth_qbi_taxmax)
- `sy`: max |residual| 0.0000 (t_ord_corp_wealth)
- `sh`: max |residual| 0.0000 (t_ord_cg_qbi)
- `etr`: max |residual| 0.0030 (t_ord_cg_corp)
- `etrc`: max |residual| 0.0050 (t_wealth_deemed_taxmax)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | static d1 err % |
|---|---|---|---|---|---|---|
| pc_cgr50_deemed | 2248.1 | 2274.0 | +1.15 | +0.46 | +0.20 | -0.01 |
| pc_cgr30_wealthr3t500 | 2180.5 | 2159.0 | -0.99 | -0.90 | -0.87 | +0.00 |
| pc_cgr50_estater60e5 | 1474.5 | 1469.2 | -0.36 | -0.59 | -1.09 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4802.0 | 4774.7 | -0.57 | -1.02 | -1.63 | +0.00 |
| pc_wealthr1t1000_deemed | 1380.0 | 1378.3 | -0.12 | -0.02 | +0.12 | +0.00 |
| pc_ordr50_cgr30 | 2415.3 | 2388.9 | -1.09 | -0.97 | -0.88 | -0.00 |
| pc_ordr50_qbi | 2932.2 | 2941.1 | +0.30 | +0.29 | +0.27 | +0.00 |
| pc_corpr28_cgr30 | 1104.2 | 1105.6 | +0.12 | +0.16 | +0.16 | +0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4374.6 | -1.00 | -0.75 |
| q02 | 8857.5 | -1.22 | -0.76 |
| q03 | 1404.7 | -0.67 | +0.34 |
| q04 | 7845.1 | -0.78 | -0.21 |
| q05 | 8725.8 | -1.73 | -1.85 |
| q06 | 6129.0 | -0.12 | -0.19 |
| q08 | 8827.0 | -0.65 | +0.11 |
| q09 | 7719.1 | -1.66 | -1.46 |
| q10 | 7046.0 | -0.47 | -0.46 |
| q11 | 6031.2 | -1.20 | -1.04 |
| q12 | 6796.4 | -0.31 | -0.19 |
| q13 | 4893.8 | -1.00 | -0.94 |
| q14 | 8857.9 | -2.84 | -2.27 |
| q15 | 8355.3 | -1.89 | -1.14 |
| stack_ref | 10583.4 | -0.45 | +0.63 |

Triples improve 3/15 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4374.6 | 4330.7 | -1.00 | -0.51 | +0.06 | 1.21 | 39.03 | 0.301 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8857.5 | 8749.9 | -1.22 | -0.96 | -0.62 | 1.72 | 108.55 | 0.831 |
| q03 | cg+deemed+estate | 1404.7 | 1395.3 | -0.67 | -0.40 | -0.30 | 1.26 | 5.77 | 0.111 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7845.1 | 7783.8 | -0.78 | -0.30 | +0.09 | 0.96 | 38.03 | 0.535 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8725.8 | 8574.8 | -1.73 | -1.74 | -1.95 | 2.41 | 205.56 | 0.752 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 6129.0 | 6121.7 | -0.12 | -0.05 | -0.16 | 0.41 | 38.49 | 0.372 |
| q07 | corp+estate+ord+qbi+wealth | 5118.5 | 5179.1 | +1.18 | +1.35 | +0.62 | 1.56 | 58.52 | 0.329 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8827.0 | 8769.2 | -0.65 | -0.31 | +0.04 | 1.14 | 86.54 | 0.533 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7719.1 | 7590.8 | -1.66 | -2.21 | -2.76 | 3.01 | 291.65 | 1.153 |
| q10 | corp+deemed+estate+ord+qbi+taxmax+wealth | 7046.0 | 7012.8 | -0.47 | -0.30 | -0.19 | 0.60 | 46.67 | 0.172 |
| q11 | cg+deemed+taxmax+wealth | 6031.2 | 5958.9 | -1.20 | -1.07 | -1.05 | 1.98 | 76.44 | 0.631 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 6796.4 | 6775.2 | -0.31 | -0.18 | -0.17 | 0.77 | 66.38 | 0.106 |
| q13 | corp+deemed+estate+qbi+wealth | 4893.8 | 4844.9 | -1.00 | -0.97 | -0.98 | 1.11 | 106.45 | 0.257 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 8857.9 | 8606.0 | -2.84 | -3.02 | -3.75 | 4.52 | 313.11 | 1.660 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8355.3 | 8197.3 | -1.89 | -2.09 | -2.36 | 2.64 | 175.98 | 0.953 |
| q16 | corp+deemed+qbi+taxmax+wealth | 5673.1 | 5654.7 | -0.32 | -0.22 | -0.00 | 0.37 | 12.14 | 0.098 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10583.4 | 10535.4 | -0.45 | -0.59 | -0.73 | 1.22 | 106.53 | 0.428 |

**Holdout bounds (max |err|, per decade): conv ±2.9% / ±3.1% / ±3.8% (median d1 quiz 1.00%); static ±1.5% / ±1.0% / ±0.9%**

## Warnings
- triples improve only 3/15 cluster-heavy holdouts
- q01: ETR cell err 0.301pp > 0.1pp
- q02: ETR cell err 0.831pp > 0.1pp
- q03: ETR cell err 0.111pp > 0.1pp
- q04: ETR cell err 0.535pp > 0.1pp
- q05: ETR cell err 0.752pp > 0.1pp
- q06: ETR cell err 0.372pp > 0.1pp
- q07: ETR cell err 0.329pp > 0.1pp
- q08: ETR cell err 0.533pp > 0.1pp
- q09: ETR cell err 1.153pp > 0.1pp
- q10: ETR cell err 0.172pp > 0.1pp
- q11: ETR cell err 0.631pp > 0.1pp
- q12: ETR cell err 0.106pp > 0.1pp
- q13: ETR cell err 0.257pp > 0.1pp
- q14: ETR cell err 1.660pp > 0.1pp
- q15: ETR cell err 0.953pp > 0.1pp
- stack_ref: ETR cell err 0.428pp > 0.1pp

**ALL CHECKS PASS.**

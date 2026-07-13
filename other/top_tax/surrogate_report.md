# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v2`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±2.6% on conventional DECADE-1 total; decades 2/3 measured+disclosed; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (pr_wealth_taxmax)
- `cy`: max |residual| 0.0000 (t_ord_cg_wealth)
- `ch`: max |residual| 0.0000 (t_ord_corp_wealth)
- `st`: max |residual| 0.0000 (t_wealth_qbi_taxmax)
- `sy`: max |residual| 0.0000 (t_ord_corp_wealth)
- `sh`: max |residual| 0.0000 (t_ord_cg_deemed)
- `etr`: max |residual| 0.0040 (t_cg_corp_qbi)
- `etrc`: max |residual| 0.0050 (t_wealth_deemed_taxmax)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | static d1 err % |
|---|---|---|---|---|---|---|
| pc_cgr50_deemed | 1864.6 | 1855.3 | -0.50 | -0.80 | -0.95 | -0.22 |
| pc_cgr30_wealthr3t500 | 2180.2 | 2157.2 | -1.06 | -0.99 | -1.00 | +0.00 |
| pc_cgr50_estater60e5 | 1542.4 | 1529.1 | -0.86 | -0.83 | -1.05 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4802.0 | 4685.7 | -2.42 | -3.38 | -4.40 | +0.00 |
| pc_wealthr1t1000_deemed | 1380.0 | 1377.4 | -0.19 | -0.08 | +0.07 | +0.00 |
| pc_ordr50_cgr30 | 2416.9 | 2415.3 | -0.07 | +0.30 | +0.29 | +7.42 |
| pc_ordr50_qbi | 2934.1 | 2942.9 | +0.30 | +0.29 | +0.27 | +0.00 |
| pc_corpr28_cgr30 | 1188.5 | 1189.2 | +0.06 | +0.04 | +0.03 | +0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4374.6 | +0.39 | +0.22 |
| q02 | 8897.1 | -0.31 | -0.90 |
| q03 | 1435.2 | -0.35 | +0.67 |
| q04 | 7839.3 | +0.63 | +0.51 |
| q05 | 8771.5 | -2.16 | -2.64 |
| q06 | 6184.2 | +0.36 | +0.25 |
| q08 | 8866.4 | -0.22 | -0.73 |
| q09 | 7816.6 | -1.34 | -1.28 |
| q10 | 7064.8 | +0.06 | +0.14 |
| q11 | 6026.5 | -1.14 | -0.96 |
| q12 | 6798.8 | -0.50 | -0.66 |
| q13 | 4971.7 | -0.82 | -0.83 |
| q14 | 8924.0 | -1.36 | -1.89 |
| q15 | 8417.9 | -1.35 | -0.96 |
| stack_ref | 10642.9 | -0.14 | -0.15 |

Triples improve 9/15 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4374.6 | 4391.8 | +0.39 | +1.05 | +1.60 | 1.96 | 115.26 | 0.433 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8897.1 | 8869.4 | -0.31 | +0.09 | +0.35 | 0.69 | 157.43 | 0.536 |
| q03 | cg+deemed+estate | 1435.2 | 1430.1 | -0.35 | -0.06 | +0.04 | 0.99 | 28.02 | 0.416 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7839.3 | 7888.7 | +0.63 | +1.26 | +1.59 | 1.73 | 285.58 | 0.773 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8771.5 | 8582.4 | -2.16 | -2.10 | -2.26 | 2.67 | 763.57 | 1.702 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 6184.2 | 6206.6 | +0.36 | +0.44 | +0.25 | 0.62 | 174.68 | 0.596 |
| q07 | corp+estate+ord+qbi+wealth | 5137.6 | 5267.6 | +2.53 | +2.91 | +2.18 | 3.09 | 213.12 | 0.357 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8866.4 | 8846.7 | -0.22 | +0.32 | +0.66 | 1.23 | 314.20 | 0.780 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7816.6 | 7711.7 | -1.34 | -1.78 | -2.37 | 2.66 | 282.50 | 1.365 |
| q10 | corp+deemed+estate+ord+qbi+taxmax+wealth | 7064.8 | 7069.2 | +0.06 | +0.33 | +0.41 | 0.54 | 87.69 | 0.172 |
| q11 | cg+deemed+taxmax+wealth | 6026.5 | 5958.0 | -1.14 | -0.98 | -1.01 | 1.49 | 80.59 | 0.568 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 6798.8 | 6765.0 | -0.50 | -0.26 | -0.29 | 0.81 | 622.79 | 1.639 |
| q13 | corp+deemed+estate+qbi+wealth | 4971.7 | 4930.9 | -0.82 | -0.74 | -0.74 | 0.88 | 103.75 | 0.257 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 8924.0 | 8802.9 | -1.36 | -1.48 | -2.28 | 3.07 | 348.69 | 1.322 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8417.9 | 8304.0 | -1.35 | -1.52 | -1.81 | 2.11 | 175.84 | 0.985 |
| q16 | corp+deemed+qbi+taxmax+wealth | 5739.0 | 5723.0 | -0.28 | -0.18 | +0.01 | 0.32 | 13.01 | 0.098 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10642.9 | 10628.0 | -0.14 | -0.18 | -0.29 | 0.82 | 40.18 | 0.268 |

**Holdout bounds (max |err|, per decade): conv ±2.6% / ±3.4% / ±4.4% (median d1 quiz 0.50%); static ±7.5% / ±6.8% / ±6.5%**

## Warnings
- q01: ETR cell err 0.433pp > 0.1pp
- q02: ETR cell err 0.536pp > 0.1pp
- q03: ETR cell err 0.416pp > 0.1pp
- q04: ETR cell err 0.773pp > 0.1pp
- q05: ETR cell err 1.702pp > 0.1pp
- q06: ETR cell err 0.596pp > 0.1pp
- q07: ETR cell err 0.357pp > 0.1pp
- q08: ETR cell err 0.780pp > 0.1pp
- q09: ETR cell err 1.365pp > 0.1pp
- q10: ETR cell err 0.172pp > 0.1pp
- q11: ETR cell err 0.568pp > 0.1pp
- q12: ETR cell err 1.639pp > 0.1pp
- q13: ETR cell err 0.257pp > 0.1pp
- q14: ETR cell err 1.322pp > 0.1pp
- q15: ETR cell err 0.985pp > 0.1pp
- stack_ref: ETR cell err 0.268pp > 0.1pp

**ALL CHECKS PASS.**

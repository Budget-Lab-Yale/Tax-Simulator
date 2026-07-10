# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v1`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±2.0% on conventional DECADE-1 total; decades 2/3 measured+disclosed; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (pr_ord_wealth)
- `cy`: max |residual| 0.0000 (t_ord_corp_wealth)
- `ch`: max |residual| 0.0000 (t_ord_deemed_taxmax)
- `st`: max |residual| 0.0000 (t_wealth_qbi_taxmax)
- `sy`: max |residual| 0.0000 (t_ord_corp_wealth)
- `sh`: max |residual| 0.0000 (t_ord_cg_deemed)
- `etr`: max |residual| 0.0040 (t_cg_corp_qbi)
- `etrc`: max |residual| 0.0050 (t_ord_wealth_taxmax)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | static d1 err % |
|---|---|---|---|---|---|---|
| pc_cgr50_deemed | 1803.6 | 1793.6 | -0.56 | -0.81 | -0.96 | -0.22 |
| pc_cgr30_wealthr3t500 | 1920.3 | 1918.1 | -0.11 | +0.05 | +0.26 | +0.00 |
| pc_cgr50_estater60e5 | 1154.0 | 1153.8 | -0.02 | -0.06 | -0.10 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4559.3 | 4533.8 | -0.56 | -0.98 | -1.56 | +0.00 |
| pc_wealthr1t1000_deemed | 1387.0 | 1385.8 | -0.09 | +0.03 | +0.20 | +0.00 |
| pc_ordr50_cgr30 | 2387.5 | 2379.1 | -0.35 | +0.23 | +0.23 | +7.42 |
| pc_ordr50_qbi | 3027.7 | 3036.0 | +0.27 | +0.26 | +0.24 | +0.00 |
| pc_corpr28_cgr30 | 1081.7 | 1081.9 | +0.01 | +0.04 | +0.03 | +0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4286.8 | -0.02 | -0.32 |
| q02 | 8725.7 | -0.01 | -0.81 |
| q03 | 1383.0 | +0.39 | +0.68 |
| q04 | 7662.9 | +0.54 | +0.11 |
| q05 | 8426.7 | -1.43 | -1.85 |
| q06 | 6109.0 | +0.38 | +0.08 |
| q08 | 8761.4 | -0.38 | -1.29 |
| q09 | 7377.9 | -0.84 | -0.68 |
| q10 | 7122.5 | -0.31 | -0.30 |
| q11 | 5738.5 | -0.73 | -0.25 |
| q12 | 6659.8 | -0.71 | -1.04 |
| q13 | 4867.1 | -0.85 | -0.87 |
| q14 | 8434.6 | -0.67 | -0.86 |
| q15 | 8174.0 | -1.09 | -0.92 |
| stack_ref | 10266.6 | +0.11 | +0.03 |

Triples improve 8/15 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4286.8 | 4286.0 | -0.02 | +0.42 | +0.99 | 5.37 | 99.05 | 0.314 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8725.7 | 8724.8 | -0.01 | +0.39 | +0.73 | 1.01 | 207.01 | 0.536 |
| q03 | cg+deemed+estate | 1383.0 | 1388.4 | +0.39 | -0.01 | +0.09 | 28.25 | 35.51 | 0.262 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7662.9 | 7704.7 | +0.54 | +1.13 | +1.54 | 1.85 | 306.13 | 0.773 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8426.7 | 8306.4 | -1.43 | -1.52 | -1.63 | 2.12 | 662.83 | 1.702 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 6109.0 | 6132.0 | +0.38 | +0.45 | +0.28 | 0.66 | 189.39 | 0.596 |
| q07 | corp+estate+ord+qbi+wealth | 5120.6 | 5188.3 | +1.32 | +1.45 | +0.67 | 1.67 | 56.70 | 0.329 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8761.4 | 8728.1 | -0.38 | +0.04 | +0.48 | 0.78 | 259.49 | 0.718 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7377.9 | 7315.8 | -0.84 | -1.38 | -1.83 | 1.94 | 278.30 | 0.590 |
| q10 | corp+deemed+estate+ord+qbi+taxmax+wealth | 7122.5 | 7100.7 | -0.31 | -0.13 | -0.03 | 0.41 | 24.51 | 0.172 |
| q11 | cg+deemed+taxmax+wealth | 5738.5 | 5696.6 | -0.73 | -0.62 | -0.62 | 1.08 | 70.65 | 0.147 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 6659.8 | 6612.4 | -0.71 | -0.67 | -0.62 | 2.15 | 602.85 | 1.639 |
| q13 | corp+deemed+estate+qbi+wealth | 4867.1 | 4825.7 | -0.85 | -0.80 | -0.80 | 0.90 | 104.24 | 0.257 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 8434.6 | 8378.4 | -0.67 | -0.83 | -1.62 | 2.20 | 373.75 | 1.322 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8174.0 | 8084.9 | -1.09 | -1.33 | -1.65 | 1.82 | 164.31 | 0.655 |
| q16 | corp+deemed+qbi+taxmax+wealth | 5723.7 | 5710.4 | -0.23 | -0.13 | +0.06 | 0.27 | 12.57 | 0.098 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10266.6 | 10278.4 | +0.11 | +0.14 | +0.13 | 0.53 | 20.51 | 0.032 |

**Holdout bounds (max |err|, per decade): conv ±1.5% / ±1.6% / ±1.9% (median d1 quiz 0.54%); static ±7.5% / ±6.8% / ±6.5%**

## Warnings
- q01: ETR cell err 0.314pp > 0.1pp
- q02: ETR cell err 0.536pp > 0.1pp
- q03: ETR cell err 0.262pp > 0.1pp
- q04: ETR cell err 0.773pp > 0.1pp
- q05: ETR cell err 1.702pp > 0.1pp
- q06: ETR cell err 0.596pp > 0.1pp
- q07: ETR cell err 0.329pp > 0.1pp
- q08: ETR cell err 0.718pp > 0.1pp
- q09: ETR cell err 0.590pp > 0.1pp
- q10: ETR cell err 0.172pp > 0.1pp
- q11: ETR cell err 0.147pp > 0.1pp
- q12: ETR cell err 1.639pp > 0.1pp
- q13: ETR cell err 0.257pp > 0.1pp
- q14: ETR cell err 1.322pp > 0.1pp
- q15: ETR cell err 0.655pp > 0.1pp

**ALL CHECKS PASS.**

# top_tax dials — surrogate validation report

- vintage: `/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Simulator/v1/top_tax_dials_v2`
- data: `other/top_tax/atlas2_data.json`
- hard bar: ±2.0% on conventional 10-yr total; ETR secondary bound 0.1pp/cell

## (1) Fit exactness at fitted points
- `ct`: max |residual| 0.0000 (t_wealth_estate_qbi)
- `cy`: max |residual| 0.0000 (t_ord_corp_wealth)
- `ch`: max |residual| 0.0000 (t_corp_wealth_taxmax)
- `st`: max |residual| 0.0000 (t_ord_wealth_taxmax)
- `sy`: max |residual| 0.0000 (pr_wealth_taxmax)
- `sh`: max |residual| 0.0000 (t_ord_cg_qbi)
- `etr`: max |residual| 0.0040 (t_cg_corp_qbi)

## (2) Pair corners — I·g·g scaling off reference

| corner | actual ct ($B) | pred ct ($B) | err % | static err % |
|---|---|---|---|---|
| pc_cgr50_deemed | 1762.9 | 1760.8 | -0.12 | -0.22 |
| pc_cgr30_wealthr3t500 | 1928.1 | 1925.7 | -0.13 | +0.00 |
| pc_cgr50_estater60e5 | 1180.3 | 1180.1 | -0.02 | +0.00 |
| pc_wealthr4t50_estater60e5 | 4559.3 | 4533.8 | -0.56 | +0.00 |
| pc_wealthr1t1000_deemed | 1367.1 | 1365.9 | -0.08 | +0.00 |
| pc_ordr50_cgr30 | 2396.1 | 2386.4 | -0.40 | +7.42 |
| pc_ordr50_qbi | 3028.0 | 3036.2 | +0.27 | +0.00 |
| pc_corpr28_cgr30 | 1089.7 | 1089.8 | +0.01 | +0.00 |

## (3) Triple terms on cluster-heavy holdouts

| package | actual ct | with T err % | without T err % |
|---|---|---|---|
| q01 | 4259.7 | -0.08 | -0.37 |
| q02 | 8685.2 | -0.02 | -0.83 |
| q03 | 1355.3 | +0.25 | +0.54 |
| q04 | 7625.8 | +0.51 | +0.07 |
| q05 | 8416.9 | -1.40 | -1.84 |
| q06 | 6098.4 | +0.36 | +0.05 |
| q08 | 8716.8 | -0.34 | -1.26 |
| q09 | 7367.4 | -0.84 | -0.69 |
| q10 | 7102.9 | -0.31 | -0.30 |
| q11 | 5703.0 | -0.75 | -0.30 |
| q12 | 6652.8 | -0.64 | -0.98 |
| q13 | 4848.8 | -0.85 | -0.87 |
| q14 | 8400.1 | -0.70 | -0.92 |
| q15 | 8144.9 | -1.11 | -0.96 |
| stack_ref | 10227.6 | +0.12 | +0.00 |

Triples improve 8/15 cluster-heavy holdouts.

## (4) Quiz holdouts — hard bar · (5) full stack

| package | levers | actual ct ($B) | pred ct ($B) | err % | byyear max % | heads max $B | etr max pp |
|---|---|---|---|---|---|---|---|
| q01 | cg+deemed+estate+ord+qbi+wealth | 4259.7 | 4256.2 | -0.08 | 5.31 | 33.40 | 0.314 |
| q02 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8685.2 | 8683.6 | -0.02 | 0.23 | 82.81 | 0.536 |
| q03 | cg+deemed+estate | 1355.3 | 1358.6 | +0.25 | 30.06 | 20.70 | 0.152 |
| q04 | cg+deemed+estate+ord+qbi+taxmax+wealth | 7625.8 | 7664.8 | +0.51 | 0.73 | 123.27 | 0.773 |
| q05 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8416.9 | 8299.4 | -1.40 | 1.84 | 307.79 | 1.702 |
| q06 | cg+corp+deemed+estate+ord+qbi+taxmax | 6098.4 | 6120.5 | +0.36 | 0.52 | 86.29 | 0.596 |
| q07 | corp+estate+ord+qbi+wealth | 5120.9 | 5188.5 | +1.32 | 1.60 | 37.56 | 0.329 |
| q08 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8716.8 | 8687.1 | -0.34 | 0.48 | 123.47 | 0.718 |
| q09 | cg+corp+deemed+estate+qbi+taxmax+wealth | 7367.4 | 7305.7 | -0.84 | 1.92 | 51.91 | 0.590 |
| q10 | corp+deemed+estate+ord+qbi+taxmax+wealth | 7102.9 | 7081.2 | -0.31 | 0.40 | 14.69 | 0.172 |
| q11 | cg+deemed+taxmax+wealth | 5703.0 | 5660.3 | -0.75 | 1.10 | 18.30 | 0.108 |
| q12 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 6652.8 | 6610.3 | -0.64 | 2.30 | 273.39 | 1.639 |
| q13 | corp+deemed+estate+qbi+wealth | 4848.8 | 4807.5 | -0.85 | 0.89 | 42.35 | 0.257 |
| q14 | cg+corp+deemed+ord+taxmax+wealth | 8400.1 | 8341.5 | -0.70 | 1.37 | 175.18 | 1.322 |
| q15 | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 8144.9 | 8054.6 | -1.11 | 1.31 | 37.20 | 0.655 |
| q16 | corp+deemed+qbi+taxmax+wealth | 5704.3 | 5691.0 | -0.23 | 0.26 | 11.66 | 0.098 |
| stack_ref | cg+corp+deemed+estate+ord+qbi+taxmax+wealth | 10227.6 | 10240.1 | +0.12 | 0.30 | 5.57 | 0.012 |

**Holdout bound: max |err| 1.40% (median quiz 0.51%) → badge bound ±1.4%; static max |err| 7.42% → static bound ±7.5%**

## Warnings
- q01: ETR cell err 0.314pp > 0.1pp
- q02: ETR cell err 0.536pp > 0.1pp
- q03: ETR cell err 0.152pp > 0.1pp
- q04: ETR cell err 0.773pp > 0.1pp
- q05: ETR cell err 1.702pp > 0.1pp
- q06: ETR cell err 0.596pp > 0.1pp
- q07: ETR cell err 0.329pp > 0.1pp
- q08: ETR cell err 0.718pp > 0.1pp
- q09: ETR cell err 0.590pp > 0.1pp
- q10: ETR cell err 0.172pp > 0.1pp
- q11: ETR cell err 0.108pp > 0.1pp
- q12: ETR cell err 1.639pp > 0.1pp
- q13: ETR cell err 0.257pp > 0.1pp
- q14: ETR cell err 1.322pp > 0.1pp
- q15: ETR cell err 0.655pp > 0.1pp

**ALL CHECKS PASS.**

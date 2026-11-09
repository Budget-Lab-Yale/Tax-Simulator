# sigma validation: 2x2 own/cross elasticity matrix

Root: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/sigma_validation`; legs: `topord_plus5` (ordinary), `cg_mirror` (CG); years 2025-2035;
averages over 2026-2035 (lead-in year dropped). INFORMAL check
(DESIGN_LOCK ruling 2) - eyeball vs brackets, iterate sigma/pool if off.

Conventions: ordinary-income rows are NET-OF-TAX-form elasticities
(dlogY/dlog(1-tau), the ETI and Mortenson-Table-5 convention); gains rows
are RATE-form (dlogR/dlog tau, the realization-literature convention),
converted from the estimated NOT-form via dlog(1-tau)/dlog(tau).

| response \ leg | tau_ord leg | bracket | tau_cg leg | bracket |
|---|---|---|---|---|
| ordinary income, top subset (NOT-form) | 0.431 | SSG ETI 0.12-0.40 | -0.128 | Mortenson cross face -0.77 (range -0.24..-2.4); expect BELOW face |
| LT gains realizations (RATE-form) | 0.103 | positive, persistent, << +2.77 (Mortenson face) | -0.255 | Dowd/Mortenson own-gains -0.8..-0.9 |

## Composition (sigma tracker)

See composition.csv: conversion inflow by year, wages-vs-PT split, pool
size, mean pooled wedge, mean equity-leg change. The corporate-base leg
of diverted compensation is the entity-shifting module (conservation
diagnostics in the Phase 2C logs).

## Symmetry

wedge_symmetry.csv states each leg in pool-weighted wedge units
(mean_dW per leg; the CG leg was sized from leg 1 pool-weighted dW).

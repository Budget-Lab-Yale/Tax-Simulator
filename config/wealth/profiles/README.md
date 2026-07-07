# Wealth-dynamics financing profiles

A **financing profile** is the per-scenario input to the wealth bathtub
(`src/sim/wealth_dynamics.R`): a **bracket-varying saving share** `s(age,
net-worth percentile)` and a **within-age percentile transition matrix** `M`.
Each profile is a folder under `config/wealth/profiles/` holding two files.

These are **operational inputs, not reform tax law** — never override them from
a scenario YAML.

## Files

| File    | Format | Meaning |
|---------|--------|---------|
| `s.csv` | header `age,nw_pctile,s`; one row per cell | saving share `s = 1 − MPC ∈ [0, 1]`. Must cover **every** cell of the `18..80` (age) × `1..n_pctiles` grid **exactly once** (the loader hard-errors on gaps, duplicates, or out-of-range values). |
| `M.csv` | headerless `n_pctiles × n_pctiles` grid | within-age percentile transition, applied to every age; raked to doubly-stochastic on load. Identity = full persistence (the realistic near-truth); uniform `1/n` = extreme diffusion. **Absent ⇒ identity.** A per-age `M` may instead be supplied as `M.rds` (a named-by-age list of matrices). |

`age` is `pmax(age1, age2)` for joint records, topcoded to `[18, 80]`;
`nw_pctile` is the within-age net-worth percentile bin (positive net worth
only), matching the cells the pre-pass and applier rank into.

## How a scenario selects a profile

Resolved by `wealth_dyn_resolve_profile()`, precedence high→low:

1. `wealth_financing = none` / `off` → channel **forced off**.
2. `wealth_financing = <folder>` → that profile (the bracket-varying path).
3. scalar `s` column set → **flat** profile (`s` everywhere, identity `M`) — the
   back-compatible shorthand; `s = 0` is a deliberate "off".
4. nothing specified → the **`default`** profile (auto-applied).

The channel is **active** iff the resolved `s` has any positive cell, so a
flat-zero profile is a no-op and skips the ~2× split-pass compute.

## Shipped profiles

- **`default/`** — **CALIBRATED** (2026-07-07) persistent-flow `s` surface,
  identity `M`. Auto-applied, so the channel is **on model-wide**: any scenario
  that names no profile and sets no scalar `s` gets this surface (and the ~2×
  split-pass compute). Opt out with `wealth_financing = none`. Concept:
  `s = 1 − MPC` out of a *persistent* net income change, anchored to Straub
  (2019), Mian–Straub–Sufi (2021), Dynan–Skinner–Zeldes (2004),
  Fagereng–Holm–Natvik (2021), De Nardi–French–Jones (2010) — rises from ~0.1
  (bottom of within-age net worth) to 0.80 (top percentile), hump-shaped in age
  with the tilt attenuated to zero at top ranks. Sourcing memo:
  `other/wealth_dynamics/default_s_calibration.md`; generator: `s_default()` in
  `other/wealth_dynamics/write_profiles.py`.
- **`example_age_wealth/`** — an **ILLUSTRATIVE** (not calibrated) `s` surface
  that rises with net-worth rank and is hump-shaped in age, with identity `M`.
  Point a scenario at it to exercise the bracket machinery.

## Regenerating

```bash
python3 other/wealth_dynamics/write_profiles.py
```

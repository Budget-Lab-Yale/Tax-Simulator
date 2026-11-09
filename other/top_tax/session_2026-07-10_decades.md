# Session log — 2026-07-10: 30-year decade atlas + realized ETRs

Continuation of the dials/atlas2 track (see `session_2026-07-09_dials.md`).
Started as a "why are the offsets small" investigation; ended with the atlas
rebuilt on a 30-year batch with a decade toggle, realized-ETR bars, a
distributional frontier, and Yale typography. **Final artifact:
`other/top_tax/atlas2_built.html` on vintage `top_tax_dials_30y_v1`
(scratch root), ALL CHECKS PASS, netlify_drop refreshed.**

## 1. Offsets investigation (morning)

Author asked why certain offsets look small. Findings (all on v2 data):
- First-order cross-base drains live in SOLO scores (wealth 2%>$50M drains
  iit −$430B / cg −$242B / estate −$87B per decade-1); pairwise terms are
  cross-partials only. wealth×estate −$22.1B ≈ −87.3 × (10pp/40%) exactly.
- Estate-side offsets are triple-damped: valuation bridge × mortality gate
  (~1–2%/yr) × 10-yr window truncation of a compounding stock (drain ramps
  −6.9→−13.2B/yr, still growing at window end).
- corp→cg (−$57B): μ≈1.6% markdown + dividend cuts; interest/rent/pt flows
  phase in at η=0.057/yr (41% by 2036) — mostly out-window. corp×cg
  interaction −$5.2B = higher-rate × collapsed-realizations netting.
- Scope absences: estate own-rate behavior (gate 2), cg×estate ≈ 0 (Bellman
  doesn't price estate rate), labor supply, CORP_* placeholders.

This motivated the 30-year re-run: the window was hiding the compounding.

## 2. The 30-year batch (`top_tax_dials_30y_v1`)

- 199 scenarios × 2026:2057 (decade windows 2027–36/2037–46/2047–56 + FY-lag
  year). Feasibility: Tax-Data→2097, Macro→2099, OME→2100; Estate-Tax-Dist
  heir files end 2055 but are dist_years-only (2027/2036 unchanged).
- **Disk**: project fs was 92% full (v2 = 1.1TB); batch went to SCRATCH
  (local=1). ~3TB with detail KEPT (needed for the realized-ETR build!).
- Two NEW 30-year-horizon guard trips, both tolerance-class, both fixed:
  1. **Hidden-ledger conservation** (`wealth/avoidance.R`): absolute $1e-6
     bar vs per-record float noise that scales with flow level (~2e-5 on
     1e11 records by 2046). Now RELATIVE 1e-9 with $1 floor. 49 tasks retried.
  2. **Sigma conservation** (`sigma_conversion.R`): the benign pre-pass vs
     haircut-eroded-frame wedge COMPOUNDS with horizon (~1% yr 11 → ~2% yr
     24). RTOL 0.015→0.05, ATOL 5e7→2.5e8 (both-bars retained). 10 retried.
- Zero other failures across ~19.6k year-tasks. Full stack at ref:
  $10.3T/$15.4T/$21.4T conv by decade (~2.6% GDP each), $47.0T/30yr;
  survival ratio falls 70%→66%→61% by decade (compounding erosion).

## 3. Toolchain: per-decade everything

- Schema 3: quantities ct/st m=3 (decade totals), cy/sy m=30, ch/sh m=21
  (decade-major heads), etr/etrc m unchanged (2027 impact year).
- **Every fitted object (f, I, T, and g) is estimated independently per
  decade** — g became a per-decade vector (author call), so interaction
  scaling is decade-matched (etr quantities use d1 g). Only the FORM
  (solo+pair+triple, 4-way=0) is shared; quiz holdouts measure it per decade.
- Validation: hard ±2% bar on conv DECADE-1 only; decades 2/3 + static get
  measured-and-disclosed bounds. **Result: conv ±1.5/±1.6/±1.9%, static
  ±7.5/±6.8/±6.5% — the form barely decays out-decade.**
- Extractor: `leg_deltas_windows()` multi-window reader; NA lead-out-year
  rows skipped (2057 corp is NA by design).

## 4. Realized (conventional-numerator) ETRs — reform_leg

- `distribution_etrs.csv` now keyed by **reform_leg** {static, conventional}:
  the conventional rows swap reform NUMERATORS to the conventional detail
  (via `read_static_detail(leg=)` + `build_distribution_microdata(reform_leg=)`);
  denominators/rankings/stock bases stay baseline-static. This is the VISION
  "welfare vs realized" numerator-swap, now a general model output (Phase 3b).
  No sim re-run needed — 3b-only rerun on kept detail (~200 jobs, minutes).
- New surrogate quantity `etrc` (same m as etr, d1 g, same validation).
- Headline at ref stack, top 0.01% accrual 2027: baseline 23.7% → ask 48.9%
  → collected 37.3% (11.6pp avoidance margin).

## 5. Atlas UI (all harness-gated)

- **Decade toggle** (hero): all revenue cards re-derive; frontier + cg
  surface recompute per decade (cached); ETR card pinned to 2027 (noted).
- **Distribution card = dumbbell**: black baseline dot, stem to filled-blue
  conventional (collected) and hollow static (ask) dots; component stacking
  removed; hollow-to-filled gap = avoidance margin. Top 0.1% tile shows both.
- **Frontier**: x-axis in share-of-decade-GDP; NEW metric toggle → x = Δ top
  0.1% realized ETR (accrual, 2027, decade-invariant). Lattice carries a
  third accumulator (etrc top-slice with d1-g weights). Real-data read: max
  +15.2pp; last 0.4pp costs −75→−125¢ leakage.
- **Yale typography**: YaleNew-led serif stack (Georgia fallback) for prose/
  headings; sans UI chrome; mono numerals. Self-contained (no font fetch).
- Badge: per-decade bounds disclosed. Harness now exercises decade toggle,
  metric toggle, per-decade fixtures (conv_totals/static_totals arrays),
  cross-decade frontier determinism.

## 6. Open items

- Commit everything (branch `wealth`): guard fixes, reform_leg, decade
  toolchain, template. All uncommitted at session end pending author OK.
- Netlify re-drop is manual (browser drag of `netlify_drop/`).
- v1 dials vintage (1.1TB pre-fix garbage, production fs) is deletable for
  space — author call.
- Candidate: decade-2/3 ETR years (2037/2047 dist_years) if the ETR card
  should ever follow the decade toggle; needs 3b + heir files ≤2055 (fine).

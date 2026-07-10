# Session log — 2026-07-09 (evening): clean factorial re-run + atlas product build-out

Continuation of the top_tax factorial work (see `VISION.md`, memory:
`top_tax_factorial_build`). This session took the project from "contaminated
first batch + design mockup" to "clean data live in a shipped interactive
product."

## 1. The re-run saga

- The 2:59pm re-run of the 128-combo factorial (`top_tax_v1`) **missed the
  entity-shifting fix**: SLURM workers source behavior modules live from the
  repo at execution time (`load_behavior_module`, `src/sim/behavior.R:140`);
  the conventional year-tasks ran 4:31–4:37pm and the fixed
  `pearce_prisinzano.R` hit disk at 4:56pm. Confirmed in output: c001 (ord
  alone) showed a 2027 SE base of $12.0T vs $693B baseline — the exact bug
  signature (SECA companions scaled by
  `(part_active + amount_shifted)/part_active`, degenerate when `part_active`
  is small relative to the full pass-through base).
- The previous session had queued a resubmit as a background task; it fired at
  ~5:05pm — after the fix landed — so that run had the fixed module. Full
  pipeline (4,572 year-tasks + bathtubs + aggregation) finished in ~50 minutes
  on a warm cluster, zero failures, conservation guard quiet at the bumped
  0.015 tolerance. **Detail files kept** this time.
- Post-run verification: SE base $691.7B vs $693.1B baseline (small negative,
  correct sign); c001 conventional below static ($55.5B vs $60.6B in 2027).
- Commits: `7b71ecddd` (module fix + `SIGMA_CONSERVE_RTOL` 0.01→0.015).
  Flag: any pre-fix conventional run pairing the entity-shifting module with
  an ordinary/CG rate reform carries the payroll artifact.

## 2. Headline results (clean top_tax_v1, 10 years 2027–36)

- Full stack: **$8.71T static / $6.71T conventional (1.69% of GDP)**.
- The package **beats the sum of standalone conventional scores by 9%**
  (static +5%). The old "sum overstates the whole" framing was backwards; copy
  rewritten sign-aware. Driver: cg+deemed closes the deferral escape valve
  (+$580B pairwise); rate stacking and base broadening drive the static side
  (cg+ord +$232B static).
- Standalone survival (conv/static): cg 24%, wealth 67%, ord 89%, corp 88%,
  deemed 251% (unlock), estate ≈100%, qbi ≈100%.
- Cross-base spillovers live in each lever's own standalone score, not the
  interaction terms: wealth drains iit −$236B / cg −$130B / estate −$48B;
  corp drains cg −$57B. Pairwise interaction terms are small because they are
  cross-partials — e.g. wealth×estate −$6.15B ≈ −$47.9B × (5pp/40%): the
  estate reform only adds 5pp of marginal rate at the $50M+ margin where the
  wealth tax bites. cg×estate ≈ 0 by scope (the kg Bellman does not price the
  estate rate); the estate lever has no behavioral response of its own.
- Top 0.01% ETR in 2027: accrual (Haig-Simons) 23.7% → 40.5% under the full
  stack; cash (expanded) 30.0% → 51.2%.
- Frontier (conventional revenue vs. leakage in ¢ per collected dollar):
  16 undominated packages. Deemed-anchored left half at negative leakage
  (deemed alone $0.7T at −60¢); ord+corp+deemed+estate+qbi = $3.67T at −5¢;
  wealth joins at $4.06T / +12¢; full stack $6.71T at +30¢. cg alone is the
  most dominated point: +311¢ per collected dollar.

## 3. Atlas product (template `atlas.html` → `build_atlas.py` → `atlas_built.html`)

Built up, then trimmed to final form on author direction.

Added during the session:
- **Shapley split of the package bar** in Parts-vs-whole: exact client-side
  computation over all 2^k sub-combination runs; contributions sum to the
  package total. Verified: zero negative contributions across all 441
  (subset, lever) pairs on real data. Full-package shares: wealth 29%,
  deemed 16%, qbi 14%, ord 13%, cg 12%, corp 11%, estate 6%.
- **Spillovers card**: per selected lever, standalone conventional revenue by
  destination, diverging around $0 (drained bases drawn left). Colors = tax
  heads. This is where the "wealth tax erodes the estate/income bases" story
  is visible.
- **Frontier section**: 127 dots, undominated line, cg-without-deemed packages
  (leakage 62–311¢) pinned as triangles at the top edge, toggle selection
  ringed, single dot color, tooltips = package composition only. Annotated
  hinge points.
- **Lever palette**: 7 hues deliberately distinct from the tax-head palette
  (ochre/cyan/rose/olive/violet/sea-green/coral), CVD-validated in light and
  dark via the dataviz validator.
- Interim placeholder build (`make_conv_placeholder.py`): real static side +
  synthetic conventional, used while the clean run cooked; retired once real
  data landed.

Removed on author direction (final trim):
- Standalone-rate-policy section, "On capital gains & dividends" footnote,
  Order-matters (stacked first/last dumbbells) card, Composition ledger card,
  ETR dot-color scale on the frontier, hero thesis chart (hero is now
  text-only, two newspaper columns, no package numbers).
- Copy de-LLM pass: no bold in body prose, frontier description rewritten
  plain, controls renamed "Tax rates" / "Tax bases", distribution card renamed
  "Distribution of tax rates" with its y-axis held constant across the
  cash/accrual toggle.

Added at the end:
- **Footer margins manifest**: two-column list of behavioral responses and
  mechanical channels included (each maps 1:1 to a real module/channel), plus
  "Not included: labor-supply responses, macroeconomic feedback, and changes
  to tax enforcement."

## 4. Build/verify hygiene

- One shipped breakage: deleting the distribution chart's axis-max loop
  removed the `var i` a later loop relied on → strict-mode ReferenceError
  killed the distribution and frontier renders. Root cause found, fixed, and
  the build check upgraded: `node --check` (syntax) **plus a headless render
  harness** — executes the built page's script in node with a DOM stub and
  asserts every chart container renders non-empty. Run before every publish:
  `node other/top_tax/check_atlas_render.js` (recreated as a durable repo
  file 2026-07-09 late evening — the original lived in a session scratchpad
  and was wiped; the new one also exercises switch-toggle re-renders and
  fails on a placeholder-data badge).

## 5. Deployment

- Claude artifact (private, same URL all session):
  https://claude.ai/code/artifact/17262b0a-b181-45bc-90f5-20cec96c8be4
- Public hosting for principals (own accounts, off-team): Netlify Drop →
  **top-tax.netlify.app**. The page is a single self-contained HTML; deploy =
  put `atlas_built.html` in a folder as `index.html`, drag onto the site's
  Deploys page. As of session end the Netlify site was serving a broken/old
  deploy (empty-folder drop → 404) — needs a re-drop of the latest build.

## 6. Commits and open items

Commits (branch `wealth`): `7b71ecddd` fix; `6be31f653` atlas tooling;
`eea324c2d` spillovers/dumbbells/frontier. **Uncommitted at session end:**
everything from the final trim onward (card removals, copy pass, frontier
simplification, render fix, text-only hero, margins footer).

Open items:
1. Re-drop latest `atlas_built.html` to Netlify (currently 404/stale).
2. Commit the final-trim state.
3. Pitched, not built: hero that answers the title outright ($6.7T / 1.69%
   GDP / 23.7→40.5% accrual top-0.01% ETR); who-pays card ($ by income group
   from distribution files); revmax intensity grid (+30/35pp extension) so the
   frontier's right edge is a true ceiling.
4. Corporate wedge is the author's real OME numbers but `CORP_*` incidence
   constants remain Phase-0c placeholders.

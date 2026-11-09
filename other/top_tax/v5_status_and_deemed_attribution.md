# Top-tax v5: where things stand, and the deemed-tax attribution question

2026-07-28. Status note written mid-decision; section 4 records the question
and its resolution: option (c) was ruled the same afternoon and is implemented.

## 1. What v5 is

A full re-run of the top-tax dials batch (interactive atlas + report), superseding
v3. Motivations, accumulated since v3 shipped:

- the net-of-tax (logs) realization response form is now the model default
- the corporate rate is scored on-model (src/sim/corp_rate.R); the corp dial
  extends to 35% with anchors at 24.5 / 28 / 31.5 / 35
- the estate avoidance activation fix is in the code but not in the shipped
  v3 vintage
- the death-gain exclusion feature (below), added after Greg Leiserson's
  review asked what an enacted step-up repeal with a per-decedent exemption
  would look like

v4 was cancelled mid-run on 2026-07-28 when the exclusion feature was scoped;
its partial vintage is deleted. v3 remains on scratch as the reference vintage
until v5 validates.

## 2. The death-gain exclusion, as built

All code is complete and uncommitted on branch `wealth`. Plan of record:
`/home/jar335/.claude/plans/prancy-gliding-bunny.md`.

- Tax law: `pref.kg_death_gain_excl` (mapper + single/married leaves, $0 under
  current law). The first X of a decedent's unrealized gain keeps step-up,
  pro-rata by gain across the asset classes whose death regime is carryover or
  deemed realization, after §121. Married amounts apply at the both-die event.
- Singles: two branches (own amount vs the married amount via a survivor
  election), blended by a widowhood probability from the 2022 SCF by age band
  and sex (`config/calibrations/estate/widowhood.yaml`; among unmarried people
  the widowed share runs from about 1% under 50 to 65-74% at 80+). Records
  with a divorce year get zero.
- The dial: the death lever gained an exclusion sub-axis (anchors $0 / $1M /
  $5M single). Positions are still never g-scaled; the exclusion interpolates
  within a position. The regenerated batch is 212 runscript rows (was 202).
- Verification, all passed on 2026-07-28:
  - unit test `other/kg_model_tests/test_death_gain_excl.R` (pro-rata
    identity, married doubling, widow blend, carryover mix)
  - a $0 byte-identity gate (pre-feature worktree vs feature tree, baseline +
    s_deemed_deemed, seven years, full sample): every detail, totals and
    revenue file byte-identical; the only diffs are the new all-zero law
    column, the provenance manifests, and the distribution files changed on
    purpose (section 4)
  - an active smoke ($1M exclusion, deemed and carryover): completes cleanly,
    allocator identity exact, no heir-ladder exhaustion
- The kg bathtub calibration hashes on apply.R / recurrence.R were re-pinned
  after the gate passed (the staleness check had stopped the first runs, as
  designed).

Headline numbers from the seven-year full-sample smoke runs (2027):

| | deemed, no excl | deemed, $1M excl | carryover, $1M excl |
|---|---|---|---|
| conventional revenue, 2027 | +$63.3B | +$39.5B | +$13.3B |
| realized LT gains vs baseline | +11.9% | +8.1% | +4.1% |

A $1M per-decedent exclusion gives back roughly 40% of deemed revenue.
Carryover raises little inside a seven-year window because its revenue arrives
only as heirs realize.

## 3. Run state

The v5 batch (212 scenarios, vintage `top_tax_dials_30y_v5`, SLURM batch mode)
was launched on 2026-07-28, killed minutes later pending the attribution
decision in section 4, and relaunched the same afternoon once the author
ruled that the decision (post-processing only) need not gate the simulations.
The killed run's partial output was deleted first. Relaunch job IDs:
20148164-20148173; launch log at logs/v5_batch_relaunch.log.

After relaunch, the remaining sequence is unchanged: drain, sweep staging
logs, refit the atlas (`other/top_tax/run_fit_chain_v5.sh`, with the render
harness probes re-pinned to v5), rebuild the dist card, then the report per
`other/top_tax/report_prep/AGENT_HANDOFF.md` §5. v3 is deleted only after v5
validates end to end.

## 4. The open question: who bears the deemed tax

Deemed-realization tax is levied on decedents, so the distribution tables must
assign it to living households. Convention (shared with the estate tax): the
heirs bear it. We know each estate's tax and each heir's inheritance, but not
which heir got which estate, so any assignment rests on an assumption about
those links. Three candidates:

**(a) Proportional smear — what v3 shipped.** Total deemed tax spread over all
heirs in proportion to inheritance. Assumes every heir bears the same average
rate. Understates concentration: the tax owed by the largest estates leaks
onto heirs of modest inheritances. Top 1% share of the 2027 burden (income
ranking) in the smoke run: 57%.

**(b) Strict rank match — what the exclusion build switched to.** Estates
sorted by size, inheritances sorted by size, tax matched cumulatively top
down; heirs below the last taxed bequest dollar bear nothing. This is the
estate tax's allocator, where it is right because zero-tax is size-determined
by the exemption. For deemed tax with no exclusion it is wrong in a way the
author caught: zero-tax estates are the ones without unrealized gains, which
occur at every size, and the rank match implicitly assigns all of their
bequest mass to the smallest inheritances. Heirs of modest inheritances are
charged exactly nothing, though many modest estates owe real deemed tax.
Top 1% share: 98%.

**(c) Rank match over all estates — RULED and implemented 2026-07-28.** Same
cumulative walk, but zero-tax estates stay on the ladder. Each heir is then
charged the average deemed-tax rate of estates at their inheritance's size —
small but positive at the bottom under a no-exclusion regime, and falling to
zero at the bottom under an exclusion because small estates then genuinely owe
nothing. The threshold behavior emerges from the law rather than the sorting
assumption, and one method serves every dial position. Approximate top 1%
share (income including inheritance, python replication of the walk on the
smoke-run detail): ~93%, against ~100% for (b) in the same replication.
Implemented in `src/data/post_processing/estate_allocator.R` (deemed mode
keeps zero-tax entries; negatives still dropped with a warning; the
degenerate early-return now also covers an all-zero deemed ladder). Deemed
tests added to `other/estate_tax/test_allocator.R` (cases 9-11), run via the
new `test_allocator.sbatch`. The estate-tax ladder is unchanged. Landed while
the v5 batch was in Phase 2, so Phase 3b distribution tables come out under
(c) with no re-run.

The economics of the choice, for the record: the incidence premise (heirs
bear their own decedent's tax, pro rata) is fixed across all three methods;
they differ only in how the unobserved heir-decedent link is imputed. The
smear assumes inheritance size is uninformative about the decedent's rate;
(b) and (c) both assume bigger inheritances come from bigger estates. That is
also the assumption the upstream Estate-Tax-Distribution model used to build
the heir file (gross estate = inheritance x 2.8, a fixed equal split), so (c)
is consistent with the file's own generating process. A refinement — drawing
heirs-per-estate and split shares from a distribution, so a given inheritance
size mixes over a band of estate sizes — would smooth threshold cliffs
further, but belongs upstream in Estate-Tax-Distribution, where the heir
marginal and the link can be generated together.

Points that bear on the choice:

- Whatever is chosen should hold at every exclusion level, including zero, so
  the method never flips as the dial moves (author ruling, 2026-07-28).
- The estate tax's own allocation is unaffected: its zero-tax region really is
  size-determined, so the strict rank match stays correct there. (Option (c)
  applied to the estate ladder would smooth the exemption threshold and is not
  proposed.)
- The choice moves published distribution tables for every deemed scenario,
  so it needs a disclosure line in the methodology memo either way, including
  a note that v3's smear is not comparable.
- It is post-processing only. If the decision changes after v5 runs, Phase 3b
  can be re-run on the finished vintage without re-simulating.

## 5. Decisions still owed

Both decisions were made 2026-07-28: attribution method (c), and the v5 batch
was relaunched. Remaining work is the post-drain sequence in section 3, plus
the methodology-memo disclosure that deemed heir attribution changed from the
v3 smear (v3 deemed distribution tables are not comparable).

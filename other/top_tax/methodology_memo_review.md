# Review of the top-tax methodology memo

## Bottom line

The memo is intellectually strong and mostly faithful to the core calculations, but it should not be published—or the current estate estimates treated as final—without fixing one material scenario-wiring defect and tightening several claims about record-level consistency.

This review treats [`methodology_memo.md`](methodology_memo.md) as the active memo. The longer `METHODOLOGY.md` draft contains useful limitations that have not made it into this version.

## Priority findings

### 1. Blocking: estate avoidance is absent from estate-only scenarios

The memo says estate reforms shrink reported estates and that income evasion propagates into the estate base ([memo, line 67](methodology_memo.md#L67)). But both channels are implemented inside `wealth/avoidance`:

- Evasion-to-estate union: [`avoidance.R`, line 408](../../config/scenarios/behavior/wealth/avoidance.R#L408)
- Estate own-rate response: [`avoidance.R`, line 421](../../config/scenarios/behavior/wealth/avoidance.R#L421)

Estate-only and ordinary-plus-estate runs omit that module—for example [`dials.csv`, line 51](../../config/runscripts/top_tax/dials.csv#L51) and [`dials.csv`, line 68](../../config/runscripts/top_tax/dials.csv#L68). The estate calculator then defaults concealment to zero ([`estate.R`, line 179](../../src/calc/functions/tax/estate.R#L179)).

A static audit finds 39 estate-named top-tax rows with this omission. Consequently:

- Estate-only estimates have no Kopczuk–Slemrod own-rate response.
- Ordinary-plus-estate estimates do not carry income evasion into estates.
- The response appears when a wealth-tax lever is added, creating an unintended wealth-by-estate interaction.

The durable fix is a separate, always-applicable estate/reporting module. The minimal fix is to load `wealth/avoidance` in every estate-relevant run; its wealth component is a no-op at a zero wealth-tax MTR. A runscript contract test should also be added because the current unit test covers module math, not module activation.

### 2. High: the estate elasticity is not applied exactly as the memo says

The memo says the “reported base” retains

$$
\left(\frac{1-\tau_S}{1-\tau_B}\right)^{0.16}
$$

([memo, line 274](methodology_memo.md#L274)).

In code, that retained fraction becomes a concealment fraction of `reported_gross`; the calculator subtracts those dollars after deductions while leaving the lifetime-gift addback unchanged ([`estate.R`, line 215](../../src/calc/functions/tax/estate.R#L215)). This is not equivalent to multiplying the taxable or reportable estate base by the power-form retention factor. The code itself acknowledges that using gross rather than taxable estate overstates the response ([`avoidance.R`, line 110](../../config/scenarios/behavior/wealth/avoidance.R#L110)).

Either implement literal base scaling or disclose this as a gross-estate approximation whose effective elasticity is amplified by the gross-to-taxable-base ratio.

### 3. High: “ground-up, internally consistent microsimulation” overstates record-level integration

Three important places operate at cohort or bookkeeping level:

- The memo says capital-gains responses are distributed to records “in proportion to their gains” ([memo, line 148](methodology_memo.md#L148)). The implementation uses a calibrated 50/50 blend of positive realizations and unrealized-gain holdings ([`kg_dynamics.R`, line 166](../../src/sim/kg_dynamics.R#L166), [allocation at line 1907](../../src/sim/kg_dynamics.R#L1907)). That choice materially affects nonlinear tax and distributional results.
- Converted compensation does not remain in the originating taxpayer’s gain stock as claimed ([memo, line 215](methodology_memo.md#L215)). It is aggregated into age cohorts ([`sigma_conversion.R`, line 430](../../src/sim/sigma_conversion.R#L430)) and enters the cohort gain state in the following year ([`kg_dynamics.R`, line 2941](../../src/sim/kg_dynamics.R#L2941)). Its later realization can therefore land on another record.
- Entity shifting uses `tau_eq` to price the behavioral wedge, but retained earnings do not enter the gain-state dynamics. They are translated back into a current-year `kg_lt` bookkeeping offset ([`pearce_prisinzano.R`, line 176](../../config/scenarios/behavior/entity_shifting/pearce_prisinzano.R#L176), [booking at line 231](../../config/scenarios/behavior/entity_shifting/pearce_prisinzano.R#L231)).

A more accurate characterization would be:

> An integrated interaction layer over shared record-level balance sheets, combining cohort-level capital-gains dynamics, sequential reduced-form reporting and form responses, and mechanical conventional-side incidence channels.

### 4. High: the corporate-incidence description contains one mathematical error and two operational misstatements

- At full phase-in, the displayed formula gives $h=[(1-.375)+.375(.40)]w=.775w$, not $w$. Therefore `phi` is not the familiar $-\Delta\tau/(1-\tau)$ “fully phased in,” as claimed in [the memo at line 307](methodology_memo.md#L307). The naive expression holds at enactment, before normal-return reallocation; the code matches the displayed formula ([`corp_incidence.R`, line 696](../../src/sim/corp_incidence.R#L696)).
- Operationally, $w_t$ is read from gross-of-offset OME `revenues.csv`, not constructed as $\Delta\tau\Pi$ ([`corp_incidence.R`, line 194](../../src/sim/corp_incidence.R#L194)). The latter should be called a conceptual approximation or cross-check.
- The claimed 40 percent foreign/nonprofit/defined-benefit exclusion is used only in a conservation diagnostic, not as a 40 percent scaling of household records ([`corp_incidence.R`, line 121](../../src/sim/corp_incidence.R#L121)).
- The memo says every provisional constant is exposed for sensitivity sweeps, but only `sigma_n`, `kappa`, and permanence are runtime knobs ([`corp_incidence.R`, line 544](../../src/sim/corp_incidence.R#L544)).

### 5. Medium: important parameters and limitations are missing from Table 1

The table should add or disclose:

- Capital-gains record allocator: 50 percent realization and 50 percent gain stock.
- Timing reference wedge: 5 percentage points moves the full timeable bucket ([`kg_dynamics.R`, line 117](../../src/sim/kg_dynamics.R#L117)).
- Carryover bequest-motive parameter: 0.5 ([`pref.yaml`, line 96](../../config/scenarios/tax_law/baseline/pref.yaml#L96)).
- Single pooled realization state rather than separate asset-class states.
- Age-cell averaging of wealth-tax carrying costs. The longer draft reports an estimated 8.5–33 percent understatement from this aggregation.
- Dissaving uses 100 within-age wealth bins and a 90 percent record-level haircut clamp.
- The actual saving-finance profile ranges from 0.04, not 0.10, to 0.80 ([bottom](../../config/wealth/profiles/default/s.csv#L2), [top](../../config/wealth/profiles/default/s.csv#L6301)).
- Estate mortality is modified by a 300-expected-death donor-clone cluster cap ([estate parameters](../../config/estate/estate_valuation_params.yaml#L15), [implementation](../../src/sim/estate.R#L67)).

### 6. Medium: explain that interactions are sequential, not a fixed-point equilibrium

Corporate incidence runs first, then the dissaving haircut, then behavior modules in runscript order ([`run.R`, line 851](../../src/sim/run.R#L851)). Behavioral modules receive the same precomputed baseline and static MTR frames and are applied sequentially ([`behavior.R`, line 41](../../src/sim/behavior.R#L41)); MTRs are not iteratively recomputed after each upstream transformation.

That is a defensible conventional-scoring architecture, but “integrated” should not imply simultaneous optimization or equilibrium convergence.

### 7. Medium: charity is in the interaction map but has no methodology subsection

The implemented channel is only cash giving, on the intensive margin, with elasticity −0.5 and a capped adjustment of ±100 percent ([`charity/50.R`, line 1](../../config/scenarios/behavior/charity/50.R#L1)). Appreciated-asset giving and extensive-margin behavior are absent. Appendix B notes part of this, but a comprehensive memo should give the module a short description.

### 8. Source and provenance cleanup is needed

- The Penn Wharton paper is authored Richard Prisinzano and James Pearce, and it cautions against extrapolating far beyond the small observed tax-wedge changes. The memo should source the separate 60 percent denominator used to turn 0.3788 into 0.63 and flag large-reform extrapolation. See the [official PWBM paper](https://budgetmodel.wharton.upenn.edu/working-papers/w2018-2.pdf).
- The DeBacker coefficients appear to come from conference slides rather than a pinned public working-paper version. The author’s current CV lists the NTA presentation under a somewhat different title; the actual slide deck used should be archived and cited. See [DeBacker’s CV](https://jasondebacker.com/pdfs/DeBacker_CV.pdf).
- The code incorrectly labels the Kopczuk–Slemrod source as “Dying to Save Taxes” ([`avoidance.R`, line 61](../../config/scenarios/behavior/wealth/avoidance.R#L61)); the memo’s 2001 book-chapter citation is the relevant source for the reported-estate elasticity.

## What the memo already characterizes well

The memo is strong on:

- The capital-gains Bellman logic and death regimes.
- The sigma wedge and residual ETI calibration.
- The evasion formula and positive-income gates.
- The wealth concealment/valuation distinction.
- Estate valuation and portability mechanics.
- Heir rank matching.
- The dissaving recurrence.
- The provisional status of corporate incidence.

The primary problem is not the overall economic story. It is that the memo sometimes describes the intended conceptual model as though every link is presently active and record-level, while the implementation uses several cohort-level approximations—and one major estate link is not activated in the scenarios it is supposed to affect.

## Validation note

The existing hidden-ledger guard test could not be executed in the current shell because `Rscript` is unavailable. Static inspection confirms that the test exercises module internals but would not detect the runscript activation defect described above.

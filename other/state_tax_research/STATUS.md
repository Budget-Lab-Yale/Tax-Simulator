# State income tax workstream — status

**As of 2026-08-19** (branch `state-tax`). Current counts: **ALL 51
jurisdictions encoded** (39 broad-IIT + NH/TN narrow + WA excise + 6 zero-tax
stubs + DC), **48 enabled** for `states=all` (CA/SC/VA conformity-gated, not
encoding-gated). **Encoding coverage is complete. Nothing is unstarted.**

**The R6 multi-regime batch is COMPLETE: MT, LA and IA encoded 2026-08-19**,
closing the encoding programme. All three are two- or three-regime states,
and each turned on one structural reading that a plain transcription would
have got wrong.

MONTANA pivots on SB 399 (2021), effective TY2024, and almost nothing
survives. Its pre-2024 bracket bounds DO NOT VARY BY FILING STATUS, which is
what made filing status 2a (separate on the same form) near-universally
better for two-earner couples. Its federal income tax deduction lived INSIDE
the itemized schedule (`st_ded.fed_tax_ded_in_itemized`), so a
standard-deduction taker forwent it entirely and the state election cannot be
inherited from the federal one. Two provisions were enacted and repealed
before ever applying: SB 399's 6.5% top rate (cut to 5.9% by SB 121) and its
30% LTCG subtraction (replaced by HB 221's 3.0%/4.1% preferential schedule,
`st_ord.kg_pref_*`). Head of household is grouped with SINGLE for the
$5,000 federal-tax cap and with JOINT for the doubled standard-deduction
bounds -- the two groupings run in opposite directions.

LOUISIANA's dominant feature is where the exemption comes off. R.S.
47:32(A)(1)/294/295(B) relieve it at the LOWEST brackets with the bracket
bounds anchored to UNREDUCED income, so `tax = sched(TTI) - sched(E)`, not
`sched(TTI - E)` (`st_ord.exempt_from_bottom`). All 43,008 cells of the
published TY2017-TY2024 tables reproduce exactly under the first form and not
the second; a TY2021 single filer with one exemption and $27,625 of tax table
income owes $765 where the naive form gives $675. **The biggest correction to
the secondary literature on Louisiana: Act 395 did NOT repeal the excess
federal itemized deduction. It narrowed it to medical and dental only, and
that version is still on the TY2025 return** (`st_ded.item_less_fed_std`).
Head of household is a hybrid -- joint exemption base, single brackets -- and
from TY2025 Louisiana has NO dependant allowance of any kind.

IOWA has three regimes and four bracket counts in nine years. Two features
are unique in the set: its federal income tax deduction reached
STANDARD-deduction filers and was uncapped, where MO/AL/OR/MT all cap it or
confine it to itemizers or both; and because its pre-2023 nine-bracket ladder
is status-invariant, it gave married couples a two-column COMBINED RETURN
instead of a joint schedule, worth $1,162.61 on a 60k/40k couple in TY2022.
That election reuses the KY/DE `combined_sep` machinery plus one new
parameter for the per-column standard deduction, which is NOT a clean
fraction of the joint one. Its ALTERNATE TAX is a ceiling on tax, so it drops
straight into the MA no-tax-status formula -- and the single-filer exclusion
falls out of the arithmetic rather than needing a gate, because a zero single
threshold makes the ceiling the alternate rate times all of income and that
rate is at or above Iowa's top marginal rate in every year. Note an ERRATUM
in a primary source: IDR's own 2023 Statistical Report Table 1 prints the
third TY2023 bracket as "$31,050", which is the TY2024 figure -- the
statute, the report's own Figure 1 and the press release all say $30,000.

**Four states now wait on the same missing piece**, the generic
minimum-liability election pass: AL separate returns, AR's Low Income Tax
Tables, MS/AR per-spouse column returns, and MT's filing status 2a. Iowa's
combined return is the same shape and IS now encoded, which means the
machinery to close the other four is largely built -- `st_ord.combined_sep`
plus a per-column deduction is most of what AL, AR, MS and MT need.

**Two known differences are large enough to name here.** The IOWA AMT
(6.7%/6.4% through TY2022, not an election) is unencoded because it needs an
Iowa AMTI base built from federal AMTI plus nonconformity adjustments, and
the model has no plumbing for it -- pre-2023 tax is understated for
high-income filers with large preference items. And LOUISIANA's Schedule E
codes 02E-05E exempt 100% of LASERS, Teachers' Retirement, federal
retirement and a long list of other named systems; in a state where public
employment is a large share of the retiree population that is the single
largest Louisiana gap, and none of the systems is separable from pension
income.

**The R6 own-base batch is COMPLETE: MA, NJ, AR and MS encoded 2026-08-18**,
following MO/AL/OR the same day. (The Mississippi commit message says 47/44;
the correct figures are 48/45 -- counted from the registry after the fact.)
Each of the four brought machinery the set did not have. MA is the first
SCHEDULAR state, though over this window its classes collapse to two rates,
so only short-term gains needed carving out (`ob_st_gains_share` +
`st_ord.st_gains_rate`); its No Tax Status and Limited Income Credit turned
out to be ONE mechanism (`st_credits.lic_*`), with the published 1.75x band
ceiling and the separate-filer exclusion both falling out of the arithmetic.
NJ needed an income-banded pension exclusion (`pension_excl_tier_*`) tested
on TOTAL income before the exclusion itself, plus `pension_excl_orie` for the
unclaimed component -- whose ceiling runs on total income where the exclusion
proper runs on pension income, a distinction that fails SILENTLY if collapsed.
AR's published schedule is a `rate x income - minus adjustment` memo rather
than a ladder, converted programmatically to base amounts and verified back
at eight published points; it needs the base-amount shape for a genuine
whole-income-table NOTCH (about $180 in TY2020) and for its recapture-tail
cliffs. MS needed no new machinery but one careful reading: its "3% bracket
phase-out" was a ZERO BRACKET growing from below, not a rate cut.

**Three states now wait on the same missing piece** -- the generic
minimum-liability election pass already queued for the WI Act 15 retirement
election: AL separate returns, AR's Low Income Tax Tables (a taxpayer
election used instead of the schedule AND instead of any deduction, and AR's
largest known difference), and MS/AR per-spouse column returns. Building it
once would close the largest remaining gap in four states at a stroke.

**The pension-source PUF limit now binds in five states**: the NY
government-pension exclusion, MO's public pension exemption, AL's IRC 414(j)
defined-benefit exclusion, MA's US/MA public contributory pension exclusion,
and (differently) NJ's 403(b)/457 deferral treatment. All clear with the
Tier 1 imputation.

**The R6 fed-ded batch is COMPLETE: MO, AL and OR encoded 2026-08-18.** The
shared `st_ded.fed_tax_ded` component carries all three, with the base set at
1040 line 22 (`liab_bc - nonref`) so the alternative minimum tax is in and
self-employment tax is out. The three states' worksheets name genuinely
different refundable-credit lists, so each credit is its own flag: MO
subtracts the earned income credit, refundable AOC and net premium tax credit
but NOT the additional child tax credit; AL subtracts the earned income
credit, ACTC and refundable AOC but NOT the net premium tax credit, and adds
the net investment income tax back; OR subtracts the AOC and premium tax
credit, strips the excess advance premium tax credit repayment out, and is
the only one of the three that does NOT subtract the earned income credit.
(In Alabama the federal earned income credit therefore RAISES state tax.)
The ceiling is a flat filing-status-mapped cap in MO ($5k/$10k), uncapped in
AL, and in OR an indexed cap cut in five frozen AGI bands. Other new
generics: `st_ord.combined_split` (MO pools deductions then splits taxable
income by each spouse's share of state AGI, rounded half UP to whole percent,
and runs the schedule per spouse — distinct from KY's `combined_sep`),
`st_ded.std_equals_federal`, `st_ded.item_add_payroll` (MO and AL both let
employee FICA into the itemized base), `st_ded.retire_exempt_*` (MO takes its
retirement exemption as a deduction AFTER state AGI, which is load-bearing
because state AGI is what MO's federal-deduction bands read),
`st_agi.bus_excl_share`, `st_ded.std_po_amount_per_step`/`_floor` (AL's
dollar-stepped standard deduction slide), `st_exempt.dep_tier_*` (AL's
AGI-tiered dependent exemption), `st_agi.ob_ira_share` (AL taxes IRAs but
exempts defined-benefit pensions), and
`st_credits.eitc_match_young`/`_max_age` (OR's higher rate for a child under
three). `sched_tax_at()` is now base-amount aware so MO's combined split uses
the published chart. **Three known differences are material and share one
root cause:** MO's public pension exemption and AL's IRC 414(j)
defined-benefit exclusion are both unmodellable because the pension-source
split is unobserved in the PUF (the NY government-pension precedent), and
both clear with the Tier 1 imputation. **OR's kicker is not modeled at all** —
it is a percentage of PRIOR-year liability, unobservable in a cross-sectional
model, and it recurs every other year. AL's overtime exemption is a second
OR-sized hole: it appears in no Form 40 booklet because it runs through
withholding and Schedule W-2. See source_packets/mo.md, al.md and or.md.

**The R6 batch-C transcription set is COMPLETE**:
KS/DE/RI/WV/NM/VT/OK/DC/NE (2026-08-13/-16) and HI/ME (2026-08-17, closing
the batch — HI added the 7.25% alternative capital-gains machinery, the
threshold-gated SALT disallowance, the banded per-person credit table and
the Act 46/163 enacted future schedules; ME added the pension phase-out,
CDCC refundable-cap and ctc_po_step generics, the reordered cap-then-
phase-out itemized flow, and the sales tax fairness credit as a dense
table — see source_packets/hi.md and me.md; both corrected review-§2.1
assumptions, notably that ME's EITC has been refundable since 2016 and its
STFC is income-keyed, not Tier-1-blocked). Companion docs in this directory:
`state_tax_implementation_plan.md` (the design of record, amended in place),
`STATE_ENCODING_REVIEW_2026_08_11.md` (coded-states review: holes,
archetypes, completion roadmap),
`state_weights_fit_issues.md` (the engine root-cause record),
`state_tax_model_research_notes.md` (original evidence base),
`nonfiler_residual_design_jii.docx` (the narrative case for the residual
non-filer methodology, JI Aug 2026 — renamed 2026-08-18 from
`Non-Filer Proposal.docx`; note it is a *different document* from the `.md`
below, despite the similar name), `nonfiler_residual_design.md` (its
implementation-level companion, amended in place) and
`nonfiler_residual/04_findings.md` (the Stage D diagnostic record).
Superseded docs live in `archive/` with a README explaining each — including
`state_weights_ml_alternative.md`, whose A/B-bake-off premise the Phase 1
sweep reframed into prior-only-vs-joint-fit.

---

## Done

**Phase 0 — plan and review.** Seven-phase plan committed; full code-verified
review resolved eight findings (SALT post-workaround semantics decided,
`st_agi.conformity_year` added for fixed-date IRC conformity states,
cross-row `states` validation relocated to `parse_globals()`, jurisdiction
set fixed at 53 = 51 modeled + PR/OA, `filing.yaml` schema home, torch
question closed). Plan later extended with locality weights (§2.6) and
LODES-based cross-border liability (Phase 7).

**Phase 2 — parameter schema + pilot states** (`3d0848853`, `bbd285687`).
`build_state_tax_law()` reuses the federal YAML machinery with `st_`
prefixing; `parse_subparam()` tolerates `reference` citation keys (indexed
subparams included — regression-tested). IL, CO, NY encoded 2017-forward
from primary sources with a citation on every subparameter, including the
2017 IL blended rate, CO's TABOR temporary rates and three deduction-addback
regimes, NY's full rate-schedule history through the enacted 2033 reversion,
NY Pease thresholds pulled per-year from the instruction PDFs, and the
2025/2026 credit restructurings in both CO and NY. Encoding conventions
locked: 10-element uniform schedules, anchor-at-2017 year lists,
feature-absent = NA.

**Phase 3 — state calculator** (`41e818d2f`). `src/calc/state/` implements
`do_state_taxes()` + `calc_st_{agi,ded,exempt,txbl,tax,credits,liab}` under
the standard calculator contract, including the NY tax-benefit recapture
from the published worksheet identity (reproduces the printed constants),
the NY §615(f)/(g) itemized limitation tiers, both Empire State child credit
regimes, CO's tiered CTC + FATC, and the state-filer flag. Verified by 12
hand-computed form-worksheet cases plus smoke/subset-state grids.

**Phase 4 — orchestration + outputs** (`38e3bf201`, `4d1a65560`). Runscript
`states`/`state_tax_law`/`state_detail` columns; per-state loop inside
`run_one_year()` (no SLURM worker sync needed); `totals/state.csv`,
`supplemental/state_rev_est.csv`, compact per-year state detail matrix,
stacked state reports; SLURM 3a/3b/4 synced. **Acceptance verified on real
data: with-state federal outputs byte-identical to without-state.** Runs on
uniform placeholder weights until Phase 1 lands (state levels not yet
meaningful; all contracts real).

**Phase 1 — state weights (engines and data done; harness remains).**
- Data: shared stores built and wired — IRS-GEO mirror (HT2 2012–2022,
  percentile, county, ZIP + SOI docguides + per-family change notes;
  public repo johniselin-budget-lab/IRS-GEO, data on the cluster share) and
  the IPUMS ACS extracts. `read_ht2()` ingests the full 24-series target
  map; `read_acs_extract()` handles the fixed-width format, implied
  decimals, and the INCTOT sentinel (`5d92e5763`, `c27cb1c99`).
- Target assembly (`55ee03ec2`): filer HT2 state×stub targets + non-filer
  ACS cells, share-normalized to PUF national totals (levels are fixed by
  construction; only geography is calibrated).
- Engine root-cause (`51a6eefc2`): invariant leak closed (negative HT2
  targets → assembly block + assertions; verified 1.1e-15); multi-series
  IPF non-convergence proven STRUCTURAL (one constraint per cell is its
  valid class — counts-only converges in 1 iteration to 100%).
- Vectorized joint fit (`cf3cd19ee`): counts-backbone IPF prior →
  exponential-tilting gradient engine; 2022 full fit hits 82.9% within 2%,
  MARD 1.43%, 7.5 min under sbatch, loss still descending.
- Reconciliation/validation battery (`7bf614823`, `418795efb`, `726de2d54`,
  `7239c26a6`): model-free individual-level IRS-vs-ACS coverage (married
  85.6% / single 77.6% / children 109.2%, wide state spreads), wage dollars
  (96.6% vs ACS — the high anchor), QWI structural check (workplace-basis
  commuter signature), LODES residence-basis fix (RAC + OD commuter matrix;
  DC residents hold 31% of DC jobs), and the candidate demographic target
  dimensions (QWI sex×age, ACS marital×age).

**Ops learned the hard way** (recorded in the issues doc): weight fits
OOM-kill on the login node (~7 GB cap) and piping masks the kill (pipeline
exit = tail's); run under `sbatch` with inputs staged on NFS scratch
(`/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`).

## Left to do

1. **Phase 1 close-out** — the §4 comparison harness: tune the joint fit to
   the ≥99%-within-2% bar (steps/lr schedule/per-series λ; SALT and
   EITC/AGI families carry the residual), β sweep, untargeted validation on
   the QWI/ACS demographic cells, downstream IL/CO/NY liability under
   candidate weights; then the `state_weights_{year}.csv` writer for
   2014/2016–2022 (2013/2015 HT2 gaps: interpolate or skip, decide),
   projection-year carry-forward, and the dispatcher swap off
   `placeholder`. The prior-only vs joint-fit comparison IS the reframed
   A/B decision.
   **⚠ SEQUENCING (JI, 2026-08-16): the non-filer rework lands BEFORE this
   swap-in** — see item 1b — so the swap-in fit happens once, on upgraded
   margins, rather than fit-on-v0-then-re-fit. Do not close Phase 1 first.

1b. **Non-filer residual rework** (NEW section, added 2026-08-18). The
   non-filer population is the one input to the weights fit that is anchored
   to nothing: Tax-Data appends ~27.6M (TY2022) units from PSZ/DINA at DINA's
   own uncalibrated weights, and the state fit places them using ACS margins
   built from a v0 filing rule that over-assigns filers ~7% nationally with a
   20pp state spread. **Stage D is DONE and committed** (`4783dc3e9`,
   2026-08-16): residual anchors built for TY2017/2022, diagnostic tables
   T1–T7 run, decisions D1–D6 resolved, findings in
   `nonfiler_residual/04_findings.md`. What it found, in one line each —
   F1 the non-filer mass is ~15–25% short (32.4M adults vs a defensible
   38–41M); **F2 the age composition is inverted** (8.9% of non-filer adults
   at 18–25 vs the anchor's 24.2%; 42.9% at 65+ vs 25.1%) and this is the
   single most consequential defect for the weights, whose non-filer cells key
   on age band; F3 investment income is identically zero (0.0% with interest,
   dividends or gains, vs Pub 5785's 14/9/4%); F4 the aging path drifts with
   no return-count discipline after 2019; F5 the v0 non-filer margins run
   0.78× (DC) to 1.51× (SD) of the anchor and the current fit reproduces them
   *exactly*; F6 group quarters are untreated and are 17% of the national
   residual but 42% in SD; F7 above-threshold non-filers are 10.6–11.9M units
   and SE-shaped.

   Remaining work, in order: **pre-flight** (resolve the Tax-Data vintage
   discrepancy in the design memo §2.1 vs `interface_versions.yaml`; settle
   the `age_band()`-vs-`a16_band()` reconciliation) → **research pass A**
   (ASEC unit/income construction — still open) → **GQ treatment**
   in `build_acs_margins()` (decision-independent, ships first) → **filing
   model on the CPS ASEC**, transferred to the ACS → **Tax-Data rework**
   (composition, national calibration, aging) as V1/V2/V3 vintages →
   **federal validation battery** → **state-weights margins/targets + re-fit**
   → swap-in per item 1.

   Two decisions worth carrying here. **The filing model is estimated on the
   CPS ASEC, not the ACS** — Cilke estimated on the ASEC, so it is the native
   environment and the ACS is the destination; the recalibration burden moves
   to an explicit, measurable transfer step. **ASEC data comes through the
   shared extract machinery** — check `raw_data` for a registered CPS/ASEC
   family first, and if absent add one through the same common IPUMS download
   machinery that maintains `ACS/acs_common`, so Affordability-Index draws on
   the same file.

   **Research pass B is DONE (2026-08-18), and `nonfiler_residual_design.md` §3.2 was
   REWRITTEN around it** — that section is now the authority on the filing model; the
   evidence is in `nonfiler_residual/05_filing_model_literature.md`, references in
   `nonfiler_residual/resources/filing_model_refs.bib`. Four changes:
   (a) **Cilke (1998) is replaced by Mok (2017), CBO WP 2017-06, Table 14** — 14 group
   filing probits with coefficients and SEs, estimated on the 2007 CPS ASEC linked to
   the IRS Individual Master File (TY2006), same design as Cilke but 16 years newer,
   with per-cell filing rates as ready-made calibration targets. Fit Mok, keep Cilke as
   the comparison, and test Mok's rank-and-cut-within-cell assignment against
   intercept-calibration-plus-uniform-draw (correct-classification is *similar* under
   both — do not expect a large gain). **Table 14 VERIFIED against the PDF 2026-08-18**
   (JI's copy in the Affordability literature folder): all 14 equations, cell Ns and
   filing rates check out exactly. Two transcription warnings: **Panel E's columns run
   "Age 65 or Older" FIRST**, reverse of the intuitive order and reverse of what text
   extraction returns — transcribe from a rendered image; and **Mok's CPS frame excludes
   the institutionalized and military-barracks populations**, so her coefficients do not
   cover the GQ records our PUF universe includes (score them under a stated assumption
   and report GQ separately — same caveat applies to Cilke).
   (b) **Why a survey model is still right for us, stated explicitly.** Treasury/IRS/JCT
   all *abandoned* survey filing models, but their replacements are information-return
   microdata — a data-access story, not a verdict on the method. The distinction that
   matters: admin *microdata* is closed to us, admin *published tabulations* are what our
   anchors already run on. And Mok's regressors are all CPS-native, so the linkage bought
   the identification while scoring needs only survey variables — we get the benefit of
   the linkage without the linkage. No peer model without admin microdata (TPC,
   PolicyEngine, Census, PWBM) has anything structurally better.
   (c) **A bias we inherit regardless of coefficients:** ASEC non-filer income falls short
   of third-party-reported income — reweighted, the ASEC reaches 42.0M against a 50.7M
   admin target, **~17% short**. It enters through the threshold test (a function of
   reported income), biases toward **too few** non-filers — the same direction as F1 — and
   its mitigations (Erard et al. 2014 on ASEC SS/pension under-reporting; Pub 5785 receipt
   ceilings; carrying the 17% in the tolerances) are now load-bearing.
   (d) **The dependent/MFS deferral is reopened.** TCJA did *not* sharply cut filing
   requirements for single/MFJ (+15%, since zeroed exemptions offset the bigger standard
   deduction), but the **dependent** threshold rose 89% and **MFS** collapsed to $5.
   Dependents are Cilke's largest group (36.4% of his non-filers) and Mok estimates a
   dependent equation directly. Decide explicitly; record the reason either way.
   **Pub 5785 stands** (no successor edition), with Treasury's Jan-2025 special study
   (50.343M TY2022 non-filers) and OTA TP-12 as newer official reference points.

   **Two blockers, one on JI.** (a) ssa.gov 403-blocks the cluster egress IP,
   so `raw_data/SSA-OASDI-SC` and `SSA-EEDATA-SC` exist but are empty — the
   OASDI and covered-worker state margins need a manual download on a
   workstation (each store carries a `README_MANUAL_DOWNLOAD.md`; then re-run
   scripts 01→02→03). They gate the state age layering (D6) and the wage
   margin. (b) `resources/cilke_coefs.csv` is not transcribed, and should not
   be until the currency check above finishes.

   **Two model-side gaps the rework cannot fix by itself**, recorded so they
   are not discovered late: there is no `become_filer_eitc` (only CTC and
   rebate have one), and `become_filer_ctc` requires `qual_ei == 0` exactly —
   so the earnings-bearing non-filers this work creates still cannot claim
   EITC, and their credits are multiplied out of every total by the `* filer`
   gate. An EITC reform will score identically across vintages. Also note
   `get_pr_totals()` has **no** filer gate, so raking non-filer weights up
   raises baseline payroll receipts, which nothing currently benchmarks.
2. **CO child-care expenses credit** (DR 0347) — researched and encoded
   (TODO carried in `co/credits.yaml`); CO 2026 rate revisit after the
   TABOR certification (~Sept 2026).
3. **Phase 5 — cross-model validation harness (record level: BUILT
   2026-07-18; per-state triage remains).** Harness at
   `other/state_tax_research/cross_model/` (see its README):
   record × state × year design on unweighted PUF samples, TAXSIM-35
   (usincometaxes WASM, 2017–2020) + PolicyEngine US (pinned venv, 2021+),
   $15/$100 match rates plus a federally-aligned "clean" metric, stage
   diagnosis from TAXSIM v32–v40, machine-readable known-differences list
   with record-level predicates. Fixing the federal `taxsim_crosswalk`
   took 12 latent-bug repairs plus income-concept alignment (other_gains,
   part_se double-count, se_health sign, state_ref, gssi rename). Results
   so far: 6 no-tax stubs + NH + TN + IL validate at ~100% clean match;
   the other 13 broad-IIT states sit at 27–86% clean match@$100 with
   per-state stage histograms as punch lists (dominant stages: exemptions
   for AZ/IN, deductions for CA/VA/NY/KY, state AGI for
   CO/ND/SC/GA/MI/NC/SC — the fed-taxable-start states' v32 labels are
   partly a TAXSIM-semantics artifact, see README caveat). PA/ID initial
   runs 2026-07-23: PA 60-63% clean vs TAXSIM (dominant wedge: TAXSIM nets
   losses across PA's floored income classes; TAXSIM DOES model Tax
   Forgiveness — verified) and 81-88% vs PE; ID 59-71% vs TAXSIM (PBF
   filing-edge ±$10, CTC −$205 cluster) and 43-58% vs PE, where
   PolicyEngine does not net the grocery credit into state_income_tax
   (whole-window annotate; candidate exclusion). ID's fed-taxable wedge
   RESOLVED (2026-07-23 dive, results/reports/id.md): non-itemizers agree
   (84.5-93.6% at wedge=0 — base machinery confirmed); the wedge is
   TAXSIM's itemizer deduction rebuild (2018+: v35 median $21.8k below
   ours + 36% spurious standard elections, consistent with removing full
   computed state tax instead of the Form 40 line 14 capped
   property-first formula; 2017: the reverse, cor −0.745 with the SALT
   income component). QBID-omission hypothesis refuted; TAXSIM
   care-deduction cap (> the form's 3k/6k) is a candidate upstream issue.
   2026-08-11: `cross_model_states()` had omitted MD/MN/WI from every
   class, so their triage reports were silently never generated — fixed,
   and `results/reports/{md,mn,wi}.md` emitted from the committed
   summary.csv (their stage-diagnosis tables need a full harness rerun;
   `results/raw/` is not committed).
   **KY triage 2026-08-11 (worst state, 0.273-0.476 TAXSIM clean): root
   causes were OURS and are fixed** — 2017 was encoded flat 5.8% instead
   of the graduated 2/3/4/5/5.8/6% schedule; married units got one
   standard deduction where KY combined returns give one PER SPOUSE (new
   generic `st_ord.combined_sep` machinery: per-column schedule + own
   std/income-share itemized, floored at zero, lower-of joint/combined —
   reusable for other combined-filing states); std vintages 2017-2021
   were shifted one year; and the Form 740 personal tax credits ($10
   regular 2017, $40 aged/blind all years, applied before the family-size
   credit) were missing. Tests KY-1..KY-11 (from 1); two TAXSIM bugs
   probe-verified and pre-registered (2017 double std ded per spouse;
   unconditional 2x std for one-earner couples); harness rerun pending.
   **Review item #9 CLOSED 2026-08-11** (the last open 2026-07-17 code
   review item): (a) worksheet-coverage layer — any credit family the
   smoke grid activates for a state must be exercised by a hand-computed
   case (self-maintaining, no param-name map); found and filled three
   gaps (IL child credit, NY + WI dependent-care) as IL-5/NY-5/WI-8.
   (b) continuity sweep — single wage-only filer, $500 AGI steps to
   $300k, all 24 states x 2017/2024, per-state jump allowances each
   citing its published cliff (IL exemption cliff, VA no-tax-below, OH
   base amounts, CT Tables A/D, KY Table C, MD exemption bands). The
   probe surfaced two live items: MN's 31.5-35k slope is exactly 5.35% +
   the UNVERIFIED 9% childless WFC phase-out (flagged in mn.md), and NY
   shows a +$327 jump at the 215,400 bracket entry in the recapture zone
   (tracker note; verify vs the IT-201 worksheet). (c) NH/TN/WA now in
   the smoke grid (24 states), asserting on net individual liability.
   **R1 triage sweep completed locally 2026-08-11** (probe-driven, via
   the local TAXSIM WASM): KY/GA/SC/IN/MN fixed (see tracker rows and
   dated commits), CO/MI cleared with negative-result or
   external-model-bug KD rows. Everything that needs the harness
   machine was queued in `cross_model/HARNESS_RERUN_2026_08_11.md`.
   **Harness-machine batch EXECUTED 2026-08-11** (runbook stamped; see
   its header for deviations): ALL-states reruns both windows confirmed
   the R1 fixes directionally (KY 2017 clean 0.27→0.64, IN 0.84/0.94,
   GA exemptions stage cleared) — no cell reaches 95%, no tracker
   flips. MI's $386 mass was NOT the Tier-2 row: TAXSIM nets the
   MI-1040CR-7 home-heating credit on a collapsed $1.01 household
   income (issues-doc T6, exclude KD). Every PE-window dig resolved to
   one class — one-time rebates booked into eligibility-year
   `state_income_tax` (issues-doc P5): NY 2023 (2025 inflation refund),
   VA 2021/2023/2024, GA 2021, AZ 2021, and CT 2022 (child tax rebate),
   each excluded via a `pe_*` rebate export predicate; ID's PE window
   promoted to whole-window exclude (SNAP-prorated grocery credit + $10
   PBF omission verified in package source). GA's PE dig also exposed
   OUR HB 593 std-deduction anchor a year early (fixed, GA-5). The NY
   +$327 recapture jump verified WORKSHEET-TRUE (IT-201 hand
   computation exact; PE concurs within cents).
   **R1 validation close-out 2026-08-11 (`58fd1211c`): all 18 broad-IIT
   states now carry full residual attribution** — every clean-mismatch
   cluster maps to a KD row (50 rows, 19 excludes), a filed issue
   (T1–T9/P1–P5), or an encoding fix. Close-out rerun: **WI clears the
   95% bar across its whole TAXSIM window (0.952–0.972) — the first
   broad state since IL**; ND's PE window jumps to 0.948/0.927 (2021/22)
   on the HB 1515 encoding; NC 2018 0.535→0.741 on the SB 99 vintage
   fix; UT TAXSIM 0.93 (2018–20) post-exclusion with its PE window
   passing outright. Lesson pinned in the MD KD row: exclusion
   predicates must key on where the bug EXCEEDS the match tolerance —
   sub-$100 bugs get annotate rows (an over-broad MD exclude briefly
   depressed the 2019 cell by removing passing records). The close-out sweep of the seven
   previously-unattributed states (UT/MD/WI/NC/CT/ND/PA) found four
   more OUR-side bugs (NC SB 99 std vintage + missing TY2017 child
   credit; ND HB 1515 relief credit wrongly documented-not-modeled; WI
   $500 capital-loss addback wrongly called not-separable; MD missing
   dependent-care subtraction) and four more probe-verified TAXSIM bugs
   (UT retirement credit to any SS recipient unphased; MD 2019
   std-minimum-everywhere; WI stale 2017-18 brackets; CT Table C
   recapture overshooting its cap), plus the mstat/dependents HoH
   conflation (UT ±$464). Harness records now carry
   age1/age2/gross_ss/n_dep so KD predicates key on exposure sets.
   Remaining: aggregate benchmarks vs HT2 total tax (weights-blocked),
   revenue-agency comparisons, and sending T1–T9/P1–P5 upstream.
4. **Phase 6 — 50-state rollout** by structural family (no-tax stubs → flat
   fed-AGI → graduated fed-AGI → fed-taxable → own-base → federal-
   deductibility), CA first (CalEITC as the credit-schema acceptance test;
   CA CPI indexation series). 2026-08-11: the 21 remaining jurisdictions
   are now classified and batch-sequenced in
   `STATE_ENCODING_REVIEW_2026_08_11.md` §2.1/R6 (preliminary classes also
   in each tracker row): eleven plain graduated-fed-AGI transcription
   states; MO/OR/AL behind one new `fed_tax_ded` component; NJ/MA/AR/MS on
   the PA own-base machinery (NJ largest, first); IA/LA/MT as multi-regime
   restructure states. Only genuinely new machinery for the remainder:
   federal-tax deductibility and per-class rates on `ob_*`.
   **MN encoded 2026-07-23** (28th jurisdiction, ~$15bn IIT — largest since
   the pilots): eight new generic components (two-tier deduction
   limitation incl. the standard deduction, share-based exemption
   phase-out, sliding + stepped SS subtraction regimes with automatic
   greater-of, non-itemizer charitable share, combined CTC+WFC credit
   with joint phase-out, two-earner marriage credit on single-schedule
   shares, dependent-care income cap, net-investment-income surtax).
   Fed-taxable start 2017 → FAGI 2018+ (the switch was TY2018, not 2019);
   the TY2018 TCJA-nonconformity year encoded as TCJA FAGI + MN's own
   pre-TCJA stack — and it validates BEST of the TAXSIM years (67.3%
   clean). Initial cross-model: TAXSIM 49-67% clean (base machinery
   confirmed: median taxable wedge 0, exemptions to $6; residuals are
   std-deduction detail, WFC schedule edges, credit point masses), PE
   75-82% (2024 low = renter's credit, excluded by design). Tests
   MN-1..MN-11.
   **PA + ID encoded 2026-07-23** (25 → 27 jurisdictions): PA is the first
   OWN-BASE state — new generic components landed for class-share bases
   with per-class loss floors (`st_agi.ob_*`, reusable for NJ/AL/AR/MS),
   the poverty-forgiveness credit family (Schedule SP), the per-person
   credit family (ID grocery credit), and a per-return excise (ID PBF —
   never repealed, contrary to prior belief). ID is fed-taxable with a
   CPI-indexed flat-tax zero bracket (not the statutory $2,500/$5,000),
   MFS on the single schedule and HoH on the MARRIED schedule. Research
   surprises: PA enacted a TY2025 state EITC (Working Pennsylvanians Tax
   Credit, 10% federal match, refundable) and a TY2025 student-loan
   deduction; ID's CTC sunsets after TY2025. Worksheet tests PA-1..7b,
   ID-1..7 pass; cross-model triage started (see rollout tracker rows).
   **MD + WI encoded 2026-07-24** (29th and 30th jurisdictions). MD is
   STATE-level only (the county piggyback, 2.25-3.3%, is deferred to the
   locality phase): graduated schedules incl. the TY2025 BRFA brackets and
   2% capital-gains surtax, 15%-of-AGI standard deduction, dual EITC
   election, CTC, senior credit; the PE harness leg now requests
   `md_income_tax` (state-only) because PE's generic `state_income_tax`
   bundles county tax. WI: sliding standard deduction, 30% LTCG exclusion,
   itemized-deduction-as-credit, married couple credit, child-count-keyed
   EITC (4/11/34/0%), school property tax credit; homestead credit omitted
   (PE includes it — a one-sided low-income divergence). Initial
   cross-model: MD TAXSIM 58-70% clean (2019/2020 TAXSIM std-ded values are
   pre-registered TAXSIM bugs), PE 84-89%; WI TAXSIM 76-92% (2019/20 near
   the bar), PE 72-78%. Worksheet tests MD-1..9, WI-1..7 pass.
   Horizon note (2026-08-11): NC and IN encode enacted law through TY2026
   while other states stop at TY2025 and carry forward — both are
   deliberate (NC/IN have enacted future rate steps) but the convention
   should be picked explicitly at the next rollout batch: either encode
   every state's enacted 2026 law (SC's H.4216 restructure and ND's
   published 2026 schedule are the known gaps) or document the two
   exceptions per state.
5. **Data extensions/imputations** — scoped 2026-07-24 in
   `state_data_imputation_plan.md`: tenure/rent/property-tax (homestead
   family incl. the MN 2024+ on-form renter credit), pension source
   split (the recurring elderly gap), household-resources income,
   deferral wage-base, dependent detail — tiered, ACS-driven,
   sequenced after the Phase 1 weights swap-in.
6. **Phase 7 — later scope**: coupled federal↔state iteration + sales-tax
   election imputation; frozen-base mechanics for fixed-date conformity;
   locality weights from SOI county data (§2.6; NYC first); cross-border
   wage taxation via the LODES OD matrix; state MTRs and combined-MTR
   behavior; state distribution tables; pre-2017 law; population-projection
   aging of weights.

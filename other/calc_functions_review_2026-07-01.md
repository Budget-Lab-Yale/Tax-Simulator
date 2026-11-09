# Calc-layer review — correctness pass over all `src/calc/functions/`

_Generated 2026-07-01 on branch `wealth`. Scope: all 26 files under
`src/calc/functions/` (credits/, deductions/, income/, tax/) plus the
orchestrator (`src/calc/do_taxes.R`) and shared helpers (`src/calc/utils.R`).
Method: 3 parallel domain sweeps (income+deductions, credits, tax/liability)
each with full pipeline context, followed by direct verification of every
BUG-tier claim against the source, git history, and (where relevant) active
YAML configs — not just re-reading the sub-agent's writeup. do_taxes.R/utils.R
were read and checked directly rather than delegated._

_**AMENDED 2026-07-01** after an independent second-pass verification: every
claim re-checked against the source, the baseline YAMLs, git history in both
this repo and Tax-Data, and the pinned Tax-Data vintage (`2026060918`) itself.
Verdict changes: **#1 refuted as stated** (no real mortgage-year/balance data
exists — reframed as dead machinery, same tier as #9); **#7 refuted outright**
(code matches the OBBBA statute — do not apply that fix); **#2 blast radius
corrected** (baseline sets `po_sequential: 0`, not 1 — bug is live only in the
`booker_repeal_*` tax-expenditure tests). Findings 3, 4, 5, 6, 8, 9, the
lower-confidence item, and the doc-nits all survived verification, with data
checks added inline. One claim in the simplification section (credit
self-limiting drift "doesn't change any output") was found to be wrong and is
corrected below._

_**FIXED 2026-07-01 (same day):** the five "fix now" items were fixed, unit-
tested, and their impact measured on full-sample baseline + reform runs —
commits `178508678` (#4), `62be3e4a1` (#6), `f4ba6f0ea` (#5), `0540f1cf2` (#3),
`205ee8e15` (#8). Measured baseline revenue error: **#5 was understating income
tax by ~$8.4B/yr** (72% of 1250-recapture revenue missing); #6 +$0.12B/yr;
#3 +$3.9M/yr; #8 −$1.3M/yr; #4 revenue-neutral but `liab_pr_ee` was understated
$17.4B/yr (2026). Reform deltas were barely affected (kg_top_5pp ≤0.001%,
sd_bump_10k ~0.04%). Full quantification and artifacts:
`other/calc_fixes/RESULTS.md`. Still open: #2 (sequential CTC, test-suite
only), #1/#9 (document dead levers), #10 + `simple_filer` nit (ask author)._

**Unlike the two prior simplify-only reviews, this pass found real correctness
bugs.** Findings are grouped: confirmed bugs (highest confidence first),
revised/refuted items retained for traceability, a lower-confidence item worth
a second look, doc-only issues, then simplification opportunities. "Confirmed"
below means the mechanism was re-derived from the actual code/config — and, as
of the amendment, independently re-verified including against the underlying
microdata where blast radius depends on it.

---

## Confirmed bugs

_(Second pass: #1 and #7 below did NOT survive verification — they are kept in
place with strikethrough titles for traceability. #2's blast radius was
corrected. The rest are confirmed, now including data-level checks.)_

### 1. ~~Mortgage-interest vintage limitation is fully disabled~~ — REVISED: the entire MID balance limitation is inert by construction (dead machinery, not a live bug) — `item_ded.R:129-130`
```r
first_mort_year = Inf,
second_mort_year = Inf,
across(.cols = c(first_mort_year, second_mort_year),
       .fns  = ~ case_when(
                    is.na(.)                        ~ Inf,
                    . <= item.mort_bal_limit_years1 ~ item.mort_bal_limit1,
                    ...
```
**Original claim (wrong):** the `= Inf` lines shadow real origination-year data,
forcing every mortgage into the post-2017 $750K bucket and understating the MID
model-wide.

**What's actually true (verified against Tax-Data source and the pinned vintage
`2026060918`):** there is no real data to shadow. Tax-Data has *never* imputed
mortgage years or balances — `first_mort_year = 0`, `first_mort_bal = 0` are
explicit placeholders in Tax-Data `src/imputations/placeholders.R`
("Placeholder imputations that will be revisited when time allows"), and the
pinned vintage ships all-zero years *and balances* (0 nonzero in a 5,000-record
sample), while `first_mort_int` is populated (`= int_exp`, i.e. reported
Schedule A interest). With zero balances, `deductible_share =
pmin(1, bal_limit / 0) = pmin(1, Inf) = 1` **regardless of which vintage bucket
any record lands in** — no mortgage is ever limited, with or without the `Inf`
lines. And because `int_exp` is *reported* (post-limitation) deductible
interest, baseline MID is approximately right by construction. The commit
(`874ebd85b`, whose whole point was a Tax-Data interface bump) reads as
deliberate neutralization, not a leftover.

**Actual issue (same tier as #9, reform-authoring trap):** the whole
`mort_bal_limit`/`mort_bal_limit_years` machinery is a dead lever. A reform
YAML that changes mortgage balance limits is a silent no-op (share is always
1). Side note: a reform setting a balance limit to literal 0 would produce
`0/0 = NaN` and poison downstream columns.

**Fix:** do NOT delete the two `= Inf` lines — that changes nothing today
(year `0` routes to the pre-1988 `Inf`-limit bucket, which also yields share
= 1) and leaves a latent everything-is-grandfathered bug if real data ever
arrives. Instead: document the balance limitation as off-model pending real
Tax-Data imputation of years/balances (the `placeholders.R` TODO), and treat
reforms to these parameters as unsupported until then.

---

### 2. CTC sequential phaseout's second tier is a mathematical no-op — `ctc.R:162-172`
```r
value1 = if_else(ctc.po_sequential == 1, pmax(0, max_value_combined - po1_reduction), ...)
value2 = if_else(ctc.po_sequential == 1, pmax(0, value1 - excess2 * po_rate2), ...)
value1 = if_else(ctc.po_sequential == 1, value1 - value2, value1)
```
Let `X = max_value_combined - po1_reduction`. Then `value2 = f(X)` for some
`f`, and the final `value1 = X - value2`. So `value1 + value2 = (X - value2) +
value2 = X` **identically, for any `value2`** — the second threshold's
`excess2 * po_rate2` term cancels out of the total no matter what it is. Since
`value1`/`value2` are only ever consumed as a sum (`ctc_nonref`,
`remaining_ctc`), the entire second-tier phaseout is inert when
`po_sequential == 1`: the credit reflects only the first threshold.

**Confirmed:** algebraically (trivial identity `a - (a-b) + (a-b) = a`); both
downstream consumers (`ctc_nonref`, `remaining_ctc`) use only the sum, and the
MFS-eligibility `across()` scales `value1`/`value2` by the same factor, so
there is no channel through which the second tier survives.

**Blast radius — CORRECTED on second pass:** the original claim that
`po_sequential: 1` is set in baseline is wrong. Baseline `ctc.yaml` sets
`po_sequential: 0`, as do all the main `public/booker-kypa/**` configs and
`tests/booker_ctc_tcja_ext` (the 15 grep hits mostly *contain* the parameter
set to 0). Only the seven `tests/tax_expenditures/booker_repeal_*/ctc.yaml`
configs flip it to 1 (from 2026). So this does NOT touch baseline or the
headline Booker-KYPA scores — it silently drops the second tier only in the
tax-expenditure test suite, and would bite any future reform using sequential
mode.

**Fix:** don't back-derive `value1` from `value2`; compute the true combined
post-both-thresholds total directly (`pmax(0, X - excess2*po_rate2)`) and
allocate `value1`/`value2` from that for reporting, rather than reassigning
`value1` from itself.

---

### 3. CDCTC earned-income cap effectively doubles when young + old dependents both present — `cdctc.R:90,101,130`
```r
ei_limit = pmax(0, if_else(filing_status == 2, pmin(ei1, ei2), ei1))
young_qual_exp = pmin(young_qual_exp, ei_limit)   # line 101
old_qual_exp   = pmin(old_qual_exp,   ei_limit)   # line 130 — same ei_limit again
```
`ei_limit` is meant to be a single cap on *total* qualifying expenses (the
lower earner's earned income, per statute). It's applied in full to
`young_qual_exp` and then, independently, in full again to `old_qual_exp` —
even though `old_qual_exp`'s expense base already subtracted the (capped)
`young_qual_exp`. Example: `ei_limit = $2,000`, one young + one old dependent,
`care_exp = $10,000`, per-dependent expense cap `$3,000` each → young caps to
$2,000, old *also* caps to $2,000, for $4,000 combined — double the true
$2,000 earned-income limit. Note the per-dependent expense cap (`n_old =
pmin(n_old, cdctc.n_dep_limit - n_young)`, line 87) correctly treats young/old
as one shared pool; the `ei_limit` application just didn't follow that pattern.

**Verified (second pass):** live in baseline — `cdctc.yaml` sets
`young_age_limit: 4` / `old_age_limit: 12`, so both buckets are active under
current law (ages 0-4 vs 5-12). Bites units with dependents in both buckets
whose lower-earner income is below combined qualified expenses.

**Fix:** apply `ei_limit` once to the combined total (e.g. cap
`young_qual_exp + old_qual_exp` at `ei_limit` with young stacking first, matching
the existing stacking convention), not to each bucket independently.

---

### 4. Employee/employer payroll tax split doesn't reconcile with the total — `pr.R:210-217`
```r
liab_hi    = ... + liab_add_med          # line 213 — includes Add'l Medicare Tax
liab_pr_ee = liab_fica_oasdi_ee1 + liab_fica_oasdi_ee2 +
             liab_fica_hi_ee1    + liab_fica_hi_ee2 +
             liab_seca_oasdi_ee1 + liab_seca_oasdi_ee2 +
             liab_seca_hi_ee1    + liab_seca_hi_ee2      # line 214-217 — no liab_add_med
liab_pr    = liab_oasdi + liab_hi        # line 222 — correctly includes it via liab_hi
```
Additional Medicare Tax is by statute 100% employee-side (no employer match),
so it belongs in `liab_pr_ee` by definition. `liab_hi` and `liab_pr` correctly
include it; `liab_pr_ee` (and `liab_pr_er`) omit it. Result: `liab_pr_ee +
liab_pr_er ≠ liab_pr` — the gap is exactly the Additional Medicare Tax amount.
Aggregate revenue (`liab_pr`) is unaffected, but `liab_pr_ee` is emitted on
every detail file and consumed in post-processing, so any ee/er breakdown is
systematically wrong.

**Verified (second pass):** consumers confirmed — `liab_pr_ee` is read in
`src/data/post_processing/summary_stats.R:247` and
`src/misc/config_parser.R:305`.

**Fix:** add `+ liab_add_med` to the `liab_pr_ee` formula.

---

### 5. Section 1250 / collectibles gain can be dropped from the tax base entirely — `tax.R:75-81`
```r
txbl_ord_inc      = pmax(0, txbl_inc - pref_inc),
txbl_adj_pref_inc = pmax(0, pmin(txbl_inc, adj_pref_inc)),
txbl_1250         = pmax(0, pmin(txbl_ord_inc - txbl_adj_pref_inc, pmin(kg_pref, kg_1250))),
```
The correct stacking order is ordinary → 1250 → collectibles → plain
preferred (top of the stack), so the "headroom" available for 1250/
collectibles should be `txbl_inc - txbl_adj_pref_inc - txbl_ord_inc` (i.e.
`pref_inc - adj_pref_inc`, which equals `kg_1250 + kg_collect` exactly when
nothing is floored). Instead the code computes `txbl_ord_inc -
txbl_adj_pref_inc` — a different quantity that goes deeply negative whenever
`adj_pref_inc` (plain LTCG/QDI) is larger than ordinary income, which is the
*normal* case for anyone whose portfolio income dominates their wage/business
income. Verified numerically: `txbl_inc=200,000`, ordinary income `=50,000`,
`kg_pref=150,000` with `kg_1250=20,000` of it → `txbl_1250` computes to `0`
instead of `20,000`; the four taxed buckets sum to `180,000` against a true
`txbl_inc` of `200,000` — $20,000 of real income never gets taxed at all (not
mistaxed — untaxed). The bug is silent whenever `kg_1250 = kg_collect = 0`
(the common case), which is why it hasn't surfaced before.

**Verified (second pass):** numeric example reproduced independently, and the
`pmin(liab_max, ...)` overall cap cannot catch it (it only guards against
*over*statement). Live in the data: in the pinned Tax-Data vintage
(`2026060918`, 2026 file), `kg_1250` is nonzero for ~0.6% of records and
`kg_collect` for ~0.2% — small but real population (depreciation-recapture
sellers with portfolio-dominated income), direction understates revenue.

**Fix:** compute the 1250/collectibles headroom as `txbl_inc -
txbl_adj_pref_inc - txbl_ord_inc` (equivalently `pref_inc - adj_pref_inc`,
floored at 0), not `txbl_ord_inc - txbl_adj_pref_inc`.

---

### 6. `magi_ss` omits tax-exempt interest, contradicting its own documented definition — `agi.R:130-138`, `ss.R:28`
`ss.R:28` documents `magi_ss` as "AGI less OASI benefits plus tax-exempt
interest." But `agi.R`'s `inc_ex_ss` (lines 92-106) never includes
`exempt_int`, and `magi_ss = inc_ex_ss - above_ded_ex_sl` (line 138) doesn't
either — `exempt_int` is a declared required variable (line 34) that's never
referenced anywhere else in the file. This is exactly the real-law provision
(IRC §86 provisional income adds back muni-bond interest even though it's
excluded from AGI): holders of tax-exempt interest have Social Security
benefit taxability systematically understated.

**Verified (second pass):** `exempt_int` is populated in the pinned Tax-Data
vintage (~1.7% of records nonzero), so this is live. Supporting evidence that
it's an oversight rather than a simplification: the code *does* correctly
implement the other §86(b)(2) subtlety — `magi_ss` uses `above_ded_ex_sl`,
i.e. it adds back the student-loan interest deduction as real law requires.

**Fix:** add `+ exempt_int` when constructing `magi_ss` specifically (not
`inc_ex_ss`/`gross_inc`/`agi`, which must keep excluding it).

---

### 7. ~~Senior deduction phases out over the wrong income range for two-senior households~~ — REFUTED: the code matches the statute. DO NOT APPLY THE ORIGINAL FIX — `below_ded.R:112-119`
```r
n_seniors  = as.integer(age1 >= 65) + as.integer(!is.na(age2) & (age2 >= 65)),
senior_ded = below.senior_ded_value * n_seniors,
po_rate    = n_seniors * below.senior_ded_po_rate,
senior_ded = pmax(0, senior_ded - pmax(0, agi - below.senior_ded_po_thresh) * po_rate)
```
**Original claim (wrong):** scaling both the deduction and the phaseout rate
by `n_seniors` makes a two-senior deduction phase out over the same income
range as a one-senior one, contrary to the codebase's flat-rate convention
(tips/OT/CTC).

**Why it's refuted:** OBBBA's senior deduction is $6,000 *per qualifying
individual*, and each individual's $6,000 is reduced by 6% of the same excess
MAGI. A two-senior MFJ couple's $12,000 therefore effectively phases out at
12%, fully gone at $250K ($150K + $6,000/0.06) — exactly what
`po_rate = n_seniors * 0.06` produces (baseline `below.yaml` confirms
`senior_ded_po_rate: 0.06`, `senior_ded_po_thresh` mapped ×2 for joint). The
universally published complete-phaseout points corroborate the per-individual
reading: $175K single / $250K joint regardless of whether one or both spouses
qualify. A flat 6% on $12,000 would instead imply $350K for two-senior
couples, which no published summary reports. The convention comparison to
tips/OT/CTC is beside the point — those provisions have flat per-return
phaseout rates in law; this one doesn't. The original fix would have
*introduced* a bug into a correct baseline-law implementation.

---

### 8. Dependent's standard deduction drops the age/blind bonus in the common low-earned-income case — `std_ded.R:57-66`
```r
std_ded     = std.value + bonus_value + std.bonus_other,        # nondependent, WITH bonus
dep_std_ded = pmax(std.dep_floor, ei + std.dep_earned_bonus),   # no bonus added
dep_std_ded = pmin(std_ded, dep_std_ded),                       # capped by bonus-inclusive value
std_ded     = if_else(!dep_status, std_ded, dep_std_ded)
```
The real IRS "Standard Deduction Worksheet for Dependents" order is: `min(base
deduction WITHOUT bonus, max(floor, earned income + bonus_addon))`, **then**
add the age/blind bonus on top. The code instead caps
`max(floor, ei+bonus_addon)` against the bonus-*inclusive* `std_ded` and never
adds the bonus separately. Whenever `max(floor, ei+bonus_addon) < std.value`
(true for any low-earning dependent — the typical case for an elderly or
blind claimed dependent), the `pmin()` just returns the floor/earned-income
figure unchanged and the age/blind bonus vanishes entirely. It's only
reflected correctly in the narrow band where earned income lands between
`std.value` and `std.value + bonus_value`.

**Verified (second pass):** confirmed against the Form 1040 worksheet order
(min against the base standard deduction *without* age/blind additions, then
add the per-box bonus on top). Note the same reassignment also swallows
`std.bonus_other` for dependents — relevant to bonus-standard-deduction
reforms.

**Fix:** cap against the bonus-*exclusive* base (`pmin(std.value, pmax(std.dep_floor,
ei + std.dep_earned_bonus))`), then add `bonus_value + std.bonus_other` after.

---

### 9. Three student-loan-interest tax-law parameters are silently dead — `agi.R:72-74`
`agi.sl_limit`, `agi.sl_po_thresh`, `agi.sl_po_range` are declared in
`req_vars` but never read in the function body — `sl_int_ded` (line 55) is a
raw pass-through input (`above_ded = above_ded_ex_sl + sl_int_ded`, line 147)
with no on-model cap or phaseout. Confirmed via repo-wide grep: unused outside
their own declaration. This isn't a live bug (the deduction still computes
*something*, just exogenously), but it's a **reform-authoring trap**: these
three parameters are fully wired up in `baseline/agi.yaml` (including a
correct MFS-disallowance mapper entry) as if overridable, but any reform YAML
that changes them is silently a no-op.

**Fix:** either restore the on-model phaseout calculation, or delete the three
dead `req_vars` entries and document that the deduction is exogenous.

---

## Needs a second look (lower confidence)

### 10. Childless EITC on a joint return may drop one spouse's earned income — `eitc.R:76-82`
```r
qual1 = (n_dep_eitc > 0) | (age1 >= eitc.min_age & age1 <= eitc.max_age)
qual2 = (n_dep_eitc > 0) | (age2 >= eitc.min_age & age2 <= eitc.max_age)   # (NA-safe)
ei = (ei1 * qual1) + if_else(filing_status == 2, ei2 * qual2, 0)
```
When there are qualifying children, `qual1`/`qual2` are both trivially true
(the `n_dep_eitc > 0` clause), so this only bites the childless credit. There,
if one spouse satisfies the age test and the other doesn't, `ei` includes only
the qualifying spouse's earnings. Real-law joint-return eligibility for the
childless credit is generally a binary gate (either spouse's age qualifies →
the couple is eligible), after which the credit is computed on the couple's
*combined* earned income — not just the qualifying spouse's. I can't rule out
this is a deliberate, long-standing modeling simplification given the data
available, so flagging for a second look rather than as confirmed. If
unintended: once eligibility is established, use `ei1 + ei2` for joint
returns rather than gating each spouse's earnings individually.

_Second pass: agree with both the substance and the hedged framing — real law
is an either-spouse age gate followed by combined earned income. Ask before
fixing; the per-record direction is ambiguous (partial `ei` can either
understate the phase-in or understate phaseout income)._

---

## Doc-only / robustness nits (no output impact)

- **`ed_cred.R:21-22`** — docstring has `ed_nonref`/`ed_ref` descriptions
  swapped ("value of refundable AOC" / "value of LLC and nonrefundable AOC");
  the code (line ~92) is correct. Risk: a future maintainer "fixes" the code
  to match the wrong comment. Fix: swap the two bullet descriptions.
- **`ctc.R`** — `ctc.po_rate_other` is read (line 151-153) but missing from
  `req_vars` (54-94), so `fill_missings = TRUE` testing callers get `NA`
  instead of `0` if it's ever omitted. Add it to `req_vars`. Also,
  `req_vars`' comments for `po_range1`/`po_range2` have their "value 1"/"value
  2" labels swapped relative to how they're actually paired at lines 149-150
  (comment-only).
- **`do_taxes.R:199-218`** — `simple_filer` ("non-itemizers whose income is
  derived solely from wages or OASDI") checks 17 income/deduction variables
  for zero but omits `other_inc`, even though `other_inc` is a real,
  separately-tracked income component (used two lines earlier in
  `expanded_inc`, line 193). As written, a filer with nonzero miscellaneous
  income (gambling, debt cancellation, etc., per Sch. 1) would still be
  counted as IRS-pre-fileable. Worth confirming whether this was deliberate;
  if not, add `other_inc == 0` to the criteria.

---

## Simplification opportunities

- **`agi_surtax.R:43-57`** — hand-rolled 3-bracket calculation is exactly what
  `integrate_rates_brackets()` (`utils.R:188`) already does; every sibling
  calculator (`tax.R`, `niit.R`, `estate.R`, `wealth.R`, `pr.R`) already uses
  the shared engine instead of unrolling bracket math by hand. _Second-pass
  caveat: not purely internal — the YAML schema uses scalar
  `surtax.threshold1`/`rate1` style rather than `rates[]`/`brackets[]` arrays,
  so the swap needs either a parameter rename (config compat) or an internal
  column mapping._
- **`ss.R:39-77`** — `calc_ss()` hand-rolls the same "(n+1)th Inf bracket /
  single-bracket rename / bracket loop+rowSums" skeleton that
  `integrate_schedule()` (`utils.R:105`) already extracts as the shared engine
  behind both `integrate_rates_brackets()` and
  `integrate_conditional_rates_brackets()` — likely predates or was missed by
  that refactor (`a142825e8`). Its only real deltas (`pmin(excess, gross_ss)`
  per-bracket clamp, final overall-rate cap) fit as a custom `bracket_fn`.
- **`amt.R:82-95`** — the itemized-deduction AMT addback is written so that
  `-ded` (which equals `-item_ded` for itemizers) algebraically cancels
  against a `+item_ded` buried inside the itemizing branch, leaving the
  effective formula as `agi - item_ded_ex_limits + salt_item_ded +
  misc_item_ded - ...`. That's very likely the *intended* result (AMT
  itemized addback should ignore the overall/Pease limitation, matching real
  Form 6251 mechanics) but reaching it via an unlabeled cancellation is a
  landmine for the next person who edits `ded` or the parenthetical without
  noticing the dependency. Not a bug; rewrite directly in terms of
  `item_ded_ex_limits - salt_item_ded - misc_item_ded` so the intent is visible.
- **`cdctc.R:103-115,132-144`** — the discretized phaseout rounding
  (`ceiling(excess/step)*step`) is hand-written 6 times (3 young + 3 old);
  `ctc.R:143-146` already does the equivalent with one `across(.cols =
  contains('excess'), ...)` call. Same rewrite would cut ~12 lines here with
  no behavior change.
- **`wage_subsidy.R:47-53`** (minor, low ROI) — primary/secondary phase-in/out
  logic is duplicated verbatim except for the `1`/`2` suffix; a shared
  `phase_in_out()` helper would remove the duplication, though each file's
  usage differs enough (different phase-out bases) that the payoff is modest.
- **Credit self-limiting `liab` formulas drift across files — CORRECTED on
  second pass: NOT output-neutral, and do not "align" them to `liab.R`'s
  list** — `caregiver_cred.R:79` omits `savers_nonref` and `ctc.R:184`'s
  `nonref` sum omits `res_energy_cred`. The original claim that this can't
  change output (because `calc_liab()` re-caps the *sum* of nonrefundable
  credits against `liab_bc`) is wrong: the re-cap protects total
  *nonrefundable* credits only. The refundable ACTC is allocated inside
  `ctc.R` via `remaining_ctc = f(ctc_nonref)`, so each credit's internal
  stacking assumption flows through to `ctc_ref` — real refund dollars — for
  low-liability families. `res_energy_cred` is nonzero for ~1.2% of records
  in the pinned vintage, so the choice is live. Moreover, the real Schedule
  8812 credit-limit worksheet does NOT subtract the residential clean energy
  credit before the CTC — so `ctc.R`'s current omission of `res_energy_cred`
  likely *matches the form*, and `liab.R`'s flat sum is not the right
  benchmark. If this is ever touched, align each credit's internal stack to
  the Form 8812 worksheet (and the analogous form logic for the others), and
  expect output diffs when doing so.

---

## Reviewed, nothing further worth flagging

`kg.R`, `pe_ded.R`, `qbi_ded.R`, `txbl_inc.R` (income/deductions);
`savers_cred.R`, `rebate.R`, `caregiver_cred.R` (credits — the latter despite
being disabled at its `do_taxes.R` call site);
`niit.R`, `estate.R`, `wealth.R`, `alt_max.R`, `liab.R` (tax/liability — `liab.R`
re-checked directly: nonrefundable-credit cap, refundable-credit allocation to
IIT vs. other taxes, and the NIIT/surtax carve-out from that allocation all
matched expectations); the Sec. 1A alt-max reallocation block in
`do_taxes.R:375-389` (checked for evaluation-order bugs given it mutates
`liab_ord`/`liab_pref`/etc. in the same `mutate()` it reads them from — confirmed
safe, all reads happen before any overwrite within the call). NIIT/Additional
Medicare Tax thresholds confirmed fixed-nominal (no `i_measure`), matching real
law. AMT preferred-rate carve-through (renaming `amt.rates/brackets` to
`ord.rates/brackets` before re-calling `calc_tax()`) confirmed correct.

---

## Suggested triage (added on second pass)

- **Fix now (real, live, cheap):** #4 (one-line), #6 (one-line), #3, #8, #5.
  All four of #3/#5/#6/#8 change baseline liability, so re-run the regression
  harness after.
- **Fix with narrower framing:** #2 — only affects `po_sequential = 1`
  scenarios (currently the `booker_repeal_*` tax-expenditure tests); re-run
  that suite after fixing.
- **Reframe, don't "fix":** #1 — document the MID balance limitation as
  off-model / dead lever (same treatment as #9) pending real Tax-Data
  mortgage imputations. Do not delete the `= Inf` lines.
- **Drop:** #7 — code matches the OBBBA statute; the proposed fix would
  introduce a bug.
- **Ask the author first:** #10 (possible deliberate simplification);
  `simple_filer`/`other_inc` nit (possibly deliberate).

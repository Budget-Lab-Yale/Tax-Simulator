# Maryland State Source Packet

State: `MD`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-09` (child and dependent care credit encoded; PolicyEngine unemployment gap P27; IRA distributions split out of the PolicyEngine pension input); previous: `2026-10-07`

> **Status note (as of 2026-07-24), kept from the packet's former Status line:**
> baseline encoded (state-level); record-level worksheet tests complete

Full research notes: [research/raw/md_research_core.md](research/raw/md_research_core.md)
(Resident Booklets 2017-2025 via the NBER mirror; TAXSIM facts probe-
verified; PolicyEngine facts read from the pinned venv tree).

## Scope

STATE tax only (Form 502 line 21 + state credits); the county piggyback
(2.25-3.3%) is deferred to the locality phase. MD is the flagship
piggyback state for that phase (rate matrix documented in the research
notes, incl. Anne Arundel/Frederick bracketed schedules).

## Structure encoded

FAGI start (rolling conformity, OBBBA business decoupling documented);
unindexed schedules I/II + TY2025 6.25/6.50% brackets (BRFA HB352) and
the 2% capital-gains surtax over $350k FAGI (new kg surtax params);
15%-of-AGI standard deduction with per-year min/max (new std_pct params;
flat from 2025); itemizing gated on the federal election with a best-of
choice (new item_fed_gate); MD itemized = federal components less income
taxes (SALT-capped, income-first assumption) + the 2025 7.5% phase-out
(existing protected-component machinery); $3,200 banded exemptions (OH
tier machinery) + unphased $1,000 aged/blind; full SS subtraction;
pension exclusion (per-year caps, 65+, IRA-ineligible via the new
incl_ira flag, cap less GROSS SS via the new cap_less_gross_ss flag);
$1,200 two-income subtraction (new twoearner_sub_max); RELIEF UI
subtraction 2020-21 with FAGI cliffs (new sub_ui_agi_limit); EITC 50%
nonref vs 27/28/45% refundable (VA dual-option machinery) + the childless
100% capped refundable override 2021+ (new eitc_childless params); CTC
under-6 from 2023 (cliff, then the 2025 phase-out); senior credit 2022+
with the one-65+ joint tier (new senior_credit_one65_amount); child and
dependent care credit (Form 502CR Part B, 2026-10-09: new cdctc_style 3).

## Worksheet tests: MD-1..MD-11

## Triage 2026-08-22 — exposure class landed; NOT closed

**Landed.** The DC/CA crosswalk-exposure class: Maryland builds its itemized
deduction from federal Schedule A less state income tax, and the crosswalk
hands TAXSIM as-reported `salt_inc_sales + salt_pers` inside `otheritem` where
nothing can identify them as SALT to strip. Federal itemizers match at 0.179
against 0.921 for non-itemizers, pooled 2017-2020. Cells 0.6246 / 0.7528 /
0.7387 / 0.7294 -> 0.9529 / 0.9274 / 0.9105 / 0.8962, matching the offline
estimate to four decimals.

**T18 was tested for Maryland and does NOT apply.** Maryland encodes the same
care deduction off the same IRC 21 base as Virginia and Idaho, and Maryland
records with care expenses do match far worse (0.447 against 0.740). It would
have been easy to carry the Virginia finding across. Probing it shows the
opposite: Maryland's care effect varies correctly with the spouse's earnings
(siitax 4,446.25 with both spouses earning, 4,607.25 with a non-earning spouse,
4,572.58 with a spouse earning $2,000), so TAXSIM applies the earned-income
limitation for Maryland. No care exclusion was added; **the Maryland care
residual is real but undiagnosed.**

**What is left.** After the exposure exclusion the residual is concentrated in
joint returns (2019: 0.883 against 0.972 for single). Two known leads, both
already annotated and neither sufficient on its own: the standard-deduction
minimum bug and the two-income-couple subtraction attribution. Note the
standard-deduction row is deliberately `annotate` -- a previous exclude on its
signature removed match@$100 passes and pushed the 2019 cell down to 0.488, and
that decision should not be re-litigated without new evidence. The one part of
it that does breach the $100 bar is head-of-household filers, where TAXSIM
applies the $1,550 single minimum against a correct $4,550, a $3,000 gap worth
$142.50 at 4.75%; excluding just that moves 2019 only 0.9291 -> 0.9336, because
545 of the 624 records it sweeps already matched.

The PolicyEngine window is also short (0.8702 / 0.9382 / 0.9208 / 0.9389) and
has not been triaged at all -- and it is now what binds Maryland, at 0.8702.

**Update, same day.** The federal-taxable-income correction to `fed_aligned`
(see the Idaho packet) lifted the Maryland TAXSIM cells to 0.9655 / 0.9542 /
0.9448 / 0.9255. 2017 and 2018 now clear; 2019 and 2020 do not, and neither
does any PolicyEngine cell. Maryland's care residual remains undiagnosed and
T18 remains refuted for Maryland. (Diagnosed 2026-10-09: the residual was our
missing child and dependent care credit; see below.)

## State EITC cross-check, 2026-10-07

Two State-EITC-Align flags (packets `MD-2020-01`, `MD-2018-02`), each verified here against the chapter
laws and booklets; State-EITC-Align was a check, not a source.

- **The 100% childless credit starts in TY2020, not TY2021.** Tax-General 10-704(b)(3)(ii)-(iv) as
  enacted by Laws of Maryland 2021 ch. 39 (SB 496, the RELIEF Act, approved February 15, 2021) reads "for a
  taxable year beginning after December 31, 2019, but before January 1, 2023 ... equal to 100% of the earned
  income credit allowable", capped at $530 and refundable. The amendment sits in Section 1 of the act, which
  has no separate applicability clause; the statute's own year range governs. (State-EITC-Align quoted Section
  14, which in the enrolled chapter applies to Section 2, the pass-through-entity provision. The conclusion is
  the same.) The 2020 booklet predates the act. `eitc_childless_match` and the $530 cap now start in 2020.
- **Childless filers under the federal minimum age, TY2018+.** 10-704(b)(3) (Laws 2018 ch. 612, HB 856,
  "applicable to all taxable years beginning after December 31, 2017"): the IRC 32 credit for an individual
  without a qualifying child "is calculated without regard to the minimum age requirement". 2018 and 2020
  booklets, Instruction 18: "calculate federal earned income credit disregarding the minimum age requirement".
  New generic `st_credits.eitc_childless_min_age_disregard`: for such filers every Maryland EITC computation
  (50% nonrefundable, refundable percentage, 100% childless credit) runs on the federal childless credit
  recomputed without the age test; the other federal tests stand (not a dependent, not separate, under 65,
  investment income within the federal limit). Tests MD-4b to MD-4e.
- The TY2023 $600 cap equals the federal childless maximum and never binds; left as encoded.

## Triage 2026-10-09: care credit encoded, three harness classes

**Our bug: the child and dependent care credit was missing.** Form 502CR Part B
(Tax-Gen 10-716) had never been encoded. It is the "undiagnosed" care residual
above, and the PolicyEngine 2021 care misses (records 345983, 157421, 273690,
where PolicyEngine pays $1,037-$1,342 and we paid nothing).

- *TY2019 onward* (Laws of Maryland 2019 ch. 432, SB 870, "applicable to all
  taxable years beginning after December 31, 2018"; current 10-716): 32% of
  the federal credit claimed on Form 2441. The share falls by 1% of itself
  (0.0032) for each $2,000 of federal AGI over $30,000, or each $3,000 over
  $50,000 for married couples filing jointly, "or fraction thereof". It is
  zero above an AGI cap and refundable below a lower limit (Worksheet 21B:
  the credit less Maryland tax). Head-of-household and separate filers use the
  individual thresholds; only joint filers use the joint ones. The thresholds
  are indexed from TY2020 (10-716(f)).
- *TY2017-2018*: the pre-2019 text, read from the struck language in the SB 870
  first-reader and the DLS fiscal note on SB 422 of 2018 (which died). 32.5% of
  the federal credit, reduced 10% of itself per $1,000 over $41,000, zero
  above $50,000, nonrefundable, with no separate joint schedule.
- Thresholds verified against the official 502CR instructions for 2020 (caps
  only) and 2023-2025. The 2021-2022 values come from PolicyEngine's parameter
  file, which cites the 2021-22 502CR instructions; those PDFs could not be
  fetched on 2026-10-09 and are marked UNVERIFIED in the YAML.
- The 2024 individual chart departs from the statute above $103,000: it steps
  every $1,000 instead of every $2,000 (0.1984 down to 0.1792 at the cap). The
  statute is encoded. The gap is at most about $10 on that band.
- Encoded as generic `cdctc_style` 3: the stepped own-rate slide
  (`cdctc_rate_*`) times the federal credit. The cap reuses the MN-style income
  cap with a zero cap amount. Tests MD-11a to MD-11e; MD-11a reproduces
  PolicyEngine record 157421 ($1,036.80 against its $1,037).

**Our bug, found on the first rerun: refundability of the two credits.** The
first rerun (md_run_1009) moved the PolicyEngine cells only partly. The
remaining care misses were ours. Record 12337 (2023 single, Maryland tax
$471.52, federal EIC $3,769, Maryland care credit $292): we owed -$1,516,
PolicyEngine -$1,224.

- Worksheet 21B refunds only the care credit's excess over the Maryland tax
  before credits. The credit itself is a nonrefundable Part AA credit. We had
  refunded all of it. New generic flag `cdctc_ref_excess_only`.
- The earned income credit is not an either/or choice in Maryland, unlike
  Virginia and Delaware. Form 502 line 22 (50% of the federal EIC) stacks with
  the line 23-24 nonrefundable credits against the tax. The refundable credit
  (line 42, Worksheet 18A) is the refundable percentage less the Maryland tax
  before credits, paid on top. The choice representation gave the same EIC
  benefit but left the tax free for the care and senior credits. New generic
  flag `eitc_alt_excess`. Tests MD-4c, MD-5 and MD-6 now report `st_eitc` as
  line 22 plus the refundable excess; their liabilities are unchanged. MD-11f
  reproduces record 12337.

**PolicyEngine bug P27: no 2021 unemployment subtraction.** policyengine-us
1.775.7 has no RELIEF Act subtraction (Laws of Maryland 2021 ch. 39; 502SU code
yy). Record 75235: `md_agi` equals federal AGI with $43,633 of unemployment
compensation in it. This was 17 of the 49 clean 2021 PolicyEngine misses.
Excluded for 2021.

**Harness fix: IRA distributions are no longer handed over as pensions.** The
PolicyEngine driver received taxable pensions plus IRA distributions as
pension income. Maryland's exclusion does not cover IRAs (Worksheet 13A; 2021
booklet Instruction 13 line 10a), and neither does PolicyEngine's Maryland
pension source list. PolicyEngine was therefore excluding IRA money that we
correctly tax. Record 331632 (2024): $72,057 of IRA distributions, PolicyEngine
$0 tax against our $1,557. With IRA distributions passed as
`taxable_ira_distributions` (split evenly on joint returns, like pensions),
federal AGI is unchanged at $127,060 and PolicyEngine's Maryland tax becomes
$1,618 against our $1,557. This changes the PolicyEngine input for every state.

**TAXSIM input coverage: one retirement input.** TAXSIM-35 has only
`pensions`, which the harness fills with pensions plus IRA distributions, so
TAXSIM grants the exclusion on IRA money. In the 2020 clean subset with a filer
65 or older, IRA-heavy records match 0.387 (joint) and 0.451 (single) against
0.923 and 0.996 for pension-heavy ones. Excluded where the pooled exclusion on
pensions plus IRAs beats ours by more than $100 of tax. Offline on the
2026-10-07 run: TAXSIM 2017-2020 cells 0.956 / 0.943 / 0.937 / 0.929 become
0.973 / 0.963 / 0.952 / 0.946.

**Data proxy: Social Security attribution in the pension offset.** Each
spouse's cap is reduced by that spouse's own benefits. We pool (the couple's
caps less total gross Social Security). The PolicyEngine driver gives all
benefits to the head and splits pensions evenly. Record 140113 (2024):
PolicyEngine subtracts $39,500, we subtract $17,248. Neither is form-true.
Excluded where the two differ by more than $100 of tax and Maryland tax
exceeds $100.

## Known differences

Military (code u) and public-safety (code v) retirement subtractions
omitted (source unobservable, one-sided); per-spouse SS attribution in
the pension-exclusion offset (unit-level gross SS used); poverty level
credit omitted (binds only when tax exceeds 50% of the federal EIC);
2021-22 disabled-child CTC omitted (unobservable, de-minimis at a $6k
cliff); 65+ dependent extra exemption omitted; SALT 17b income-first
allocation assumed; 502CG surtax exceptions unobserved; MD-source UI
requirement unobservable.

## Cross-model

TAXSIM (2017-20): siitax EXCLUDES county tax (probe-verified) - compare
directly. Pre-registered TAXSIM issues: TY2019 std-deduction maxima bug
(1,550/3,100 instead of 2,250/4,550), TY2020 stale values, and the
pension exclusion missing the SS offset (single-probe; corroborate).
PolicyEngine (2021-24): state_income_tax INCLUDES the county tax
(defaults to Allegany ~3% with no county input) - the harness now
requests the state-only md_income_tax (src/tests/state/cross_model/pe_state_tax.py
STATE_ONLY_LIAB_VARS). PE's md_montgomery_eitc is harmless under the
default county.

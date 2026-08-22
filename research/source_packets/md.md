# Maryland State Source Packet

State: `MD`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

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
with the one-65+ joint tier (new senior_credit_one65_amount).

## Worksheet tests: MD-1..MD-9

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
has not been triaged at all.

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

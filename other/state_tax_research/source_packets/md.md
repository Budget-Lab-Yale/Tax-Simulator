# Maryland State Source Packet

State: `MD`
Status: `baseline encoded (state-level); record-level worksheet tests complete`
Last updated: `2026-07-24`

Full research notes: [raw/md_research_core.md](../raw/md_research_core.md)
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
requests the state-only md_income_tax (pe_state_tax.py
STATE_ONLY_LIAB_VARS). PE's md_montgomery_eitc is harmless under the
default county.

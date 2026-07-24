# Wisconsin State Source Packet

State: `WI`
Status: `baseline encoded; record-level worksheet tests complete`
Last updated: `2026-07-24`

Full research notes: [raw/wi_research_core.md](../raw/wi_research_core.md)
(Form 1 booklets/Schedules 2017-2025, DOR rate pages, LFB Informational
Paper 2).

## Structure encoded

FAGI start (fixed-date conformity modeled rolling; targeted gaps
documented); four-bracket schedules with the verified rate history incl.
the 2019-only 3.86/5.04 Wayfair-funded rates and the TY2025 bracket-2
expansion (50,480/67,300/33,650); SLIDING standard deduction (new
std_po_* machinery; fixed statutory rates 12/19.778/22.515%, HoH floored
at the single schedule via the second sliding pair); $700/$250
exemptions; 30% LTCG exclusion (60% farm unobservable); full SS
subtraction; $5,000 retirement exclusion at 65+ under the 15k/30k FAGI
cliffs (new pension_excl_agi_limit); 5% itemized-deduction credit (new
item_credit machinery; medical floor difference documented); married
couple credit 3%/$480 (new twoearner credit); WI EITC 4/11/34/0% by
child count (new eitc_match_by_kids family); school property tax credit
12% of first $2,500 (rate-cap extension; renters unobserved); dependent
care subtraction 2017-21 then 50%/100% federal-credit match.

## Worksheet tests: WI-1..WI-7

## Known differences

Homestead credit not modeled (PE includes it: expected one-sided
low-income divergence); military/pre-1964 government pensions
(unobservable source); $500 capital-loss limit 2017-2022 (federal $3,000
embedded in PUF gains); UI partial exclusion; pre-ARPA federal EITC base
2021-22; WI 10% medical floor 2017/2019/2020 vs federal amounts; renters'
share of the school property tax credit; the 2025 Act 15 $24k/$48k
retirement election with credit forfeiture (deferred; documented);
WI-2441 10k/20k expense caps above the federal base 2024+.

## Cross-model

TAXSIM 2017-20: traps flagged = the 2019 one-time rates and the $500
loss limit; internals unverified. PolicyEngine 2021+: includes homestead
credit (one-sided), sliding SD, all four credits, LTCG exclusion.

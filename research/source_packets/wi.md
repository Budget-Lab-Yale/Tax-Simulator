# Wisconsin State Source Packet

State: `WI`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-08` (unemployment worksheet; PE window); previous: `2026-10-08` (child care credit base); previous: `2026-07-24`

> **Status note (as of 2026-07-24), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

Full research notes: [research/raw/wi_research_core.md](research/raw/wi_research_core.md)
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

## PolicyEngine window, 2026-10-08

The 2021 cell (0.907) was the Schedule SB unemployment compensation worksheet (Wis. Stat. 71.05(6)(b)8): Wisconsin taxes only the lesser of the benefits or half of federal AGI over a base ($18,000 joint, $12,000 single and head of household, $0 for a separate filer living with the spouse), less taxable Social Security and state refunds, and subtracts the rest. It had been documented as "small at PUF incomes"; the 2021 pandemic benefits made it the whole residual (29 clean records with benefits over $10,200 matched at 0.17; record 57026, $206,047 of benefits, subtraction $103,120 in PolicyEngine). Now encoded in the generic UI subtraction (`sub_ui_worksheet`, base filing-status mapped; test WI-10); it applies in every year, where it was negligible before 2020. A second row covers separate filers claiming the care credit (IRC 21(e)(2), the Minnesota class). Cells after: TAXSIM 0.983-0.990, PolicyEngine 0.967 / 0.941 / 0.947 / 0.949 (2025 0.932). Not closed: 2022-2024 sit 0.1-0.9pp under the bar, the misses high-income separate and single filers with +-$100-$1,900 gaps, unattributed.

## Child care credit base, 2026-10-08

Federal base sweep (does the state credit read the federal credit before or after the federal tax-liability limit, Form 2441 line 9/9c vs line 11?), each read from the state's forms and statute. TY2022-2023: Form 1 line 14 reads Form 2441 line 9c (tentative), now `cdctc_fed_base` = 1. TY2024+: Schedule WI-2441 computes 100% of a federal-formula credit on expenses up to $10,000 / $20,000 with no federal tax-liability limit (71.07(9g)), now `cdctc_fed_base` = 2 with those caps. Both had used the claimed federal credit. Test WI-9.

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

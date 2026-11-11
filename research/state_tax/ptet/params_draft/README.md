---
title: "PTET state law parameters: draft ptet.yaml per jurisdiction"
role: notes
workstream: state_tax
status: open
updated: 2026-11-10
sot: research/state_tax/ptet/ptet_proposal.md
supersedes: []
superseded_by: null
---

# PTET state law parameters: draft `ptet.yaml` per jurisdiction

A pass-through entity tax (PTET) is an elective state income tax paid by a partnership or S corporation on its owners' shares. The entity deducts it federally without limit, so the owners route state tax around the federal SALT (state and local tax) deduction cap. The owners then get a state credit or an income exclusion.

This directory holds a draft parameter file for each of the 42 jurisdictions in `research/state_tax/ptet/ptet_state_law.csv`. That is the 41 states with a broad income tax plus DC. The files implement the "PTET law parameters" row of the implementation table in `research/state_tax/ptet/ptet_proposal.md` section 6. **The model reads none of them.** They are not under `config/`, and `params_schema.yaml` is unchanged.

## Contents

- `{st}/ptet.yaml`: one file per jurisdiction, in the state-YAML style.
  - Year series begin at 2017, and every subparameter carries a `reference:`.
  - Unknown values are `NA`, with a comment saying what was searched.
  - Seven files have no PTET (VT, DE, PA, DC and ND, plus the pre-enactment years everywhere). They still record whether the state credits another state's PTET.
- `params_schema_ptet_snippet.yaml`: the proposed `st_ptet.*` addition to `config/scenarios/tax_law_state/params_schema.yaml`, not applied. It defines:
  - 16 scalars.
  - Numeric enumerations: `method` 0 none / 1 credit / 2 exclusion; `*_scope` 1 all-source / 2 state-source.
  - A `rates`/`brackets` vector family for graduated entity schedules (NY, NJ, OR, LA, MS).

## Encoding choices to review

- **`rate` is the top entity rate.** Graduated schedules (NY Article 24-A, NJ BAIT, OR PTE-E, LA, MS) also carry `rates`/`brackets`. Their brackets are on the *entity's* income, so a record-level calculator can only approximate them. The snippet recommends `rate` for the first implementation.
- **`rate_tracks_top: 1`** marks a statute that sets the PTET rate equal to the top individual rate. In those states `rate` equals our own `max(st_ord.rates)` for the year, except where our `ord.yaml` is stale (see the disagreements below).
  - CO and ID tie the PTET rate to the *corporate* rate. That rate equals the individual top rate in every year, but the flag is 0 with a comment.
- **Scalars cannot hold entity-type splits.** These are described in each file's header instead:
  - WI: S corporations pay 7.9% (`rate_scorp`).
  - MN: the all-source resident base applies to partnerships only; S-corp shareholders are taxed on state-source income only.
  - NJ: from 2022 the all-source base applies to partnerships only.
  - NY: a resident S corporation versus a standard one.
  - MT: the all-source base requires a separate resident election.
  - IN and KS: the entity chooses the resident base, so it is `NA`.
- **MD's rate includes the 2.25% lowest county rate** (8.0% for 2020–24, 8.75% from 2025, for individual members). Our MD calculator models only the state tax line, so the credit has to be split between state and county, or state tax will be over-credited.
- **NYC** is carried as `local_rate` 0.03876 and `local_credit_share` 1.0 from 2022. These are documentation only, because the state calculator models no local income tax.
- **MA Chapter 63E** is carried as `surtax_rate` 0.04 and `surtax_credit_share` 0.9 from 2026. This is the extra 4% entity tax on income above the millionaire-surtax threshold. Its threshold is tested on each member's share *from that entity*, not on the member's total income.
- Some files leave `method`, `rate` or `credit_share` at their post-enactment values in years before enactment. Those values are inert because `available` is 0 in those years.

## Proposal section 7 item 5 and the section 3 changes, resolved

| Item | Finding | Source quality |
|---|---|---|
| NY and NYC credit cuts in 2026 | **Not enacted.** The FY2027 budget kept the state and NYC credits at 100%. | A secondary source only: a BDO post-enactment alert ("considered but not enacted"). The enacted revenue bill text was not read. |
| MS rate | **5% schedule:** 0% on the first $5k, 4% on the next $5k, 5% above $10k. It is *not* the individual rate (4.4% in 2025, 4.0% in 2026). The credit is 100%, and the excess is refundable or carried forward. | Primary: MS DOR 2025 Form 84-100 instructions. Only 2025 was read; 2022–24 are assumed the same. |
| NM credit or exclusion | **Exclusion for TY2022** (HB 102, 2022). **Refundable credit from TY2023** (HB 368, 2023), with the credit amount added back. Rate 5.9%. | Primary: session laws |
| The ~14 states with unconfirmed year-by-year rates | All are now encoded from statutes or forms. Corrections are below. | Mostly primary |
| CA SB 132, extension to 2030 | Confirmed: 9.3%, nonrefundable credit with a 5-year carryover. From 2026, a missed June 15 prepayment cuts the credit by 12.5% instead of voiding the election. | Primary: FTB |
| IL permanence (P.A. 104-0453) | Still secondary only. ilga.gov blocks automated access. IDOR Pub-129 (revised 12/25) shows no end date. | Secondary |
| VA permanence (HB 29, 2026) | Confirmed. Va. Code 58.1-390.3 A.2 has no end date; the history cites 2026 c. 7. | Primary |
| UT permanence (HB 77, 2026) | Confirmed. Section 18 of the enrolled bill strikes the 12/31/2025 end date. | Primary |
| MN re-enactment for 2026–27 | Confirmed as Laws 2026 c. 128 art. 2 s. 17, retroactive to 1/1/2026. The tax ends after TY2027 and runs continuously over TY2021–2027. | Primary |
| OR sunset 2027 | Confirmed: SB 1510 = Or Laws 2026 ch. 75. The 2023 extension was HB 2083 (Or Laws 2023 ch. 399). | Primary; the signing date is still unconfirmed |
| ME from 2026 at 90% | Confirmed: 7.15% (excluding the new 2% surcharge), 90% refundable credit, add-back, residents taxed on all-source income. | Primary: 36 M.R.S. |
| MA Chapter 63E from 2026 | Confirmed from St. 2026 c. 101 s. 21 and s. 45. | Primary |

## Corrections applied to `ptet_state_law.csv` (2026-10-28)

Each corrected row also has a note appended, tagged `2026-10-28 params_draft:`.

| State | Years | Column | Old → new | Source |
|---|---|---|---|---|
| AR | 2022 | ptet_rate | 5.9 → 4.9 | AR1100PET 2024 instructions (the rate is the top individual rate: 4.9 / 4.7 / 3.9) |
| AR | 2024 | ptet_rate | 4.4 → 3.9 | same |
| AR | 2026–30 | ptet_rate | 3.9 → 3.7 | Act 1 of the 2026 special session. **EY alert only; the enrolled act was not read** |
| AZ | 2022 | ptet_rate | 4.5 → 2.98 | A.R.S. 43-1014(A); ADOR Publication 713 |
| CO | 2019 | ptet_rate | 4.63 → 4.5 | C.R.S. 39-22-344(1); TABOR temporary cut |
| ID | 2022 | ptet_rate | 5.8 → 6.0 | Idaho Code 63-3025(1)(e) |
| IN | 2027–30 | ptet_rate | 2.95 → 2.9 | IC 6-3-2-1(b); DOR notices |
| NE | 2022 / 2023 / 2024 | ptet_rate | 6.64 / 5.84 / 5.2 → 6.84 / 6.64 / 5.84 (the table was one year early) | Neb. Rev. Stat. 77-2715.03(2)(c) |
| NJ | 2020–21 | ptet_rate | 10.75 → 10.9 (the original schedule's top bracket, over $5M) | Secondary (BDO/EY/CBIZ); the original P.L. 2019 c. 320 was not read |
| OH | 2022 | ptet_rate | 3.0 → 5.0 | ORC 5747.38; 2022 IT 4738 instructions |
| WV | 2026–30 | ptet_rate | 4.82 → 4.58 | EPT-100 instructions; W. Va. Code 11-21-4j (SB 392, 2026) |
| IA | 2022 / 2023 / 2024 | credit_share | blank → 0.9147 / 0.94 / 0.943 | Pre-2025 Iowa Code 422.16C(5)(a)(2): 100% minus the top individual rate |
| RI | 2025–30 | credit_share | 1.0 → 0.9 | R.I. Gen. Laws 44-11-2.3, as amended by P.L. 2024 ch. 117 art. 6 s. 11 |
| NM | 2022 | method / credit_share | credit / 1.0 → exclusion / blank | HB 102 (2022); HB 368 (2023) |
| SC | all | resident_credit_other_state_ptet | unknown → no | SC Revenue Ruling 21-15, Q26 |
| PA | all | resident_credit_other_state_ptet | yes → **partial** (a new value): yes for S-corp shareholders, no for partners | Secondary (PA Chamber memo, PICPA). No DOR bulletin was found |
| NJ | 2020–21 | resident_credit_other_state_ptet | yes → unknown (confirmed only for 2022+) | NJ BAIT FAQ |
| ME | 2018–25 | resident_credit_other_state_ptet | yes → unknown (the explicit credit is 36 M.R.S. 5217-A(2), TY2026+) | statute |
| NE, IN, KS, ID, OR, UT, ND | all | resident_credit_other_state_ptet | unknown → yes | 77-2730; IC 6-3-3-3(d); K.S.A. 79-32,111(c); 63-3026B(7)(b); OAR 150-316-0082; USTC; Schedule ND-1CR |
| MS, NC, NE, WI, WV, ID, NM, MT | various | confirmed | unconfirmed → yes | see the notes on each row |

Facts corrected in notes only:
- KS's enacting act is HB 2239 (2022), not SB 47. SB 410 (2024) set the rate to the top individual rate, retroactive to 2022.
- WI partnerships pay 7.9%, not the individual graduated schedule. They could elect only from TY2019.
- MD's all-source resident base is postponed to TY2027.
- NC taxes resident partners' full shares in TY2022 only.
- IA owners add back the PTET in TY2022 only.
- CT's credit is confirmed from statute history, and its 87.5% share was unchanged by the 2024 switch to an elective tax.
- GA's 4.99% for 2026 is confirmed: HB 463 was signed 5/11/2026.

`research/state_tax/ptet/ptet_state_law.md` was not rewritten. Its "Open and unconfirmed items" section is now partly superseded by this README.

## Disagreements with our own state parameters

Each of these is a gap in `config/scenarios/tax_law_state/baseline/`, not in the PTET drafts, and needs a separate fix. Do not edit those files while the all-state jobs run.

| File | Ours | Enacted law | Source |
|---|---|---|---|
| `baseline/ne/ord.yaml` | top rate held at 4.55% from 2026 | **3.99% from TY2027** | Neb. Rev. Stat. 77-2715.03(2)(c)(vi) (LB 754, 2023) |
| `baseline/ga/ord.yaml` | 5.19% from 2025 | **4.99% for TY2026** (further cuts reported) | HB 463, signed 5/11/2026 |
| `baseline/ar/ord.yaml` | 3.9% carried into 2026 | **3.7% from TY2026** | Act 1, 2026 special session (EY only) |
| `baseline/mt/ord.yaml` | 5.9% from 2024; HB 337 deliberately left out | **5.65% in 2026, 5.4% from 2027** | HB 337 (2025) |
| `baseline/me/ord.yaml` (and no `surtax.yaml`) | no surcharge | **2% surcharge from TY2026** on Maine taxable income above $1M (single) or $1.5M (joint / head of household) | 36 M.R.S. 5111(7) |

None of the source packets (`research/source_packets/{st}.md`) contradicts these findings. Most either list the PTET as unmodeled (CT, ID, KY, MI, MT, VA, WV) or say nothing about it.

## Still unresolved

**The law itself**
- **NY and NYC 2026:** the conclusion rests on one secondary source. Reading the enacted FY2027 revenue bill (the Part amending Tax Law 606(kk) and Article 30) would settle it.
- **IL P.A. 104-0453:** read only through secondary sources.
- **AR 3.7% for 2026:** read only through EY.
- **NJ 2020–21 schedule:** read only through secondary sources.
- **OK exclusion:** the cite, 68 O.S. 2358(A)(11), came from a Deloitte alert; the subsection was not read.
- **WV:** nonrefundability and carryforward come from secondary sources.
- **OR SB 1510:** the signing date is unconfirmed.
- **MS 2022–24:** assumed to follow the 2025 schedule.
- **CO 2026:** a temporary TABOR rate cut is still possible once the fall-2026 surplus is certified.
- **Rate paths not encoded:** GA's reported 0.125-point annual cuts to 3.99% (press reports only), and the revenue-trigger cuts in NC and OK.

**`addback` (whether owners add back the entity's deduction)**
- `NA` for AL, KY, MS and WV.
- Inferred from statute rather than forms for IL, IN, KS, MN and ID.
- MI rests on a search excerpt.
- MA rests on secondary sources: mass.gov returned 403 for TIR 22-6 and the Form 63D-ELT instructions.
- CT and MO are 0, inferred from the absence of an add-back line.

**`resident_credit_for_other_state_ptet` (whether the credit for taxes paid to other states covers another state's PTET)**
- `NA` for AL, GA, KY, LA, MS, NC, OK, WV, IA, MI, OH, NM, MT, VT and DE.
- `NA` in early years for NJ (before 2022), ME (before 2026) and MD (before 2020).
- DC rests on the title and summary of OTR Notice 2022-03 only.

**`resident_scope` (all-source versus state-source base for resident owners)**
- `NA` for HI: Tax Information Release 2024-01 is silent, and the statute site was blocked.
- `NA` for IN and KS, where the entity elects the base.
- MN and MT are coded 1 with caveats.

**`credit_refundable`**
- Inferred from where the credit sits on the form for RI and MD.
- Secondary sources only for KS, MI and IL.

**Citations**
- NY's subsection letters (Tax Law 860, 862, 867 and the NYC Article 30 credit) are marked "not verified".
- CA is cited at section level only.

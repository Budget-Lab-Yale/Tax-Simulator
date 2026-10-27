# State pass-through entity tax (PTET) law, tax years 2018–2030

Companion to `ptet_state_law.csv` (one row per state × tax year, 41 broad-income-tax states + DC = 42 jurisdictions, 2018–2030). Compiled 2026-10-26 by web research. The starting point was Comey, Gallagher & Gorman (JCT, Oct 2025), Tables 1 and B.1, which draw on Bloomberg Tax.

## Methods and column conventions

- **Current law only.** `ptet_available = 1` for every year from the first effective year through the last year the statute allows. A PTET with no end date, or one tied to the federal SALT cap, runs through 2030. OBBBA (P.L. 119-21) keeps the IRC 164(b)(6) cap in place permanently ($40,000 for 2025–2029, then $10,000 from 2030), so a state election that lasts "while the federal limit applies" stays open.
- **`sunset_basis`:**
  - `fixed_year`: a statutory end year (CA 2030, MN 2027, OR 2027).
  - `federal_cap`: the election lasts only while 164(b)(6) applies (CO, IA, MA, MI).
  - `permanent`: no end date was found.
  - `none`: no PTET.
- **`credit_share`:** the owner credit as a share of the owner's portion of entity tax. It is blank for exclusion states and where the share could not be confirmed (Iowa 2022–2024).
- **`ptet_rate`:** the top rate. Graduated schedules (NY, NJ, OR, LA, WI partnerships) and member-type differences (MD, OK, WI) are described in `notes`.
- **`confirmed`:** refers to whether the PTET exists, its effective years and sunset, its method and its credit share.
  - `yes` means one of two things. Either a primary source was read (statute, session law, or a state revenue page or form), or two or more reputable secondary sources agreed (EY, Bloomberg Tax, Big-4 or law-firm alerts).
  - **Year-by-year rates are often from my own knowledge of each state's top-rate schedule and are not individually verified.** The notes flag these as "unconfirmed".
  - `unconfirmed` means only one weak secondary source was found, or none.
- **Cross-check for 2026.** The Arkansas DFA "List of States With and Without Similar Taxes" for tax years beginning in 2026 (dated 6/3/2026) is a primary source. It lists exactly 37 PTET states: the 36 in the JCT table plus Maine ("New 2026"). It lists DE, ND, PA, VT and DC as having no PTET: https://www.dfa.arkansas.gov/wp-content/uploads/States_with_Similar_Tax_PET_2026.pdf. The CSV has 37 states available in 2026 and 35 in 2030, because MN and OR lapse after 2027.

## What changed in 2025–2026

| State | Change | Source |
|---|---|---|
| CA | **SB 132** (signed 6/27/2025) extends the elective tax to taxable years beginning before **1/1/2031** (2026–2030). From 2026, missing the June 15 prepayment no longer voids the election. Instead, each owner's credit is cut by 12.5% of their share of the unpaid amount. | FTB Tax News Aug 2025, https://www.ftb.ca.gov/about-ftb/newsroom/tax-news/2025/08.html ; FTB FAQ https://www.ftb.ca.gov/file/business/credits/pass-through-entity-elective-tax/help.html |
| IL | **SB 1911 / P.A. 104-0453** (signed 12/12/2025) removed the sunset (the original law covered years beginning before 1/1/2026). The PTET is now permanent. | EisnerAmper https://www.eisneramper.com/insights/tax/state-ptet-updates-0226/ ; Wipfli; Forvis Mazars https://www.forvismazars.us/forsights/2026/06/a-refresh-and-an-update-on-the-pass-through-entity-tax |
| VA | 2025 legislation first moved the sunset to 1/1/2027. **HB 29** (2026 budget, signed 2/20/2026) then made the PTET permanent. The deemed-paid credit for other-state PTE taxes is permanent too. | EY https://taxnews.ey.com/news/2026-0614-virginia-changes-irc-conformity-decouples-from-certain-obbba-provisions-makes-elective-pass-through-entity-tax-permanent |
| UT | **HB 77** (2026; effective 5/6/2026, applies from 1/1/2026) removed the sunset of 12/31/2025. The PTET is now permanent. | Bloomberg Tax https://news.bloombergtax.com/daily-tax-report/utah-makes-pass-through-entity-income-tax-permanent-1 ; AV text https://le.utah.gov/Session/2026/bills/amended/AV_HB0077_2026-01-27_11-54-43.pdf |
| MN | The PTET expired after TY2025. **HF 2438** (signed 5/27/2026) re-enacted it retroactively for **TY2026–2027** (years beginning before 1/1/2028). It also extends the resident credit for PTE taxes paid to other states. | EY https://taxnews.ey.com/news/2026-1193-minnesota-updates-irc-conformity-addresses-certain-obbba-provisions-and-extends-elective-pass-through-entity-tax ; Stinson |
| OR | **SB 1510** (2026 session; passed March 2026) extends PTE-E to tax years beginning before **1/1/2028**. The law previously ran through TY2025. | Oregon DOR https://www.oregon.gov/dor/programs/businesses/Pages/Pass-Through-Entity-Elective-Tax.aspx |
| MA | Ch. 63D (5% rate, 90% refundable credit, tied to the federal cap) continues. **NEW Ch. 63E** (St. 2026 c. 101 §21, H 5470, signed 6/12/2026) creates an elective **additional 4%** entity tax for TY2026+ on member income above the 4% surtax threshold, with a 90% refundable credit. The combined top entity rate on surtax income is 9%. | mass.gov c.63E §2 https://www.mass.gov/info-details/mass-general-laws-c63e-ss-2 ; EY https://taxnews.ey.com/news/2026-1331-massachusetts-delays-conformity-to-certain-federal-tax-changes-made-by-the-obbba-creates-an-elective-ptet-for-surtax-on-high-income-earners |
| ME | **NEW PTET**: P.L. 2025 c. 650 Pt. N (enacted 4/10/2026); 36 M.R.S. §§5199-A–I and 5219-CCC. It applies to TY2026+. The rate is the top individual rate excluding the 2% surcharge (7.15%), and owners get a **refundable 90% credit**. Qualified members are individuals, trusts and estates. Residents are taxed on all-source income and nonresidents on Maine-source income. No sunset was found. | https://legislature.maine.gov/statutes/36/title36sec5199-C.html ; https://legislature.maine.gov/statutes/36/title36sec5199-A.html ; https://legislature.maine.gov/statutes/36/title36sec5219-CCC-1.html |
| MD | The 2025 Budget Reconciliation and Financing Act (BRFA, HB 352) raised the top rate to 6.5%. The PTET rate for individual members is therefore 8.75% from 2025 (top state rate plus the 2.25% lowest county rate). The act also expanded the resident base to all-source income. The BRFA of 2026 suspended that base change until 2027, so TY2026 stays on Maryland-source income only. | Comptroller alert https://www.marylandcomptroller.gov/legal-library/ta-13-apr2026pte.html ; Grant Thornton |
| HI | Act 58 SLH 2025 requires owners to **add back** their share of PTE tax from TY2025. | https://tax.hawaii.gov/pte/ |
| OH | HB 96 (2025) leaves the 3% PTE rate unchanged. From TY2025 the electing entity can claim a refundable credit for taxes paid to other states. | EY https://taxnews.ey.com/news/2025-1441 |
| AL, MI, OK | Procedural changes to election timing only. AL now makes or revokes the election on the return (2025+). MI's deadline moved to the 9th month after year-end (2024+). OK's deadline is now the return due date (2024+). | EisnerAmper (above) |
| NY / NYC | The 2026 one-house budget bills (S 9009 / A 10009) proposed cutting the state credit to 90% and the NYC credit to 75%. **Enactment is not confirmed.** The CSV keeps 100%. | ICSC summary https://www.icsc.com/news-and-views/icsc-exchange/new-york-legislature-proposes-ptet-reduction-increase-to-corporate-income-tax |

**No new PTET in DE, ND, PA, VT or DC.**
- DC: B26-0324, introduced 7/2025, had a hearing on 3/19/2026 and has not been enacted.
- PA: a PTET was dropped from SB 659 in 2024.
- DE: HB 489 died in 2022.
- VT: S.45 passed only the Senate, in 2023.

**Implication for 2030.** Once the federal cap reverts to $10,000, 35 states still offer a PTET under current law. MN and OR lapse after 2027 unless they are extended again. CA runs through 2030.

## Disagreements with or additions to the JCT table (Comey et al. 2025, Tables 1 / B.1)

1. **CT credit share.** It was 93.01% in TY2018 only and has been **87.5% since TY2019** (2019 legislation, HB 7424). It was not 93.01% throughout. The PTET was mandatory 2018–2023 and elective from 2024. Source: DRS FY2022-23 Annual Report.
2. **IA credit share is below 100%.** Under Iowa Code 422.16C(5), the credit equals the member's share of tax times (100% minus the individual rate), which is **96.2%** at the 3.8% rate. The section also applies only while 164(b)(6) applies.
3. **MO 2025 rate.** The statute sets the rate at the highest individual rate (4.7% in 2025). JCT's 4.80 looks like the 2024 value.
4. **AZ top individual rate.** JCT lists 4.50 for 2025, but Arizona has had a flat 2.5% rate since 2023. The PTET rate of 2.5% is correct.
5. **HI rate.** JCT's 9.0% is right for 2024+, but TY2023 was 11%. From 2024, qualified members are restricted to individuals, trusts and estates.
6. **MA.** JCT's 5% is incomplete from 2026: Ch. 63E adds an elective 4% on surtax income.
7. **MD.** The 8.75% rate includes the 2.25% county component, so it is not out of line with the 6.5% state rate. Entity members pay 8.25%.
8. **MS rate.** JCT shows 5.00% PTET against a 4.40% top individual rate. I could not confirm which rate applies to electing entities. This item is unconfirmed.
9. **LA.** Electing entities are taxed at individual-type rates per LDR regulation: graduated 1.85/3.5/4.25% for 2022–24, after a 2019–21 schedule (2/4/6%) that is only recalled from memory, not checked against a source. JCT's 3% for 2025 matches the 2025 individual flat rate but was not confirmed against LDR guidance.
10. **Sunset treatment.** JCT reports no sunsets. CA, MN, OR and VA had fixed end dates, and CO, IA, MA and MI are tied to the federal cap. See the table above for current status.

## Published PTET totals (dollars)

| State | Measure | Period | Amount | Source |
|---|---|---|---|---|
| CT | PE tax collections | FY2020-21 | $1,553.8M | DRS FY2022-23 Annual Report, https://portal.ct.gov/-/media/drs/research/annualreport/drs-fy23-annual-report.pdf |
| CT | PE tax collections | FY2021-22 | $2,308.2M | same |
| CT | PE tax collections | FY2022-23 | $2,049.1M | same |
| CT | PE tax collections | FY2023-24 | $1,964.8M | DRS FY2024-25 Annual Report, https://portal.ct.gov/-/media/drs/research/annualreport/drs-fy25-annual-report.pdf |
| CT | PE tax collections | FY2024-25 | $2,130.7M | same |
| NY | PTET collections | SFY2021-22 | $16.4B | OSC 2022 Financial Condition Report, https://osc.ny.gov/reports/finance/2022-fcr/taxes |
| NY | PTET collections | SFY2022-23 | $14.9B | OSC 2023 FCR, https://osc.ny.gov/reports/finance/2023-fcr/taxes |
| NY | PIT credits claimed | TY2021 | ~$11.5B | OSC 2023 FCR (same) |
| NYC | City PTET PIT credits | TY2022 / TY2023 | $1.6B / $1.7B | NYC Comptroller, https://comptroller.nyc.gov/reports/the-nyc-personal-income-tax-before-and-after-the-pandemic/ (from search excerpt; not re-read) |
| CA | PIT revenue loss from PTE credit usage (DOF estimate) | FY2021-22 / 22-23 / 23-24 | $19.0B / $18.0B / $18.7B | DOF 2023-24 May Revision, Revenue Estimates, https://ebudget.ca.gov/2023-24/pdf/Revised/BudgetSummary/RevenueEstimates.pdf (PTE payments ≈9.5% of GF revenue over 2021-22 to 2023-24) |
| MA | One-time net revenue gain from the PTE excise | FY2022 | ~$1.4B | MA DOR FY2022 revenue release, https://www.mass.gov/news/fiscal-year-2022-revenue-collections-total-41105-billion |
| MA | Net PTE revenue impact (forecast) | FY2023 / FY2024 | −$13M / +$102M | Commissioner Snyder FY2024 consensus revenue testimony, https://www.mass.gov/doc/commissioner-snyders-testimony-fy2024-consensus-revenue-estimate-hearing/download |
| NJ | PT-BAIT collections, year-to-date | FY2021 through Mar 2021 | $1.410B | NJ Treasury release (via search excerpt) https://www.nj.gov/njbonds/treasury/news/2021/01202021.shtml |
| NJ | PT-BAIT collections, year-to-date | FY2026 through Jan 2026 | $2.600B | NJ Treasury monthly release (via news excerpt) |

All of these are fiscal-year cash figures unless noted. They mix the PTET payment side with the PIT credit side, so they are not a tax-year liability series. No full-year figures were found for NJ, IL, MI, VA, OR, UT or MT.

## Open and unconfirmed items

- **Rates by year** (CSV notes say "unconfirmed"): AR 2023–24, CO (TABOR temporary cuts), GA 2026 (4.99% via HB 463), IA 2022–24, ID, KY, MS (all years), MT 2023, NE, NC, OK 2026, UT 2022–24, WV, NJ 2020–21 top bracket. In most of these states the statute ties the PTET rate to the top individual or corporate rate, so the rate should follow Tax-Simulator's own state rate parameters.
- **Sunset status from secondary sources only:** ID, MS, MT, NC, NE, NJ, NM, NY, OK, RI, SC, WV, WI were coded `permanent` because no end date was found. The statute text was not read for these states. The 2026 Arkansas DFA list confirms they are all live for 2026.
- **IL:** the P.A. 104-0453 statute text could not be read (TLS error on ilga.gov). Several secondary sources agree that the sunset was removed.
- **UT HB 77:** only the amended-version text and a Bloomberg summary were seen, not the enrolled bill.
- **OR:** the 2023 bill that first extended the PTET through TY2025 was not identified, and SB 1510's signing date was not confirmed (sources say March 31, 2026).
- **IA credit share for 2022–2024:** the pre-2024-amendment formula was not read, so those cells are blank.
- **NM:** sources disagree on whether owners get a credit (JCT) or an exemption.
- **NY / NYC:** I could not confirm whether the 2026 enacted budget cut the state credit to 90% or the NYC credit to 75%. This is high priority because the NYC credit affects the largest PTET base after CA.
- **Resident credit for other-state PTET:** confirmed yes for AZ, AR, CO, HI, IL, MA, ME, MN, MO, VA, CA, NY, NJ, CT, PA and DC. The CA, NY, NJ and CT cases rest on general knowledge plus secondary sources, and DC rests on the title of OTR Notice 2022-03. Wisconsin is yes, except where the entity also elected in WI. The other states are `unknown`.
- **Eligible owners and add-back:** these were captured in the notes only where found: CO (199A and state-tax add-back), HI (from 2025), UT (add-back), CA (individuals, trusts and estates only), HI 2024+ and ME (individuals, trusts and estates), and NC (partnerships with qualifying owners only). They were not collected systematically.
- **ND, DE, VT:** there was no evidence of any 2025–26 bill. A negative search result is weak evidence.

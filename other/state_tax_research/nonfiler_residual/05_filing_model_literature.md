# Filing-Model Literature — Are Cilke (1998) and Pub 5785 Still Current?

**Date:** 2026-08-18
**Status:** Research pass B of the design memo §8, executed. This is the deliverable
that gates transcribing `resources/cilke_coefs.csv`.
**Companions:** `../nonfiler_residual_design.md` §3.2/§8 (the models this feeds),
`04_findings.md` (Stage D diagnostics), `resources/filing_model_refs.bib`.

---

## 1. Bottom line

**Cilke (1998) should be replaced, not re-calibrated.** There is a direct successor
the design memo does not cite: **Mok (2017), CBO Working Paper 2017-06, Table 14** —
fourteen group-specific probits of the filing decision, coefficients and standard
errors printed, estimated on the **2007 CPS ASEC linked (via PIK) to the IRS
Individual Master File for TY2006**. Same research design as Cilke's WP-78 (survey
linked to administrative filing truth), same broad covariate set — Mok's own text
says her regressors are "similar to the set of covariates used by Cilke" — but 16
years newer, on a larger linkage, and with per-cell filing rates published as
ready-made calibration targets. Transcribing 1990-vintage slopes when 2006-vintage
slopes exist in the same functional form is not defensible. **The paper was obtained
and checked line by line on 2026-08-18 — the recommendation survives verification
(§7), with one subsidiary claim corrected and two caveats added: Panel E's reversed
column order, and the fact that Mok's CPS frame excludes the institutionalized and
military-barracks populations that our PUF universe includes.**

**Publication 5785 should be retained, but it is no longer the newest official
source and was never the richest.** No successor edition exists: the TY2017–2022
figures in the tax-gap series are *projections* built with the older Administrative
Data Method and carry no counts, and the IRS's fall-2025 estimate release is
officially delayed. Three documents should be read alongside it — Hertz et al.
(2021/2022) for the TY2010 predecessor under eight method variants, Treasury OTA's
January 2025 special study for the newest counts (TY2022), and OTA Technical Paper
12 for Treasury's current construction recipe.

**The finding that should reframe the design.** Treasury, the IRS and JCT have all
*abandoned* the survey-probit approach rather than update it, and said so in print.
Hertz et al. state the IRS "abandoned the Census Method because even with the income
imputations, the income reported on the CPS-ASEC by nonfilers still fell short of the
income reported to the IRS by third parties" — and quantify it: against a 50.7M
administrative target, the reweighted ASEC reaches 42.0M, a ~17% undercount. Cilke
himself switched methods in his 2014 paper, which contains no probit at all.

This does not invalidate our approach — we do not have administrative microdata, so a
survey filing model is the only option open to us. But it means: (a) the model is a
second-best whose error direction is known and published; (b) the calibration targets
should be administrative counts, which is already the design; and (c) the memo should
say plainly that the institutions with better data stopped doing it this way.

---

## 2. Correcting a premise in our own brief

The research brief asked whether TCJA's larger standard deduction means "many fewer
people required to file." **For the main filing statuses this is wrong**, and the plan
should not lean on it. The §6012(a)(1) threshold is the standard deduction *plus*
exemptions pre-TCJA; zeroing exemptions offset most of the deduction increase:

| Status | TY2017 | TY2018 | Change |
|---|---|---|---|
| Single &lt;65 | $10,400 | $12,000 | +15.4% |
| MFJ both &lt;65 | $20,800 | $24,000 | +15.4% |
| HoH &lt;65 | $13,400 | $18,000 | +34.3% |
| **Single dependent (earned income)** | **$6,350** | **$12,000** | **+89%** |
| **Married filing separately** | **$4,050** | **$5** | **collapse** |

Returns filed rose over the period (150.3M in 2016 → 153.8M in 2018 → 164.4M in 2020).

**But two groups did move sharply, and both matter to us.** Dependents are Cilke's
*largest* group — 31.1% of his below-threshold population and 36.4% of his non-filers
— and their threshold nearly doubled. MFS inverted. Since dependent non-filers and MFS
are both currently out of scope for v1 (design memo §5.1), this is a reason to revisit
that scoping, not to ignore the TCJA question.

---

## 3. The three named papers

### 3.1 "**Attaching** the Left Tail" — note the title

Mortenson, Cilke, Udell & Zytnick (2009), *NTA Proceedings* 102, pp. 88–98. The title
begins **"Attaching"**, not "Attacking" — verified against the PDF.

No filing model and no coefficients; it is a tabulation from information returns.
What it gives us: TY2003 levels (294.3M resident population, 259.0M on returns,
**35.3M implied non-filers**, of whom 29.9M appear on ≥1 of 17 information-return
types) and receipt/income tables by income class and by age. Two facts worth carrying:
**40% of non-filers are 65+ vs 10% of filers**, and only 8.3M of 29.9M had any
withholding — those with it averaged $20,578 of taxable income against $3,911 for
those without.

### 3.2 "The Case of the Missing Strangers"

Cilke (2014), *NTA Proceedings* 107. **This is Cilke's own move away from WP-78's
method** — the word "probit" does not appear — which is the single most direct piece of
evidence that the 1998 approach was superseded by its author.

TY2010/2011 funnel from 41,756 sampled persons down to **~30.1M non-filers**, with
every exclusion step published (Table 1) and an age × gender breakdown (Table 3, ages
0–17 through 75+, total 30,158k, 49.7% male). Table 2 is the cleanest
information-return incidence table in the literature (see §5).

Directly relevant to a *voluntary*-filing model: **10.5 million filers had positive
wages below the single threshold and were not required to file**, with withholding at
5.6% of wages against 2.6% for comparable non-filers. That is the refund-claiming
mechanism, measured. Cilke also states the limitation that bites us: in administrative
data "marital status and parental status are largely non-existent."

### 3.3 "Imagine All the People" — Brady & Bass (2023)

SOI Joint Statistical Research Program working paper. No filing model — filing status
is observed rather than modeled — but the best-decomposed level estimate available, and
directly comparable to our residual anchors.

TY2016 (Table 4): total population **331.095M** = filers **204.492M** (62%) +
dependent non-filers **84.235M** (25%) + **non-dependent non-filers 42.368M** (13%).
Table 5 gives a Census-equivalent estimate of 326.508M, **101.1% of the Census
estimate**. Non-dependent non-filer share by single year of age runs ~5% to age 16,
**11% at 24, 16% at 60, 30% at 80** — an independent read on the age shape that F2
indicts our file for getting backwards.

Its appendix reconciles tax-data against Census population estimates, citing Cilke
(2014) and Larrimore et al. — i.e. the PEP-vs-tax-data anchoring problem our §3.1
anchors face, already worked through by someone else. **Read that appendix before
finalizing the anchor tolerances (P4).**

Methodologically it deliberately avoids tax units, sampling *individuals* instead,
precisely to sidestep the linking problem. Useful as a contrast case, not a template.
Its one transferable trick is including **Form 1095** health-coverage records
(following Lurie & Pearce 2021), which pulls in ~9.9M people no prior study captured.

---

## 4. What to use instead — ranked

### Tier 1: directly substitutable parameters

1. **Mok (2017), CBO WP 2017-06, Table 14.** Fourteen probits: unmarried &lt;65 ×
   {0, 1, 2+ dependents}; unmarried 65+ × same; married &lt;65 × same; married 65+ ×
   same; dependent filers × {&lt;65, 65+}. Coefficients, standard errors, N, and
   weighted mean of the dependent variable all printed. Regressors are all CPS-native:
   log gross income, negative-gross-income indicator, presence indicators for wages /
   interest / dividends / self-employment / rent / retirement / Social Security,
   means-tested transfer receipt, count of household members on Medicaid, education,
   race/ethnicity.

   **Published calibration targets** (weighted filing rates): unmarried &lt;65
   0.82 / 0.77 / 0.79; unmarried 65+ 0.62 / 0.60 / 0.63; married &lt;65 0.92 / 0.92 /
   0.92; married 65+ 0.83 / 0.77 / 0.80; dependents 65+ 0.23; **dependents &lt;65 0.10**.
   **Levels:** 141.7M constructed tax units = 117.9M filers + **23.7M non-filers**.

   **A diagnostic Cilke does not report, and a caution for us:** the predicted-income
   match reproduces filing behavior for 83% of units — **94% of filers but only 27%
   of non-filers**. Whatever we build will be far better at the margin we care least
   about.

   Mok evaluates two match-ranking rules: **predicted income** (CBO's production
   method) and **predicted probability of filing**, the latter ranking units within
   group and cutting the lowest-probability tail to hit the observed non-filer share.
   **Correction (verified 2026-08-18):** an earlier draft of this note said the
   probability rule outperforms by ~10M non-filers. It does not, and no such figure is
   in the paper — her conclusion states the correct-classification share is *"similar
   under both methods."* Its genuine advantages are that simulated filers' demographic
   composition matches the linked data by construction, and that simulated non-filers'
   average income is closer to the truth. Worth testing; not worth assuming a large gain.

2. **Erard, Langetieg, Payne & Plumley (2020)**, "Flying under the Radar" (*CESifo
   Economic Studies*) / "Ghosts in the Income Tax Machinery" (MPRA 100036). The only
   modern published model of the **above-threshold** non-compliance hazard — the
   Pub 5785 side — with full coefficients, three specifications, pooled TY2001–2013,
   N≈2.47M.

   **⚠ A sign instability not to paper over.** `MARRIED` flips across vintages of the
   same model by the same authors: **+0.2025** in the 2016 IRS Research Bulletin
   (TY2000–2012), **−0.3404** in the 2020 CESifo (TY2010), **−0.4506** in the 2020
   MPRA (TY2001–2013), over nearly identical years. Do not transcribe any single
   vintage as settled. This matters directly: Pub 5785 reports &lt;20% of
   above-threshold non-filers are married, and D3's national scalar has to allocate
   across marital status somehow.

3. **Treasury OTA Technical Paper 12 (2023)**, "U.S. Treasury Individual Income Tax
   Model." A complete, current, official recipe for constructing non-filer tax units
   from information returns plus Form 1095, with the exclusion funnel, reweighting
   factors, and a weighted output table (Table A1, TY2016: **31.5M** non-filer units,
   24.5M single-no-dependent, 5.7M joint-no-dependent, 0.9M HoH-with-dependent).

4. **Hertz, Langetieg, Payne, Plumley & Jones (2021/2022)**, IRS Pub 1500 pp. 93–124 —
   Pub 5785's TY2010 predecessor with **eight side-by-side method/weight variants**,
   so we can see how far the answer moves with the method. Preferred variant: 10.27M
   obligated units against 9.30M from the administrative method.

5. **Treasury OTA (Jan 2025)**, "Income Tax Withholding Among 2022 Non-filers." Newest
   official counts anywhere: **50.343M** TY2022 non-filers, and the only published
   **persistence** data — 14.805M had not filed in any of TY2019–2021. $70.5B withheld.

### Tier 2: level estimates and imputation models

- **Lawrence, Udell & Young (2011)**, IRS Research Bulletin — TY2005: 38.6M no-return
  persons → **22.8M simulated tax returns built from March 2006 CPS family structure**,
  11.8M with an obligation. **The closest existing template for what we are building.**
- **Langetieg, Payne & Plumley (2017)** — three transcribable **self-employment
  imputation models** (probit for presence, ordered probit for category, log-amount
  regression with an explicit `SE = exp(β̂'x + e)` rule). This is the hardest missing
  -income problem for non-filers and bears directly on F7's SE-shaped above-threshold
  population.
- **Erard et al. (2014)**, NTA — bivariate probit for **CPS-ASEC under-reporting of
  Social Security and pension income**. If we build tax units on the ASEC, we need
  exactly this: our OASDI margin assumes ASEC Social Security receipt is right.
- **Census: Lin (2022)** on the ASEC tax-unit algorithm, and **Bee, Hokayem & Lin
  (2023)**, which reports that only **48.4%** of ASEC units with children agree with
  the linked 1040 on the number of qualifying children. That is the realistic accuracy
  ceiling for ASEC tax-unit construction, and it should temper what we promise.
- **Shantz (2025)**, Census Federal and State Tax Parameter Workbook — Census's filing
  rule as named published parameters, including an admitted fudge factor
  (`FED105 = $2,000`, "assumption… got us closer to the IRS targets").
- **Auten & Splinter online appendix** §1.c–1.d — an information-return non-filer
  method with **address-based marriage matching**, validated against the 2007
  rebate-driven filing surge.

### Tier 3: what peer models actually do

| Model | Filing treatment | Parameters published? |
|---|---|---|
| **TPC** | Thresholds → **Cilke probits** → uniform draw → recalibrate constants | Procedure yes (Rohaly et al. 2005 pp. 12–13); adjustment factors withheld |
| **CBO** | CPS↔SOI statistical match, ranking units within demographic group by **predicted income**; unmatched = non-filers | Method yes (Mok 2017). ⚠ An earlier draft of this note said Mok shows it yields "~10M too few non-filers" — **no such figure is in the paper; claim withdrawn.** What she does report: the predicted-income method reproduces observed filing behavior for 83% of units but only **27% of non-filers** |
| **PolicyEngine** | required ∪ (credit-eligible ∧ would-file) ∪ voluntary | **Yes** — 16-cell table in `voluntary_filing.yaml`, but hand-set, not estimated |
| **PSL taxdata** | `filing_rules.json` exists but `_must_file()` has **no call sites**; live rule is `filestat != 6` | Effectively "trust the CPS recode" |
| **TAXSIM / TRIM3** | **None.** TRIM3 states it "does not attempt to simulate the fact that some units do not file" | Nothing to transcribe |
| **PWBM** | Statutory rules + explicit elective-filing probability module | Design yes; probabilities withheld |
| **Budget Lab (current)** | DINA `filer == 0` append, no filing model | — |

**TPC's footnote 17 deserves quoting in full**, because it is the exact argument our
design memo makes: "Although the Cilke estimates are dated, they are the only evidence
of which we are aware." That was written in **2005**. Mok (2017) postdates it.

---

## 5. Transcribable tables

| Source | Table | Contents |
|---|---|---|
| **Mok (2017)** | **14** | **14 group probits, coefficients + SEs + N + weighted filing rates. The recommended replacement.** |
| Cilke (1998) | 3 (pp. 26–29) | 9 group probits, 24 variables, coefficients + SEs. Group Ns 8,469 / 3,544 / 4,379 / **462** / 2,590 / 940 / **233** / 2,413 / 692 — two groups are too thin to support 24 parameters |
| Cilke (1998) | 1 | Below-threshold non-filing rates by group; overall **55.5% non-filing ⇒ 44.5% voluntary filing, TY1990** |
| Cilke (2014) | 2 | Information-return incidence among non-filers, TY2010: SSA-1099 **55.9%**, W-2 **24.7%**, 1099-INT 15.6%, 1099-R 14.3%, 1099-G 11.1%, 1099-MISC 7.8%, 1099-DIV 4.3%, 1099-B 2.6% |
| Cilke (2014) | 3 | Non-filers by age × gender, 8 bands, total 30,158k |
| Pub 5785 | 2 | Receipt rates among potential non-filers (the F3 ceilings) |
| Pub 5785 | 3 | Obligated non-filer units by filing status |
| Erard et al. (2020) | 5 | Timely-filing probit, 3 models, pooled TY2001–2013 |
| Langetieg et al. (2017) | 3–5 | SE imputation: presence probit, category ordered probit, log-amount regression |
| Brady & Bass (2023) | 4–5 | Population decomposition and non-filer share by single year of age |
| OTA TP-12 | A1 | 31.5M non-filer units by filing status and age, TY2016 |

### Two extraction hazards

**Cilke Table 3 sign convention.** The paper contradicts itself: the appendix codes
`INON: 0 = NON-FILER, 1 = FILER`, but the table title and body say a positive
coefficient means *more likely non-filer*. Resolved empirically against the paper's own
anchor — it states the *no earned income* parameter is positive in all subgroups, and
extraction confirms `NO EARNED INCOME` is positive in all nine columns (0.5069 …
0.9598). **Table 3 predicts P(non-filer); ignore the appendix coding.**

**`pdftotext -layout` scrambles the row-label/value alignment in Cilke Table 3.**
PyMuPDF word-position extraction works. Naive extraction silently mis-assigns
coefficients — the worst possible failure mode, since it produces plausible numbers.

### ⚠ Definitional incompatibility — never difference these counts

They estimate different quantities: Pub 5785's **11.2M** = tax units *with an
obligation*; its **50M** = *potential non-filer persons*; OTA TP-12's **31.5M** = *all*
non-filer tax units, no obligation screen; OTA's **50.3M** = persons on an information
return but on no tax return; Brady–Bass's **42.4M** = *non-dependent* non-filers;
Mok's **23.7M** = CPS-constructed non-filer units; Lawrence et al.'s 38.6M persons /
11.8M obligated. Our own comparable-universe anchor (38–41M adults) is yet another.
Every cross-source comparison must state which estimand it is using.

**Tax gap, current vintage:** Pub 5869 (Rev. 10-2024) puts the TY2022 nonfiling gap at
**$63B**. Note a 2024 methodology change "to better account for the share of taxpayers
who will eventually file a late tax return as opposed to never filing" **cut the TY2021
projection by $20B** — anything citing the Oct-2023 figures is superseded.

---

## 6. Gaps — what we would have to estimate ourselves

1. **No modern re-estimation of Cilke's exact estimand.** Mok is the nearest, but
   estimates over *all* tax units on age-65 / 0-1-2+ cells, not Cilke's below-threshold,
   age-62, nine-group design. If we want that specific object we must estimate it.
2. **No successor to Pub 5785.** TY2017–2022 are projections without counts; the
   fall-2025 release is delayed.
3. **No study of TCJA's effect on filing rates or filer composition.**
4. **Coefficients withheld** in several models that describe themselves: Pub 5785's
   SE-imputation and linkage-reweighting probits, TPC's adjustment factors, PWBM's
   elective-filing probabilities.
5. **Erard (2018)**, "Modeling Qualitative Outcomes by Supplementing Participant Data
   with General Population Data" — the methodological basis for every calibrated probit
   above, cited in all three Erard papers, **not publicly locatable**. Without it the
   calibrated-probit estimator is not reimplementable from published sources.
6. **Rothbaum (2023)**, "Research on Creating Synthetic Data to Better Model the Income
   of Nonfilers through the Release of Public-Use Parameters" — cited and operationally
   used by Census, but in no public series. By its title this is exactly a public-use
   non-filer parameter release. **Worth emailing Census SEHSD directly.**
7. **No published single "tax-unit match rate"** for survey-vs-administrative unit
   construction; assemble from Mok's 71% full-match and Bee–Hokayem–Lin's 48.4%.

---

## 7. Verification status of this note

**Mok (2017) was obtained and checked directly on 2026-08-18** (JI downloaded it to the
Affordability literature folder, `Literature/Reweighting/53125-nonfilers.pdf`; 60pp,
md5 `eb649ef0c5918ad0571b2005dc1d2437`). Everything this note attributes to Mok is
confirmed **except one claim, corrected below**:

| Claim | Status |
|---|---|
| Title, author, "Working Paper 2017-06", September 2017 | ✅ verified, p1 |
| 2007 CPS ASEC linked to IRS **Individual Master File**, TY2006 returns | ✅ verified, pp8, 10 |
| Table 14 = 14 group probits (A/B unmarried &lt;65 and 65+, C/D married &lt;65 and 65+, each × 0/1/2+ dependents; E dependent filers × 2 ages) | ✅ verified, pp48–50 |
| Panel A coefficients (log gross income 0.125\*\*\* (0.010), wages 0.669\*\*\* (0.046), interest 0.211\*\*\* (0.027), dividends 0.14\*\*\* (0.043), SE −0.108\*\* (0.049), SS −0.114\*\* (0.045), means-tested −0.276\*\*\*, Medicaid −0.273\*\*\*, intercept −0.696\*\*\*) | ✅ verified exactly |
| Cell Ns 24,634 / 5,594 / 5,931 (unmarried &lt;65) etc.; all 14 weighted filing rates 0.82/0.77/0.79, 0.62/0.60/0.63, 0.92/0.92/0.92, 0.83/0.77/0.80, 0.23, 0.10 | ✅ verified exactly |
| 141.7M units = 117.9M filers + 23.7M non-filers (40.7M individuals) | ✅ verified, p13 |
| Match diagnostic: 83% of units correctly simulated, 94% of filers, **27% of non-filers** | ✅ verified, p24 |
| "similar to the set of covariates used by Cilke" | ✅ verified, p23 — the quote is exact |
| **"Rank-and-cut outperforms the match-residual method by ~10M non-filers"** | ❌ **NOT SUPPORTED — corrected in §4.1.** Mok's conclusion says the correct-classification share is *"similar under both methods."* No ~10M figure appears anywhere in the paper |
| **"Mok shows CBO's production method yields ~10M too few non-filers"** (§4 Tier-3 table) | ❌ **NOT SUPPORTED — withdrawn.** Same absent figure. A search of every million-scale number in the paper returns no such comparison |

**Two things the direct read added that the search pass missed**, both now written into
the design memo §3.2.2:

1. **Panel E's columns run "Age 65 or Older" *first*, then "Under Age 65"** — the reverse
   of the intuitive order, and automated text extraction returns the two headers in the
   wrong sequence. Sample sizes (909 vs 62,438) disambiguate; the page was checked as a
   rendered image. Transcribe Table 14 visually, not from a text dump.
2. **Mok's frame excludes the institutionalized and military-barracks populations** —
   "the characteristics of filers and nonfilers outside of the CPS sampling frame, such
   as people who are institutionalized or living outside the United States, are not
   considered" (p8). Her coefficients are estimated on a household-population frame,
   while our PUF universe includes group quarters. Scoring her equations on GQ records
   extrapolates outside the estimation frame, and must be done under a stated assumption
   and reported separately. The same applies to Cilke.

Still not retrieved: Erard & Ho (2001) (paywalled — get through Yale), TPC and JCT pages
(403), Lurie & Pearce (2021) (paywalled), TIGTA reports (timeouts), and Cilke (1994),
which appears never to have been posted. **Whether TPC still uses Cilke in 2026 is
therefore unconfirmed** — the 2005 documentation is the last public evidence.

Not retrieved at all: Erard & Ho (2001) (paywalled — get it through Yale), TPC and JCT
pages (403), Lurie & Pearce (2021) (paywalled), TIGTA reports (timeouts), and Cilke
(1994), which appears never to have been posted. **Whether TPC still uses Cilke in 2026
is therefore unconfirmed** — the 2005 documentation is the last public evidence.

---

## 8. Recommended next step

**Fit both.** Cilke Table 3 and Mok Table 14 use overlapping CPS-native covariates, so
we can score the same ASEC file under each, calibrate both to the same administrative
target, and compare the implied non-filer age and income distributions against
Brady–Bass's by-age shares and Cilke (2014) Table 3. That turns "keep the 1990s slopes?"
into an empirical question and produces the negative-result documentation either way —
which is what the design memo §8 asked for. Budget the extra work as small: it is one
additional scoring pass over a file we have to build anyway.

If only one is fit, fit **Mok**.

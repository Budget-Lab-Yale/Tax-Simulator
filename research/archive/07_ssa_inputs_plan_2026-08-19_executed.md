# SSA Inputs — Plan to Close the Last Stage D Blocker

**Date:** 2026-08-19
**Status:** **Tasks 1-3 EXECUTED on the server 2026-08-19**, plus the Chrome
check from task 4. Results are recorded inline below under each task; task 4's
readers remain the open work. Originally written as a plan, before anything had
been run on the server.
**Blocker this closes:** `04_findings.md` §5 item 1 — the two SSA statcomps
families were created empty because ssa.gov refuses automated retrieval. They
gate the state × age allocation of the residual (decision **D6**) and the
covered-worker margin.
**Companions:** `06_verify_ssa_inputs.R` (the check task 1 runs),
`../nonfiler_residual_design.md` §3.1 steps 5–6 and §4.1 (what the margins are
for), `resources/README.md`.

---

## Background: what was actually blocking this, and what unblocks it

`01_fetch_residual_inputs.R` reports the two SSA families as blocked, and the
`README_MANUAL_DOWNLOAD.md` it writes says to download on a workstation. Worth
recording precisely *why*, because the reason narrows the fix:

**ssa.gov's block is on TLS fingerprint, not user agent.** Verified 2026-08-19
from a workstation, not just the cluster: `curl` with full browser headers → 403;
.NET `Invoke-WebRequest` → 403; the hosted WebFetch service → 403; and the block
covers static assets (`.xlsx`, `.json`) as well as HTML. **Real Chrome in
`--headless=new` mode retrieves everything without complaint.**

So this is not inherently a manual step. If Chrome or Chromium is available on
the cluster, `01_fetch_residual_inputs.R` could shell out to it and the SSA
families would fetch automatically like the others. **Worth checking (`which
google-chrome chromium chromium-browser`) before accepting a permanent manual
step in the pipeline** — see task 4.

---

## Task 1 — Confirm the files already on the server are the right ones

JI downloaded the four workbooks to the server manually on 2026-08-19. Nothing
has checked them, and the failure modes are quiet ones: the wrong data year, a
per-state extract (`ca.xlsx`) instead of the all-tables workbook, or a
publication-structure change between vintages.

```bash
module load R/4.4.1-foss-2022b && \
  Rscript other/state_tax_research/nonfiler_residual/06_verify_ssa_inputs.R
```

The script finds the workbook for each anchor year in each family, reads the
national ("All areas") row of the sheet the anchors will consume, and compares
against control totals verified against the publications:

| Family | Sheet | Control totals checked |
|---|---|---|
| `SSA-OASDI-SC` | `Table 2` | 11 values — total beneficiaries, six benefit-type columns, disability spouses/children, and **Aged 65 or older: Men / Women** |
| `SSA-EEDATA-SC` | `Table 1` | 4 values — total persons, **wage-and-salary**, self-employed, taxable earnings ($000s) |

It accepts either SSA's own filenames (`oasdi_sc22.xlsx`) or the pattern in the
README (`oasdi_sc_2022_*.xlsx`), refuses a two-letter per-state extract, exits
non-zero on any mismatch, and writes
`results/ssa_input_verification.csv`.

**Expected on success** (these are the numbers to eyeball):

| Margin | TY2017 | TY2022 |
|---|---|---|
| OASDI beneficiaries aged 65+ | 45,808,776 | 52,052,807 |
| Persons with covered wage-and-salary earnings | 161,986,000 | 168,525,999 |

**RESULT (2026-08-19): PASS, all four cells.** JI placed **more than the plan
asked for** — OASDI 2017-2025 (9 workbooks) and EEDATA 2017-2023 (7), which
covers the back years design memo §8 wants. Both headline margins matched
exactly: 65+ beneficiaries 45,808,776 / 52,052,807, covered wage-and-salary
persons 161,986,000 / 168,525,999. Record at
`results/ssa_input_verification.csv`.

**Acceptance:** all four year × family cells PASS. ~~A sanity cross-check that
is not in the script: 52.05M against the national anchor's `65p` PEP population
of 57,505,037 implies ~90% coverage, which matches the publication's own Table 1
coverage percentage.~~ **That cross-check was wrong and is withdrawn.** It
compares `All areas` beneficiaries — which include people residing abroad and in
the territories — against a **US-resident** population. On a consistent
51-jurisdiction basis the ratio is **0.878**, stable across 2017-2024. It only
appeared to agree with the publication's own Table 1 percentage because through
the 2018 edition SSA published that percentage on the same mismatched basis;
from the 2019 edition it is the consistent 0.878-style ratio. The corrected
check: **51-jurisdiction 65+ beneficiaries over the PEP 65+ population should
land near 0.878**, and a result near 0.90 means `All areas` has been picked up
by mistake. See `SSA-OASDI-SC/NOTES.md` §5.

**On failure:** do not build anchors on the files. The script prints which
control totals differ, which usually identifies the problem immediately (all
values wrong → wrong year or wrong publication; a few wrong → vintage change in
the table's column layout, which the reader in task 4 must then handle).

---

## Task 2 — Add the OASDI flat-series files to the shared store

**What they are.** OASDI-SC publishes a **flattened multi-year time series** —
five tables as JSON, covering **1999-12 through 2025-12** — linked from the
publication's `flat-series.html` and catalogued at data.gov. Two are useful:

| File | Rows | Contents |
|---|---|---|
| `oasdi_sc_flatseries_table2_beneficiaries.json` | 1,520 | **The D6 anchor.** Beneficiaries by month × state × benefit type, with `persons_oasdi_65_older_men` / `_women` |
| `oasdi_sc_flatseries_table1_population_shares.json` | 1,404 | Total and 65+ population by state with the share receiving benefits — useful as an independent read on the PEP denominator |

**Why bother, given the workbooks already verify.** Three reasons: every year at
once instead of a download per anchor year, which matters as soon as back-year
weights are fit (design memo §8 lists 2014 and 2016-2019 as needed then); a
documented schema with labelled `dimensions` and `measures`, which is far better
to write a reader against than a workbook with two-row merged headers; and it
**agrees with the workbooks to the digit** (both give 65+ of 45,808,776 for 2017
and 52,052,807 for 2022), so adopting it costs no reconciliation.

**They are staged in this repo** at
`resources/ssa_flatseries/`, committed because ssa.gov cannot be reached from the
cluster — the same reasoning as the Mok PDF. **Copy them into the store:**

```bash
cp other/state_tax_research/nonfiler_residual/resources/ssa_flatseries/*.json \
   /nfs/roberts/project/pi_nrs36/shared/raw_data/SSA-OASDI-SC/
module load R/4.4.1-foss-2022b && \
  Rscript other/state_tax_research/nonfiler_residual/01_fetch_residual_inputs.R
```

Re-running 01 registers them in `SSA-OASDI-SC/manifest.csv` with size, md5 and
date, and should now print `registered N manually-placed file(s)` for both
families instead of `BLOCKED (ssa.gov 403)`.

**RESULT (2026-08-19): DONE** — `registered 11` (OASDI, incl. both JSONs) and
`registered 7` (EEDATA). The staged copies have been deleted from git, as
`resources/README.md` directed. Three fixes to `01_fetch_residual_inputs.R`
were needed along the way:
- **`retrieved` was silently blanked on every re-run.** `fread` parses the ISO
  date back as `IDate`, so `rbindlist` coerced the incoming character date to
  integer and wiped the dates of every already-registered file. Census-PEP and
  BLS-QCEW had already lost theirs; restored from file mtimes (2026-08-16).
- Manually-placed files registered with `year = NA`. Now parsed from SSA's
  two-digit filename suffix; the flat series correctly stays `NA` (multi-year).
- `NOTES.md` would have been registered as a data file; now excluded.

**Source-of-record decision: the flat series, workbooks as the cross-check.**
The cross-check has been run and is exact — **59 areas × 11 measures × 2 anchor
years, zero mismatches**. Quirks the reader must handle are in
`SSA-OASDI-SC/NOTES.md` §2: **2010 is missing** from the series, and the U.S.
Virgin Islands is labelled `Virgin Islands` before 2007.

**A decision to make explicitly, not by default:** whether the flat series or the
per-year workbooks is the *source of record* for the OASDI margin. Recommendation
is the flat series, with the workbooks retained as the cross-check that has
already been performed. Whichever is chosen, the reader in task 4 should read one
and assert against the other, so the agreement is enforced rather than assumed.

**No equivalent exists for EEDATA-SC** — per-year workbooks only, and the series
ends at data year **2023**. Any anchor year after 2023 has no covered-worker
margin at all, which bounds how far the method extends without a substitute.

---

## Task 3 — Write the manifest document for the two new families

`manifest.csv` records provenance mechanically (path, url, bytes, md5, date). It
does not record what the files *mean*, which is what the next person needs. Write
a companion note per the convention the design memo §4.1 points at for the IRS-Ind
store (`notes/national_bysize.md`), covering both families in one document:

**RESULT (2026-08-19): DONE.** Drafted in the repo at
`resources/ssa_notes/SSA-{OASDI,EEDATA}-SC_NOTES.md` and placed in the store as
`NOTES.md` in each family (matching IRS-Ind's uppercase filename, one
consolidated file per family). All six points below are covered, and every
number in them was computed from the files rather than copied from a summary.
Findings worth reading before writing the readers:

- **Use the 51-jurisdiction sum, not `All areas`** — the anchor values are
  44,635,968 (2017) and 50,766,317 (2022), 2.5-2.6% below the `All areas` rows.
- **EEDATA publishes two universes**: Tables 1/2 are OASDI-covered, Tables 4/5
  are HI (Medicare)-covered — broader by ~4.1M persons, and **uncapped** in
  dollars. **The QCEW dollar cross-check must use Table 4**: HI wage-and-salary
  earnings match QCEW to ~1% (1.007× in 2017, 1.013× in 2022) while OASDI's
  capped earnings sit ~17% low.
- **EEDATA is a 1% sample** (Continuous Work History Sample); OASDI-SC is 100%
  data. Small-state EEDATA margins carry real sampling error.
- **EEDATA Tables 2/5 carry state × age**, in SS-eligibility bands (…, 60-61,
  62-64, 65-69, 70+), which do not nest inside `age_band()` without a decision.
- **Point 4 answered, not deferred:** the 2017 and 2022 editions are on the
  same geographic basis in both families (identical source and residence notes,
  identical layouts, exact geography partitions, stable 51-juris coverage ratio
  0.873-0.881 across 2017-2024). The two discontinuities found are documented:
  Table 1's **national percentage basis changed with the 2019 edition**, and the
  Virgin Islands label changed in 2007.
- **Point 6 (vintage pairing) answered:** SSA Table 1's population is the Census
  vintage current at publication, ~0.5% above the later PEP vintages in
  `Census-PEP/`. Rule recorded: **counts from SSA, denominators from PEP**,
  never an SSA-published share against a PEP denominator.

**Home:** `raw_data/SSA-OASDI-SC/NOTES.md` and `raw_data/SSA-EEDATA-SC/NOTES.md`
in the store (mirroring IRS-Ind), with the substance drafted in the repo so it is
reviewable in git before it is placed.

**Contents, per family:**

1. **Publication identity** — full title, publisher, release frequency, landing
   page, data.gov catalogue page, and the fact that retrieval requires a real
   browser engine.
2. **Table → measure map for the tables we actually consume**, naming the sheet
   and the columns. For OASDI-SC: `Table 2`, columns `Aged 65 or older: Men` and
   `... Women`, summed to the 65+ margin. For EEDATA-SC: `Table 1`, column
   `Wage and salary` under `Number` (**not** `Total`, which includes the
   self-employed), and `Taxable earnings: Total` in **$ thousands**. Also record
   that EEDATA `Table 2` carries persons by state **× age group** (Under 20,
   20–29, 30–39, …) — an age dimension the design memo did not expect to have,
   and the natural input to the working-age layer of D6.
3. **Universe and concept caveats**, the part that will otherwise be rediscovered
   painfully:
   - EEDATA counts **persons with covered earnings**, HT2 counts **returns with
     wages**. The margin is a difference of two different objects; the memo's
     ~75% ±9pp ratio is the wedge, not an error.
   - Coverage is *Social-Security-covered* employment. Non-covered workers (some
     state and local government, certain railroad employment) are out of scope by
     construction and will not appear in the margin.
   - OASDI counts are **December current-payment status**, a point-in-time stock;
     tax concepts are annual flows.
   - The 65+ split is published **by sex only**, so finer age detail within 65+
     is not available from this source — relevant if `age_band()`'s `65_74`/`75p`
     split is retained (design memo pre-flight, task P2).
   - Territories and an "All areas" row are present; state tables carry 59 rows,
     not 51. Reader must select deliberately.
4. **Geography-assignment continuity.** SSA has been revising how it assigns
   geography in these tabulations. Compare each edition's technical notes across
   2017 and 2022 and **record explicitly whether the two years are on the same
   basis** — if not, the cross-year comparison the anchors make needs a caveat or
   an adjustment. This is currently an untested assumption.
5. **Consumers** — `other/state_tax_research/nonfiler_residual/` (Stage D
   anchors), and later Affordability-Index. Per design memo §7.3, any margin
   shared between the two systems must carry an explicit **universe tag**
   (`resident` vs `household`); note that SSA's universe is neither exactly —
   it is *covered workers* and *beneficiaries*, which cut across both.
6. **Vintage pairing** — which PEP vintage each SSA year is paired with, closing
   the loop with the design memo §8 open question.

---

## Task 4 — Follow-on, once tasks 1–3 pass

1. ~~**Check for Chrome on the cluster.**~~ **DONE 2026-08-19: absent.**
   `google-chrome`, `chromium` and `chromium-browser` are all off `PATH`, and no
   Lmod module provides a browser engine. **The manual step stands**; recorded in
   both store `NOTES.md` files so nobody re-litigates it. Node 24 is available, so
   a Puppeteer-managed Chromium is the only route if this is ever worth
   automating — not attempted.
2. **Write the readers** — the real remaining work.
   `02_build_residual_anchors.R:197` still hardcodes `ssa_covered_persons =
   NA_real_`, and nothing parses either publication. One reader per family,
   following the `read_pub1304_t16()` pattern already in that script.
3. **Wire the margins in:** OASDI 65+ counts into the state × age allocation to
   close **D6**; covered-worker persons into the
   `nonfiler_wage_margin_{year}.csv` columns, with the returns-per-person ratio
   and the QCEW dollar cross-check.
4. **Re-run 01 → 02 → 03** and update `04_findings.md` — D6 moves from
   "partially resolved" to resolved, and T5's state margins gain the OASDI and
   covered-wage columns they were specified with.

---

## Acceptance for this plan as a whole

- [x] `06_verify_ssa_inputs.R` exits 0 on the server, with
  `results/ssa_input_verification.csv` committed as the record.
- [x] Both SSA families' `manifest.csv` list their files;
  `01_fetch_residual_inputs.R` prints `registered` rather than `BLOCKED` for both.
- [x] A `NOTES.md` exists in each family covering the six points in task 3, with
  the geography-continuity question answered rather than deferred.
- [x] `04_findings.md` §5 item 1 is struck.

**All acceptance criteria met 2026-08-19.** What remains is task 4's readers,
which were always follow-on work and are now the only thing between here and a
resolved D6.

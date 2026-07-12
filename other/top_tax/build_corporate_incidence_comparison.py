#!/usr/bin/env python3
"""Compare current and legacy corporate incidence on the s_corp_r28 run."""

import csv
import html
from pathlib import Path


RUN_ROOT = Path("/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v1")
SCENARIO = "s_corp_r28"
SUPP = RUN_ROOT / SCENARIO / "static" / "supplemental"
OUT = Path(__file__).with_name("corporate_incidence_comparison_28pct.html")
YEARS = (2027, 2036)
GROUPS = ("Overall", "Negative income", "Quintile 1", "Quintile 2", "Quintile 3",
          "Quintile 4", "Quintile 5", "Top 10%", "Top 5%", "Top 1%",
          "Top 0.1%", "Top 0.01%")
CURRENT_ALLOCATED_SHARE = 0.675  # sigma=.375; capital legs receive 40% haircut
FOREIGN_DOMESTIC_SHARE = 0.60


def read_csv(path):
    with path.open(newline="") as f:
        return list(csv.DictReader(f))


def f(row, key):
    return float(row[key])


def old_labor_share(year):
    # Legacy code: .2 * clamp((year - first_year) / 10, 0, 1), first_year=2027.
    return 0.2 * max(0.0, min(1.0, (year - 2027) / 10.0))


def money_b(value):
    return f"${value:,.2f}B"


def dollars(value):
    return f"${value:,.0f}"


def pct(value):
    return f"{100 * value:.3f}%"


def pp(value):
    return f"{100 * value:+.3f} pp"


dist = read_csv(SUPP / "distribution.csv")
etr = read_csv(SUPP / "distribution_etrs.csv")

# The broad tier differs from the death tier only by CIT and VAT. VAT is zero in
# this corporate-only scenario, so their difference recovers current CIT dollars.
dist_index = {(int(r["year"]), r["group_dimension"], r["group"], r["taxes_included"]): r for r in dist}


def dist_row(year, group, tier):
    dim = "Overall" if group == "Overall" else "Income"
    return dist_index[(year, dim, group, tier)]


dist_results = []
year_totals = {}
for year in YEARS:
    overall_broad = dist_row(year, "Overall", "iit_pr_death_cit_vat_wealth")
    overall_death = dist_row(year, "Overall", "iit_pr_death_wealth")
    current_total = f(overall_broad, "net_change") - f(overall_death, "net_change")
    national_delta = current_total / CURRENT_ALLOCATED_SHARE
    ls = old_labor_share(year)
    legacy_total = national_delta * (ls + (1 - ls) * FOREIGN_DOMESTIC_SHARE)
    year_totals[year] = (national_delta, current_total, legacy_total, ls)

    # distribution.csv stores labor and capital as weighted group totals in $B.
    labor_total = f(overall_broad, "labor")
    capital_total = f(overall_broad, "capital")
    for group in GROUPS:
        broad = dist_row(year, group, "iit_pr_death_cit_vat_wealth")
        death = dist_row(year, group, "iit_pr_death_wealth")
        n = f(broad, "n_tax_units")
        current_cit = f(broad, "net_change") - f(death, "net_change")
        labor_sum = f(broad, "labor")
        capital_sum = f(broad, "capital")
        legacy_cit = national_delta * (ls * labor_sum / labor_total +
                                       (1 - ls) * FOREIGN_DOMESTIC_SHARE * capital_sum / capital_total)
        legacy_net = f(death, "net_change") + legacy_cit
        ati_baseline = f(broad, "ati_baseline")
        current_ati_change = f(broad, "pct_chg_ati")
        current_ati_reform = ati_baseline * (1 + current_ati_change)
        legacy_ati_reform = current_ati_reform + current_cit - legacy_cit
        legacy_ati_change = legacy_ati_reform / ati_baseline - 1
        dist_results.append({
            "year": year, "group": group, "n": n,
            "current_cit": current_cit, "legacy_cit": legacy_cit,
            "current_avg": f(broad, "avg"),
            "legacy_avg": legacy_net * 1e9 / n,
            "current_share": current_cit / current_total if current_total else 0,
            "legacy_share": legacy_cit / legacy_total if legacy_total else 0,
            "current_ati_change": current_ati_change,
            "legacy_ati_change": legacy_ati_change,
        })

# ETR experiment: use expanded income, fixed ranks, headline current convention.
# Recover non-CIT numerators and pre-CIT denominators from the published rows,
# then insert legacy corporate allocations. Baseline receipts use the old long-run
# 20/80 split because the historical delta allocator did not define level ETRs.
etr_rows = [r for r in etr if int(r["year"]) in YEARS
            and r["income_definition"] == "expanded" and r["ranking"] in ("fixed", "n/a")
            and r["taxes_included"] == "wealth_cit_vat"
            and r["corp_convention"] == "equity_supernormal"
            and ((r["group_dimension"] == "Overall" and r["group"] == "Overall")
                 or r["group_dimension"] == "Income percentile")]

etr_results = []
for r in etr_rows:
    year = int(r["year"])
    group = r["group"]
    if group not in GROUPS:
        continue
    broad = dist_row(year, group, "iit_pr_death_cit_vat_wealth")
    overall = dist_row(year, "Overall", "iit_pr_death_cit_vat_wealth")
    n = f(broad, "n_tax_units")
    labor_share_group = f(broad, "labor") / f(overall, "labor")
    capital_share_group = f(broad, "capital") / f(overall, "capital")

    inc_b = f(r, "income_baseline")
    inc_r = f(r, "income_reform")
    tax_b = f(r, "tax_baseline")
    tax_r = f(r, "tax_reform")
    current_cit_b = f(r, "etr_corp_baseline") * inc_b
    current_cit_r = f(r, "etr_corp_reform") * inc_r
    current_cit_delta = current_cit_r - current_cit_b

    # Recover national baseline corporate receipts from the overall row below.
    overall_match = next(x for x in etr_rows if int(x["year"]) == year
                         and x["reform_leg"] == r["reform_leg"] and x["group"] == "Overall")
    overall_inc_b = f(overall_match, "income_baseline")
    overall_current_cit_b = f(overall_match, "etr_corp_baseline") * overall_inc_b
    national_baseline = overall_current_cit_b / CURRENT_ALLOCATED_SHARE
    legacy_cit_b = national_baseline * (0.20 * labor_share_group +
                                        0.80 * FOREIGN_DOMESTIC_SHARE * capital_share_group)
    national_delta = year_totals[year][0]
    ls = old_labor_share(year)
    legacy_delta = national_delta * (ls * labor_share_group +
                                     (1 - ls) * FOREIGN_DOMESTIC_SHARE * capital_share_group)
    legacy_cit_r = legacy_cit_b + legacy_delta

    core_inc_b = inc_b - current_cit_b
    core_inc_r = inc_r - current_cit_r
    noncit_tax_b = tax_b - current_cit_b
    noncit_tax_r = tax_r - current_cit_r
    legacy_inc_b = core_inc_b + legacy_cit_b
    legacy_inc_r = core_inc_r + legacy_cit_r
    legacy_tax_b = noncit_tax_b + legacy_cit_b
    legacy_tax_r = noncit_tax_r + legacy_cit_r
    etr_results.append({
        "year": year, "leg": r["reform_leg"], "group": group,
        "current_base": f(r, "etr_baseline"), "current_reform": f(r, "etr_reform"),
        "current_change": f(r, "etr_reform") - f(r, "etr_baseline"),
        "legacy_base": legacy_tax_b / legacy_inc_b,
        "legacy_reform": legacy_tax_r / legacy_inc_r,
        "legacy_change": legacy_tax_r / legacy_inc_r - legacy_tax_b / legacy_inc_b,
        "current_cit_delta": current_cit_delta,
        "legacy_cit_delta": legacy_delta,
    })


def table(headers, rows):
    head = "".join(f"<th>{html.escape(h)}</th>" for h in headers)
    body = "".join("<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows)
    return f'<div class="table-wrap"><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def grouped_bar_chart(title, subtitle, labels, series, value_format, zero=False):
    """Build a responsive inline-SVG grouped horizontal bar chart."""
    width, left, right, top = 940, 145, 35, 68
    plot_w = width - left - right
    bar_h, gap, group_gap = 10, 5, 14
    per_group = len(series) * (bar_h + gap) - gap
    height = top + len(labels) * (per_group + group_gap) + 38
    values = [v for _, _, vals in series for v in vals]
    lo = min(0, min(values)) if zero else 0
    hi = max(values)
    span = hi - lo or 1
    x = lambda v: left + (v - lo) / span * plot_w
    zero_x = x(0)
    out = [f'<figure class="chart"><figcaption><strong>{html.escape(title)}</strong><span>{html.escape(subtitle)}</span></figcaption>',
           f'<svg viewBox="0 0 {width} {height}" role="img" aria-label="{html.escape(title)}">']
    for tick in range(5):
        val = lo + span * tick / 4
        xx = x(val)
        out.append(f'<line x1="{xx:.1f}" y1="{top-10}" x2="{xx:.1f}" y2="{height-30}" class="grid"/>')
        out.append(f'<text x="{xx:.1f}" y="{height-10}" class="tick" text-anchor="middle">{html.escape(value_format(val))}</text>')
    if zero:
        out.append(f'<line x1="{zero_x:.1f}" y1="{top-12}" x2="{zero_x:.1f}" y2="{height-28}" class="zero"/>')
    for i, label in enumerate(labels):
        gy = top + i * (per_group + group_gap)
        out.append(f'<text x="{left-10}" y="{gy + per_group/2 + 4:.1f}" class="label" text-anchor="end">{html.escape(label)}</text>')
        for j, (name, color, vals) in enumerate(series):
            v = vals[i]
            yy = gy + j * (bar_h + gap)
            bx, bw = min(zero_x, x(v)), abs(x(v) - zero_x)
            out.append(f'<rect x="{bx:.1f}" y="{yy:.1f}" width="{max(bw,1):.1f}" height="{bar_h}" fill="{color}" rx="1"/>')
    lx = left
    for name, color, _ in series:
        out.append(f'<rect x="{lx}" y="20" width="12" height="12" fill="{color}" rx="1"/><text x="{lx+18}" y="30" class="legend">{html.escape(name)}</text>')
        lx += 165
    out.append('</svg></figure>')
    return ''.join(out)


sections = []
for year in YEARS:
    rows = []
    for x in dist_results:
        if x["year"] != year:
            continue
        rows.append((html.escape(x["group"]), dollars(x["current_avg"]), dollars(x["legacy_avg"]),
                     money_b(x["current_cit"]), money_b(x["legacy_cit"]),
                     pct(x["current_share"]), pct(x["legacy_share"])))
    national, current, legacy, ls = year_totals[year]
    exclusive = [x for x in dist_results if x["year"] == year and
                 (x["group"] == "Negative income" or x["group"].startswith("Quintile"))]
    chart = grouped_bar_chart(
        f"{year} corporate burden distribution",
        "Share of the US-household corporate allocation; exclusive income groups",
        [x["group"] for x in exclusive],
        [("Current stock-based", "#17664f", [100*x["current_share"] for x in exclusive]),
         ("Legacy capital-income", "#245f91", [100*x["legacy_share"] for x in exclusive])],
        lambda v: f"{v:.0f}%")
    all_groups = [x for x in dist_results if x["year"] == year and x["group"] != "Overall"]
    ati_chart = grouped_bar_chart(
        f"{year} change in after-tax income",
        "Percent change in ATI; top-percentile groups overlap with quintiles",
        [x["group"] for x in all_groups],
        [("Current stock-based", "#17664f", [100*x["current_ati_change"] for x in all_groups]),
         ("Legacy capital-income", "#245f91", [100*x["legacy_ati_change"] for x in all_groups])],
        lambda v: f"{v:+.2f}%", zero=True)
    sections.append(f"""
      <h3>{year}</h3>
      <p class="summary">National corporate revenue change: <strong>{money_b(national)}</strong>. Current household allocation: <strong>{money_b(current)}</strong>. Legacy household allocation: <strong>{money_b(legacy)}</strong>. Legacy labor share in this year: <strong>{100*ls:.0f}%</strong>.</p>
      {chart}
      {ati_chart}
      {table(('Group','Current avg. change','Legacy avg. change','Current CIT','Legacy CIT','Current CIT share','Legacy CIT share'), rows)}
    """)

etr_sections = []
for year in YEARS:
    chart_groups = ("Quintile 1", "Quintile 5", "Top 10%", "Top 1%", "Top 0.1%", "Top 0.01%")
    chart_rows = {(x["leg"], x["group"]): x for x in etr_results if x["year"] == year}
    etr_sections.append(grouped_bar_chart(
        f"{year} ETR change under each incidence method",
        "Percentage-point change in expanded-income ETR; fixed ranks",
        list(chart_groups),
        [("Current static", "#17664f", [100*chart_rows[("static", g)]["current_change"] for g in chart_groups]),
         ("Legacy static", "#245f91", [100*chart_rows[("static", g)]["legacy_change"] for g in chart_groups]),
         ("Current conventional", "#72a892", [100*chart_rows[("conventional", g)]["current_change"] for g in chart_groups]),
         ("Legacy conventional", "#7c9fc0", [100*chart_rows[("conventional", g)]["legacy_change"] for g in chart_groups])],
        lambda v: f"{v:+.2f} pp", zero=True))
    for leg in ("static", "conventional"):
        rows = []
        for x in etr_results:
            if x["year"] == year and x["leg"] == leg:
                rows.append((html.escape(x["group"]), pct(x["current_base"]), pct(x["current_reform"]), pp(x["current_change"]),
                             pct(x["legacy_base"]), pct(x["legacy_reform"]), pp(x["legacy_change"])))
        etr_sections.append(f"<h3>{year} {leg.title()}</h3>" + table(
            ('Group','Current base','Current reform','Current change','Legacy base','Legacy reform','Legacy change'), rows))

document = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>28% Corporate Rate: Current vs Legacy Incidence</title>
<style>
:root{{--ink:#17201c;--muted:#5d6862;--line:#d9dfdb;--band:#f2f5f3;--green:#17664f;--blue:#245f91;--gold:#96611c}}
*{{box-sizing:border-box}} body{{margin:0;color:var(--ink);font:15px/1.55 Arial,sans-serif;letter-spacing:0}}
header{{padding:42px 24px;background:#19352d;color:white}} header>div,main{{width:min(1180px,calc(100% - 36px));margin:auto}}
h1{{margin:0 0 8px;font:700 36px/1.15 Georgia,serif;letter-spacing:0}} h2{{margin:42px 0 10px;font:700 26px Georgia,serif;letter-spacing:0}} h3{{margin:28px 0 8px;font-size:18px}}
.sub{{color:#d8e5df;max-width:850px}} main{{padding:28px 0 60px}} .callout{{padding:16px 18px;background:var(--band);border-left:4px solid var(--green);margin:15px 0}}
.method{{border-left-color:var(--gold)}} code{{background:#edf1ef;padding:2px 5px;overflow-wrap:anywhere}} ul{{padding-left:22px}}
.summary{{color:#36413c}} .table-wrap{{overflow:auto;border:1px solid var(--line);margin:10px 0 24px}}
table{{width:100%;border-collapse:collapse;min-width:920px;font-variant-numeric:tabular-nums}} th,td{{padding:9px 11px;border-bottom:1px solid var(--line);text-align:right;white-space:nowrap}} th:first-child,td:first-child{{text-align:left}} th{{background:var(--band);font-size:12px;text-transform:uppercase}} tr:last-child td{{border-bottom:0}}
.chart{{margin:16px 0 24px;border:1px solid var(--line);padding:14px 16px 8px}} .chart figcaption{{display:flex;flex-direction:column;margin-bottom:4px}} .chart figcaption span{{color:var(--muted);font-size:13px}} .chart svg{{display:block;width:100%;height:auto;max-height:510px}} .chart .grid{{stroke:#e2e7e4;stroke-width:1}} .chart .zero{{stroke:#69736e;stroke-width:1.2}} .chart text{{font-family:Arial,sans-serif;letter-spacing:0;fill:#34403a}} .chart .tick{{font-size:11px}} .chart .label{{font-size:12px}} .chart .legend{{font-size:12px}}
.current{{color:var(--green)}} .legacy{{color:var(--blue)}} footer{{margin-top:42px;padding-top:16px;border-top:1px solid var(--line);color:var(--muted);font-size:13px}}
@media(max-width:600px){{h1{{font-size:29px}} header>div,main{{width:calc(100% - 24px)}}}}
</style></head><body>
<header><div><h1>28% Corporate Rate: Current vs Legacy Incidence</h1><p class="sub">A custom reallocation experiment on the completed <code>{SCENARIO}</code> run. The policy score, household records, rankings, behavioral responses, and all noncorporate taxes are held fixed.</p></div></header>
<main>
<div class="callout"><strong>Result:</strong> The prior capital-income method is more top-heavy in 2027 and assigns a slightly smaller amount to US households because its labor share begins at zero. By 2036 its household total is close to the current method, but its distribution remains more concentrated at the top.</div>
<h2>Experiment Design</h2>
<p><strong class="current">Current:</strong> 37.5% normal returns; the normal portion is split 50/50 between labor and broad capital; supernormal returns use corporate-equity stocks; capital legs receive a 40% foreign haircut; no phase-in.</p>
<p><strong class="legacy">Legacy:</strong> capital income is the capital allocation base; the labor share of a corporate rate change phases from 0% to 20% over ten years; the capital portion receives the same 40% foreign haircut. The exact formula is recovered from the parent of git commit <code>8cedc523a</code>.</p>
<div class="callout method"><strong>ETR extension:</strong> The legacy code predated <code>distribution_etrs.csv</code> and specified only reform deltas. For this experiment, baseline corporate receipts use the legacy long-run 20% labor / 80% capital split. The reform delta uses the historical phase-in: 0% labor in 2027 and 18% in 2036. Corporate tax is grossed up in both the numerator and denominator exactly as in the current ETR file.</div>
<p>Run source: <code>{RUN_ROOT}</code>. Output years are the run's distribution years, 2027 and 2036. Overlapping top groups are intentionally not additive.</p>
<h2>Standard Distribution Table</h2>
<p>The average change includes all taxes in the broad <code>iit_pr_death_cit_vat_wealth</code> tier. The CIT columns isolate the corporate allocation. VAT is zero in this corporate-only scenario.</p>
{''.join(sections)}
<h2>Effective Tax Rates</h2>
<p>Expanded-income ETRs with fixed expanded-income ranks. Current results use <code>equity_supernormal</code>; legacy results replace only corporate incidence. Static means tax asked; conventional means tax collected after modeled responses.</p>
{''.join(etr_sections)}
<h2>Interpretation</h2>
<ul><li>The impact-year difference is driven by both the allocation base and timing: current incidence includes a labor/general-capital normal-return leg immediately, while legacy incidence puts the 2027 delta entirely on capital income.</li>
<li>The household totals differ because foreign-borne capital is excluded. Current assumptions allocate 67.5% of the national rate-change score; legacy assumptions allocate 60% in 2027 and 67.2% in 2036.</li>
<li>This is an incidence reallocation, not a new behavioral simulation. It does not rerun the corporate markdown, income-flow, capital-gains, or individual-income-tax channels under a different structural corporate model.</li></ul>
<footer>Generated by <code>other/top_tax/build_corporate_incidence_comparison.py</code> from <code>distribution.csv</code> and <code>distribution_etrs.csv</code>. No production outputs were modified.</footer>
</main></body></html>"""

OUT.write_text(document, encoding="utf-8")
print(OUT)

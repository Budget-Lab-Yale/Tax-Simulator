#!/usr/bin/env python3
"""Build the self-contained house-style Table 1: the revenue implied by taking
every dollar of income above a threshold, holding tax below it at current law.

Reads hundred_pct_bracket.csv, written by hundred_pct_bracket.R, which measures
each case with two calculator passes rather than a marginal-rate blend. Emits
both threshold conventions -- round dollar figures and each concept's own top
shares -- so the report can pick one.
"""
import csv, os

HERE = os.path.dirname(__file__)
ROWS = list(csv.DictReader(open(os.path.join(HERE, "hundred_pct_bracket.csv"))))
OUT = os.path.join(HERE, "hundred_pct_table.html")

GDP_B = float(ROWS[0]["gdp_B"])
DEFICIT_B = float(ROWS[0]["deficit_B"])
YEAR = ROWS[0]["year"]

CONCEPTS = [
    ("taxable", "Taxable income", "AGI less deductions: the base the rate schedule applies to"),
    ("ordinary", "Ordinary income", "AGI less capital gains and qualified dividends"),
    ("agi", "Adjusted gross income", "the broadest measure the current system counts"),
    ("accrual", "Accrual income", "wages and business income plus the year's asset appreciation, realized or not"),
]


def money(v):
    return f"${v/1e6:.0f}M" if v >= 1e6 else f"${v/1e3:.0f}k"


def share(v):
    v *= 100
    if v >= 1:
        return f"{v:.1f}%"
    if v >= 0.1:
        return f"{v:.2f}%"
    return f"{v:.3f}%"


def body(kind):
    out = []
    for key, name, gloss in CONCEPTS:
        rows = [r for r in ROWS
                if r["concept"] == key
                and (r["threshold_type"] == "dollar" if kind == "dollar"
                     else r["threshold_type"].startswith("top_"))]
        if not rows:
            continue
        out.append(f'<tr class="grp"><th colspan="6" scope="colgroup">{name}'
                   f'<span class="gloss">{gloss}</span></th></tr>')
        for r in rows:
            thr = float(r["threshold"])
            out.append(
                "<tr>"
                f'<th scope="row">{money(thr)}</th>'
                f'<td class="dim">{share(float(r["share_above"]))}</td>'
                f'<td>{float(r["taken_B"]):,.0f}</td>'
                f'<td class="dim">{float(r["tax_paid_now_B"]):,.0f}</td>'
                f'<td class="strong">{float(r["revenue_B"]):,.0f}</td>'
                f'<td class="strong">{float(r["revenue_pct_gdp"]):.1f}%</td>'
                "</tr>")
    return "\n".join(out)


HEAD = """  <thead>
    <tr>
      <th scope="col" style="text-align:left">Threshold</th>
      <th scope="col">Share of<br>filers above</th>
      <th scope="col">Income above<br>threshold ($B)</th>
      <th scope="col">Tax already<br>paid on it ($B)</th>
      <th scope="col">Revenue<br>raised ($B)</th>
      <th scope="col">Revenue,<br>% of GDP</th>
    </tr>
  </thead>"""

HTML = f"""<title>What taking all income above a threshold would raise</title>
<style>
  :root {{
    --paper:#f9fafb; --ink:#1c2733; --ink2:#3c4a59; --muted:#5a6b7d; --line:#dce3ea; --grid:#eef2f6;
    --card:#ffffff; --accent:#1a5b9e; --head:#eef2f6;
  }}
  @media (prefers-color-scheme: dark) {{
    :root {{ --paper:#131a22; --ink:#e4ebf2; --ink2:#c3ccd6; --muted:#8da0b3; --line:#2b3947; --grid:#1e2937;
      --card:#18212b; --accent:#7db0e0; --head:#1e2937; }}
  }}
  :root[data-theme="dark"] {{ --paper:#131a22; --ink:#e4ebf2; --ink2:#c3ccd6; --muted:#8da0b3; --line:#2b3947;
    --grid:#1e2937; --card:#18212b; --accent:#7db0e0; --head:#1e2937; }}
  :root[data-theme="light"] {{ --paper:#f9fafb; --ink:#1c2733; --ink2:#3c4a59; --muted:#5a6b7d; --line:#dce3ea;
    --grid:#eef2f6; --card:#ffffff; --accent:#1a5b9e; --head:#eef2f6; }}

  * {{ box-sizing:border-box; }}
  html {{ background:var(--paper); }}
  body {{ margin:0; background:var(--paper); color:var(--ink);
    font-family:Charter,"Bitstream Charter",Cambria,Georgia,serif; font-size:16px; line-height:1.55; }}
  .wrap {{ max-width:54rem; margin:0 auto; padding:2rem 1.25rem 4rem; }}
  header {{ border-bottom:2px solid var(--ink); padding-bottom:.9rem; margin-bottom:1.3rem; }}
  .eyebrow {{ font-family:system-ui,sans-serif; font-size:.7rem; font-weight:600; letter-spacing:.13em;
    text-transform:uppercase; color:var(--accent); margin:0 0 .5rem; }}
  h1 {{ font-size:1.5rem; margin:0 0 .35rem; line-height:1.22; text-wrap:balance; }}
  .standfirst {{ color:var(--muted); font-style:italic; margin:0; max-width:46rem; }}
  .opt {{ font-family:system-ui,sans-serif; font-size:.68rem; font-weight:700; letter-spacing:.1em;
    text-transform:uppercase; color:var(--accent); margin:1.7rem 0 .45rem; }}
  .card {{ background:var(--card); border:1px solid var(--line); border-radius:9px;
    padding:.4rem .4rem 0; margin:0 0 1.3rem; overflow-x:auto; }}
  table {{ border-collapse:collapse; width:100%; font-family:system-ui,"Segoe UI",sans-serif; }}
  th, td {{ padding:.55rem .8rem; text-align:right; font-variant-numeric:tabular-nums; white-space:nowrap; }}
  thead th {{ font-size:.72rem; font-weight:700; color:var(--ink2); line-height:1.25; vertical-align:bottom;
    border-bottom:1.5px solid var(--line); }}
  tbody tr.grp th {{ text-align:left; font-size:.78rem; letter-spacing:.04em; text-transform:uppercase;
    color:var(--accent); background:var(--head); border-top:1px solid var(--line);
    border-bottom:1px solid var(--line); padding:.45rem .8rem; white-space:normal; }}
  tbody tr.grp .gloss {{ display:block; font-size:.68rem; letter-spacing:0; text-transform:none;
    font-weight:500; color:var(--muted); }}
  tbody th[scope="row"] {{ text-align:left; font-weight:700; color:var(--ink); font-size:.9rem; }}
  tbody td {{ font-size:.92rem; }}
  tbody td.dim {{ color:var(--muted); font-weight:500; }}
  tbody td.strong {{ font-weight:700; }}
  .note {{ font-family:system-ui,sans-serif; font-size:.72rem; color:var(--muted); line-height:1.55;
    margin:1rem 0 0; }}
  .note b {{ color:var(--ink); }}
</style>

<div class="wrap">
<header>
  <p class="eyebrow">The Budget Lab · static illustration · FY{YEAR}</p>
  <h1>What would taking all income above a threshold raise?</h1>
  <p class="standfirst">For each threshold, the government takes every dollar of income above it and
  leaves tax on income below it exactly as current law sets it. Nobody below the line is touched.
  Bases are held fixed, so this is a ceiling on revenue capacity rather than a revenue estimate.
  The FY{YEAR} deficit is ${DEFICIT_B/1000:.1f}&nbsp;trillion, {100*DEFICIT_B/GDP_B:.1f}% of GDP.</p>
</header>

<p class="opt">Table 1 · round thresholds</p>
<div class="card">
<table>
{HEAD}
  <tbody>
{body('dollar')}
  </tbody>
</table>
</div>

<p class="opt">The same exercise at each measure's own top shares</p>
<div class="card">
<table>
{HEAD}
  <tbody>
{body('top')}
  </tbody>
</table>
</div>

<p class="note">
  <b>How to read it.</b> The revenue raised is the income above the threshold less the tax already
  collected on that income today, so it is the increment over current law rather than the gross amount
  the government ends up holding. Reading down within a measure, the higher the threshold the less there
  is to take. Reading across measures, ordinary income raises the least because top income is
  disproportionately capital gains, and accrual income raises the most because most of the appreciation
  it counts is untaxed in the year it accrues. Whether that appreciation is untaxed or merely taxed later
  is the question the accrual rows leave open: current law reaches it at sale, or not at all if it is
  stepped up at death.
</p>
<p class="note">
  <b>Method.</b> Each case is measured with two passes of the tax calculator. Every above-threshold
  record's positive income flows are scaled down until that record's income sits on the threshold, and
  the record is repriced under current law; the tax already paid on the slice is the difference between
  the two passes. No marginal rate is assumed anywhere. Federal individual income tax plus payroll tax,
  FY{YEAR}, bases held fixed. Source: The Budget Lab Tax-Simulator, {YEAR} baseline.
</p>
</div>
"""

open(OUT, "w").write(HTML)
print("wrote", OUT, os.path.getsize(OUT), "bytes")

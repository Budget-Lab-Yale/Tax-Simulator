#!/usr/bin/env python3
"""Build a standalone ETR component visualization from distribution_etrs.csv."""

from __future__ import annotations

import csv
import html
from pathlib import Path


ROOT = Path("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator")
SOURCE = Path(
    "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/"
    "dist_etrs_other_v1/corp_perm/static/supplemental/distribution_etrs.csv"
)
OUT_TIER_DIFF = ROOT / "other/distribution_etrs/tax_type_etr_contributions.html"
OUT_COMMON_DENOM = ROOT / "other/distribution_etrs/tax_type_etr_common_denominator.html"
OUT_AGI_VS_ACCRUAL = ROOT / "other/distribution_etrs/tax_type_etr_agi_vs_accrual.html"
OUT_CASH_VS_ACCRUAL = ROOT / "other/distribution_etrs/tax_type_etr_cash_vs_accrual.html"

GROUPS = [
    "Quintile 1",
    "Quintile 2",
    "Quintile 3",
    "Quintile 4",
    "Quintile 5",
    "Top 10%",
    "Top 5%",
    "Top 1%",
    "Top 0.1%",
    "Top 0.01%",
]
NET_WORTH_GROUPS = [
    "Negative net worth",
    "Quintile 1",
    "Quintile 2",
    "Quintile 3",
    "Quintile 4",
    "Quintile 5",
    "Top 10%",
    "Top 5%",
    "Top 1%",
    "Top 0.1%",
    "Top 0.01%",
    "Billionaires",
]
TOP_GROUPS = {"Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"}

COMPONENTS = [
    ("income", "Income tax", "#356a9a"),
    ("payroll", "Payroll", "#2a9d8f"),
    ("death", "Estate / deemed", "#9b5de5"),
    ("corp", "Corporate / wealth / VAT", "#d97706"),
]

# Cash-vs-accrual chart only: append the standalone state+local+federal-excise
# `other` tier (dina_other_taxes_rate) as a fifth, regressive tax type. Kept out
# of the global COMPONENTS so the other three charts are unaffected.
CASH_ACCRUAL_COMPONENTS = COMPONENTS + [
    ("other", "State & local + excise", "#c1121f"),
]


def fmt_pp(value: float) -> str:
    return f"{value:+.2f} pp"


def pct(value: float) -> str:
    return f"{value:.2f}%"


def read_rows() -> list[dict[str, str]]:
    with SOURCE.open(newline="") as f:
        return list(csv.DictReader(f))


def indexed_slice(rows: list[dict[str, str]]) -> dict[tuple[str, str], dict[str, str]]:
    out: dict[tuple[str, str], dict[str, str]] = {}
    for row in rows:
        if row["income_definition"] != "expanded":
            continue
        if row["ranking"] != "fixed":
            continue
        if row["group_dimension"] != "Income percentile":
            continue
        if row["group"] not in GROUPS:
            continue
        if row["taxes_included"] == "wealth_cit_vat":
            if row["corp_convention"] != "equity_supernormal":
                continue
        elif row["corp_convention"] != "n/a":
            continue
        out[(row["group"], row["taxes_included"])] = row
    return out


def component_rows(
    rows: list[dict[str, str]],
    *,
    common_denominator: bool,
) -> list[dict[str, object]]:
    idx = indexed_slice(rows)
    out: list[dict[str, object]] = []
    for group in GROUPS:
        tiers = {
            tier: idx[(group, tier)]
            for tier in ("iit", "iit_pr", "death", "wealth_cit_vat")
        }
        for leg in ("baseline", "reform"):
            wealth_row = tiers["wealth_cit_vat"]
            component_col = f"etr_income_tax_{leg}"
            if common_denominator and component_col in wealth_row:
                comps = {
                    "income": float(wealth_row[f"etr_income_tax_{leg}"]) * 100,
                    "payroll": float(wealth_row[f"etr_payroll_{leg}"]) * 100,
                    "death": (
                        float(wealth_row[f"etr_estate_{leg}"])
                        + float(wealth_row[f"etr_deemed_{leg}"])
                    ) * 100,
                    "corp": (
                        float(wealth_row[f"etr_wealth_{leg}"])
                        + float(wealth_row[f"etr_corp_{leg}"])
                        + float(wealth_row[f"etr_vat_{leg}"])
                    ) * 100,
                }
                total = float(wealth_row[f"etr_{leg}"]) * 100
            elif common_denominator:
                denom = float(tiers["wealth_cit_vat"][f"income_{leg}"])
                tax = {
                    tier: float(row[f"tax_{leg}"])
                    for tier, row in tiers.items()
                }
                comps = {
                    "income": tax["iit"] / denom * 100,
                    "payroll": (tax["iit_pr"] - tax["iit"]) / denom * 100,
                    "death": (tax["death"] - tax["iit_pr"]) / denom * 100,
                    "corp": (tax["wealth_cit_vat"] - tax["death"]) / denom * 100,
                }
                total = tax["wealth_cit_vat"] / denom * 100
            else:
                etr = {
                    tier: float(row[f"etr_{leg}"]) * 100
                    for tier, row in tiers.items()
                }
                comps = {
                    "income": etr["iit"],
                    "payroll": etr["iit_pr"] - etr["iit"],
                    "death": etr["death"] - etr["iit_pr"],
                    "corp": etr["wealth_cit_vat"] - etr["death"],
                }
                total = etr["wealth_cit_vat"]
            out.append(
                {
                    "group": group,
                    "section": "top" if group in TOP_GROUPS else "quintile",
                    "leg": leg,
                    "total": total,
                    "components": comps,
                }
            )
    return out


def agi_vs_accrual_rows(rows: list[dict[str, str]]) -> list[dict[str, object]]:
    idx: dict[tuple[str, str], dict[str, str]] = {}
    for row in rows:
        if row["income_definition"] not in ("agi", "hs"):
            continue
        if row["ranking"] != "fixed":
            continue
        if row["group_dimension"] != "Income percentile":
            continue
        if row["group"] not in GROUPS:
            continue
        if row["taxes_included"] != "wealth_cit_vat":
            continue
        if row["corp_convention"] != "equity_supernormal":
            continue
        idx[(row["group"], row["income_definition"])] = row

    out: list[dict[str, object]] = []
    labels = {"agi": "AGI", "hs": "Accrual"}
    for group in GROUPS:
        for defn in ("agi", "hs"):
            row = idx[(group, defn)]
            comps = {
                "income": float(row["etr_income_tax_baseline"]) * 100,
                "payroll": float(row["etr_payroll_baseline"]) * 100,
                "death": (
                    float(row["etr_estate_baseline"])
                    + float(row["etr_deemed_baseline"])
                ) * 100,
                "corp": (
                    float(row["etr_wealth_baseline"])
                    + float(row["etr_corp_baseline"])
                    + float(row["etr_vat_baseline"])
                ) * 100,
            }
            out.append(
                {
                    "group": group,
                    "section": "top" if group in TOP_GROUPS else "quintile",
                    "leg": labels[defn],
                    "total": float(row["etr_baseline"]) * 100,
                    "components": comps,
                }
            )
    return out


def component_values(row: dict[str, str], leg: str = "baseline") -> dict[str, float]:
    return {
        "income": float(row[f"etr_income_tax_{leg}"]) * 100,
        "payroll": float(row[f"etr_payroll_{leg}"]) * 100,
        "death": (
            float(row[f"etr_estate_{leg}"])
            + float(row[f"etr_deemed_{leg}"])
        ) * 100,
        "corp": (
            float(row[f"etr_wealth_{leg}"])
            + float(row[f"etr_corp_{leg}"])
            + float(row[f"etr_vat_{leg}"])
        ) * 100,
    }


def cash_vs_accrual_panel_rows(
    rows: list[dict[str, str]],
    *,
    group_dimension: str,
    ranking: str,
    groups: list[str],
) -> list[dict[str, object]]:
    idx: dict[tuple[str, str], dict[str, str]] = {}
    idx_other: dict[tuple[str, str], dict[str, str]] = {}
    for row in rows:
        if row["income_definition"] not in ("expanded", "hs"):
            continue
        if row["ranking"] != ranking:
            continue
        if row["group_dimension"] != group_dimension:
            continue
        if row["group"] not in groups:
            continue
        key = (row["group"], row["income_definition"])
        if (
            row["taxes_included"] == "wealth_cit_vat"
            and row["corp_convention"] == "equity_supernormal"
        ):
            idx[key] = row
        elif row["taxes_included"] == "other":
            # standalone state+local+excise tier (corp_convention = n/a)
            idx_other[key] = row

    # `other` tax DOLLARS put over the wealth_cit_vat denominator so all five
    # components share one denominator and sum to an all-in ETR. Zero if the
    # source predates the imputation (older Tax-Data vintage -> no `other` tier).
    def other_pp(fed_row: dict[str, str], defn: str, group: str) -> float:
        o = idx_other.get((group, defn))
        denom = float(fed_row["income_baseline"])
        if o is None or denom == 0:
            return 0.0
        return float(o["tax_baseline"]) / denom * 100

    out: list[dict[str, object]] = []
    for group in groups:
        cash = idx[(group, "expanded")]
        accrual = idx[(group, "hs")]
        cash_comps = component_values(cash)
        cash_comps["other"] = other_pp(cash, "expanded", group)
        accrual_comps = component_values(accrual)
        accrual_comps["other"] = other_pp(accrual, "hs", group)
        cash_total = float(cash["etr_baseline"]) * 100 + cash_comps["other"]
        accrual_total = float(accrual["etr_baseline"]) * 100 + accrual_comps["other"]
        out.append(
            {
                "group": group,
                "cash": {
                    "total": cash_total,
                    "components": cash_comps,
                },
                "accrual": {
                    "total": accrual_total,
                    "components": accrual_comps,
                },
                "pp_gap": cash_total - accrual_total,
                "ratio": cash_total / accrual_total if accrual_total else None,
            }
        )
    return out


def stacked_bar(row: dict[str, object], scale: float) -> str:
    comps = row["components"]
    assert isinstance(comps, dict)
    neg_width = sum(-v for v in comps.values() if v < 0) / scale * 100
    pos_width = sum(v for v in comps.values() if v > 0) / scale * 100

    neg_segments = []
    for key, _, color in reversed(COMPONENTS):
        value = float(comps[key])
        if value >= 0:
            continue
        neg_segments.append(
            f'<span class="seg" style="width:{(-value / scale * 100):.4f}%;'
            f'background:{color}" title="{html.escape(key)} {fmt_pp(value)}"></span>'
        )

    pos_segments = []
    for key, _, color in COMPONENTS:
        value = float(comps[key])
        if value <= 0:
            continue
        pos_segments.append(
            f'<span class="seg" style="width:{(value / scale * 100):.4f}%;'
            f'background:{color}" title="{html.escape(key)} {fmt_pp(value)}"></span>'
        )

    return f"""
      <div class="bar" style="--neg:{neg_width:.4f}%;--pos:{pos_width:.4f}%">
        <div class="zero"></div>
        <div class="neg">{''.join(neg_segments)}</div>
        <div class="pos">{''.join(pos_segments)}</div>
      </div>
    """


def chart_section(title: str, rows: list[dict[str, object]], scale: float) -> str:
    body = []
    for group in dict.fromkeys(row["group"] for row in rows):
        group_rows = [row for row in rows if row["group"] == group]
        body.append(f'<div class="group-label">{html.escape(str(group))}</div>')
        body.append('<div class="group-bars">')
        for row in group_rows:
            leg = str(row["leg"])
            leg_label = {"baseline": "Base", "reform": "Policy"}.get(leg, leg)
            body.append(
                f"""
                <div class="leg">{html.escape(leg_label)}</div>
                {stacked_bar(row, scale)}
                <div class="total">{pct(float(row["total"]))}</div>
                """
            )
        body.append("</div>")
    return f"""
      <section>
        <h2>{html.escape(title)}</h2>
        <div class="axis"><span>-8 pp</span><span>0</span><span>{scale:.0f} pp</span></div>
        <div class="chart">
          {''.join(body)}
        </div>
      </section>
    """


def table(rows: list[dict[str, object]]) -> str:
    trs = []
    for row in rows:
        comps = row["components"]
        assert isinstance(comps, dict)
        trs.append(
            "<tr>"
            f"<td>{html.escape(str(row['group']))}</td>"
            f"<td>{html.escape({'baseline': 'Baseline', 'reform': 'Policy'}.get(str(row['leg']), str(row['leg'])))}</td>"
            + "".join(f"<td>{fmt_pp(float(comps[key]))}</td>" for key, _, _ in COMPONENTS)
            + f"<td>{pct(float(row['total']))}</td>"
            "</tr>"
        )
    return f"""
      <details>
        <summary>Component values</summary>
        <div class="table-wrap">
          <table>
            <thead>
              <tr>
                <th>Group</th><th>Leg</th>
                {''.join(f'<th>{html.escape(label)}</th>' for _, label, _ in COMPONENTS)}
                <th>Total ETR</th>
              </tr>
            </thead>
            <tbody>{''.join(trs)}</tbody>
          </table>
        </div>
      </details>
    """


def render(
    rows: list[dict[str, object]],
    *,
    title: str,
    description: str,
    note: str,
) -> str:
    max_pos = max(
        sum(v for v in row["components"].values() if v > 0)  # type: ignore[union-attr]
        for row in rows
    )
    scale = max(30.0, round(max_pos + 1))
    quintiles = [row for row in rows if row["section"] == "quintile"]
    top = [row for row in rows if row["section"] == "top"]
    legend = "".join(
        f'<span><i style="background:{color}"></i>{html.escape(label)}</span>'
        for _, label, color in COMPONENTS
    )
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{html.escape(title)}</title>
<style>
:root {{
  color-scheme: light;
  --bg: #f7f4ef;
  --panel: #ffffff;
  --ink: #1f2933;
  --muted: #64748b;
  --rule: #d7dce2;
  --zero: #1f2933;
}}
body {{
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  line-height: 1.45;
}}
.wrap {{
  max-width: 1120px;
  margin: 0 auto;
  padding: 34px 24px 44px;
}}
h1 {{
  font-size: 28px;
  margin: 0 0 8px;
  letter-spacing: 0;
}}
h2 {{
  margin: 30px 0 10px;
  font-size: 19px;
}}
p {{
  max-width: 860px;
  margin: 0 0 16px;
  color: var(--muted);
}}
code {{
  background: #eceff3;
  border-radius: 4px;
  padding: 1px 4px;
}}
.legend {{
  display: flex;
  flex-wrap: wrap;
  gap: 10px 18px;
  margin: 18px 0 8px;
  color: var(--muted);
  font-size: 13px;
}}
.legend span {{
  display: inline-flex;
  align-items: center;
  gap: 7px;
}}
.legend i {{
  width: 13px;
  height: 13px;
  display: inline-block;
  border-radius: 3px;
}}
section {{
  margin-top: 14px;
}}
.axis {{
  display: grid;
  grid-template-columns: 80px 74px 1fr 64px;
  color: var(--muted);
  font-size: 12px;
  margin: 0 0 4px;
}}
.axis span:nth-child(1) {{ grid-column: 2; text-align: left; }}
.axis span:nth-child(2) {{ grid-column: 3; margin-left: 21%; }}
.axis span:nth-child(3) {{ grid-column: 4; text-align: right; }}
.chart {{
  background: var(--panel);
  border: 1px solid var(--rule);
  border-radius: 8px;
  padding: 16px;
}}
.group-label {{
  font-weight: 700;
  margin-top: 14px;
}}
.group-label:first-child {{
  margin-top: 0;
}}
.group-bars {{
  display: grid;
  grid-template-columns: 62px minmax(260px, 1fr) 62px;
  gap: 7px 12px;
  align-items: center;
  margin-top: 6px;
}}
.leg, .total {{
  color: var(--muted);
  font-size: 12px;
}}
.total {{
  text-align: right;
  font-variant-numeric: tabular-nums;
}}
.bar {{
  position: relative;
  height: 18px;
  background:
    linear-gradient(to right, transparent 0 21%, rgba(31, 41, 51, .14) 21% calc(21% + 1px), transparent calc(21% + 1px)),
    #f1f3f5;
  border-radius: 4px;
  overflow: hidden;
}}
.zero {{
  position: absolute;
  left: 21%;
  top: 0;
  bottom: 0;
  width: 2px;
  background: #000000;
  z-index: 3;
}}
.neg, .pos {{
  position: absolute;
  top: 0;
  bottom: 0;
  display: flex;
}}
.neg {{
  right: 79%;
  width: var(--neg);
  justify-content: flex-end;
}}
.pos {{
  left: 21%;
  width: var(--pos);
}}
.seg {{
  display: block;
  height: 100%;
}}
.note {{
  background: #fff7ed;
  border: 1px solid #fed7aa;
  border-radius: 8px;
  padding: 12px 14px;
  margin: 18px 0 8px;
  color: #7c2d12;
  max-width: 900px;
}}
details {{
  margin-top: 24px;
  background: var(--panel);
  border: 1px solid var(--rule);
  border-radius: 8px;
  padding: 12px 14px;
}}
summary {{
  cursor: pointer;
  font-weight: 700;
}}
.table-wrap {{
  overflow-x: auto;
  margin-top: 12px;
}}
table {{
  border-collapse: collapse;
  width: 100%;
  min-width: 760px;
  font-size: 13px;
}}
th, td {{
  border-bottom: 1px solid var(--rule);
  padding: 7px 8px;
  text-align: right;
  font-variant-numeric: tabular-nums;
}}
th:first-child, td:first-child, th:nth-child(2), td:nth-child(2) {{
  text-align: left;
}}
@media (max-width: 720px) {{
  .wrap {{ padding: 24px 14px 34px; }}
  .group-bars {{ grid-template-columns: 52px minmax(180px, 1fr) 56px; gap-column: 8px; }}
  .axis {{ grid-template-columns: 70px 58px 1fr 52px; }}
}}
</style>
</head>
<body>
<main class="wrap">
  <h1>{html.escape(title)}</h1>
  <p>{description}</p>
  <div class="legend">{legend}</div>
  <div class="note">{note}</div>
  {chart_section("Mutually Exclusive Quintiles", quintiles, scale)}
  {chart_section("Top-Tail Breakouts", top, scale)}
  {table(rows)}
  <p style="margin-top:18px">
    Source:
    <code>{html.escape(str(SOURCE))}</code>
  </p>
</main>
</body>
</html>
"""


def compare_bar(cell: dict[str, object], scale: float) -> str:
    comps = cell["components"]
    assert isinstance(comps, dict)
    total = float(cell["total"])
    neg_width = sum(-v for v in comps.values() if v < 0) / scale * 100
    pos_width = sum(v for v in comps.values() if v > 0) / scale * 100
    neg_segments = []
    pos_segments = []
    for key, _, color in reversed(CASH_ACCRUAL_COMPONENTS):
        value = float(comps[key])
        if value < 0:
            neg_segments.append(
                f'<span class="seg" style="width:{(-value / scale * 100):.4f}%;'
                f'background:{color}" title="{html.escape(key)} {fmt_pp(value)}"></span>'
            )
    for key, _, color in CASH_ACCRUAL_COMPONENTS:
        value = float(comps[key])
        if value > 0:
            pos_segments.append(
                f'<span class="seg" style="width:{(value / scale * 100):.4f}%;'
                f'background:{color}" title="{html.escape(key)} {fmt_pp(value)}"></span>'
            )
    return f"""
      <div class="compare-cell">
        <div class="compare-bar" style="--neg:{neg_width:.4f}%;--pos:{pos_width:.4f}%">
          <div class="zero"></div>
          <div class="neg">{''.join(neg_segments)}</div>
          <div class="pos">{''.join(pos_segments)}</div>
        </div>
        <div class="compare-total">{pct(total)}</div>
      </div>
    """


def compare_panel(title: str, rows: list[dict[str, object]], scale: float) -> str:
    body = []
    for row in rows:
        ratio = row["ratio"]
        ratio_text = "n/a" if ratio is None else f"{float(ratio):.2f}x"
        body.append(
            f"""
            <div class="compare-group">{html.escape(str(row["group"]))}</div>
            {compare_bar(row["cash"], scale)}
            {compare_bar(row["accrual"], scale)}
            <div class="metric">{fmt_pp(float(row["pp_gap"]))}</div>
            <div class="metric">{ratio_text}</div>
            """
        )
    return f"""
      <section>
        <h2>{html.escape(title)}</h2>
        <div class="compare-head">
          <div>Group</div><div>Cash income</div><div>Accrual</div><div>Gap</div><div>Ratio</div>
        </div>
        <div class="compare-grid">{''.join(body)}</div>
      </section>
    """


def render_cash_vs_accrual(
    income_rows: list[dict[str, object]],
    wealth_rows: list[dict[str, object]],
) -> str:
    all_rows = income_rows + wealth_rows
    max_pos = max(
        max(
            sum(v for v in row[side]["components"].values() if v > 0)  # type: ignore[index,union-attr]
            for side in ("cash", "accrual")
        )
        for row in all_rows
    )
    scale = max(30.0, round(max_pos + 1))
    legend = "".join(
        f'<span><i style="background:{color}"></i>{html.escape(label)}</span>'
        for _, label, color in CASH_ACCRUAL_COMPONENTS
    )
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Baseline ETR: Cash Income vs Accrual</title>
<style>
:root {{
  color-scheme: light;
  --bg: #f7f4ef;
  --panel: #ffffff;
  --ink: #1f2933;
  --muted: #64748b;
  --rule: #d7dce2;
}}
body {{
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  line-height: 1.45;
}}
.wrap {{
  max-width: 1260px;
  margin: 0 auto;
  padding: 34px 24px 44px;
}}
h1 {{
  font-size: 28px;
  margin: 0 0 8px;
  letter-spacing: 0;
}}
h2 {{
  margin: 30px 0 10px;
  font-size: 19px;
}}
p {{
  max-width: 940px;
  margin: 0 0 16px;
  color: var(--muted);
}}
code {{
  background: #eceff3;
  border-radius: 4px;
  padding: 1px 4px;
}}
.legend {{
  display: flex;
  flex-wrap: wrap;
  gap: 10px 18px;
  margin: 18px 0 8px;
  color: var(--muted);
  font-size: 13px;
}}
.legend span {{
  display: inline-flex;
  align-items: center;
  gap: 7px;
}}
.legend i {{
  width: 13px;
  height: 13px;
  display: inline-block;
  border-radius: 3px;
}}
.note {{
  background: #fff7ed;
  border: 1px solid #fed7aa;
  border-radius: 8px;
  padding: 12px 14px;
  margin: 18px 0 8px;
  color: #7c2d12;
  max-width: 980px;
}}
.compare-head, .compare-grid {{
  display: grid;
  grid-template-columns: 150px minmax(250px, 1fr) minmax(250px, 1fr) 72px 64px;
  gap: 8px 12px;
  align-items: center;
}}
.compare-head {{
  color: var(--muted);
  font-size: 12px;
  font-weight: 700;
  padding: 0 16px 6px;
}}
.compare-grid {{
  background: var(--panel);
  border: 1px solid var(--rule);
  border-radius: 8px;
  padding: 14px 16px;
}}
.compare-group {{
  font-weight: 700;
}}
.compare-cell {{
  display: grid;
  grid-template-columns: minmax(150px, 1fr) 58px;
  gap: 8px;
  align-items: center;
}}
.compare-total, .metric {{
  color: var(--muted);
  font-size: 12px;
  text-align: right;
  font-variant-numeric: tabular-nums;
}}
.compare-bar {{
  position: relative;
  height: 18px;
  background:
    linear-gradient(to right, transparent 0 21%, #000000 21% calc(21% + 2px), transparent calc(21% + 2px)),
    #f1f3f5;
  border-radius: 4px;
  overflow: hidden;
}}
.zero {{
  position: absolute;
  left: 21%;
  top: 0;
  bottom: 0;
  width: 2px;
  background: #000000;
  z-index: 3;
}}
.neg, .pos {{
  position: absolute;
  top: 0;
  bottom: 0;
  display: flex;
}}
.neg {{
  right: 79%;
  width: var(--neg);
  justify-content: flex-end;
}}
.pos {{
  left: 21%;
  width: var(--pos);
}}
.seg {{
  display: block;
  height: 100%;
}}
@media (max-width: 900px) {{
  .wrap {{ padding: 24px 14px 34px; }}
  .compare-head {{ display: none; }}
  .compare-grid {{
    grid-template-columns: 1fr;
  }}
  .compare-group {{
    margin-top: 12px;
  }}
  .compare-group:first-child {{
    margin-top: 0;
  }}
  .metric {{
    text-align: left;
  }}
}}
</style>
</head>
<body>
<main class="wrap">
  <h1>Baseline ETR: Cash Income vs Accrual</h1>
  <p>
    Baseline law, equity/supernormal corporate convention, 2028. Cash income is
    <code>expanded</code>; accrual is <code>hs</code>. Each bar decomposes the
    all-in ETR into tax components over that row's denominator, now including the
    imputed state &amp; local + federal-excise <code>other</code> tax type
    (DINA <code>dina_other_taxes_rate</code> &times; reconstructed broad income) &mdash;
    a steeply regressive layer that shrinks as a share of income toward the top and
    is nearly invisible under the accrual denominator at the very top of the wealth
    distribution.
  </p>
  <div class="legend">{legend}</div>
  <div class="note">
    Rows are arranged as horizontal columns: group, cash-income ETR, accrual ETR,
    cash-minus-accrual gap, and cash/accrual ratio. The black vertical line marks zero.
    The <code>other</code> component is state+local+federal-excise tax placed over the
    same denominator as the federal components (so all five sum to the all-in ETR); it
    is exogenous to federal law and identical across baseline and policy legs.
  </div>
  {compare_panel("By Cash-Income Rank", income_rows, scale)}
  {compare_panel("By Net-Worth Rank", wealth_rows, scale)}
  <p style="margin-top:18px">
    Source:
    <code>{html.escape(str(SOURCE))}</code>
  </p>
</main>
</body>
</html>
"""


def main() -> None:
    rows = read_rows()

    tier_diff = component_rows(rows, common_denominator=False)
    OUT_TIER_DIFF.write_text(
        render(
            tier_diff,
            title="ETR Contribution by Tax Type",
            description=(
                'Expanded income, inheritance-inclusive fixed income ranking, '
                'equity/supernormal corporate convention, 2028. Components are '
                'differences between cumulative ETR tiers in '
                '<code>distribution_etrs.csv</code>.'
            ),
            note=(
                'The "Corporate / wealth / VAT" component is effectively the corporate '
                'channel in this <code>corp_perm</code> smoke run because wealth tax and '
                'VAT amounts are zero. Top groups are nested breakouts of the top '
                'quintile, not additional mutually exclusive bins.'
            ),
        )
    )
    print(OUT_TIER_DIFF)

    common_denom = component_rows(rows, common_denominator=True)
    OUT_COMMON_DENOM.write_text(
        render(
            common_denom,
            title="ETR Composition by Tax Type",
            description=(
                'Expanded income, inheritance-inclusive fixed income ranking, '
                'equity/supernormal corporate convention, 2028. Each component is tax '
                'dollars from that layer divided by the same full-tier expanded-income '
                'denominator, so components sum directly to the total ETR.'
            ),
            note=(
                'This common-denominator view is the cleaner answer to what types of '
                'taxes make up the ETR. The "Corporate / wealth / VAT" component is '
                'effectively the corporate channel in this <code>corp_perm</code> smoke '
                'run because wealth tax and VAT amounts are zero.'
            ),
        )
    )
    print(OUT_COMMON_DENOM)

    agi_accrual = agi_vs_accrual_rows(rows)
    OUT_AGI_VS_ACCRUAL.write_text(
        render(
            agi_accrual,
            title="Baseline ETR Composition: AGI vs Accrual",
            description=(
                'Baseline law, inheritance-inclusive fixed income ranking, '
                'equity/supernormal corporate convention, 2028. Bars compare '
                'the same grouped tax units under AGI and Haig-Simons accrual '
                'income denominators.'
            ),
            note=(
                'Components are read directly from <code>distribution_etrs.csv</code> '
                'and use each row denominator. The black vertical line marks zero.'
            ),
        )
    )
    print(OUT_AGI_VS_ACCRUAL)

    cash_income = cash_vs_accrual_panel_rows(
        rows,
        group_dimension="Income percentile",
        ranking="fixed",
        groups=GROUPS,
    )
    cash_wealth = cash_vs_accrual_panel_rows(
        rows,
        group_dimension="Net worth",
        ranking="wealth",
        groups=NET_WORTH_GROUPS,
    )
    OUT_CASH_VS_ACCRUAL.write_text(render_cash_vs_accrual(cash_income, cash_wealth))
    print(OUT_CASH_VS_ACCRUAL)


if __name__ == "__main__":
    main()

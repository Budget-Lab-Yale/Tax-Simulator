#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# build_networth_chart.py  <shares_csv>  [out_html]
#
# Two-panel line chart of top-1% (left) and top-0.1% (right) NET-WORTH share over
# a 30-year horizon under s=1 + perfect mobility. Three lines: baseline (solid
# black), Warren (dashed teal), Alan Davis = nickel/dime (dashed amber). x-axis =
# policy year 0-30 (2026 = yr 1 ... 2055 = yr 30). Self-contained artifact body.
# -----------------------------------------------------------------------------
import csv, sys, os

CSV = sys.argv[1] if len(sys.argv) > 1 else \
    "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics/networth_shares_warren_nd_30yr.csv"
OUT = sys.argv[2] if len(sys.argv) > 2 else \
    "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics/networth_chart_artifact.html"

rows = list(csv.DictReader(open(CSV)))
# scenario -> {policy_year: value}
SER = {"baseline": {}, "warren": {}, "nickeldime": {}}
for r in rows:
    sc = r["scenario"]
    if sc not in SER:
        continue
    yr = int(float(r["year"])) - 2025          # 2026 -> 1 ... 2055 -> 30
    SER[sc][yr] = {"top1": float(r["top1_pct"]), "top0_1": float(r["top0_1_pct"])}

STYLE = {
    "baseline":   dict(label="Baseline (no tax)", color="#16242e", dash="none"),
    "warren":     dict(label="Warren",            color="#0e7c8b", dash="7 5"),
    "nickeldime": dict(label="Alan Davis (nickel/dime)", color="#c07426", dash="7 5"),
}
ORDER = ["baseline", "warren", "nickeldime"]

def panel(metric, title):
    W, H = 430, 360
    L, R, T, Bm = 56, 18, 42, 46
    xs_all = sorted({y for sc in SER.values() for y in sc})
    vals = [SER[sc][y][metric] for sc in SER for y in SER[sc]]
    vmin, vmax = min(vals), max(vals)
    pad = (vmax - vmin) * 0.18 or 1.0
    vmin -= pad; vmax += pad
    def X(yr): return L + yr / 30.0 * (W - L - R)
    def Y(v):  return T + (vmax - v) / (vmax - vmin) * (H - T - Bm)
    parts = []
    # y gridlines + labels (~5 ticks)
    import math
    raw = (vmax - vmin) / 5.0
    step = next((s for s in (0.1, 0.2, 0.5, 1, 2, 5, 10) if s >= raw), 10)
    dp = 1 if step < 1 else 0
    lo = math.floor(vmin / step) * step
    v = lo
    while v <= vmax:
        if vmin <= v <= vmax:
            parts.append(f'<line x1="{L}" y1="{Y(v):.1f}" x2="{W-R}" y2="{Y(v):.1f}" stroke="#e6ebee"/>')
            parts.append(f'<text x="{L-8}" y="{Y(v)+3:.1f}" text-anchor="end" class="ax">{v:.{dp}f}%</text>')
        v += step
    # x ticks 0,10,20,30
    for xt in (0, 10, 20, 30):
        parts.append(f'<line x1="{X(xt):.1f}" y1="{H-Bm:.1f}" x2="{X(xt):.1f}" y2="{H-Bm+4:.1f}" stroke="#9aa3ad"/>')
        parts.append(f'<text x="{X(xt):.1f}" y="{H-Bm+18:.1f}" text-anchor="middle" class="ax">{xt}</text>')
    parts.append(f'<text x="{(L+W-R)/2:.0f}" y="{H-6}" text-anchor="middle" class="axl">policy year</text>')
    # lines
    for sc in ORDER:
        st = STYLE[sc]
        pts = sorted(SER[sc].items())
        d = "M" + " L".join(f"{X(yr):.1f},{Y(v[metric]):.1f}" for yr, v in pts)
        dash = "" if st["dash"] == "none" else f' stroke-dasharray="{st["dash"]}"'
        parts.append(f'<path d="{d}" fill="none" stroke="{st["color"]}" stroke-width="2.4"{dash}/>')
        for yr, v in pts:
            parts.append(f'<circle cx="{X(yr):.1f}" cy="{Y(v[metric]):.1f}" r="3.3" fill="{st["color"]}"/>')
    return (f'<div class="panel"><div class="pt">{title}</div>'
            f'<svg viewBox="0 0 {W} {H}" role="img" aria-label="{title}">{"".join(parts)}</svg></div>')

legend = "".join(
    f'<span><i style="border-top:3px {"solid" if STYLE[sc]["dash"]=="none" else "dashed"} {STYLE[sc]["color"]}"></i>{STYLE[sc]["label"]}</span>'
    for sc in ORDER)

HEAD = """<title>Real (economic) top net-worth shares under s=1, identity M</title>
<style>
:root{--ink:#16242e;--body:#33444f;--muted:#6b7c86;--hair:#dbe3e8;
--serif:"Iowan Old Style",Palatino,Georgia,serif;--sans:system-ui,-apple-system,"Segoe UI",Roboto,Arial,sans-serif;--mono:ui-monospace,Menlo,Consolas,monospace;}
*{box-sizing:border-box}body{margin:0;background:#f5f7f8;color:var(--body);font-family:var(--sans);line-height:1.55}
.wrap{max-width:980px;margin:0 auto;padding:40px 24px 64px}
h1{font-family:var(--serif);color:var(--ink);font-size:1.7rem;font-weight:600;margin:0 0 .2em;letter-spacing:-.01em;text-wrap:balance}
.sub{color:var(--muted);font-size:.95rem;margin:0 0 18px;max-width:70ch}
.eyebrow{font-family:var(--mono);font-size:.72rem;letter-spacing:.16em;text-transform:uppercase;color:#0e7c8b;font-weight:600}
.panels{display:grid;grid-template-columns:1fr 1fr;gap:18px}
@media(max-width:680px){.panels{grid-template-columns:1fr}}
.panel{background:#fff;border:1px solid var(--hair);border-radius:14px;padding:14px 12px 8px}
.panel .pt{font-family:var(--serif);color:var(--ink);font-size:1.05rem;font-weight:600;text-align:center;margin:2px 0 4px}
svg{display:block;width:100%;height:auto}
.ax{font-family:var(--mono);font-size:10px;fill:var(--muted)}.axl{font-family:var(--mono);font-size:10.5px;fill:var(--muted)}
.legend{display:flex;flex-wrap:wrap;gap:8px 22px;justify-content:center;margin:16px 0 0;font-size:.85rem;color:var(--body)}
.legend span{display:inline-flex;align-items:center;gap:8px}.legend i{width:22px;display:inline-block}
.note{color:var(--muted);font-size:.8rem;margin-top:14px;text-align:center}
</style>"""

BODY = f"""<div class="wrap">
<div class="eyebrow">Wealth bathtub · s = 1 · identity M (full persistence, realistic)</div>
<h1>Real (economic) top net-worth shares over 30 years</h1>
<p class="sub">Share of <strong>real economic net worth</strong> (&Sigma; value.* &minus; debts) held by the top 1% (left) and top 0.1% (right) under each annual wealth tax, s = 1, identity M (within-age ranks persist). The avoidance / reporting response is <strong>excluded</strong> &mdash; this isolates the tax's genuine effect on concentration through the financing drawdown. Points at policy years 1, 10, 20, 30 (2026, 2035, 2045, 2055).</p>
<div class="panels">{panel('top1','Top 1% share')}{panel('top0_1','Top 0.1% share')}</div>
<div class="legend">{legend}</div>
<div class="note">The deconcentration <strong>builds gradually</strong>: the financing drain stays on the payers (the top, who owe the tax) and compounds, so the top shares fall progressively below the baseline trajectory &mdash; ~&minus;0.1pp at year 1 widening to ~&minus;2.3pp (top 1%) by year 30. The large <em>immediate</em> drop on reported wealth was the uncalibrated avoidance/reporting response, excluded here. Economic net worth = static net_worth &minus; D_alloc.</div>
</div>"""

open(OUT, "w").write(HEAD + "\n" + BODY)
print("wrote", OUT, f"({os.path.getsize(OUT)} bytes)")

#!/usr/bin/env python3
"""Generate eta_dial_report.html — plain-language results doc for the KG_ETA
sensitivity batch (elasticity dial). Self-contained HTML, inline SVG charts,
light+dark themes. Rebuild: python3 gen_report.py"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))
ETAS = [1.2, 2.3992, 3.6, 4.8]
ETA_KEYS = ['conv_e12', 'conv_central', 'conv_e36', 'conv_e48']
ETA_LABEL = {1.2: 'sluggish (1.2)', 2.3992: 'central (2.4)',
             3.6: 'jumpy (3.6)', 4.8: 'hair-trigger (4.8)'}
ETA_SHORT = {1.2: '1.2', 2.3992: '2.4 (central)', 3.6: '3.6', 4.8: '4.8'}

# ---- data ------------------------------------------------------------------
fits = {}
with open(os.path.join(HERE, 'eta_fit_results.csv')) as f:
    for r in csv.DictReader(f):
        fits[(r['scenario'], r['decade'])] = {
            k: (v if k in ('scenario', 'decade') else float(v))
            for k, v in r.items()}

efull = []
with open(os.path.join(HERE, 'efull_by_eta.csv')) as f:
    for r in csv.DictReader(f):
        efull.append({k: (v if k == 'vintage' else float(v))
                      for k, v in r.items()})

def conv(scen, dec):
    row = fits[(scen, dec)]
    return [row[k] for k in ETA_KEYS]

# ---- svg helpers ------------------------------------------------------------
def lin(dlo, dhi, plo, phi):
    return lambda v: plo + (v - dlo) / (dhi - dlo) * (phi - plo)

def poly(pts):
    return ' '.join(f'{x:.1f},{y:.1f}' for x, y in pts)

def fmt_b(v, signed=True):
    s = '+' if (signed and v > 0) else ('−' if v < 0 else '')
    a = abs(v)
    return f'{s}${a/1000:.1f}T' if a >= 995 else f'{s}${a:.0f}B'

SERIES = ['var(--eta1)', 'var(--eta2)', 'var(--eta3)', 'var(--eta4)']

def axis_y(sy, ticks, x0, x1, fmt=lambda v: f'{v:,.0f}'):
    out = []
    for t in ticks:
        y = sy(t)
        cls = 'baseline' if t == 0 else 'grid'
        out.append(f'<line class="{cls}" x1="{x0}" y1="{y:.1f}" '
                   f'x2="{x1}" y2="{y:.1f}"/>')
        out.append(f'<text class="tick" x="{x0-8}" y="{y+4:.1f}" '
                   f'text-anchor="end">{fmt(t)}</text>')
    return ''.join(out)

# ---- chart 1: laffer curves --------------------------------------------------
def chart_laffer():
    W, H, L, R, T, B = 820, 400, 78, 168, 26, 44
    rates = [20, 25, 40, 50]
    sx = lin(20, 50, L, W - R)
    sy = lin(-800, 1200, H - B, T)
    parts = [axis_y(sy, range(-800, 1201, 400), L, W - R)]
    for rt in rates:
        parts.append(f'<text class="tick" x="{sx(rt):.1f}" y="{H-B+20}" '
                     f'text-anchor="middle">{rt}%</text>')
    for i, eta in enumerate(ETAS):
        vals = [0.0] + [conv(s, 'd1')[i] for s in
                        ('s_cg_r25', 's_cg_r40', 's_cg_r50')]
        pts = [(sx(r), sy(v)) for r, v in zip(rates, vals)]
        parts.append(f'<polyline class="ln" points="{poly(pts)}" '
                     f'stroke="{SERIES[i]}"/>')
        for (x, y), r, v in zip(pts, rates, vals):
            if r == 20:
                continue
            parts.append(
                f'<circle class="pt" cx="{x:.1f}" cy="{y:.1f}" r="4.5" '
                f'fill="{SERIES[i]}" data-tip="Top rate {r}% · reaction '
                f'{ETA_SHORT[eta]}: {fmt_b(v)} over 2027–36"/>')
        ex, ey = pts[-1]
        parts.append(f'<text class="dlab" x="{ex+10:.1f}" y="{ey+4:.1f}" '
                     f'fill="{SERIES[i]}">{ETA_LABEL[eta]}</text>')
    parts.append(f'<text class="axlab" x="{(L+W-R)/2}" y="{H-6}" '
                 f'text-anchor="middle">top capital-gains rate</text>')
    return svg_wrap(W, H, parts,
                    'Revenue from raising the capital-gains rate, first decade '
                    '(2027–36), with behavior — one line per investor-reaction setting ($B)')

# ---- chart 2: sensitivity ranges ---------------------------------------------
RANGE_ROWS = [
    ('s_cg_r40', 'Cap-gains rate 40%'),
    ('s_cg_r50', 'Cap-gains rate 50%'),
    ('s_cg_r25', 'Cap-gains rate 25%'),
    ('pr_cg_deemed', '40% + tax gains at death'),
    ('s_deemed_deemed', 'Tax gains at death only'),
    ('s_deemed_carryover', 'Carryover basis only'),
]
def chart_ranges():
    W, L, R, T, ROWH = 820, 205, 30, 34, 52
    H = T + ROWH * len(RANGE_ROWS) + 46
    sx = lin(-700, 2100, L, W - R)
    parts = []
    for t in range(-500, 2001, 500):
        x = sx(t)
        cls = 'baseline' if t == 0 else 'grid'
        parts.append(f'<line class="{cls}" x1="{x:.1f}" y1="{T-8}" '
                     f'x2="{x:.1f}" y2="{H-38}"/>')
        parts.append(f'<text class="tick" x="{x:.1f}" y="{H-22}" '
                     f'text-anchor="middle">{fmt_b(t) if t else "$0"}</text>')
    for j, (scen, label) in enumerate(RANGE_ROWS):
        y = T + ROWH * j + ROWH / 2
        lo_eta, c, _, hi_eta = conv(scen, 'd1')
        a, b = sorted((lo_eta, hi_eta))
        parts.append(f'<text class="rowlab" x="{L-12}" y="{y+4:.1f}" '
                     f'text-anchor="end">{label}</text>')
        parts.append(f'<line class="range" x1="{sx(a):.1f}" y1="{y:.1f}" '
                     f'x2="{sx(b):.1f}" y2="{y:.1f}"/>')
        tip = (f'{label}, 2027–36: {fmt_b(lo_eta)} if sluggish (1.2) '
               f'→ {fmt_b(hi_eta)} if hair-trigger (4.8); '
               f'central {fmt_b(c)}')
        for v, col, r_ in ((lo_eta, SERIES[0], 6), (hi_eta, SERIES[3], 6),
                           (c, SERIES[1], 5)):
            parts.append(f'<circle class="pt" cx="{sx(v):.1f}" cy="{y:.1f}" '
                         f'r="{r_}" fill="{col}" data-tip="{tip}"/>')
    ly = H - 4
    for eta, col, txt in ((1.2, SERIES[0], 'sluggish (1.2)'),
                          (2.3992, SERIES[1], 'central (2.4)'),
                          (4.8, SERIES[3], 'hair-trigger (4.8)')):
        parts.append(f'<circle cx="{L + [0,150,300][ [1.2,2.3992,4.8].index(eta) ]}" '
                     f'cy="{ly-4}" r="5" fill="{col}"/>')
        parts.append(f'<text class="tick" x="{L + [0,150,300][ [1.2,2.3992,4.8].index(eta) ] + 10}" '
                     f'y="{ly}">{txt}</text>')
    return svg_wrap(W, H, parts,
                    'Same policies, three reaction settings — revenue with behavior, '
                    '2027–36 ($B)')

# ---- chart 3: package by decade ----------------------------------------------
def chart_package():
    W, H, L, R, T, B = 820, 380, 78, 30, 30, 64
    decades = [('d1', '2027–36'), ('d2', '2037–46'), ('d3', '2047–56')]
    sy = lin(0, 24000, H - B, T)
    parts = [axis_y(sy, range(0, 24001, 6000), L, W - R,
                    fmt=lambda v: f'${v/1000:.0f}T')]
    gw = (W - L - R) / 3
    bw, gap = 34, 10
    for gi, (dec, dl) in enumerate(decades):
        vals = conv('stack_ref', dec)
        x0 = L + gi * gw + (gw - 4 * bw - 3 * gap) / 2
        for i, v in enumerate(vals):
            x = x0 + i * (bw + gap)
            y = sy(v)
            parts.append(
                f'<rect class="bar" x="{x:.1f}" y="{y:.1f}" width="{bw}" '
                f'height="{sy(0)-y:.1f}" rx="4" fill="{SERIES[i]}" '
                f'data-tip="Full package, {dl} · reaction {ETA_SHORT[ETAS[i]]}: '
                f'{fmt_b(v, signed=False)} collected"/>')
        lo, c, _, hi = vals
        swing = 100 * (lo - hi) / c
        parts.append(f'<text class="dlab" x="{x0 + 2*bw + 1.5*gap:.1f}" '
                     f'y="{sy(max(vals))-12:.1f}" text-anchor="middle">'
                     f'swing {swing:.0f}%</text>')
        parts.append(f'<text class="tick" x="{x0 + 2*bw + 1.5*gap:.1f}" '
                     f'y="{H-B+20}" text-anchor="middle">{dl}</text>')
    ly = H - 14
    for i, eta in enumerate(ETAS):
        x = L + i * 170
        parts.append(f'<rect x="{x}" y="{ly-10}" width="12" height="12" rx="3" '
                     f'fill="{SERIES[i]}"/>')
        parts.append(f'<text class="tick" x="{x+18}" y="{ly}">'
                     f'{ETA_LABEL[eta]}</text>')
    return svg_wrap(W, H, parts,
                    'The full reference package: revenue collected per decade, by '
                    'reaction setting ($T)')

# ---- chart 4: blind-test errors ------------------------------------------------
def chart_loo():
    scens = [s for s, _ in RANGE_ROWS] + ['stack_ref']
    names = dict(RANGE_ROWS); names['stack_ref'] = 'Full package'
    W, L, R, T, ROWH = 820, 205, 30, 36, 46
    H = T + ROWH * len(scens) + 44
    sx = lin(-6, 6, L, W - R)
    parts = [f'<rect x="{sx(-2):.1f}" y="{T-10}" width="{sx(2)-sx(-2):.1f}" '
             f'height="{H-T-24}" class="band"/>']
    for t in range(-6, 7, 2):
        x = sx(t)
        cls = 'baseline' if t == 0 else 'grid'
        parts.append(f'<line class="{cls}" x1="{x:.1f}" y1="{T-10}" '
                     f'x2="{x:.1f}" y2="{H-34}"/>')
        parts.append(f'<text class="tick" x="{x:.1f}" y="{H-18}" '
                     f'text-anchor="middle">{t:+d}%</text>')
    for j, scen in enumerate(scens):
        y = T + ROWH * j + ROWH / 2
        parts.append(f'<text class="rowlab" x="{L-12}" y="{y+4:.1f}" '
                     f'text-anchor="end">{names[scen]}</text>')
        for k, dec in enumerate(('d1', 'd2', 'd3')):
            row = fits[(scen, dec)]
            e = row['loo_err_pct_static']
            yy = y + (k - 1) * 11
            col = 'var(--pos)' if e > 0 else 'var(--neg)'
            parts.append(
                f'<line class="err" x1="{sx(0):.1f}" y1="{yy:.1f}" '
                f'x2="{sx(e):.1f}" y2="{yy:.1f}" stroke="{col}"/>')
            parts.append(
                f'<circle class="pt" cx="{sx(e):.1f}" cy="{yy:.1f}" r="4" '
                f'fill="{col}" data-tip="{names[scen]}, decade {k+1}: formula '
                f'missed the real central run by {row["loo_err_B"]:+,.0f}$B '
                f'({e:+.1f}% of the no-behavior total)"/>')
    parts.append(f'<text class="axlab" x="{(L+W-R)/2}" y="{H-2}" '
                 f'text-anchor="middle">blind-test miss, % of the no-behavior revenue '
                 f'(shaded = ±2%; three dots per row = decades 1–3)</text>')
    return svg_wrap(W, H, parts,
                    'Blind test: predict the central run from the other three, '
                    'then compare against the real thing')

# ---- chart 5: E_full linearity --------------------------------------------------
def chart_efull():
    W, H, L, R, T, B = 820, 360, 78, 210, 26, 56
    sx = lin(0, 5.2, L, W - R)
    sy = lin(0, 5.6, H - B, T)
    slope = sum(-e['E_full'] * e['eta'] for e in efull) / \
            sum(e['eta'] ** 2 for e in efull)
    parts = [axis_y(sy, range(0, 6), L, W - R, fmt=lambda v: f'{v:.0f}')]
    for t in range(0, 6):
        parts.append(f'<text class="tick" x="{sx(t):.1f}" y="{H-B+20}" '
                     f'text-anchor="middle">{t}</text>')
    parts.append(f'<line class="fit" x1="{sx(0):.1f}" y1="{sy(0):.1f}" '
                 f'x2="{sx(5.2):.1f}" y2="{sy(5.2*slope):.1f}"/>')
    for i, e in enumerate(efull):
        x, y = sx(e['eta']), sy(-e['E_full'])
        parts.append(f'<circle class="pt" cx="{x:.1f}" cy="{y:.1f}" r="6" '
                     f'fill="{SERIES[i]}" data-tip="Dial set to {e["eta"]:g} '
                     f'→ measured response {e["E_full"]:.2f} '
                     f'(ratio {-e["E_full"]/e["eta"]:.3f})"/>')
        parts.append(f'<text class="dlab" x="{x:.1f}" y="{y-14:.1f}" '
                     f'text-anchor="middle">{-e["E_full"]:.2f}</text>')
    parts.append(f'<text class="dlab" x="{sx(5.2)+8:.1f}" '
                 f'y="{sy(5.2*slope)+4:.1f}">straight line, slope {slope:.2f}</text>')
    parts.append(f'<text class="axlab" x="{(L+W-R)/2}" y="{H-8}" '
                 f'text-anchor="middle">dial setting (eta)</text>')
    parts.append(f'<text class="axlab" transform="rotate(-90)" x="{-(H-B+T)/2}" '
                 f'y="24" text-anchor="middle">measured response in the full model</text>')
    return svg_wrap(W, H, parts,
                    'Dial setting vs. what the full model actually delivers — '
                    'a straight line means no recalibration needed')

def svg_wrap(w, h, parts, title):
    return (f'<figure><figcaption>{title}</figcaption>'
            f'<div class="scroller"><svg viewBox="0 0 {w} {h}" '
            f'width="{w}" role="img" aria-label="{title}">'
            + ''.join(parts) + '</svg></div></figure>')

# ---- appendix table --------------------------------------------------------------
def table():
    names = dict(RANGE_ROWS); names['stack_ref'] = 'Full package'
    hdr = ('<tr><th>Policy</th><th>Decade</th><th>No behavior</th>'
           + ''.join(f'<th>reaction {ETA_SHORT[e]}</th>' for e in ETAS)
           + '<th>Blind-test miss</th></tr>')
    rows = []
    for scen in list(names) :
        for k, dec in enumerate(('d1', 'd2', 'd3')):
            r = fits[(scen, dec)]
            rows.append(
                '<tr><td>' + (names[scen] if k == 0 else '') + f'</td>'
                f'<td>{k+1}</td><td>{fmt_b(r["static"])}</td>'
                + ''.join(f'<td>{fmt_b(r[key])}</td>' for key in ETA_KEYS)
                + f'<td>{r["loo_err_pct_static"]:+.1f}%</td></tr>')
    return ('<details><summary>All the numbers (revenue vs. no-reform baseline, '
            'per decade, $B/$T)</summary><div class="scroller"><table>'
            + hdr + ''.join(rows) + '</table></div></details>')

# ---- page ------------------------------------------------------------------------
d = {(s, dec): conv(s, dec) for s in
     ('s_cg_r40', 's_deemed_deemed', 'stack_ref') for dec in ('d1',)}
cg = d[('s_cg_r40', 'd1')]
pk = d[('stack_ref', 'd1')]
slope = sum(-e['E_full'] * e['eta'] for e in efull) / sum(e['eta']**2 for e in efull)
swing_pk = 100 * (pk[0] - pk[3]) / pk[1]
central_eta = ETAS[1]
peak_pct = 100 / central_eta
cg40 = fits[('s_cg_r40', 'd1')]
cg50 = fits[('s_cg_r50', 'd1')]

html = f"""<title>Capital-gains elasticity dial — what the test runs show</title>
<style>
:root {{
  --page:#f9f9f7; --surface:#fcfcfb; --ink:#0b0b0b; --ink2:#52514e;
  --muted:#898781; --grid:#e1e0d9; --axis:#c3c2b7; --accent:#00356b;
  --band:rgba(42,120,214,.08); --border:rgba(11,11,11,.10);
  --eta1:#86b6ef; --eta2:#5598e7; --eta3:#2a78d6; --eta4:#184f95;
  --pos:#2a78d6; --neg:#e34948;
}}
@media (prefers-color-scheme: dark) {{ :root {{
  --page:#0d0d0d; --surface:#1a1a19; --ink:#ffffff; --ink2:#c3c2b7;
  --muted:#898781; --grid:#2c2c2a; --axis:#383835; --accent:#9ec5f4;
  --band:rgba(57,135,229,.14); --border:rgba(255,255,255,.10);
  --eta1:#9ec5f4; --eta2:#6da7ec; --eta3:#3987e5; --eta4:#1c5cab;
  --pos:#3987e5; --neg:#e66767;
}} }}
:root[data-theme="dark"] {{
  --page:#0d0d0d; --surface:#1a1a19; --ink:#ffffff; --ink2:#c3c2b7;
  --muted:#898781; --grid:#2c2c2a; --axis:#383835; --accent:#9ec5f4;
  --band:rgba(57,135,229,.14); --border:rgba(255,255,255,.10);
  --eta1:#9ec5f4; --eta2:#6da7ec; --eta3:#3987e5; --eta4:#1c5cab;
  --pos:#3987e5; --neg:#e66767;
}}
:root[data-theme="light"] {{
  --page:#f9f9f7; --surface:#fcfcfb; --ink:#0b0b0b; --ink2:#52514e;
  --muted:#898781; --grid:#e1e0d9; --axis:#c3c2b7; --accent:#00356b;
  --band:rgba(42,120,214,.08); --border:rgba(11,11,11,.10);
  --eta1:#86b6ef; --eta2:#5598e7; --eta3:#2a78d6; --eta4:#184f95;
  --pos:#2a78d6; --neg:#e34948;
}}
body {{ background:var(--page); color:var(--ink); margin:0;
  font:16px/1.55 Georgia, 'Times New Roman', serif; }}
main {{ max-width:900px; margin:0 auto; padding:40px 22px 80px; }}
.eyebrow {{ font:600 12px/1 system-ui,-apple-system,sans-serif;
  letter-spacing:.14em; text-transform:uppercase; color:var(--accent); }}
h1 {{ font-size:34px; line-height:1.15; margin:.35em 0 .2em; text-wrap:balance; }}
h2 {{ font-size:23px; margin:2.2em 0 .4em; text-wrap:balance; }}
.sub {{ color:var(--ink2); margin:0 0 1.4em; }}
p {{ max-width:66ch; }}
.tiles {{ display:grid; grid-template-columns:repeat(auto-fit,minmax(230px,1fr));
  gap:14px; margin:26px 0 8px; }}
.tile {{ background:var(--surface); border:1px solid var(--border);
  border-radius:10px; padding:16px 18px; }}
.tile .v {{ font:700 30px/1.1 system-ui,-apple-system,sans-serif; }}
.tile .k {{ font:600 12px/1.3 system-ui,sans-serif; letter-spacing:.08em;
  text-transform:uppercase; color:var(--muted); margin-bottom:8px; }}
.tile .d {{ color:var(--ink2); font-size:14px; margin-top:6px; }}
figure {{ margin:1.4em 0 2em; background:var(--surface);
  border:1px solid var(--border); border-radius:10px; padding:14px 14px 6px; }}
figcaption {{ font:600 13.5px/1.4 system-ui,sans-serif; color:var(--ink2);
  margin:2px 4px 10px; }}
.scroller {{ overflow-x:auto; }}
svg {{ display:block; max-width:100%; height:auto; }}
svg text {{ font:12px system-ui,-apple-system,sans-serif; fill:var(--ink2);
  font-variant-numeric:tabular-nums; }}
.tick {{ fill:var(--muted); }}
.rowlab {{ font-size:13px; fill:var(--ink); }}
.dlab {{ font-weight:600; font-size:12.5px; }}
.axlab {{ fill:var(--muted); font-size:12px; }}
.grid {{ stroke:var(--grid); stroke-width:1; }}
.baseline {{ stroke:var(--axis); stroke-width:1.5; }}
.ln {{ fill:none; stroke-width:2.5; }}
.range {{ stroke:var(--axis); stroke-width:3; stroke-linecap:round; }}
.err {{ stroke-width:2.5; }}
.fit {{ stroke:var(--muted); stroke-width:1.5; stroke-dasharray:5 4; }}
.band {{ fill:var(--band); }}
.pt {{ stroke:var(--surface); stroke-width:2; cursor:default; }}
.bar {{ stroke:var(--surface); stroke-width:1; }}
#tip {{ position:fixed; pointer-events:none; background:var(--ink);
  color:var(--page); font:12.5px/1.4 system-ui,sans-serif; padding:7px 10px;
  border-radius:7px; max-width:290px; opacity:0; transition:opacity .12s; z-index:9; }}
table {{ border-collapse:collapse; font:13.5px system-ui,sans-serif;
  font-variant-numeric:tabular-nums; margin:10px 0; }}
th,td {{ text-align:right; padding:6px 12px; border-bottom:1px solid var(--grid); }}
th:first-child,td:first-child {{ text-align:left; }}
th {{ color:var(--muted); font-weight:600; }}
details {{ background:var(--surface); border:1px solid var(--border);
  border-radius:10px; padding:12px 16px; margin:1.4em 0; }}
summary {{ cursor:pointer; font:600 14px system-ui,sans-serif; color:var(--ink2); }}
.note {{ font-size:14px; color:var(--ink2); border-left:3px solid var(--accent);
  padding-left:14px; }}
</style>
<main>
<div class="eyebrow">Tax-Simulator · top-tax atlas · July 11, 2026</div>
<h1>How much does the capital-gains story depend on the elasticity?</h1>
<p class="sub">We re-ran the model’s capital-gains scenarios at three alternative
investor-reaction strengths, without touching anything else. Three findings: the
answer for a stand-alone rate hike swings enormously; the full package barely
moves; and the results follow a simple curve exactly well enough to power a live
“elasticity dial” on the atlas page.</p>

<div class="tiles">
<div class="tile"><div class="k">Stand-alone 40% rate, 2027–36</div>
<div class="v">{fmt_b(cg[0])} → {fmt_b(cg[3])}</div>
<div class="d">from sluggish to hair-trigger investors — the sign flips</div></div>
<div class="tile"><div class="k">Full package, same test</div>
<div class="v">{swing_pk:.0f}% swing</div>
<div class="d">taxing gains at death backstops the rate hike</div></div>
<div class="tile"><div class="k">Dial formula, blind test</div>
<div class="v">≈ 2%</div>
<div class="d">typical miss when predicting a run it never saw</div></div>
</div>

<h2>What we did</h2>
<p>The model’s capital-gains behavior has one headline knob: how strongly
investors change their selling in response to the tax rate (the parameter
“eta”, our calibrated central value 2.4, in line with the long-run
semi-elasticity evidence). We re-ran 7 scenarios — three rate hikes, two
death-tax regimes, one combination, and the full reference package — at eta =
1.2 (“sluggish”, half our central), 3.6 (“jumpy”, 1.5×)
and 4.8 (“hair-trigger”, double). 21 runs, 30-year horizon, full sample.
Everything else — the law, the data, every other behavioral setting — is
identical, and the “no-behavior” (static) numbers came out identical to
the fifth decimal, which confirms the knob only touches what it should.</p>

<h2>1 · A stand-alone rate hike lives or dies by this number</h2>
<p>Raise the top capital-gains rate to 40% and keep step-up at death, and the
first-decade take ranges from <b>{fmt_b(cg[0])}</b> (sluggish) to
<b>{fmt_b(cg[3])}</b> (hair-trigger). The whole shape of the revenue curve
changes: with sluggish investors, revenue keeps climbing to a 50% rate; with
hair-trigger investors the curve tips negative before 25%. The peak sits roughly
at 1/eta, exactly as the model’s math says it should.</p>
{chart_laffer()}

<h3>Why does the central curve flatten around 40%?</h3>
<p>This is a feature of the behavioral functional form, not smoothing in the
chart. For each age cell, the model makes the realization rate respond as
<i>r<sub>policy</sub> = r<sub>base</sub> exp[−eta × tax-cost increase]</i>.
The statutory rate raises tax collected on each sale, while the exponential
term shrinks the number of taxable sales. In the simple one-rate version,
revenue is proportional to <i>tau exp[−eta(tau−tau<sub>base</sub>)]</i>, whose
peak is approximately <i>1/eta</i>. At the central eta of {central_eta:.4f},
that is about {peak_pct:.1f}% — almost exactly the 40% point.</p>
<p>The raw simulations show the trade-off: without behavior, moving from 40%
to 50% adds {fmt_b(cg50['static'] - cg40['static'], signed=False)} in the first
decade. With central behavior, it adds only
{fmt_b(cg50['conv_central'] - cg40['conv_central'], signed=False)}
({fmt_b(cg40['conv_central'])} to {fmt_b(cg50['conv_central'])}). The full
model also includes heterogeneous marginal tax rates, gain stocks, mortality,
and timing, so the peak is an approximation rather than a universal exact
rate; but those mechanisms do not create the flatness. The exponential
realization response does.</p>

<h3>Is that a standard empirical form?</h3>
<p>Yes: it is the familiar <b>constant semi-elasticity</b> specification. A
regression written as <i>log(realizations) = a + b × tax rate</i> implies
<i>realizations = exp(a) exp(b × tax rate)</i>. With a negative coefficient,
that is the same exponential response used here; revenue is then roughly
<i>tax rate × exp(b × tax rate)</i>, which peaks at <i>−1/b</i>.</p>
<p>The model writes the same idea as <i>log(r<sub>S</sub>) −
log(r<sub>B</sub>) = −eta × (MC<sub>S</sub> − MC<sub>B</sub>)</i>. Its
regressor is the relevant marginal tax cost rather than simply the statutory
top rate, which lets it account for tax interactions and different taxpayers.
This is one standard choice; another common form regresses log realizations on
the log net-of-tax rate, <i>log(1−tax rate)</i>, which has different curvature
near a 100% tax rate.</p>

<h2>2 · Death-based taxes lean the other way</h2>
<p>Here is the same experiment for each policy piece. The rate-hike pieces
(top rows) collect much less when investors react more. But taxing gains at
death moves in the <b>opposite direction</b>: the harder investors defer to
dodge the annual tax, the more gains pile up to be taxed at death. Reaction
strength shifts revenue from during-life collection to at-death collection
rather than destroying it — which is why the combination row is so much
flatter than the rate-hike rows.</p>
{chart_ranges()}

<h2>3 · The full package is robust</h2>
<p>The reference package (ordinary + capital gains + corporate + wealth + estate
+ gains-at-death) collects about <b>$10.3T</b> in the first decade at our
central setting. Quadrupling the reaction strength moves that by only
{swing_pk:.0f}% end to end — the death-tax backstop and the package’s
other bases absorb most of what the rate hike loses. The elasticity is a
first-order question for a stand-alone rate hike and a second-order question
for the package.</p>
{chart_package()}

<h2>4 · The dial formula passes its blind test</h2>
<p>The model’s functional form implies revenue should follow a two-parameter
exponential curve in eta. We tested that the honest way: for every policy and
decade, fit the curve on the three <i>new</i> runs only, predict the central
run it never saw, and compare. Typical miss: 1–2% of the no-behavior
revenue; worst: 4.7% on the smallest scenario (carryover basis, a $1.9B miss on
a $40B total). The real dial will pin all four measured points exactly and only
interpolate between them, so it will be tighter than this test everywhere.</p>
{chart_loo()}

<h2>5 · And we may never need to recalibrate this knob again</h2>
<p>The dial setting is an <i>internal</i> parameter; what matters is the
elasticity the full model actually delivers, interactions and all. If the ratio
between the two drifted as the dial moved, every new setting would need its own
calibration loop. It doesn’t drift: measured response is a straight line
through the origin, slope {slope:.2f} (ratios 1.11, 1.08, 1.07, 1.08 —
constant to ≈3%). Want the model to deliver elasticity E? Set the dial to
E÷{slope:.2f}. That one division replaces the calibration loop.</p>
{chart_efull()}

<p class="note"><b>Fine print.</b> “Revenue” = change vs. current law,
summed over the decade shown, conventional scoring (with behavior); “no
behavior” = static scoring. Decade windows: 2027–36, 2037–46,
2047–56. Runs use the same 2026-start convention and baseline as the shipped
atlas vintage (top_tax_dials_30y_v1). The response measurement (chart 5) uses the
+5pp rate scenario at year 2055 on the no-wealth-channel leg; its <i>level</i> is
not comparable to the calibration’s official dilution factor (different shock
size and leg) — the finding is the straightness, not the level. Death-regime
scenarios include their revenue in the year-after-death fiscal booking.</p>

<h2>What this unlocks</h2>
<p>Next step is wiring the dial into the atlas capital-gains card: a slider
labeled in delivered-elasticity units (≈1.1 to ≈4.4), moving every
conventional number, the leakage split, and the Laffer surface client-side via
the fitted curves — the static “ask” never moves, by construction.
The three ref-package runs also kept full microdata, so the realized-ETR tile
can respond to the dial too (one post-processing pass, no new simulation).</p>

{table()}
</main>
<div id="tip" role="status"></div>
<script>
(function () {{
  var tip = document.getElementById('tip');
  document.addEventListener('mousemove', function (e) {{
    var t = e.target.closest ? e.target.closest('[data-tip]') : null;
    if (t) {{
      tip.textContent = t.getAttribute('data-tip');
      tip.style.opacity = 1;
      var x = Math.min(e.clientX + 14, window.innerWidth - 300);
      tip.style.left = x + 'px';
      tip.style.top = (e.clientY + 16) + 'px';
    }} else {{ tip.style.opacity = 0; }}
  }});
}})();
</script>
"""

out = os.path.join(HERE, 'eta_dial_report.html')
with open(out, 'w') as f:
    f.write(html)
print('wrote', out, len(html), 'bytes')

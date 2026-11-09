#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# build_bounding_report.py
#
# Renders other/wealth_dynamics/bounding_report.html from bounding_grid.json
# (receipts grid) + bounding_drain.csv (mechanical diagnostics). Self-contained
# HTML, house style borrowed from s_impact_report.html. No external assets.
# -----------------------------------------------------------------------------
import csv, json, os

HERE = "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
grid_d = json.load(open(os.path.join(HERE, "bounding_grid.json")))
B   = grid_d["baseline"]
G   = grid_d["grid"]                      # G[M][tag] -> by_type dict (or None)
S   = [("s00",0.0),("s25",0.25),("s50",0.5),("s75",0.75),("s100",1.0)]

drain = list(csv.DictReader(open(os.path.join(HERE, "bounding_drain.csv"))))
def dlast(M, s):                          # final-year (2036) drain row for (M,s)
    rs = [r for r in drain if r["M"]==M and abs(float(r["s"])-s) < 1e-9]
    if not rs: return None
    return max(rs, key=lambda r: int(r["year"]))

TT = ["income","payroll","estate","total"]
def head(M,tag):
    g = G[M][tag]
    return None if g is None else {tt: g[tt]-B[tt] for tt in TT}   # reform - baseline

# headline totals (reform - baseline) per M,s
HT = {M: {tag: (None if G[M][tag] is None else head(M,tag)) for tag,_ in S} for M in ("identity","uniform")}
# s-impact (vs s=0, identity) and M-envelope (uniform - identity)
ref0 = G["identity"]["s00"]
simp = {tag: (None if G["identity"][tag] is None else {tt: G["identity"][tag][tt]-ref0[tt] for tt in TT}) for tag,_ in S}
menv = {}
for tag,_ in S:
    gi, gu = G["identity"][tag], G["uniform"][tag]
    menv[tag] = None if (gi is None or gu is None) else {tt: gu[tt]-gi[tt] for tt in TT}

def f(x, dp=1, sign=False):
    if x is None: return "&mdash;"
    s = f"{x:+.{dp}f}" if sign else f"{x:.{dp}f}"
    return s

# ---- SVG line chart: total reform estimate vs s, identity & uniform + band ----
def svg_estimate():
    W,H = 720,330; L,R,T,Bm = 64,24,24,52
    xs = [sv for _,sv in S]
    yi = [HT["identity"][tag]["total"] for tag,_ in S]
    yu = [HT["uniform"][tag]["total"]  for tag,_ in S]
    vmin = min(min(yi),min(yu)); vmax = max(max(yi),max(yu))
    pad=(vmax-vmin)*0.18 or 1; vmin-=pad; vmax-=0; vmax+=pad
    def X(s): return L + s*(W-L-R)
    def Y(v): return T + (vmax-v)/(vmax-vmin)*(H-T-Bm)
    def path(ys): return "M"+" L".join(f"{X(xs[i]):.1f},{Y(ys[i]):.1f}" for i in range(len(xs)))
    band = ("M"+" L".join(f"{X(xs[i]):.1f},{Y(yi[i]):.1f}" for i in range(len(xs)))
            +" L"+" L".join(f"{X(xs[i]):.1f},{Y(yu[i]):.1f}" for i in range(len(xs)-1,-1,-1))+" Z")
    # y gridlines
    gl=[]
    import math
    lo=math.floor(vmin/5)*5; hi=math.ceil(vmax/5)*5
    v=lo
    while v<=hi:
        if vmin<=v<=vmax:
            gl.append(f'<line x1="{L}" y1="{Y(v):.1f}" x2="{W-R}" y2="{Y(v):.1f}" stroke="var(--hair)"/>'
                      f'<text x="{L-8}" y="{Y(v)+3:.1f}" text-anchor="end" class="axt">{v:.0f}</text>')
        v+=5
    xlab="".join(f'<text x="{X(sv):.1f}" y="{H-Bm+20}" text-anchor="middle" class="axt">{sv:g}</text>' for _,sv in S)
    di="".join(f'<circle cx="{X(xs[i]):.1f}" cy="{Y(yi[i]):.1f}" r="3.4" fill="var(--teal-deep)"/>' for i in range(len(xs)))
    du="".join(f'<circle cx="{X(xs[i]):.1f}" cy="{Y(yu[i]):.1f}" r="3.4" fill="var(--amber)"/>' for i in range(len(xs)))
    return f'''<svg viewBox="0 0 {W} {H}" role="img" aria-label="Total reform estimate vs s">
      {''.join(gl)}
      <path d="{band}" fill="var(--teal-wash)" opacity="0.7" stroke="none"/>
      <path d="{path(yi)}" fill="none" stroke="var(--teal-deep)" stroke-width="2.4"/>
      <path d="{path(yu)}" fill="none" stroke="var(--amber)" stroke-width="2.4" stroke-dasharray="5 4"/>
      {di}{du}
      <text x="{X(0)}" y="{Y(yi[0])-12:.1f}" text-anchor="start" class="axt" fill="var(--ink)">s=0: ${yi[0]:.0f}B</text>
      <text x="{(L+W-R)/2:.0f}" y="{H-8}" text-anchor="middle" class="axt">saving share  s  =  1 &minus; MPC</text>
    </svg>'''

# ---- SVG bars: s-impact total per s (cross-base cost) -------------------------
def svg_simpact():
    W,H=720,250; L,R,T,Bm=64,18,18,52
    tags=[t for t,_ in S if t!="s00"]; vals=[simp[t]["total"] for t in tags]
    vmax=0; vmin=min(vals); vmin*=1.18
    def Y(v): return T+(0-v)/(0-vmin)*(H-T-Bm)
    n=len(tags); bw=(W-L-R)/n*0.5
    bars=[];
    for i,t in enumerate(tags):
        cx=L+(i+0.5)*(W-L-R)/n; v=vals[i]; y0=Y(0); y1=Y(v)
        bars.append(f'<rect x="{cx-bw/2:.1f}" y="{min(y0,y1):.1f}" width="{bw:.1f}" height="{abs(y1-y0):.1f}" fill="var(--lost)" rx="3"/>'
                    f'<text x="{cx:.1f}" y="{y1+15:.1f}" text-anchor="middle" class="axt" fill="var(--lost)">{v:.1f}</text>'
                    f'<text x="{cx:.1f}" y="{H-Bm+20}" text-anchor="middle" class="axt">s={dict(S)[t]:g}</text>')
    zero=f'<line x1="{L}" y1="{Y(0):.1f}" x2="{W-R}" y2="{Y(0):.1f}" stroke="var(--hair-strong)"/>'
    return f'<svg viewBox="0 0 {W} {H}" role="img" aria-label="s-impact total by s">{zero}{"".join(bars)}<text x="{L}" y="{Y(0)-6:.1f}" class="axt">$0</text></svg>'

# headline range numbers
tot_s0 = HT["identity"]["s00"]["total"]
tot_s1_id = HT["identity"]["s100"]["total"]; tot_s1_un = HT["uniform"]["s100"]["total"]
tot_lo = min(tot_s1_id, tot_s1_un);
simp50 = simp["s50"]; simp100 = simp["s100"]
dr50 = dlast("identity",0.5); dr100 = dlast("identity",1.0)
cl_id = dlast("identity",1.0); cl_un = dlast("uniform",1.0)
nw50 = float(dr50["nw_total_T"]); drain50 = float(dr50["drain_B"]); drain100=float(dr100["drain_B"])

def row_cells(d, dp=1, sign=False, cls=""):
    return "".join(f'<td class="n {cls}">{f(d[tt],dp,sign) if d else "&mdash;"}</td>' for tt in TT)

# build s-impact + headline + envelope table rows
def headline_rows(M="identity"):
    out=[]
    for tag,sv in S:
        d=HT[M][tag]
        out.append(f'<tr><td class="num">{sv:g}</td>{row_cells(d,1)}</tr>')
    return "".join(out)
def simpact_rows():
    out=[]
    for tag,sv in S:
        d=simp[tag]; cls = "" if sv==0 else "lost"
        out.append(f'<tr><td class="num">{sv:g}</td>{row_cells(d,2,sign=(sv!=0),cls=cls)}</tr>')
    return "".join(out)
def menv_rows():
    out=[]
    for tag,sv in S:
        d=menv[tag]
        out.append(f'<tr><td class="num">{sv:g}</td>{row_cells(d,2,sign=(sv!=0))}</tr>')
    return "".join(out)
def drain_rows():
    out=[]
    for M in ("identity","uniform"):
        for tag,sv in S:
            if sv==0: continue
            r=dlast(M, sv)
            if not r: continue
            out.append(f'<tr><td>{M}</td><td class="num">{sv:g}</td>'
                       f'<td class="n">{float(r["drain_B"]):.0f}</td>'
                       f'<td class="n">{float(r["nw_total_T"]):.0f}</td>'
                       f'<td class="n">{int(r["n_clamped"])}</td>'
                       f'<td class="n">{float(r["pop_clamped_M"]):.2f}</td></tr>')
    return "".join(out)

CSS = """
:root{--ground:#f5f7f8;--surface:#fff;--surface-2:#eef2f4;--ink:#16242e;--body:#33444f;
--muted:#6b7c86;--hair:#dbe3e8;--hair-strong:#c6d2d8;--teal:#0e7c8b;--teal-deep:#0a4f59;
--teal-wash:#e3f0f1;--amber:#c07426;--plum:#8a4b86;--lost:#b5473d;--lost-wash:#fbf3f1;--good:#2f7d5b;
--serif:"Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif;
--sans:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
--mono:ui-monospace,"SF Mono",Menlo,Consolas,monospace;}
*{box-sizing:border-box}
body{background:var(--ground);color:var(--body);font-family:var(--sans);line-height:1.6;margin:0;-webkit-font-smoothing:antialiased}
.wrap{max-width:980px;margin:0 auto;padding:0 24px 96px}.col{max-width:70ch}
h1,h2,h3{font-family:var(--serif);color:var(--ink);font-weight:600;letter-spacing:-.01em;line-height:1.15;text-wrap:balance}
h2{font-size:1.7rem;margin:0 0 .35em}h3{font-size:1.16rem;margin:1.7em 0 .4em}p{margin:0 0 1em}
strong{color:var(--ink);font-weight:600}a{color:var(--teal-deep)}
.eyebrow{font-family:var(--mono);font-size:.72rem;letter-spacing:.16em;text-transform:uppercase;color:var(--teal);font-weight:600}
.num{font-family:var(--mono);font-variant-numeric:tabular-nums}.lost{color:var(--lost)}.good{color:var(--good)}.dim{color:var(--muted)}
header.hero{padding:60px 0 26px}header.hero h1{font-size:2.8rem;margin:.18em 0 .25em;line-height:1.06}
.lede{font-size:1.18rem;color:var(--body);max-width:66ch}.lede .key{color:var(--ink);font-weight:600}
.meta-line{font-family:var(--mono);font-size:.76rem;color:var(--muted);margin-top:16px;display:flex;flex-wrap:wrap;gap:6px 16px}
section{padding:28px 0;border-top:1px solid var(--hair)}section.flush{border-top:none}
.verdict{background:var(--ink);color:#dfe8ec;border-radius:14px;padding:24px 26px;margin:6px 0}
.verdict h2{color:#fff;font-size:1.2rem;margin-bottom:.5em}.verdict p{color:#c3d2d8;max-width:70ch;margin-bottom:.6em}
.verdict .lost{color:#f0a59b}.verdict .pos{color:#8fd0b3}.verdict p:last-child{margin-bottom:0}
.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(158px,1fr));gap:14px;margin-top:18px}
.stat{background:var(--surface);border:1px solid var(--hair);border-radius:12px;padding:15px 16px 13px}
.stat .v{font-family:var(--mono);font-variant-numeric:tabular-nums;font-size:1.45rem;color:var(--ink);font-weight:600;line-height:1.1}
.stat .v.lost{color:var(--lost)}.stat .l{font-size:.8rem;color:var(--muted);margin-top:5px;line-height:1.35}
.tbl-wrap{overflow-x:auto;margin:8px 0 2px;border:1px solid var(--hair);border-radius:12px}
table{border-collapse:collapse;width:100%;font-size:.88rem;background:var(--surface)}
caption{caption-side:bottom;color:var(--muted);font-size:.78rem;text-align:left;padding:8px 4px 0}
th,td{padding:9px 13px;text-align:left;border-bottom:1px solid var(--hair);white-space:nowrap}
thead th{font-family:var(--mono);font-size:.7rem;letter-spacing:.04em;text-transform:uppercase;color:var(--muted);font-weight:600;background:var(--surface-2)}
tbody tr:last-child td{border-bottom:none}td.n,th.n{text-align:right;font-family:var(--mono);font-variant-numeric:tabular-nums}
tr.tot td{font-weight:600;color:var(--ink);background:var(--surface-2)}
.chart{background:var(--surface);border:1px solid var(--hair);border-radius:14px;padding:18px 20px 12px;margin:8px 0}
.chart .ct{font-family:var(--serif);color:var(--ink);font-size:1.04rem;font-weight:600}
.chart .cs{font-size:.84rem;color:var(--muted);margin:2px 0 10px}
.legend{display:flex;flex-wrap:wrap;gap:6px 18px;margin-top:8px;font-size:.8rem}
.legend span{display:inline-flex;align-items:center;gap:7px;color:var(--body)}
.legend i{width:16px;height:3px;border-radius:2px;display:inline-block}.legend i.d{height:0;border-top:3px dashed var(--amber)}
svg{display:block;width:100%;height:auto;overflow:visible}.axt{font-family:var(--mono);font-size:10.5px;fill:var(--muted)}
.note{background:var(--lost-wash);border:1px solid #eccfc9;border-radius:11px;padding:14px 16px;font-size:.9rem;color:var(--body)}
ul{margin:0 0 1em;padding-left:1.15em}li{margin:.25em 0}
"""

TITLE = "Bounding s &amp; M — capital-gains +5pp under carryover basis"
INNER = f"""<div class="wrap"><div class="col">

<header class="hero">
<div class="eyebrow">Wealth bathtub &middot; sensitivity bounds</div>
<h1>How much do <span class="num">s</span> and <span class="num">M</span> move a capital-gains reform?</h1>
<p class="lede">Bounding the wealth saving-financing channel on a <span class="key">+5pp top capital-gains rate under carryover basis at death</span>. The saving share <span class="num">s</span> is the first-order knob; the within-age wealth-mobility operator <span class="num">M</span> is second-order. Across the full plausible range the conventional 10-year estimate sits at <span class="key">${tot_s0:.0f}B</span> (no channel) falling to <span class="key">${tot_lo:.0f}B</span> at the <span class="num">s=1</span> extreme &mdash; an envelope of about <span class="key">{tot_s0-tot_lo:.0f}B</span>, of which <span class="num">M</span> contributes at most <span class="num">~$2B</span>.</p>
<div class="meta-line"><span>reform: tests/kg_dyn_rate_up_carryover</span><span>behavior: kg_dynamics/turnover</span><span>full sample</span><span>FY2027&ndash;2036</span><span>Tax-Data 2026060918</span></div>
</header>

<section class="flush"><div class="verdict">
<h2>Bottom line</h2>
<p>For a capital-income reform, <strong>s dominates</strong>: financing the tax hike out of wealth (rather than consumption) erodes the future capital-income and estate bases, and the cross-base cost scales almost linearly &mdash; from <span class="lost">&minus;${abs(simp['s25']['total']):.1f}B</span> at s=0.25 to <span class="lost">&minus;${abs(simp100['total']):.1f}B</span> at s=1 (about <span class="lost">&minus;$22B per unit of s</span>). At a central <span class="pos">s=0.5</span> the channel costs <span class="lost">&minus;${abs(simp50['total']):.1f}B</span> over ten years (~2% of the estimate).</p>
<p><strong>M is second-order.</strong> Swapping full persistence (identity) for extreme diffusion (uniform 1/n) moves the <em>total</em> by at most <span class="num">~$2B</span> over ten years (&lt;0.4%) &mdash; and even that is mostly <em>reallocation</em>, not magnitude: diffusion shifts leakage off the threshold'd estate base (persistence concentrates the drain on estate-taxable wealth) and onto payroll (it spreads onto working-age pass-through owners), the two nearly cancelling. At realistic near-identity mobility, M is negligible.</p>
</div>

<div class="stats">
<div class="stat"><div class="v">${tot_s0:.0f}B</div><div class="l">10y conventional estimate, no channel (s=0)</div></div>
<div class="stat"><div class="v lost">&minus;${abs(simp50['total']):.1f}B</div><div class="l">cross-base cost of the saving channel at s=0.5</div></div>
<div class="stat"><div class="v lost">&minus;${abs(simp100['total']):.1f}B</div><div class="l">&hellip; at the s=1 upper bound</div></div>
<div class="stat"><div class="v">&plusmn;$2B</div><div class="l">full M envelope (identity &harr; uniform), 10y</div></div>
</div>
</section>

<section>
<h2>First order: the saving share s</h2>
<p>At <span class="num">s=0</span> the wealth channel is dormant and the reform is the clean law-plus-realization estimate: <strong>${tot_s0:.0f}B</strong> over ten years, essentially all income tax. Turning the channel on diverts a share <span class="num">s</span> of the net during-life tax increase into wealth decumulation, which compounds and shrinks the bases that later get taxed.</p>
<div class="chart">
<div class="ct">Total 10-year estimate vs. saving share</div>
<div class="cs">Reform &minus; baseline, conventional. The teal&ndash;amber band is the entire M sensitivity &mdash; hair-thin against the s slope.</div>
{svg_estimate()}
<div class="legend"><span><i style="background:var(--teal-deep)"></i> M = identity (persistence)</span><span><i class="d"></i> M = uniform (diffusion)</span><span><i style="background:var(--teal-wash)" class="sq"></i> M envelope</span></div>
</div>
<div class="tbl-wrap"><table>
<thead><tr><th class="num">s</th><th class="n">income</th><th class="n">payroll</th><th class="n">estate</th><th class="n">total</th></tr></thead>
<tbody>{headline_rows()}</tbody>
<caption>Headline conventional estimate (reform &minus; baseline), $B over FY2027&ndash;36, at M = identity. Baseline levels: income ${B['income']/1000:.1f}T, payroll ${B['payroll']/1000:.1f}T, estate ${B['estate']:.0f}B.</caption>
</table></div>

<h3>The cross-base cost, isolated</h3>
<p>Subtracting the s=0 leg cancels the baseline and the direct reform effect, leaving the pure interaction &mdash; what the saving response costs, by base it leaks from.</p>
<div class="chart"><div class="ct">Cross-base cost &nbsp;=&nbsp; conv(s) &minus; conv(s=0)</div><div class="cs">Total revenue lost to the saving-financing channel, $B/10y, M = identity.</div>{svg_simpact()}</div>
<div class="tbl-wrap"><table>
<thead><tr><th class="num">s</th><th class="n">income</th><th class="n">payroll</th><th class="n">estate</th><th class="n">total</th></tr></thead>
<tbody>{simpact_rows()}</tbody>
<caption>s-impact = conv(s) &minus; conv(s=0), $B/10y, M = identity. Negative = revenue lost. Roughly linear in s; income is the dominant leak, estate the cross-base signature.</caption>
</table></div>
</section>

<section>
<h2>Second order: the mobility operator M</h2>
<p>M re-grids the accumulated wealth deficit across within-age net-worth percentiles each year. To bound it we ran the <em>identical reform twice</em>: under <strong>identity</strong> (full persistence &mdash; a record's wealth rank never moves; the v1 default, and where the truth sits) and under <strong>uniform 1/n</strong> (extreme diffusion &mdash; ranks fully re-drawn every year). The two estimates and their difference are below. <strong>At s=0 they are identical</strong> &mdash; the channel is dormant, so M cannot bite until s&gt;0.</p>
<p>What M changes is <strong>where the leakage lands, not its size</strong>. Under identity the drain stays on the high-net-worth records that paid the tax, so leakage concentrates in income and the threshold'd <strong>estate</strong> base. Under diffusion it spreads onto working-age pass-through / self-employment owners, shifting leakage toward <strong>payroll</strong> and away from estate. The two nearly cancel: the total moves by at most <span class="num">~$2B</span> over ten years.</p>
<h3>Levels &mdash; M = identity</h3>
<div class="tbl-wrap"><table>
<thead><tr><th class="num">s</th><th class="n">income</th><th class="n">payroll</th><th class="n">estate</th><th class="n">total</th></tr></thead>
<tbody>{headline_rows("identity")}</tbody>
<caption>Conventional estimate (reform &minus; baseline), $B/10y, full persistence.</caption>
</table></div>
<h3>Levels &mdash; M = uniform</h3>
<div class="tbl-wrap"><table>
<thead><tr><th class="num">s</th><th class="n">income</th><th class="n">payroll</th><th class="n">estate</th><th class="n">total</th></tr></thead>
<tbody>{headline_rows("uniform")}</tbody>
<caption>Same reform, same baseline, recomputed under extreme diffusion, $B/10y.</caption>
</table></div>
<h3>Delta &mdash; uniform &minus; identity</h3>
<div class="tbl-wrap"><table>
<thead><tr><th class="num">s</th><th class="n">income</th><th class="n">payroll</th><th class="n">estate</th><th class="n">total</th></tr></thead>
<tbody>{menv_rows()}</tbody>
<caption>Difference of the two tables above. Positive estate = <em>less</em> estate leakage under diffusion; negative payroll = <em>more</em> payroll leakage. |total| &le; ~$2B across all s &mdash; reallocation, not magnitude.</caption>
</table></div>
</section>

<section>
<h2>Mechanism &amp; the clamp caveat</h2>
<p>The drained wealth is small against the stock: at s=0.5 the cumulative drain reaches <strong>${drain50:.0f}B</strong> by 2036 against a <strong>${nw50:.0f}T</strong> net-worth base (~0.12%), rising to <strong>${drain100:.0f}B</strong> at s=1. The aggregate drain is conserved across M (the operator is doubly-stochastic) &mdash; M only redistributes it.</p>
<div class="note"><strong>Clamp caveat.</strong> The per-record haircut is capped at |f| &le; fmax = 0.9. Under identity it almost never binds (&le;{int(cl_id['n_clamped'])} records, {float(cl_id['pop_clamped_M']):.2f}M people at s=1). Under uniform it binds heavily &mdash; <strong>{int(cl_un['n_clamped'])} records ({float(cl_un['pop_clamped_M']):.1f}M people) at s=1</strong> &mdash; because diffusion dumps deficit onto low-net-worth cells that cannot absorb it. So the high-s uniform bound is partly clamp-shaped, not pure diffusion; the realistic (near-identity) case carries negligible clamping. This <em>reinforces</em> that M is second-order for plausible mobility.</p>
<div class="tbl-wrap"><table>
<thead><tr><th>M</th><th class="num">s</th><th class="n">drain 2036 ($B)</th><th class="n">net worth ($T)</th><th class="n">records clamped</th><th class="n">pop clamped (M)</th></tr></thead>
<tbody>{drain_rows()}</tbody>
<caption>Mechanical channel size at the final year, conventional detail. drain = &Sigma; w&middot;D_alloc; clamp incidence at fmax=0.9.</caption>
</table></div>
</section>

<section>
<h2>Method</h2>
<ul>
<li><strong>Reform.</strong> Top preferred rate 0.20&rarr;0.25 (+5pp) with carryover basis at death for all asset classes (kg_death_regime_* = 1); behavioral realization via kg_dynamics/turnover. Baseline = current law.</li>
<li><strong>Grid.</strong> s &isin; {{0, .25, .5, .75, 1}} at M = identity; s &isin; {{.25, .5, .75, 1}} at M = uniform (s=0 is channel-dormant, hence M-independent). Two serialized full-sample pipelines (M is read live at the wealth pre-pass, so they cannot overlap).</li>
<li><strong>Window.</strong> 10 fiscal years FY2027&ndash;2036 (policy effective CY2026, receipts FY-booked with a filing-season t+1 lag; estate booked death-year+1). Sim run one year past the window.</li>
<li><strong>What's varied &mdash; and what isn't.</strong> Only s and M. s is flat across ages and symmetric (v1); calibration of kg realization (psi/planned_share) is held fixed. Static estimates are M- and s-invariant by construction; the interaction is conventional-only.</li>
<li><strong>Conservative s=1.</strong> The fmax clamp means s=1 understates the unconstrained drain &mdash; treat it as a soft upper bound.</li>
</ul>
<p class="dim">Vintages: cgcarry_bound_identity, cgcarry_bound_uniform. Data: bounding_grid.csv, bounding_drain.csv (this directory).</p>
</section>

</div></div>
"""

HEAD = f'<title>{TITLE}</title>\n<style>{CSS}</style>'
# (1) standalone document for opening the file directly off the cluster
standalone = ('<!doctype html><html lang="en"><head><meta charset="utf-8">'
              '<meta name="viewport" content="width=device-width,initial-scale=1">'
              f'{HEAD}</head><body>{INNER}</body></html>')
# (2) artifact body: the publish skeleton supplies <!doctype>/<head>/<body>,
#     so emit only <title> + <style> + content (no html/head/body wrappers)
artifact = f'{HEAD}\n{INNER}'

open(os.path.join(HERE, "bounding_report.html"), "w").write(standalone)
open(os.path.join(HERE, "bounding_report_artifact.html"), "w").write(artifact)
print("wrote bounding_report.html (standalone) +",
      "bounding_report_artifact.html (artifact body)",
      f"[{len(standalone)} / {len(artifact)} bytes]")

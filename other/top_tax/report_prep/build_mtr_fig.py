#!/usr/bin/env python3
"""Build the self-contained 'marginal rate by percentile + deficit-closing top
rate' figure from mtr_viz_data.json (data inlined for the artifact CSP)."""
import json, os

HERE = os.path.dirname(__file__)
DATA = json.load(open(os.path.join(HERE, "mtr_viz_data.json")))
OUT = os.path.join(HERE, "mtr_marginal_rate_fig.html")

HTML = r"""<title>The marginal rate it would take to close the deficit from the top</title>
<style>
  :root {
    --paper:#f9fafb; --ink:#1c2733; --ink2:#3c4a59; --muted:#5a6b7d; --line:#dce3ea; --grid:#e8edf2;
    --axis:#8a99a8; --card:#ffffff; --accent:#1a5b9e;
    --cur:#0072B2; --gap:#D55E00; --cg:#009E73; --ghost:#8a99a8; --danger:#a8433a;
    --chip:#eef2f6; --chipink:#3c4a59;
  }
  @media (prefers-color-scheme: dark) {
    :root { --paper:#131a22; --ink:#e4ebf2; --ink2:#c3ccd6; --muted:#8da0b3; --line:#2b3947; --grid:#223040;
      --axis:#556777; --card:#18212b; --accent:#7db0e0;
      --cur:#3a90d0; --gap:#e8722a; --cg:#1bb98a; --ghost:#6b7c8c; --danger:#e69a92;
      --chip:#1e2937; --chipink:#c3ccd6; }
  }
  :root[data-theme="dark"] { --paper:#131a22; --ink:#e4ebf2; --ink2:#c3ccd6; --muted:#8da0b3; --line:#2b3947;
    --grid:#223040; --axis:#556777; --card:#18212b; --accent:#7db0e0;
    --cur:#3a90d0; --gap:#e8722a; --cg:#1bb98a; --ghost:#6b7c8c; --danger:#e69a92; --chip:#1e2937; --chipink:#c3ccd6; }
  :root[data-theme="light"] { --paper:#f9fafb; --ink:#1c2733; --ink2:#3c4a59; --muted:#5a6b7d; --line:#dce3ea;
    --grid:#e8edf2; --axis:#8a99a8; --card:#ffffff; --accent:#1a5b9e;
    --cur:#0072B2; --gap:#D55E00; --cg:#009E73; --ghost:#8a99a8; --danger:#a8433a; --chip:#eef2f6; --chipink:#3c4a59; }

  * { box-sizing:border-box; }
  html { background:var(--paper); }
  body { margin:0; background:var(--paper); color:var(--ink);
    font-family:Charter,"Bitstream Charter",Cambria,Georgia,serif; font-size:16px; line-height:1.55; }
  .wrap { max-width:60rem; margin:0 auto; padding:2rem 1.25rem 4rem; }
  header { border-bottom:2px solid var(--ink); padding-bottom:.9rem; margin-bottom:1.2rem; }
  .eyebrow { font-family:system-ui,sans-serif; font-size:.7rem; font-weight:600; letter-spacing:.13em;
    text-transform:uppercase; color:var(--accent); margin:0 0 .5rem; }
  h1 { font-size:1.55rem; margin:0 0 .35rem; line-height:1.2; text-wrap:balance; }
  .standfirst { color:var(--muted); font-style:italic; margin:0; max-width:52rem; }
  .fig { background:var(--card); border:1px solid var(--line); border-radius:8px; padding:1.2rem 1.3rem 1rem; margin:1.3rem 0; }
  .controls { display:flex; gap:.5rem; align-items:center; font-family:system-ui,sans-serif; margin:0 0 .3rem; flex-wrap:wrap; }
  .controls .lab { font-size:.72rem; font-weight:700; text-transform:uppercase; letter-spacing:.08em; color:var(--muted); }
  .seg { display:inline-flex; border:1px solid var(--line); border-radius:999px; overflow:hidden; }
  .seg button { font-family:system-ui,sans-serif; font-size:.8rem; font-weight:600; border:0; background:transparent;
    color:var(--ink2); padding:.32rem .8rem; cursor:pointer; }
  .seg button[aria-pressed="true"] { background:var(--accent); color:#fff; }
  .seg button:focus-visible { outline:2px solid var(--accent); outline-offset:2px; }
  .tiles { display:flex; gap:.7rem; flex-wrap:wrap; margin:.7rem 0 .2rem; }
  .tile { flex:1 1 8rem; background:var(--chip); border:1px solid var(--line); border-radius:7px; padding:.5rem .7rem; }
  .tile .k { font-family:system-ui,sans-serif; font-size:.66rem; font-weight:700; text-transform:uppercase; letter-spacing:.06em; color:var(--muted); }
  .tile .v { font-family:system-ui,sans-serif; font-size:1.35rem; font-weight:800; font-variant-numeric:tabular-nums; line-height:1.15; margin-top:.1rem; }
  svg { width:100%; height:auto; display:block; }
  svg text { font-family:system-ui,"Segoe UI",sans-serif; }
  .legend { display:flex; flex-wrap:wrap; gap:.3rem 1.1rem; font-family:system-ui,sans-serif; font-size:.74rem; color:var(--muted); margin:.5rem 0 0; align-items:center; }
  .key { display:inline-flex; align-items:center; gap:.4rem; }
  .sw { width:14px; height:11px; border-radius:2px; display:inline-block; }
  .sw.line { height:3px; border-radius:2px; }
  .cap { font-family:system-ui,sans-serif; font-size:.72rem; color:var(--muted); border-top:1px solid var(--grid); margin-top:.8rem; padding-top:.55rem; line-height:1.5; }
  .cap b { color:var(--ink); }
</style>

<div class="wrap">
<header>
  <p class="eyebrow">The Budget Lab · static illustration · FY2027</p>
  <h1>What marginal rate would it take to close the deficit from the top alone?</h1>
  <p class="standfirst">The colored curve is the marginal tax rate people actually face today, by income
  percentile. The tall bar is the rate the top bracket would need — applied only to income above the
  top-1% threshold, with nothing below it touched — to raise the entire $1.9&nbsp;trillion 2027 deficit,
  statically. Toggle the tax base to see why the base is the binding constraint.</p>
</header>

<div class="fig">
  <div class="controls">
    <span class="lab">Tax base</span>
    <span class="seg" role="group" aria-label="Tax base">
      <button id="b-agi" aria-pressed="true">AGI (incl. capital gains)</button>
      <button id="b-ord" aria-pressed="false">Ordinary income only</button>
    </span>
  </div>
  <div class="tiles" id="tiles"></div>
  <svg id="chart" role="img"></svg>
  <div class="legend">
    <span class="key"><span class="sw line" style="background:var(--cur)"></span>Current marginal rate, by percentile</span>
    <span class="key"><span class="sw" style="background:var(--gap)"></span>Increase needed on the top bracket</span>
    <span class="key"><span class="sw" style="background:var(--ghost);height:2px" ></span>100% — the last dollar</span>
  </div>
  <p class="cap" id="cap"></p>
</div>
</div>

<script>
const DATA = __DATA__;

(function(){
  "use strict";
  function cv(n){ return getComputedStyle(document.documentElement).getPropertyValue(n).trim(); }
  let view = "agi";

  const VIEWS = {
    agi:  { curve: DATA.pct_curve_by_agi.mtr_agi,     req: DATA.required_top_rate.agi,
            baseLabel:"AGI (all income, including capital gains)" },
    ordinary: { curve: DATA.pct_curve_by_ord.mtr_ordinary, req: DATA.required_top_rate.ordinary,
            baseLabel:"ordinary income only (capital gains excluded)" },
  };

  // x-transform: expand the top decile so the top-1% action is visible.
  function xpos(p){ return p<=90 ? (p/90)*0.42 : 0.42 + ((p-90)/10)*0.58; }

  function money(v){ return "$"+(v/1e6).toFixed(2)+"M"; }

  function render(){
    const V = VIEWS[view], curve = V.curve, req = V.req;
    const reqRate = req.tau_required, curTop = req.m_current, thr = req.threshold;
    const impossible = reqRate > 100;

    // tiles
    document.getElementById("tiles").innerHTML = [
      ["Current top marginal rate", curTop.toFixed(0)+"%", "--cur"],
      ["Rate needed on income above the threshold", reqRate.toFixed(0)+"%", impossible?"--danger":"--gap"],
      ["How much higher", "+"+req.increment.toFixed(0)+" pts", "--gap"],
    ].map(t=>`<div class="tile"><div class="k">${t[0]}</div><div class="v" style="color:${cv(t[2])}">${t[1]}</div></div>`).join("");

    const W=760, H=430, mL=54, mR=150, mT=22, mB=52;
    const x0=mL, x1=W-mR, y0=H-mB, y1=mT;
    const yMax=140;
    const X = p => x0 + xpos(p)*(x1-x0);
    const Y = v => y0 - Math.min(v,yMax)/yMax*(y0-y1);
    let o=[];

    // y gridlines
    [0,20,40,60,80,100,120,140].forEach(g=>{
      const y=Y(g), is100=(g===100);
      o.push(`<line x1="${x0}" y1="${y.toFixed(1)}" x2="${x1}" y2="${y.toFixed(1)}" stroke="${is100?cv('--ghost'):(g===0?cv('--axis'):cv('--grid'))}" stroke-width="${g===0||is100?1.4:1}"${is100?' stroke-dasharray="5 4"':''}/>`);
      o.push(`<text x="${x0-8}" y="${(y+3.5).toFixed(1)}" font-size="10" text-anchor="end" fill="${cv('--muted')}">${g}%</text>`);
    });
    o.push(`<text x="${x1-2}" y="${(Y(100)-5).toFixed(1)}" font-size="9.5" text-anchor="end" font-style="italic" fill="${cv('--muted')}">100% of the last dollar</text>`);

    // x ticks
    [0,50,90,99,100].forEach(p=>{
      const x=X(p);
      o.push(`<line x1="${x.toFixed(1)}" y1="${y0}" x2="${x.toFixed(1)}" y2="${y0+5}" stroke="${cv('--axis')}" stroke-width="1"/>`);
      const lab = p===100?"100th":(p+"th");
      o.push(`<text x="${x.toFixed(1)}" y="${y0+18}" font-size="10" text-anchor="${p===100?'end':(p===0?'start':'middle')}" fill="${cv('--muted')}">${lab}</text>`);
    });
    o.push(`<text x="${((x0+x1)/2).toFixed(1)}" y="${y0+40}" font-size="11.5" text-anchor="middle" fill="${cv('--ink2')}">Income percentile (top decile expanded)</text>`);

    // current-rate area (percentiles 1..99), filled
    const xThr = X(99);
    let area = `M ${X(1).toFixed(1)} ${Y(curve[0]).toFixed(1)}`;
    for(let p=1;p<=99;p++){ area += ` L ${X(p).toFixed(1)} ${Y(curve[p-1]).toFixed(1)}`; }
    area += ` L ${xThr.toFixed(1)} ${y0} L ${X(1).toFixed(1)} ${y0} Z`;
    o.push(`<path d="${area}" fill="${cv('--cur')}" opacity="0.13"/>`);
    // current-rate line 1..100
    let line="";
    for(let p=1;p<=100;p++){ line += (p===1?"M":"L")+` ${X(p).toFixed(1)} ${Y(curve[p-1]).toFixed(1)}`; }
    o.push(`<path d="${line}" fill="none" stroke="${cv('--cur')}" stroke-width="2.4" stroke-linejoin="round"/>`);

    // threshold marker at p99
    o.push(`<line x1="${xThr.toFixed(1)}" y1="${y1}" x2="${xThr.toFixed(1)}" y2="${y0}" stroke="${cv('--axis')}" stroke-width="1" stroke-dasharray="2 3" opacity="0.7"/>`);

    // top-bracket required bar: from top-1% zone (p99..100)
    const bx0=X(99), bx1=X(100), bw=bx1-bx0;
    const yCur=Y(curTop), yReq=Y(reqRate);
    // base portion (current), then the increase on top
    o.push(`<rect x="${bx0.toFixed(1)}" y="${yCur.toFixed(1)}" width="${bw.toFixed(1)}" height="${(y0-yCur).toFixed(1)}" fill="${cv('--cur')}" opacity="0.35"/>`);
    o.push(`<rect x="${bx0.toFixed(1)}" y="${yReq.toFixed(1)}" width="${bw.toFixed(1)}" height="${(yCur-yReq).toFixed(1)}" fill="${cv(impossible?'--danger':'--gap')}" opacity="0.9"/>`);
    // required value label
    o.push(`<text x="${((bx0+bx1)/2).toFixed(1)}" y="${(yReq-6).toFixed(1)}" font-size="13" font-weight="800" text-anchor="middle" fill="${cv(impossible?'--danger':'--gap')}" font-variant-numeric="tabular-nums">${reqRate.toFixed(0)}%</text>`);
    if(impossible)
      o.push(`<text x="${((bx0+bx1)/2).toFixed(1)}" y="${(yReq-20).toFixed(1)}" font-size="9" font-weight="700" text-anchor="middle" fill="${cv('--danger')}">impossible</text>`);

    // right-side callouts
    const cxr = x1+10;
    o.push(`<text x="${cxr}" y="${(yReq+3).toFixed(1)}" font-size="11" font-weight="700" text-anchor="start" fill="${cv(impossible?'--danger':'--gap')}">Needed: ${reqRate.toFixed(0)}%</text>`);
    o.push(`<text x="${cxr}" y="${(yReq+16).toFixed(1)}" font-size="9.5" text-anchor="start" fill="${cv('--muted')}">on income above ${money(thr)}</text>`);
    o.push(`<text x="${cxr}" y="${(yCur+4).toFixed(1)}" font-size="10.5" font-weight="700" text-anchor="start" fill="${cv('--cur')}">Today: ${curTop.toFixed(0)}%</text>`);
    // "no change below" note near mid-curve
    o.push(`<text x="${X(55).toFixed(1)}" y="${(Y(curve[54])-8).toFixed(1)}" font-size="9.5" font-style="italic" text-anchor="middle" fill="${cv('--muted')}">unchanged below the top 1%</text>`);

    const svg=document.getElementById("chart");
    svg.setAttribute("viewBox",`0 0 ${W} ${H}`);
    svg.innerHTML=o.join("");

    document.getElementById("cap").innerHTML =
      `<b>Base: ${V.baseLabel}.</b> The curve is the weighted-average marginal rate on the next dollar, by `+
      `income percentile (Tax-Simulator v3, 2027 baseline). The bar is a new top-bracket rate on `+
      `${view==='agi'?'AGI':'ordinary income'} above ${money(thr)} (the top-1% floor), set so its static `+
      `revenue equals the $1.9T FY2027 deficit — nothing below the threshold changes. `+
      (impossible
        ? `Because most top income is capital gains that sit outside the ordinary base, the required rate exceeds <b>100%</b> — it cannot be done on ordinary income alone.`
        : `Widening the base to all income (AGI) brings the required rate down to <b>${reqRate.toFixed(0)}%</b> — still near-confiscatory. `)+
      ` Static, base held fixed; behavioral responses would push it higher. At the top, a dollar of capital gains faces only ~19% at the margin, vs ~37% on ordinary income — which is why the base is the binding constraint.`;

    document.getElementById("b-agi").setAttribute("aria-pressed", view==="agi");
    document.getElementById("b-ord").setAttribute("aria-pressed", view==="ordinary");
  }

  document.getElementById("b-agi").addEventListener("click",()=>{view="agi";render();});
  document.getElementById("b-ord").addEventListener("click",()=>{view="ordinary";render();});
  render();

  const mq=window.matchMedia("(prefers-color-scheme: dark)");
  if(mq.addEventListener) mq.addEventListener("change", render);
  new MutationObserver(render).observe(document.documentElement,{attributes:true,attributeFilter:["data-theme"]});
})();
</script>
"""

open(OUT, "w").write(HTML.replace("__DATA__", json.dumps(DATA)))
print("wrote", OUT, os.path.getsize(OUT), "bytes")

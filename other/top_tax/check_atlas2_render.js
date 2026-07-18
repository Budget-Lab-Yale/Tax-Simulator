#!/usr/bin/env node
// Headless render + surrogate-parity check for the built atlas2 page.
// Fork+extend of check_atlas_render.js. Run before every publish:
//
//   node other/top_tax/check_atlas2_render.js [other/top_tax/atlas2_built.html]
//   node other/top_tax/check_atlas2_render.js --allow-placeholder <file>   (stub track only)
//
// Beyond the v1 container checks, asserts the interpolation machinery:
//   - anchor reproduction: every lever x non-zero grid row, evalQ === stored (exact)
//   - zero identity: empty state evaluates to exactly 0
//   - holdout fixtures (meta.surrogate.checks) within the validated bound —
//     FAILS if absent (a data build without validation is unshippable)
//   - Shapley additivity at 3 probe states
//   - inputs wired: set value, fire onchange, containers change; out-of-range
//     input clamps to the meta bound
//   - frontier: non-empty and byte-identical across two fresh vm runs
//   - badge states the validated bound; fails on Preview/placeholder
"use strict";
const fs = require("fs");
const path = require("path");
const vm = require("vm");

const args = process.argv.slice(2);
const allowPlaceholder = args.includes("--allow-placeholder");
const fileArg = args.filter(a => a !== "--allow-placeholder")[0];
const file = fileArg || path.join(__dirname, "atlas2_built.html");
const html = fs.readFileSync(file, "utf8");

// ---- minimal DOM stub (v1 + value/onchange/min/max/step for dial inputs) ----
function makeWorld() {
  const created = [];
  class El {
    constructor(tag, id) {
      this.tagName = (tag || "div").toUpperCase();
      this.id = id || "";
      this.children = [];
      this.dataset = {};
      this.style = {};
      this.attrs = {};
      this.innerHTML = "";
      this.textContent = "";
      this.className = "";
      this.clientWidth = 900;
      this.value = "";
      this.min = "";
      this.max = "";
      this.step = "";
      this.placeholder = "";
      this.type = "";
      this.onclick = null;
      this.onchange = null;
      this.width = 0;
      this.height = 0;
      this.classList = { add() {}, remove() {} };
      this.parentElement = { clientWidth: 900 };
    }
    appendChild(c) { this.children.push(c); return c; }
    setAttribute(k, v) { this.attrs[k] = String(v); }
    getAttribute(k) { return k in this.attrs ? this.attrs[k] : null; }
    addEventListener() {}
    // canvas surface (the 3D section): every 2D-context method is a no-op
    getContext() { return new Proxy({}, { get: () => () => {}, set: () => true }); }
    getBoundingClientRect() { return { width: 900, height: 540, left: 0, top: 0 }; }
    setPointerCapture() {}
  }
  const byId = {};
  for (const m of html.matchAll(/id="([A-Za-z][\w-]*)"/g)) {
    if (!byId[m[1]]) byId[m[1]] = new El("div", m[1]);
  }
  function querySelector(sel) {
    let m;
    if ((m = sel.match(/^#([\w-]+)$/))) return byId[m[1]] || null;
    if ((m = sel.match(/^\.sw\[data-key="(.+)"\]$/)))
      return created.find(e => e.className === "sw" && e.dataset.key === m[1]) || null;
    if ((m = sel.match(/^\.lever\[data-key="(.+)"\]$/)))
      return created.find(e => /(^| )lever( |$)/.test(e.className) && e.dataset.key === m[1]) || null;
    if ((m = sel.match(/^\.seg button\[data-key="(.+)"\]\[data-pos="(.+)"\]$/)))
      return created.find(e => e.dataset.key === m[1] && e.dataset.pos === m[2]) || null;
    return null;
  }
  // parse the dist-card toggle buttons (static HTML inside <span class="unit" id=...>)
  function parseBtns(togId) {
    const block = (html.match(new RegExp('id="' + togId + '"[\\s\\S]*?</span>')) || [""])[0];
    return [...block.matchAll(/<button ([^>]*)>/g)].map(m => {
      const e = new El("button");
      for (const a of m[1].matchAll(/data-(\w+)="([^"]*)"/g)) e.dataset[a[1]] = a[2];
      created.push(e);
      return e;
    });
  }
  const distViewBtns = parseBtns("distViewTog"), etrDefBtns = parseBtns("etrDefTog");
  function querySelectorAll(sel) {
    if (sel === "input[data-lever]")
      return created.filter(e => e.tagName === "INPUT" && e.dataset.lever);
    if (sel === "#distViewTog button") return distViewBtns;
    if (sel === "#etrDefTog button") return etrDefBtns;
    return [];
  }
  const documentStub = {
    querySelector, querySelectorAll,
    createElement(tag) { const e = new El(tag); created.push(e); return e; },
    addEventListener() {},
    body: { insertAdjacentHTML() {} },
    _distViewBtns: distViewBtns, _etrDefBtns: etrDefBtns,
  };
  const windowStub = {
    addEventListener() {},
    devicePixelRatio: 1,
    matchMedia: () => ({ matches: true }),   // reduced-motion path: no animation loop
  };
  const sandbox = {
    document: documentStub,
    window: windowStub,
    console,
    setTimeout: (fn) => fn && 0,
    clearTimeout: () => {},
    performance: { now: () => 0 },
    requestAnimationFrame: () => 0,
    Math, JSON, Array, Object, Number, String, Date, isFinite, isNaN,
    parseFloat, parseInt, Infinity, NaN,
  };
  sandbox.window.document = documentStub;
  vm.createContext(sandbox);
  return { sandbox, byId, created };
}

function runScripts(world) {
  const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map(m => m[1]);
  if (!scripts.length) fail("no <script> block found in " + file);
  for (const src of scripts) new vm.Script(src);           // syntax gate
  for (const src of scripts) vm.runInContext(src, world.sandbox, { filename: file });
}

const problems = [];
const w1 = makeWorld();
try { runScripts(w1); }
catch (err) { fail("script threw during initial render:\n" + err.stack); }

const A = w1.sandbox.window.__ATLAS2__;
if (!A) fail("window.__ATLAS2__ not exposed — page hooks missing");
const DATA = A.DATA;

// ---- boot contract: page loads at current law, nothing selected ---------------
const bootSel = A.surState();
if (Object.keys(bootSel).length)
  problems.push(`boot state: expected all levers off at load, got ${JSON.stringify(bootSel)}`);

// ---- container render checks -------------------------------------------------
// selection-dependent panels are empty at boot by design; drive a probe
// package through the harness hook so every panel has content to check
A.setState({ ord: { rate: 44.8 }, cg: { rate: 40 }, wealth: { rate: 2, thr: 50e6 }, deemed: { pos: "deemed" } });
const mustRenderSvg  = ["stackfig", "etr", "frontChart"];
const mustBeNonEmpty = ["tiles", "stackfigLegend", "etrLegend", "frontLegend", "rateSw", "structSw"];
for (const id of mustRenderSvg) {
  const e = w1.byId[id];
  if (!e) problems.push(`#${id}: container missing from HTML`);
  else if (!/<svg/i.test(e.innerHTML)) problems.push(`#${id}: no <svg> rendered (innerHTML ${e.innerHTML.length} chars)`);
}
for (const id of mustBeNonEmpty) {
  const e = w1.byId[id];
  if (!e) problems.push(`#${id}: container missing from HTML`);
  else if (!(e.innerHTML.trim() || e.textContent.trim() || e.children.length))
    problems.push(`#${id}: rendered empty`);
}

// ---- anchor reproduction: evalQ at every non-zero grid row === stored row -----
// Reconstruct each lever's row-major grid states from meta knots (the same
// contract fit_surrogate.py uses), then demand EXACT equality at the knots.
function gridStates(lv) {
  const axes = lv.params.map(p => ({ key: p.key, knots: p.knots }));
  if (axes.length === 1) return axes[0].knots.map(k => ({ [axes[0].key]: k }));
  const out = [];
  for (const a of axes[0].knots) for (const b of axes[1].knots)
    out.push({ [axes[0].key]: a, [axes[1].key]: b });
  return out;
}
let nAnchor = 0;
for (const lv of DATA.meta.levers) {
  const rows = DATA.surrogate.solo[lv.key];
  const states = gridStates(lv);
  for (const qid of ["ct", "st"]) {
    if (!rows[qid]) continue;
    states.forEach((vals, i) => {
      const stored = rows[qid][i];
      const isZero = stored.every(v => v === 0);
      if (isZero) return;                       // off-knot rows: covered by zero identity
      const s = { [lv.key]: vals };
      const pred = A.evalQ(qid, s);
      nAnchor++;
      for (let z = 0; z < stored.length; z++) {
        if (pred[z] !== stored[z]) {
          problems.push(`anchor: ${lv.key} ${qid} row ${i} elem ${z}: evalQ ${pred[z]} !== stored ${stored[z]}`);
          return;
        }
      }
    });
  }
}
if (nAnchor < 50) problems.push(`anchor: only ${nAnchor} anchor rows exercised (grid wiring suspect)`);

// ---- zero identity -------------------------------------------------------------
for (const qid of DATA.meta.surrogate.quantities) {
  const v = A.evalQ(qid, {});
  if (!v.every(x => x === 0)) problems.push(`zero identity: evalQ(${qid}, {}) != 0`);
}

// ---- holdout fixtures within the validated per-decade bounds --------------------
const val = DATA.meta.surrogate.validation;
const checks = DATA.meta.surrogate.checks;
const N_DEC = (DATA.meta.decades || []).length;
if (!val || !checks || !checks.length) {
  problems.push("meta.surrogate.validation/checks absent — unvalidated data is unshippable");
} else if (!val.bounds_pct || val.bounds_pct.length !== N_DEC || !val.static_bounds_pct) {
  problems.push("meta.surrogate.validation lacks per-decade bounds_pct/static_bounds_pct");
} else {
  for (const c of checks) {
    if (!c.conv_totals || c.conv_totals.length !== N_DEC)
      { problems.push(`fixture ${c.id}: conv_totals missing or wrong length`); continue; }
    const pc = A.evalQ("ct", c.state);
    const ps = A.evalQ("st", c.state);
    for (let d = 0; d < N_DEC; d++) {
      const bound = val.bounds_pct[d] / 100 + 1e-9;
      const err = Math.abs(pc[d] - c.conv_totals[d]) / Math.max(1e-9, Math.abs(c.conv_totals[d]));
      if (err > bound)
        problems.push(`fixture ${c.id} d${d + 1}: conv pred ${pc[d].toFixed(1)} vs run ${c.conv_totals[d]} — ${(100 * err).toFixed(2)}% > ±${val.bounds_pct[d]}%`);
      // static gates against its own stamped (looser) per-decade bound: the hard
      // bar is conv-d1-only; static accuracy is measured and disclosed, not assumed
      const boundS = val.static_bounds_pct[d] / 100 + 1e-9;
      const errS = Math.abs(ps[d] - c.static_totals[d]) / Math.max(1e-9, Math.abs(c.static_totals[d]));
      if (errS > boundS)
        problems.push(`fixture ${c.id} d${d + 1}: static pred ${ps[d].toFixed(1)} vs run ${c.static_totals[d]} — ${(100 * errS).toFixed(2)}% > ±${val.static_bounds_pct[d]}%`);
    }
  }
}

// ---- Shapley additivity at 3 probe states ----------------------------------------
const probes = (checks && checks.length >= 3 ? checks.slice(0, 3).map(c => c.state) : [
  { ord: { rate: 44.8 }, qbi: { on: 1 } },
  { cg: { rate: 40.0 }, deemed: { pos: "deemed" } },
  { wealth: { rate: 2.0, thr: 50e6 }, estate: { rate: 50.0, exem: 8460000.0 }, cg: { rate: 40.0 } },
]);
for (const s of probes) {
  for (let d = 0; d < Math.max(1, N_DEC); d++) {
    const phi = A.shapley(s, d);
    const tot = A.evalQ("ct", s)[d];
    const sum = Object.values(phi).reduce((a, b) => a + b, 0);
    if (Math.abs(sum - tot) > 1e-6 * Math.max(1, Math.abs(tot)))
      problems.push(`shapley additivity d${d + 1}: sum(phi)=${sum} != evalQ ct ${tot} at ${JSON.stringify(s)}`);
  }
}

// ---- inputs wired: value -> onchange -> re-render; clamping --------------------
const ordInput = w1.created.find(e => e.tagName === "INPUT" && e.dataset.lever === "ord");
const ordSw = w1.created.find(e => e.tagName === "BUTTON" && e.className === "sw" && e.dataset.key === "ord");
if (!ordInput) problems.push("input wiring: no created <input> for lever ord");
else if (typeof ordInput.onchange !== "function") problems.push("input wiring: ord input has no onchange handler");
else {
  // dials boot at current law with the switch off: turn ord on through its
  // real switch (levers off at boot ⇒ surState().ord would be undefined)
  A.setState({});
  if (!ordSw || typeof ordSw.onclick !== "function") problems.push("input wiring: no .sw switch button for lever ord");
  else ordSw.onclick();
  const before = w1.byId.tiles.innerHTML;
  ordInput.value = "46.5";
  ordInput.onchange();
  const after = w1.byId.tiles.innerHTML;
  if (before === after) problems.push("input wiring: changing ord rate did not change #tiles");
  const ordState = A.surState().ord;
  if (!ordState || Math.abs(ordState.rate - 46.5) > 1e-9)
    problems.push(`input wiring: ord state ${JSON.stringify(ordState)} after typing 46.5`);
  // out-of-range clamps to the meta bound
  ordInput.value = "99";
  ordInput.onchange();
  const ordMax = DATA.meta.levers.find(l => l.key === "ord").params[0].max;
  const clamped = A.surState().ord;
  if (!clamped || clamped.rate !== ordMax)
    problems.push(`clamping: typed 99, state rate ${clamped && clamped.rate} != max ${ordMax}`);
  if (String(ordInput.value) !== String(ordMax))
    problems.push(`clamping: input box shows ${ordInput.value}, expected ${ordMax}`);
}

// toggle every switch off/on; segmented + toggle buttons re-render safely
try {
  for (const b of w1.created.filter(e => e.onclick)) { b.onclick(); b.onclick(); }
} catch (err) {
  fail("script threw during control-toggle re-render:\n" + err.stack);
}
if (typeof A.setDecade === "function") A.setDecade(0);   // toggle loop can leave a non-default decade

// ---- distribution card: each of the three views renders valid marks ---------------
{
  const dv = w1.sandbox.document._distViewBtns, def = w1.sandbox.document._etrDefBtns;
  const byV = {}; dv.forEach(b => byV[b.dataset.v] = b);
  // activate a plan so there is real avoidance (static ≠ conventional)
  if (typeof A.setState === "function") A.setState({ ord: { rate: 44.8 }, cg: { rate: 40 }, wealth: { rate: 2, thr: 50e6 } });
  if (!byV.context || !byV.new || !byV.etr) problems.push("dist card: view toggle buttons missing");
  else {
    const etrEl = w1.byId.etr, capEl = w1.byId.distCap;
    const expect = { context: /<rect /, new: /<rect /, etr: /<circle / };
    for (const v of ["context", "new", "etr"]) {
      byV[v].onclick();
      const h = etrEl.innerHTML;
      if (!h || h.length < 200) problems.push(`dist view ${v}: chart empty`);
      if (!expect[v].test(h)) problems.push(`dist view ${v}: missing expected marks`);
      if (/NaN|undefined|\$NaN/.test(h)) problems.push(`dist view ${v}: NaN/undefined in output`);
      if (v !== "etr" && !/url\(#disthatch\)/.test(h)) problems.push(`dist view ${v}: no leakage hatch`);
      if (!capEl.innerHTML || capEl.innerHTML.length < 40) problems.push(`dist view ${v}: caption not set`);
    }
    // cash vs accrual must actually move the dollar bars (accrual base is larger)
    byV.context.onclick();
    def.forEach(b => { if (b.dataset.def === "expanded") b.onclick(); });
    const cash = w1.byId.etr.innerHTML;
    def.forEach(b => { if (b.dataset.def === "hs") b.onclick(); });
    byV.context.onclick();
    const accr = w1.byId.etr.innerHTML;
    if (cash === accr) problems.push("dist card: cash vs accrual did not change the In-context bars");
    def.forEach(b => { if (b.dataset.def === "expanded") b.onclick(); });
    byV.context.onclick();
  }
}

// ---- decade toggle: switching re-renders and changes the numbers -----------------
if (typeof A.setDecade !== "function" || N_DEC < 2) {
  problems.push("decade toggle: setDecade hook or meta.decades missing");
} else {
  A.setState({ ord: { rate: 44.8 }, wealth: { rate: 2, thr: 50e6 } });
  const t0 = w1.byId.tiles.innerHTML;
  A.setDecade(1);
  const t1 = w1.byId.tiles.innerHTML;
  if (t0 === t1) problems.push("decade toggle: switching to decade 2 did not change #tiles");
  if (A.getDecade() !== 1) problems.push("decade toggle: getDecade() != 1 after setDecade(1)");
  for (const id of ["pvw", "spill", "frontChart"]) {
    const e = w1.byId[id];
    if (e && !/<svg/i.test(e.innerHTML)) problems.push(`#${id}: no <svg> after decade switch`);
  }
  A.setDecade(2);
  if (!A.FRONTD() || !A.FRONTD().pts.length)
    problems.push("frontier: empty lattice under decade 3");
  A.setDecade(0);
  A.setState({});
}

// ---- dist card correctness: REF dollars reproduce the stack_ref run --------------
// The dollar views are surrogate ETR-delta × fixed baseline income. At the reference
// dials this must reproduce the actual stack_ref run (top 0.01%: current-law $358B,
// new taxes $259B, leakage $121B). v2 (2026-07-12, top_tax_dials_30y_v2): re-pinned
// from the v1 split ($206B new / $178B leak) — the estate-margins + wealth-carry
// physics raised top-0.01% conventional survival (more collected, less leaked); total
// ask ~flat ($384B->$380B). Verified against the REAL run via extract_atlas_data on
// the expanded/fixed/wealth_cit_vat slice — surrogate reproduces it to <$0.4B.
{
  const D = JSON.parse(fs.readFileSync(path.join(__dirname, "atlas2_data.json"), "utf8"));
  const lev = D.meta.levers, groups = D.meta.etr_groups, comps = D.meta.etr_comps;
  const idf = "expanded", di = D.meta.etr_income_defs.indexOf(idf);
  const gi = groups.indexOf("Top 0.01%"), NC = comps.length, NG = groups.length;
  const inc = D.income_levels[idf]["Top 0.01%"];
  const base = D.etr_base[idf][String(D.meta.dist_years[0])]["Top 0.01%"];
  const five = ["income_tax", "payroll", "estate", "deemed", "wealth", "corp"].map(c => comps.indexOf(c));
  const ref = {}; lev.forEach(l => { ref[l.key] = {}; l.params.forEach(p => { ref[l.key][p.key] = p.ref; }); });
  const etrD = A.evalQ("etr", ref), etrcD = A.evalQ("etrc", ref);
  const rowTot = (delta) => { let t = 0; for (const c of five) { const d = delta ? delta[(di * NG + gi) * NC + c] : 0; t += Math.max(0, base[c] + d); } return t; };
  const baseRate = five.reduce((t, c) => t + base[c], 0);
  const statRate = rowTot(etrD), convRate = rowTot(etrcD);
  const clD = baseRate / 100 * inc, newD = (convRate - baseRate) / 100 * inc, leakD = (statRate - convRate) / 100 * inc;
  const exp = { cl: 358.354, nw: 258.910, lk: 121.321 };
  const near = (a, b, tol) => Math.abs(a - b) <= tol;
  if (!near(clD, exp.cl, 1)) problems.push(`dist REF: current-law $${clD.toFixed(1)}B vs stack_ref $${exp.cl}B`);
  if (!near(newD, exp.nw, 12)) problems.push(`dist REF: new taxes $${newD.toFixed(1)}B vs stack_ref $${exp.nw}B (surrogate tol)`);
  if (!near(leakD, exp.lk, 12)) problems.push(`dist REF: leakage $${leakD.toFixed(1)}B vs stack_ref $${exp.lk}B (surrogate tol)`);
  A.setState({});
}

// ---- frontier: non-empty + byte-identical across two fresh vm runs ---------------
const FR = A.FRONTD();
if (!FR || !FR.pts.length)
  problems.push("frontier: empty lattice");
else {
  for (const metric of ["rev", "etr"]) {
    const fr = A.frontFor(FR, metric);
    if (!fr.length) problems.push(`frontier: empty undominated set for metric ${metric}`);
  }
  // metric toggle re-renders and the ETR axis produces finite positive spans
  if (typeof A.setFrontMetric === "function") {
    A.setState({ ord: { rate: 44.8 }, cg: { rate: 40 } });
    A.setFrontMetric("etr");
    const e = w1.byId.frontChart;
    if (e && !/<svg/i.test(e.innerHTML)) problems.push("#frontChart: no <svg> under ETR metric");
    if (!FR.pts.some(p => isFinite(p.ex) && p.ex > 0))
      problems.push("frontier: no package with positive top-0.1% ETR change");
    A.setFrontMetric("rev");
    A.setState({});
  } else problems.push("frontier: setFrontMetric hook missing");
}
const w2 = makeWorld();
try { runScripts(w2); } catch (err) { fail("second vm run threw:\n" + err.stack); }
const f1 = JSON.stringify(A.frontierFor(0).pts) + "|" + JSON.stringify(A.frontierFor(2).pts);
const A2 = w2.sandbox.window.__ATLAS2__;
const f2 = JSON.stringify(A2.frontierFor(0).pts) + "|" + JSON.stringify(A2.frontierFor(2).pts);
if (f1 !== f2) problems.push("frontier: lattice differs between two identical runs (non-deterministic)");

// ---- badge -------------------------------------------------------------------------
const badge = w1.byId.dataBadge && (w1.byId.dataBadge.textContent || "").trim();
if (!badge) problems.push("#dataBadge: empty");
else if (/preview/i.test(badge)) {
  if (!allowPlaceholder) problems.push("#dataBadge: still on placeholder data (" + badge + ")");
} else if (val && val.bounds_pct &&
           !val.bounds_pct.every(b => badge.includes("±" + b + "%"))) {
  problems.push(`#dataBadge: does not state every per-decade bound ${JSON.stringify(val.bounds_pct)} (got: ${badge})`);
}

if (problems.length) fail(problems.join("\n"));
console.log(`OK ${path.basename(file)} — containers rendered; ${nAnchor} anchor rows exact; ` +
  `${checks ? checks.length : 0} holdout fixtures within per-decade bounds ` +
  `${val && val.bounds_pct ? val.bounds_pct.map(b => "±" + b + "%").join("/") : "?"}; ` +
  `frontier ${FR.pts.length} pts deterministic across decades; badge: ${badge}`);

function fail(msg) { console.error("RENDER CHECK FAILED\n" + msg); process.exit(1); }

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
    }
    appendChild(c) { this.children.push(c); return c; }
    setAttribute(k, v) { this.attrs[k] = String(v); }
    getAttribute(k) { return k in this.attrs ? this.attrs[k] : null; }
    addEventListener() {}
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
  function querySelectorAll(sel) {
    if (sel === "input[data-lever]")
      return created.filter(e => e.tagName === "INPUT" && e.dataset.lever);
    return [];
  }
  const documentStub = {
    querySelector, querySelectorAll,
    createElement(tag) { const e = new El(tag); created.push(e); return e; },
    addEventListener() {},
    body: { insertAdjacentHTML() {} },
  };
  const windowStub = { addEventListener() {} };
  const sandbox = {
    document: documentStub,
    window: windowStub,
    console,
    setTimeout: (fn) => fn && 0,
    clearTimeout: () => {},
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
const mustRenderSvg  = ["pvw", "spill", "etr", "frontChart"];
const mustBeNonEmpty = ["tiles", "pvwLegend", "spillLegend", "etrLegend", "frontLegend", "rateSw", "structSw"];
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

// ---- holdout fixtures within the validated bound --------------------------------
const val = DATA.meta.surrogate.validation;
const checks = DATA.meta.surrogate.checks;
if (!val || !checks || !checks.length) {
  problems.push("meta.surrogate.validation/checks absent — unvalidated data is unshippable");
} else {
  const bound = val.bound_pct / 100 + 1e-9;
  for (const c of checks) {
    const pc = A.evalQ("ct", c.state)[0];
    const err = Math.abs(pc - c.conv_total10) / Math.max(1e-9, Math.abs(c.conv_total10));
    if (err > bound)
      problems.push(`fixture ${c.id}: conv pred ${pc.toFixed(1)} vs run ${c.conv_total10} — ${(100 * err).toFixed(2)}% > ±${val.bound_pct}%`);
    const ps = A.evalQ("st", c.state)[0];
    const errS = Math.abs(ps - c.static_total10) / Math.max(1e-9, Math.abs(c.static_total10));
    // static gates against its own stamped (looser) bound: the hard bar is
    // conv-only; static accuracy is measured and disclosed, not assumed
    const boundS = ((val.static_bound_pct != null ? val.static_bound_pct : val.bound_pct) / 100) + 1e-9;
    if (errS > boundS)
      problems.push(`fixture ${c.id}: static pred ${ps.toFixed(1)} vs run ${c.static_total10} — ${(100 * errS).toFixed(2)}% > ±${val.static_bound_pct}%`);
  }
}

// ---- Shapley additivity at 3 probe states ----------------------------------------
const probes = (checks && checks.length >= 3 ? checks.slice(0, 3).map(c => c.state) : [
  { ord: { rate: 44.8 }, qbi: { on: 1 } },
  { cg: { rate: 40.0 }, deemed: { pos: "deemed" } },
  { wealth: { rate: 2.0, thr: 50e6 }, estate: { rate: 50.0, exem: 8460000.0 }, cg: { rate: 40.0 } },
]);
for (const s of probes) {
  const phi = A.shapley(s);
  const tot = A.evalQ("ct", s)[0];
  const sum = Object.values(phi).reduce((a, b) => a + b, 0);
  if (Math.abs(sum - tot) > 1e-6 * Math.max(1, Math.abs(tot)))
    problems.push(`shapley additivity: sum(phi)=${sum} != evalQ ct ${tot} at ${JSON.stringify(s)}`);
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

// ---- frontier: non-empty + byte-identical across two fresh vm runs ---------------
if (!A.FRONTD || !A.FRONTD.pts.length || !A.FRONTD.front.length)
  problems.push("frontier: empty lattice or empty frontier");
const w2 = makeWorld();
try { runScripts(w2); } catch (err) { fail("second vm run threw:\n" + err.stack); }
const f1 = JSON.stringify(w1.sandbox.window.__ATLAS2__.FRONTD.pts);
const f2 = JSON.stringify(w2.sandbox.window.__ATLAS2__.FRONTD.pts);
if (f1 !== f2) problems.push("frontier: lattice differs between two identical runs (non-deterministic)");

// ---- badge -------------------------------------------------------------------------
const badge = w1.byId.dataBadge && (w1.byId.dataBadge.textContent || "").trim();
if (!badge) problems.push("#dataBadge: empty");
else if (/preview/i.test(badge)) {
  if (!allowPlaceholder) problems.push("#dataBadge: still on placeholder data (" + badge + ")");
} else if (val && !badge.includes("±" + val.bound_pct + "%")) {
  problems.push(`#dataBadge: does not state the validated bound ±${val.bound_pct}% (got: ${badge})`);
}

if (problems.length) fail(problems.join("\n"));
console.log(`OK ${path.basename(file)} — containers rendered; ${nAnchor} anchor rows exact; ` +
  `${checks ? checks.length : 0} holdout fixtures within ±${val ? val.bound_pct : "?"}%; ` +
  `frontier ${A.FRONTD.pts.length} pts deterministic; badge: ${badge}`);

function fail(msg) { console.error("RENDER CHECK FAILED\n" + msg); process.exit(1); }

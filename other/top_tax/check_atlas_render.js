#!/usr/bin/env node
// Headless render check for the built atlas page. Run before every publish:
//
//   node other/top_tax/check_atlas_render.js [other/top_tax/atlas_built.html]
//
// Extracts the page's inline script, executes it in a vm with a minimal DOM
// stub, and asserts every chart container rendered non-empty. Catches the
// class of bug where a template edit leaves the script syntactically valid
// but a render function throws at runtime (e.g. the removed-loop-variable
// ReferenceError shipped on 2026-07-09). Pure node, safe on the login node.
"use strict";
const fs = require("fs");
const path = require("path");
const vm = require("vm");

const file = process.argv[2] ||
  path.join(__dirname, "atlas_built.html");
const html = fs.readFileSync(file, "utf8");

// ---- minimal DOM stub -------------------------------------------------
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
    this.onclick = null;
  }
  appendChild(c) { this.children.push(c); return c; }
  setAttribute(k, v) { this.attrs[k] = String(v); }
  getAttribute(k) { return k in this.attrs ? this.attrs[k] : null; }
  addEventListener() {}
}

// every id="..." in the static HTML gets a live element
const byId = {};
for (const m of html.matchAll(/id="([A-Za-z][\w-]*)"/g)) {
  if (!byId[m[1]]) byId[m[1]] = new El("div", m[1]);
}

function querySelector(sel) {
  let m;
  if ((m = sel.match(/^#([\w-]+)$/))) return byId[m[1]] || null;
  // '.cls[data-key="x"]' over script-created elements (switch buttons)
  if ((m = sel.match(/^\.[\w-]+\[data-key="(.+)"\]$/)))
    return created.find(e => e.dataset.key === m[1]) || null;
  return null;
}

const documentStub = {
  querySelector,
  querySelectorAll: () => [],   // only used for static-HTML buttons; skipping wires no handlers, which is fine
  createElement(tag) { const e = new El(tag); created.push(e); return e; },
  addEventListener() {},
};
const windowStub = { addEventListener() {} };

// ---- extract and run the inline script --------------------------------
const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map(m => m[1]);
if (!scripts.length) fail("no <script> block found in " + file);

const sandbox = {
  document: documentStub,
  window: windowStub,
  console,
  setTimeout: (fn) => fn && 0,   // resize debounce only; never fire
  clearTimeout: () => {},
  Math, JSON, Array, Object, Number, String, Date, isFinite, isNaN, parseFloat, parseInt,
};
sandbox.window.document = documentStub;
vm.createContext(sandbox);
try {
  for (const src of scripts) new vm.Script(src);          // syntax (≈ node --check)
  for (const src of scripts) vm.runInContext(src, sandbox, { filename: file });
} catch (err) {
  fail("script threw during initial render:\n" + err.stack);
}

// ---- exercise the interactions the stub can reach ----------------------
// Toggle every created switch button off and on; each click calls renderAll().
try {
  for (const b of created.filter(e => e.onclick)) { b.onclick(); b.onclick(); }
} catch (err) {
  fail("script threw during switch-toggle re-render:\n" + err.stack);
}

// ---- assertions ---------------------------------------------------------
const mustRenderSvg  = ["pvw", "spill", "etr", "frontChart"];
const mustBeNonEmpty = ["tiles", "pvwLegend", "spillLegend", "etrLegend", "frontLegend"];
const problems = [];
for (const id of mustRenderSvg) {
  const e = byId[id];
  if (!e) problems.push(`#${id}: container missing from HTML`);
  else if (!/<svg/i.test(e.innerHTML)) problems.push(`#${id}: no <svg> rendered (innerHTML ${e.innerHTML.length} chars)`);
}
for (const id of mustBeNonEmpty) {
  const e = byId[id];
  if (!e) problems.push(`#${id}: container missing from HTML`);
  else if (!(e.innerHTML.trim() || e.textContent.trim())) problems.push(`#${id}: rendered empty`);
}
const badge = byId.dataBadge && (byId.dataBadge.textContent || "").trim();
if (!badge) problems.push("#dataBadge: empty");
else if (/preview/i.test(badge)) problems.push("#dataBadge: still on placeholder data (" + badge + ")");

if (problems.length) fail(problems.join("\n"));
console.log(`OK ${path.basename(file)} — all chart containers rendered; badge: ${badge}`);

function fail(msg) { console.error("RENDER CHECK FAILED\n" + msg); process.exit(1); }

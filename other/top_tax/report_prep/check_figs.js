#!/usr/bin/env node
// Headless render check for the report figures: runs each page's script
// against a minimal DOM stub and asserts the SVGs fill with real marks.
//   node other/top_tax/report_prep/check_figs.js <fig.html> [...]
"use strict";
const fs = require("fs"), vm = require("vm");
let bad = 0;
for (const file of process.argv.slice(2)) {
  const html = fs.readFileSync(file, "utf8");
  const els = {};
  const el = id => els[id] || (els[id] = {
    id, attrs: {}, _html: "",
    set innerHTML(v){ this._html = v; }, get innerHTML(){ return this._html; },
    setAttribute(k, v){ this.attrs[k] = String(v); },
    getAttribute(k){ return this.attrs[k] ?? null; },
    addEventListener(){}, dataset: {}, style: {}
  });
  const doc = {
    getElementById: el,
    documentElement: { attributes: {}, addEventListener(){} },
    querySelectorAll: () => [],
    addEventListener(){}
  };
  const ctx = {
    document: doc,
    window: { matchMedia: () => ({ addEventListener(){}, matches: false }), addEventListener(){} },
    getComputedStyle: () => ({ getPropertyValue: () => "#888" }),
    MutationObserver: class { observe(){} },
    console
  };
  vm.createContext(ctx);
  const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map(m => m[1]);
  try { scripts.forEach(s => vm.runInContext(s, ctx)); }
  catch (e) { console.error(`FAIL ${file}: script threw: ${e.message}`); bad++; continue; }
  const svgs = Object.values(els).filter(e => e._html);
  const marks = svgs.reduce((n, s) =>
    n + (s._html.match(/<(circle|rect|polyline|line|path)\b/g) || []).length, 0);
  if (svgs.length === 0 || marks < 20) {
    console.error(`FAIL ${file}: svgs=${svgs.length} marks=${marks}`); bad++;
  } else {
    console.log(`OK ${file}: ${svgs.length} svg(s), ${marks} marks`);
  }
}
process.exit(bad ? 1 : 0);

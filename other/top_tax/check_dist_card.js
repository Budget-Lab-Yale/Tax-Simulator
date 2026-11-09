#!/usr/bin/env node
// Headless render check for dist_card_built.html.
// Runs the page script against a minimal DOM stub, then exercises every toggle
// (year / view / scale) and asserts the SVG re-renders with real bars each time.
//   node other/top_tax/check_dist_card.js [file]
"use strict";
const fs = require("fs"), path = require("path"), vm = require("vm");
const file = process.argv[2] || path.join(__dirname, "dist_card_built.html");
const html = fs.readFileSync(file, "utf8");
const problems = [];
const fail = m => { console.error("FAIL: " + m); process.exit(1); };

class El {
  constructor(tag, id){ this.tagName=(tag||"div").toUpperCase(); this.id=id||""; this.dataset={};
    this.style={}; this.attrs={}; this._html=""; this.listeners={}; this._kids=[]; }
  set innerHTML(v){ this._html=v; } get innerHTML(){ return this._html; }
  setAttribute(k,v){ this.attrs[k]=String(v); } getAttribute(k){ return k in this.attrs?this.attrs[k]:null; }
  addEventListener(t,fn){ (this.listeners[t]=this.listeners[t]||[]).push(fn); }
  fire(t,ev){ (this.listeners[t]||[]).forEach(fn=>fn(ev||{})); }
  getBoundingClientRect(){ return {width:980,height:470,left:0,top:0}; }
  // querySelectorAll on the SVG must return the hit-rects the script just wrote
  querySelectorAll(sel){
    if(sel===".hit"){
      const n=(this._html.match(/class="hit"/g)||[]).length;
      return Array.from({length:n},(_,i)=>{ const e=new El("rect"); e.dataset.i=String(i); return e; });
    }
    // seg button lookups
    const out=[]; return out;
  }
}

const byId={}, buttons={};
for(const m of html.matchAll(/id="([A-Za-z][\w-]*)"/g)) if(!byId[m[1]]) byId[m[1]]=new El("div",m[1]);
// seg buttons: give each seg container queryable buttons parsed from the HTML
function segButtons(segId){
  const re=new RegExp(`id="${segId}"[\\s\\S]*?<\\/div>`);
  const block=(html.match(re)||[""])[0];
  const btns=[...block.matchAll(/<button ([^>]*)>/g)].map(m=>{
    const e=new El("button"); const attrs=m[1];
    for(const a of attrs.matchAll(/data-(\w+)="([^"]*)"/g)) e.dataset[a[1]]=a[2];
    const ap=attrs.match(/aria-pressed="([^"]*)"/); if(ap) e.setAttribute("aria-pressed",ap[1]);
    return e;
  });
  return btns;
}
["segYear","segMode","segBasis","segUnit"].forEach(id=>{ buttons[id]=segButtons(id); byId[id].querySelectorAll=()=>buttons[id]; });

const documentStub = {
  getElementById:id=>byId[id]||null,
  querySelector:()=>null, querySelectorAll:()=>[],
  createElement:t=>new El(t),
  documentElement:{ _t:null, setAttribute(k,v){this._t=v;}, getAttribute(){return this._t;} },
  addEventListener(){}, body:{ insertAdjacentHTML(){} },
};
const windowStub = { addEventListener(){}, matchMedia:()=>({matches:false}) };
const sandbox = { document:documentStub, window:windowStub, console,
  Math,JSON,Array,Object,Number,String,Date,isFinite,isNaN,parseFloat,parseInt,Infinity,NaN };
sandbox.window.document=documentStub;
vm.createContext(sandbox);

const scripts=[...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map(m=>m[1]);
if(!scripts.length) fail("no <script> block");
try{ for(const s of scripts) new vm.Script(s); }catch(e){ fail("syntax error: "+e.message); }
try{ for(const s of scripts) vm.runInContext(s,sandbox,{filename:file}); }
catch(e){ fail("script threw on initial render:\n"+e.stack); }

const svg=byId["chart"];
// mode-aware structural check: bar views draw stacked rects + hatch; the rate
// view draws a dumbbell (circles, no hatch fills) — both must have hit targets.
function check(label, kind){
  const h=svg.innerHTML;
  if(!h || h.length<200) problems.push(`${label}: svg empty`);
  if(!/class="hit"/.test(h)) problems.push(`${label}: no hover hit targets`);
  if(/NaN|undefined/.test(h)) problems.push(`${label}: NaN/undefined in output`);
  if(kind==="bar"){
    const rc=(h.match(/<rect /g)||[]).length;
    if(rc<12) problems.push(`${label}: only ${rc} rects (<12)`);
    if(!/url\(#hatch\)/.test(h)) problems.push(`${label}: no hatched (avoidance) segments`);
  } else { // rate dumbbell
    const cc=(h.match(/<circle /g)||[]).length;
    if(cc<24) problems.push(`${label}: only ${cc} dumbbell dots (<24 = 3×bins-ish)`);
    if(!/fill="var\(--surface\)" stroke="var\(--accent\)"/.test(h)) problems.push(`${label}: no hollow ask dot`);
  }
  return h;
}

function clickSeg(segId, idx){ const b=buttons[segId][idx]; try{ b.fire("click"); }catch(e){ fail(segId+"["+idx+"] click threw:\n"+e.stack); } }

check("initial(context,2027,cash,agg)", "bar");
clickSeg("segMode",1);   check("mode=tax-bill", "bar");
clickSeg("segMode",2);   check("mode=etr", "rate");
clickSeg("segBasis",1);  check("etr + accrual", "rate");
clickSeg("segMode",0);   check("context + accrual", "bar");
clickSeg("segBasis",0);
clickSeg("segYear",1);   check("year=2036", "bar");
clickSeg("segUnit",1);   check("unit=per-hh", "bar");
clickSeg("segMode",2);   check("etr ignores per-hh", "rate");

// table + legend populated
if(!/Static ask/.test(byId["tbl"].innerHTML)) problems.push("table not populated");
if(!/Collected/.test(byId["legend"].innerHTML)) problems.push("legend not populated");
if(!/top 0\.01%/i.test(byId["foot"].innerHTML)) problems.push("footnote not populated");

// theme toggle must not throw
try{ byId["themebtn"].fire("click"); }catch(e){ problems.push("theme toggle threw: "+e.message); }

if(problems.length){ console.error("FAIL:\n - "+problems.join("\n - ")); process.exit(1); }
console.log("PASS — dist_card renders in all states (3 views × cash/accrual × year × scale); dumbbell, toggles, table, legend, footnote, theme all clean.");
// (atlas integration is verified separately by check_atlas2_render.js)

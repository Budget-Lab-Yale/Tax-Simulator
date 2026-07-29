#!/usr/bin/env python3
"""
report_metrics.py -- key metrics for the top-tax public report, computed from
top_tax_dials_30y_v5 (current physics, 2026-07-28: net-of-tax realization
form, on-model corporate rate to 35, estate-avoidance fix, death-gain
exclusion dial), kg_v5_revmax (current physics, direct death-regime
grid), and atlas2_data.json (surrogate over dials v5).

Pure stdlib (no pandas on login node). All revenue figures are $B,
scenario-minus-baseline, FY windows 2027-2036 ("10y") and 2027-2056 ("30y")
unless labeled otherwise. Prints a markdown report to stdout.

External constants (NOT model output) are marked EXTERNAL with their source.
"""
import csv, json, os, sys
from collections import OrderedDict

# NB: variable still named V2 for minimal edit surface; it now points at the v5 vintage.
V2   = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v5"
RVMX = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_v5_revmax"
ATLAS = "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/atlas2_data.json"

W10 = range(2027, 2037)
W30 = range(2027, 2057)

# EXTERNAL: CBO Feb 2026 baseline via CRFB -- cumulative FY2027-2036 deficits.
CUM_DEFICIT_10Y_B = 24_400.0   # $B
DEFICIT_2026_B    = 1_900.0    # $B (FY2026)
DEFICIT_2036_B    = 3_100.0    # $B (FY2036)

def read_rev(vintage, scenario, leg):
    """revenue_estimates.csv -> {year: delta $B}, NA rows dropped."""
    p = os.path.join(vintage, scenario, leg, "supplemental", "revenue_estimates.csv")
    out = {}
    with open(p) as f:
        for row in csv.DictReader(f):
            if row["total"] in ("NA", "", "NaN"):
                continue
            out[int(row["year"])] = float(row["total"])
    return out

def wsum(d, win):
    return sum(v for y, v in d.items() if y in win)

def read_receipts(vintage, scenario, leg):
    """totals/receipts.csv -> {year: {head: level $B}}"""
    p = os.path.join(vintage, scenario, leg, "totals", "receipts.csv")
    out = {}
    with open(p) as f:
        for row in csv.DictReader(f):
            y = int(row["year"])
            out[y] = {k: (float(v) if v not in ("NA", "") else 0.0)
                      for k, v in row.items() if k != "year"}
    return out

def head_deltas(scenario, leg, win):
    """receipts-by-head delta vs baseline, summed over win. $B.
    Baseline totals exist only under static/ (baseline conventional/totals is
    empty); receipts.csv starts FY2028 in this vintage (no CY2026 lead-in), so
    ledgers cover FY2028-2036."""
    base = read_receipts(V2, "baseline", "static")
    scen = read_receipts(V2, scenario, leg)
    heads = [k for k in next(iter(scen.values())) ]
    out = OrderedDict()
    for h in heads:
        out[h] = sum(scen[y][h] - base[y][h] for y in win if y in scen and y in base)
    return out

def fmt(x, nd=1):
    return f"{x:,.{nd}f}"

# ---------------------------------------------------------------- atlas load
with open(ATLAS) as f:
    atlas = json.load(f)
inc_lv  = atlas["income_levels"]           # {def: {group: $B}} 2027
etrb    = atlas["etr_base"]                # {def: {2027: {group: [8 comps pct]}}}
comps   = atlas["meta"]["etr_comps"]
sur     = atlas["surrogate"]
gdp_dec = atlas["meta"]["gdp_fy_decades"]  # $B cumulative FY GDP per decade

print("# Metrics for the top-tax report")
print()
print(f"*Source vintages: dials `{os.path.basename(V2)}` (current physics, run 2026-07-28: "
      "net-of-tax realization form eta_tilde=1.6625, sigma=0.16, on-model corporate rate to 35, "
      "uncapped CG rate, estate-avoidance fix, death-gain exclusion dial; deemed heir attribution "
      "is the full-ladder rank match, not comparable to the v3 smear), "
      f"revmax grid `{os.path.basename(RVMX)}` (same physics, direct death-regime grid to +30pp). "
      "All $ figures $B unless noted; windows FY2027-2036 and FY2027-2056.*")
print()

# =============================================================== 1. incomes
print("## 1. Income at the top, 2027 (baseline)")
print()
tot_c = sum(inc_lv["expanded"][g] for g in
            ["Negative income","Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5"])
tot_h = sum(inc_lv["hs"][g] for g in
            ["Negative income","Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5"])
print("| Group | Cash (expanded) $B | Share of cash income | Accrual (Haig-Simons) $B | Share of accrual income | Accrual/cash |")
print("|---|---|---|---|---|---|")
groups_show = ["Quintile 5", "Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"]
for g in groups_show:
    c, h = inc_lv["expanded"][g], inc_lv["hs"][g]
    print(f"| {g} | {fmt(c,0)} | {100*c/tot_c:.1f}% | {fmt(h,0)} | {100*h/tot_h:.1f}% | {h/c:.2f} |")
print()
print(f"- Total income, all tax units 2027: cash **${fmt(tot_c/1000,1)}T**, accrual **${fmt(tot_h/1000,1)}T** "
      "(shares are of these totals, which include the negative-income group).")
# bottom-X crossover for top 1%
cum = 0.0
target = inc_lv["expanded"]["Top 1%"]
labels = [("Q1",  "Quintile 1", 20), ("Q2", "Quintile 2", 40),
          ("Q3", "Quintile 3", 60), ("Q4", "Quintile 4", 80)]
prev_pct, prev_cum = 0, 0.0
for _, g, pct in labels:
    prev_cum = cum
    cum += inc_lv["expanded"][g]
    if cum >= target:
        # linear interpolation within the quintile
        frac = (target - prev_cum) / inc_lv["expanded"][g]
        xpct = prev_pct + frac * (pct - prev_pct)
        print(f"- Top 1% cash income (${fmt(target,0)}B) equals the combined cash income of "
              f"roughly the **bottom {xpct:.0f}%** of tax units (interp within quintiles).")
        break
    prev_pct = pct
cumh, target_h = 0.0, inc_lv["hs"]["Top 1%"]
prev_pct, prev_cum = 0, 0.0
for _, g, pct in labels:
    prev_cum = cumh
    cumh += inc_lv["hs"][g]
    if cumh >= target_h:
        frac = (target_h - prev_cum) / inc_lv["hs"][g]
        xpct = prev_pct + frac * (pct - prev_pct)
        print(f"- On an accrual basis the top 1% (${fmt(target_h,0)}B) matches roughly the "
              f"**bottom {xpct:.0f}%**.")
        break
    prev_pct = pct
print()

# ============================================== 2. baseline ETRs + taxes paid
print("## 2. Baseline federal taxes and ETRs by group, 2027")
print()
print("(taxes included: iit + payroll + estate + deemed + wealth + corp + vat; corp convention: capital_income)")
print()
print("| Group | Taxes $B | ETR cash | ETR accrual | of which: income tax | payroll | estate | corp |")
print("|---|---|---|---|---|---|---|---|")
for g in ["Quintile 3", "Quintile 4", "Quintile 5", "Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"]:
    v_c = etrb["expanded"]["2027"][g]; v_h = etrb["hs"]["2027"][g]
    etr_c, etr_h = sum(v_c), sum(v_h)
    taxes = etr_c / 100.0 * inc_lv["expanded"][g]
    i = {c: v_c[k] for k, c in enumerate(comps)}
    print(f"| {g} | {fmt(taxes,0)} | {etr_c:.1f}% | {etr_h:.1f}% | {i['income_tax']:.1f}% | "
          f"{i['payroll']:.1f}% | {i['estate']:.1f}% | {i['corp']:.1f}% |")
print()

# ================================= 3. thresholds + 2036 levels from CSV
print("## 3. Group thresholds and 2036 levels (from distribution_etrs baseline columns)")
print()
def read_detrs(scenario):
    p = os.path.join(V2, scenario, "static", "supplemental", "distribution_etrs.csv")
    rows = []
    with open(p) as f:
        for row in csv.DictReader(f):
            rows.append(row)
    return rows

detrs = read_detrs("stack_ref")
def slice_rows(rows, **kw):
    out = []
    for r in rows:
        if all(r[k] == v for k, v in kw.items()):
            out.append(r)
    return out

base_slice = slice_rows(detrs, reform_leg="static", ranking="fixed",
                        taxes_included="wealth_cit_vat", corp_convention="capital_income",
                        group_dimension="Income percentile")
print("| Group | 2027 income floor $ | 2027 cash income $B | 2036 cash income $B | 2027 taxes $B | 2036 taxes $B | n (2027, M) |")
print("|---|---|---|---|---|---|---|")
lv = {}
for g in ["Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"]:
    r27 = [r for r in base_slice if r["group"] == g and r["year"] == "2027" and r["income_definition"] == "expanded"][0]
    r36 = [r for r in base_slice if r["group"] == g and r["year"] == "2036" and r["income_definition"] == "expanded"][0]
    lv[g] = dict(inc27=float(r27["income_baseline"]), inc36=float(r36["income_baseline"]),
                 tax27=float(r27["tax_baseline"]),   tax36=float(r36["tax_baseline"]),
                 cut27=float(r27["income_cutoff"]),  n27=float(r27["n_tax_units"]))
    print(f"| {g} | {fmt(lv[g]['cut27'],0)} | {fmt(lv[g]['inc27'],0)} | {fmt(lv[g]['inc36'],0)} | "
          f"{fmt(lv[g]['tax27'],0)} | {fmt(lv[g]['tax36'],0)} | {lv[g]['n27']/1e6:.2f} |")
print()
r27h = [r for r in base_slice if r["group"] == "Top 1%" and r["year"] == "2027" and r["income_definition"] == "hs"][0]
r36h = [r for r in base_slice if r["group"] == "Top 1%" and r["year"] == "2036" and r["income_definition"] == "hs"][0]
print(f"- Top 1% accrual income: 2027 **${fmt(float(r27h['income_baseline']),0)}B**, "
      f"2036 **${fmt(float(r36h['income_baseline']),0)}B**.")
print()

# =========================================== 4. stylized doubling calculation
print("## 4. Stylized calculation: doubling the top 1% effective rate")
print()
g = "Top 1%"
inc27, inc36 = lv[g]["inc27"], lv[g]["inc36"]
tax27, tax36 = lv[g]["tax27"], lv[g]["tax36"]
grow_i = (inc36 / inc27) ** (1/9)
grow_t = (tax36 / tax27) ** (1/9)
inc10 = sum(inc27 * grow_i**k for k in range(10))
tax10 = sum(tax27 * grow_t**k for k in range(10))
etr27 = tax27 / inc27
print(f"- Top 1% baseline cash ETR 2027 (this tax set): **{100*etr27:.1f}%**; taxes 2027 ${fmt(tax27,0)}B on ${fmt(inc27,0)}B income.")
print(f"- 10-year (FY2027-36) top 1% cash income, geometric interp 2027->2036: **${fmt(inc10/1000,1)}T**; taxes: ${fmt(tax10/1000,1)}T.")
print(f"- Doubling the ETR ({100*etr27:.1f}% -> {200*etr27:.1f}%) with a FROZEN base adds revenue equal to "
      f"baseline taxes: **+${fmt(tax10/1000,1)}T over 10 years**.")
print(f"- EXTERNAL yardstick: cumulative FY2027-2036 deficits **${fmt(CUM_DEFICIT_10Y_B/1000,1)}T** "
      f"(CBO Feb 2026 baseline, via CRFB).")
print(f"- So the frozen-base doubling closes **{100*tax10/CUM_DEFICIT_10Y_B:.0f}%** of the 10-year deficit.")
print(f"- Same calc on ACCRUAL ETR (2027 accrual ETR "
      f"{100*tax27/float(r27h['income_baseline']):.1f}%): doubling that rate on accrual income is the same +${fmt(tax10/1000,1)}T.")
print()

# ============================================ 5. solo lever scores (direct)
print("## 5. Standalone lever scores (direct runs, dials v3)")
print()
solos = [
    ("s_ord_r39p6",       "Top ordinary rate 37% -> 39.6%"),
    ("s_ord_r44p8",       "Top ordinary rate 37% -> 44.8% (REF)"),
    ("s_ord_r50",         "Top ordinary rate 37% -> 50%"),
    ("s_cg_r25",          "CG & div top rate 20% -> 25%"),
    ("s_cg_r30",          "CG & div top rate 20% -> 30%"),
    ("s_cg_r35",          "CG & div top rate 20% -> 35%"),
    ("s_cg_r40",          "CG & div top rate 20% -> 40% (REF)"),
    ("s_cg_r45",          "CG & div top rate 20% -> 45%"),
    ("s_cg_r50",          "CG & div top rate 20% -> 50%"),
    ("s_corp_r24p5",      "Corporate rate 21% -> 24.5%"),
    ("s_corp_r28",        "Corporate rate 21% -> 28% (REF)"),
    ("s_corp_r31p5",      "Corporate rate 21% -> 31.5%"),
    ("s_corp_r35",        "Corporate rate 21% -> 35%"),
    ("s_wealth_r1_t50",   "Wealth tax 1% > $50M"),
    ("s_wealth_r2_t50",   "Wealth tax 2% > $50M (REF)"),
    ("s_deemed_carryover","Carryover basis at death (no rate change)"),
    ("s_deemed_deemed",   "Deemed realization at death (no rate change)"),
    ("s_estate_r50_e8p46","Estate tax 40->50%, exemption $15.4M->$8.46M (REF)"),
    ("s_estate_r60_e5",   "Estate tax 40->60%, exemption -> $5M"),
    ("s_qbi_on",          "Repeal QBI (199A)"),
    ("s_taxmax_on",       "Eliminate SS taxable max"),
]
print("| Lever | 10y static | 10y conv | conv/static | 30y conv |")
print("|---|---|---|---|---|")
solo_res = {}
for sid, label in solos:
    try:
        st = read_rev(V2, sid, "static"); cv = read_rev(V2, sid, "conventional")
    except FileNotFoundError:
        print(f"| {label} | (missing: {sid}) | | | |")
        continue
    s10, c10, c30 = wsum(st, W10), wsum(cv, W10), wsum(cv, W30)
    solo_res[sid] = dict(s10=s10, c10=c10, s30=wsum(st, W30), c30=c30)
    ratio = c10/s10 if abs(s10) > 1e-9 else float("nan")
    print(f"| {label} | {fmt(s10,0)} | {fmt(c10,0)} | {ratio:.2f} | {fmt(c30,0)} |")
print()

# ==================================== 6. destination ledgers (receipts heads)
print("## 6. Destination ledgers -- receipts-by-head deltas, FY2028-2036 conventional ($B)")
print()
print("(receipts.csv starts FY2028 in this vintage -- no CY2026 lead-in -- so these ledgers "
      "cover 9 of the 10 window years; use for composition/direction, not window totals)")
print()
heads_pretty = OrderedDict([
    ("revenues_income_tax",  "income tax"),
    ("revenues_payroll_tax", "payroll"),
    ("revenues_corp_tax",    "corporate"),
    ("revenues_estate_tax",  "estate"),
    ("revenues_wealth_tax",  "wealth"),
    ("outlays_tax_credits",  "credits (outlay)"),
])
feature = ["s_cg_r40", "s_cg_r25", "s_corp_r28", "s_wealth_r2_t50", "s_wealth_r1_t50",
           "s_deemed_deemed", "s_ord_r44p8", "s_estate_r50_e8p46", "s_taxmax_on", "stack_ref"]
print("| Scenario | " + " | ".join(heads_pretty.values()) + " | TOTAL |")
print("|---|" + "---|" * (len(heads_pretty) + 1))
for sid in feature:
    try:
        hd = head_deltas(sid, "conventional", W10)
    except FileNotFoundError:
        continue
    tot = sum(v for k, v in hd.items() if k.startswith("revenues")) - hd.get("outlays_tax_credits", 0.0)
    cells = " | ".join(fmt(hd.get(k, 0.0), 0) for k in heads_pretty)
    print(f"| {sid} | {cells} | {fmt(tot,0)} |")
print()
print("(Note: 'TOTAL' here = sum of revenue heads minus credit outlays; small differences vs "
      "revenue_estimates.csv reflect the FY booking conventions inside calc_receipts.)")
print()

# ============================================== 7. parts vs whole exhibits
print("## 7. Parts-vs-whole (naive sum vs actual package)")
print()

def pvw(label, part_ids, whole_id):
    ps10 = sum(solo_res[p]["s10"] for p in part_ids)
    pc10 = sum(solo_res[p]["c10"] for p in part_ids)
    st = read_rev(V2, whole_id, "static"); cv = read_rev(V2, whole_id, "conventional")
    ws10, wc10 = wsum(st, W10), wsum(cv, W10)
    pc30 = sum(solo_res[p]["c30"] for p in part_ids)
    wc30 = wsum(cv, W30)
    print(f"**{label}** (10y unless noted)")
    print()
    print(f"- Sum of standalone STATIC scores:        ${fmt(ps10,0)}B")
    print(f"- Sum of standalone CONVENTIONAL scores:  ${fmt(pc10,0)}B")
    print(f"- Package STATIC:                         ${fmt(ws10,0)}B")
    print(f"- Package CONVENTIONAL (the truth):       ${fmt(wc10,0)}B")
    print(f"- Package interaction (pkg conv - sum conv): ${fmt(wc10-pc10,0)}B "
          f"({100*(wc10-pc10)/pc10:+.1f}% of the naive conventional sum)")
    print(f"- Behavioral survival: naive {100*pc10/ps10:.0f}%, package {100*wc10/ws10:.0f}%")
    print(f"- 30y: naive conv ${fmt(pc30,0)}B vs package conv ${fmt(wc30,0)}B "
          f"({100*(wc30-pc30)/pc30:+.1f}%)")
    print()

for p in ["pr_cg_corp", "pr_cg_deemed", "prco_cg_deemed", "pc_corpr35_cgr30"]:
    pass  # direct pair availability noted below

# 7a. full stack vs 8 solos
REF_SOLOS = ["s_ord_r44p8", "s_cg_r40", "s_corp_r28", "s_wealth_r2_t50",
             "s_deemed_deemed", "s_estate_r50_e8p46", "s_qbi_on", "s_taxmax_on"]
pvw("Full 8-lever reference stack (stack_ref) vs sum of its 8 solos", REF_SOLOS, "stack_ref")

# 7b. corp28 + cg30 package (the closest direct analogue of the draft's Figure 3)
pvw("Corporate 21->35% + CG/div 20->30% (pc_corpr35_cgr30)",
    ["s_corp_r35", "s_cg_r30"], "pc_corpr35_cgr30")

# 7c. cg40 x deemed (crown pair, both at REF)
pvw("CG/div 20->40% + deemed realization at death (pr_cg_deemed)",
    ["s_cg_r40", "s_deemed_deemed"], "pr_cg_deemed")

# 7d. surrogate-style estimate of the draft's literal Figure 3: everything to 25%
print("**Draft Figure 3 as written -- corporate AND CG/div all to 25% (surrogate-style estimate; no direct run)**")
print()
c25_s10 = solo_res["s_cg_r25"]["s10"]; c25_c10 = solo_res["s_cg_r25"]["c10"]
corp_frac = (25.0 - 21.0) / (28.0 - 21.0)   # corp dial linear in delta-tau by construction
k25_s10 = corp_frac * solo_res["s_corp_r28"]["s10"]
k25_c10 = corp_frac * solo_res["s_corp_r28"]["c10"]
naive_s, naive_c = c25_s10 + k25_s10, c25_c10 + k25_c10
res = sur["pairs"].get("cg|corp", {})
inter10 = 0.0
if "ct" in res:
    g_cg = sur["g"]["cg"][1][0]     # cg knot index 1 = 25%, decade 1
    inter10 = g_cg * corp_frac * res["ct"][0]
print(f"- Sum of standalone static:       ${fmt(naive_s,0)}B  (cg25 {fmt(c25_s10,0)} + corp25 {fmt(k25_s10,0)}, corp linear-scaled 4/7 of corp28)")
print(f"- Sum of standalone conventional: ${fmt(naive_c,0)}B  (cg25 {fmt(c25_c10,0)} + corp25 {fmt(k25_c10,0)})")
print(f"- Package conventional (surrogate: sum + g-scaled cg|corp interaction {fmt(inter10,1)}): "
      f"**${fmt(naive_c + inter10,0)}B**")
print(f"- Surrogate validation context: quiz max err 2.5%, corners 2.4% -- treat as +/- a few percent.")
print()

# 7e. draft Figure 4: cg 20->25 + deemed realization
print("**Draft Figure 4 -- CG/div 20->25% + deemed realization at death**")
print()
d_s10, d_c10 = solo_res["s_deemed_deemed"]["s10"], solo_res["s_deemed_deemed"]["c10"]
naive_s4, naive_c4 = c25_s10 + d_s10, c25_c10 + d_c10
res_cd = sur["pairs"].get("cg|deemed", {})
inter4 = 0.0
if "byPos" in res_cd and "deemed" in res_cd["byPos"] and "ct" in res_cd["byPos"]["deemed"]:
    g_cg = sur["g"]["cg"][1][0]
    inter4 = g_cg * res_cd["byPos"]["deemed"]["ct"][0]
elif "ct" in res_cd:
    g_cg = sur["g"]["cg"][1][0]
    inter4 = g_cg * res_cd["ct"][0]
print(f"- Current-physics (dials v3, surrogate composition at cg=25):")
print(f"  - Sum of standalone static:       ${fmt(naive_s4,0)}B   (cg25 {fmt(c25_s10,0)} + deemed {fmt(d_s10,0)})")
print(f"  - Sum of standalone conventional: ${fmt(naive_c4,0)}B   (cg25 {fmt(c25_c10,0)} + deemed {fmt(d_c10,0)})")
print(f"  - Package conventional estimate:  **${fmt(naive_c4 + inter4,0)}B** (interaction {fmt(inter4,1)})")
try:
    st_su  = read_rev(RVMX, "cg_05pp_stepup", "static");  cv_su = read_rev(RVMX, "cg_05pp_stepup", "conventional")
    st_d0  = read_rev(RVMX, "cg_00pp_deemed", "static");  cv_d0 = read_rev(RVMX, "cg_00pp_deemed", "conventional")
    st_d5  = read_rev(RVMX, "cg_05pp_deemed", "static");  cv_d5 = read_rev(RVMX, "cg_05pp_deemed", "conventional")
    print(f"- Direct runs (kg_v5_revmax, current physics), 10y:")
    print(f"  - Parts static:  cg+5pp {fmt(wsum(st_su,W10),0)} + deemed {fmt(wsum(st_d0,W10),0)} = "
          f"${fmt(wsum(st_su,W10)+wsum(st_d0,W10),0)}B")
    print(f"  - Parts conv:    cg+5pp {fmt(wsum(cv_su,W10),0)} + deemed {fmt(wsum(cv_d0,W10),0)} = "
          f"${fmt(wsum(cv_su,W10)+wsum(cv_d0,W10),0)}B")
    print(f"  - Package static ${fmt(wsum(st_d5,W10),0)}B; package conv **${fmt(wsum(cv_d5,W10),0)}B**")
except FileNotFoundError as e:
    print(f"  (revmax direct runs unavailable: {e})")
print()

# ================================================== 8. Laffer curves
print("## 8. Capital-gains Laffer curves by death regime")
print()
print("### 8a. Current physics (dials v5, uncapped CG): step-up direct; deemed/carryover via surrogate composition")
print()
knots = [25, 30, 35, 40, 45, 50]
print("| Top CG rate | Step-up 10y conv | Deemed-conditional 10y conv delta* | Carryover-conditional* | Step-up 30y conv |")
print("|---|---|---|---|---|")
for i, k in enumerate(knots):
    sid = f"s_cg_r{k}"
    c10 = solo_res[sid]["c10"]; c30 = solo_res[sid]["c30"]
    row_idx = i + 1  # solo grid row 0 = off (20%)
    extra = {}
    for pos in ("deemed", "carryover"):
        r = sur["pairs"].get("cg|deemed", {})
        inter = None
        if "byPos" in r and pos in r["byPos"] and "ct" in r["byPos"][pos]:
            g_cg = sur["g"]["cg"][row_idx][0]
            inter = g_cg * r["byPos"][pos]["ct"][0]
        if inter is not None:
            base_pos = solo_res["s_deemed_deemed" if pos == "deemed" else "s_deemed_carryover"]["c10"]
            extra[pos] = c10 + inter  # cg-rate marginal revenue conditional on regime (excl. the regime's own base revenue)
        else:
            extra[pos] = float("nan")
    print(f"| {k}% | {fmt(c10,0)} | {fmt(extra['deemed'],0)} | {fmt(extra['carryover'],0)} | {fmt(c30,0)} |")
print()
print("*Conditional columns = the CG-rate increase's own 10y conventional yield when the death regime "
      "is already in place (solo cg + g-scaled cg|deemed interaction residual; the regime's own "
      "revenue is NOT included). Surrogate-composed -- +/- a few percent.*")
print()

print("### 8b. Direct grid (kg_v5_revmax, current physics): total 10y conventional by cell")
print()
print("| Top CG rate | Step-up | Carryover | Deemed | Step-up static | Deemed static |")
print("|---|---|---|---|---|---|")
for pp in [0, 5, 10, 15, 20, 25]:
    rate = 20 + pp
    row = [f"| {rate}%"]
    for reg in ["stepup", "carryover", "deemed"]:
        sid = f"cg_{pp:02d}pp_{reg}"
        if pp == 0 and reg == "stepup":
            row.append("0")
            continue
        try:
            cv = read_rev(RVMX, sid, "conventional")
            row.append(fmt(wsum(cv, W10), 0))
        except FileNotFoundError:
            row.append("--")
    for reg in ["stepup", "deemed"]:
        sid = f"cg_{pp:02d}pp_{reg}"
        if pp == 0 and reg == "stepup":
            row.append("0")
            continue
        try:
            st = read_rev(RVMX, sid, "static")
            row.append(fmt(wsum(st, W10), 0))
        except FileNotFoundError:
            row.append("--")
    print(" | ".join(row) + " |")
print()
print("Leakage sign check (static minus conventional, 10y): positive = behavior loses revenue.")
for sid, lab in [("cg_05pp_stepup", "cg+5pp under step-up"),
                 ("cg_00pp_deemed", "deemed alone"),
                 ("cg_05pp_deemed", "cg+5pp + deemed package")]:
    try:
        st = read_rev(RVMX, sid, "static"); cv = read_rev(RVMX, sid, "conventional")
        print(f"- {lab}: static {fmt(wsum(st,W10),0)}, conv {fmt(wsum(cv,W10),0)}, "
              f"leakage {fmt(wsum(st,W10)-wsum(cv,W10),0)}")
    except FileNotFoundError:
        pass
print()

# ============================= 9. ask vs collected ETRs for the stack
print("## 9. 'Ask' vs 'collected' ETRs under the full reference stack (stack_ref, 2027)")
print()
print("(accrual = Haig-Simons denominator; cash = expanded income; taxes wealth_cit_vat; corp capital_income)")
print()
print("| Group | Baseline | Static 'ask' | Conventional 'collected' | Avoidance margin (pp) |")
print("|---|---|---|---|---|")
for idef, lbl in [("hs", "accrual"), ("expanded", "cash")]:
    print(f"| **{lbl} denominator** | | | | |")
    for g in ["Quintile 5", "Top 1%", "Top 0.1%", "Top 0.01%"]:
        rs = [r for r in detrs if r["group"] == g and r["year"] == "2027"
              and r["income_definition"] == idef and r["ranking"] == "fixed"
              and r["taxes_included"] == "wealth_cit_vat" and r["corp_convention"] == "capital_income"
              and r["group_dimension"] == "Income percentile"]
        ask = [r for r in rs if r["reform_leg"] == "static"]
        col = [r for r in rs if r["reform_leg"] == "conventional"]
        if not ask or not col:
            continue
        b = 100 * float(ask[0]["etr_baseline"])
        a = 100 * float(ask[0]["etr_reform"])
        c = 100 * float(col[0]["etr_reform"])
        print(f"| {g} | {b:.1f}% | {a:.1f}% | {c:.1f}% | {a-c:.1f} |")
print()

# ====================== 10. decade survival trend for the stack + GDP shares
print("## 10. Decade profile of the full stack")
print()
st = read_rev(V2, "stack_ref", "static"); cv = read_rev(V2, "stack_ref", "conventional")
decs = [(2027, 2036), (2037, 2046), (2047, 2056)]
print("| Decade | Static $B | Conv $B | Survival | Conv % of GDP |")
print("|---|---|---|---|---|")
for i, (a, b) in enumerate(decs):
    w = range(a, b + 1)
    s, c = wsum(st, w), wsum(cv, w)
    print(f"| {a}-{b} | {fmt(s,0)} | {fmt(c,0)} | {100*c/s:.0f}% | {100*c/gdp_dec[i]:.2f}% |")
print()

# ============================= 11. behavioral color from kg_dynamics_summary
print("## 11. Realization-model color (kg_dynamics_summary)")
print()
def kg_summary_col(scenario, cols):
    p = os.path.join(V2, scenario, "conventional", "supplemental", "kg_dynamics_summary.csv")
    out = {}
    try:
        with open(p) as f:
            rows = list(csv.DictReader(f))
    except FileNotFoundError:
        return None
    for c in cols:
        if rows and c in rows[0]:
            vals = [(int(r["year"]), float(r[c])) for r in rows if r.get(c) not in (None, "", "NA", "NaN")]
            out[c] = vals
    return out

for sid in ["s_cg_r40", "pr_cg_deemed", "stack_ref"]:
    res = kg_summary_col(sid, ["semi_elast_implied", "deemed_realized", "taxable_death_stock"])
    if not res:
        print(f"- {sid}: kg_dynamics_summary.csv not found or empty")
        continue
    parts = [f"**{sid}**:"]
    if "semi_elast_implied" in res and res["semi_elast_implied"]:
        v = [x for _, x in res["semi_elast_implied"]]
        parts.append(f"implied realization semi-elasticity mean {sum(v)/len(v):.2f} "
                     f"(range {min(v):.2f} to {max(v):.2f})")
    if "deemed_realized" in res and res["deemed_realized"]:
        v10 = sum(x for y, x in res["deemed_realized"] if y in W10)
        parts.append(f"gains deemed-realized at death 10y {fmt(v10/1e9,0)}B" if abs(v10) > 1e12 else
                     f"deemed_realized 10y {fmt(v10,1)} (units as stored)")
    print("- " + " ".join(parts))
print()
print("*(Interpretation note: the implied semi-elasticity is an OUTPUT here -- compare it across "
      "regimes to quantify 'policy chooses its own elasticity'.)*")

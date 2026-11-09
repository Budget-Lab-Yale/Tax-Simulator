#!/usr/bin/env python3
"""
accrual_variance_check.py -- what happens to self-ranked top-group accrual
(Haig-Simons) income shares if we crudely inject return variance into the
imputed accruals?

Construction (CRUDE, for report prep -- not a Tax-Data change):
  - Rebuild HS income per record: expanded_inc - realized gains + accruals_sum
    - txbl_ira_dist - gross_pens_dist * dc_share, with dc_share approximated
    as value.dc / (value.dc + value.db). Inheritance is NOT added to the rank
    (the production self-ranking adds it), so validate the no-noise shares
    against the official self-ranked numbers before reading the noisy ones.
  - Noise: one standard-normal draw z per record (a common portfolio shock),
    added to accruals as z * (sigma_a * value_a) summed over shocked classes.
    Mean-zero by construction; sigmas are round numbers, stated below.
  - Re-rank by the noisy measure, sum the noisy measure by top group, average
    shares over K seeded draws.

Sigmas (annual return sd, crude): equities 0.25, pass-throughs 0.25,
real estate (homes + re_fund) 0.10, dc 0.12, trusts 0.15.
"""
import csv, math, random

DETAIL  = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v2/baseline/static/detail/2027.csv"
TAXDATA = "/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026070814/baseline/tax_units_2027.csv"
K_DRAWS = 20
SEED    = 20260717

SIG = { "eq": 0.25, "pt": 0.25, "re": 0.10, "dc": 0.12, "tr": 0.15 }
ACCRUAL_COLS = ["accruals.equities", "accruals.pass_throughs", "accruals.primary_home",
                "accruals.other_home", "accruals.re_fund", "accruals.dc", "accruals.trusts"]

def f(row, k):
    v = row.get(k, "0")
    return float(v) if v not in ("", "NA", "NaN") else 0.0

# ---- read sim detail: id, weight, expanded_inc, realized gains
det = {}
with open(DETAIL) as fh:
    for r in csv.DictReader(fh):
        det[r["id"]] = (f(r, "weight"), f(r, "expanded_inc"), f(r, "kg_st") + f(r, "kg_lt"))

# ---- read Tax-Data: accruals, values, HS adjustment fields
recs = []
with open(TAXDATA) as fh:
    for r in csv.DictReader(fh):
        d = det.get(r["id"])
        if d is None:
            continue
        w, exp_inc, kg = d
        acc = sum(f(r, c) for c in ACCRUAL_COLS)
        vdc, vdb = f(r, "value.dc"), f(r, "value.db")
        dc_share = vdc / (vdc + vdb) if (vdc + vdb) > 0 else 0.0
        hs = (exp_inc - kg - f(r, "other_gains") + acc
              - f(r, "txbl_ira_dist") - f(r, "gross_pens_dist") * dc_share)
        vol = (SIG["eq"] * f(r, "value.equities")
               + SIG["pt"] * f(r, "value.pass_throughs")
               + SIG["re"] * (f(r, "value.primary_home") + f(r, "value.other_home") + f(r, "value.re_fund"))
               + SIG["dc"] * f(r, "value.dc")
               + SIG["tr"] * f(r, "value.trusts"))
        recs.append((w, hs, vol))
print(f"joined records: {len(recs):,} (detail rows: {len(det):,})")

GROUPS = [("Top 20%", 0.20), ("Top 10%", 0.10), ("Top 5%", 0.05),
          ("Top 1%", 0.01), ("Top 0.1%", 0.001), ("Top 0.01%", 0.0001)]

def top_shares(vals):
    """vals: list of (weight, measure). Rank by measure, return {group: (total$B, share)}."""
    total = sum(w * m for w, m in vals)
    W = sum(w for w, _ in vals)
    s = sorted(vals, key=lambda t: -t[1])
    out, cum_w, cum_m, gi = {}, 0.0, 0.0, 0
    for w, m in s:
        cum_w += w; cum_m += w * m
        while gi < len(GROUPS) and cum_w >= W * GROUPS[gi][1]:
            out[GROUPS[gi][0]] = (cum_m / 1e9, cum_m / total)
            gi += 1
        if gi >= len(GROUPS):
            break
    # note GROUPS must be ordered smallest-share last; re-order results
    return out, total / 1e9

# GROUPS ordered largest to smallest cutoffs? cutoffs .2 > .1 > ... crossing order:
# cum_w grows, so smallest share crossed first -> reorder GROUPS ascending
GROUPS = sorted(GROUPS, key=lambda g: g[1])

base = [(w, hs) for w, hs, _ in recs]
res0, tot0 = top_shares(base)
print(f"\nNo-noise crude HS (validate vs official self-ranked): total ${tot0:,.0f}B")
for g, _ in sorted(GROUPS, key=lambda g: -g[1]):
    t, s = res0[g]
    print(f"  {g:>9}: ${t:,.0f}B  {100*s:.1f}%")

rng = random.Random(SEED)
acc_res = {g: [] for g, _ in GROUPS}
acc_tot = []
for k in range(K_DRAWS):
    noisy = [(w, hs + rng.gauss(0.0, 1.0) * vol) for w, hs, vol in recs]
    resk, totk = top_shares(noisy)
    acc_tot.append(totk)
    for g, _ in GROUPS:
        acc_res[g].append(resk[g])

print(f"\nNoisy HS, {K_DRAWS} draws (mean [min..max]); total ${sum(acc_tot)/K_DRAWS:,.0f}B avg")
for g, _ in sorted(GROUPS, key=lambda g: -g[1]):
    ts = [t for t, _ in acc_res[g]]; ss = [s for _, s in acc_res[g]]
    print(f"  {g:>9}: ${sum(ts)/K_DRAWS:,.0f}B  {100*sum(ss)/K_DRAWS:.1f}%  "
          f"[{100*min(ss):.1f}%..{100*max(ss):.1f}%]")

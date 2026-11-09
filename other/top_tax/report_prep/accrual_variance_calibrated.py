#!/usr/bin/env python3
"""
accrual_variance_calibrated.py -- self-ranked top shares of one-year accrual
(Haig-Simons) income in a "regular year": the market return is held at its
mean (aggregate accruals pinned EXACTLY at the deterministic total, per class,
per draw), and households draw idiosyncratic returns around the class mean.

Idiosyncratic structure (ballpark anchors, stated so they can be argued with):
  - Public equity is split into a directly-held (concentrated) slice and a
    fund (diversified) slice, with the direct share rising in the size of the
    equity position -- an SCF-gradient stand-in -- and 100% for billion-dollar
    positions (the Forbes records are identified single-company stakes):
        equity < $100k: 15% direct | <$1M: 25% | <$10M: 40% | <$100M: 60%
        | <$1B: 80% | >=$1B: 100%
    Direct slice draws sigma = 0.35 (single-stock idiosyncratic vol, Campbell-
    Lettau-Malkiel-Xu range); fund slice sigma = 0.04.
  - Pass-through business: sigma = 0.30 (dispersion of private business
    returns, Fagereng et al. / SZZ ballpark).
  - Homes: sigma = 0.10 (individual-house idiosyncratic vol, Giacoletti-type
    estimates); re_fund 0.08; DC 0.04; trusts 0.04.
  - Draws independent across classes within a record (conservative at the very
    top, where a founder's public and private stakes are the same firm).
  - Each class's weighted mean shock is subtracted each draw, so class-level
    aggregate accruals match the deterministic totals exactly: one market,
    no level variation, allocation only.

HS reconstruction and ranking conventions as in accrual_variance_check.py
(no inheritance in the rank; dc_share = value.dc/(value.dc+value.db)); the
no-noise shares validate against the official self-ranked numbers.
"""
import csv, random

DETAIL  = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v2/baseline/static/detail/2027.csv"
TAXDATA = "/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026070814/baseline/tax_units_2027.csv"
K_DRAWS = 20
SEED    = 20260717

def f(row, k):
    v = row.get(k, "0")
    return float(v) if v not in ("", "NA", "NaN") else 0.0

def direct_share(eq):
    if eq >= 1e9:  return 1.00
    if eq >= 1e8:  return 0.80
    if eq >= 1e7:  return 0.60
    if eq >= 1e6:  return 0.40
    if eq >= 1e5:  return 0.25
    return 0.15

det = {}
with open(DETAIL) as fh:
    for r in csv.DictReader(fh):
        det[r["id"]] = (f(r, "weight"), f(r, "expanded_inc"), f(r, "kg_st") + f(r, "kg_lt"))

ACC = ["accruals.equities", "accruals.pass_throughs", "accruals.primary_home",
       "accruals.other_home", "accruals.re_fund", "accruals.dc", "accruals.trusts"]

# vol classes: (label, sigma, value expression)
recs = []   # (w, hs_base, [vol_amounts per class])
with open(TAXDATA) as fh:
    for r in csv.DictReader(fh):
        d = det.get(r["id"])
        if d is None:
            continue
        w, exp_inc, kg = d
        acc = sum(f(r, c) for c in ACC)
        vdc, vdb = f(r, "value.dc"), f(r, "value.db")
        dcs = vdc / (vdc + vdb) if (vdc + vdb) > 0 else 0.0
        hs = (exp_inc - kg - f(r, "other_gains") + acc
              - f(r, "txbl_ira_dist") - f(r, "gross_pens_dist") * dcs)
        eq = f(r, "value.equities"); ds = direct_share(eq)
        vols = [
            0.35 * eq * ds,                 # direct equity
            0.04 * eq * (1 - ds),           # fund equity
            0.30 * f(r, "value.pass_throughs"),
            0.10 * (f(r, "value.primary_home") + f(r, "value.other_home")),
            0.08 * f(r, "value.re_fund"),
            0.04 * vdc,
            0.04 * f(r, "value.trusts"),
        ]
        recs.append((w, hs, vols))
n_cls = 7
print(f"joined records: {len(recs):,}")

GROUPS = sorted([("Top 20%", 0.20), ("Top 10%", 0.10), ("Top 5%", 0.05),
                 ("Top 1%", 0.01), ("Top 0.1%", 0.001), ("Top 0.01%", 0.0001)],
                key=lambda g: g[1])

def top_stats(vals, marks=None):
    """vals: (weight, measure, mark). Returns {grp: (total$B, share, overlap)}."""
    total = sum(w * m for w, m, _ in vals)
    W = sum(w for w, _, _ in vals)
    s = sorted(vals, key=lambda t: -t[1])
    out, cum_w, cum_m, cum_mark, gi = {}, 0.0, 0.0, 0.0, 0
    for w, m, mk in s:
        cum_w += w; cum_m += w * m; cum_mark += w * mk
        while gi < len(GROUPS) and cum_w >= W * GROUPS[gi][1]:
            out[GROUPS[gi][0]] = (cum_m / 1e9, cum_m / total, cum_mark / cum_w)
            gi += 1
        if gi >= len(GROUPS):
            break
    return out, total / 1e9

# deterministic baseline + membership marks for overlap diagnostics
base_vals = [(w, hs, 0.0) for w, hs, _ in recs]
res0, tot0 = top_stats(base_vals)
print(f"\nNo-noise crude HS (validation): total ${tot0:,.0f}B")
for g, _ in sorted(GROUPS, key=lambda g: -g[1]):
    t, s, _ = res0[g]
    print(f"  {g:>9}: ${t:,.0f}B  {100*s:.1f}%")

# mark records in the deterministic top 1%
W_all = sum(w for w, _, _ in recs)
order = sorted(range(len(recs)), key=lambda i: -recs[i][1])
mark = [0.0] * len(recs)
cw = 0.0
for i in order:
    if cw >= 0.01 * W_all:
        break
    mark[i] = 1.0
    cw += recs[i][0]

rng = random.Random(SEED)
acc = {g: [] for g, _ in GROUPS}
tots = []
for k in range(K_DRAWS):
    # per-class draws with exact weighted demeaning (single market)
    shocks = [[rng.gauss(0.0, 1.0) for _ in range(n_cls)] for _ in recs]
    for c in range(n_cls):
        num = sum(recs[i][0] * shocks[i][c] * recs[i][2][c] for i in range(len(recs)))
        den = sum(recs[i][0] * recs[i][2][c] for i in range(len(recs)))
        adj = num / den if den > 0 else 0.0
        for i in range(len(recs)):
            shocks[i][c] -= adj
    noisy = [(recs[i][0],
              recs[i][1] + sum(shocks[i][c] * recs[i][2][c] for c in range(n_cls)),
              mark[i]) for i in range(len(recs))]
    resk, totk = top_stats(noisy)
    tots.append(totk)
    for g, _ in GROUPS:
        acc[g].append(resk[g])

print(f"\nCalibrated idiosyncratic dispersion, single market, {K_DRAWS} draws")
print(f"(aggregate accruals pinned per class per draw; total ${sum(tots)/K_DRAWS:,.0f}B, "
      f"range {min(tots):,.0f}..{max(tots):,.0f})")
print(f"{'group':>10} | {'mean $B':>8} | {'mean share':>10} | {'share range':>13} | {'overlap w/ det. top 1%':>22}")
for g, _ in sorted(GROUPS, key=lambda g: -g[1]):
    ts = [t for t, _, _ in acc[g]]; ss = [s for _, s, _ in acc[g]]; ov = [o for _, _, o in acc[g]]
    print(f"{g:>10} | {sum(ts)/K_DRAWS:>8,.0f} | {100*sum(ss)/K_DRAWS:>9.1f}% | "
          f"[{100*min(ss):.1f}%..{100*max(ss):.1f}%] | {100*sum(ov)/K_DRAWS:>21.0f}%")
print("\n(overlap column = share of the noisy group's tax units that are in the "
      "DETERMINISTIC top 1%; meaningful mainly for the top-1%-and-up rows)")

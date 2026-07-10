#!/usr/bin/env python3
"""
Validate the fitted top_tax dial surrogate against its held-out runs.
Promotion of the factorial-era pairwise test (scratchpad
pairwise_surrogate_test.py, 2026-07-09) to the dials build.

Checks, in order:
  (1) fit exactness   — residual == 0 (at shipped 3-dp precision) at every
                        FITTED point: all solo grid cells, pairs, triples
  (2) pair corners    — the 8 off-reference corners vs I_ij * g_i * g_j
                        scaling, +/-2% bar on conv total10
  (3) triples improve — quiz packages with >=3 cluster members predicted
                        better WITH the T_ijk terms than without
  (4) quiz bar        — +/-2% HARD bar on conv total10 for every holdout quiz
                        package; secondary: byyear %, per-head $B, per-ETR-cell
                        <= 0.1pp
  (5) full stack      — stack_ref (all 8 at REF) within the same bar

Emits other/top_tax/surrogate_report.md and stamps
meta.surrogate.validation + meta.surrogate.checks (holdout fixtures for the
render harness) into atlas2_data.json IN PLACE.

Exit nonzero on hard failures (checks 1/2/4/5), naming the offending lever /
pair so the patch loop (levers.PATCHES -> build_dial_runs.py --patch) can be
targeted.

Usage (pure file I/O — login-node safe):
  python3 other/top_tax/validate_surrogate.py [vintage_root] [data_json]
"""

import copy
import json
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import levers as L
import extract_atlas_data as X
import fit_surrogate as F

REPORT = os.path.join(HERE, "surrogate_report.md")

HARD_PCT = 2.0            # quiz / corner / stack bar on conv DECADE-1 total, percent
ETR_PP = 0.1              # secondary: per-ETR-cell bound, percentage points
EXACT_TOL = {"ct": 2e-3, "cy": 2e-3, "ch": 2e-3, "st": 2e-3, "sy": 2e-3,
             "sh": 2e-3, "etr": 6e-3}     # 3-dp rounding + etr omission floor
N_DEC = len(F.DECADES)    # 3 decade windows; hard bar applies to decade 1 only,
                          # decades 2/3 get measured-and-disclosed bounds


def pct_err(pred, actual):
    if abs(actual) < 1e-9:
        return 0.0 if abs(pred) < 1e-6 else float("inf")
    return 100.0 * (pred - actual) / abs(actual)


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else F.DEFAULT_ROOT
    data_path = sys.argv[2] if len(sys.argv) > 2 else F.DEFAULT_OUT
    with open(data_path) as fh:
        data = json.load(fh)

    legend = F.read_legend()
    by_kind = {}
    for scen_id, kind, state in legend:
        by_kind.setdefault(kind, []).append((scen_id, state))

    base_totals = os.path.join(root, "baseline", "static", "totals")
    bh, bcg = X.receipts_heads(base_totals), X.cg_carve(base_totals)
    base_heads = {"static": bh, "conventional": bh}
    base_cg = {"static": bcg, "conventional": bcg}
    groups = data["meta"]["etr_groups"]
    etr_base_flat = F.flatten_etr(data["etr_base"], groups)

    def read(scen_id):
        return F.read_scenario(root, scen_id, base_heads, base_cg,
                               etr_base_flat, groups)

    lines = ["# top_tax dials — surrogate validation report", ""]
    lines.append(f"- vintage: `{root}`")
    lines.append(f"- data: `{data_path}`")
    lines.append(f"- hard bar: ±{HARD_PCT}% on conventional DECADE-1 total; "
                 f"decades 2/3 measured+disclosed; ETR secondary bound {ETR_PP}pp/cell")
    lines.append("")
    hard_failures = []
    warnings = []

    # ---- (1) fit exactness -------------------------------------------------- #
    lines.append("## (1) Fit exactness at fitted points")
    worst = {}
    for kind in ("solo", "pair", "triple", "pairco"):
        for scen_id, state in by_kind.get(kind, []):
            q = read(scen_id)
            for qid in F.QIDS:
                pred = F.eval_state(data, qid, state)
                res = max(abs(a - b) for a, b in zip(pred, q[qid]))
                if res > worst.get(qid, (0, ""))[0]:
                    worst[qid] = (res, scen_id)
                if res > EXACT_TOL[qid]:
                    hard_failures.append(
                        f"exactness: {scen_id} qid={qid} residual {res:.4f}")
    for qid in F.QIDS:
        r, sid = worst.get(qid, (0.0, "—"))
        lines.append(f"- `{qid}`: max |residual| {r:.4f} ({sid})")
    lines.append("")

    # ---- (2) pair corners --------------------------------------------------- #
    lines.append("## (2) Pair corners — I·g·g scaling off reference")
    lines.append("")
    lines.append("| corner | actual d1 ($B) | pred d1 ($B) | d1 err % | d2 err % "
                 "| d3 err % | static d1 err % |")
    lines.append("|---|---|---|---|---|---|---|")
    corner_errs = []                       # decade-1 (hard bar)
    dec_errs = [[] for _ in range(N_DEC)]  # conv per decade, all holdouts
    static_dec_errs = [[] for _ in range(N_DEC)]
    for scen_id, state in by_kind.get("corner", []):
        q = read(scen_id)
        pc = F.eval_state(data, "ct", state)
        ps = F.eval_state(data, "st", state)
        ec = [pct_err(a, b) for a, b in zip(pc, q["ct"])]
        es = [pct_err(a, b) for a, b in zip(ps, q["st"])]
        corner_errs.append(abs(ec[0]))
        for d in range(N_DEC):
            dec_errs[d].append(abs(ec[d]))
            static_dec_errs[d].append(abs(es[d]))
        lines.append(f"| {scen_id} | {q['ct'][0]:.1f} | {pc[0]:.1f} | {ec[0]:+.2f} "
                     f"| {ec[1]:+.2f} | {ec[2]:+.2f} | {es[0]:+.2f} |")
        if abs(ec[0]) > HARD_PCT:
            hard_failures.append(f"corner: {scen_id} conv d1 err {ec[0]:+.2f}% "
                                 f"(levers {'+'.join(state)})")
    lines.append("")

    # ---- (3) triples improve the cluster combos ------------------------------ #
    lines.append("## (3) Triple terms on cluster-heavy holdouts")
    lines.append("")
    no_t = copy.deepcopy(data)
    no_t["surrogate"]["triples"] = {}
    lines.append("| package | actual ct | with T err % | without T err % |")
    lines.append("|---|---|---|---|")
    improved, total_heavy = 0, 0
    heavy = [(sid, s) for sid, s in
             by_kind.get("quiz", []) + by_kind.get("stack", [])
             if sum(k in L.CLUSTER for k in s) >= 3]
    for scen_id, state in heavy:
        q = read(scen_id)
        e_with = pct_err(F.eval_state(data, "ct", state)[0], q["ct"][0])   # decade 1
        e_wo = pct_err(F.eval_state(no_t, "ct", state)[0], q["ct"][0])
        total_heavy += 1
        improved += abs(e_with) <= abs(e_wo)
        lines.append(f"| {scen_id} | {q['ct'][0]:.1f} | {e_with:+.2f} | {e_wo:+.2f} |")
    if total_heavy and improved < math.ceil(total_heavy / 2):
        warnings.append(f"triples improve only {improved}/{total_heavy} "
                        "cluster-heavy holdouts")
    lines.append("")
    lines.append(f"Triples improve {improved}/{total_heavy} cluster-heavy holdouts.")
    lines.append("")

    # ---- (4) quiz + (5) full stack ------------------------------------------- #
    lines.append("## (4) Quiz holdouts — hard bar · (5) full stack")
    lines.append("")
    lines.append("| package | levers | actual d1 ($B) | pred d1 ($B) | d1 err % | "
                 "d2 err % | d3 err % | byyear max % | heads max $B | etr max pp |")
    lines.append("|---|---|---|---|---|---|---|---|---|---|")
    quiz_errs, fixtures = [], []
    byyear_worst, heads_worst, etr_worst = 0.0, 0.0, 0.0
    for scen_id, state in by_kind.get("quiz", []) + by_kind.get("stack", []):
        q = read(scen_id)
        pc = F.eval_state(data, "ct", state)
        ps = F.eval_state(data, "st", state)
        ec = [pct_err(a, b) for a, b in zip(pc, q["ct"])]
        es = [pct_err(a, b) for a, b in zip(ps, q["st"])]
        quiz_errs.append(abs(ec[0]))
        for d in range(N_DEC):
            dec_errs[d].append(abs(ec[d]))
            static_dec_errs[d].append(abs(es[d]))
        cy_p = F.eval_state(data, "cy", state)
        e_by = max(abs(pct_err(a, b)) for a, b in zip(cy_p, q["cy"]) if abs(b) > 5.0)
        ch_p = F.eval_state(data, "ch", state)
        e_h = max(abs(a - b) for a, b in zip(ch_p, q["ch"]))
        etr_p = F.eval_state(data, "etr", state)
        e_e = max(abs(a - b) for a, b in zip(etr_p, q["etr"]))
        byyear_worst, heads_worst = max(byyear_worst, e_by), max(heads_worst, e_h)
        etr_worst = max(etr_worst, e_e)
        lines.append(f"| {scen_id} | {'+'.join(state)} | {q['ct'][0]:.1f} | {pc[0]:.1f} "
                     f"| {ec[0]:+.2f} | {ec[1]:+.2f} | {ec[2]:+.2f} "
                     f"| {e_by:.2f} | {e_h:.2f} | {e_e:.3f} |")
        if abs(ec[0]) > HARD_PCT:
            hard_failures.append(f"quiz: {scen_id} conv d1 err {ec[0]:+.2f}% "
                                 f"(levers {'+'.join(state)})")
        if e_e > ETR_PP:
            warnings.append(f"{scen_id}: ETR cell err {e_e:.3f}pp > {ETR_PP}pp")
        fixtures.append(dict(id=scen_id, state=state,
                             conv_totals=[round(v, 3) for v in q["ct"]],
                             static_totals=[round(v, 3) for v in q["st"]]))
    # corners join the fixture set too (they are holdouts)
    for scen_id, state in by_kind.get("corner", []):
        q = read(scen_id)
        fixtures.append(dict(id=scen_id, state=state,
                             conv_totals=[round(v, 3) for v in q["ct"]],
                             static_totals=[round(v, 3) for v in q["st"]]))
    lines.append("")

    all_hold = quiz_errs + corner_errs   # decade-1 conv, the hard-bar set
    med = sorted(quiz_errs)[len(quiz_errs) // 2] if quiz_errs else 0.0

    # Per-decade measured bounds. Only decade-1 conv carries the hard bar;
    # decades 2/3 (and every static decade) get measured-and-disclosed bounds
    # — the I·g·g interaction scaling and the g impact-decade convention are
    # extrapolations out there, so the badge must state what the holdouts
    # actually show rather than assert the d1 bar. Infinite pct errors
    # (near-zero actuals) are excluded from the bound but warned.
    def bnd(errs, label):
        finite = [e for e in errs if e != float("inf")]
        if len(finite) < len(errs):
            warnings.append(f"{label}: {len(errs) - len(finite)} holdout(s) with "
                            "near-zero actual excluded from the bound")
        return math.ceil(max(finite) * 10) / 10 if finite else 0.0

    bound = bnd(all_hold, "conv d1")
    bounds = [bnd(dec_errs[d], f"conv d{d + 1}") for d in range(N_DEC)]
    static_bounds = [bnd(static_dec_errs[d], f"static d{d + 1}")
                     for d in range(N_DEC)]
    lines.append(f"**Holdout bounds (max |err|, per decade): "
                 f"conv ±{bounds[0]}% / ±{bounds[1]}% / ±{bounds[2]}% "
                 f"(median d1 quiz {med:.2f}%); "
                 f"static ±{static_bounds[0]}% / ±{static_bounds[1]}% / "
                 f"±{static_bounds[2]}%**")
    lines.append("")

    # ---- verdict + stamp ------------------------------------------------------ #
    if warnings:
        lines.append("## Warnings")
        lines += [f"- {w}" for w in warnings]
        lines.append("")
    if hard_failures:
        lines.append("## HARD FAILURES")
        lines += [f"- {f}" for f in hard_failures]
        lines.append("")
        lines.append("Patch loop: add targeted anchors/corners to levers.PATCHES, "
                     "run build_dial_runs.py --patch, run the patch batch into the "
                     "same vintage, refit, revalidate.")
    else:
        lines.append("**ALL CHECKS PASS.**")

    with open(REPORT, "w") as fh:
        fh.write("\n".join(lines) + "\n")
    print(f"Wrote {REPORT}")

    data["meta"]["surrogate"]["validation"] = dict(
        quiz=dict(n=len(quiz_errs), max_pct=round(max(quiz_errs), 3),
                  median_pct=round(med, 3)),
        corners=dict(n=len(corner_errs),
                     max_pct=round(max(corner_errs), 3) if corner_errs else None),
        byyear_max_pct=round(byyear_worst, 3),
        heads_max_b=round(heads_worst, 3),
        etr_max_pp=round(etr_worst, 4),
        bound_pct=bound, bounds_pct=bounds, static_bounds_pct=static_bounds,
        hard_bar_pct=HARD_PCT,
        passed=not hard_failures, date="2026-07-10")
    data["meta"]["surrogate"]["checks"] = fixtures
    with open(data_path, "w") as fh:
        json.dump(data, fh, separators=(",", ":"))
    print(f"Stamped validation + {len(fixtures)} holdout fixtures -> {data_path}")

    if hard_failures:
        print("HARD FAILURES:")
        for f_ in hard_failures:
            print("  -", f_)
        sys.exit(1)
    print(f"ALL CHECKS PASS — badge bound ±{bound}% on {len(quiz_errs)} holdout packages")


if __name__ == "__main__":
    main()

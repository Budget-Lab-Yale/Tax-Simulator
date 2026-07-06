#!/usr/bin/env python3
#------------------------------------------------------------------------------
# clausing_stacked_wide_table.py
#
# Builds the copy-paste-ready wide-format Clausing-Sarin distribution table:
# two panels (Average Tax Change in 2026 dollars; Percent change in after-tax
# income, pp), rows = line items in the published-table order, columns =
# income groups, plus an income-group-cutoff row.
#
# Inputs (all derived from a completed model run + the excise pipeline):
#   - {VINTAGE}/clausing_corp_sin_stacked_data_avg_2030_2039.csv
#       (per-piece 10-yr averages, written by
#        other/analysis/clausing_corp_sin_stacked_distribution.R)
#   - other/analysis_scripts/public/clausing_excise_distribution_by_year.csv
#       (off-model excise + COLA-offset detail by year, written by
#        clausing_excise_distribution.R)
#   - {VINTAGE}/01_clinton_rates/static/supplemental/distribution.csv
#       (income-group cutoffs; identical across scenarios)
#   - Macro-Projections chained CPI (2026 = 1 after normalization)
#
# The COLA'd-benefit offset (SS/SNAP/SSI respond to the excise price level,
# one-year lag) is FOLDED INTO each excise line rather than shown as its own
# row: each year's offset is allocated across the five excises by their share
# of the package's excise revenue that year -- the same proportion by which
# each tax contributes to the price-level effect pi. The offset's incidence
# pattern across income groups is identical regardless of which excise drives
# it, so revenue shares are the exact allocator. Net rows are invariant to
# this presentation choice.
#
# On-model pieces (layers 01-07 + corporate) come from the stacked-data file
# already deflated; off-model pieces are deflated here (chained CPI, 2026$)
# and averaged over 2030-2039, mirroring the chart script's convention.
#
# Output: {VINTAGE}/clausing_corp_sin_stacked_wide_avg_2030_2039.csv
#
# Pure-CSV work; fine to run anywhere (no R, no heavy compute):
#   python3 other/analysis/clausing_stacked_wide_table.py
#------------------------------------------------------------------------------

import csv
import os
from collections import defaultdict

VINTAGE = os.environ.get('CLAUSING_VINTAGE', 'clausing_v2_s50')
V = f'/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/{VINTAGE}'
R = ('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/'
     'other/analysis_scripts/public')
MACRO = ('/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/'
         'v3/2026022522/baseline/projections.csv')

groups = ['Quintile 1', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5',
          'Top 10%', 'Top 5%', 'Top 1%', 'Top 0.1%']
excises = ['carbon', 'gambling', 'guns', 'alcohol', 'tobacco']
YEARS = range(2030, 2040)

# ccpiu deflator, 2026 = 1
ccpiu = {}
with open(MACRO) as f:
    for r in csv.DictReader(f):
        ccpiu[int(r['year'])] = float(r['ccpiu'])
base = ccpiu[2026]

# off-model by-year detail: avg (nominal $), pct, and Overall dollars_B (for
# the per-year excise revenue shares that allocate the offset)
avg_y = defaultdict(dict)
pct_y = defaultdict(dict)
rev_y = defaultdict(dict)
with open(f'{R}/clausing_excise_distribution_by_year.csv') as f:
    for r in csv.DictReader(f):
        y, m, g = int(r['year']), r['measure'], r['group']
        if y not in YEARS:
            continue
        if g in groups:
            avg_y[(m, g)][y] = float(r['avg'])
            pct_y[(m, g)][y] = float(r['pct_chg_ati'])
        if g == 'Overall' and m in excises:
            rev_y[m][y] = float(r['dollars_B'])
share = {m: {y: rev_y[m][y] / sum(rev_y[e][y] for e in excises) for y in YEARS}
         for m in excises}

def tenyr(m, g, netted=False):
    # 10-yr average of (deflated avg $, pct); excises get their revenue-share
    # slice of the benefit offset folded in per year
    a = p = 0.0
    for y in YEARS:
        av, pc = avg_y[(m, g)][y], pct_y[(m, g)][y]
        if netted:
            av += avg_y[('benefit_offset', g)][y] * share[m][y]
            pc += pct_y[('benefit_offset', g)][y] * share[m][y]
        a += av / (ccpiu[y] / base)
        p += pc
    return a / 10, p / 10 * 100

# on-model pieces from the stacked data file (already 2026$ / pp)
avg_m = defaultdict(dict)
pct_m = defaultdict(dict)
with open(f'{V}/clausing_corp_sin_stacked_data_avg_2030_2039.csv') as f:
    for r in csv.DictReader(f):
        avg_m[r['piece_id']][r['group']] = float(r['avg'])
        pct_m[r['piece_id']][r['group']] = float(r['pct_chg_ati_pp'])

# income-group cutoffs: 10-yr avg in 2026$, identical across scenarios
cuts = defaultdict(list)
with open(f'{V}/01_clinton_rates/static/supplemental/distribution.csv') as f:
    for r in csv.DictReader(f):
        if (r['taxes_included'] == 'iit_pr_death_cit_vat_wealth'
                and r['group_dimension'] == 'Income' and r['group'] in groups
                and 2030 <= int(r['year']) <= 2039):
            cuts[r['group']].append(
                float(r['income_cutoff']) / (ccpiu[int(r['year'])] / base))
cutoff = {g: sum(v) / len(v) for g, v in cuts.items()}

# (key, kind, published label) in published-table row order
rows = [
    ('01_clinton_rates',        'model', 'Restore rates and brackets to (inflation-indexed) 1997 levels'),
    ('02_restore_bottom_rates', 'model', 'Exempt first two brackets from 1997 restoration'),
    ('03_199a',                 'model', 'Repeal QBI deduction'),
    ('04_carryover_basis',      'model', 'Institute carryover basis for inherited assets'),
    ('05_pref_rates',           'model', 'Increase capital gains and dividend rates by 5pp'),
    ('carried_interest',        'off',   'Tax carried interest as ordinary income'),
    ('06_niit_reform',          'model', 'Subject all income above $250K to 3.8% Medicare tax'),
    ('07_estate',               'model', 'Lower the estate tax exemption to $5M/$10M and raise the rate to 45%'),
    ('qsbs',                    'off',   'Repeal QSBS preference'),
    ('oz',                      'off',   'Repeal OZ preference'),
    ('08_corporate',            'model', 'Corporate (off-model)'),
    ('carbon',                  'exc',   'Enact a carbon tax'),
    ('gambling',                'exc',   'Enact an excise tax on sports gambling'),
    ('guns',                    'exc',   'Increase the excise tax on guns'),
    ('alcohol',                 'exc',   'Increase the excise tax on alcohol'),
    ('tobacco',                 'exc',   'Increase the excise tax on tobacco'),
]

def cell(key, kind, g):
    if kind == 'model':
        return avg_m[key][g], pct_m[key][g]
    return tenyr(key, g, netted=(kind == 'exc'))

vals = {(k, g): cell(k, kind, g) for k, kind, _ in rows for g in groups}

def panel(title, idx, fmt):
    lines = [title, ',' + ','.join(groups)]
    lines.append('Income group cutoff,'
                 + ','.join(str(int(round(cutoff[g]))) for g in groups))
    net = {g: sum(vals[(k, g)][idx] for k, _, _ in rows) for g in groups}
    lines.append('Net,' + ','.join(fmt(net[g]) for g in groups))
    for k, kind, lab in rows:
        lines.append(lab + ',' + ','.join(fmt(vals[(k, g)][idx]) for g in groups))
    return lines

out = (panel('Average Tax Change (2026 dollars)', 0, lambda x: str(int(round(x))))
       + ['']
       + panel('Percent change in after-tax income (pp)', 1, lambda x: f'{x:.3f}'))
path = f'{V}/clausing_corp_sin_stacked_wide_avg_2030_2039.csv'
with open(path, 'w') as f:
    f.write('\n'.join(out) + '\n')
print('written:', path)

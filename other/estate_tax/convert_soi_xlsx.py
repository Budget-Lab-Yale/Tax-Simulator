#!/usr/bin/env python3
"""
Convert raw IRS SOI estate Table 1 filing-year workbooks (16/17/18es01fy.xlsx)
into the cleaned-csv schema of estate_tax_filed_2019_2023.csv and write the
combined file estate_tax_filed_2016_2023.csv.

Layout facts (verified for all three files):
  - money amounts in THOUSANDS of dollars (csv is in dollars -> x1000)
  - header labels on row 4/5, one label per Number/Amount column pair
    (number at the label's column, amount at the next column); items map by
    LABEL, not index (2016 orders deduction detail before the total)
  - data rows 9-26: All / All taxable / All nontaxable blocks, each with
    Under $5M / 5-10M / 10-20M / 20-50M / 50M+ sub-rows
  - 'd' = disclosure-deleted (kept blank, same convention as the 2019-2023
    csv's suppressed cells); '*' = small-sample caution (marker stripped)
  - net_worth is not reported in these years -> blank
"""
import csv
import re
import sys
import zipfile
import xml.etree.ElementTree as ET

M = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'

FILES = {
    2016: '16es01fy.xlsx',
    2017: '17es01fy.xlsx',
    2018: '18es01fy.xlsx',
}
EXISTING = 'estate_tax_filed_2019_2023.csv'
OUT = 'estate_tax_filed_2016_2023.csv'

LABEL_TO_ITEM = {
    'Gross estate for tax purposes':       'gross_estate_for_tax_purposes',
    'Personal residence':                  'personal_residence',
    'Other real estate':                   'other_real_estate',
    'Real estate partnerships':            'real_estate_partnerships',
    'Closely held stock':                  'closely_held_stock',
    'Publicly traded stock':               'publicly_traded_stock',
    'State and local bonds':               'state_and_local_bonds',
    'Federal bonds':                       'federal_bonds',
    'Corporate and foreign bonds':         'corporate_and_foreign_bonds',
    'Bond funds':                          'bond_funds',
    'Unclassifiable mutual funds':         'unclassifiable_mutual_funds',
    'Unallocated investments':             'unallocated_investments',
    'Cash assets':                         'cash_assets',
    'Net life insurance':                  'net_life_insurance',
    'Farm assets':                         'farm_assets',
    'Private equity and hedge funds':      'private_equity_and_hedge_funds',
    'Other limited partnerships':          'other_limited_partnerships',
    'Other noncorporate business assets':  'other_noncorporate_business_assets',
    'Mortgages and notes':                 'mortgages_and_notes',
    'Retirement assets':                   'retirement_assets',
    'Depletables / intangibles':           'depletables_intangibles',
    'Art':                                 'art',
    'Other assets':                        'other_assets',
    'Community property':                  'community_property',
    'Total joint property':                'total_joint_property',
    'Total lifetime transfers':            'total_lifetime_transfers',
    'Total allowable deductions':          'total_allowable_deductions',
    'Funeral expenses':                    'funeral_expenses',
    "Executors' commissions":              'executors_commissions',
    "Attorneys' fees":                     'attorneys_fees',
    'Other expenses/losses':               'other_expenses_losses',
    'Debts and mortgages':                 'debts_and_mortgages',
    'Bequests to surviving spouse':        'bequests_to_surviving_spouse',
    'Charitable deduction':                'charitable_deduction',
    'State death tax deduction':           'state_death_tax_deduction',
    'Taxable estate':                      'taxable_estate',
    'Adjusted taxable gifts':              'adjusted_taxable_gifts',
    'Adjusted taxable estate':             'adjusted_taxable_estate',
    'Tentative estate tax':                'tentative_estate_tax',
    'Gift tax paid':                       'gift_tax_paid',
    'Total tax before credits':            'total_tax_before_credits',
    'Deceased spousal unused exclusion':   'deceased_spousal_unused_exclusion',
    'Allowable unified credit':            'allowable_unified_credit',
    'Net estate tax':                      'net_estate_tax',
}

ROW_MAP = {
    'All returns':            ('all', 'all'),
    'All taxable returns':    ('taxable', 'all'),
    'All nontaxable returns': ('nontaxable', 'all'),
}
SIZE_MAP = {
    'Under $5 million':           'under_5m',
    '$5 million < $10 million':   '5m_10m',
    '$10 million < $20 million':  '10m_20m',
    '$20 million < $50 million':  '20m_50m',
    '$50 million or more':        '50m_plus',
}


def col_idx(ref):
    i = 0
    for ch in re.match(r'([A-Z]+)', ref).group(1):
        i = i * 26 + (ord(ch) - 64)
    return i - 1


def read_grid(path):
    z = zipfile.ZipFile(path)
    shared = []
    if 'xl/sharedStrings.xml' in z.namelist():
        for si in ET.fromstring(z.read('xl/sharedStrings.xml')).findall(f'{M}si'):
            shared.append(''.join(t.text or '' for t in si.iter(f'{M}t')))
    grid = {}
    for row in ET.fromstring(z.read('xl/worksheets/sheet1.xml')).iter(f'{M}row'):
        r = int(row.get('r'))
        for c in row:
            v = c.find(f'{M}v')
            if v is None:
                continue
            val = shared[int(v.text)] if c.get('t') == 's' else v.text
            grid[(r, col_idx(c.get('r')))] = (val or '').strip()
    return grid


def clean_num(raw):
    """'' / 'd' -> blank; strip '*' caution markers; otherwise numeric string."""
    s = raw.replace('*', '').strip()
    if s in ('', 'd', '[d]', '-', '--'):
        return ''
    try:
        return float(s)
    except ValueError:
        sys.exit(f'Unparseable cell value: {raw!r}')


def parse_file(year, path):
    grid = read_grid(path)

    # Label columns from header rows 4 and 5
    label_cols = {}
    for (r, c), val in grid.items():
        if r in (4, 5) and val in LABEL_TO_ITEM:
            label_cols[LABEL_TO_ITEM[val]] = c
    missing = set(LABEL_TO_ITEM.values()) - set(label_cols)
    if missing:
        sys.exit(f'{path}: unmapped items: {missing}')

    rows = []
    status = None
    for r in range(9, 27):
        label = grid.get((r, 0), '')
        if label in ROW_MAP:
            status, size_bin = ROW_MAP[label]
        elif label in SIZE_MAP:
            size_bin = SIZE_MAP[label]
        else:
            sys.exit(f'{path}: unexpected row label at R{r}: {label!r}')

        rec = {'year': year, 'tax_status': status, 'size_bin': size_bin}
        for item, c in label_cols.items():
            n = clean_num(grid.get((r, c), ''))
            amt = clean_num(grid.get((r, c + 1), ''))
            rec[item + '_n'] = '' if n == '' else f'{n:.0f}'
            rec[item + '_amt'] = '' if amt == '' else f'{amt * 1000:.0f}'
        rows.append(rec)
    return rows


def main():
    with open(EXISTING) as fh:
        rdr = csv.reader(fh)
        header = next(rdr)
        existing_rows = list(rdr)

    new_rows = []
    for year in sorted(FILES):
        new_rows += parse_file(year, FILES[year])

    with open(OUT, 'w', newline='') as fh:
        w = csv.writer(fh)
        w.writerow(header)
        for rec in new_rows:
            w.writerow([rec.get(col, '') for col in header])
        for row in existing_rows:
            w.writerow(row)

    # Sanity summary
    print(f'Wrote {OUT}: {len(new_rows)} new rows (2016-2018) + '
          f'{len(existing_rows)} existing rows')
    for rec in new_rows:
        if rec['tax_status'] == 'taxable' and rec['size_bin'] == 'all':
            print(f"  filing {rec['year']} taxable all: "
                  f"n={rec['gross_estate_for_tax_purposes_n']}, "
                  f"net_tax=${float(rec['net_estate_tax_amt'])/1e9:.1f}B, "
                  f"gifts=${float(rec['adjusted_taxable_gifts_amt'])/1e9:.1f}B")


if __name__ == '__main__':
    main()

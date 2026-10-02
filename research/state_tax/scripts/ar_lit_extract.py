"""Extract Arkansas Low Income Tax Tables from the DFA AR1000F booklets.

Reads word coordinates (pdftotext -bbox) rather than the -layout text, where
three tables share a line and a row can wrap. On each table page the numeric
words cluster into right-aligned columns by xMax; consecutive column triples
are (from, to, tax) for one table, and rows are matched by y. Tables come out
in page order: single; head of household 0-1 and 2+ dependents; joint 0-1
and 2+ dependents. Each table is checked to start at 0 and to chain without a
gap (each row starts $1 above the previous row's upper bound).

    module load poppler/25.07.0-GCC-13.3.0
    python3 research/state_tax/scripts/ar_lit_extract.py <booklet_dir> <out_csv>

Writes config/scenarios/tax_law_state/baseline/ar/credit_tables.csv (the
booklets themselves live in the gitignored output/ar_booklets/).
"""
import csv, os, re, subprocess, sys

TABLES = [  # (credit key: filing_status, dependents key)
    ('single', 1, 0), ('hoh_0_1', 4, 0), ('hoh_2', 4, 2),
    ('joint_0_1', 2, 0), ('joint_2', 2, 2)]
WORD = re.compile(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" '
                  r'yMax="([\d.]+)">([^<]+)</word>')


def table_pages(pdf):
    """1-based pages whose text carries the low income table header."""
    txt = subprocess.run(['pdftotext', '-layout', pdf, '-'], capture_output=True,
                         text=True).stdout.split('\f')
    return [i + 1 for i, pg in enumerate(txt)
            if re.search(r'Low Income Tax Tables', pg) and
            re.search(r'IF YOUR ADJUSTED|ADJUSTED GROSS', pg)]


def page_tables(pdf, page):
    html = subprocess.run(['pdftotext', '-bbox', '-f', str(page), '-l', str(page),
                           pdf, '-'], capture_output=True, text=True).stdout
    # A word carrying trailing tabs ('67\t\t' in the TY2020 joint table) has
    # its xMax stretched across the gutter; rebuild that right edge from the
    # digit advance (~4.45pt in these tables) so it lands in its own column
    nums = []
    for x1, y1, x2, _, w in WORD.findall(html):
        v = w.strip()
        if not re.fullmatch(r'\d[\d,]*', v):
            continue
        right = float(x2) if v == w else float(x1) + 4.45 * len(v)
        nums.append((right, float(y1), int(v.replace(',', ''))))
    # cluster right edges into columns; keep columns with many entries
    nums.sort()
    cols, cur = [], [nums[0]]
    for n in nums[1:]:
        if n[0] - cur[-1][0] > 12:
            cols.append(cur); cur = [n]
        else:
            cur.append(n)
    cols.append(cur)
    cols = [c for c in cols if len(c) >= 5]
    if len(cols) % 3:
        raise ValueError(f'page {page}: {len(cols)} columns, not a multiple of 3')
    tables = []
    for k in range(0, len(cols), 3):
        lo, hi, tax = cols[k:k + 3]
        rows = []
        for x, y, a in lo:
            b = [v for _, yy, v in hi if abs(yy - y) < 3]
            t = [v for _, yy, v in tax if abs(yy - y) < 3]
            if len(b) == 1 and len(t) == 1:
                rows.append((a, b[0], t[0]))
        tables.append(sorted(rows))
    return tables


def check(rows, label):
    """Validate the chain; repair a lower bound printed $1 low (the 2017
    head-of-household table prints 21,900-22,000 after 21,801-21,900)."""
    if rows[0][0] != 0 or rows[0][2] != 0:
        raise ValueError(f'{label}: first row {rows[0]} is not the zero band')
    for i in range(1, len(rows)):
        p, n = rows[i - 1], rows[i]
        if n[0] == p[1]:
            print(f'  {label}: repaired printed lower bound {n} -> {n[0] + 1}')
            rows[i] = n = (n[0] + 1, n[1], n[2])
        if n[0] != p[1] + 1:
            raise ValueError(f'{label}: gap between {p} and {n}')
        if n[2] < p[2]:
            raise ValueError(f'{label}: tax falls between {p} and {n}')


if __name__ == '__main__':
    bdir, out_csv = sys.argv[1], sys.argv[2]
    out = []
    for yr in range(2017, 2026):
        pdf = os.path.join(bdir, f'i{yr}.pdf')
        tables = [t for p in table_pages(pdf) for t in page_tables(pdf, p)]
        if len(tables) != len(TABLES):
            raise ValueError(f'{yr}: found {len(tables)} tables')
        for (name, fs, key), rows in zip(TABLES, tables):
            check(rows, f'{yr} {name}')
            print(f'{yr} {name:9s} rows {len(rows):3d} zero to {rows[0][1]:>6,} '
                  f'top {rows[-1][1]:>6,} tax {rows[-1][2]}')
            for a, b, t in rows:
                out.append(['low_income_tax_table', 'AR', yr, fs, key, a, b, t])
            # the qualification ceiling, so a unit above the table is told
            # apart from one in a zero band
            out.append(['low_income_tax_table_max', 'AR', yr, fs, key, '-Inf', 'Inf',
                        rows[-1][1]])
    with open(out_csv, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['credit_id', 'state', 'year', 'filing_status', 'key_concept',
                    'income_lower', 'income_upper', 'value'])
        w.writerows(out)

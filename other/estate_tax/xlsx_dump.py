#!/usr/bin/env python3
"""Minimal stdlib xlsx -> TSV dumper (first worksheet) for inspecting SOI tables."""
import sys
import zipfile
import xml.etree.ElementTree as ET
import re

NS = {'m': 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}

def col_to_idx(ref):
    m = re.match(r'([A-Z]+)', ref)
    idx = 0
    for ch in m.group(1):
        idx = idx * 26 + (ord(ch) - 64)
    return idx - 1

def main(path, max_rows=999):
    z = zipfile.ZipFile(path)
    shared = []
    if 'xl/sharedStrings.xml' in z.namelist():
        root = ET.fromstring(z.read('xl/sharedStrings.xml'))
        for si in root.findall('m:si', NS):
            shared.append(''.join(t.text or '' for t in si.iter(
                '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}t')))
    # first sheet listed in workbook
    wb = ET.fromstring(z.read('xl/workbook.xml'))
    sheets = wb.find('m:sheets', NS).findall('m:sheet', NS)
    print(f"# sheets: {[s.get('name') for s in sheets]}", file=sys.stderr)
    sheet_xml = 'xl/worksheets/sheet1.xml'
    root = ET.fromstring(z.read(sheet_xml))
    for row in root.iter('{http://schemas.openxmlformats.org/spreadsheetml/2006/main}row'):
        if int(row.get('r')) > max_rows:
            break
        cells = {}
        for c in row:
            ref = c.get('r')
            t = c.get('t')
            v = c.find('m:v', NS)
            if v is None:
                is_node = c.find('m:is', NS)
                val = ''.join(tt.text or '' for tt in is_node.iter(
                    '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}t')) if is_node is not None else ''
            else:
                val = shared[int(v.text)] if t == 's' else v.text
            cells[col_to_idx(ref)] = (val or '').replace('\n', ' ').replace('\t', ' ')
        if cells:
            n = max(cells) + 1
            print(f"R{row.get('r')}\t" + '\t'.join(cells.get(i, '') for i in range(n)))

if __name__ == '__main__':
    main(sys.argv[1], int(sys.argv[2]) if len(sys.argv) > 2 else 999)

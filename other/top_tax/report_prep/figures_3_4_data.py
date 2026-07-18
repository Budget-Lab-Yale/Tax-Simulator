# figures_3_4_data.py -- regenerates the numbers behind report_prep/figures_3_4.html
# Ordered-stacking, gains-at-death folded into capital gains, third decade (FY2047-56),
# % of GDP. Faithful port of the atlas2.html surrogate evaluator (solo + g-scaled
# pair/triple ANOVA); head->base carve puts the deemed portion of the iit head into cg.
# Verified to reproduce the first-decade mock within rounding. Re-run after any dials
# vintage rebuild (e.g. the pending estate-avoidance fix) and repoint atlas2_data.json.
#   python3 other/top_tax/report_prep/figures_3_4_data.py
import json, math
D = json.load(open('/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/atlas2_data.json'))
SUR = D['surrogate']; MQ = D['meta']['surrogate']['m']
HEADS = D['meta']['surrogate']['heads_order']          # iit cg pay corp est wealth other
LEVERS = D['meta']['levers']; BYKEY = {l['key']: l for l in LEVERS}
LORDER = [l['key'] for l in LEVERS]
DECS = D['meta']['decades']; NDEC = len(DECS)
GDP = D['meta']['gdp_fy_decades']

# DECI: element index -> decade (decade-major money vectors; etr is impact-year)
DECI = {}
for qid, m in MQ.items():
    if qid in ('etr','etrc') or NDEC < 2:
        DECI[qid] = [0]*m
    else:
        per = m//NDEC; DECI[qid] = [dd for dd in range(NDEC) for _ in range(per)]

def locate(knots, x, scale):
    k = knots
    if scale == 'log':
        k = [math.log(v) for v in knots]; x = math.log(max(x, 1e-12))
    if x <= k[0]: return (0, 0.0)
    if x >= k[-1]: return (len(k)-1, 0.0)
    for i in range(len(k)-1):
        if x == k[i]: return (i, 0.0)
        if x < k[i+1]: return (i, (x-k[i])/(k[i+1]-k[i]))
    return (len(k)-1, 0.0)

def evalGrid(lv, rows, vals):
    axes = lv['params']
    if len(axes) == 1:
        p = axes[0]; x = vals[p['key']]
        if p['scale'] == 'pos':
            return list(rows[p['knots'].index(x)])
        i, t = locate(p['knots'], x, p['scale'])
        if t == 0: return list(rows[i])
        return [(1-t)*rows[i][z] + t*rows[i+1][z] for z in range(len(rows[i]))]
    p1, p2 = axes[0], axes[1]; n2 = len(p2['knots'])
    i1, t1 = locate(p1['knots'], vals[p1['key']], p1['scale'])
    i2, t2 = locate(p2['knots'], vals[p2['key']], p2['scale'])
    row = lambda a, b: rows[a*n2 + b]
    if t1 == 0 and t2 == 0: return list(row(i1, i2))
    j1 = min(i1+1, len(p1['knots'])-1); j2 = min(i2+1, n2-1)
    r00, r01, r10, r11 = row(i1,i2), row(i1,j2), row(j1,i2), row(j1,j2)
    return [(1-t1)*((1-t2)*r00[z]+t2*r01[z]) + t1*((1-t2)*r10[z]+t2*r11[z]) for z in range(len(r00))]

def gOf(key, vals):
    return evalGrid(BYKEY[key], SUR['g'][key], vals)

def pairTerm(a, b, qid, s, g):
    pe = SUR['pairs'].get(a+'|'+b)
    if not pe: return None
    ladder = 'deemed' if (a=='deemed' or b=='deemed') else None
    if pe.get('byPos') and ladder:
        pos = s[ladder]['pos']
        if pos in pe['byPos']:
            other = b if a == ladder else a
            v = pe['byPos'][pos].get(qid)
            return {'vec': v, 'w': list(g[other])} if v else None
    if pe.get(qid):
        return {'vec': pe[qid], 'w': [g[a][z]*g[b][z] for z in range(NDEC)]}
    return None

def tripleTerm(entry, ks, qid, s, g):
    w = [1.0]*NDEC
    if entry.get('byPos') and 'deemed' in ks:
        pos = s['deemed']['pos']
        if pos in entry['byPos']:
            for k in ks:
                if k != 'deemed':
                    for z in range(NDEC): w[z] *= g[k][z]
            v = entry['byPos'][pos].get(qid)
            return {'vec': v, 'w': w} if v else None
    if not entry.get(qid): return None
    for k in ks:
        for z in range(NDEC): w[z] *= g[k][z]
    return {'vec': entry[qid], 'w': w}

def evalQ(qid, s):
    m = MQ[qid]; deci = DECI[qid]; tot = [0.0]*m
    on = [k for k in LORDER if s.get(k)]
    g = {}
    for k in on:
        f = SUR['solo'][k].get(qid)
        if f:
            v = evalGrid(BYKEY[k], f, s[k])
            for x in range(m): tot[x] += v[x]
        g[k] = gOf(k, s[k])
    for i in range(len(on)):
        for j in range(i+1, len(on)):
            pt = pairTerm(on[i], on[j], qid, s, g)
            if pt:
                for x in range(m): tot[x] += pt['vec'][x]*pt['w'][deci[x]]
    for tk, entry in SUR['triples'].items():
        ks = tk.split('|')
        if all(k in g for k in ks):
            tt = tripleTerm(entry, ks, qid, s, g)
            if tt:
                for x in range(m): tot[x] += tt['vec'][x]*tt['w'][deci[x]]
    return tot

def heads(qid, s, dec):
    v = evalQ(qid, s); NH = len(HEADS)
    return {HEADS[z]: v[dec*NH + z] for z in range(NH)}


def strip_deemed(s): return {k:v for k,v in s.items() if k!='deemed'}

# base vector for an absolute package: deemed portion of iit head -> cg base
def bases(qid, s, dec):
    h  = heads(qid, s, dec)
    hd = heads(qid, strip_deemed(s), dec)            # same package, deemed removed
    deemed_iit = h['iit'] - hd['iit']                # the deemed (gains-at-death) tax
    return {
      'cg':   h['cg']  + deemed_iit,
      'iit':  h['iit'] - deemed_iit + h['pay'] + h['wealth'] + h['other'],
      'corp': h['corp'],
      'est':  h['est'],
    }

P = {
 'cg':          {'cg':{'rate':25}},
 'cgcorp':      {'cg':{'rate':25},'corp':{'rate':25}},
 'deemed':      {'deemed':{'pos':'deemed'}},
 'deemedcg':    {'deemed':{'pos':'deemed'},'cg':{'rate':25}},
 'deemedcgcorp':{'deemed':{'pos':'deemed'},'cg':{'rate':25},'corp':{'rate':25}},
}
BS = ['iit','cg','corp','est']
def diff(a,b): return {k:a[k]-b[k] for k in BS}

def emit(dec, unit):
    g = GDP[dec]; conv = lambda v: (v/g*100.0 if unit=='pct' else v)
    print('#'*64); print('# DECADE %d %s  GDP=%.1f  unit=%s' % (dec, DECS[dec], g, unit))
    def jr(qid, s2, s1=None):
        b = bases(qid, s2, dec)
        if s1: b = diff(b, bases(qid, s1, dec))
        return {k: round(conv(b[k]), 3 if unit=='pct' else 0) for k in BS}
    def line(tag, s2, s1=None):
        c = jr('ch', s2, s1); st = jr('sh', s2, s1)
        cn = round(sum(c.values()),3); sn = round(sum(st.values()),3)
        cc = {k:v for k,v in c.items() if abs(v)>= (0.001 if unit=='pct' else 0.5)}
        ss = {k:v for k,v in st.items() if abs(v)>= (0.001 if unit=='pct' else 0.5)}
        print('  %-14s conv %-8s %s' % (tag, cn, cc))
        print('  %-14s stat %-8s %s' % ('',  sn, ss))
    print('--FIG3--')
    line('cg',        P['cg'])
    line('+corp',     P['cgcorp'], P['cg'])
    line('PACKAGE',   P['cgcorp'])
    print('--FIG4--')
    line('deemed',    P['deemed'])
    line('+cg',       P['deemedcg'], P['deemed'])
    line('+corp',     P['deemedcgcorp'], P['deemedcg'])
    line('PACKAGE',   P['deemedcgcorp'])

emit(0, 'B')     # verify vs mock
emit(2, 'B')     # third decade, dollars
emit(2, 'pct')   # third decade, % of GDP  <-- the figure

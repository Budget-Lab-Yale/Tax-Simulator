#!/usr/bin/env python3
"""
(a) Donor-clone fingerprint for Tax-Data 2022.

Detects records that share an EXACT economic-gross value (donor replication) and
ranks the resulting clusters by death-weight (sum w*q1, single-equiv mortality).
The $17.65M / age-80 cluster surfaced in the estate shape diagnostic; this asks
how many clones exist, how concentrated they are, and whether the pathology
recurs at other wealth points.
"""
import csv
import sys
from collections import defaultdict
TD=sys.argv[1] if len(sys.argv)>1 else '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv'
def num(s):
    s=(s or '').strip()
    if s in ('','NA','NaN','None'): return 0.0
    try: return float(s)
    except: return 0.0
ASSET=['value.cash','value.equities','value.bonds','value.dc','value.db','value.life_ins',
 'value.annuities','value.trusts','value.other_fin','value.pass_throughs','value.primary_home',
 'value.other_home','value.re_fund','value.other_nonfin']
FLOOR=10e6   # focus on estate-relevant wealth

# cluster on exact gross (to the dollar)
nrec=defaultdict(int); sw=defaultdict(float); swq=defaultdict(float); ages=defaultdict(list)
tot_dw=0.0
with open(TD) as fh:
    rdr=csv.reader(fh); hdr=next(rdr); ix={n:i for i,n in enumerate(hdr)}
    iw=ix['weight'];ifs=ix['filing_status'];iq1=ix['q_death1'];iq2=ix['q_death2']
    iage=ix.get('age1'); ia=[ix[c] for c in ASSET]
    for r in rdr:
        w=num(r[iw])
        if w<=0:continue
        g=sum(num(r[j]) for j in ia)
        if g<FLOOR:continue
        q1=num(r[iq1]); married=(r[ifs]=='2' and num(r[iq2])>0)
        dw=w*(q1*num(r[iq2]) if married else q1)
        tot_dw+=dw
        key=round(g)            # exact dollars
        nrec[key]+=1; sw[key]+=w; swq[key]+=dw
        if iage is not None: ages[key].append(num(r[iage]))

clusters=[(k,nrec[k],sw[k],swq[k]) for k in nrec if nrec[k]>=2]
clusters.sort(key=lambda x:-x[3])
print(f"Total death-weight (>= $10M gross): {tot_dw:,.0f}")
print(f"Distinct exact-gross values with >=2 records: {len(clusters):,}")
clus_dw=sum(c[3] for c in clusters)
clus_dw5=sum(c[3] for c in clusters if c[1]>=5)
print(f"Death-weight in any multi-record cluster (n>=2): {clus_dw:,.0f} ({clus_dw/tot_dw:.0%})")
print(f"Death-weight in clusters with n>=5 records:      {clus_dw5:,.0f} ({clus_dw5/tot_dw:.0%})")
print()
print("Top 20 clusters by death-weight (sum w*q1):")
print(f"{'gross$M':>10} {'n_rec':>6} {'pop_wt':>10} {'E[deaths]':>10} {'%tot_dw':>8} {'avg_age':>8}")
for k,n,w,dw in clusters[:20]:
    aa=sum(ages[k])/len(ages[k]) if ages[k] else -1
    print(f"{k/1e6:>10.3f} {n:>6} {w:>10,.0f} {dw:>10,.1f} {dw/tot_dw:>7.1%} {aa:>8.1f}")
# how much of total death-weight do the top 10/50 clusters carry?
for topn in (10,50,200):
    print(f"  top {topn} clusters = {sum(c[3] for c in clusters[:topn]):,.0f} E[deaths] "
          f"({sum(c[3] for c in clusters[:topn])/tot_dw:.0%} of >=$10M death-weight)")

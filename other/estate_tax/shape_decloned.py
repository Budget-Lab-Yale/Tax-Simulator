#!/usr/bin/env python3
"""
(b) Re-run the uncalibrated multi-year shape diagnostic with donor-clone death-weight
DOWN-WEIGHTED, to confirm the 10-20M overshoot is the artifact.

Clusters = records sharing exact economic gross (>= $10M). A cluster's death-weight
(sum w*q1 single-equiv, at the 2022 level) is capped at `cap`; every member's
effective weight is scaled by cap/cluster_dw when the cluster exceeds it. Variants:
  - baseline      : no de-cloning (matches first_order_shape_multiyear)
  - drop_1765     : remove the $17.65M cluster entirely
  - cap@300/@150  : winsorize every cluster's death-weight at the cap
"""
import csv
import sys
from collections import defaultdict
TD=sys.argv[1] if len(sys.argv)>1 else '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv'
SOI='/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax/estate_tax_filed_2019_2023.csv'
RATE=0.40
NW={2018:105951254,2019:112549872,2020:120073861,2021:144807269,2022:146284336}
BEA={2018:11.18e6,2019:11.40e6,2020:11.58e6,2021:11.70e6,2022:12.06e6}
YEARS=[2018,2019,2020,2021,2022]
ASSET=['value.cash','value.equities','value.bonds','value.dc','value.db','value.life_ins',
 'value.annuities','value.trusts','value.other_fin','value.pass_throughs','value.primary_home',
 'value.other_home','value.re_fund','value.other_nonfin']
DEBT=['value.primary_mortgage','value.other_mortgage','value.credit_lines','value.credit_cards',
 'value.installment_debt','value.other_debt']
BINS=[(0,10e6,'under_10m'),(10e6,20e6,'10m_20m'),(20e6,50e6,'20m_50m'),(50e6,9e99,'50m_plus')]
def bin_of(g):
    for lo,hi,lab in BINS:
        if g<hi:return lab
    return '50m_plus'
def num(s):
    s=(s or '').strip()
    if s in ('','NA','NaN','None'):return 0.0
    try:return float(s)
    except:return 0.0

soi=list(csv.DictReader(open(SOI)))
def soi_params(Y):
    fy=str(Y+1); fd={};pd={};fdsue={};cnt={};tax={}
    for _,_,b in BINS:
        r=next(x for x in soi if x['year']==fy and x['tax_status']=='taxable' and x['size_bin']==b)
        gn=num(r['gross_estate_for_tax_purposes_n']);ga=num(r['gross_estate_for_tax_purposes_amt'])
        ded=num(r['total_allowable_deductions_amt']);debt=num(r['debts_and_mortgages_amt'])
        dn=num(r['deceased_spousal_unused_exclusion_n']);da=num(r['deceased_spousal_unused_exclusion_amt'])
        fd[b]=(ded-debt)/ga if ga else 0; pd[b]=dn/gn if gn else 0
        fdsue[b]=(da/dn)/(ga/gn) if dn and gn else 0
        cnt[b]=gn; tax[b]=num(r['net_estate_tax_amt'])
    return fd,pd,fdsue,cnt,tax

# load records + compute 2022-level cluster death-weight
recs=[]
clus_dw=defaultdict(float)
with open(TD) as fh:
    rdr=csv.reader(fh);hdr=next(rdr);ix={n:i for i,n in enumerate(hdr)}
    iw=ix['weight'];ifs=ix['filing_status'];iq1=ix['q_death1'];iq2=ix['q_death2']
    ia=[ix[c] for c in ASSET];idb=[ix[c] for c in DEBT]
    for r in rdr:
        w=num(r[iw])
        if w<=0:continue
        g=sum(num(r[j]) for j in ia)
        if g<=0:continue
        d=sum(num(r[j]) for j in idb);q1=num(r[iq1]);q2=num(r[iq2])
        married=(r[ifs]=='2' and q2>0)
        recs.append((w,g,d,q1,q2,married))
        if g>=10e6:
            clus_dw[round(g)] += w*(q1*q2 if married else q1)

def scale_factor(g, variant):
    if g<10e6: return 1.0
    key=round(g); dw=clus_dw[key]
    if variant=='baseline': return 1.0
    if variant=='drop_1765': return 0.0 if abs(g-17.647e6)<1e3 else 1.0
    if variant.startswith('cap'):
        cap=float(variant[3:])
        return min(1.0, cap/dw) if dw>cap else 1.0
    return 1.0

def run(variant):
    accmc=defaultdict(float);accsc=defaultdict(float);accmt=defaultdict(float);accst=defaultdict(float)
    for Y in YEARS:
        fd,pd,fdsue,sc,st=soi_params(Y); s=NW[Y]/NW[2022]; ex=BEA[Y]
        for (w,g0,d0,q1,q2,married) in recs:
            sf=scale_factor(g0,variant)
            if sf==0:continue
            w_eff=w*sf
            g=g0*s; d=d0*s; b=bin_of(g)
            taxable=max(g-d-fd[b]*g,0)
            if married:
                tax=max(taxable-2*ex,0)*RATE;mort=q1*q2
                if tax>0: accmc[b]+=w_eff*mort
                accmt[b]+=w_eff*mort*tax
            else:
                dsue=fdsue[b]*g
                lw=max(taxable-ex,0)*RATE;lwd=max(taxable-dsue-ex,0)*RATE
                el=pd[b]*lwd+(1-pd[b])*lw;mort=q1
                if (taxable-ex)>0: accmc[b]+=w_eff*mort
                accmt[b]+=w_eff*mort*el
        for _,_,b in BINS:
            accsc[b]+=sc[b]; accst[b]+=st[b]/1e9
    n=len(YEARS)
    return {b:(accmc[b]/n,accsc[b]/n,accmt[b]/n,accst[b]/n) for _,_,b in BINS}

print("5-year-averaged shape, MODEL (r=1) vs SOI, under clone down-weighting variants")
print("(count and tax$B per year; cnt_err vs SOI count)\n")
for variant in ['baseline','drop_1765','cap300.0','cap150.0']:
    res=run(variant)
    print(f"### variant = {variant}")
    print(f"{'bin':>10} | {'mdl_cnt':>8} {'soi_cnt':>8} {'cnt_err':>8} | {'mdl_tax$B':>9} {'soi_tax$B':>9}")
    tmc=tsc=0
    for _,_,b in BINS:
        mc,sc,mt,st=res[b]; ce=mc/sc-1 if sc else 0
        print(f"{b:>10} | {mc:>8.0f} {sc:>8.0f} {ce:>+7.0%} | {mt:>9.1f} {st:>9.1f}")
        tmc+=mc;tsc+=sc
    print(f"{'TOTAL':>10} | {tmc:>8.0f} {tsc:>8.0f} {tmc/tsc-1:>+7.0%} |\n")

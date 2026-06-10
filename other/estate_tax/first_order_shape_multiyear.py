#!/usr/bin/env python3
"""
Multi-year version of the uncalibrated first-order shape diagnostic.

The model has ONE wealth year (Tax-Data death year 2022). To ask "is 2022 a weird
target?", we view that fixed 2022 wealth through each SOI death year's lens:

  - deflate 2022 wealth to year Y:  W_Y = W_2022 * (NW_Y / NW_2022)   [FRED TNWBSHNO]
  - apply year Y's actual exemption (BEA_Y)
  - estimate f_ded / p_dsue / f_dsue from year Y's SOI (filing year Y+1)
  - compare modeled taxable count & tax to year Y's SOI

Reporting factor still = 1 (no calibration). Same joint/single reduced form.
Averaging across years denoises the lumpy top and reveals whether 2022 is an outlier.
"""
import csv
import sys
from collections import defaultdict

TD=sys.argv[1] if len(sys.argv)>1 else '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv'
SOI = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax/estate_tax_filed_2019_2023.csv'
RATE = 0.40

# FRED household net worth (annual avg), indexed by DEATH year; only ratios used.
NW = {2018:105951254, 2019:112549872, 2020:120073861, 2021:144807269, 2022:146284336}
# Applicable exclusion by DEATH year.
BEA = {2018:11.18e6, 2019:11.40e6, 2020:11.58e6, 2021:11.70e6, 2022:12.06e6}
DEATH_YEARS = [2018,2019,2020,2021,2022]

ASSET_COLS = ['value.cash','value.equities','value.bonds','value.dc','value.db',
              'value.life_ins','value.annuities','value.trusts','value.other_fin',
              'value.pass_throughs','value.primary_home','value.other_home',
              'value.re_fund','value.other_nonfin']
DEBT_COLS = ['value.primary_mortgage','value.other_mortgage','value.credit_lines',
             'value.credit_cards','value.installment_debt','value.other_debt']
BINS = [(0,10e6,'under_10m'),(10e6,20e6,'10m_20m'),(20e6,50e6,'20m_50m'),(50e6,9e99,'50m_plus')]
def bin_of(g):
    for lo,hi,lab in BINS:
        if g < hi: return lab
    return '50m_plus'
def num(s):
    s=(s or '').strip()
    if s in ('','NA','NaN','None'): return 0.0
    try: return float(s)
    except: return 0.0

# ---- SOI params + targets by death year ----
soi = list(csv.DictReader(open(SOI)))
def soi_params(death_year):
    fy = str(death_year+1)
    fd={}; pd={}; fdsue={}; cnt={}; tax={}
    for _,_,b in BINS:
        r = next(x for x in soi if x['year']==fy and x['tax_status']=='taxable' and x['size_bin']==b)
        gn=num(r['gross_estate_for_tax_purposes_n']); ga=num(r['gross_estate_for_tax_purposes_amt'])
        ded=num(r['total_allowable_deductions_amt']); debt=num(r['debts_and_mortgages_amt'])
        dn=num(r['deceased_spousal_unused_exclusion_n']); da=num(r['deceased_spousal_unused_exclusion_amt'])
        fd[b]=(ded-debt)/ga if ga else 0.0
        pd[b]=dn/gn if gn else 0.0
        fdsue[b]=(da/dn)/(ga/gn) if dn and gn else 0.0
        cnt[b]=gn; tax[b]=num(r['net_estate_tax_amt'])
    return fd,pd,fdsue,cnt,tax

# ---- Load Tax-Data 2022 once into memory ----
recs=[]
with open(TD) as fh:
    rdr=csv.reader(fh); hdr=next(rdr); ix={n:i for i,n in enumerate(hdr)}
    iw=ix['weight']; ifs=ix['filing_status']; iq1=ix['q_death1']; iq2=ix['q_death2']
    ia=[ix[c] for c in ASSET_COLS]; idb=[ix[c] for c in DEBT_COLS]
    for row in rdr:
        w=num(row[iw])
        if w<=0: continue
        g=sum(num(row[j]) for j in ia)
        if g<=0: continue
        d=sum(num(row[j]) for j in idb); q1=num(row[iq1]); q2=num(row[iq2])
        married=(row[ifs]=='2' and q2>0)
        recs.append((w,g,d,q1,q2,married))
print(f"Loaded {len(recs):,} Tax-Data 2022 records (gross assets > 0)\n")

def run_year(Y):
    fd,pd,fdsue,soi_cnt,soi_tax = soi_params(Y)
    s = NW[Y]/NW[2022]      # deflate 2022 wealth to year Y
    ex = BEA[Y]
    mc=defaultdict(float); mt=defaultdict(float)
    for (w,g0,d0,q1,q2,married) in recs:
        g=g0*s; d=d0*s
        b=bin_of(g)
        taxable=max(g - d - fd[b]*g, 0.0)
        if married:
            tax=max(taxable-2*ex,0.0)*RATE; mort=q1*q2
            if tax>0: mc[b]+=w*mort
            mt[b]+=w*mort*tax
        else:
            dsue=fdsue[b]*g
            lw=max(taxable-ex,0.0)*RATE; lwd=max(taxable-dsue-ex,0.0)*RATE
            el=pd[b]*lwd+(1-pd[b])*lw; mort=q1
            if (taxable-ex)>0: mc[b]+=w*mort
            mt[b]+=w*mort*el
    return mc,mt,soi_cnt,soi_tax

# ---- Per-year totals ----
print("="*86)
print("UNCALIBRATED MODEL (r=1) vs SOI, by death year (2022 wealth deflated to each year)")
print("="*86)
print(f"{'death_yr':>8} {'NW_ratio':>8} {'exempt$M':>9} | {'mdl_cnt':>8} {'soi_cnt':>8} {'cnt_err':>8} | {'mdl_tax$B':>9} {'soi_tax$B':>9} {'tax_err':>8}")
print("-"*86)
acc=defaultdict(lambda: defaultdict(float))
avg=defaultdict(float)
for Y in DEATH_YEARS:
    mc,mt,sc,st = run_year(Y)
    tmc=sum(mc.values()); tsc=sum(sc.values()); tmt=sum(mt.values())/1e9; tst=sum(st.values())/1e9
    print(f"{Y:>8} {NW[Y]/NW[2022]:>8.3f} {BEA[Y]/1e6:>9.2f} | {tmc:>8.0f} {tsc:>8.0f} {tmc/tsc-1:>+7.0%} | {tmt:>9.1f} {tst:>9.1f} {tmt/tst-1:>+7.0%}")
    for _,_,b in BINS:
        acc[b]['mc']+=mc[b]; acc[b]['sc']+=sc[b]; acc[b]['mt']+=mt[b]/1e9; acc[b]['st']+=st[b]/1e9
    avg['mc']+=tmc; avg['sc']+=tsc; avg['mt']+=tmt; avg['st']+=tst
n=len(DEATH_YEARS)
print("-"*86)
print(f"{'AVG':>8} {'':>8} {'':>9} | {avg['mc']/n:>8.0f} {avg['sc']/n:>8.0f} {avg['mc']/avg['sc']-1:>+7.0%} | {avg['mt']/n:>9.1f} {avg['st']/n:>9.1f} {avg['mt']/avg['st']-1:>+7.0%}")

# ---- 5-year-averaged shape by bin ----
print()
print("5-year-averaged shape by bin (denoised):")
print(f"{'bin':>10} | {'mdl_cnt':>8} {'soi_cnt':>8} {'cnt_err':>8} | {'mdl_tax$B':>9} {'soi_tax$B':>9} {'tax_err':>8}")
print("-"*70)
for _,_,b in BINS:
    a=acc[b]
    ce=a['mc']/a['sc']-1 if a['sc'] else 0; te=a['mt']/a['st']-1 if a['st'] else 0
    print(f"{b:>10} | {a['mc']/n:>8.0f} {a['sc']/n:>8.0f} {ce:>+7.0%} | {a['mt']/n:>9.1f} {a['st']/n:>9.1f} {te:>+7.0%}")

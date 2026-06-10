#!/usr/bin/env python3
"""
First-order, UNCALIBRATED estate-tax shape diagnostic.

Implements the joint/single reduced form with NO calibration:
  - reporting factor f(assets) = 1  (gross estate = gross assets, no valuation discount)
  - f_ded, f_dsue, p_dsue are ESTIMATED directly from SOI (not optimized)

Per record (death year 2022, applied at the actual 2022 exemption):
  gross    = sum(14 asset value.* cols)                       # r = 1
  debts    = sum(6 debt value.* cols)                         # actual micro debts
  f_ded    = SOI non-debt deductions / gross, by gross bin    # marital+charity+admin+state
  taxable  = max(gross - debts - f_ded*gross, 0)
  JOINT  (filing_status==2 & q2>0): both-die event
      tax  = max(taxable - 2*exemption, 0) * rate
      E[.] = weight * q1 * q2 * tax
  SINGLE (everyone else): single-death event, DSUE blend
      dsue        = f_dsue * gross                            # mean dsue/gross among claimants
      liab_without= max(taxable -            exemption, 0) * rate
      liab_with   = max(taxable - dsue     - exemption, 0) * rate
      E[liab]     = p_dsue*liab_with + (1-p_dsue)*liab_without
      E[.] = weight * q1 * E[liab]

Compares modeled death-weighted taxable count & tax by bin to SOI death-year 2022
(= SOI filing year 2023). The point is to read the shape with r=1 BEFORE calibrating.
"""
import csv
from collections import defaultdict

TD = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv'
SOI = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax/estate_tax_filed_2019_2023.csv'

EXEMPTION = 12.06e6          # 2022 applicable exclusion
RATE = 0.40                  # top estate rate; ~exact above the exemption
SOI_FILING_YEAR = '2023'     # death year 2022

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
    s = (s or '').strip()
    if s in ('','NA','NaN','None'): return 0.0
    try: return float(s)
    except: return 0.0

# ---- 1. Estimate f_ded, p_dsue, f_dsue from SOI (death year 2022 = filing 2023) ----
soi = list(csv.DictReader(open(SOI)))
def soi_row(b):
    return next(x for x in soi if x['year']==SOI_FILING_YEAR and x['tax_status']=='taxable' and x['size_bin']==b)
f_ded={}; p_dsue={}; f_dsue={}; soi_count={}; soi_tax={}
for _,_,b in BINS:
    r = soi_row(b)
    gross_n = num(r['gross_estate_for_tax_purposes_n'])
    gross   = num(r['gross_estate_for_tax_purposes_amt'])
    ded     = num(r['total_allowable_deductions_amt'])
    debt    = num(r['debts_and_mortgages_amt'])
    dn      = num(r['deceased_spousal_unused_exclusion_n'])
    da      = num(r['deceased_spousal_unused_exclusion_amt'])
    f_ded[b]   = (ded - debt) / gross          # non-debt deductions as share of gross
    p_dsue[b]  = dn / gross_n if gross_n else 0.0
    f_dsue[b]  = (da/dn)/(gross/gross_n) if dn and gross_n else 0.0   # mean dsue / mean gross
    soi_count[b] = gross_n
    soi_tax[b]   = num(r['net_estate_tax_amt'])

print("SOI-estimated parameters (death year 2022):")
print(f"{'bin':>10} {'f_ded':>7} {'p_dsue':>7} {'f_dsue':>7}")
for _,_,b in BINS:
    print(f"{b:>10} {f_ded[b]:>7.3f} {p_dsue[b]:>7.3f} {f_dsue[b]:>7.3f}")
print()

# ---- 2. Stream Tax-Data 2022, apply the uncalibrated model ----
m_count=defaultdict(float); m_tax=defaultdict(float)            # taxable, death-weighted
m_count_j=defaultdict(float); m_count_s=defaultdict(float)
m_tax_j=defaultdict(float); m_tax_s=defaultdict(float)
raw_pop=defaultdict(float)                                       # death-weighted, pre-taxable filter
with open(TD) as fh:
    rdr=csv.reader(fh); hdr=next(rdr); ix={n:i for i,n in enumerate(hdr)}
    iw=ix['weight']; ifs=ix['filing_status']; iq1=ix['q_death1']; iq2=ix['q_death2']
    ia=[ix[c] for c in ASSET_COLS]; idb=[ix[c] for c in DEBT_COLS]
    for row in rdr:
        w=num(row[iw])
        if w<=0: continue
        gross=sum(num(row[j]) for j in ia)
        if gross<=0: continue
        debts=sum(num(row[j]) for j in idb)
        q1=num(row[iq1]); q2=num(row[iq2])
        married = (row[ifs]=='2' and q2>0)
        b = bin_of(gross)
        raw_pop[b] += w*(q1*q2 if married else q1)
        taxable = max(gross - debts - f_ded[b]*gross, 0.0)
        if married:
            tax = max(taxable - 2*EXEMPTION, 0.0)*RATE
            mort = q1*q2
            if tax>0:
                m_count_j[b]+=w*mort; m_count[b]+=w*mort
            m_tax_j[b]+=w*mort*tax; m_tax[b]+=w*mort*tax
        else:
            dsue = f_dsue[b]*gross
            liab_wo = max(taxable - EXEMPTION, 0.0)*RATE
            liab_w  = max(taxable - dsue - EXEMPTION, 0.0)*RATE
            eliab = p_dsue[b]*liab_w + (1-p_dsue[b])*liab_wo
            mort=q1
            # "taxable" for the count = base exceeds exemption (use no-dsue threshold)
            if (taxable - EXEMPTION) > 0:
                m_count_s[b]+=w*mort; m_count[b]+=w*mort
            m_tax_s[b]+=w*mort*eliab; m_tax[b]+=w*mort*eliab

# ---- 3. Compare to SOI ----
print("="*92)
print("UNCALIBRATED MODEL (r=1) vs SOI, death year 2022")
print("="*92)
print(f"{'bin':>10} | {'mdl_cnt':>8} {'soi_cnt':>8} {'cnt_err':>8} | {'mdl_tax$B':>9} {'soi_tax$B':>9} {'tax_err':>8} | {'cnt_J':>7} {'cnt_S':>7}")
print("-"*92)
tot=defaultdict(float)
for _,_,b in BINS:
    mc=m_count[b]; sc=soi_count[b]; mt=m_tax[b]/1e9; st=soi_tax[b]/1e9
    ce=(mc/sc-1) if sc else 0; te=(mt/st-1) if st else 0
    print(f"{b:>10} | {mc:>8.0f} {sc:>8.0f} {ce:>+7.0%} | {mt:>9.1f} {st:>9.1f} {te:>+7.0%} | {m_count_j[b]:>7.0f} {m_count_s[b]:>7.0f}")
    tot['mc']+=mc; tot['sc']+=sc; tot['mt']+=mt; tot['st']+=st; tot['cj']+=m_count_j[b]; tot['cs']+=m_count_s[b]
print("-"*92)
ce=tot['mc']/tot['sc']-1; te=tot['mt']/tot['st']-1
print(f"{'TOTAL':>10} | {tot['mc']:>8.0f} {tot['sc']:>8.0f} {ce:>+7.0%} | {tot['mt']:>9.1f} {tot['st']:>9.1f} {te:>+7.0%} | {tot['cj']:>7.0f} {tot['cs']:>7.0f}")
print()
print("raw_pop (death-weighted decedents by gross bin, before taxable filter):")
for _,_,b in BINS:
    print(f"  {b:>10}: {raw_pop[b]:>12,.0f}")

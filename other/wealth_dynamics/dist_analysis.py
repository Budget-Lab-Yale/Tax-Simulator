import csv, os, sys
V="/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/202606241723"
YEAR="2031"
def load(sc,pas):
    p=os.path.join(V,sc,pas,'detail',YEAR+'.csv'); d={}
    with open(p,newline='') as f:
        r=csv.reader(f); hdr=next(r)
        iw=hdr.index('weight'); inw=hdr.index('net_worth'); iid=hdr.index('id')
        for row in r:
            d[row[iid]]=(float(row[iw]), float(row[inw]))
    return d
base=load('baseline','static')          # no reform: pre-everything net worth
s0  =load('wealth_warren_s0','conventional')   # reform + avoidance, channel OFF
s50 =load('wealth_warren_s50','conventional')  # reform + avoidance, channel ON
ids=[i for i in base if i in s0 and i in s50]
# rank by baseline net worth (positive only), weighted
recs=[]
for i in ids:
    w,nb=base[i]
    if nb>0: recs.append((nb, w, s0[i][1], s50[i][1]))
recs.sort(key=lambda x:x[0])
W=sum(r[1] for r in recs)
groups=[("Bottom 50%",0,.50),("50–90%",.50,.90),("90–99%",.90,.99),
        ("99–99.9%",.99,.999),("Top 0.1%",.999,1.0001)]
cum=0; agg={g[0]:[0,0,0,0] for g in groups}  # [Wsum, base, s0, s50] in $
for nb,w,n0,n50 in recs:
    p0=cum/W; cum+=w; p1=cum/W; pmid=(p0+p1)/2
    for name,lo,hi in groups:
        if lo<=pmid<hi:
            a=agg[name]; a[0]+=w; a[1]+=w*nb; a[2]+=w*n0; a[3]+=w*n50; break
tot_drain=sum((agg[g[0]][2]-agg[g[0]][3]) for g in groups)
print(f"WARREN net-worth tax, {YEAR} — net worth by wealth group ($T), ranked by baseline NW")
print(f"{'group':<12}{'baseline':>10}{'s=0':>10}{'s=0.5':>10}{'drained':>9}{'%ofdrain':>9}{'chan%chg':>9}{'vsbase%':>9}")
for name,_,_ in [(g[0],0,0) for g in groups]:
    Wn,b,n0,n50=agg[name]
    drain=(n0-n50)/1e12
    chanpct=100*(n50-n0)/n0 if n0 else 0
    vsbase=100*(n50-b)/b if b else 0
    print(f"{name:<12}{b/1e12:>10.2f}{n0/1e12:>10.2f}{n50/1e12:>10.2f}{drain:>9.3f}{100*drain*1e12/tot_drain:>8.1f}%{chanpct:>8.2f}%{vsbase:>8.2f}%")
print(f"{'TOTAL':<12}{sum(agg[g[0]][1] for g in groups)/1e12:>10.2f}{sum(agg[g[0]][2] for g in groups)/1e12:>10.2f}{sum(agg[g[0]][3] for g in groups)/1e12:>10.2f}{tot_drain/1e12:>9.3f}{100:>8.1f}%")

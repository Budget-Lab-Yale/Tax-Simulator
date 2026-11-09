suppressMessages(library(data.table))
ROOT<-"/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"; V<-"warren_bound_identity"
ws<-function(nw,w,f){o<-order(nw,decreasing=TRUE);nw<-nw[o];w<-w[o];cut<-f*sum(w);cw<-cumsum(w);k<-which(cw>=cut)[1];p<-if(k>1)cw[k-1] else 0;fu<-if(k>1)sum(nw[1:(k-1)]*w[1:(k-1)]) else 0;100*(fu+nw[k]*(cut-p))/sum(nw*w)}
ec<-function(sc,k,y,join_dalloc){st<-fread(file.path(ROOT,V,sc,k,"detail",paste0(y,".csv")),select=c("id","weight","net_worth"))
  if(join_dalloc){cv<-fread(file.path(ROOT,V,sc,"conventional","detail",paste0(y,".csv")),select=c("id","D_alloc"));st<-merge(st,cv,by="id",all.x=TRUE);st[is.na(D_alloc),D_alloc:=0];st[,dr:=pmin(D_alloc,pmax(net_worth,0))];st[,e:=net_worth-dr]}else st[,e:=net_worth]
  data.table(sc=sc,y=y,top1=ws(st$e,st$weight,.01),top01=ws(st$e,st$weight,.001))}
cat("IDENTITY-M economic top shares (s=1):\n")
print(rbind(ec("baseline","static",2036,FALSE), ec("warren_s100","static",2036,TRUE)))

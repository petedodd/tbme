## this file is for generating summary tables (run after estimates)
## NOTE writes to folder results, which is in the repo - no large files or private data pls
library(here)
source(here('utilities.R'))

## loading
load(file=here('outdata/AN.Rdata'))



## global
G <- AN[,.(TBM.treated=sum(TBM.treated),TBM.untreated=sum(TBM.untreated),
           TBMdeaths.treated=sum(TBMdeaths.treated),
           TBMdeaths.untreated=sum(TBMdeaths.untreated))]
GA <- AN[,.(TBM.treated=sum(TBM.treated),TBM.untreated=sum(TBM.untreated),
            TBMdeaths.treated=sum(TBMdeaths.treated),
            TBMdeaths.untreated=sum(TBMdeaths.untreated)),by=.(sex,age)]
GA

## regional
R <- AN[,.(TBM.treated=sum(TBM.treated),TBM.untreated=sum(TBM.untreated),
           TBMdeaths.treated=sum(TBMdeaths.treated),
           TBMdeaths.untreated=sum(TBMdeaths.untreated)),by=g_whoregion]
RA <- AN[,.(TBM.treated=sum(TBM.treated),TBM.untreated=sum(TBM.untreated),
            TBMdeaths.treated=sum(TBMdeaths.treated),
            TBMdeaths.untreated=sum(TBMdeaths.untreated)),
         by=.(g_whoregion,sex,age)]
R

G[,g_whoregion:='Global']
GR <- rbind(R[order(g_whoregion)],G)


GRO <- cbind(Region=GR$g_whoregion,GR[,lapply(.SD,see),.SDcols=2:5])

## save out
fwrite(GRO,file=here('results/GRO.csv'))

save(G,file=here('outdata/G.Rdata'))
save(GA,file=here('outdata/GA.Rdata'))
save(R,file=here('outdata/R.Rdata'))
save(RA,file=here('outdata/RA.Rdata'))

## check aggrergate implied CFR
(1e2*G[,TBMdeaths.treated/TBM.treated]) #18.5%, cf 0.16 (0.10, 0.24) in Anna


## TODO list
## 1. Pete  - sketch table structure & unc + format approach
## 2. Anna - include uncertainty and formatting

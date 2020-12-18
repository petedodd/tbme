## this file is for generating summary tables (run after estimates)
## NOTE writes to folder results, which is in the repo - no large files or private data pls
library(here)
source(here('utilities.R'))

## loading
load(file=here('outdata/AN.Rdata'))


## === for tables
## --- global
G <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) + sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) + sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) + sum(TBMdeaths.hp.untreated))]



## --- regional
R <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) + sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) + sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) + sum(TBMdeaths.hp.untreated)),
        by=g_whoregion]
R

## TODO uncertainty calculations
## ie analog of above for sd's: replace sum with ssum and similarly .+...+. with sqrt( .^2 + ...+.^2)

G[,g_whoregion:='Global']
GR <- rbind(R[order(g_whoregion)],G)
GRO <- cbind(Region=GR$g_whoregion,GR[,lapply(.SD,seer),.SDcols=2:ncol(GR)])

## TODO use sd's to generate UIs

## --- save/write
fwrite(GRO,file=here('results/GRO.csv'))
save(G,file=here('outdata/G.Rdata'))
save(R,file=here('outdata/R.Rdata'))


## === for graphs

## global
GA <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated)),by=.(sex,age)]

## regional
RA <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
            TBM.hp.treated=sum(TBM.hp.treated),
            TBM.hn.untreated=sum(TBM.hn.untreated),
            TBM.hp.untreated=sum(TBM.hp.untreated),
            TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
            TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
            TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
            TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated)),
         by=.(g_whoregion,sex,age)]

## save out
save(RA,file=here('outdata/RA.Rdata'))
save(GA,file=here('outdata/GA.Rdata'))
## check aggrergate implied CFR
(1e2*G[,TBMdeaths.hn.treated/TBM.hn.treated]) # 18.5%, cf 0.16 (0.10, 0.24) in Anna
(1e2*G[,TBMdeaths.hp.treated/TBM.hp.treated]) # 52% - bit low?


## TODO list
## 1. Pete  - sketch table structure & unc + format approach DONE
## 2. Anna - include uncertainty and formatting

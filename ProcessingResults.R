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
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated))]
G


## --- regional
R <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated)),
        by=g_whoregion]

R

## TODO uncertainty calculations
## ie analog of above for sd's: replace sum with ssum and similarly .+...+. with sqrt( .^2 + ...+.^2)

G[,g_whoregion:='Global']
GR <- rbind(R[order(g_whoregion)],G)
GRO <- cbind(Region=GR$g_whoregion,
             GR[,lapply(.SD,seer),.SDcols=2:ncol(GR)])

## TODO use sd's to generate UIs



## version like (I,D) x (M,F,T) x (H+,H-,T)
## calculate
G2 <- AN[,
         .(
             TBM.hp = sum(TBM.hp.treated) + sum(TBM.hp.untreated),
             TBM.hn = sum(TBM.hn.treated) + sum(TBM.hn.untreated),
             TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
                 sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
             TBMdeaths.hp = sum(TBMdeaths.hp.treated) +
                 sum(TBMdeaths.hp.untreated),
             TBMdeaths.hn = sum(TBMdeaths.hn.treated) +
                 sum(TBMdeaths.hn.untreated),
             TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated)
         ),
         by=.(age,sex)]
G2

## reshape
G2 <- melt(G2,id=c('age','sex'))
G2[,qty:=ifelse(grepl('deaths',variable),'deaths','incidence')]
G2[,hiv:=ifelse(grepl('total',variable),'total','HIV-negative')]
G2[grepl('hp',variable),hiv:='HIV-positive']
G2[,c('variable'):=NULL] #ditch

tmp2 <- G2[,.(value=sum(value)),by=.(qty,sex,hiv)]
tmp2[,age:='total'] #one margin

tmp <- G2[,.(value=sum(value)),by=.(age,qty,hiv)]
tmp[,sex:='total'] #one margin

tmp3 <- tmp[,.(value=sum(value)),by=.(qty,hiv)]
tmp3[,c('sex','age'):='total'] #'corner'

## tmp2 <- G2[,.(value=sum(value)),by=.(qty,hiv)]
## tmp2[,age:='total'] #last corner

G2 <- rbind(G2,tmp) #join
G2 <- rbind(G2,tmp2)
G2 <- rbind(G2,tmp3)

G2 <- dcast(G2,qty + sex + hiv ~ age,value.var = 'value')
G2$qty <- factor(G2$qty,levels=c('incidence','deaths'),ordered = TRUE)
G2$sex <- factor(G2$sex,levels=c('F','M','total'),ordered = TRUE)
G2$hiv <- factor(G2$hiv,levels=c('HIV-negative','HIV-positive','total'),
                 ordered = TRUE)
G2 <- G2[order(qty,sex,hiv)]

## format
GO2 <- cbind(G2[,.(qty,sex,hiv)],
             G2[,lapply(.SD,seer),.SDcols=4:ncol(G2)])

## --- save/write
fwrite(GRO,file=here('results/GRO.csv'))
GROt <- transpose(GRO)
GROt <- cbind(data.table(quantity=c(names(GRO))),GROt)
fwrite(GROt,file=here('results/GROt.csv'),col.names = FALSE)
save(G,file=here('outdata/G.Rdata'))
save(R,file=here('outdata/R.Rdata'))

fwrite(GO2,file=here('results/GO2.csv'))

## === for graphs

## global
GA <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated)),
         by=.(sex,age)]

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
## table that is:

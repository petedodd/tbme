## this file is for generating summary tables (run after estimates)
## NOTE writes to folder results, which is in the repo - no large files or private data pls
library(here)
source(here('utilities.R'))

## loading
load(file=here('outdata/AN.Rdata'))

## write out country level
ANO <- copy(AN)
ind <-   which(sapply(ANO, is.numeric))
for(j in ind){ #clean out few NAs in sd (result from 0 in denom)
    set(ANO, i = which(is.na(ANO[[j]])), j = j, value = 0)
}
fwrite(ANO,file=here('results/country.level.csv'))

## === for tables
## --- global
G <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBM.untreated=sum(TBM.hn.untreated)+sum(TBM.hp.untreated),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated)+
               sum(TBMdeaths.hp.untreated),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated))]
G

## sensitivity analysis version 2
G.sa2 <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated2),
           TBM.hp.untreated=sum(TBM.hp.untreated2),
           TBM.untreated=sum(TBM.hn.untreated2)+sum(TBM.hp.untreated2),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated2),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated2),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated2) + sum(TBM.hp.untreated2),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated2),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated2),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated2)+
               sum(TBMdeaths.hp.untreated2),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated2),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated2),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated2) +
               sum(TBMdeaths.hp.untreated2))]
G.sa2

## sensitivity analysis version 3
G.sa3 <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated3),
           TBM.hp.untreated=sum(TBM.hp.untreated3),
           TBM.untreated=sum(TBM.hn.untreated3)+sum(TBM.hp.untreated3),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated3),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated3),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated3) + sum(TBM.hp.untreated3),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated3),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated3),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated3)+
               sum(TBMdeaths.hp.untreated3),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated3),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated3),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated3) +
               sum(TBMdeaths.hp.untreated3))]
G.sa3

## uncertainty
G.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated.sd)^2+
                                 ssum(TBM.hp.untreated.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated.sd)^2 +
                               ssum(TBM.hp.untreated.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated.sd)^2+
               ssum(TBMdeaths.hp.untreated.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated.sd)^2))]
G.sd

G.sa2.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated2.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated2.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated2.sd)^2+
                                 ssum(TBM.hp.untreated2.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated2.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated2.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated2.sd)^2 +
                               ssum(TBM.hp.untreated2.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated2.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated2.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated2.sd)^2+
               ssum(TBMdeaths.hp.untreated2.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated2.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated2.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated2.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated2.sd)^2))
           ]
G.sa2.sd


G.sa3.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated3.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated3.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated3.sd)^2+
                                 ssum(TBM.hp.untreated3.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated3.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated3.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated3.sd)^2 +
                               ssum(TBM.hp.untreated3.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated3.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated3.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated3.sd)^2+
               ssum(TBMdeaths.hp.untreated3.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated3.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated3.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated3.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated3.sd)^2))
           ]
G.sa3.sd

## ----------------------------------------------------
## --- regional
R <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated),
           TBM.hp.untreated=sum(TBM.hp.untreated),
           TBM.untreated=sum(TBM.hn.untreated)+sum(TBM.hp.untreated),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated)+
               sum(TBMdeaths.hp.untreated),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated)),
        by=g_whoregion]
R


## sensitivity analysis version 2
R.sa2 <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated2),
           TBM.hp.untreated=sum(TBM.hp.untreated2),
           TBM.untreated=sum(TBM.hn.untreated2)+sum(TBM.hp.untreated2),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated2),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated2),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated2) + sum(TBM.hp.untreated2),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated2),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated2),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated2)+
               sum(TBMdeaths.hp.untreated2),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated2),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated2),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated2) +
               sum(TBMdeaths.hp.untreated2)),
           by=g_whoregion]
R.sa2

## sensitivity analysis version 3
R.sa3 <- AN[,.(TBM.hn.treated=sum(TBM.hn.treated),
           TBM.hp.treated=sum(TBM.hp.treated),
           TBM.treated = sum(TBM.hn.treated)+sum(TBM.hp.treated),
           TBM.hn.untreated=sum(TBM.hn.untreated3),
           TBM.hp.untreated=sum(TBM.hp.untreated3),
           TBM.untreated=sum(TBM.hn.untreated3)+sum(TBM.hp.untreated3),
           TBM.hn.total = sum(TBM.hn.treated) + sum(TBM.hn.untreated3),
           TBM.hp.total = sum(TBM.hp.treated) + sum(TBM.hp.untreated3),
           TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated3) + sum(TBM.hp.untreated3),
           TBMdeaths.hn.treated=sum(TBMdeaths.hn.treated),
           TBMdeaths.hp.treated=sum(TBMdeaths.hp.treated),
           TBMdeaths.treated=sum(TBMdeaths.hn.treated)+
               sum(TBMdeaths.hp.treated),
           TBMdeaths.hn.untreated=sum(TBMdeaths.hn.untreated3),
           TBMdeaths.hp.untreated=sum(TBMdeaths.hp.untreated3),
           TBMdeaths.untreated=sum(TBMdeaths.hn.untreated3)+
               sum(TBMdeaths.hp.untreated3),
           TBMdeaths.hn.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hn.untreated3),
           TBMdeaths.hp.total = sum(TBMdeaths.hp.treated) +
               sum(TBMdeaths.hp.untreated3),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated3) +
               sum(TBMdeaths.hp.untreated3)),
           by=g_whoregion]
R.sa3

## uncertainty
R.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated.sd)^2+
                                 ssum(TBM.hp.untreated.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated.sd)^2 +
                               ssum(TBM.hp.untreated.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated.sd)^2+
               ssum(TBMdeaths.hp.untreated.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated.sd)^2)),
           by=g_whoregion]
R.sd

R.sa2.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated2.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated2.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated2.sd)^2+
                                 ssum(TBM.hp.untreated2.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated2.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated2.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated2.sd)^2 +
                               ssum(TBM.hp.untreated2.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated2.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated2.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated2.sd)^2+
               ssum(TBMdeaths.hp.untreated2.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated2.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated2.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated2.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated2.sd)^2))
         , by=g_whoregion]
R.sa2.sd


R.sa3.sd <- AN[,.(TBM.hn.treated.sd=ssum(TBM.hn.treated.sd),
           TBM.hp.treated.sd=ssum(TBM.hp.treated.sd),
           TBM.treated.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2),
           TBM.hn.untreated.sd=ssum(TBM.hn.untreated3.sd),
           TBM.hp.untreated.sd=ssum(TBM.hp.untreated3.sd),
           TBM.untreated.sd=sqrt(ssum(TBM.hn.untreated3.sd)^2+
                                 ssum(TBM.hp.untreated3.sd)^2),
           TBM.hn.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2 +
                              ssum(TBM.hn.untreated3.sd)^2),
           TBM.hp.total.sd = sqrt(ssum(TBM.hp.treated.sd)^2 +
                                  ssum(TBM.hp.untreated3.sd)^2),
           TBM.total.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                               ssum(TBM.hp.treated.sd)^2+
                               ssum(TBM.hn.untreated3.sd)^2 +
                               ssum(TBM.hp.untreated3.sd)^2),
           TBMdeaths.hn.treated.sd=ssum(TBMdeaths.hn.treated.sd),
           TBMdeaths.hp.treated.sd=ssum(TBMdeaths.hp.treated.sd),
           TBMdeaths.treated.sd=sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hp.treated.sd)^2),
           TBMdeaths.hn.untreated.sd=ssum(TBMdeaths.hn.untreated3.sd),
           TBMdeaths.hp.untreated.sd=ssum(TBMdeaths.hp.untreated3.sd),
           TBMdeaths.untreated.sd=sqrt(ssum(TBMdeaths.hn.untreated3.sd)^2+
               ssum(TBMdeaths.hp.untreated3.sd)^2),
           TBMdeaths.hn.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
               ssum(TBMdeaths.hn.untreated3.sd)^2),
           TBMdeaths.hp.total.sd = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
               ssum(TBMdeaths.hp.untreated3.sd)^2),
           TBMdeaths.total.sd = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                     ssum(TBMdeaths.hp.treated.sd)^2+
                                     ssum(TBMdeaths.hn.untreated3.sd)^2 +
                                     ssum(TBMdeaths.hp.untreated3.sd)^2))
          ,by=g_whoregion]
R.sa3.sd


## --- examine & join
## central estimates
G[,g_whoregion:='Global']; G.sa2[,g_whoregion:='Global'];
G.sa3[,g_whoregion:='Global']
GR <- rbind(R[order(g_whoregion)],G);
GR.sa2 <- rbind(R.sa2[order(g_whoregion)],G.sa2)
GR.sa3 <- rbind(R.sa3[order(g_whoregion)],G.sa3)
## SDs
G.sd[,g_whoregion:='Global']; G.sa2.sd[,g_whoregion:='Global'];
G.sa3.sd[,g_whoregion:='Global']
GR.sd <- rbind(R.sd[order(g_whoregion)],G.sd);
GR.sa2.sd <- rbind(R.sa2.sd[order(g_whoregion)],G.sa2.sd)
GR.sa3.sd <- rbind(R.sa3.sd[order(g_whoregion)],G.sa3.sd)

## make tables
GRO <- tablemaker(GR,GR.sd)
GRO.sa2 <- tablemaker(GR.sa2,GR.sa2.sd)
GRO.sa3 <- tablemaker(GR.sa3,GR.sa3.sd)

## -------write out
fwrite(GRO,file=here('results/GRO.csv'))
GROt <- transpose(GRO)
GROt <- cbind(data.table(quantity=c(names(GRO))),GROt)
fwrite(GROt,file=here('results/GROt.csv'),col.names = FALSE) #table 2

## SAs
fwrite(GRO.sa2,file=here('results/GRO.sa2.csv'))
GROt.sa2 <- transpose(GRO.sa2)
GROt.sa2 <- cbind(data.table(quantity=c(names(GRO.sa2))),GROt.sa2)
fwrite(GROt.sa2,file=here('results/GROt.sa2.csv'),col.names = FALSE)
fwrite(GRO.sa3,file=here('results/GRO.sa3.csv'))
GROt.sa3 <- transpose(GRO.sa3)
GROt.sa3 <- cbind(data.table(quantity=c(names(GRO.sa3))),GROt.sa3)
fwrite(GROt.sa3,file=here('results/GROt.sa3.csv'),col.names = FALSE)

## seprate files (for graphs)
save(G,file=here('outdata/G.Rdata'))
save(R,file=here('outdata/R.Rdata'))
save(G.sa2,file=here('outdata/G.sa2.Rdata'))
save(R.sa2,file=here('outdata/R.sa2.Rdata'))
save(G.sa3,file=here('outdata/G.sa3.Rdata'))
save(R.sa3,file=here('outdata/R.sa3.Rdata'))
## ...SAs
save(G,file=here('outdata/G.sd.Rdata'))
save(R.sd,file=here('outdata/R.sd.Rdata'))
save(G.sa2.sd,file=here('outdata/G.sa2.sd.Rdata'))
save(R.sa2.sd,file=here('outdata/R.sa2.sd.Rdata'))
save(G.sa3.sd,file=here('outdata/G.sa3.sd.Rdata'))
save(R.sa3.sd,file=here('outdata/R.sa3.sd.Rdata'))



## ------------------ age version
## utility function for this section
reformatAS <- function(DT){
    ## reshape
    DT <- melt(DT,id=c('age','sex'))
    DT[,qty:=ifelse(grepl('deaths',variable),'deaths','incidence')]
    DT[,hiv:=ifelse(grepl('total',variable),'total','HIV-negative')]
    DT[grepl('hp',variable),hiv:='HIV-positive']
    DT[,c('variable'):=NULL] #ditch
    tmp2 <- DT[,.(value=sum(value)),by=.(qty,sex,hiv)]
    tmp2[,age:='total'] #one margin
    tmp <- DT[,.(value=sum(value)),by=.(age,qty,hiv)]
    tmp[,sex:='total'] #one margin
    tmp3 <- tmp[,.(value=sum(value)),by=.(qty,hiv)]
    tmp3[,c('sex','age'):='total'] #'corner'
    DT <- rbind(DT,tmp) #join
    DT <- rbind(DT,tmp2)
    DT <- rbind(DT,tmp3)
    DT <- dcast(DT,qty + sex + hiv ~ age,value.var = 'value')
    DT$qty <- factor(DT$qty,levels=c('incidence','deaths'),
                     ordered = TRUE)
    DT$sex <- factor(DT$sex,levels=c('F','M','total'),ordered = TRUE)
    DT$hiv <- factor(DT$hiv,levels=c('HIV-negative','HIV-positive',
                                     'total'),
                     ordered = TRUE)
    DT <- DT[order(qty,sex,hiv)]
    return(DT)
}

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

G2.sd <- AN[,
         .(
             TBM.hp.sd = sqrt(ssum(TBM.hp.treated.sd)^2+
                              ssum(TBM.hp.untreated.sd)^2),
             TBM.hn.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                              ssum(TBM.hn.untreated.sd)^2),
             TBM.total = sqrt(ssum(TBM.hn.treated.sd)^2+
                              ssum(TBM.hp.treated.sd)^2+
                              ssum(TBM.hn.untreated.sd)^2+
                              ssum(TBM.hp.untreated.sd)^2),
             TBMdeaths.hp = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
                                 ssum(TBMdeaths.hp.untreated.sd)^2),
             TBMdeaths.hn = sqrt(ssum(TBMdeaths.hn.treated.sd)^2 +
                                 ssum(TBMdeaths.hn.untreated.sd)^2),
             TBMdeaths.total = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                 ssum(TBMdeaths.hp.treated.sd)^2+
                 ssum(TBMdeaths.hn.untreated.sd)^2+
                 ssum(TBMdeaths.hp.untreated.sd)^2)
         ),
         by=.(age,sex)]
G2.sd


## SAs....
G2.sa2 <- AN[,
         .(
             TBM.hp = sum(TBM.hp.treated) + sum(TBM.hp.untreated2),
             TBM.hn = sum(TBM.hn.treated) + sum(TBM.hn.untreated2),
             TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
                 sum(TBM.hn.untreated2) + sum(TBM.hp.untreated2),
             TBMdeaths.hp = sum(TBMdeaths.hp.treated) +
                 sum(TBMdeaths.hp.untreated2),
             TBMdeaths.hn = sum(TBMdeaths.hn.treated) +
                 sum(TBMdeaths.hn.untreated2),
             TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated2) +
               sum(TBMdeaths.hp.untreated2)
         ),
         by=.(age,sex)]
G2.sa2

G2.sa2.sd <- AN[,
            .(
                TBM.hp.sd = sqrt(ssum(TBM.hp.treated.sd)^2+
                                 ssum(TBM.hp.untreated2.sd)^2),
                TBM.hn.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hn.untreated2.sd)^2),
                TBM.total = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2+
                                 ssum(TBM.hn.untreated2.sd)^2+
                                 ssum(TBM.hp.untreated2.sd)^2),
                TBMdeaths.hp = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
                                    ssum(TBMdeaths.hp.untreated2.sd)^2),
                TBMdeaths.hn = sqrt(ssum(TBMdeaths.hn.treated.sd)^2 +
                                    ssum(TBMdeaths.hn.untreated2.sd)^2),
                TBMdeaths.total = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                       ssum(TBMdeaths.hp.treated.sd)^2+
                                       ssum(TBMdeaths.hn.untreated2.sd)^2+
                                       ssum(TBMdeaths.hp.untreated2.sd)^2)
            ),
         by=.(age,sex)]
G2.sa2.sd

G2.sa3 <- AN[,
         .(
             TBM.hp = sum(TBM.hp.treated) + sum(TBM.hp.untreated3),
             TBM.hn = sum(TBM.hn.treated) + sum(TBM.hn.untreated3),
             TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
                 sum(TBM.hn.untreated3) + sum(TBM.hp.untreated3),
             TBMdeaths.hp = sum(TBMdeaths.hp.treated) +
                 sum(TBMdeaths.hp.untreated3),
             TBMdeaths.hn = sum(TBMdeaths.hn.treated) +
                 sum(TBMdeaths.hn.untreated3),
             TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated3) +
               sum(TBMdeaths.hp.untreated3)
         ),
         by=.(age,sex)]
G2.sa3


G2.sa3.sd <- AN[,
            .(
                TBM.hp.sd = sqrt(ssum(TBM.hp.treated.sd)^2+
                                 ssum(TBM.hp.untreated3.sd)^2),
                TBM.hn.sd = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hn.untreated3.sd)^2),
                TBM.total = sqrt(ssum(TBM.hn.treated.sd)^2+
                                 ssum(TBM.hp.treated.sd)^2+
                                 ssum(TBM.hn.untreated3.sd)^2+
                                 ssum(TBM.hp.untreated3.sd)^2),
                TBMdeaths.hp = sqrt(ssum(TBMdeaths.hp.treated.sd)^2+
                                    ssum(TBMdeaths.hp.untreated3.sd)^2),
                TBMdeaths.hn = sqrt(ssum(TBMdeaths.hn.treated.sd)^2 +
                                    ssum(TBMdeaths.hn.untreated3.sd)^2),
                TBMdeaths.total = sqrt(ssum(TBMdeaths.hn.treated.sd)^2+
                                       ssum(TBMdeaths.hp.treated.sd)^2+
                                       ssum(TBMdeaths.hn.untreated3.sd)^2+
                                       ssum(TBMdeaths.hp.untreated3.sd)^2)
            ),
         by=.(age,sex)]
G2.sa3.sd


## reshape
G2 <- reformatAS(G2)
G2.sd <- reformatAS(G2.sd) #SD
GASO <- tablemaker(G2,G2.sd,3)

G2.sa2 <- reformatAS(G2.sa2)
G2.sa2.sd <- reformatAS(G2.sa2.sd) #SD
GASO.sa2 <- tablemaker(G2.sa2,G2.sa2.sd,3)


G2.sa3 <- reformatAS(G2.sa3)
G2.sa3.sd <- reformatAS(G2.sa3.sd) #SD
GASO.sa3 <- tablemaker(G2.sa3,G2.sa3.sd,3)


## --- save/write
fwrite(GASO,file=here('results/GASO.csv'))
fwrite(GASO.sa2,file=here('results/GASO.sa2.csv'))
fwrite(GASO.sa3,file=here('results/GASO.sa3.csv'))

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
(1e2*G[,TBMdeaths.hn.treated/TBM.hn.treated]) # 18%, cf 0.16 (0.10, 0.24) in Anna
(1e2*G[,TBMdeaths.hp.treated/TBM.hp.treated]) # 59% - comparable with Anna


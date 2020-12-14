## sketching global estimates
library(data.table)
library(here)

## utilities
xfun <- function(X,Y,X.sd,Y.sd) X*Y*sqrt((X.sd/X)^2+(Y.sd/Y)^2) #unc for X x Y

## === now using the propm & CFR from other meta-analysis
load(here('indata/preds/TBMP.nh.Rdata'))
load(here('indata/preds/TBMP.h.Rdata'))
load(here('indata/preds/CFR.nh.Rdata'))
load(here('indata/preds/CFR.h.Rdata'))

## HIV-ves
BBM <- merge(TBMP.nh[,.(age,sex,propm=pred,propm.sd=(pred.hi-pred.lo)/3.92)],
             CFR.nh[,.(age,sex,cfr=pred,cfr.sd=(pred.hi-pred.lo)/3.92)],
             by=c('age','sex'))
BBM[,sex:=ifelse(sex=='female','F','M')]

## HIV+ves

## === load WHO notifications
## http://www.who.int/tb/country/data/download/en/
N <- fread(here('indata/TB_notifications_2020-02-24.csv'))

## below introduces an analysis of the proportions pulmonary
## relevant variables
nmz <- grep("^newrel",names(N),value=TRUE)  #keep right patterns
nmz <- nmz[!grepl("unk",nmz)] #drop unknown
nmz <- nmz[!grepl("fu|mu",nmz)]       #more unknowns dropped
nmz <- nmz[!grepl("hiv|art",nmz)]       #HIV stuff dropped
nmz <- nmz[!grepl('plus|514|04|014',nmz)] #drop children & not-child catchall
nmz <- c('iso3','year',nmz)

## reduce to relevant data
NP <- N[year==2018,..nmz]
NP <- melt(NP,id.vars = c('iso3','year'))
NP[,sex:=ifelse(grepl("f",variable),'F','M')]
NP[,age:=gsub("[a-z]|_","",variable)]
NP[,age:=gsub("(\\d{2})(\\d*)","\\1-\\2",age,perl=TRUE)]
NP[,total.notes:=sum(value,na.rm=TRUE),by=iso3]
origiso <- NP[,unique(iso3)]
NP <- NP[total.notes>200]                     #remove small-TB countries
(dropped <- setdiff(origiso,NP[,unique(iso3)]))
cat(dropped,file=here('indata/drop_lonote.txt'))
NP[age=='65-',age:='65+']

## === load WHO age-specific incidence estimates
A <- fread(here('indata/TB_burden_age_sex_2020-02-24.csv'))
## keep only relevant categories
A <- A[year==2018]
A <- A[sex!='a']
A <- A[!age_group %in% c('0-4','0-14','5-14','all','15plus')]
A <- A[risk_factor=='all']
A[,age:=age_group]
## harmonize namings
A[age=='65plus',age:='65+']
A[sex=='f',sex:='F']
A[sex=='m',sex:='M']
unique(A[,.(sex,age)])                  #check

## HIV
H <- fread(here('indata/TB_burden_countries_2020-02-24.csv'))
H <- H[year==2018,.(iso3,e_tbhiv_prct,e_tbhiv_prct_lo,e_tbhiv_prct_hi)]
H[is.na(e_tbhiv_prct),c('e_tbhiv_prct','e_tbhiv_prct_lo','e_tbhiv_prct_hi'):=0] #check OK to drop
H[,hiv:=e_tbhiv_prct/100]
H[,hiv.sd:=(e_tbhiv_prct_hi-e_tbhiv_prct_lo)/392]


## === merge data
AN <- merge(NP[,.(iso3,sex,age,notes=value)],
            A[,.(iso3,sex,age,inc=best,lo,hi)],
            by=c('iso3','sex','age'),all.x=TRUE,all.y=FALSE)
AN[notes>inc]                           #a few to watch out for
AN[,untreated:=pmax(inc-notes,0)]
AN <- merge(AN,H[,.(iso3,hiv,hiv.sd)],by=c('iso3'),all.x=TRUE,all.y=FALSE)

## merge in BBM
AN <- merge(AN,BBM,by=c('sex','age'),all.x=TRUE)


## calculations of TBM
## cases
AN[,c('TBM.treated','TBM.untreated'):=.(notes*propm,untreated*propm)]
## AN[,c('TBM.treated.sd','TBM.untreated.sd'):=.(TODO,TODO)]
## TODO using Z.sd = Z * sqrt((X.sd/X)^2+(Y.sd/Y)^2) for Z = X * Y: see xfun near top
## TODO split by HIV

## sanity checks
AN[,sum(TBM.treated)]
AN[,sum(notes)]*5/1e3
AN[,sum(untreated)]*5/1e3

## deaths
AN[,c('TBMdeaths.treated','TBMdeaths.untreated'):=.(TBM.treated*cfr,TBM.untreated)]
## TODO:
## HIV split
## uncertainty for untreated CFR (currently 100%)
## uncertainty for cfr

## exploring results
AN <- merge(AN,unique(N[,.(iso3,g_whoregion)]),by='iso3',all.x = TRUE,all.y=FALSE)

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


## looking at big numbers
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers

GRO <- cbind(Region=GR$g_whoregion,GR[,lapply(.SD,see),.SDcols=2:5])

## save out
fwrite(GRO,file=here('outdata/GRO.csv'))
fwrite(AN,file=here('outdata/AN.csv'))

save(G,file=here('outdata/G.Rdata'))
save(GA,file=here('outdata/GA.Rdata'))
save(R,file=here('outdata/R.Rdata'))
save(RA,file=here('outdata/RA.Rdata'))

## check aggrergate implied CFR
(1e2*G[,TBMdeaths.treated/TBM.treated]) #18.5%, cf 0.16 (0.10, 0.24) in Anna

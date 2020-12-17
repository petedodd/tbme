## sketching global estimates
## NOTE requires data in indata folder
## NOTE please make a folder outdata for temporary data (excluded from repo)
library(here)
source(here('utilities.R'))

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
A[,best.sd:=(hi-lo)/3.92]

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
AN[,untreated.sd:=(hi-lo)/3.92]         #as per incidence
AN <- merge(AN,H[,.(iso3,hiv,hiv.sd)],by=c('iso3'),all.x=TRUE,all.y=FALSE)

## merge in BBM
AN <- merge(AN,BBM,by=c('sex','age'),all.x=TRUE)


## calculations of TBM
## cases
AN[,c('TBM.treated','TBM.untreated'):=.(notes*propm,untreated*propm)]
AN[,c('TBM.treated.sd','TBM.untreated.sd'):=.(xfun(notes,propm,0,propm.sd),
                                              xfun(untreated,propm,untreated.sd,propm.sd))]
## TODO split by HIV

## sanity checks
AN[,sum(TBM.treated)]
AN[,sum(notes)]*5/1e3
AN[,sum(untreated)]*5/1e3

## deaths
AN[,c('TBMdeaths.treated','TBMdeaths.untreated'):=.(TBM.treated*cfr,TBM.untreated)]
AN[,c('TBMdeaths.treated.sd','TBMdeaths.untreated.sd'):=.(xfun(TBM.treated,cfr,
                                                               TBM.treated.sd,cfr.sd),
                                                          xfun(TBM.untreated,1,
                                                               TBM.untreated.sd,0))]
## TODO:
## HIV split


## exploring results
AN <- merge(AN,unique(N[,.(iso3,g_whoregion)]),by='iso3',all.x = TRUE,all.y=FALSE)


save(AN,file=here('outdata/AN.Rdata'))

## TODO list
## 1. Pete - include HIV split
## 2. Anna - check unc calx

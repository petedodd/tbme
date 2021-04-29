## sketching global estimates
## NOTE requires data in indata folder
## NOTE please make a folder outdata for temporary data (excluded from repo)
library(here)
source(here('utilities.R'))

## === using the propm & CFR from meta-analysis
## read meta-analysis results
maest <- fread('metaanalysis/meta.combined.estimates.csv')

## reshape HIV-
BBM <- maest[hiv=='hiv-']
BBM <- dcast(BBM[,.(qty,sex,age,pred,se)],age+sex~qty,
             value.var = c('pred','se'))
BBM <- BBM[,.(age,sex,
              propm=pred_prop,propm.sd=se_prop,
              cfr=pred_cfr,cfr.sd=se_cfr)]
BBM[,sex:=ifelse(sex=='female','F','M')]

## reshape HIV+
BBMp <- maest[hiv=='hiv+']
BBMp <- dcast(BBMp[,.(qty,sex,age,pred,se)],age+sex~qty,
             value.var = c('pred','se'))
BBMp <- BBMp[,.(age,sex,
                propmp=pred_prop,propmp.sd=se_prop,
                cfrp=pred_cfr,cfrp.sd=se_cfr)]
BBMp[,sex:=ifelse(sex=='female','F','M')]

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
cat(dropped,file=here('outdata/drop_lonote.txt'))
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
AN <- merge(AN,H[,.(iso3,hiv,hiv.sd)],by=c('iso3'),
            all.x=TRUE,all.y=FALSE)

## merge in BBM
AN <- merge(AN,BBM,by=c('sex','age'),all.x=TRUE)  #HIV -ve
AN <- merge(AN,BBMp,by=c('sex','age'),all.x=TRUE) #HIV +ve

## === calculations of TBM
## SA = sensitivity analysis
## --- cases
AN[,c('TBM.hn.treated','TBM.hn.untreated'):=.(notes*propm*(1-hiv),untreated*propm*(1-hiv))] #HIV-ve
AN[,c('TBM.hp.treated','TBM.hp.untreated'):=.(notes*propmp*hiv,untreated*propmp*hiv)] #HIV+ve
## sensitivity analyses
AN[,c('TBM.hn.untreated2','TBM.hp.untreated2'):=.(TBM.hn.untreated/2,TBM.hp.untreated/2)] #SA 1
AN[,c('TBM.hn.untreated3','TBM.hp.untreated3'):=.(TBM.hn.untreated*2,TBM.hp.untreated*2)] #SA 2

## case uncertainty
AN[,c('untreated.hn.sd','untreated.hp.sd'):=.(xfun(untreated,(1-hiv),untreated.sd,hiv.sd),
                                              xfun(untreated,(hiv),untreated.sd,hiv.sd))]
AN[,c('TBM.hn.treated.sd','TBM.hn.untreated.sd'):=.(xfun(notes*(1-hiv),propm,notes*hiv.sd,propm.sd),
                                                    xfun(untreated*(1-hiv),propm,untreated.hn.sd,propm.sd))] #HIV-
AN[,c('TBM.hp.treated.sd','TBM.hp.untreated.sd'):=.(xfun(notes,propmp,0,propmp.sd),
                                                    xfun(untreated*hiv,propmp,untreated.hp.sd,propmp.sd))] #HIV+
## sensitivity analyses
AN[,c('TBM.hn.untreated2.sd','TBM.hp.untreated2.sd'):=.(xfun(untreated*(1-hiv)/2,propm,untreated.hn.sd/2,propm.sd),
                                                        xfun(untreated*hiv/2,propmp,untreated.hp.sd/2,propmp.sd))] #SA 1
AN[,c('TBM.hn.untreated3.sd','TBM.hp.untreated3.sd'):=.(xfun(untreated*(1-hiv)*2,propm,untreated.hn.sd*2,propm.sd),
                                                        xfun(untreated*hiv*2,propmp,untreated.hp.sd*2,propmp.sd))] #SA 2

## sanity checks
AN[,sum(TBM.hn.treated)]
AN[,sum(TBM.hp.treated)]
AN[,sum(notes)]*5/1e3
AN[,sum(untreated)]*5/1e3

## --- deaths
## HIV-ve
AN[,c('TBMdeaths.hn.treated',
      'TBMdeaths.hn.untreated'):=.(TBM.hn.treated*cfr,TBM.hn.untreated)]
AN[,c('TBMdeaths.hn.treated.sd',
      'TBMdeaths.hn.untreated.sd'):=.(xfun(TBM.hn.treated,cfr,
                                           TBM.hn.treated.sd,cfr.sd),
                                      xfun(TBM.hn.untreated,1,
                                           TBM.hn.untreated.sd,0))]
## HIV+ve
AN[,c('TBMdeaths.hp.treated',
      'TBMdeaths.hp.untreated'):=.(TBM.hp.treated*cfrp,TBM.hp.untreated)]
AN[,c('TBMdeaths.hp.treated.sd',
      'TBMdeaths.hp.untreated.sd'):=.(xfun(TBM.hp.treated,cfrp,
                                           TBM.hp.treated.sd,cfrp.sd),
                                      xfun(TBM.hp.untreated,1,
                                           TBM.hp.untreated.sd,0))]

## sensitivity analyses
AN[,c('TBMdeaths.hn.untreated2',
      'TBMdeaths.hp.untreated2'):=.(TBM.hn.untreated2,TBM.hp.untreated2)]
AN[,c('TBMdeaths.hn.untreated3',
      'TBMdeaths.hp.untreated3'):=.(TBM.hn.untreated3,TBM.hp.untreated3)]
AN[,c('TBMdeaths.hn.untreated2.sd',
      'TBMdeaths.hp.untreated2.sd'):=.(xfun(TBM.hn.untreated2,1,
                                            TBM.hn.untreated2.sd,0),
                                       xfun(TBM.hp.untreated2,1,
                                            TBM.hp.untreated2.sd,0))]
AN[,c('TBMdeaths.hn.untreated3.sd',
      'TBMdeaths.hp.untreated3.sd'):=.(xfun(TBM.hn.untreated3,1,
                                            TBM.hn.untreated3.sd,0),
                                       xfun(TBM.hp.untreated3,1,
                                            TBM.hp.untreated3.sd,0))]


## === saving out results
AN <- merge(AN,unique(N[,.(iso3,g_whoregion)]),
            by='iso3',all.x = TRUE,all.y=FALSE)
save(AN,file=here('outdata/AN.Rdata'))

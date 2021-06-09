## libraries
rm(list = ls())
library(here)
library(metafor)
library(scales)
library(rms)
library(data.table)
library(ggplot2)
library(ggpubr)

## colorscheme & plot themes
clz <-  c("#000000", "#E69F00",
          "#56B4E9", "#009E73",
          "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7")

cnz <- c("BRA"=clz[1], "GBR"=clz[3], "USA"="orchid2",
         "VNM"=clz[5], "ZAF"=clz[4])

myth <- theme_bw() + 
    theme(axis.text.x = element_text(angle=45,hjust=1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"))+
    ggpubr::grids()

## binomial CIs
binom.test(2,2)$conf.int
ciz <- function(x,y){
    if(y==0) return(list(0,1))
    as.list(binom.test(x,y)$conf.int)
}
ssum <- function(x,y)sqrt(x^2+y^2)
ilogit <- function(x)1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

## --------- iso3=GBR ---------
UK <- fread(here('metain/GBR/UKTBM.csv'))
UK

UK$sex <- factor(UK$sex)
resuk <- rma(measure="PLO", xi=TBM, ni=TB, data=UK,mods=~age+sex)
print(resuk, digits=3)


df <- unique(data.table(UK[,.(sex,age)]))
df0a <- predict(resuk,  transf=transf.ilogit)
df0a <- as.data.table(df0a)
df0a <- cbind(df0a,UK)

ggplot(df0a,aes(x=age,y=TBM/TB,col=sex,size=TB)) +
  geom_point(shape=1) +
  facet_wrap(~sex) +
  geom_line(data=df0a,aes(x=age,y=pred))


## checking predictions and pred errors
ukpp <- unique(df0a[,.(sex,age,pred,ci.lb,ci.ub)])#
df0as <- unique(df0a[,.(sex,age,pred,ci.lb,ci.ub)]) #for just below
test <- coef(summary(resuk))


## --------- iso3=USA ---------
US <- fread(here('USA/USTBM.csv'))
US
US$age <- gsub('\\s','',as.character(US$age))
US$age <- factor(US$age)


## US
resus <- rma(measure="PLO", xi=TBM, ni=TB, data=US,mods=~age+sex)
print(resus, digits=3)
## forest(res)

## US deaths
resusd <- rma(measure="PLO", xi=TBMD, ni=TBM, data=US,mods=~age+sex)
print(resusd, digits=3)

df0 <- predict(resus, transf=transf.ilogit)
df0 <- as.data.table(df0)
df0 <- cbind(df0,US)

df0s <- unique(df0[,.(sex,age,pred,ci.lb,ci.ub)])

df0[,country:='US']; df0a[,country:='UK']
df0s[,country:='US']; df0as[,country:='UK']
both <- rbind(df0s,df0as)

## predictions
df <- unique(US[,.(age,sex)])
uspd <- predict(resusd, transf=transf.ilogit,newdata = df)
uspd <- as.data.table(uspd)
uspd <- cbind(uspd,df)

uspp <- predict(resus, transf=transf.ilogit,newdata = df)
uspp <- as.data.table(uspp)
uspp <- cbind(uspp,df)
uspp <- unique(uspp)

## ---- US/UK look
ggplot(both,aes(x=age,y=pred,ymin=ci.lb,ymax=ci.ub,
                col=country,group=country)) +
    geom_point()+
    geom_line() +
    scale_y_continuous(label=percent)+
    expand_limits(y=0) +
    xlab('Age group') + ylab('Fraction of TB that is TBM')+
    geom_point(data=df0,aes(y=TBM/TB,size=TB),shape=1,alpha=0.4)+
    geom_point(data=df0a,aes(y=TBM/TB,size=TB),shape=1,alpha=0.4)+
    geom_errorbar(aes(ymin=ci.lb,ymax=ci.ub),width=0.2) +
    facet_wrap(~sex) + myth

ggsave(here('metaplots/USUK.pdf'),w=10,h=5)

## --------- iso3=VNM ---------
## cases
DMN <- fread(here("metain/VNM/TBMhn.csv"))
DMP <- fread(here("metain/VNM/TBMhp.csv"))
DAN <- fread(here("metain/VNM/TBAhn.csv"))
DAP <- fread(here("metain/VNM/TBAhp.csv"))

## all TB
DAP <- melt(DAP,id=c('year','age'))
DAN <- melt(DAN,id=c('year','age'))
names(DAP)[3] <- names(DAN)[3] <- 'sex'
names(DAP)[4] <- names(DAN)[4] <- 'TB'
DAP[,hiv:='hiv+ve']; DAN[,hiv:='hiv-ve'];
DA <- rbind(DAN,DAP)

## TB men
DMP <- melt(DMP,id=c('year','age'))
DMN <- melt(DMN,id=c('year','age'))
names(DMP)[3] <- names(DMN)[3] <- 'sex'
names(DMP)[4] <- names(DMN)[4] <- 'TBM'
DMP[,hiv:='hiv+ve']; DMN[,hiv:='hiv-ve'];
DM <- rbind(DMN,DMP)

## all
D <- merge(DA,DM,by=c('year','age','sex','hiv'))
D #check

D$year <- factor(D$year,ordered=TRUE)
D$age <- factor(D$age,ordered=TRUE,levels=unique(D$age))

## check
DS <- D[,.(mn=sum(TBM)/sum(TB)),by=.(age,sex,hiv)] #summary
DS

D[age=='>=65',age:='65+']
D[,sex:=tolower(sex)]
D$year <- as.integer(as.character(D$year))
D$age <- factor(D$age,ordered = FALSE)

VNMn <- D[hiv=='hiv-ve']
VNMp <- D[hiv=='hiv+ve']

## HIV -ve
resvn <- rma(measure="PLO", xi=TBM, ni=TB, data=VNMn,mods=~age+sex)
print(resvn, digits=3)

df <- unique(data.table(VNMn[,.(sex,age)]))
dfv <- predict(resvn,  transf=transf.ilogit)
dfv <- as.data.table(dfv)
dfv <- cbind(dfv,VNMn)

ggplot(dfv,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBM/TB,col=sex,size=TB),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/VNM.pdf'),w=10,h=5)

vnpn <- unique(dfv[,.(pred,ci.lb,ci.ub,sex,age)])

## HIV +ve
resvp <- rma(measure="PLO", xi=TBM, ni=TB, data=VNMp,mods=~age+sex)
print(resvp, digits=3)

df <- unique(data.table(VNMp[,.(sex,age)]))
dfv2 <- predict(resvp,  transf=transf.ilogit)
dfv2 <- as.data.table(dfv2)
dfv2 <- cbind(dfv2,VNMp)

ggplot(dfv2,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBM/TB,col=sex,size=TB),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/VNM.H.pdf'),w=10,h=5)

vnpp <- unique(dfv2[,.(pred,ci.lb,ci.ub,sex,age)])

VNMc <- copy(D)

## deaths
VDN <- fread(here('metain/VNM/deathHN.csv'),skip=2)
VDP <- fread(here('metain/VNM/deathHP.csv'),skip=2)

names(VDN) <- names(VDP) <- c('age','TBM.male','TBM.female',
                              'TBMdeaths.male','TBMdeaths.female')
## HIV-
VDNm <- melt(VDN,id='age')
VDNm[,c('qty','sex'):=tstrsplit(variable,split="\\.")]
VDNm <- dcast(VDNm,age+sex~qty,value.var = 'value')
VDNm[age=='>=65',age:='65+']
VDNm$age <- factor(VDNm$age,ordered = FALSE)
VDNm <- VDNm[age!='Total']

## HIV+
VDPm <- melt(VDP,id='age')
VDPm[,c('qty','sex'):=tstrsplit(variable,split="\\.")]
VDPm <- dcast(VDPm,age+sex~qty,value.var = 'value')
VDPm[age=='>=65',age:='65+']
VDPm$age <- factor(VDPm$age,ordered = FALSE)
VDPm <- VDPm[age!='Total']

for( i in 1:nrow(VDPm)) VDPm[i,c('lo','hi'):=ciz(TBMdeaths,TBM)]
for( i in 1:nrow(VDNm)) VDNm[i,c('lo','hi'):=ciz(TBMdeaths,TBM)]
VDPm[,cfr:=TBMdeaths/TBM]
VDNm[,cfr:=TBMdeaths/TBM]


## --------- iso3=ZAF ---------
ZC <- fread(here('metain/ZAF/ZC.csv'))
ZD <- fread(here('metain/ZAF/ZD.csv'))

ZC[,sex:=tolower(sex)]; ZD[,sex:=tolower(sex)]
ZC[,TB:=TBcase]
ZC[,hiv:=tolower(hiv)]; ZD[,hiv:=tolower(hiv)];
ZC[,age:=gsub(' years','',agecat8)]
ZD[,age:=gsub(' years','',agecat8)]
ZC$age <- factor(ZC$age,ordered = FALSE)
ZD$age <- factor(ZD$age,ordered = FALSE)


ZD <- merge(ZD,ZC[,.(year,age,sex,hiv,TBM)],
            by=c('year','age','sex','hiv')) #merge to get TBM denom

ZCN <- ZC[hiv=='hiv-']; ZCP <- ZC[hiv=='hiv+']
ZDN <- ZD[hiv=='hiv-']; ZDP <- ZD[hiv=='hiv+']

## --- cases
## HIV -ve
reszn <- rma(measure="PLO", xi=TBM, ni=TB, data=ZCN,mods=~age+sex)
print(reszn, digits=3)

df <- unique(data.table(ZCN[,.(sex,age)]))
dfz <- predict(reszn,  transf=transf.ilogit)
dfz <- as.data.table(dfz)
dfz <- cbind(dfz,ZCN)

ggplot(dfz,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBM/TB,col=sex,size=TB),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/ZAF.pdf'),w=10,h=5)


df <- unique(data.table(ZCN[,.(sex,age)]))
dfz <- predict(reszn,  transf=transf.ilogit)
dfz <- as.data.table(dfz)
zppn <- cbind(dfz,df)

## HIV +ve
reszp <- rma(measure="PLO", xi=TBM, ni=TB, data=ZCP,mods=~age+sex)
print(reszp, digits=3)

df <- unique(data.table(ZCP[,.(sex,age)]))
dfz2 <- predict(reszp,  transf=transf.ilogit)
dfz2 <- as.data.table(dfz2)
dfz2 <- cbind(dfz2,ZCP)

ggplot(dfz2,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBM/TB,col=sex,size=TB),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/ZAF.H.pdf'),w=10,h=5)

df <- unique(data.table(ZCP[,.(sex,age)]))
dfz2 <- predict(reszp,  transf=transf.ilogit)
dfz2 <- as.data.table(dfz2)
zppp <- cbind(dfz2,df)



## --- deaths
## HIV -ve
reszdn <- rma(measure="PLO", xi=TBMdeaths, ni=TBM, data=ZDN,
              mods=~age+sex)
print(reszdn, digits=3)

df <- unique(data.table(ZCN[,.(sex,age)]))
dfzd <- predict(reszdn,  transf=transf.ilogit)
dfzd <- as.data.table(dfzd)
dfzd <- cbind(dfzd,ZDN)

ggplot(dfzd,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBMdeaths/TBM,col=sex,size=TBM),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/ZAF.d.pdf'),w=10,h=5)


df <- unique(data.table(ZCN[,.(sex,age)]))
dfzd <- predict(reszdn,  transf=transf.ilogit)
dfzd <- as.data.table(dfzd)
zpdn <- cbind(dfzd,df)

## HIV +ve
reszdp <- rma(measure="PLO", xi=TBMdeaths, ni=TBM, data=ZDP,
              mods=~age+sex)
print(reszdp, digits=3)

df <- unique(data.table(ZCN[,.(sex,age)]))
dfzd2 <- predict(reszdp,  transf=transf.ilogit)
dfzd2 <- as.data.table(dfzd2)
dfzd2 <- cbind(dfzd2,ZDP)

ggplot(dfzd2,aes(x=age,y=pred,col=sex,group=sex)) +
    facet_wrap(~sex) +
    scale_y_continuous(label=percent)+
    geom_point(aes(x=age,y=TBMdeaths/TBM,col=sex,size=TBM),shape=1) +
    geom_line() + myth

ggsave(here('metaplots/ZAF.d.H.pdf'),w=10,h=5)


df <- unique(data.table(ZCN[,.(sex,age)]))
dfzd2 <- predict(reszdp,  transf=transf.ilogit)
dfzd2 <- as.data.table(dfzd2)
zpdp <- cbind(dfzd2,df)
zpdp <- unique(zpdp)



## --------- iso3=BRA ---------
## HIV-ves
## read
b1n <- fread(here('metain/BRA/HnTB.csv'))
b2n <- fread(here('metain/BRA/HnTBM.csv'))

## reshape
b1n <- b1n[,.(year,
              `15-19`,`20-39`,`40-59`,`60-64`,`65-69`,`70-79`,`80+`)]
b1n <- melt(b1n,id='year')
b2n <- b2n[,.(year,
              `15-19`,`20-39`,`40-59`,`60-64`,`65-69`,`70-79`,`80+`)]
b2n <- melt(b2n,id='year')

## join
names(b2n)[2] <- names(b1n)[2] <- 'age'
names(b2n)[3] <- 'TBM'; names(b1n)[3] <- 'TBN'
bb <- merge(b1n,b2n,by=c('year','age'))

## add age midpoints
midder <- function(x) mean(as.numeric(unlist(strsplit(as.character(x),
                                                      split='-'))))
midders <- function(x) unlist(lapply(x,midder))
bb[age!='80+',agemid:=midders(age)]
bb[age=='80+',agemid:=82.5]
unique(bb[,.(age,agemid)])              #check

bb
bbs <- bb[,.(TBN=sum(TBN),TBM=sum(TBM)),by=.(age,agemid)]

## knots <- c(20,30,40,50,60,70)
## resc <- rma(measure="PLO", xi=TBM, ni=TBN, data=bb, mods=~rcs(agemid,knots))


resc <- rma(measure="PLO", xi=TBM, ni=TBN, data=bb, mods=~rcs(agemid,4))
summary(resc)
coef(resc)
knots <- attr(rcs(model.matrix(resc)[,2], 4), "parms")

xs <- 15:83

pdz <- predict(resc,newmods=rcspline.eval(xs,knots,inclx=TRUE),
               transf=transf.ilogit)


pdf(here('metaplots/brazil.preds.pdf'))

plot(xs,
     pdz$pred,
     col="red",lwd=2,type='l',
     ylim=c(0,1e-2),xlab='Age',ylab='Proportion of TB that is TBM')
points(bbs$agemid,bbs$TBM/bbs$TBN)
lines(xs,pdz$ci.lb,col=2,lty=2)
lines(xs,pdz$ci.ub,col=2,lty=2)

##
agz <- c('15-24','25-34','35-44','45-54','55-64','65+')
mdz <- midders(agz)
mdz[is.na(mdz)] <- 70

pdza <- predict(resc,newmods=rcspline.eval(mdz,knots,inclx=TRUE),
                transf=transf.ilogit)

points(mdz,pdza$pred,col='blue')
for(i in 1:length(mdz))
  lines(c(mdz[i],mdz[i]),c(pdza$cr.lb[i],pdza$cr.ub[i]),col='blue')

dev.off()                               #NOTE


pdza <- as.data.table(pdza)
pdza[,iso3:='BRA']
pdza[,age:=agz]
pdza[,sex:=NA]

pdza    #BRA MA summary HIV

## HIV+ves
b1p <- fread(here('metain/BRA/HpTB.csv'))
b2p <- fread(here('metain/BRA/HpTBM.csv'))

## reshape
b1p <- b1p[,.(year,
              `15-19`,`20-39`,`40-59`,`60-64`,`65-69`,`70-79`,`80+`)]
b1p <- melt(b1p,id='year')
b2p <- b2p[,.(year,
              `15-19`,`20-39`,`40-59`,`60-64`,`65-69`,`70-79`,`80+`)]
b2p <- melt(b2p,id='year')

## join
names(b2p)[2] <- names(b1p)[2] <- 'age'
names(b2p)[3] <- 'TBM'; names(b1p)[3] <- 'TBN'
bp <- merge(b1p,b2p,by=c('year','age'))

## add age midpoints
bp[age!='80+',agemid:=midders(age)]
bp[age=='80+',agemid:=82.5]
unique(bp[,.(age,agemid)])              #check

bp
bps <- bp[,.(TBN=sum(TBN),TBM=sum(TBM)),by=.(age,agemid)]


resc <- rma(measure="PLO", xi=TBM, ni=TBN, data=bp, mods=~rcs(agemid,4))
summary(resc)
coef(resc)
knots <- attr(rcs(model.matrix(resc)[,2], 4), "parms")

xs <- 15:83

pdz <- predict(resc,newmods=rcspline.eval(xs,knots,inclx=TRUE),
               transf=transf.ilogit)

pdf(here('metaplots/brazil.H.preds.pdf'))

plot(xs,
     pdz$pred,
     col="red",lwd=2,type='l',
     ylim=c(0,.1),xlab='Age',ylab='Proportion of TB that is TBM')
points(bps$agemid,bps$TBM/bps$TBN)
lines(xs,pdz$ci.lb,col=2,lty=2)
lines(xs,pdz$ci.ub,col=2,lty=2)


pdz0 <- as.data.table(pdz)
pdz0[,age:=xs]

##
agz <- c('15-24','25-34','35-44','45-54','55-64','65+')
mdz <- midders(agz)
mdz[is.na(mdz)] <- 70

pdzap <- predict(resc,newmods=rcspline.eval(mdz,knots,inclx=TRUE),
                transf=transf.ilogit)

points(mdz,pdzap$pred,col='blue')
for(i in 1:length(mdz))
  lines(c(mdz[i],mdz[i]),c(pdzap$cr.lb[i],pdzap$cr.ub[i]),col='blue')

dev.off()                               #NOTE


pdzap <- as.data.table(pdzap)
pdzap[,iso3:='BRA']
pdzap[,age:=agz]
pdzap[,sex:=NA]

pdzap                                   #BRA MA summary HIV+

## assume BRA data for female and generate comparable MA results
pdzap[,estimate:=transf.logit(pred)]
pdza[,estimate:=transf.logit(pred)]

pdzap[,se:=(cr.ub-cr.lb)/3.92] #use credible intervals
pdza[,se:=(cr.ub-cr.lb)/3.92]

## --------- all iso3 summary & plot ---------
## --- US props
us <- coef(summary(resus))
nmz <- row.names(us)
us <- as.data.table(us)
us[,c('cft','qty','iso3'):=.(nmz,'prop','USA')]
uspp                                    #props predicted

## --- US deaths
usd <- coef(summary(resusd))
usd <- as.data.table(usd)
usd[,c('cft','qty','iso3'):=.(nmz,'cfr','USA')]
uspd                                    #deaths predicted

## data
US[,c('iso3','hiv'):=.('USA','hiv-')]


## --- UK props
## MA output
uk <- coef(summary(resuk))
uk <- as.data.table(uk)
uk[,c('cft','qty','iso3'):=.(nmz,'prop','GBR')]

## data
UK[,c('iso3','hiv','TBMdeaths'):=.('GBR','hiv-',NA)]


## --- VNM props
## HIV-
vn <- coef(summary(resvn))
vn <- as.data.table(vn)
vn[,c('cft','qty','iso3'):=.(nmz,'prop','VNM')]

## HIV+
vnp <- coef(summary(resvp))
vnp <- as.data.table(vnp)
vnp[,c('cft','qty','iso3'):=.(nmz,'prop','VNM')]

## data
VNMc[,c('iso3','TBMdeaths'):=.('VNM',NA)]
VNMc[,hiv:=gsub('ve','',hiv)]

## --- VNM deaths
## both data and estimates - no years
VDPm[,c('iso3','hiv','year','TB'):=.('VNM','hiv+',NA,NA)]
VDNm[,c('iso3','hiv','year','TB'):=.('VNM','hiv-',NA,NA)]


## join data
VNALL <- rbindlist(list(VNMc[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)],
                        VDNm[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)],
                        VDPm[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)]
                        ))


## --- ZAF props
## MA output
## HIV-ve
zan <- coef(summary(reszn))
zan <- as.data.table(zan)
zan[,c('cft','qty','iso3'):=.(nmz,'prop','ZAF')]
zppn <- unique(zppn) #preds

## HIV+ve
zap <- coef(summary(reszp))
zap <- as.data.table(zap)
zap[,c('cft','qty','iso3'):=.(nmz,'prop','ZAF')]
zppp <- unique(zppp) #preds

## data
ZCN[,iso3:='ZAF']
ZCP[,iso3:='ZAF']

## --- ZAF deaths
## MA output
## HIV-ve
zadn <- coef(summary(reszdn))
zadn <- as.data.table(zadn)
zadn[,c('cft','qty','iso3'):=.(nmz,'prop','ZAF')]
zpdn                                    #preds

## HIV+ve
zadp <- coef(summary(reszdp))
zadp <- as.data.table(zadp)
zadp[,c('cft','qty','iso3'):=.(nmz,'prop','ZAF')]
zpdp                                    #preds

## data
ZDN[,iso3:='ZAF']
ZDP[,iso3:='ZAF']


ZDN <- merge(ZDN,ZCN[,.(year,age,sex,hiv,iso3,TB)],
             by=c('year','age','sex','hiv','iso3'))
ZDP <- merge(ZDP,ZCP[,.(year,age,sex,hiv,iso3,TB)],
             by=c('year','age','sex','hiv','iso3'))

## join data
ZAALL <- rbindlist(list(ZDN[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)],
                        ZDP[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)]
                        ))

## --- BRA props
## MA output
pdza                                    #HIV-ve
pdzap                                    #HIV+ve

## data
bb[,c('iso3','hiv','sex','TBMdeaths'):=.('BRA','hiv-',NA,NA)]     #HIV-ve
bp[,c('iso3','hiv','sex','TBMdeaths'):=.('BRA','hiv+',NA,NA)]     #HIV+ve

bboth <- rbind(bb,bp)
bboth[,TB:=TBN]

## --- join input data in clean format ---
ALL <- rbindlist(list(
  bboth[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)],
  UK[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths)],
  US[,.(iso3,year,hiv,sex,age,TB,TBM,TBMdeaths=TBMD)],
  VNALL,
  ZAALL
))
ALL[,unique(age)]
ALL[,unique(hiv)]

## NOTE US data needs cells under 5 redacted
ALL.US.no.lt.5 <- copy(ALL)
ALL.US.no.lt.5[,togo:=FALSE]; ALL.US.no.lt.5[,togo0:=FALSE]
ALL.US.no.lt.5[iso3=='USA' & TBM<5,togo0:=TRUE]
ALL.US.no.lt.5[iso3=='USA' & TBMdeaths<5,togo:=TRUE]
ALL.US.no.lt.5[,c('TBM','TBMdeaths'):=.(as.character(TBM),
                                        as.character(TBMdeaths))]
ALL.US.no.lt.5[togo0==TRUE,TBM:='<5']
ALL.US.no.lt.5[togo==TRUE,TBMdeaths:='<5']
ALL.US.no.lt.5[,c('togo','togo0'):=NULL]
fwrite(ALL.US.no.lt.5,file=here('meta.combined.count.data.clean.csv'))

## compute counts etc here
rnge <- ALL[,range(year,na.rm=TRUE)]
cat(rnge,file=here('metaout/cases.years.txt'))

cases <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                TBM=sum(TBM,na.rm = TRUE),
                TBMdeaths=sum(TBMdeaths,na.rm = TRUE))]
fwrite(cases,file=here('metaout/cases.csv'))

cases.hiv <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                TBM=sum(TBM,na.rm = TRUE),
                TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),by=hiv]
fwrite(cases.hiv,file=here('metaout/cases.hiv.csv'))

casesprop <- ALL[!is.na(year),.(TB=sum(TB,na.rm = TRUE),
                                TBM=sum(TBM,na.rm = TRUE))]
fwrite(casesprop,file=here('metaout/casesprop.csv'))
casesprop.hiv <- ALL[!is.na(year),.(TB=sum(TB,na.rm = TRUE),
                                TBM=sum(TBM,na.rm = TRUE)),by=hiv]
fwrite(casesprop.hiv,file=here('metaout/casesprop.hiv.csv'))

casesCFR <- ALL[!is.na(TBMdeaths),.(TBM=sum(TBM,na.rm = TRUE),
                                     TBMdeaths=sum(TBMdeaths,na.rm = TRUE))]
fwrite(casesCFR,file=here('metaout/casesCFR.csv'))
casesCFR.hiv <- ALL[!is.na(TBMdeaths),.(TBM=sum(TBM,na.rm = TRUE),
                                        TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),by=hiv]
fwrite(casesCFR.hiv,file=here('metaout/casesCFR.hiv.csv'))

cases.iso3 <- ALL[,.(TBM=sum(TBM,na.rm=TRUE)),by=iso3]
fwrite(cases.iso3,file=here('metaout/cases.iso3.csv'))

cases.sex <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                    TBM=sum(TBM,na.rm = TRUE),
                    TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),by=sex]
cases.sex[is.na(sex),sex:='missing']
fwrite(cases.sex,file=here('metaout/cases.sex.csv'))


## === "participant" table ====
ALL

names(cases.sex)[1] <- names(cases.hiv)[1] <- 'qty'
ALL[,qty:=iso3]
ALL[iso3=='VNM' & is.na(TBMdeaths),qty:=paste0(iso3,' (cases)')]
ALL[iso3=='VNM' & is.na(TB),qty:=paste0(iso3,' (deaths)')]
tmp.iso3 <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                   TBM=sum(TBM,na.rm = TRUE),
                   TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),by=qty]
## ages
tmp.age <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                  TBM=sum(TBM,na.rm = TRUE),
                  TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),
               by=.(qty=age)]
tmp.age1 <- tmp.age[1:7]
tmp.age2 <- tmp.age[8:13]
tmp.age1[,QTY:='Age (BRA)']
tmp.age2[,QTY:='Age (not BRA)']

## year
tmp.year <- ALL[,.(TB=sum(TB,na.rm = TRUE),
                  TBM=sum(TBM,na.rm = TRUE),
                  TBMdeaths=sum(TBMdeaths,na.rm = TRUE)),
                by=.(qty=year)]
tmp.year[,qty:=as.character(qty)]
tmp.year[is.na(qty),qty:='missing']
tmp.year <- tmp.year[order(qty)]

cases.sex[,QTY:='Sex']
cases.hiv[,QTY:='HIV']
tmp.iso3[,QTY:='Country']
tmp.year[,QTY:='Year']

tmp1 <- data.table(QTY='Total',qty='Total',cases)
nnn <- c('QTY','qty','TB','TBM','TBMdeaths')


participant <- rbindlist(list(
    tmp1[,..nnn],cases.sex[,..nnn],
    cases.hiv[,..nnn],tmp.iso3[,..nnn],
    tmp.age1[,..nnn],tmp.age2[,..nnn],
    tmp.year[,..nnn]
))
participant

participant[,c('TB.t','TBM.t','TBMdeaths.t'):=.(
                 sum(TB),
                 sum(TBM),
                 sum(TBMdeaths)),by=QTY]
participant[,c('TB.p','TBM.p','TBMdeaths.p'):=.(
                 round(1e2*TB/TB.t),
                 round(1e2*TBM/TBM.t),
                 round(1e2*TBMdeaths/TBMdeaths.t))]
participant[,c('TB.x','TBM.x','TBMdeaths.x'):=.(
                 paste0(TB,' (',TB.p,'%)'),
                 paste0(TBM,' (',TBM.p,'%)'),
                 paste0(TBMdeaths,' (',TBMdeaths.p,'%)')
             )]
participant[QTY=='Age (BRA)',TBMdeaths.x:=NA]
participant <- participant[,.(QTY,qty,
                              TB=TB.x,TBM=TBM.x,
                              `TBM deaths`=TBMdeaths.x)]


fwrite(participant,file=here('participant.csv'))

## ============= FIGURES & OVERALL POOLING

## --- Figure Ma: TBM prop HIV-ve
## "BRA" "GBR" "USA" "VNM" "ZAF"
tmpa <- ALL[!is.na(year) & hiv=='hiv-']
tmpa[,unique(iso3)]
tmpa[iso3=='BRA',sex:='both']
tmpa[,value:=TBM/TB]
tmpa2 <- tmpa[iso3=='BRA']
tmpa1 <- tmpa[iso3!='BRA']
tmpa2[,age2:=midders(tmpa2$age)]
tmpa2[is.na(age),age2:=82.5]
## 1  = 20 and 1 per 10
tmpa2[,age:=1+(age2-20)/10]

## data for MA [,.(iso3,sex,age,pred,se,hi,lo)]
mad.a <- rbind(
    uspp[,.(iso3='USA',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    ukpp[,.(iso3='GBR',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    zppn[,.(iso3='ZAF',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    vnpn[,.(iso3='VNM',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    pdza[,.(iso3='BRA',sex='female',age,pred,hi=cr.ub,lo=cr.lb)]
)

mad.a[,se:=(hi-lo)/3.92]

## meta-analysis
ma.a <- rma(y=pred,sei=se,mods = ~sex+age,data = mad.a)
df <- unique(mad.a[,.(sex,age)])
map.a <- predict(object=ma.a)
map.a <- as.data.table(map.a)
map.a <- cbind(map.a,mad.a[,.(sex,age)])
map.a <- unique(map.a)
## map.a[,c('value','lo','hi'):=.(pred,ci.lb,ci.ub)]

save(map.a,file=here('metaout/map.a.Rdata'))

mad.a[iso3=='BRA',sex:='both']
save(mad.a,file=here('metaout/mad.a.Rdata'))


## xtra cols for plot
map.a[,c('value','lo','hi','iso3'):=.(pred,ci.lb,ci.ub,NA)]
mad.a[,value:=pred]
mad.a[,sex:=factor(sex,levels=c('female','male','both'),ordered = TRUE)]
map.a[,sex:=factor(sex,levels=c('female','male','both'),ordered = TRUE)]
tmpa[,value:=TBM/TB]

a <- 0.2

## graph
MA <- ggplot(mad.a,aes(x=age,y=value,col=iso3,group=iso3)) +
    ## raw data
    geom_point(data=tmpa1,aes(y=value,size=TBM),shape=1,alpha=a)+
    geom_point(data=tmpa2,aes(y=value,size=TBM),shape=1,alpha=a)+ 
    geom_point()+
    geom_line() +
    geom_ribbon(aes(x=age,ymin=lo,ymax=hi,fill=iso3),col=NA,alpha=.3)+
    facet_wrap(~sex) +
    myth +
    scale_y_continuous(label=percent)+
    scale_color_manual(values=cnz)+
    scale_fill_manual(values=cnz)+
    expand_limits(y=0) +
    xlab('Age group') + ylab('TBM in TB') +
    ## MA
    geom_point(data=map.a,aes(x=age,y=pred),col=2,size=2)+
    geom_line(data=map.a,aes(x=age,y=pred),col=2,size=0.7)+
    geom_errorbar(data=map.a,aes(x=age,ymin=ci.lb,ymax=ci.ub),
                  col=2,width=0.25)+
    guides(fill=FALSE,size=FALSE)
MA

ggsave(MA,file=here('metaout/MA.pdf'),w=6,h=4)
save(MA,file=here('USA/MA.Rdata')) #jj


## --- Figure Mb: TBM prop HIV+ve
tmpb <- ALL[!is.na(year) & hiv=='hiv+']
##  "BRA" "VNM" "ZAF"
tmpb[,unique(iso3)]
tmpb[iso3=='BRA',sex:='both']
tmpb[,value:=TBM/TB]
tmpb2 <- tmpb[iso3=='BRA']
tmpb1 <- tmpb[iso3!='BRA']
tmpb2[,age2:=midders(tmpb2$age)]
tmpb2[is.na(age2),age2:=82.5]
## 1  = 20 and 1 per 10
tmpb2[,age:=1+(age2-20)/10]

## data for MA
mad.b <- rbind(
    zppp[,.(iso3='ZAF',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    vnpp[,.(iso3='VNM',sex,age,pred,hi=ci.ub,lo=ci.lb)],
    pdzap[,.(iso3='BRA',sex='female',age,pred,hi=cr.ub,lo=cr.lb)]
)

mad.b[,se:=(hi-lo)/3.92]

## meta-analysis
ma.b <- rma(y=pred,sei=se,mods = ~sex+age,data = mad.b)
df <- unique(mad.b[,.(sex,age)])
map.b <- predict(object=ma.b)
map.b <- as.data.table(map.b)
map.b <- cbind(map.b,mad.b[,.(sex,age)])
map.b <- unique(map.b)
## map.b[,c('value','lo','hi'):=.(pred,ci.lb,ci.ub)]

save(map.b,file=here('metaout/map.b.Rdata'))

mad.b[iso3=='BRA',sex:='both']
save(mad.b,file=here('metaout/mad.b.Rdata'))


## xtra cols for plot
map.b[,c('value','lo','hi','iso3'):=.(pred,ci.lb,ci.ub,NA)]
mad.b[,value:=pred]
mad.b[,sex:=factor(sex,levels=c('female','male','both'),ordered = TRUE)]
map.b[,sex:=factor(sex,levels=c('female','male','both'),ordered = TRUE)]

tmpb[,value:=TBM/TB]

## graph
MB <- ggplot(mad.b,aes(x=age,y=value,col=iso3,group=iso3)) +
    ## raw data
    geom_point(data=tmpb1,aes(y=value,size=TBM),shape=1,alpha=a)+
    geom_point(data=tmpb2,aes(y=value,size=TBM),shape=1,alpha=a)+ 
    geom_point()+
    geom_line() +
    geom_ribbon(aes(x=age,ymin=lo,ymax=hi,fill=iso3),col=NA,alpha=.3)+
    facet_wrap(~sex) +
    myth +
    scale_y_continuous(label=percent)+
    scale_color_manual(values=cnz)+
    scale_fill_manual(values=cnz)+
    expand_limits(y=0) +
    xlab('Age group') + ylab('TBM in TB') +
    ## MA
    geom_point(data=map.b,aes(x=age,y=pred),col=2,size=2)+
    geom_line(data=map.b,aes(x=age,y=pred),col=2,size=0.7)+
    geom_errorbar(data=map.b,aes(x=age,ymin=ci.lb,ymax=ci.ub),
                  col=2,width=0.25)+
    guides(fill=FALSE,size=FALSE)
MB

save(MB,file=here('metaout/MB.Rdata'))
ggsave(MB,file=here('metaout/MB.pdf'),w=6,h=4)

## --- Figure Mc: TBM CFR HIV-ve
tmpc <- ALL[!is.na(TBMdeaths) & hiv=='hiv-']
tmpc[,unique(iso3)]

## add in 
uspd[,se:=(ci.ub-ci.lb)/3.92]
zpdn[,se:=(ci.ub-ci.lb)/3.92]
VDNm[,c('pred','se'):=.(cfr,(hi-lo)/3.92)]
uspd[,iso3:='USA']
zpdn[,iso3:='ZAF']
VDNm[,iso3:='VNM']

## data for MA
mad.c <- rbind(uspd[,.(iso3,sex,age,pred,se,hi=ci.ub,lo=ci.lb)],
               VDNm[,.(iso3,sex,age,pred,se,hi,lo)],
               zpdn[,.(iso3,sex,age,pred,se,hi=ci.ub,lo=ci.lb)])

save(mad.c,file=here('metaout/mad.c.Rdata'))

mad.c[,value:=pred]
mad.c[,vi:=se^2]

## meta-analysis
ma.c <- rma(y=value,sei=se,mods = ~sex+age,data = mad.c)
df <- unique(mad.c[,.(sex,age)])
map.c <- predict(object=ma.c)
map.c <- as.data.table(map.c)
map.c <- cbind(map.c,mad.c[,.(sex,age)])
map.c <- unique(map.c)
## map.c[,c('value','lo','hi'):=.(pred,ci.lb,ci.ub)]

save(map.c,file=here('metaout/map.c.Rdata'))

## xtra cols for plot
map.c[,c('value','lo','hi','iso3'):=.(pred,ci.lb,ci.ub,NA)]
tmpc[,value:=TBMdeaths/TBM]

## graph
MC <- ggplot(mad.c,aes(x=age,y=value,col=iso3,group=iso3)) +
  geom_point(data=tmpc,aes(y=value,size=TBM),shape=1,alpha=a)+ #raw data
  geom_point()+
  geom_line() +
  geom_ribbon(aes(x=age,ymin=lo,ymax=hi,fill=iso3),col=NA,alpha=.3)+
  facet_wrap(~sex) +
  myth +
  scale_y_continuous(label=percent)+
  scale_color_manual(values=cnz)+
  scale_fill_manual(values=cnz)+
  expand_limits(y=0) +
  xlab('Age group') + ylab('CFR in treated TBM') +
  ## MA
  geom_point(data=map.c,aes(x=age,y=pred),col=2,size=2)+
  geom_line(data=map.c,aes(x=age,y=pred),col=2,size=0.7)+
  geom_errorbar(data=map.c,aes(x=age,ymin=ci.lb,ymax=ci.ub),
                col=2,width=0.25)+
  guides(fill=FALSE,size=FALSE)
MC

ggsave(MC,file=here('metaout/MC.pdf'),w=6,h=4)
save(MC,file=here('USA/MC.Rdata'))


## --- Figure Md: TBM CFR HIV+ve
## use SA data for weighting
W <- ALL[iso3=='ZAF' & hiv=='hiv+',.(TBM=sum(TBM)),by=.(sex,age)]

## data for plotting
tmpd <- ALL[!is.na(TBMdeaths) & hiv=='hiv+']
tmpd[,unique(iso3)]

## add in extras
zpdp[,se:=(ci.ub-ci.lb)/3.92]
VDPm[,c('pred','se'):=.(cfr,(hi-lo)/3.92)]
zpdp[,iso3:='ZAF']
VDPm[,iso3:='VNM']

## scale SA data to match Stadelman
## 0.57 (0.48, 0.67)
stm <- 0.57
stsd <- abs(0.48-0.67)/3.92
zpdp <- merge(zpdp,W,by=c('sex','age'))
mcfr <- weighted.mean(zpdp$pred,zpdp$TBM) #17%
cat(stm/mcfr,file=here('metaout/zafac.txt'))
zpdp[,pred2:=pred*stm/mcfr]
zpdp[,pred2:=pmin(1,pred2)]
zpdp[,se2:=pred2*sqrt((stsd/stm)^2+(se/pred)^2)]
zpdp[,c('lo2','hi2'):=.(pred2-1.96*se2,pred2+1.96*se2)]
zpdp[,hi2:=pmin(1,hi2)]

## data for MA
mad.d <- rbind(VDPm[,.(iso3,sex,age,pred,se,hi,lo)],
               zpdp[,.(iso3,sex,age,pred=pred2,
                       se=se2,hi=hi2,lo=lo2)])

save(mad.d,file=here('metaout/mad.d.Rdata'))

mad.d[,value:=pred]
tmpd[,value:=TBMdeaths/TBM]

ggplot(tmpd,aes(age,value,col=iso3,group=iso3)) +
    geom_point(shape=1)+ facet_wrap(~sex)+
    geom_line(data=mad.d,aes(age,value))+
    geom_ribbon(data=mad.d,aes(x=age,ymin=lo,ymax=hi,fill=iso3),
                col=NA,alpha=.3)

## variance weighted mean
map.d <- copy(mad.d)
map.d[,wt:=1/se^2]
map.d[pred==0,wt:=0]
map.d[is.na(pred),c('pred','wt'):=.(0.5,0)]
map.d <- map.d[,.(pred=weighted.mean(pred,wt),
                  pred.var=weighted.mean(se^2,wt^2)),by=.(sex,age)]
map.d[,se:=sqrt(pred.var)]
map.d[,c('ci.lb','ci.ub'):=.(pmax(0,pred-1.96*se),pmin(1,pred+1.96*se))]

save(map.d,file=here('metaout/map.d.Rdata'))

map.d[,c('value','lo','hi','iso3'):=.(pred,ci.lb,ci.ub,NA)]

## graph
MD <- ggplot(mad.d,aes(x=age,y=value,col=iso3,group=iso3)) +
  geom_point(data=tmpd,aes(y=value,size=TBM),shape=1,alpha=a)+ #raw data
  geom_point()+
  geom_line() +
  geom_ribbon(aes(x=age,ymin=lo,ymax=hi,fill=iso3),col=NA,alpha=.3)+
  facet_wrap(~sex) +
  myth +
  scale_y_continuous(label=percent)+
  scale_color_manual(values=cnz)+
  scale_fill_manual(values=cnz)+
  expand_limits(y=0) +
  xlab('Age group') + ylab('CFR in treated TBM') +
  ## MA
  geom_point(data=map.d,aes(x=age,y=pred),col=2,size=2)+
  geom_line(data=map.d,aes(x=age,y=pred),col=2,size=0.7)+
  geom_errorbar(data=map.d,aes(x=age,ymin=ci.lb,ymax=ci.ub),
                col=2,width=0.25)+
  guides(fill=FALSE,size=FALSE)
MD

ggsave(MD,file=here('metaout/MD.pdf'),w=6,h=4)
save(MD,file=here('metaout/MD.Rdata'))


## =========== joint figure ========
PAPa <- MA + guides(size=FALSE)
PAPb <- MB + guides(size=FALSE)
PAPc <- MC + guides(size=FALSE)
PAPd <- MD + guides(size=FALSE)

papa <- ggarrange(PAPa,PAPb,PAPc,PAPd,
                  ncol=2,nrow=2,
                  labels='AUTO',
                  common.legend = TRUE)
papa

ggsave(papa,file=here('FigM.pdf'),w=10,h=8)
ggsave(papa,file=here('FigM.png'),w=10,h=8)


## ======== estimates output for use in modelling ===
map.a[,c('qty','hiv'):=.('prop','hiv-')]
map.b[,c('qty','hiv'):=.('prop','hiv+')]
map.c[,c('qty','hiv'):=.('cfr','hiv-')]
map.d[,c('qty','hiv'):=.('cfr','hiv+')]

parms <- rbindlist(list(
    map.a[,.(qty,hiv,sex,age,pred,ci.lb,ci.ub,se)],
    map.b[,.(qty,hiv,sex,age,pred,ci.lb,ci.ub,se)],
    map.c[,.(qty,hiv,sex,age,pred,ci.lb,ci.ub,se)],
    map.d[,.(qty,hiv,sex,age,pred,ci.lb,ci.ub,se)]
))

fwrite(parms,file=here('meta.combined.estimates.csv'))

## NOTE
## TODO consider removing metaplots?

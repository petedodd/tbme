## libraries
library(here)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(scales)

## utilities
absspace <- function(x,...) {             #works
  format(abs(x), ..., big.mark=" ",scientific = FALSE, trim = TRUE)
}
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnlth <-     theme(axis.text.x = element_text(angle=45,hjust=1),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.background = element_blank(),
                   legend.position = 'top',
                   panel.border = element_rect(colour = "black"))



## load data
load(here('outdata/GA.Rdata'))
load(here('outdata/RA.Rdata'))
load(here('outdata/R.Rdata'))

## Global age and sex
GAM <- melt(GA,id.vars = c('sex','age'))
GAM[,qty:=ifelse(grepl('deaths',variable),'deaths','incidence')]
GAM[,treatment:=ifelse(grepl('unt',variable),'untreated','treated')]
agz <- GAM[,unique(age)]
GAM$age <- factor(GAM$age,levels=agz,ordered=TRUE)
GAM$treatment <- factor(GAM$treatment,levels=c('untreated','treated'),
                        ordered=TRUE)


GP <- ggplot(GAM,aes(age,value,fill=treatment)) +
  geom_bar(stat='identity') +
  facet_grid(qty~sex) +
  scale_y_continuous(label=absspace) +
  ylab('Number')+
  ggthemes::scale_fill_colorblind()+
  theme_minimal() + theme(legend.position = 'top')
GP

## ggsave(GP,file=here('outdata/GA.pdf'),h=7,w=10)
## ggsave(GP,file=here('outdata/GA.png'),h=7,w=10)


GP <- ggplot(GAM,aes(age,value,fill=treatment)) +
    geom_bar(stat='identity',position = 'dodge') +
    facet_grid(qty~sex) +
    scale_y_continuous(label=absspace) +
    ylab('Number')+
    ggthemes::scale_fill_colorblind()+
    theme_minimal() + theme(legend.position = 'top')
GP

## ggsave(GP,file=here('outdata/GA2.pdf'),h=7,w=10)
## ggsave(GP,file=here('outdata/GA2.png'),h=7,w=10)



## regional
RAM <- melt(RA,id.vars = c('g_whoregion','sex','age'))
RAM <- RAM[,.(value=sum(value)),by=.(g_whoregion,age,variable)]
RAM[,qty:=ifelse(grepl('deaths',variable),'deaths','incidence')]
RAM[,treatment:=ifelse(grepl('unt',variable),'untreated','treated')]
RAM$age <- factor(RAM$age,levels=agz,ordered=TRUE)
RAM$treatment <- factor(RAM$treatment,levels=c('untreated','treated'),
                        ordered=TRUE)
RAM$qty <- factor(RAM$qty,levels=c('incidence','deaths'),ordered=TRUE)


GP <- ggplot(RAM,aes(age,value,fill=treatment)) +
  geom_bar(stat='identity') +
  facet_grid(qty~g_whoregion) +
  scale_y_continuous(label=absspace) +
  ylab('Number')+
  ggthemes::scale_fill_colorblind()+
  theme_bw() + pnlth + ggpubr::grids()
GP

ggsave(GP,file=here('graphs/RA.pdf'),h=7,w=12)
ggsave(GP,file=here('graphs/RA.png'),h=7,w=12)


GP <- ggplot(RAM,aes(age,value,fill=treatment)) +
    geom_bar(stat='identity',position = 'dodge') +
    facet_grid(qty~g_whoregion) +
    scale_y_continuous(label=absspace) +
    ylab('Number')+
    ggthemes::scale_fill_colorblind()+
    theme_minimal() + theme(legend.position = 'top') + rot45
GP

## ggsave(GP,file=here('graphs/RA2.pdf'),h=7,w=12)
## ggsave(GP,file=here('graphs/RA2.png'),h=7,w=12)



GP <- ggplot(RAM,aes(age,value,fill=treatment)) +
  geom_bar(stat='identity') +
  facet_grid(g_whoregion~qty) +
  scale_y_continuous(label=absspace) +
  ylab('Number')+
  ggthemes::scale_fill_colorblind()+
  theme_minimal() + theme(legend.position = 'top')
GP

## ggsave(GP,file=here('graphs/RAb.pdf'),w=7,h=12)
## ggsave(GP,file=here('graphs/RAb.png'),w=7,h=12)


GP <- ggplot(RAM,aes(age,value,fill=treatment)) +
    geom_bar(stat='identity',position='dodge') +
    facet_grid(g_whoregion~qty) +
    scale_y_continuous(label=absspace) +
    ylab('Number')+
    ggthemes::scale_fill_colorblind()+
    theme_minimal() + theme(legend.position = 'top')
GP

## ggsave(GP,file=here('graphs/RA2b.pdf'),w=7,h=12)
## ggsave(GP,file=here('graphs/RA2b.png'),w=7,h=12)


RAMS <- melt(RA,id.vars = c('g_whoregion','sex','age'))
RAMS[,qty:=ifelse(grepl('deaths',variable),'deaths','incidence')]
RAMS[,hiv:=ifelse(grepl('hn',variable),'HIV-negative','HIV-positive')]
RAMS[,treatment:=ifelse(grepl('unt',variable),'untreated','treated')]

RAMS$age <- factor(RAMS$age,levels=agz,ordered=TRUE)
RAMS$treatment <- factor(RAMS$treatment,
                         levels=rev(c('untreated','treated')),
                        ordered=TRUE)
RAMS$qty <- factor(RAMS$qty,levels=c('incidence','deaths'),ordered=TRUE)
RAMS$hiv <- factor(RAMS$hiv,levels=c('HIV-negative','HIV-positive'),
                  ordered=TRUE)

## age & sex

## age & sex & HIV


GP <- ggplot(RAMS,aes(age,value,fill=paste(treatment,hiv))) +
    geom_bar(stat='identity') +
    facet_grid(qty + sex~g_whoregion) +
    scale_y_continuous(label=absspace) +
    ylab('Number')+
    ggthemes::scale_fill_colorblind()+
    theme_bw() + pnlth + ggpubr::grids() +
    labs(fill='')
GP

ggsave(GP,file=here('graphs/RAhs.pdf'),h=10,w=12)
ggsave(GP,file=here('graphs/RAhs.png'),h=10,w=12)

GP <- ggplot(RAMS,aes(age,value,fill=paste(treatment,hiv))) +
    geom_bar(stat='identity') +
    facet_grid(g_whoregion ~ qty + sex) +
    scale_y_continuous(label=absspace) +
    ylab('Number')+
    ggthemes::scale_fill_colorblind()+
    theme_bw() + pnlth + ggpubr::grids() +
    labs(fill='')
GP

ggsave(GP,file=here('graphs/RAhs2.pdf'),h=10,w=12)
ggsave(GP,file=here('graphs/RAhs2.png'),h=10,w=12)


## ======================
## --- maps ------
## libraries for mapping
library(sf)
library(tmap)
library(maptools)
library(rgeos)

## loading
load(file=here('outdata/AN.Rdata')) #country level estimates
H <- fread(here('indata/TB_burden_countries_2020-02-24.csv'))
H <- H[year==2018,.(iso3,e_pop_num)] #for population
H[,sum(e_pop_num)]/1e9               #check units
## NOTE should be over 15 pop

## --- global
mpd <- AN[,.(TBM.total = sum(TBM.hn.treated) + sum(TBM.hp.treated) +
               sum(TBM.hn.untreated) + sum(TBM.hp.untreated),
           TBMdeaths.total = sum(TBMdeaths.hn.treated) +
               sum(TBMdeaths.hp.treated) + sum(TBMdeaths.hn.untreated) +
               sum(TBMdeaths.hp.untreated)),by=iso3]

mpd <- merge(mpd,H,by='iso3')
mpd[,`TBM incidence`:=TBM.total]
mpd[,`TBM incidence per million`:=1e6*TBM.total/e_pop_num]
mpd[,TBI:=1e6*TBM.total/e_pop_num]

mpd[,`TBM deaths`:=TBMdeaths.total]
mpd[,`TBM deaths per million`:=1e6*TBMdeaths.total/e_pop_num]
mpd[,TBmu:=1e6*TBMdeaths.total/e_pop_num]

## world map data
data(wrld_simpl)

## create  map data
MPD <- wrld_simpl#[wrld_simpl$ISO3 %in% mpd$iso3, ]
MPD <- sp::merge(MPD,mpd,by.x = 'ISO3',by.y='iso3')

names(MPD)
MP <- st_as_sf(MPD)
MP$mid <- st_centroid(MP$geometry)

mpd[,range(TBM.total)]

##  version
p <- ggplot(data=MP) +
    geom_sf(aes(fill=TBI)) +
    scale_fill_distiller(name='TBM incidence per million',
                         palette='Reds',direction = 1)+
    geom_sf(aes(geometry = mid, size = as.numeric(TBM.total)),
            show.legend = "point")+ #,shape=1) +
    scale_size_continuous(name='TBM incidence',
                          range=c(1,18))+ #,breaks=c(1,10,50,100)
    theme_minimal() +
    theme(legend.position = c(.1,.45),#'left',
          legend.title.align = 0.5,
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
p

ggsave(p,file=here('graphs/Map.pdf'),w=18,h=12)
ggsave(p,file=here('graphs/Map.png'),w=18,h=12)

## TODO
## fix denominator data to be population >=15

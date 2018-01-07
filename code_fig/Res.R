################################################
## Figures Res
##
## first version: Aug 7th 2016
## last modification: Aug 7th 2016
## Kevin Cazelles
##
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin15.0.0 (64-bit)
## Running under: OS X 10.11 (El Capitan)
################################################
################################################




#### PATH / PACKAGES / FUNCTIONS / DATA
source("~/ownCloud/Dom_cooc_inter/code/load_data.R")

mylegfend <- function(x, col, ...){
  legend(x=x, bty="n", legend=c("Homogeneous", "GLM", "RF"), fill=col,  border=1, ...)
}


## Packages
mapspacks()
library(igraph)

## Colors
colb1="#50b5d5"
blupal <- colorRampPalette(c("#50b5d5","black"))(100)
colb2=blupal[20]
colb3=blupal[40]
colb4=blupal[60]
colb5=blupal[80]
##
colg1="grey19"
colg2="grey25"
colg3="grey70"
colg4="grey75"
colg5="grey85"


## Basic par
mypar <- list(bg=1, fg=colg5, col.axis=colg5, col.lab=colg5, col.main=colg5, cex.main=3, cex.lab=2.4, font=2)

#### figure size (cm)
hg=15
wi=15*1.62



#### POST TREATMENT

##-- Keeping species found in at least 1% of the total numer of sits
slcAbdtSp <- function(x, nsite, perc=1){
  ids <- which(x$occ_sp1>(.01*perc*nsite) & x$occ_sp2>(.01*perc*nsite))
  return(x[ids,])
}

## A- Roselin
load("./data/roselin/roselin_sites.Rdata")
nsros <- length(roselin_sites)
roselin_cooc_all %<>% slcAbdtSp(nsros)


## B- Pitcher
# load("./data/pitcher/pitcher_sites.Rdata")
nsrop <- table(pitcher_presences$siteno) %>% sum
##
rep_site <- table(pitcher_presences[,1])
pitcher_cooc_all %<>% slcAbdtSp(nsrop)
#-- remove detrirus
idde <- which(pitcher_cooc_all$id_sp1==1  | pitcher_cooc_all$id_sp2==1)
pitcher_cooc_all %<>% extract(-idde,)


##-- C- Hummingbirds
humbirds_sites <- readRDS("./data/humbirds/humbirds_sites.Rds")
nsroh <- length(humbirds_sites)
## Species 1 and 2 are the same species => sp2 removed the first row
idsp2 <- humbirds_cooc_all$id_sp1 == 2 | humbirds_cooc_all$id_sp2 == 2
humbirds_cooc_all %<>% extract(!idsp2, )


##-- D- North American Trees
tree_species[,4:14] %>% kable(row.names=FALSE) %>%  cat(file="./tab/table_trees.md", sep = "\n")


##-- E- Stoc
stoc_sites <- readRDS("./data/stoc/stoc_sites.Rds")
nssto <- length(stoc_sites)
stoc_cooc_all %<>% slcAbdtSp(nssto)





## Path

################################################
##-- Figures distance selected pair 'figOrder'
##-- selected the different kind of interaction
idsh <- which(roselin_cooc_all$type_sp1=="Salix" & roselin_cooc_all$type_sp2=="Herbivore")
ordsh <- getIdOrder(roselin_cooc_all$sht_pth[idsh])
idhp <- which(roselin_cooc_all$type_sp1=="Herbivore" & roselin_cooc_all$type_sp2=="Parasitoid")
ordhp <- getIdOrder(roselin_cooc_all$sht_pth[idhp])

##-- select humming birds
idb <-  c(1,3:11)
idhb1 <- which(humbirds_cooc_all$id_sp1%in%idb &
  humbirds_cooc_all$id_sp2 > 11)
ordhb1 <- getIdOrder(humbirds_cooc_all$sht_pth[idhb1])
ordhb2 <- getIdOrder(humbirds_cooc_all$sht_pth[idhb2])

##-- pitcher all
ordp2 <- getIdOrder(pitcher_cooc_all$sht_pth)

findeg <- . %>% unique %$% deg_hs %>% equals(0) %>% not %>% which
mycol <- c("grey50", colb1, colb4)
pdeg <- 82

png(file="/Users/KevCaz/ownCloud/presESA/fig/figR1.png", width=.5*wi, height=.5*hg, res=300, unit="cm")
  par(mypar)
  par(mar=c(0,0,0,0), las=1)
  ##
  panel2(1, 1, ylab="", args_lab=c(cex=cex_leg), byrow=TRUE, cex_labx=.12, cex_laby=0.1)
  # expression(frac(O[ij]-E[ij],SD[ij]))
  mylegend("center", col=mycol, cex=.95, ncol=3)
  ##---
  par(mar=c(2.5,1.5,1.5,.5), lwd=0.8)
  ##---
  groupedBoxplot(roselin_cooc_all[idsh, c('Z_hyp', 'Z_glm', 'Z_rf')], ordsh,
    c(-4,20), col=mycol, by=4, txt_ord=c("1","3","5","7"), cex_txt=1)
  ids <- roselin_cooc_all[, c("id_sp2","deg_hs")] %>% findeg
  mtext(2, at=1.08*percY(100), text="Z-score", las=1)
  text(percX(99), percY(92), "mean degree in:", pos=2, cex=1.1)
  text(percX(99), percY(pdeg), paste0(roselin_cooc_all$deg_hs[ids] %>% mean %>% round(2)), pos=2, cex=1.1)
  ##---
dev.off()


png(file="/Users/KevCaz/ownCloud/presESA/fig/figR2.png", width=.5*wi, height=.5*hg, res=300, unit="cm")
  par(mypar)
  par(mar=c(0,0,0,0), las=1)
  ##
  panel2(1, 1, ylab="", args_lab=c(cex=cex_leg), byrow=TRUE, cex_labx=.12, cex_laby=0.1)
  # expression(frac(O[ij]-E[ij],SD[ij]))
  mylegend("center", col=mycol, cex=.95, ncol=3)
  ##---
  par(mar=c(2.5,1.5,1.5,.5), lwd=0.8)
  ##---
  groupedBoxplot(roselin_cooc_all[idhp, c('Z_hyp', 'Z_glm', 'Z_rf')], ordhp,
    c(-4,20), col=mycol, by=4, txt_ord=c("1","3","5","7"), cex_txt=1)
  #- degree ph
  idp <- roselin_cooc_all[,c("id_sp2","deg_ph")] %>% unique %$% deg_ph %>% equals(0) %>% not %>% which
  mtext(2, at=1.08*percY(100), text="Z-score", las=1)
  text(percX(99), percY(92), "mean degree in:", pos=2, cex=1.1)
  text(percX(99), percY(pdeg), paste0(roselin_cooc_all$deg_ph[idp] %>% mean %>% round(2)), pos=2, cex=1.1)

dev.off()


png(file="/Users/KevCaz/ownCloud/presESA/fig/figR3.png", width=.5*wi, height=.5*hg, res=300, unit="cm")
  par(mypar)
  par(mar=c(0,0,0,0), las=1)
  ##
  panel2(1, 1, ylab="", args_lab=c(cex=cex_leg), byrow=TRUE, cex_labx=.12, cex_laby=0.1)
  # expression(frac(O[ij]-E[ij],SD[ij]))
  mylegend("center", col=mycol, cex=.95, ncol=3)
  ##---
  par(mar=c(2.5,1.5,1.5,.5), lwd=0.8)
  ##---
  groupedBoxplot(humbirds_cooc_all[idhb1, c('Z_hyp', 'Z_glm', 'Z_rf')], ordhb1, c(-4,6), col=mycol, by=2, txt_ord=c("1","3","5","7"), cex_txt=1)
  #-- deg
  mdeg <- humbirds_cooc_all[idhb1, c('id_sp1', 'deg_sp1')] %$% deg_sp1 %>% mean
  mtext(2, at=1.08*percY(100), text="Z-score", las=1)
  text(percX(99), percY(92), "mean degree in:", pos=2, cex=1.1)
  text(percX(99), percY(pdeg), paste0(mdeg %>% round(2)), pos=2, cex=1.1)
  ##
dev.off()




png(file="/Users/KevCaz/ownCloud/presESA/fig/figR4.png", width=.5*wi, height=.5*hg, res=300, unit="cm")
  par(mypar)
  par(mar=c(0,0,0,0), las=1)
  ##
  panel2(1, 1, ylab="", args_lab=c(cex=cex_leg), byrow=TRUE, cex_labx=.12, cex_laby=0.1)
  # expression(frac(O[ij]-E[ij],SD[ij]))
  mylegend("center", col=mycol, cex=.95, ncol=3)
  ##---
  par(mar=c(2.5,1.5,1.5,.5), lwd=0.8)
  ##---
  groupedBoxplot(pitcher_cooc_all[, c('Z_hyp', 'Z_glm', 'Z_rf')], ordp2, c(-4,6), col=mycol, by=2, txt_ord=c("1","2","3"), cex_txt=1)
  mtext(2, at=1.08*percY(100), text="Z-score", las=1)
  text(percX(99), percY(92), "mean degree in:", pos=2, cex=1.2)
  text(percX(99), percY(pdeg), labels=degree(pitcher_metaweb, mode="in") %>% mean %>% round(2), pos=2, cex=1.2)
  ##---
dev.off()

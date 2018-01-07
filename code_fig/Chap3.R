#################################################
################################################
## Figures Chapitre 3
##
## first version: Apr 18th 2016
## last modification: Apr 18th 2016
## Kevin Cazelles
##
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin15.0.0 (64-bit)
## Running under: OS X 10.11 (El Capitan)
################################################
################################################



#################
##################

## Path
setwd("/Users/kcazelles/Dropbox/Seminaire")

## Packages
mapspacks()
library(igraph)
library(mgcv)

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
colg3="grey50"
colg4="grey75"
colg5="grey85"

## Figure size (cm)
hg=15
wi=15*1.62

## Basic par
mypar <- list(bg=1, fg=colg5, col.axis=colg5, col.lab=colg5, col.main=colg5, cex.main=3, cex.lab=2.6, font=2, las=1, cex.axis=1.4)

##
bgcol <- function(col=colg5) rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col=col, border=NA)



load("~/Dropbox/DomKev/Dom_cooc_inter/data/tree/tree_sites.Rdata")
load("~/Dropbox/DomKev/Dom_cooc_inter/data/humbirds/humbirds_sites.Rdata")
load("~/Dropbox/DomKev/Dom_cooc_inter/data/pitcher/pitcher_sites.Rdata")
load("~/Dropbox/DomKev/Dom_cooc_inter/data/roselin/roselin_sites.Rdata")

carab <- readOGR(dsn="/Users/kcazelles/Documents/Data/Geodata/carib_wvs_geo_wgs84", layer="carib_wvs_geo_wgs84")
CAN1 <- readOGR(dsn="/Users/kcazelles/Documents/Data/Geodata/Pays_adm/CAN_adm", layer="CAN_adm1")
USA1 <- readOGR(dsn="/Users/kcazelles/Documents/Data/Geodata/Pays_adm/USA_adm", layer="USA_adm1")
MEX0 <- readOGR(dsn="/Users/kcazelles/Documents/Data/Geodata/Pays_adm/MEX_adm", layer="MEX_adm0")
lCAN1 <- gSimplify(CAN1, tol=0.01)
lUSA1 <- gSimplify(USA1, tol=0.005)
lMEX0 <- gSimplify(MEX0, tol=0.01)

## Salix
world <- readOGR(dsn="/Users/kcazelles/Documents/Data/Geodata/Pays_adm/countries_shp", layer="countries")
europe <- world[which(world$CONTINENT=="Europe"),]
europe2 <- gSimplify(europe, tol=0.001, topologyPreserve=TRUE)




figmaps <- function(folder="~/Dropbox/DomKev/Dom_cooc_inter/fig/", col2=colind, col1=colrf, mypar=list(mar=c(2,4,2,4), las=1)){

## Hummingbirds
jpeg(filename=paste0(folder,"map_hbirds.jpeg"), unit="cm", width=wi*0.8, height=hg, res=300)
  par(mypar)
  par(mar=c(4,4,4,4))
  plot(c(-85,-55), c(10,25), type="n", axes=FALSE, ann=FALSE)
  plot(carab, col=col2, border=NA, add=TRUE)
  plot(humbirds_sites, add=TRUE, pch=19, col=col1, cex=0.8)
  seqlon <- seq(-85,-55,10)
  seqlat <- seq(10,25,5)
  axis(1, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(3, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(2, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  axis(4, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  abline(v=seqlon, h=seqlat, lty=2, lwd=.4)
  pchImage(-58, 23, file="~/Dropbox/Seminaire/img/shape.png", cex.x=2, cex.y=2.4, col=col1)
dev.off()


## Pitcher
jpeg(filename=paste0(folder,"map_pitcher.jpeg"), unit="cm", width=wi*1.1, height=hg, res=300)
  par(mypar)
  par(mar=c(4,4,4,4))
  seqlon <- seq(-120,-55,20)
  seqlat <- seq(15,65,10)
  plot(c(-124,-55), c(25,60), type="n", axes=FALSE, ann=FALSE)
  plot(lCAN1, add=TRUE, col=col2, lwd=0.2)
  plot(lUSA1, add=TRUE, col=col2, lwd=0.2)
  plot(lMEX0, add=TRUE, col=col2, lwd=0.2)
  axis(1, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(3, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(2, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  axis(4, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  abline(v=seqlon, h=seqlat, lty=2, lwd=.4)
  ##
  plot(pitcher_sites, add=TRUE, pch=19, col=col1, cex=1.2)
  ##
  pchImage(-62,30,file="./img/sarracenia.png", cex.x=1, cex.y=2.2, col=col1)
dev.off()

## Salix
jpeg(filename=paste0(folder,"map_salix.jpeg"), unit="cm", width=1.4*hg, height=wi, res=300)
  layout(matrix(c(1,2),1), widths=c(1,0.4))
  par(mypar)
  par(mar=c(4,4,4,4))
  seqlon <- seq(0,30,10)
  seqlat <- seq(30,75,10)
  plot(c(2,32), c(38,70), type="n", axes=FALSE, ann=FALSE)
  plot(europe2, col=col2, lwd=0.2, add=TRUE)
  axis(1, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(3, lwd=0, at=seqlon, labels=paste0(abs(seqlon), "°E"))
  axis(2, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  axis(4, lwd=0, at=seqlat, labels=paste0(seqlat, "°N"))
  abline(v=seqlon, h=seqlat, lty=2, lwd=.4)
  ##
  plot(roselin_sites, add=TRUE, pch=19, col=col1, cex=0.6)
  ##
  par(mar=c(4,0,1,0))
  plot0(c(0.75,1.25),c(0.75,3.25))
  text(rep(1,3),1:3, c("S","H","P"),col=col1, cex=3.8, font=1)
  vcx0 <- rep(1,3)
  vcy0 <- c(1.25, 2.25)
  vcy1 <- c(1.75, 2.75)
  for (i in 1:3) arrows2(x0=vcx0[i], y0=vcy0[i], y1=vcy1[i], col=col1, cex.arr=0.28, cex.hl=0.8, cex.hh=1.8, border=NA)
  ##
dev.off()
}


figmaps(folder="/Users/kcazelles/Dropbox/Seminaire/fig/", col2=colg5, col1=colb3, mypar=c(mypar, mar=c(2,2,2,2)))






################################################
################################################
################################################
################################################

##
setwd("/Users/kcazelles/Dropbox/DomKev/Dom_cooc_inter")
source("./code/function_cooc.R")

load("./data/roselin/roselin_species.Rdata")
##
load("./data/roselin/roselin_presences.Rdata")
load("./data/roselin/roselin_occ_glm.Rdata")
load("./data/roselin/roselin_occ_rf.Rdata")
##
load("./data/tree/tree_presences.Rdata")
load("./data/tree/tree_occ_glm.Rdata")
load("./data/tree/tree_occ_rf.Rdata")
##
load("./data/pitcher/pitcher_presences.Rdata")
load("./data/pitcher/pitcher_occ_glm.Rdata")
load("./data/pitcher/pitcher_occ_rf.Rdata")
##
load("./data/humbirds/humbirds_presences.Rdata")
load("./data//humbirds/humbirds_occ_glm.Rdata")
load("./data//humbirds/humbirds_occ_rf.Rdata")
##
load("./data/roselin/roselin_cooc_all.Rdata")
load("./data/pitcher/pitcher_cooc_all.Rdata")
load("./data/humbirds/humbirds_cooc_all.Rdata")
load("./data/tree/tree_cooc_all.Rdata")



#### Data :

id <- which(roselin_cooc_all$occ_sp1>5)
roselin_cooc_all <- roselin_cooc_all[id,]
##
dim(roselin_species[unique(roselin_cooc_all$id_sp1),])

##
id <- which(pitcher_cooc_all$occ_sp1>5)
pitcher_cooc_all <- pitcher_cooc_all[id,]

##
ros <- getVal(roselin_cooc_all)
pit <- getVal(pitcher_cooc_all)
hum <- getVal(humbirds_cooc_all)
tre <- getVal(tree_cooc_all)

##
ros_ord <- getIdOrder(roselin_cooc_all$sht_pth)
lapply(ros_ord,length)
##
pit_ord <- getIdOrder(pitcher_cooc_all$sht_pth)
lapply(pit_ord,length)
##
hum_ord <- getIdOrder(humbirds_cooc_all$sht_pth)
tre_ord <- getIdOrder(tree_cooc_all$dist_class)
tre_ord[[2]] <- c(tre_ord[[1]],tre_ord[[2]])
tre_ord <- tre_ord[-1]


################################################
################################################


setwd("/Users/kcazelles/Dropbox/Seminaire")

myRect <- function(col="#CCCCCCAA") rect(par()$usr[1],-1.96,par()$usr[2],1.96, col=col, border=NA)

mylegend2 <- function(x, colors, ...){
  legend(x=x, bty="n", legend=c("Homogène", "GLM", "RF"), fill=colors,  border=c(1,NA,NA,NA), ncol=3, ...)
}

################################################
############### SYNTHESE
################################################

myplotsyn <- function(val, ord, rgy, colors, by, txt1="Interactions directes", txt2="Interactions indirectes"){

  plot0(c(0.5,7.5), rgy)
  title(ylab="Z-score")
  myRect(col=colg2)
  ##
  vali <- lapply(val, function(x) x[ord[[1]]])
  valn <- lapply(val, function(x) x[unlist(ord[-1])])
  seqi <- 1:3
  seqn <- 4+1:3
  ##
  myboxplot(vali[-1], at=seqi, col=colors, lwd=1.4)
  myboxplot(valn[-1], at=seqn, col=colors, lwd=1.4)
  abline(h=0, lty=2)
  lines(c(.5,3.5),rep(par()$usr[3],2), lwd=4)
  lines(c(4.5,7.5),rep(par()$usr[3],2), lwd=4)
  ##
  mtext(side=1, at=mean(c(.5,3.5)), text=c(txt1,paste0("n = ",length(vali[[1]]))), cex=c(2,1.7), line=c(1,2.8))
  mtext(side=1, at=mean(4+c(.5,3.5)),text=c(txt2,paste0("n = ",length(valn[[1]]))), cex=c(2,1.7), line=c(1,2.8))
}

mycol <- c(colg3, colb1, colb4)

figHU <- function(filename="./fig/fig20.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
  par(mypar)
  par(mar=c(5,5,2,2), cex.axis=1.6, las=1)
  valy <- c(-2,0,4)
  myplotsyn(hum, hum_ord, colors=mycol, rgy=valy[c(1,3)], 4)
  axis(2, at= valy, labels=valy, col=colg3, lwd=1.4)
  mylegend2("topright", colors=mycol, cex=1.8)
  dev.off()
}
figHU()


figPI <- function(filename="./fig/fig21.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
  par(mypar)
  par(mar=c(5,5,2,2), cex.axis=1.4, las=1)
  valy <- c(-3,0,5)
  myplotsyn(pit, pit_ord, valy[c(1,3)], 4, colors=mycol)
  axis(2, at= valy, labels=valy, col=colg3, lwd=1.4)
  mylegend2("topright", colors=mycol, cex=1.8)
  dev.off()
}
figPI()


figSA <- function(filename="./fig/fig22.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
  par(mypar)
  par(mar=c(5,5,2,2), cex.axis=1.4)
  valy <- c(-4,0,12)
  myplotsyn(ros, ros_ord, valy[c(1,3)], 4, colors=mycol)
  axis(2, at= valy, labels=valy, col=colg3, lwd=1.4)
  mylegend2("topright", colors=mycol, cex=1.8)
  dev.off()
}
figSA()








################################################
############### FIGURE ORDER
################################################

##
id1 <- roselin_cooc_all$id_sp1
id2 <- roselin_cooc_all$id_sp2
##
id_SH <- which(roselin_species$Type[id1]=="Salix" & roselin_species$Type[id2]=="Herbivore")
id_HP <- which(roselin_species$Type[id1]=="Herbivore" & roselin_species$Type[id2]=="Parasitoid")
##
id_SS <- which(roselin_species$Type[id1]=="Salix" & roselin_species$Type[id2]=="Salix")
id_HH <- which(roselin_species$Type[id1]=="Herbivore" & roselin_species$Type[id2]=="Herbivore")
id_PP <- which(roselin_species$Type[id1]=="Parasitoid" & roselin_species$Type[id2]=="Parasitoid")
##
ls_SH <- lapply(ros_ord, function(x) x[x%in%id_SH])
ls_HP <- lapply(ros_ord, function(x) x[x%in%id_HP])
ls_SS <- lapply(ros_ord, function(x) x[x%in%id_SS])
ls_HH <- lapply(ros_ord, function(x) x[x%in%id_HH])
ls_PP <- lapply(ros_ord, function(x) x[x%in%id_PP])

##
plotorder <- function(val, ord, rgy, by=2, colors, ngp=length(ord), txt_ord=NULL){
  rgx <- c(0.5,(5*ngp-.5))
  plot0(rgx,rgy)
  title(ylab="Z-score")
  myRect(col=colg2)
  ##
  for (i in 1:ngp){
    (i-1)*5+c(1,4)
    bsv <- (i-1)*5
    myboxplot(lapply(val[-1], function(x) x[ord[[i]]]), at=seq(bsv+1,bsv+4,len=3), col=colors)
    trt <- bsv+c(1,4)+c(-.5,.5)
    lines(trt, rep(min(rgy),2))
    if (!is.null(txt_ord)) txt <- txt_ord[i]
    else txt <- i
    mtext(side=1, text=txt, at=mean(trt), line=-0.25, cex=1.8, font=2)
    mtext(side=1, text=paste0("n=",length(ord[[i]])), at=mean(trt), line=1.4, cex=1.6)
    ##
    abline(h=0, lty=2)
    axis(2, at=seq(rgy[1],rgy[2],by=by), labels=seq(rgy[1],rgy[2],by=by), cex.axis=1.5)
    #
  }
}



figSPH <- function(filename="./fig/fig23.png",  txt=c("","")){
  png(filename, height=hg, width=1.5*wi, unit="cm", res=300)
  par(mypar)
  par(mar=c(5,5,2,2), cex.axis=1.4, mfrow=c(1,2))
  ##
  plotorder(val=ros, ord=ls_SH[c(1,3,5)], rgy=c(-4,20), ngp=3, by=4, txt_ord=paste0("ordre ",c(1,3,5)),  colors=mycol)
  mtext(side=3, text="Saules / Insectes herbivores", line=.2, adj=0, cex=2)
  mtext(side=3, line=-8, text=txt[1], cex=2.6)
  ##
  plotorder(val=ros, ord=ls_HP[c(1,3,5)], rgy=c(-4,20), ngp=3, by=4, txt_ord=paste0("ordre ",c(1,3,5)),  colors=mycol)
  mtext(side=3, text="Insectes herbivores / Parasitoïdes", line=.2, adj=0, cex=2)
  mtext(side=3, line=-8, text=txt[2], cex=2.6)
  mylegend2("topright", colors=mycol, cex=1.5)
  dev.off()
}
figSPH()

figSPH(filename="./fig/fig24.png", txt=c("Spécialistes", "Généralistes"))

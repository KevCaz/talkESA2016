#################################################
################################################
## Figures Chapitre 1
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



## Path
setwd("/Users/kcazelles/Dropbox/Seminaire")

## Packages
mapspacks()
library(igraph)
library(mgcv)

## Colors
colb1="#50b5d5"
colb2="#1781a2"
colg1="grey15"
colg2="grey25"
colg3="grey50"
colg4="grey75"
colg5="grey85"

## Figure size (cm)
hg=15
wi=15*1.62

## Basic par
mypar <- list(bg=1, fg=colg5, col.axis=colg5, col.lab=colg5, col.main=colg5, cex.main=3, cex.lab=2.6, font=2, las=1)








## ---- figMW MacArthur Wilson
figMW <- function(filename="./fig/fig8.png"){
  n=100
  A = 0:n
  B = matrix(rep(0,3*(n+1)),101)
  C = matrix(rep(0,3*(n+1)),101)
  for (k in 0:n) {
	  c=0.002
	  e=0.002
	  B[k+1,1]=1-(1-c)^(n-k)
	  C[k+1,1]=1-(1-e)^k
	  c=0.001
	  e=0.005
	  B[k+1,2]=1-(1-c)^(n-k)
	  C[k+1,2]=1-(1-e)^k
	  c=0.005
	  e=0.001
	  B[k+1,3]=1-(1-c)^(n-k)
	  C[k+1,3]=1-(1-e)^k
  }
  ##
  png(filename, height=hg, width=wi, unit="cm", res=300)
    layout(matrix(c(1,2),ncol=2), width=c(1,0.22))
    par(mypar)
    par(mar=c(4,4,1,.5), mgp=c(2,1,0))
    ##
    plot(c(0,100),c(0,0.44), col=0, ann=FALSE, axes=FALSE)
    pal=c(colg5,colb1,colg5)
    for (i in 2:3){
	    lines(A, B[,i], col=pal[i], lwd=4)
	    lines(A, C[,i], col=pal[i], lwd=4, lty=4)
    }
    title(xlab="Richesse spécifique sur l'île", ylab="Probabilité d'un évènement", col.lab=colg3)
    box(bty="l", lwd=3.4, col=colg2)
    # axis(1)
    # axis(2)
    points(c(16.53,82.92), c(0.08,0.08), col=c(colb1,colg5), cex=3.2, pch=19)
    legend("top", c("Colonisation", "Extinction"), lty=c(1,4), ncol=1, bty="n", cex=2, col=colg3, lwd=3.5, seg.len=3)
    axis(1,at=c(16.53,82.92), lwd=0, lwd.ticks=4, labels=c("",""), col=colg2, tck=.04)
    ##
    par(mar=c(4,0,1,0))
    plot0(c(0,10),c(0,10))
    abline(h=8, lwd=2, col=colg3)
    text(0,9,"Continent", font=2, cex=2, pos=4, col=colg3)
    rect(.5,6.2,6,3.5, col=colg5, border=NA)
    arrows2(x0=mean(c(.5,6)), y0=8, y1=6.2, col=colg2, border=NA, prophead=FALSE)
    rect(7.5,3,9.5,1.8, col=colb1, border=NA)
    arrows2(x0=mean(c(7.5,9.5)), y0=8, y1=3, col=colg2, border=NA, prophead=FALSE)
  dev.off()
}
figMW()







## ---- FIGURE 9 : Post Ecogrpahy
emptyply <- function(x,y) plot(x,y, type="n", axes=FALSE, ann=FALSE)
bgcol <- function(col=colg5) rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col=col, border=NA)

mypal <- colorRampPalette(c("#032739", colb1, "white"))(100)
mypal2 <- c(mypal[50],mypal[75],mypal[75],mypal[75],mypal[20],mypal[80])
myblue <- mypal[75]
cex_txt <- 2.8
cex_lb <- 3.4

figEcogr <- function(filename="./fig/fig9.png"){

  jpeg(file=filename, units="cm", res=300, width=1.2*wi, height=hg)
  ##
  layout(matrix(c(1,2,2,2,3,4,5,6,0,7,7,7,9,8,8,8,9,10,10,10),4), heights=c(0.25,1), widths=c(0.9,1,0.1,0.16,0.6))
  par(mypar)
  par(mar=c(0,0,0,0))

  ## Title 1
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0,.5, labels="Ensemble régional", cex=cex_txt, col=colg5, pos=4)
  ## Plot 1
  emptyply(c(0.5,3.5),c(0,5))
  seqx <- c(2,1,3,2,1,3)
  seqy <- c(4,3,3,2,1,1)
  edges <- matrix(c(1,2,1,3,2,4,2,5,4,5,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=4, col=colg2)
  points(seqx, seqy, pch=21, col=colg2, bg=colg4, cex=12, lwd=4)
  text(seqx, seqy, labels=1:6, cex=cex_txt, col=colg2)
  ##

  ## Title 2
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(1,0.5, labels="Communautés locales", cex=cex_txt, pos=2)
  ## Plot 2-A
  par(mar=c(2,14,2,2))
  emptyply(c(0.5,3.5),c(0,5))
    bgcol(col=mypal[80])
  id_sp1 <- c(1,3,6)
  edges <- matrix(c(1,3,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg2, bg=colg4, cex=6, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=1.8, col=colg2)
  ## Plot 2-B
  emptyply(c(0.5,3.5),c(0,5))
  bgcol(col=mypal[40])
  id_sp1 <- c(1,2,3,4,6)
  edges <- matrix(c(1,2,1,3,2,4,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg2, bg=colg4, cex=6, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=1.8, col=colg2)
  ## Plot 2-C
  par(mar=c(2,8,2,2))
  emptyply(c(0.5,8),c(0,6))
  bgcol(col=mypal[10])
  id_sp1 <- c(1,2,4,5)
  edges <- matrix(c(1,2,2,4,2,5,4,5), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg2, bg=colg4, cex=6, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=1.8, col=colg2)
  sz_sg <- 0.4
  segments(x0=2-c(sz_sg,sz_sg),y0=2+1.6*c(sz_sg,-sz_sg),x1=2+c(sz_sg,sz_sg),y1=2-1.6*c(sz_sg,-sz_sg), lwd=3.2, col=colb3)
  #
  seqx <- 6.5+c(1,1,1)
  seqy <- c(1,3,5)
  edges <- matrix(c(1,2,2,3), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  arrows2(x0=2.8, y0=2.5, x1=6.6, cex.arr=2.8, lwd=1.6, col=colg5, border=NA, prophead=FALSE)
  points(seqx, seqy, pch=21, col=colg2, bg=colg4, cex=6, lwd=3)
  text(seqx, seqy, labels=c(5,2,1), cex=1.8, col=colg2)
  text(4.8, 3.8, "Extinction", cex=cex_txt)


  #### Gradient
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,0.5,labels="Gradient environnemental", srt=270, cex=cex_txt)
  ####
  par(mar=c(2,1,2,2), font=2)
  image(matrix(1:100, nrow=1), col=mypal, axes=FALSE, ann=FALSE)


  ## Title 3
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,.5, labels="Taux de colonisation", cex=cex_txt)
  #### Plot3
  par(mar=c(2,0,2,2), xaxs="i",yaxs="i")
  seqt <- seq(-10,10, by=0.1)
  plot0(c(0,0.5),range(seqt))
  box2(2:3, col2fill=NULL, col=colg2)
  moy <- -c(0,2,-3,4,7.5,-6)
  ect <- c(4,1.5,1.5,1.2,1.5,1.5)
  wei <- c(3.2,1.4,0.7,1,1.4,1.2)
  #
  for (i in 1:6) lines(wei[i]*dnorm(seqt,moy[i],ect[i]),seqt, lwd=3.2, col=mypal2[i])
  abline(v=0,h=10,lwd=4)
  seqx <- c(0.38,0.44,0.25,0.4, 0.44, 0.38)
  seqy <- moy
  points(seqx, seqy, pch=21, col=colg2, bg=colg4, cex=8, lwd=3)
  text(seqx, seqy, labels=1:6, cex=2.4, col=colg2)


  ####
  par(new=TRUE, fig=c(0,1,0,1))
  emptyply(c(0,1),c(0,1))
  arrows2(x0=0.32, y0=0.47, x1=0.48, lwd=4, col=colg5, border=NA)
  text(0.4, 0.54, labels="Colonisation", cex=cex_txt)

  ##
  dev.off()
}
figEcogr()


######
###### Perspectives

figperspec <- function(filename="./fig/fig26.png"){
  jpeg(file=filename, units="cm", res=300, width=1.25*wi, height=hg)
  ##
    par(mypar)
    par(mar=c(0,0,0,6), mfrow=c(1,2))
    ##
    seqx <- c(2,1,3,2,1,3)
    seqy <- c(4,3,3,2,1,1)
    edges <- matrix(c(1,2,1,3,2,4,2,5,4,5,3,6), ncol=2, byrow=TRUE)
    ##
    plot0(c(0.5,3.5),c(0,5))
    points(seqx, seqy, pch=21, col=colg2, bg=colg4, cex=12, lwd=6)
    text(seqx, seqy, labels=1:6, cex=3.2, col=colg2)
    ##
    par(mar=c(0,6,0,0))
    plot0(c(0.5,3.5),c(0,5))
    for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=8, col=colg3)
    points(seqx, seqy, pch=21, col=colg2, bg=colg4, cex=12, lwd=6)
    text(seqx, seqy, labels=1:6, cex=3.2, col=colg2)
    ##
    dev.off()
}

figperspec()





####### FIGURE 10 : Connectance*Gradient

load("/Users/kcazelles/Documents/Data/for_figures/res_gtib_fig5.Rdata")


figCG <- function(filename="./fig/fig10.png", ind=1:5){
  mypal3 <- mypal[floor(seq(10,95,len=5))]
  conc <- c(1,11,21,41,81)[ind]
  png(filename, height=hg, width=1.25*wi, unit="cm", res=300)
    layout(matrix(c(1,2,3,4,4,4),2,3, byrow=TRUE), heights=c(1,0.3))
    par(mypar)
    par(mar=c(5,4.5,6,0), mgp=c(2.2,1,0), cex.lab=2)
    Plab <- c("Compétition (-/-)", "Prédation (-/+)", "Mutualisme (+/+)")
    ## trois plots
    ord1 <- c(5,4,6)
    for (i in 1:3){
      plot0(c(0,30), c(0,10))
      k <- 0
      for (j in conc) {
        k <- k+1
        lines(res_gtib_fig5$envir, res_gtib_fig5[[3]][,1]+res_gtib_fig5[[ord1[i]]][,j], type="l", col=mypal3[k], lwd=3.2)
      }
      mtext(Plab[i], side=3, cex=2.2, adj=0, line=1)
      box(bty="l", lwd=3.4, col=colg2)
      if (i==1) title(ylab="Richesse spécifique", cex.lab=3)
      if (i==2) {
        title(xlab="Gradient environnemental", cex.lab=2.6)
        mtext(side=1, text="(ex: température)", line=5, cex=1.6)
      }
    }
    ## gradient
    par(mar=c(5,32,3,14), bty="n", mgp=c(3,1.4,0))
    image(matrix(1:5), ylab="", axes=FALSE, col=mypal3)
    axis(1,at=seq(0,1,length.out=5), labels=res_gtib_fig5$connect[c(1,11,21,41,81)], col=NA, col.ticks=1, cex.axis=2.8)
    mtext(side=2, "Connectance:", at=0, cex=2, las=1, line=1)
  dev.off()
}
figCG("./fig/fig10a.png", ind=NULL)
figCG("./fig/fig10b.png", ind=1)
figCG("./fig/fig10c.png", ind=1:2)
figCG("./fig/fig10d.png", ind=1:3)
figCG("./fig/fig10e.png", ind=1:4)
figCG("./fig/fig10f.png", ind=1:5)

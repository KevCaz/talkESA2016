#################################################
################################################
## Figures conntext
##
## first version: Apr 17th 2016
## last modification: Apr 18th 2016
## Kevin Cazelles
##
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin15.0.0 (64-bit)
## Running under: OS X 10.11 (El Capitan)
################################################
################################################

## Path
setwd("/Users/KevCaz/ownCloud/presESA")

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


#### Geodata
can <- readOGR(dsn="/Users/KevCaz/Documents/Data/Geodata/Pays_adm/CAN_adm/", layer="CAN_adm1")
gSimplify(can, tol=10)
quebec <- readOGR(dsn="/Users/KevCaz/Documents/Data/Geodata/quebec/", layer="quebec")
load("~/Documents/Data/Geodata/quebec/Presok.Rdata")

##
matweb1 <- matrix(c(0,1,0,0),2)
metaweb1 <- igraph::graph.adjacency(matweb1)
matweb2 <- matrix(c(0,1,0,0,0,1,0,0,0),3)
metaweb2 <- igraph::graph.adjacency(matweb2)
##
matweb3 <- matrix(rbinom(12*12,1,0.22),12,12)
diag(matweb3) <- 0
metaweb3 <- igraph::graph.adjacency(matweb3)





## --- FIGURE 1 : Quebec + 2 species
bbq <- bbox(quebec)
npt <- 14000
mypt <- SpatialPoints(coords=cbind(runif(npt,bbq[1,1],bbq[1,2]), runif(npt,bbq[2,1],bbq[2,2])), proj4string=quebec@proj4string)
##
spA <- rbinom(npt, 1, prob=dnorm(mypt@coords[,2], 48, 1))
spB <- rbinom(npt, 1, prob=dnorm(mypt@coords[,2], 52, 1.5))
##
# idinA <- which(!is.na(mypt%over%quebec) & spA==1)
main <-  SpatialPolygons(list(Polygons(list(quebec@polygons[[1]]@Polygons[1][[1]], quebec@polygons[[1]]@Polygons[32][[1]]),ID=1)), proj4string=quebec@proj4string)
idinA <- which(!is.na(mypt%over%main) & spA==1)

idinB <- which(!is.na(mypt%over%quebec) & spB==1)
##
fig1 <- function(filename="./fig/fig1.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
    # par(mfrow=c(1,2), mar=c(2,1,4,1), bg=1, fg=colg5, col.axis=colg5, col.lab=colg5, col.main=colg5, cex.main=3, cex.lab=2.6, font=2)
    par(mypar)
    par(mfrow=c(1,2), mar=c(2,1,2.5,1))
    ##
    plot(quebec, border=colg4, lwd=1.25)
    plot(mypt[idinA], add=TRUE, col=colb2, bg=colb1, pch=21, cex=0.5, lwd=0.5)
    mtext(side=3, adj=0, text="Species A", cex=2.4, line=0.8, col=colb1)
    ##
    plot(quebec, border=colg4, lwd=1.25)
    plot(mypt[idinB], add=TRUE, col=colg2, bg=colg5, pch=21, cex=0.5, lwd=0.5)
    mtext(side=3, adj=0, text="Species B",  cex=2.4, line=0.8, col=colg4)
  dev.off()
}
fig1()



## --- FIGURE 3 : Pédoclimatique explication!
fig3 <- function(filename="./fig/fig3.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
    par(mypar)
    par(mar=c(5.2,4,1,1), mgp=c(2,1,0))
    seqg <- seq(-10,10,0.01)
    plot0(range(seqg), c(0,.2))
    ##
    lines(seqg,0.7*dnorm(seqg,3.8,2.2), lwd=4, col=colb1)
    text(6,0.14, labels="Species A", col=colb1, cex=2.6)
    ##
    lines(seqg,dnorm(seqg,-2.6,2.4), lwd=4, col=colg5)
    text(-3,0.182, labels="Species B", col=colg5, cex=2.6, adj=1)
    ##
    title(xlab="Environmental Gradient", ylab="Occurrence probability")
    mtext(side=1, text="(ex: temperature)", line=3.8, col=colg3, cex=1.8)
    box(bty="l", lwd=3.4, col=colg3)
  dev.off()
}
fig3()




## ---  FIGURE 4 : Mouvement sur anti-costi
fig4 <- function(filename="./fig/fig4.png"){
  png(filename, height=wi, width=1.4*wi, unit="cm", res=300)
    par(mypar)
    par(mar=c(0,0,0,0), mgp=c(2,1,0))
    plot0(c(-72, -60), c(54,46), asp=1)
    plot(gSimplify(can[11,],0.001), border=colg4, col=NA, lwd=3, add=TRUE)
    plot(mypt[idinA], add=TRUE, col=colb1, bg=colb1, pch=21, cex=1.5, lwd=1.5)
    # box(lwd=4, col=colg2)
    par(new=TRUE, fig=c(0,0.3,0.6,1))
    plot(quebec, border=colg4, lwd=1.25)
    plot(mypt[idinA], add=TRUE, col=colb2, bg=colb1, pch=21, cex=0.5, lwd=0.5)
    mtext(side=3, adj=0, text="Species A", cex=2.4, line=0.8, col=colb1)
    box2(side=c(1,4),col2fill=NULL, col=colg3, lwd=2)
    ##
  dev.off()
}
fig4()



## ---  FIGURE 5 : SDM
# which(spB==B)
# polyA <- adehabitat::mcp(mypt[which(spA==1)])
# polyA <- adehabitat::mcp(mypt[which(spB==1)])
fig5 <- function(filename="./fig/fig5.png"){
  png(filename, height=hg, width=wi,  unit="cm", res=300)
    par(mypar)
    par(mfrow=c(1,2), mar=c(2,1,2.5,1))
    plot(quebec, border=NA)
    plot(gIntersection(gConvexHull(mypt[idinA]), quebec), add=TRUE, dens=12, angle=110, col=colb1, border=NA, lwd=1.4)
    plot(gIntersection(gConvexHull(mypt[idinB]), quebec), add=TRUE,  dens=12, col=colg5, border=NA, lwd=1.4)
    plot(quebec, border=colg4, lwd=1.8, add=TRUE)
    text(c(-68, -65.25),c(46.5, 53.7), labels=c("A","B"), col=c(colb1,colg5), cex=2.2)
    mtext(side=3, adj=0, text="Today", cex=2.4, line=0.8, col=colg5)
    ##
    plot(quebec, border=colg4, lwd=1.4)
    plot(Presok[[2]], add=TRUE, dens=12, angle=110, col=colb1, border=NA, lwd=1.4)
    plot(Presok[[4]], add=TRUE, dens=12, col=colg5, border=NA, lwd=1.4)
    mtext(side=3, adj=0, text="Tomorrow", cex=2.4, line=0.8, col=colg5)
    dev.off()
}
fig5()




## --- FIGURE 6b : LV

## Integration numérique
MyRK4 <- function(FUN, Xi, ti=0, tf=1, d_t=0.001, ...){
    ## Time sequence
    seqt <- seq(ti,tf,d_t)
    nt <- length(seqt)
    ## Values
    X <- matrix(0, ncol=length(Xi), nrow=nt)
    X[1,] <- Xi
    ## Loop
    for (i in 1:(nt-1)){
        k1 <- FUN(X[i,],...)
        k2 <- FUN(X[i,]+.5*d_t*k1,...)
        k3 <- FUN(X[i,]+.5*d_t*k2,...)
        k4 <- FUN(X[i,]+d_t*k3,...)
        X[i+1,] <- X[i,]+ d_t*(1/6)*(k1+2*k2+2*k3+k4)
    }
    return(data.frame(seqt,X))
}

## Système dynamique
LotkVolt <- function(X, alpha, beta){
    Y <- matrix(0,ncol=2)
    Y[1] <- alpha[1]*X[1] - beta[1]*X[1]*X[2]
    Y[2] <- beta[2]*X[2]*X[1] - alpha[2]*X[2]
    return(Y)
}


fig6b <- function(filename="./fig/fig6b.png"){
  png(filename, height=hg, width=1.2*wi, unit="cm", res=300)
    par(mypar)
    lwd.ln <- 4.2
    A = c(1,0.75)
    B = c(2,2)
    par(mar=c(4,4,1,1), mgp=c(2,1,0))
    plot0(c(0,50), c(0,1.1))
    LV1 <- MyRK4(ti=0, tf=50, d_t=0.01, FUN=LotkVolt, Xi=c(0.4,0.9), beta=B, alpha=A)
    lines(LV1[,1], LV1[,2], type="l", col=colb1, lwd=lwd.ln)
    lines(LV1[,1], LV1[,3], type="l", col=colg5, lwd=lwd.ln)
    legend("topright", legend=c("Species A", "Species B"), bty="n", cex=2.8, ncol=2, seg.len=2.6, lwd=lwd.ln, col=c(colb1, colg5))
    title(xlab="Time", ylab="Abundance", cex.lab=3.4)
    box(bty="l", lwd=3.4, col=colg3)
  dev.off()
}
fig6b()



seqx <- seq(0,10,0.01)
a <- -0.34
env1 <- 5+3*exp(a*seqx)
env2 <- 10-env1

env3 <- -(env1 + rev(env1))+5
env4 <- -(env2 + rev(env2))+5
## --- FIGURE 6 : Une vision éronnée ? A REFAIRE
fig6 <- function(filename="./fig/fig6.png", mode=FALSE){
  png(filename, height=hg, width=wi, units="cm", res=300)
    par(mypar)
    par(mar=c(4.5,1,1,1), mgp=c(2,0.8,0), xaxs="i")
    ##
    plot0(c(0,10), c(-10,10))
    envelop(seqx[1:700], env1[1:700], env2[1:700], col=colb1, border=NA)
    if (mode){
      lines(rep(seqx[700], 2), c(5,4), lwd=4, col=colb1)
      text(seqx[700], 3.4, labels="Denmark", pos=1,  col=colb1, cex=2.2)
    }

    ##
    envelop(seqx, env3, env4, col=colg5, border=NA)
    ##
    abline(h=c(-5,5), lty=2, lwd=1.8, col=colg3)
    axis(1, at=c(0,1,5,9,10), labels=c("","local", "regional", "continental", ""), lwd=0, cex.axis=2)
    axis(2, at=c(-10,10), labels=c("",""), lwd=0)
    box(bty="l", lwd=3.2, col=colg3)
    text(4, c(8,-8), labels=c("Ecological interactions","Climatic factors"), col=c(colb1, colg5), pos=4, cex=2.2)
    ##
    #title(ylab="Importance")
    par(mgp=c(3.4,0.8,0))
    title(xlab="Spatial scale")
    ##
  dev.off()
}
fig6()
fig6("./fig/fig6c.png", mode=TRUE)





## ---  Fig 7 : SDM et interaction

fig7 <- function(filename="./fig/fig7.png"){
  png(filename, height=hg, width=wi, unit="cm", res=300)
    layout(matrix(c(1,2),ncol=2), width=c(1,1))
    par(mypar)
    par(mfrow=c(1,2), mar=c(2,1,2.5,1))
    plot(metaweb3,
      vertex.frame.color=colb2,
      vertex.color=colg5,
      vertex.label.color=colb2,
      vertex.label.font=2,
      vertex.size=20,
      vertex.label=LETTERS[1:ncol(matweb3)],
      layout=layout.fruchterman.reingold,
      edge.arrow.size=0.9,
      edge.width=0.9,
      edge.color=colg5,
      vertex.label.cex=1.8
    )
    ##
    plot(quebec, border=colg4, lwd=1.4)
    plot(Presok[[2]], add=TRUE, dens=12, angle=110, col=colb1, border=NA, lwd=1.4)
    plot(Presok[[4]], add=TRUE, dens=12, col=colg5, border=NA, lwd=1.4)
    mtext(side=3, adj=0, text="Tomorrow", cex=2.4, line=0.8, col=colg5)
    text(coordinates(Presok[[1]])+c(0,2), labels="?", cex=18, col=colg4)
  dev.off()
}
fig7()

#################################################
################################################
## Figures Chapitre 2
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
setwd("/Users/KevCaz/ownCloud/presESA")

## Packages
mapspacks()
library(igraph)
library(mgcv)
library(plotrix)

## Colors
colb1="#50b5d5"
colb2="#1781a2"
colb3="#042d4b"
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

##
bgcol <- function(col=colg5, ...) rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col=col, border=NA, ...)







### FIGURE C-score
nsite <- 144
presA <- rbinom(nsite,1,0.32)
presB <- rbinom(nsite,1,0.32)*2
prest2 <- prest <- presA+presB
prest2[prest2==3] <- rbinom(nsite,1,.5)+1
mypalcb <- c(1, colb1, colg5, colb3)
txt <- c("", "A", "B", "")

figCB <- function(filename1="./fig/fig11.png", filename2="./fig/fig12.png", filename3="./fig/fig13.png"){
  png(filename1, height=hg, width=hg, unit="cm", res=300)
    par(mypar)
    par(mfrow=c(12,12), mar=c(0.1,0.1,0.1,0.1))
    for (i in 1:nsite){
      plot0()
      bgcol(col=mypalcb[1+prest[i]])
    }
  dev.off()
  ##
  png(filename2, height=hg, width=hg, unit="cm", res=300)
    par(mypar)
    par(mfrow=c(12,12), mar=c(0.1,0.1,0.1,0.1))
    for (i in 1:nsite){
      plot0()
      bgcol(mypalcb[1+prest2[i]])
      text(0,0, txt[1+prest2[i]], col=1, cex=3.6)
    }
  dev.off()
  ##
  # png(filename3, height=hg, width=hg, unit="cm", res=300)
  #   par(mypar)
  #   par(mfrow=c(11,11), mar=c(0.1,0.1,0.1,0.1))
  #   j <- 0
  #   for (i in 1:nsite){
  #     j<- j+1
  #     plot0()
  #     bgcol(col=mypalcb[2+(j%%2)])
  #   }
  # dev.off()
}
figCB()



### FIGURE : Shortest path
figshtpth <- function(filename="./fig/fig14a.png", addtable=FALSE){
  png(filename, res=300, height=hg, width=wi, unit="cm")
    par(mypar)
    par(mar=c(0,0,0,0))
    plot0(c(0.75,5.25),c(0.75,4.25))
    text(rep(c(1,2,3),2),rep(c(4,1),3), labels=c("A","D"), col=colg5, cex=3.4, font=1)
    text(c(2,3,3),c(2.5,2,3), labels=c("B","B","C"), col=colb1, cex=3.4, font=1)
    vcx0 <- rep(1:3,1:3)
    vcy0 <- c(1.25,1.25, 2.75, 1.25, 2.25, 3.25)
    vcy1 <- c(3.75,2.25, 3.75, 1.75, 2.75, 3.75)#,3.75,3.75)
    for (i in 1:6) arrows2(x0=vcx0[i], y0=vcy0[i], y1=vcy1[i], col=colg5, cex.arr=0.28, cex.hl=0.8, cex.hh=1.8)
    ##
    if (addtable){
      mat <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0),4,4)
      table1 <- data.frame(mat, row.names=LETTERS[1:4])
      names(table1) <- LETTERS[1:4]
      addtable2plot(4,2.75,table1,bty="n",display.rownames=TRUE, cex=2.6)
      if (addtable==2){
        mat2 <- matrix(c(0,1,2,3, 1,0,1,2, 2,1,0,1, 3,2,1,0),4,4)
        table2 <- data.frame(mat2, row.names=LETTERS[1:4])
        names(table2) <- LETTERS[1:4]
        addtable2plot(4,1,table2,bty="n",display.rownames=TRUE, cex=2.6)
      }
    }
  dev.off()
}
figshtpth(filename="./fig/fig14a.png", addtable=FALSE)
figshtpth(filename="./fig/fig14b.png", addtable=TRUE)
figshtpth(filename="./fig/fig14c.png", addtable=2)



### FIGURE Simple Network
## ----
## Pour le plot 2
# moycd<- read.table("/Users/kcazelles/Documents/Codes/C_C++_WS/LVICETF092014/dataMS/moycd", header=TRUE)
# sdcd<- read.table("/Users/kcazelles/Documents/Codes/C_C++_WS/LVICETF092014/dataMS/sdcd", header=TRUE)
# moysp <- read.table("/Users/kcazelles/Documents/Codes/C_C++_WS/LVICETF092014//dataMS/moysp", header=TRUE)
# sdsp <- read.table("/Users/kcazelles/Documents/Codes/C_C++_WS/LVICETF092014//dataMS/sdsp", header=TRUE)
#
#
# panelBC <- list(moycd=moycd, sdcd=sdcd, moysp=moysp, sdsp=sdsp )

load("/Users/KevCaz/Documents/Data/for_figures/res_theo_cooc.Rdata")

figorder <-  function(filename="./fig/fig15.png"){
  ##
  pal2 <- c(colb1, colb2, colb3)
  png(filename, res=300, height=hg, width=wi, unit="cm")
  ##
    layout(matrix(1:2,ncol=2),widths=c(1,0.15))
    par(mypar)
    par(mar=c(6.5,6.5,2,2), mgp=c(4,1,0), las=1)
    ##
    plot0(c(0,0.12),c(0,0.12))
    title(ylab= "Observed co-occurrence", xlab="Independent co-occurrence", cex.lab=2.2, col=colg3)
    #expression(P["i"]*P["j"]), ylab=expression(P["i,j"]), cex.lab=2.6, col=colg2)
    for (i in 1:3) plot(res_theo_cooc[[1]][[i]], col=pal2[i], border=1, add=TRUE)
    abline(a=0, b=1, lwd=3, lty=2, col=colg2)
    axis(1, cex.axis=1.4)
    axis(2, cex.axis=1.4)
    box(bty="l", lwd=2, col=colg3)
    legend("bottomright",paste0("order ",1:3), pch=15, col=pal2[1:3], bty="n", cex=2.4)
    ##
    ##
    par(mar=c(4,0,1,0))
    plot0(c(0.75,1.25),c(0.75,4.25))
    text(rep(1,4),1:4, LETTERS[4:1],col=colg5, cex=3.4, font=1)
    vcx0 <- rep(1,3)
    vcy0 <- c(1.25, 2.25, 3.25)
    vcy1 <- c(1.75, 2.75, 3.75)#,3.75,3.75)
    for (i in 1:3) arrows2(x0=vcx0[i], y0=vcy0[i], y1=vcy1[i], col=colg5, cex.arr=0.28, cex.hl=0.8, cex.hh=1.8)
    dev.off()
}
figorder()




myRect <- function(col=colg2) rect(par()$usr[1],-1.96,par()$usr[2],1.96, col=col, border=NA)



### FIGURE Re-explaining
figthatis <- function(filename="./fig/fig17.png", textbw="Shortest path", txt=1:3){
  myboxplot <- function(x,at,col) boxplot(x, add=TRUE, at=at, axes=FALSE, ann=FALSE, outline=FALSE, col=col, lwd=1.6)
  ngp <- 3
  nmt <- 2
  seqa <- seq(0.5,by=nmt*0.75,length=ngp)
  seqb <- seq(1,by=nmt*0.75,length=ngp)
  ##
  rmy <- c(5,2.7,0.9)
  rmy2 <- c(2,1.4,0.25)
  rsd <- c(1.8,1.2,1)
  rsd2 <- c(1.2,.7,0.6)
  ##
  jpeg(filename=filename, unit="cm", width=1*wi, height=1*hg, res=300)
    par(mypar)
    par(mar=c(6,5,3,2), mgp=c(2,1,0), las=1)
    plot0(c(0,nmt*0.75*ngp-0.5)+c(0,.2), c(-4,10))
    myRect()
    abline(h=0, lty=34, lwd=3.2, col=colg3)
    for (i in 1:ngp){
      val1 <- rnorm(1000, rmy[i], rsd[i])
      val2 <- rnorm(1000, rmy2[i], rsd2[i])
      myboxplot(val1, at=mean(c(seqa[i],seqb[i]))-.25, col=colg2)
      myboxplot(val2, at=mean(c(seqa[i],seqb[i]))+.25, col=colb1)
      lines(c(seqa[i],seqb[i])+0.25*c(-1,1), -4.5*c(1,1), lwd=2)
      mtext(paste0(" ",txt[i]), side=1, at=mean(c(seqa[i],seqb[i])), line=1.4, cex=2.6)
      mtext(textbw, side=1, line=3.6, cex=2.4)
      mtext(side=2, at=0, text="0", las=1, cex=2, font=1, line=0.34)
      legend('topright', legend=c('Homogenous', 'SDM'), fill=c(colg2, colb1), bty='n', cex=2)
    }
    mtext(3, at=-.25, text='Z-score', cex=2.6)
    ##
  box2(2, lwd=4, col2fill=NULL, col=colg3)
  dev.off()
}
##
figthatis()
figthatis("./fig/fig18.png",  textbw="Interactions", txt=c('-','+','++'))

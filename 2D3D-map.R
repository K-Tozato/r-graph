setwd("~/GitLab/stats/r-graph/")

library(rgl)

##################
## Example data ## -----------------
##################
xx <- 1:nrow(volcano)*10
yy <- 1:ncol(volcano)*10
zz <- volcano
##--------------------------------

######################
## Set individually ##------------
######################
outfile.2d <- "./img/2Dmap.png"
outfile.3d <- "./img/3Dmap.png"
outfile.lab <- "./img/lab_map.png"

clr.c <- rainbow(24)[1:20] #color for contour
ev <- zz ## values for contour

## axis label##
xlab <- ""
ylab <- ""
zlab <- "Elevation [m]"
##--------------------------------

## Contour ##
clr <- clr.c
mx <- max(ev) ; mn <- min(ev)
if(length(clr)>1){
  clr <- matrix(0,nrow(ev),ncol(ev))
  a <- length(clr.c)
  for(i in 1:nrow(ev)){
    ev1 <- ev[i,]
    for (k in 1:(a-1)){
      nn <- 1:length(ev1)
      nn <- nn[ev1 < mn+k*(mx-mn)/a]
      nn <- nn[ev1[nn] >= mn+(k-1)*(mx-mn)/a]
      clr[i,nn] <- clr.c[a-k+1]
    }
    nn <- 1:length(ev1)
    nn <- nn[ev1 >=mx-(mx-mn)/a]
    clr[i,nn] <- clr.c[1]
  }
}

dxyz <- c(max(xx)-min(xx),max(yy)-min(yy),max(zz)-min(zz))


#############
## 2D maps ##------------
#############
## plot functions ##
f.plot <- function(ev,xx,yy){
  con <- as.vector(ev)
  coord <- matrix(0,length(xx)*length(yy),2)
  coord[,1] <- rep(xx,length(yy))
  coord[,2] <- as.vector(t(matrix(rep(yy,length(xx)),length(yy),length(xx))))
  
  xl <- range(coord[,1]) ; yl <- range(coord[,2])
  
  #label
  plt.size <- 0.21*sqrt(nrow(coord))
  txt.size <- 2.5
  
  par(family="serif")
  plot(coord[,1],coord[,2],col=as.vector(clr),pch=15,cex=plt.size, main="", cex.main=1.7*txt.size,
       xlim=c(xl[1],xl[2]),ylim=c(yl[1],yl[2]),xlab=xlab,ylab=xlab,xaxt="n",yaxt="n")
}

if(length(clr.c)>1){
  ratio <- length(yy)/length(xx)
  png(outfile.2d,width = 1000, height = 1000*ratio)
  f.plot(ev,xx,yy)
  dev.off()
}
##-----------------------

#############
## 3D maps ##------------
#############
rgl::open3d()
rgl::persp3d(xx,yy,zz,col=clr,
             xlab=xlab, ylab=ylab, zlab=zlab)#, axes = F)
rgl::aspect3d(dxyz[1],dxyz[2],dxyz[3])

if(file.exists("./pv.txt")==T){
  load("./pv.txt")
  par3d(windowRect=pv$windowRect)  
  par3d(zoom=pv$zoom)              
  par3d(userMatrix=pv$userMatrix)  
}
axes3d()

rgl.snapshot(outfile.3d, fmt="png")
##----------------------


###############
## Color bar ##---------------------------
###############
f.label <- function(outfile.lab,zlab){
  png(outfile.lab,width=460,height=1400)
  txt.size <- 4.5
  COL <- clr.c
  a <- length(clr.c)
  par(family="serif")
  plot("",xlim=c(0,46),ylim=c(0,130), axes=F, xlab="",ylab="")
  aa <- 0
  dd <- 120
  rect(0, seq(dd,aa+(dd-aa)/a,length=a),
       10,seq(dd-(dd-aa)/a,aa,length=a),col=COL[1:a])
  b <- 5
  bb <- c()
  for (i in 1:b){
    bb[i] <- round((i-1)*(mx-(mn))/(b-1)+(mn),digits=2)
  }
  for (i in 1:b){
    text(20,aa+(i-1)*(dd-aa)/(b-1),bb[i],cex=txt.size)
  }
  text(18,130,zlab,cex=txt.size)
  dev.off()
}

if(length(clr.c)>1){
  f.label(outfile.lab, zlab)
}
##----------------------------------------


## If make pv.txt, run below 2 lines after adjusting the angle and magnification ##---------
#pv<-par3d()
#save(pv, file="./pv.txt", ascii=T)
##------------------------------------------------------------------------------------------

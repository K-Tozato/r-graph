setwd("~/R_works/R_graph")

##################
## data example ##----------------------
##################
ndata <- 3
x1 <- seq(0,10,0.5)
x3 <- x1 
x2 <- x1
y1 <- c() ; y2 <- c() ; y3 <- c()
for(i in 1:length(x1)){
  y1[i] <- x1[i]^2 - 2*x1[i] + 4
  y2[i] <- 0.3*x1[i]^2 + 3*x1[i] + 5
  y3[i] <- 2*x1[i] + 15
}
##---------------------------------------


##############################
## Plot parameters (defalt) ##----------------------
##############################
type <- rep("l", ndata) # p, b or l ## Plot type 
lty <- rep(1,ndata) ## Line type
lwd <- rep(3,ndata) ## Line width
pch <- rep(NA,ndata) ## Plot shape
clr <- 1:ndata ## Colors
cex <- rep(2.5,ndata) ## Plot size
font <- "serif" # serif, sans or mono
cex.lab <- 3.0 ## size of labels
cex.axis <- 2.8 ## size of axes

f.grid <- T # T or F

cex.leg <- 3

logxy <- "" # log axis ("x", "y" or "xy")

xlim <- range(x1) ; ylim <- range(y1) ## plot range
for(i in 1:ndata){
  eval(parse(text=paste("xx <- x",i,sep="")))
  eval(parse(text=paste("yy <- y",i,sep="")))
  xlim <- range(xlim, xx)
  ylim <- range(ylim, yy)
}
##----------------------------

######################
## Set individually ## -------------------
######################

fname <- "./figure/graph.png" ## figure name
p.ratio <- 1.0 ## plot ratio

f.legend <- T # T or F
legend <- c("Value A", "Value B", "Value C") # legend 
p.leg <- "topleft" #Position for legend

xlab <- "X label" # label for x axis
ylab <- "Value A & B" # label for y axis

axis2 <- F
if(axis2){
  naxis <- c(1,1,2)
  ylab2 <- "Value C"
  
  ylim <- c() ; ylim2 <- c()  ## plot range
  for(i in 1:ndata){
    eval(parse(text=paste("yy <- y",i,sep="")))
    if(naxis[i]==1){ylim <- range(ylim, yy)}
    if(naxis[i]==2){ylim2 <- range(ylim2, yy)}
  }
}

# If changing plot parameters, write here. 
type <- c("l", "l", "b") # p, b or l ## Plot type 
pch <- c(NA,NA,16) ## Plot shape
##------------------------------------------

##########
## Plot ##-----------------------------------
##########
png(fname, width = 1000, height = p.ratio*1000)
par(mar = c(6,6,3,3), mgp = c(4,1.5,0))
par(family=font)

plot("", "", xlim= xlim, ylim = ylim, xlab = xlab, ylab = ylab,
     cex = cex[i], cex.lab = cex.lab, cex.axis = cex.axis, log=logxy)
if(f.grid){grid()}
for(i in 1:ndata){
  eval(parse(text=paste("xx <- x",i,sep="")))
  eval(parse(text=paste("yy <- y",i,sep="")))
  
  par(new=T)
  plot(xx, yy, xlim= xlim, ylim = ylim, 
       xlab = "", ylab = "", 
       type=type[i], lty=lty[i], lwd=lwd[i], pch=pch[i], col=clr[i],
       cex = cex[i], xaxt="n", yaxt="n",
       log=logxy)
}
if(f.legend){
  legend(p.leg,legend = legend, lty=lty, lwd=lwd, pch=pch, col=clr, cex=cex.leg)
}
dev.off()
##-----------------------------------------

##################
## 2-axis graph ##--------------------------
##################
if(axis2){
  png(fname, width = 1000, height = p.ratio*1000)
  par(mar = c(6,6,3,6), mgp = c(4,1.5,0))
  par(family=font)
  
  plot("", "", xlim= xlim, ylim = ylim, xlab = xlab, ylab = ylab,
       cex = cex[i], cex.lab = cex.lab, cex.axis = cex.axis, log=logxy)
  if(f.grid){grid()}
  for(i in 1:ndata){
    eval(parse(text=paste("xx <- x",i,sep="")))
    eval(parse(text=paste("yy <- y",i,sep="")))
    if(naxis[i]==1){
      par(new=T)
      plot(xx, yy, xlim= xlim, ylim = ylim, 
           xlab = "", ylab = "", 
           type=type[i], lty=lty[i], lwd=lwd[i], pch=pch[i], col=clr[i],
           cex = cex[i], axes=F,
           log=logxy)
    }
    if(naxis[i]==2){
      par(new=T)
      plot(xx, yy, xlim= xlim, ylim = ylim2, 
           xlab = "", ylab = "", 
           type=type[i], lty=lty[i], lwd=lwd[i], pch=pch[i], col=clr[i],
           cex = cex[i], axes=F,
           log=logxy)
    }
  }
  mtext(ylab2,side = 4, line = 4, cex = cex.lab)  # right y label
  axis(4, cex.axis=cex.axis)
  
  if(f.legend){
    legend(p.leg,legend = legend, lty=lty, lwd=lwd, pch=pch, col=clr, cex=cex.leg)
  }
  dev.off()
}
##------------------------------------------


#setwd("~/R_works/R_graph")

##################
## data example ##----------------------
##################
ndata1 <- 2
ndata2 <- 2

x2 <- x1 <- 0:10
da <- matrix(0,2,length(x1))
da[1,] <- c(1,2,5,6,8,8,10,7,5,4,3)
da[2,] <- c(3,3,6,4,2,4,10,12,8,6,5)  

y1 <- c() ; y2 <- c()
for(i in 1:length(x1)){
  y1[i] <- sum(da[1,1:i])
  y2[i] <- sum(da[2,1:i])
}
##---------------------------------------


##############################
## Plot parameters (defalt) ##----------------------
##############################
## parameters for lines
type <- rep("l", ndata1) # p, b or l ## Plot type 
lty <- rep(1,ndata1) ## Line type
lwd <- rep(3,ndata1) ## Line width
pch <- rep(NA,ndata1) ## Plot shape
clr1 <- 1:ndata1 ## Colors
cex <- rep(2.5,ndata1) ## Plot size

ylim1 <- range(y1) ## plot range
for(i in 1:ndata1){
  eval(parse(text=paste("yy <- y",i,sep="")))
  ylim1 <- range(ylim1, yy)
}

## parameter for bars
clr2 <- 1:nrow(da)
ylim2 <- range(range(da*1.1),0)


font <- "serif" # serif, sans or mono
cex.lab <- 3.0 ## size of labels
cex.axis <- 2.8 ## size of axes

f.grid <- T # T or F
f.box <- T # T or F

cex.leg <- 2.5

logxy <- "" # log axis ("x", "y" or "xy")
##----------------------------

######################
## Set individually ## -------------------
######################

fname <- "./figure/line-bar.png" ## figure name
p.ratio <- 0.75 ## plot ratio

names.arg <- x1 # labels of x axis

f.legend <- T # T or F
legend1 <- c("Cumulative value for A", "Cumulative value for B") # legend (line)
legend2 <- c("Value A", "Value B") # legend (bar)

p.leg <- "topleft" #Position for legend

xlab <- "X label" # label for x axis
ylab1 <- "Cumulative value for A & B"
ylab2 <- "Value A & B" # label for y axis


# If changing plot parameters, write here. 
type <- c("b","b")
pch <- c(16,16)
clr2 <- c("grey", "pink")
##------------------------------------------


##########
## Plot ##-----------------------------------
##########
png(fname, width = 1200, height = p.ratio*1000)
par(mar = c(6,6,3,6), mgp = c(4,1.5,0))
par(family=font)


dx <- x1[2]-x1[1]
barx <- barplot(
  da, beside=T, col=clr2, space=c(0.10*dx, 0.20*dx), width = dx*(0.8/ndata1), 
  ylim = ylim2, names.arg=names.arg, xlab=xlab, ylab=ylab2,
  cex.axis = cex.axis, cex.names = cex.lab, cex.lab=cex.lab)
par(new=T)

xlim <- range(barx)

for(i in 1:ndata1){
  eval(parse(text=paste("yy <- y",i,sep="")))
  
  par(new=T)
  plot(colMeans(barx), yy, xlim= xlim, ylim = ylim1, 
       xlab = "", ylab = "", 
       type=type[i], lty=lty[i], lwd=lwd[i], pch=pch[i], col=clr1[i],
       cex = cex[i], axes=F,
       log=logxy)
  par(new=T)
}
mtext(ylab1,side = 4, line = 4, cex = cex.lab)  # right y label
axis(4, cex.axis=cex.axis)


if(f.grid){grid()}
if(f.box){box()}
if(f.legend){
  legend(p.leg,legend = c(legend1,legend2), lty=c(lty,rep(0,ndata2)), 
         lwd=c(lwd,rep(0,ndata2)), pch=c(pch,15), col=c(clr1,clr2), cex=cex.leg)
}

dev.off()
##-----------------------------------------

setwd("~/R_works/R_graph")

##################
## data example ##----------------------
##################
ndata <- 3
x1 <- seq(0,10,1)
x3 <- 10-x1 
x2 <- x1*0.5
da <- t(matrix(c(x1,x2,x3),length(x1),ndata))
##---------------------------------------

##############################
## Plot parameters (defalt) ##----------------------
##############################
clr <- 1:nrow(da)
ylim <- c(min(da),max(da*1.1))

font <- "serif" # serif, sans or mono
cex.lab <- 3.0 ## size of labels
cex.axis <- 2.8 ## size of axes

f.grid <- T # T or F
f.box <- T # T or F

cex.leg <- 3

logxy <- "" # log axis ("x", "y" or "xy")
##--------------------------------------------------

######################
## Set individually ## -------------------
######################
fname <- "./figure/barplot.png" ## figure name
p.ratio <- 0.75 ## plot ratio

names.arg <- 0:10 # labels of x axis

f.legend <- T # T or F
legend <- c("Value A", "Value B", "Value C") # legend (if f.legend==T)
p.leg <- "top" #Position for legend (if f.legend==T)

xlab <- "X label" # label for x axis
ylab <- "Y label" # label for y axis

# If changing plot parameters, write here. 
clr <- c("blue", "red", "yellow")
##------------------------------------------

##########
## Plot ##-----------------------------------
##########
png(fname, width = 1200, height = p.ratio*1000)
par(mar = c(6,6,3,3), mgp = c(4,1.5,0))
par(family=font)

par(family=font)
barplot(da,beside = T, col=clr, ylim=ylim, log=logxy, 
        names.arg = names.arg, cex.sub= cex.leg,
        cex.axis = cex.axis, cex.names = cex.lab, 
        xlab = xlab, ylab = ylab, cex.lab=cex.lab)
if(f.grid){grid()}
if(f.box){box()}
if(f.legend){
  legend(p.leg,legend = legend, pch=15, col=clr, cex=cex.leg)
}

dev.off()
##-----------------------------------------














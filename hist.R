#setwd("~/R_works/R_graph")

##################
## data example ##----------------------
##################
x1 <- rnorm(10000,0,1)
x2 <- rnorm(10000,1,3)

ndata <- 2
##---------------------------------------

##############################
## Plot parameters (defalt) ##----------------------
##############################
font <- "serif" # serif, sans or mono
cex.lab <- 3.0 ## size of labels
cex.axis <- 2.8 ## size of axes

f.grid <- T # T or F
f.box <- T # T or F

cex.leg <- 3
nbreak <- 100

freq <- T
if(freq==T){ylab <- "Frequancy"} # label for y axis
if(freq==F){ylab <- "Density"} # label for y axis
##--------------------------------------------------

######################
## Set individually ## -------------------
######################
fname <- "./figure/histgram.png" ## figure name
p.ratio <- 0.75 ## plot ratio

f.legend <- T # T or F
legend <- c("Value A", "Value B") # legend (if f.legend==T)
p.leg <- "topright" #Position for legend (if f.legend==T)

xlab <- "X label" # label for x axis

# If changing plot parameters, write here. 
clr <- c("grey", "pink")
border <- c("black", "red")
##------------------------------------------

##########
## Plot ##-----------------------------------
##########
colors.hex <- function( x=colors() ) { 
  color.hex <- function(x) do.call( "rgb", as.list(col2rgb(x)/255) ) 
  sapply( x, color.hex ) 
}

clr2 <- c()
for(i in 1:ndata){clr2[i] <- paste(colors.hex(clr[i]),50,sep="")}
xlim <- c()
for(i in 1:ndata){
  eval(parse(text=paste("xx <- x",i,sep="")))
  xlim <- range(xlim,xx)
}
ylim <- c()
for(i in 1:ndata){
  eval(parse(text=paste("xx <- x",i,sep="")))
  a <- hist(xx, breaks = seq(xlim[1],xlim[2],length=100),freq)
  if(freq==T){ylim <- range(ylim,a$counts)}
  if(freq==F){ylim <- range(ylim,a$density)}
}

png(fname, width = 1200, height = p.ratio*1000)
par(mar = c(6,6,3,3), mgp = c(4,1.5,0))
par(family=font)

for(i in 1:ndata){
  eval(parse(text=paste("xx <- x",i,sep="")))
  add <- T
  if(i==1){add <- F}
  hist(xx, col=clr2[i], breaks = seq(xlim[1],xlim[2],length=100),
       border = border[i], xlim=xlim, ylim=ylim, 
       cex.axis = cex.axis, cex.lab = cex.lab, 
       xlab = xlab, ylab = ylab, main = "", freq = freq, add=add)
}
if(f.grid){grid()}
if(f.box){box()}
if(f.legend){legend(p.leg,legend = legend, pch=15, col=clr, cex=cex.leg)}

dev.off()
##-----------------------------------------


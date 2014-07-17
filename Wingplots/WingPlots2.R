
setwd("/Users/Will/Documents/=WORK=/Wingplots")
mean.shape <- read.csv("Combined_Mean_Shape.csv")
segment <- read.csv("wing_segment.csv")

#setwd("/Users/Will/Documents/=WORK=/DGRP/Assoc_Analyses")

wings.LM <- read.csv(file="Spline_output_30Apr.csv", nrows=14693, header=TRUE)
wings.LM <- read.csv("/Users/Will/Documents/=WORK=/Maine_NC_lines/Maine&NC_output.csv", header=TRUE, nrows=6650)

PrcCrds <- wings.LM[,4:99]

WingPlot <- function( PrcCrds, no.LMs, colour="blue", lines=TRUE ){
	
	if(no.LMs==15){
		odds <- seq(1, 29, 2)
		evens <- seq(2, 30, 2)
		} else {
			odds <- seq(1, 95, 2)
			evens <- seq(2, 96, 2)
			}

  LMx <- PrcCrds[,c(odds)]
  LMy <- PrcCrds[,c(evens)]
  LMx.mean <- colMeans( LMx, na.rm=TRUE)
  LMy.mean <- colMeans( LMy, na.rm=TRUE)
  
plot( colMeans(LMx), colMeans(LMy), xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.3, type="n", xaxt="n", yaxt="n", ann=FALSE)

points( LMx.mean, LMy.mean, cex=0.75, pch=1)

	if(no.LMs==15){
		from <- c(1,12,12,14,14,11,11,9,9,9,6,13,6,6,7,11,3,4,14)
		to <- c(7,7,13,13,15,15,10,10,8,3,8,8,12,2,2,5,5,5,10)
		} else {
			from <- segment$from
			to <- segment$to
			}
			
			if(lines==TRUE){
				for (i in 1:length(from)) {
    	lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=colour, lty=1, lwd=1 )
    	}	} else {
    		text( -0.20, -0.20, "proximal")
    		text( 0.22, -0.20, "distal")}
	}

WingPlot(PrcCrds, 48, colour="dark green")

for (j in 1:length(levels(wings$Line))) {
	WingPlot(PrcCrds[j,], 48, colour="blue")
	}


####################
require(animation)
mean.shape <- read.csv("DGRP_mean_shape.csv")
LMx.mean <- mean.shape$LMx.mean
LMy.mean <- mean.shape$LMy.mean

par(mar=c(1,1,1,1))
plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.5, xaxt="n", yaxt="n", ann=FALSE)

chrs2L_1 <- read.csv("/Users/Will/Documents/=WORK=/DGRP/Association_Analysis_Output/2Lchunk1_spline_assoc.csv", header=TRUE, nrow=116410)

names(chrs2L_1)
names(chrs2R)

loadings <- chrs2R[,10:105]
loadings <- crap[,10:105]


comment(wingmovie) <- "the function requires 'loadings', which should be a 88 column dataframe (eg: loadings <- chrsX1[,10:97] ), comprising the 44x&44y coordinates part of the output from CPReaderer. 'SNP' is provided as a column index."

wingmovie <- function ( SNP, meanline=FALSE ) {
	
	LMx.mean <- mean.shape$LMx.mean
	LMy.mean <- mean.shape$LMy.mean
	
	x.vals <- seq(1, 95, 2)		#these pull out the odd...
	y.vals <- seq(2, 96, 2)		#...and even columns...
	x.loadings <- loadings[,c(x.vals)]		#...to become the X...
	y.loadings <- loadings[,c(y.vals)]		#...and Y coordinates

multiplier <- c(seq(-2, 2, 0.2), seq(2, -2, -0.2))
#multiplier is just a sequence from -2 to 2 and back to scale the frames

	x.plus <- (LMx.mean + x.loadings[SNP,])  #vector of X mean + loadings
	x.minus <- (LMx.mean - x.loadings[SNP,])  #vector of X mean - loadings
	y.plus <- (LMy.mean + y.loadings[SNP,])  #vector of Y mean + loadings
	y.minus <- (LMy.mean - y.loadings[SNP,])  #vector of Y mean - loadings
	
	#'from' and 'to' are paired vectors - the wing outline is made up of lines that are plotted FROM from[n] TO to[n]...
		from <- segment$from
		to <- segment$to

	par(mar=c(1, 1, 1, 1))	#margins not needed as we have no labels
	
for (j in 1:length(multiplier)) {	 #i.e. for each frame of the animation
	plotX <- as.numeric(LMx.mean + (x.loadings[SNP,] * multiplier[j]))
	plotY <- as.numeric(LMy.mean + (y.loadings[SNP,] * multiplier[j]))
	#plotX&Y are vectors of mean coords + (loadings * scale)

	plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=1, col="black", xaxt="n", yaxt="n", ann=FALSE) 	#sets up the plot and adds mean points
	#text( 0, 0.25, "brk")		#labels the plot with 'SNP'
	
		for (i in 1:length(from)) {
			if(meanline==TRUE){
		lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col="blue", lty=1, lwd=3 )
					}
    	lines( c(plotX[from[i]], plotX[to[i]]), c(plotY[from[i]], plotY[to[i]]), col="red", lty=1, lwd=3 )		#draws the outline segments for each 'from-to' pair
    	
#    	for (j in 1:length(LMx.mean)) {
#	lines( c(x.plus[j], x.minus[j]), c(y.plus[j], y.minus[j]), col="blue", lwd=2)
#	}		#this loop draws in the vectors of change across the mean for each LM/sLM
    	}
	}
}


wingmovie(30099, meanline=TRUE)
wingmovie(3, meanline=TRUE)
wingmovie(85, meanline=TRUE)

saveGIF(expr=wingmovie(85, meanline=TRUE), interval=0.1, "test!.gif")



################


plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.5, type="n", xaxt="n", yaxt="n", ann=FALSE)

#bob <- 46064    #smallest p
#bob <- 66021    #largest proc dist

points( (LMx.mean + x.loadings[bob,]), (LMy.mean + y.loadings[bob,]), cex=0.4, col="black")
points( (LMx.mean - x.loadings[bob,]), (LMy.mean - y.loadings[bob,]), cex=0.4, col="black")


for (i in 1:length(LMx.mean)) {
	lines( c(x.plus[i], x.minus[i]), c(y.plus[i], y.minus[i]), col="red")
	}

plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.5, type="n", xaxt="n", yaxt="n", ann=FALSE)

for (i in 1:length(multiplier)) {
	points( (LMx.mean + (x.loadings[bob,] * multiplier[i])), (LMy.mean + (y.loadings[bob,] * multiplier[i])), cex=0.4, col="black")
	}

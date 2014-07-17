###Manhattan plots for EDG presentation

chrsX <- read.csv("/Users/Will/Desktop/chrsX_Spline_assoc.csv", header=TRUE, nrow=331851)

names(chrsX)
logP <- -log10(chrsX$p.value)
plot( chrsX$SNP.position, logP, xaxt="n", xlab="SNP location", main="X Chromosome", ylab="-ve log P value")
lines(c(0,max(chrsX$SNP.position)), c(6,6), lty=2, col="red")

chrs2L <- read.csv("/Users/Will/Desktop/chrs2L_Spline_assoc.csv", header=TRUE, nrow=582025)
logP <- -log10(chrs2L$p.value)
plot( chrs2L$SNP.position, logP, xaxt="n", xlab="SNP location", main="Chromosome 2L", ylab="-ve log P value")
lines(c(0,max(chrs2L$SNP.position)), c(6,6), lty=2, col="red")

chrs2R <- read.csv("/Users/Will/Desktop/chrs2R_Spline_assoc.csv", header=TRUE, nrow=582025)
logP <- -log10(chrs2R$p.value)
plot( chrs2R$SNP.position, logP, xaxt="n", xlab="SNP location", main="Chromosome 2R", ylab="-ve log P value")
lines(c(0,max(chrs2R$SNP.position)), c(6,6), lty=2, col="red")

chrs3L <- read.csv("/Users/Will/Desktop/chrs3L_Spline_assoc.csv", header=TRUE, nrow=582025)
logP <- -log10(chrs3L$p.value)
plot( chrs3L$SNP.position, logP, xaxt="n", xlab="SNP location", main="Chromosome 3L", ylab="-ve log P value")
lines(c(0,max(chrs3L$SNP.position)), c(6,6), lty=2, col="red")


###################

setwd("/Users/Will/Documents/=WORK=/DGRP/RAL_Splines/RAL24C_output_5Apr")
#setwd("/Users/Will/Documents/=WORK=/DGRP/Assoc_Analyses")

wings.LM <- read.csv(file="Output_Landmarks+Veins+Outline.csv", nrows=14693, header=TRUE)

PrcCrds <- wings.LM[,15:110]

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
  LMx.mean <- colMeans( LMx )
  LMy.mean <- colMeans( LMy )
  
plot( colMeans(LMx), colMeans(LMy), xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.3, type="n", xaxt="n", yaxt="n", ann=FALSE)

points( LMx.mean[-48][-47][-42][-36], LMy.mean[-48][-47][-42][-36], cex=0.75, pch=1)

	if(no.LMs==15){
		from <- c(1,12,12,14,14,11,11,9,9,9,6,13,6,6,7,11,3,4,14)
		to <- c(7,7,13,13,15,15,10,10,8,3,8,8,12,2,2,5,5,5,10)
		} else {
			from <- c(6,5,46,26,46,25,24,23,4,22,21,3,20,2,19,18,17,1,16,15,14,12,30,29,7,28,2,32,33,8,34,9,35,3,37,38,39,40,10,41,11,4,43,44,45,8,10)
			to <- c(5,46,26,27,25,24,23,4,22,21,3,20,2,19,18,17,1,16,15,14,13,30,29,7,28,1,32,33,8,34,9,35,31,37,38,39,40,10,41,11,27,43,44,45,11,7,9)
			}
			
			if(lines==TRUE){
				for (i in 1:length(from)) {
    	lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=colour, lty=1, lwd=1 )
    	}	} else {
    		text( -0.20, -0.20, "proximal")
    		text( 0.22, -0.20, "distal")}
	}

WingPlot(PrcCrds, 48)


####################
require(animation)

LMx.mean <- LMx.mean[-48][-47][-42][-36]
LMy.mean <- LMy.mean[-48][-47][-42][-36]

par(mar=c(1,1,1,1))
plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.5, xaxt="n", yaxt="n", ann=FALSE)

chrsX1 <- read.csv("/Users/Will/Documents/=WORK=/DGRP/Assoc_Analyses/Xchunk1_LM_assoc.csv", header=TRUE, nrow=66375)
chrs2L_1 <- read.csv("/Users/Will/Desktop/2Lchunk1_LM_assoc.csv", header=TRUE, nrow=116410)
chrsX5 <- read.csv("/Users/Will/Desktop/Xchunk5_LM_assoc.csv", header=TRUE, nrow=66372)

names(chrsX1)
names(chrs2L_1)
names(chrsX5)

loadings <- chrsX1[,10:97]
loadings <- chrs2L_1[,10:97]
loadings <- chrs2R[,10:97]

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

comment(wingmovie) <- "the function requires 'loadings', which should be a 88 column dataframe (eg: loadings <- chrsX1[,10:97] ), comprising the 44x&44y coordinates part of the output from CPReaderer. 'SNP' is provided as a column index."

wingmovie <- function ( SNP, meanline=FALSE ) {
	
	LMx.mean <- c(0.058367853, 0.224740292, 0.240597079, 0.156247511, -0.112275132, -0.185506186, 0.002065871, 0.009307206, -0.080507945, -0.078516552, -0.139911044, -0.161003260, -0.189203900, -0.158421958, -0.090372838, -0.014881408, 0.100833875, 0.147752619, 0.190303376, 0.237729016, 0.223268050, 0.193138142, 0.097780636, 0.038348836, -0.032406825, -0.145572186, -0.186885578, 0.033660534, -0.047094142, -0.102842530, -0.186604739, 0.155638235, 0.083803118, -0.033494953, -0.131864972, 0.180145641, 0.123406944, 0.059080499, -0.010135095, -0.106051847, 0.102852963, 0.050431861, -0.008903797, -0.106093814)
#I have hard-coded these maen values in to avoid the user needing to read in a separate file for the Procrustes Coordinates
	LMy.mean <- c(-0.115526215, -0.031449221, 0.016181201, 0.087553255, 0.058460791, 0.047400667, -0.061833145, -0.020373341, -0.002516886, 0.014291731, 0.017306331, -0.014007708, -0.021637203, -0.089058692, -0.124422149, -0.127381694, -0.105211147, -0.088761688, -0.064681763, -0.008550775, 0.048987854, 0.072119519, 0.096862362, 0.095030952, 0.084874748, 0.033746470, 0.022672702, -0.084053371, -0.042596779, -0.027037364, 0.007053234, -0.024201111, -0.019786716, -0.012940107, -0.000192117, 0.024471143, 0.027488724, 0.027603548, 0.024680686, 0.015211537, 0.073878038, 0.065362038, 0.054968285, 0.055086123)
	
	x.vals <- seq(1, 87, 2)		#these pull out the odd...
	y.vals <- seq(2, 88, 2)		#...and even columns...
	x.loadings <- loadings[,c(x.vals)]		#...to become the X...
	y.loadings <- loadings[,c(y.vals)]		#...and Y coordinates

multiplier <- c(seq(-2, 2, 0.2), seq(2, -2, -0.2))
#multiplier is just a sequence from -2 to 2 and back to scale the frames

	x.plus <- (LMx.mean + x.loadings[SNP,])  #vector of X mean + loadings
	x.minus <- (LMx.mean - x.loadings[SNP,])  #vector of X mean - loadings
	y.plus <- (LMy.mean + y.loadings[SNP,])  #vector of Y mean + loadings
	y.minus <- (LMy.mean - y.loadings[SNP,])  #vector of Y mean - loadings
	
	from <- c(6,5,44,26,44,25,24,23,4,22,21,3,20,2,19,18,17,1,16,15,14,12,30,29,7,28,2,32,33,8,34,9,35,3,36,37,38,39,10,40,11,4,41,42,43,8,10)
#'from' and 'to' are paired vectors - the wing outline is made up of lines that are plotted FROM from[n] TO to[n]...
	to <- c(5,44,26,27,25,24,23,4,22,21,3,20,2,19,18,17,1,16,15,14,13,30,29,7,28,1,32,33,8,34,9,35,31,36,37,38,39,10,40,11,27,41,42,43,11,7,9)

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
    	
#    	for (i in 1:length(LMx.mean)) {
#	lines( c(x.plus[i], x.minus[i]), c(y.plus[i], y.minus[i]), col="red")
#	}		#
    	}
	}
}


wingmovie(83565, meanline=TRUE)
wingmovie(84672, meanline=TRUE)
wingmovie(19022, meanline=TRUE)
wingmovie(424595, meanline=TRUE)

saveGIF(expr=wingmovie(424595, meanline=TRUE), interval=0.1, "2R_8201409.gif")




#######USEFUL 'SCAFFOLDING' FOR BUILDING/FIXING WINGPLOTS#######

for (i in 1:length(LMx.mean)) {
		char <- as.character(i)
    	points( LMx.mean[i], LMy.mean[i], col="blue", cex=1, pch=char )
    	}

plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.5, type="n", xaxt="n", yaxt="n", ann=FALSE)

for (i in 1:length(LMx.mean)) {
		char <- as.character(i)
    	text( LMx.mean[i], LMy.mean[i], col="blue", char, cex=0.75 )
    	}


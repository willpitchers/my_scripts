
setwd("/Users/Will/Documents/=WORK=/Code_Toolbox/Wingplots")
mean.shape <- read.csv("Combined_Mean_Shape.csv")
segment <- read.csv("wing_segment.csv")

####################

#PrcCrds <- wings.LM[,4:99]

WingPlot <- function( PrcCrds, wingcol="blue", lines=TRUE, add=FALSE, wingcex=0.75, winglwd=1, winglty=1, wingpch=1 ){
	
	odds <- seq(1, 95, 2)
	evens <- seq(2, 96, 2)
	if ( class(PrcCrds) == "data.frame" ){
		LMx <- PrcCrds[,c(odds)]
		LMy <- PrcCrds[,c(evens)]
		LMx.mean <- colMeans( LMx, na.rm=TRUE)
		LMy.mean <- colMeans( LMy, na.rm=TRUE)
	} 	else {
			LMx.mean <- PrcCrds[c(odds)]
			LMy.mean <- PrcCrds[c(evens)]
	}
	
	if (add == FALSE){
	plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=0.3, type="n", xaxt="n", yaxt="n", ann=FALSE)
	points( LMx.mean, LMy.mean, cex=wingcex, pch=wingpch)
		}
	from <- segment$from
	to <- segment$to
		
	if(lines==TRUE){
		for (i in 1:length(from)) {
    	lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=wingcol, lty=winglty, lwd=winglwd )
    	}	}
	}

#WingPlot(PrcCrds, 48, wingcol="dark green")

comment(WingPlot) <- c("WingPlot takes a matrix of Proc Coords and 'joins the dots' to plot the wing outline and veins. It plots mean points for each LM and semi-LM and connects them with lines when lines=TRUE (default). Can accept a 'wingcol' argument if you don't like blue")


##################

WingBlur <- function ( PrcCrds, wingcol="#add8e632", winglwd=3, winglty=1, wingpch=1, wingcex=0.9, meanline=TRUE, meancol="blue", meanlty=1, meanlwd=2 ) {
	# set up required coord vectors
	from <- segment$from
	to <- segment$to
	x.vals <- seq(1, 95, 2)		#these pull out the odd...
	y.vals <- seq(2, 96, 2)		#...and even columns...
	shape.X <- PrcCrds[,x.vals]	# make x coord matrix
	shape.Y <- PrcCrds[,y.vals]	# and one for y
	LMx.mean <- colMeans(PrcCrds)[x.vals]	# make mean x vector
	LMy.mean <- colMeans(PrcCrds)[y.vals]	# and one for y
	
	# set up the plot
	plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), type="n", xaxt="n", yaxt="n", ann=FALSE)

	# nested loops to draw each line segment for each row in shape.X matrices
	for (j in 1:nrow(shape.X)){
		for (i in 1:length(from)) {
		lines( c(shape.X[j,from[i]], shape.X[j,to[i]]), c(shape.Y[j,from[i]], shape.Y[j,to[i]]), col=wingcol, lty=winglty, lwd=3 )
		}	}
	
	# loop to draw the mean lines if requested
	if (meanline ==TRUE) {
		for (i in 1:length(from)) {
    	lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=meancol, lty=meanlty, lwd=meanlwd )
    	}	}
    	
	# finally, this drop in the mean LM/semi-LM points
	points(LMx.mean, LMy.mean, cex=wingcex, pch=wingpch)
}


comment(WingBlur) <- c("This draws a wing outline&veins for each row of the matrix 'PrcCrds' - can be individuals, line means, whatever... the default setting are for translucent lines so that the coloured area represents that phenotypic space sampled by the wings measured. Mean shape overlaid as points + outline, but can be suppressed")

##################

WingEffect <- function ( meanshape, effectplus, effectminus, wingcol=c("black", "black", "red"), winglwd=c(2, 2, 2), winglty=c(1, 1, 1), scale.factor=2, meanline=FALSE, wingcex=c(0,0,0), winglabel="", add=FALSE, wingframe=T, scale.display=T, wingpoints=F, wingpch=c(16,1,1) ) {

	# set up required coord vectors
		from <- segment$from	# from and to indices drawn from the 'segment' file
		to <- segment$to
		x.vals <- seq(1, 95, 2)	# indices to specify x and y coords
		y.vals <- seq(2, 96, 2)		
		LMx.mean <- meanshape[x.vals]	# apply x & y indices to mean data
		LMy.mean <- meanshape[y.vals] 
		# x & y indices to effectplus vector
		x.plus <- LMx.mean + (effectplus[x.vals] * scale.factor)
		y.plus <- LMy.mean + (effectplus[y.vals] * scale.factor)
		# x & y indices to effectminus vector
		x.minus <- LMx.mean - (effectminus[x.vals] * scale.factor)
		y.minus <- LMy.mean - (effectminus[y.vals] * scale.factor)

	# plot mean shape
		if ( add == FALSE ) { # opens blank plot
		plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), type="n", xaxt="n", yaxt="n", ann=FALSE, frame=wingframe)
		}
		if (meanline == TRUE){
		# this for loop draws line segments for the mean shape
		for (i in 1:length(from)) {
	    	lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=wingcol[1], lty=winglty[1], lwd=winglwd[2])
	    	}	}
	    if (wingpoints == TRUE) {
	    	points( x.plus, y.plus, cex=wingcex[2], pch=wingpch[2], col=wingcol[2])
	    	points( x.minus, y.minus, cex=wingcex[3], pch=wingpch[3], col=wingcol[3])
	    }
	    	
		# this drop in the mean LM/semi-LM points
		points(LMx.mean, LMy.mean, cex=wingcex[1], pch=wingpch[1], col=wingcol[1])
		
	# this for loop draws the mean+effect line segments
	for (i in 1:length(from)) {
		lines( c(x.plus[from[i]], x.plus[to[i]]), c(y.plus[from[i]], y.plus[to[i]]), col=wingcol[2], lty=winglty[2], lwd=winglwd[2] )
		lines( c(x.minus[from[i]], x.minus[to[i]]), c(y.minus[from[i]], y.minus[to[i]]), col=wingcol[3], lty=winglty[3], lwd=winglwd[3] )
		}
	# adding an annotation of the effect scaling factor
	if (scale.display == T) {
		text( 0.025, -0.215, paste("scale factor =", as.character(scale.factor)) )
		}
	text(0, 0.25, winglabel) # optional centre-top label
} 


comment(WingEffect) <- c("This makes mean ± an effect of choice wingplots - mean LMs/semi-LMs connected by outline&veins lines, with superimposed outline&veins lines for a specifed modifier effect. e.g. specify a vector of effect coefficients from a model to plot lines for mean+effect and mean-effect")

####################
require(animation)

WingMovie <- function ( meanshape, effect, wingcol=c("blue", "red"), winglwd=c(2, 2), winglty=c(1, 1), wingcex=1, scale.factor=2, effectlines=FALSE, meanline=TRUE, winglabel="" ) {
	
	# set up required coord vectors
		from <- segment$from	# from and to indices drawn from the 'segment' file
		to <- segment$to
		x.vals <- seq(1, 95, 2)	# indices to specify x and y coords
		y.vals <- seq(2, 96, 2)		
		LMx.mean <- meanshape[x.vals]	# apply x & y indices to mean data
		LMy.mean <- meanshape[y.vals] 
	# set up X & Y coords from effect vector
		x.loadings <- effect[c(x.vals)]		#...to become the X...
		y.loadings <- effect[c(y.vals)]		#...and Y coordinates
	# set up stepping vectors
		multiplier <- c(seq(-scale.factor, scale.factor, (scale.factor/10)), seq(scale.factor, -scale.factor, -(scale.factor/10)))	# multiplier is just a sequence from -2 to 2 and back to scale the frames
		x.plus <- (LMx.mean + scale.factor *(x.loadings))  #vector of X mean + loadings
		x.minus <- (LMx.mean - scale.factor *(x.loadings))  #vector of X mean - loadings
		y.plus <- (LMy.mean + scale.factor *(y.loadings))  #vector of Y mean + loadings
		y.minus <- (LMy.mean - scale.factor *(y.loadings))  #vector of Y mean - loadings

	# set up animation 'frames'
	for (j in 1:length(multiplier)) {
		plotX <- as.numeric(LMx.mean + (x.loadings * multiplier[j]))
		plotY <- as.numeric(LMy.mean + (y.loadings * multiplier[j]))
		# plotX&Y are vectors of mean coords + (loadings * scale)

		plot( LMx.mean, LMy.mean, xlim=c(-0.22,0.27), ylim=c(-0.22,0.27), cex=wingcex, col=wingcol[1], xaxt="n", yaxt="n", ann=FALSE) 	# sets up plot, adds mean points
		text( 0.025, -0.215, paste("scale factor =", as.character(scale.factor)) ) #labels with scale factor
		text(0, 0.25, winglabel) # optional centre-top label
		
		for (i in 1:length(from)) {
			if (meanline==TRUE) {
				lines( c(LMx.mean[from[i]], LMx.mean[to[i]]), c(LMy.mean[from[i]], LMy.mean[to[i]]), col=wingcol[1], lty=winglty[1], lwd=winglwd[1] )
				}
		    	lines( c(plotX[from[i]], plotX[to[i]]), c(plotY[from[i]], plotY[to[i]]), col=wingcol[2], lty=winglty[2], lwd=winglwd[2] )	# draws the outline segments for each 'from-to' pair
    		if (effectlines==TRUE){
   				for (j in 1:length(LMx.mean)) {
					lines( c(x.plus[j], x.minus[j]), c(y.plus[j], y.minus[j]), col="black", lwd=1)
				}	}		# this loop draws in the vectors of change across the mean for each LM/sLM
    	}
	}
}


comment(WingMovie) <- c("This makes mean ± an effect of choice wingplots - mean LMs/semi-LMs connected by outline&veins lines, with an animated line tracking from +effect to -effect [*scale factor]. Colours and line types may be specified as a vector e.g. wingcols=c('blue', 'red')... Call with saveGIF(expr=WingMovie( meanshape, effect ), interval=0.1, filename='name.gif') ")


####################
require(animation)

WingMovieSNP <- function ( SNP, meanline=FALSE ) {
	
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

comment(WingMovieSNP) <- "the function requires 'loadings', which should be a 88 column dataframe (eg: loadings <- chrsX1[,10:97] ), comprising the 44x&44y coordinates part of the output from CPReaderer. 'SNP' is provided as a column index."


#wingmovie(85, meanline=TRUE)

#saveGIF(expr=wingmovie(85, meanline=TRUE), interval=0.1, "test!.gif")



################
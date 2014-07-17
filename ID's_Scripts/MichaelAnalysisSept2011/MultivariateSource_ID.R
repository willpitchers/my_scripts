#  Source script for Multivariate functions.
# Ian Dworkin   
# Updated October 1st 2011

####################################

# calculate tangent approximaton for tangent approximates Procrustes Distance (Euclidean Distance) 
# This is just the magnitude of the vector!
PD <- function(x) { 
	sqrt(t(x)%*%x)}
comment(PD) <- c("This just computes the Euclidean Distance (norm) for a vector")



####################################	

# This function will compute the angle between two vectors.	BEWARE if the vectors can be of arbitrary sign, use the function below this one.

ang.vec <- function(vec1, vec2){
	vec.cor <- (t(vec1) %*% vec2)/(PD(vec1)*PD(vec2))
	vec.angle <- acos(vec.cor)*(180/pi)
	return(c(vector.cor=vec.cor, vec.angle=vec.angle))}	
comment(ang.vec) <- c(" This computes both the vector correlation, and angle, between two vectors.", " to compare to the Pearson correlation coefficient make sure to center and standardize vectors", "DO NOT USE THIS IF VECTORS CAN BE OF ARBITRARY SIGN")

####### When the vectors can be of arbitrary sign, use this which computes the magnitude of the vector correlation, and then computes the angle.
ang.vec.abs <- function(vec1, vec2){
	vec.cor <- abs((t(vec1) %*% vec2)/(PD(vec1)*PD(vec2)))
	vec.angle <- acos(vec.cor)*(180/pi)
	return(c(vector.cor=vec.cor, vec.angle=vec.angle))}	
comment(ang.vec) <- c(" This computes both the vector correlation, and angle, between two vectors.", " to compare to the Pearson correlation coefficient make sure to center and standardize vectors", "set it up to compute the absolute values of the vector correlation")


#################################

# PLS function from Claude 2010, with modification
PLS <- function(M1, M2, p2 = 1) {
  # we allow p2 to be a simple vector, instead of a matrix for our needs. otherwise specify p2 = dim(M2)[2]
  p1 <- dim(M1)[2]
  # p2 <- dim(M2)[2]  # original set up, we should fix this to make it more generally useful
  n <- dim(M1)[1]
  sM12 <- svd(var(cbind(M1,M2))[1:p1, (p1+1):(p1+p2)])
  vM12 <- var(cbind(M1,M2))[1:p1, (p1+1):(p1+p2)]
  vM21 <- var(cbind(M1,M2))[(p1+1):(p1+p2), 1:p1]
  v11 <- var(M1)
  v22 <- var(M2)
  D <- sM12$d
  F1 <- sM12$u
  F2 <- sM12$v
  Rv <- sum(diag(vM12%*%vM21))/sqrt(sum(diag(v11%*%v11))*
  sum(diag(v22%*%v22)))
  return(c(Rv=Rv, D=D, F1=F1, F2=F2))
}
comment(PLS) <- "we allow p2 to be a simple vector, instead of a matrix for our needs. otherwise specify p2 = dim(M2)[2]"


############################################

# Klingenberg's "shape score"
# now let us compute Klingenberg's "shape score" as per Drake and Klingenberg

ShapeScore <- function(Beta, Y) {
	## This computes the "shape score" of Drake and Klingenberg 2008
	## Beta is the coefficients for each procrustes coordinate from a multivariate multiple regression. 
	## i.e. shape.model.1 <- lm(cbind(Y1, Y2) ~ X1 + X2, data=data.set )
	## Beta ...   beta.model <- t(coef(shape.model.1)[k,]) # where k is the covariate of focus
	## 
	## Y is the raw procrustes coordinates (Y1, Y2, ...)
	# Since Klingenberg uses the generalized inverses, we have tested this for the 2p-4 PCs and have gotten identical results.
	s = Y %*% t(Beta) %*% ((Beta %*% t(Beta))^-0.5)
	return(shapeScore=s)
}

comment(ShapeScore) <- c("This computes the 'shape score' of Drake and Klingenberg 2008", 
  "Beta is the coefficients for each procrustes coordinate from a multivariate multiple regression. ",
  "i.e. shape.model.1 <- lm(cbind(Y1, Y2) ~ X1 + X2, data=data.set )",
  "Beta ...   beta.model <- t(coef(shape.model.1)[k,]) # where k is the covariate of focus",
  "Y is the raw procrustes coordinates (Y1, Y2, ...)",
  "Klingenberg uses the generalized inverses, we have tested this for the 2p-4 PCs and have gotten identical results.")



##################################
KirkPatrickDimensionalityMeasures <- function(x){
	## This implements Kirkpatrick's (2009) simple measures of evolvability and dimensionality
	## The input is a Genetic covariance matrix for mean standardized traits.
	EigenValues <- svd(x)$d
	EffDim <- sum(EigenValues/EigenValues[1])
	MaxEvol <- sqrt(EigenValues[1])
	TotalGenetVar <- sum(EigenValues)
    return( c(Dimensionality = EffDim, MaximumEvolvability = MaxEvol, TotalGenetVar = TotalGenetVar  ))
}
comment(KirkPatrickDimensionalityMeasures) <- c("This implements Kirkpatrick's (2009) simple measures of evolvability and dimensionality", "The input is a Genetic covariance matrix for mean standardized traits.")




######################################
# Function to computer the CI's for vector correlations and angles.
###### This still needs to be generalized for either MCMC or bootstrapping. Then functionalized.

##### Currently Using the posterior distributions to make the comparisons

# Grab the resampled data or posterior MCMC simulations
# data.1 <- model.legs.2.wt.18$VCV #SET 1
# data.2 <- model.legs.3.wt.18$VCV #SET2

# # Make sure the resampling/ posterior sims are the same length (number of iterations)
# N1 <- nrow(data.1)
# N2 <- nrow(data.2)
# N1==N2 # check to make sure the data sets are the same size. Make this into a function, and if it fails, either give an error or just use min(N1,N2) for length

# # Set up empty matrix to store the results for the vector correlations and angles for each of three eigenvectors
# # Make a variable in the function for the number of vectors to compare (and change ncol)
# angles.vectors <- matrix(NA, ncol=6, nrow=N1)

# # just setting up names. This can easily be extended using paste or interaction.
# colnames(angles.vectors) <- c("VecCor1", "Angle1","VecCor2", "Angle2", "VecCor3", "Angle3")

# for (i in 1:N1) {
	# # First extract the sample of the covariance matrix from the posterior simulation
	# cov.1 <- matrix(data.1[i,1:9], nrow=3, ncol=3)
	# cov.2 <- matrix(data.2[i,1:9], nrow=3, ncol=3)
    # # compute the vector correlations and angles. I could use svd(), also needs to be generalized to more than 3 vectors.
    # angles.vectors[i,] <- c(  ang.vec.abs(eigen(cov.1)$vectors[,1], eigen(cov.2)$vectors[,1]),
                              # ang.vec.abs(eigen(cov.1)$vectors[,2], eigen(cov.2)$vectors[,2]),
                              # ang.vec.abs(eigen(cov.1)$vectors[,3], eigen(cov.2)$vectors[,3]) )
# }



########################### WHAT STILL NEEDS TO BE IMPLEMENTED
# PROCRUSTES ANOVA
# VARIANCE ACCOUNTED FOR VIA PROCRUSTES DISTANCE


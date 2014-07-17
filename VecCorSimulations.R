
source("/Users/Will/Documents/=WORK=/FUNCTION_MASTER.R")
require(car)
require(MCMCglmm)

# I'm going to simulate random vectors in 58-d space to put a number on the likelihood of vector correlation by chance...

# from main dataset:
mean_sd <- matrix(NA, 58, 2)

for (i in 1:58) {
	mean_sd[i,1] <- mean(wings.PC$x[,i])
	mean_sd[i,2] <- var(wings.PC$x[,i])
}


nitt <- 1000
rveccor <- rep( NA, nitt )

	randvec_1 <- mvrnorm( n=nitt, mu=mean_sd[,1], Sigma=diag(mean_sd[,2]), empirical=T )
	randvec_2 <- mvrnorm( n=nitt, mu=mean_sd[,1], Sigma=diag(mean_sd[,2]), empirical=T )


for (i in seq(nitt)) {
	# randvec_1 <- matrix( rnorm(58, mean=0, sd=1), nrow=1 )
	# randvec_2 <- matrix( rnorm(58, mean=0, sd=1), nrow=1 )
	rveccor[i] <- ang.vec.abs( randvec_1[i,], randvec_2[i,] )[1]
}

quantile( rveccor, probs=c(0.95) )

vchpd <- HPDinterval( as.mcmc( rveccor ) )

plot( density(rveccor), lwd=2 )
	# abline( v=0.66, col="red" )
	polyX <- density(rveccor)$x[ density(rveccor)$x>=vchpd[1] & density(rveccor)$x<=vchpd[2] ]
	polyY <- density(rveccor)$y[ density(rveccor)$x>=vchpd[1] & density(rveccor)$x<=vchpd[2] ]
	polygon( c(polyX[1], polyX, polyX[length(polyX)]), c(0, polyY, 0), col="#32323232", border=NULL )


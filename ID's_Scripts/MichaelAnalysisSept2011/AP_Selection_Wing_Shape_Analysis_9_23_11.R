#### Analysis of the experimental evolution lines after 31&32 generations of selection.
## Michael DeNieu updated 9/17/11
## ID updated 9/20/11

#### Functions for this script..


require(MCMCglmm)
require(MASS)
# Calculate the angle between vectors for bootstrapped PLS
# We want to write a function that will calculate the angle between
# singular vectors for each set of bootstrapped PLS
AngVecAbsBoot <- function(boot.vectors1, boot.vectors2) {
	# vector1 is the first matrix of singular vectors to be compared
	# vector2 is the second matrix of singular vectors to be compared
	# rows of the matrices must be each bootstrapped singular vector
	ang.vec.boots <- matrix(data = NA, nrow = nrow(boot.vectors1), ncol =2)

	for( i in 1:nrow(boot.vectors1)) {
		ang.vec.boots[i, ] <- ang.vec.abs(boot.vectors1[i, ], boot.vectors2[i, ])
	}
	return(ang.vec.boots = ang.vec.boots)
}



# For Michael's machine
setwd("/home/Michael/Documents/Predation Wing Size/Selection Differential/Final Text Files")
source("/home/Michael/Documents/Predation Wing Size/Selection Differential/Analysis Files/MultivariateSource_ID.R")

# For Ian's machine
setwd("/Users/ian/Projects_current/mantid selection/MD_Mantid_Selection_Gen30/")
source("/Users/ian/R/RSourceFiles/MultivariateSource_ID.R")

wings.complete <- read.csv("selection_analysis_wings_predation_lines.csv", h=T)
wings.complete$individual <- as.factor(wings.complete$individual)
comment(wings.complete) <- "block represents generation (30 and 31). However Replicate 1 flies and replicate two flies were not measured at the same time (~5 days apart within each generation)"

dim(wings.complete)
head(wings.complete)
tail(wings.complete)
str(wings.complete)
comment(wings.complete)


# I want to average size and shape values
# Create vector of names for aggregate function
names <- paste("ProcCoord", 2:30, sep = '')
coord.list <- paste(names, names, sep = " = ", collapse=" , ")

# First sort by individual
wings.complete <- wings.complete[order(wings.complete$individual),]
str(wings.complete)
summary(wings.complete)
head(wings.complete)

# Aggregate the data using the mean value for each individual
# The list x contains all of the values which will be averaged specifying the old
# column identifier and the new header identifyer (none of the headers are being modified here)
# The by list specifies the colunns that should be maintained unaltered
# FUN denotes the funtion that will be used to aggregate (in this case mean())
averaged.data <- with(wings.complete, aggregate(x=list( Centroid_Size = Centroid_Size, Log_Centroid_Size=Log_Centroid_Size, ProcCoord1 = ProcCoord1, ProcCoord2 = ProcCoord2, ProcCoord3 = ProcCoord3, ProcCoord4 = ProcCoord4, ProcCoord5 = ProcCoord5, ProcCoord6 = ProcCoord6, ProcCoord7 = ProcCoord7, ProcCoord8 = ProcCoord8, ProcCoord9 = ProcCoord9, ProcCoord10 = ProcCoord10, ProcCoord11 = ProcCoord11, ProcCoord12 = ProcCoord12,  ProcCoord13 = ProcCoord13, ProcCoord14 = ProcCoord14, ProcCoord15 = ProcCoord15, ProcCoord16 = ProcCoord16, ProcCoord17 = ProcCoord17, ProcCoord18 = ProcCoord18, ProcCoord19 = ProcCoord19, ProcCoord20 = ProcCoord20, ProcCoord21 = ProcCoord21, ProcCoord22 = ProcCoord22, ProcCoord23 = ProcCoord23, ProcCoord24 = ProcCoord24, ProcCoord25 = ProcCoord25, ProcCoord26 = ProcCoord26, ProcCoord27 = ProcCoord27, ProcCoord28 = ProcCoord28, ProcCoord29 = ProcCoord29, ProcCoord30 = ProcCoord30), by = list(individual = individual, Survival = Survival, Treatment = Treatment, Replicate = Replicate, Block = Block, Sex=Sex, Survive=Survive), FUN=mean))
str(averaged.data)
summary(averaged.data)

head(averaged.data)

check <- duplicated(averaged.data[,1])
summary(check)


averaged.data <- averaged.data[order(averaged.data$individual),]

# Create subsets of data for use in analysis with PLS


# First let's test all possible subsets and then collapse from there if particular covariates
# are not explaining different portions of the variation
sel.r1.b1.m <- subset(averaged.data, Treatment == "SEL" & Replicate == "R1" & Block == "B1" & Sex == "M")
sel.r1.b1.f <- subset(averaged.data, Treatment == "SEL" & Replicate == "R1" & Block == "B1" & Sex == "F")
con.r1.b1.m <- subset(averaged.data, Treatment == "CON" & Replicate == "R1" & Block == "B1" & Sex == "M")
con.r1.b1.f <- subset(averaged.data, Treatment == "CON" & Replicate == "R1" & Block == "B1" & Sex == "F")
sel.r2.b1.m <- subset(averaged.data, Treatment == "SEL" & Replicate == "R2" & Block == "B1" & Sex == "M")
sel.r2.b1.f <- subset(averaged.data, Treatment == "SEL" & Replicate == "R2" & Block == "B1" & Sex == "F")
con.r2.b1.m <- subset(averaged.data, Treatment == "CON" & Replicate == "R2" & Block == "B1" & Sex == "M")
con.r2.b1.f <- subset(averaged.data, Treatment == "CON" & Replicate == "R2" & Block == "B1" & Sex == "F")

sel.r1.b2.m <- subset(averaged.data, Treatment == "SEL" & Replicate == "R1" & Block == "B2" & Sex == "M")
sel.r1.b2.f <- subset(averaged.data, Treatment == "SEL" & Replicate == "R1" & Block == "B2" & Sex == "F")
con.r1.b2.m <- subset(averaged.data, Treatment == "CON" & Replicate == "R1" & Block == "B2" & Sex == "M")
con.r1.b2.f <- subset(averaged.data, Treatment == "CON" & Replicate == "R1" & Block == "B2" & Sex == "F")
sel.r2.b2.m <- subset(averaged.data, Treatment == "SEL" & Replicate == "R2" & Block == "B2" & Sex == "M")
sel.r2.b2.f <- subset(averaged.data, Treatment == "SEL" & Replicate == "R2" & Block == "B2" & Sex == "F")
con.r2.b2.m <- subset(averaged.data, Treatment == "CON" & Replicate == "R2" & Block == "B2" & Sex == "M")
con.r2.b2.f <- subset(averaged.data, Treatment == "CON" & Replicate == "R2" & Block == "B2" & Sex == "F")



# PLS function from Morphometrics in R, in the Dworkin lab multivariate source file. Has been modified


### Test with whole data set (to make sure function is working)
# Columns
# 1.  individual        (factor)
# 2.  Survival          (Factor: "D" and "S")
# 3.  Treatment         (Factor: "CON" and "SEL")
# 4.  Replicate         (Factor: "R1" and "R2")
# 5.  Block             (Factor: "B1" and "B2")
# 6.  Sex               (Factor: "M" and "F")
# 7.  Survive           (Int: 0 = dead, 1 = survived)
# 8.  Centroid_Size     (numeric)
# 9.  Log_Centroid_Size (numeric)
# 10. ProcCoord1        (numeric)
# ...............................
# 39. ProcCoord30       (numeric)

PLS(averaged.data[,10:39], averaged.data[, 7])

# Let's compute the angles between vectors for sets of the data

pls.sel.r1.b1.m <- PLS(sel.r1.b1.m[ ,10:39], sel.r1.b1.m[ , 7])
pls.sel.r1.b1.f <- PLS(sel.r1.b1.f[ ,10:39], sel.r1.b1.f[ , 7])
pls.con.r1.b1.m <- PLS(con.r1.b1.m[ ,10:39], con.r1.b1.m[ , 7])
pls.con.r1.b1.f <- PLS(con.r1.b1.f[ ,10:39], con.r1.b1.f[ , 7])
pls.sel.r2.b1.m <- PLS(sel.r2.b1.m[ ,10:39], sel.r2.b1.m[ , 7])
pls.sel.r2.b1.f <- PLS(sel.r2.b1.f[ ,10:39], sel.r2.b1.f[ , 7])
pls.con.r2.b1.m <- PLS(con.r2.b1.m[ ,10:39], con.r2.b1.m[ , 7])
pls.con.r2.b1.f <- PLS(con.r2.b1.f[ ,10:39], con.r2.b1.f[ , 7])

pls.sel.r1.b2.m <- PLS(sel.r1.b2.m[ ,10:39], sel.r1.b2.m[ , 7])
pls.sel.r1.b2.f <- PLS(sel.r1.b2.f[ ,10:39], sel.r1.b2.f[ , 7])
pls.con.r1.b2.m <- PLS(con.r1.b2.m[ ,10:39], con.r1.b2.m[ , 7])
pls.con.r1.b2.f <- PLS(con.r1.b2.f[ ,10:39], con.r1.b2.f[ , 7])
pls.sel.r2.b2.m <- PLS(sel.r2.b2.m[ ,10:39], sel.r2.b2.m[ , 7])
pls.sel.r2.b2.f <- PLS(sel.r2.b2.f[ ,10:39], sel.r2.b2.f[ , 7])
pls.con.r2.b2.m <- PLS(con.r2.b2.m[ ,10:39], con.r2.b2.m[ , 7])
pls.con.r2.b2.f <- PLS(con.r2.b2.f[ ,10:39], con.r2.b2.f[ , 7])

##  Pull out the Rv and SV both the mean, and bootstrap CI.

# To look at the angle between vectors, use the ang.vec.abs() function  

# We first want to compare the differences between blocks
ang.vec.abs(pls.sel.r1.b1.m[3:32], pls.sel.r1.b2.m[3:32])
ang.vec.abs(pls.con.r1.b1.m[3:32], pls.con.r1.b2.m[3:32])
ang.vec.abs(pls.sel.r2.b1.m[3:32], pls.sel.r2.b2.m[3:32])
ang.vec.abs(pls.con.r2.b1.m[3:32], pls.con.r2.b2.m[3:32])

ang.vec.abs(pls.sel.r1.b1.f[3:32], pls.sel.r1.b2.f[3:32])
ang.vec.abs(pls.con.r1.b1.f[3:32], pls.con.r1.b2.f[3:32])
ang.vec.abs(pls.sel.r2.b1.f[3:32], pls.sel.r2.b2.f[3:32])
ang.vec.abs(pls.con.r2.b1.f[3:32], pls.con.r2.b2.f[3:32])

# Comparing males and females
ang.vec.abs(pls.sel.r1.b1.m[3:32], pls.sel.r1.b1.f[3:32])
ang.vec.abs(pls.con.r1.b1.m[3:32], pls.con.r1.b1.f[3:32])
ang.vec.abs(pls.sel.r2.b1.m[3:32], pls.sel.r2.b1.f[3:32])
ang.vec.abs(pls.con.r2.b1.m[3:32], pls.con.r2.b1.f[3:32])

ang.vec.abs(pls.sel.r1.b2.m[3:32], pls.sel.r1.b2.f[3:32])
ang.vec.abs(pls.con.r1.b2.m[3:32], pls.con.r1.b2.f[3:32])
ang.vec.abs(pls.sel.r2.b2.m[3:32], pls.sel.r2.b2.f[3:32])
ang.vec.abs(pls.con.r2.b2.m[3:32], pls.con.r2.b2.f[3:32])

### Double check via mass::lda

# Now we want to write a resampler that does pairs (row level) bootstrapping to generate CIs for 

PLS.resampler <- function(x = your.data.frame, block1, block2) {
	# x is the data frame that you have all of the data in for the PLS analysis
	# block1 are the columns in x for the first block of variables
	# block2 are the columns in x for the second block of variables
	
	# This does a pairs resample
	x.resample <- x[sample(nrow(x), nrow(x), replace=T), ]
	PLS(M1 = x.resample[, block1], M2 = x.resample[ , block2], p2 = 1)
	
}

# Let's try it with a for loop and see if it is any faster
# The loop is slightly slower than using replicate. both are approximately 2.5 minutes on my machine
PLS.loop.resampler <- function(x = your.data.frame, block1, block2, bootstrap.num) {
	# x is the data frame that you have all of the data in for the PLS analysis
	# block1 are the columns in x for the first block of variables
	# block2 are the columns in x for the second block of variables
	boot <- matrix(data = NA, nrow = bootstrap.num, ncol = 33)
	# This does a pairs resample
	for(i in 1:bootstrap.num) {
		x.resample <- x[sample(nrow(x), nrow(x), replace=T), ]
		boot[i, ] <- PLS(M1 = x.resample[ , block1], M2 = x.resample[ , block2], p2 = 1)
	}
	return(boot.call = boot)
}

system.time(test <- PLS.loop.resampler(x = averaged.data, block1 = 10:39, block2 = 9, 1000))

# Now we run 1000 replicates
system.time(PLS.boot.1000 <- t( replicate(1000, PLS.resampler(x = averaged.data, block1 = 10:39, block2 = 9))))

quantile(PLS.boot.1000[,1], probs=c(0.025, 0.975))

pls.sel.r1.b1.m <- t( replicate(1000, PLS.resampler(sel.r1.b1.m, 10:39, 7)))
quantile(pls.sel.r1.b1.m[,1], probs=c(0.025, 0.975))
pls.sel.r1.b1.f <- t( replicate(1000, PLS.resampler(sel.r1.b1.f, 10:39, 7)))
pls.con.r1.b1.m <- t( replicate(1000, PLS.resampler(con.r1.b1.m, 10:39, 7)))
pls.con.r1.b1.f <- t( replicate(1000, PLS.resampler(con.r1.b1.f, 10:39, 7)))
pls.sel.r2.b1.m <- t( replicate(1000, PLS.resampler(sel.r2.b1.m, 10:39, 7)))
pls.sel.r2.b1.f <- t( replicate(1000, PLS.resampler(sel.r2.b1.f, 10:39, 7)))
pls.con.r2.b1.m <- t( replicate(1000, PLS.resampler(con.r2.b1.m, 10:39, 7)))
pls.con.r2.b1.f <- t( replicate(1000, PLS.resampler(con.r2.b1.f, 10:39, 7)))

pls.sel.r1.b2.m <- t( replicate(1000, PLS.resampler(sel.r1.b2.m, 10:39, 7)))
pls.sel.r1.b2.f <- t( replicate(1000, PLS.resampler(sel.r1.b2.f, 10:39, 7)))
pls.con.r1.b2.m <- t( replicate(1000, PLS.resampler(con.r1.b2.m, 10:39, 7)))
pls.con.r1.b2.f <- t( replicate(1000, PLS.resampler(con.r1.b2.f, 10:39, 7)))
pls.sel.r2.b2.m <- t( replicate(1000, PLS.resampler(sel.r2.b2.m, 10:39, 7)))
pls.sel.r2.b2.f <- t( replicate(1000, PLS.resampler(sel.r2.b2.f, 10:39, 7)))
pls.con.r2.b2.m <- t( replicate(1000, PLS.resampler(con.r2.b2.m, 10:39, 7)))
pls.con.r2.b2.f <- t( replicate(1000, PLS.resampler(con.r2.b2.f, 10:39, 7)))


# Here we calculate confidence intervals for the angle between singular vectors
# First we use the function above to calculate the vecotr correlation and vector 
# angle for each of the bootstrapped pairs

angles1 <- AngVecAbsBoot(pls.sel.r1.b1.m[ ,3:32], pls.sel.r1.b2.m[ ,3:32])
angles2 <- AngVecAbsBoot(pls.con.r1.b1.m[ ,3:32], pls.con.r1.b2.m[ ,3:32])
angles3 <- AngVecAbsBoot(pls.sel.r2.b1.m[ ,3:32], pls.sel.r2.b2.m[ ,3:32])
angles4 <- AngVecAbsBoot(pls.con.r2.b1.m[ ,3:32], pls.con.r2.b2.m[ ,3:32])

angles5 <- AngVecAbsBoot(pls.sel.r1.b1.f[ ,3:32], pls.sel.r1.b2.f[ ,3:32])
angles6 <- AngVecAbsBoot(pls.con.r1.b1.f[ ,3:32], pls.con.r1.b2.f[ ,3:32])
angles7 <- AngVecAbsBoot(pls.sel.r2.b1.f[ ,3:32], pls.sel.r2.b2.f[ ,3:32])
angles8 <- AngVecAbsBoot(pls.con.r2.b1.f[ ,3:32], pls.con.r2.b2.f[ ,3:32])

# Here we print the 95% confidence intervals for the vector correlation and angle
quantile(angles1[,1], probs=c(0.025, 0.975))
quantile(angles2[,1], probs=c(0.025, 0.975))
quantile(angles3[,1], probs=c(0.025, 0.975))
quantile(angles4[,1], probs=c(0.025, 0.975))
quantile(angles5[,1], probs=c(0.025, 0.975))
quantile(angles6[,1], probs=c(0.025, 0.975))
quantile(angles7[,1], probs=c(0.025, 0.975))
quantile(angles8[,1], probs=c(0.025, 0.975))

quantile(angles1[,2], probs=c(0.025, 0.975))
quantile(angles2[,2], probs=c(0.025, 0.975))
quantile(angles3[,2], probs=c(0.025, 0.975))
quantile(angles4[,2], probs=c(0.025, 0.975))
quantile(angles5[,2], probs=c(0.025, 0.975))
quantile(angles6[,2], probs=c(0.025, 0.975))
quantile(angles7[,2], probs=c(0.025, 0.975))
quantile(angles8[,2], probs=c(0.025, 0.975))





# Here we compare the effect of sex 

sex1 <- AngVecAbsBoot(pls.sel.r1.b1.m[ ,3:32], pls.sel.r1.b1.f[ ,3:32])
sex2 <- AngVecAbsBoot(pls.con.r1.b1.m[ ,3:32], pls.con.r1.b1.f[ ,3:32])
sex3 <- AngVecAbsBoot(pls.sel.r2.b1.m[ ,3:32], pls.sel.r2.b1.f[ ,3:32])
sex4 <- AngVecAbsBoot(pls.con.r2.b1.m[ ,3:32], pls.con.r2.b1.f[ ,3:32])

sex5 <- AngVecAbsBoot(pls.sel.r1.b2.m[ ,3:32], pls.sel.r1.b2.f[ ,3:32])
sex6 <- AngVecAbsBoot(pls.con.r1.b2.m[ ,3:32], pls.con.r1.b2.f[ ,3:32])
sex7 <- AngVecAbsBoot(pls.sel.r2.b2.m[ ,3:32], pls.sel.r2.b2.f[ ,3:32])
sex8 <- AngVecAbsBoot(pls.con.r2.b2.m[ ,3:32], pls.con.r2.b2.f[ ,3:32])

quantile(sex1[,1], probs=c(0.025, 0.975))
quantile(sex2[,1], probs=c(0.025, 0.975))
quantile(sex3[,1], probs=c(0.025, 0.975))
quantile(sex4[,1], probs=c(0.025, 0.975))
quantile(sex5[,1], probs=c(0.025, 0.975))
quantile(sex6[,1], probs=c(0.025, 0.975))
quantile(sex7[,1], probs=c(0.025, 0.975))
quantile(sex8[,1], probs=c(0.025, 0.975))

quantile(sex1[,2], probs=c(0.025, 0.975))
quantile(sex2[,2], probs=c(0.025, 0.975))
quantile(sex3[,2], probs=c(0.025, 0.975))
quantile(sex4[,2], probs=c(0.025, 0.975))
quantile(sex5[,2], probs=c(0.025, 0.975))
quantile(sex6[,2], probs=c(0.025, 0.975))
quantile(sex7[,2], probs=c(0.025, 0.975))
quantile(sex8[,2], probs=c(0.025, 0.975))

## Remind Ian to BCa bootstrap working, to double check the results.





pc.wings <- prcomp(averaged.data[,10:39])
str(pc.wings)
summary(pc.wings)
plot(pc.wings)

# the loadings/rotations
pc.wings$rotation


# This contains the computed PCAs.. to use for your manova
MikeResponsePCA <- pc.wings$x[,1:26]

averaged.data.with.PC <- data.frame(averaged.data, MikeResponsePCA)
# Big model

shape.manova <- manova(MikeResponsePCA ~ Treatment*Replicate*Sex*Block, data = averaged.data)
shape.manova.reduced.1 <- manova(MikeResponsePCA ~ (Treatment  + Replicate + Sex + Block)^3, data = averaged.data)
shape.manova.reduced.2 <- manova(MikeResponsePCA ~ (Treatment  + Replicate + Sex + Block)^2, data = averaged.data)
summary.manova(shape.manova)
summary(shape.manova)

car::Manova(shape.manova)


### Using MCMCglmm to do this.

fmla.wing.LHS <- paste("PC", 1:26, sep="", collapse = " , ")
fmla.wing.PC <- as.formula(paste("cbind(", fmla.wing.LHS, ")", "~", "trait + trait:Treatment  + trait:Replicate + trait:Sex + trait:Block + trait:Treatment:Replicate + trait:Treatment:Sex + trait:Treatment:Block + trait:Replicate:Sex + trait:Replicate:Block + trait:Sex:Block -1"  ))


fmla.wing.PC.reduced <- as.formula(paste("cbind(", fmla.wing.LHS, ")", "~", "trait + trait:Treatment  + trait:Replicate + trait:Sex + trait:Block -1"  ))

prior.model.1 <- list( R=list(V=diag(26), nu=0.05))
fam.test <- rep("gaussian", 26 )
                                 
shape.manova.MCMC <- MCMCglmm(fmla.wing.PC , data = averaged.data.with.PC, 
  rcov=~ idh(trait):units, 
  family = fam.test, prior =  prior.model.1, nitt=4000, burnin=2000)


summary(shape.manova.MCMC)



### linear discriminant functiona analysis ASK CHRIS!!!!
lda.1 <- lda(averaged.data[,10:39], grouping = averaged.data$Sex, method="mle")
#OR...

lda.formula.RHS <- paste("ProcCoord", 1:30, sep="", collapse =" + ")
lda.formula     <- as.formula(paste( "Sex", "~",  lda.formula.RHS))

#  This has issues with the colinearity!
lda.1.alternativeSpecification <- lda(lda.formula, data=averaged.data) # WHY DOES THIS NOT WORK


# teaching stuff for Michael.
# # So to get the value for PC1 for the first observation

# # take the loadings for PC1
# loadings.1 <- pc.wings$rotation[,1]

# # and multiply by the first observation
# observations.all <- as.matrix(averaged.data[, 10:39], ncol=30)

# pc.crap <- observations.all %*% loadings.1
# pc.crap

# MikeResponsePCA[1:6,1]

# plot(pc.crap, MikeResponsePCA[,1])

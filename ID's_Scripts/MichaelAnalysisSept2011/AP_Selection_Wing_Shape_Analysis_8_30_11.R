#### Analysis of the experimental evolution lines after 31&32 generations of selection.
## Michael DeNieu updated 9/17/11
## ID updated 9/20/11

source("/Users/ian/R/RSourceFiles/MultivariateSource_ID.R")
#setwd("/home/Michael/Documents/Predation Wing Size/Selection Differential/Final Text Files")


# For Ian's machine
setwd("/Users/ian/Projects_current/mantid selection/MD_Mantid_Selection_Gen30/")


wings.complete <- read.csv("selection_analysis_wings_predation_lines.csv", h=T)
comment(wings.complete) <- "block represents generation (30 and 31). However Replicate 1 flies and replicate two flies were not measured at the same time (~5 days apart within each generation)"

dim(wings.complete)
head(wings.complete)
tail(wings.complete)
str(wings.complete)
comment(wings.complete)

wings.complete$individual <- as.factor(wings.complete$individual)

# I want to average size and shape values
# Create 
names <- paste("ProcCoord", 2:30, sep = '')
coord.list <- paste(names, names, sep = " = ", collapse=" , ")

# First sort by individual
wings.complete <- wings.complete[order(wings.complete$individual),]
str(wings.complete)
summary(wings.complete)
head(wings.complete)

averaged.data <- with(wings.complete, aggregate(x=list( Centroid_Size=Centroid_Size, Log_Centroid_Size=Log_Centroid_Size, ProcCoord1 = ProcCoord1, ProcCoord2 = ProcCoord2, ProcCoord3 = ProcCoord3, ProcCoord4 = ProcCoord4, ProcCoord5 = ProcCoord5, ProcCoord6 = ProcCoord6, ProcCoord7 = ProcCoord7, ProcCoord8 = ProcCoord8, ProcCoord9 = ProcCoord9, ProcCoord10 = ProcCoord10, ProcCoord11 = ProcCoord11, ProcCoord12 = ProcCoord12,  ProcCoord13 = ProcCoord13, ProcCoord14 = ProcCoord14, ProcCoord15 = ProcCoord15, ProcCoord16 = ProcCoord16, ProcCoord17 = ProcCoord17, ProcCoord18 = ProcCoord18, ProcCoord19 = ProcCoord19, ProcCoord20 = ProcCoord20, ProcCoord21 = ProcCoord21, ProcCoord22 = ProcCoord22, ProcCoord23 = ProcCoord23, ProcCoord24 = ProcCoord24, ProcCoord25 = ProcCoord25, ProcCoord26 = ProcCoord26, ProcCoord27 = ProcCoord27, ProcCoord28 = ProcCoord28, ProcCoord29 = ProcCoord29, ProcCoord30 = ProcCoord30), by=list(individual=individual, Survival=Survival, Treatment=Treatment, Replicate= Replicate, Block=Block, Sex=Sex, Survive=Survive), FUN=mean))
str(averaged.data)
summary(averaged.data)

head(averaged.data)

check <- duplicated(averaged.data[,1])
summary(check)


averaged.data <- averaged.data[order(averaged.data$individual),]

# 

# PLS function from Morphometrics in R, in the Dworkin lab multivariate source file. Has been modified


### Test with whole data set (to make sure function is working)

PLS(averaged.data[,10:39], averaged.data[, 9])

# Let's compute the angles between vectors for sets of the data


PLS.out.C.R1 <- PLS(averaged.data[averaged.data$Treatment == "CON" & averaged.data$Replicate =="R1" ,10:39], 
   averaged.data[averaged.data$Treatment == "CON" & averaged.data$Replicate =="R1", 9])
   
PLS.out.SEL.R1 <- PLS(averaged.data[averaged.data$Treatment == "SEL" & averaged.data$Replicate =="R1" ,10:39], 
   averaged.data[averaged.data$Treatment == "SEL" & averaged.data$Replicate =="R1", 9])   
   
   
# To look at the angle between vectors, use the ang.vec.abs() function  

ang.vec.abs(PLS.out.C.R1[3:32], PLS.out.SEL.R1[3:32]) 



# Now we want to write a resampler that does pairs (row level) bootstrapping to generate CIs for 

PLS.resampler <- function(x = your.data.frame, block1, block2) {
	# x is the data frame that you have all of the data in for the PLS analysis
	# block1 are the columns in x for the first block of variables
	# block2 are the columns in x for the second block of variables
	
	# This does a pairs resample
	x.resample <- x[sample(nrow(x), nrow(x), replace=T), ]
	PLS(M1 = x.resample[, block1], M2 = x.resample[ , block2], p2 = 1)
	
}

# Now we run 1000 replicates
PLS.boot.1000 <- t( replicate(1000, PLS.resampler(x = averaged.data, block1 = 10:39, block2 = 9)))


quantile(PLS.boot.1000[,1], probs=c(0.025, 0.975))


pc.wings <- prcomp(averaged.data[,10:39])

summary(pc.wings)

# the loadings/rotations
pc.wings$rotation


# This contains the computed PCAs.. to use for your manova
MikeResponsePCA <- pc.wings$x[,1:26]


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
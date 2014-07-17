# bootstrap CI - pretty

### Data in
setwd("/Users/ian/R/R scripts/Dll data/") 
dll.data = read.csv("dll.txt", header=TRUE)   #data frame input


dll.data$temp <- factor(dll.data$temp)
dll.data$replicate <- factor(dll.data$replicate)
dll.data$genotype <- relevel(dll.data$genotype, "wt")  # Setting the wild-type (wt) as reference
dll.data <- na.omit(dll.data)  # I am only doing this to look at some diagnostic plots, which get all bothered by the missing data.
dll.data$tarsus.scaled <- dll.data$tarsus - mean(dll.data$tarsus)
################


regression.1 <-lm(SCT ~ tarsus.scaled, data=dll.data)
par(mfrow=c(1,1))
plot(SCT ~ tarsus.scaled, data=dll.data, xlim=range(dll.data$tarsus.scaled))
abline(regression.1)


# bootstrap for pretty CI curves
# values to predict on
new_tarsus <- data.frame(tarsus.scaled = seq(from=range(dll.data$tarsus.scaled)[1], to=range(dll.data$tarsus.scaled)[2], length=1000) )   # Makes a new data frame for the x values to predict on


nitt = 10000
predicted.boot <- matrix(NA, nrow=nitt, ncol=1000)
for(i in 1:nitt) {
	dat.boot <- dll.data[sample(nrow(dll.data), nrow(dll.data),replace=T),]
	model.boot <- lm(SCT ~ tarsus.scaled, data=dat.boot)
	predicted.boot[i,] <-predict(model.boot, new_tarsus, interval="none")

}



# quick check that we get the lines out. We add these onto the plot
for(i in 1:nitt) {
	lines(predicted.boot[i,] ~ new_tarsus$tarsus.scaled, col="grey")
}


# but what we want is the quantiles
lower <- apply(X=predicted.boot, MARGIN=2, FUN=quantile, probs=c(0.025))
upper <- apply(X=predicted.boot, MARGIN=2, FUN=quantile, probs=c(0.975))

par(mfrow=c(1,1))
plot(SCT ~ tarsus.scaled, data=dll.data, xlim=range(dll.data$tarsus.scaled))
abline(regression.1, lwd=2)

# plot the lines from the quantiles of the bootstrap.
lines(x = new_tarsus[,1], y = lower, lwd=3, lty=2, col="grey")
lines(x = new_tarsus[,1], y = upper, lwd=3, lty=2, col="grey")


# let's add on classic CI's

predicted.model.1  <-predict(regression.1, new_tarsus, interval="confidence")
#lines(x = new_tarsus[,1], y = predicted.model.1[,1], lwd=4) # this would be the same as abline(model.1)
lines(x = new_tarsus[,1], y = predicted.model.1[,3], lwd=3, lty=3, col="red")
lines(x = new_tarsus[,1], y = predicted.model.1[,2], lwd=3, lty=3, col = "red")


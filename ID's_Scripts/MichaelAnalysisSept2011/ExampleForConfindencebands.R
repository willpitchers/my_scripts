### Block of code for CIs


par(mfrow=c(1,2))
with(data.legs.pca, plot(DaysSurvived ~ PC1, col=densCols(DaysSurvived, PC1), pch=16, xlab = " PC1 (measure of overall body size)"))
with(data.legs.pca, lines(lowess(x=PC1, y=DaysSurvived, iter=10, f=0.9), col="grey", lwd=2))
# with(data.legs.pca, lines(smooth.spline(x=PC1, y=DaysSurvived), col="blue", lwd=2))

new.dat <- data.frame(PC1=seq(-0.4, 0.4, 0.01))
pred.data <- predict(model.pc1, new.dat, interval="confidence")
lines(x=new.dat[,1], y=pred.data[,1], col="black", lwd=4)
lines(x=new.dat[,1], y=pred.data[,2], col="black", lwd=4, lty=2)
lines(x=new.dat[,1], y=pred.data[,3], col="black", lwd=4, lty=2)



# Or with the coloured confidence bands
# THis may look nicer
with(dll.data[dll.data$temp==25,], plot(SCT~tarsus, pch=16, col=c("green", "black")[dll.data$genotype], 
  type = "n", ylim= c(8,17),
  ylab = " # of Sex Comb Teeth",
  xlab = "Tarsus Length",
  main = expression(paste("Scaling relationships between SCT and tarsus lengths across ", italic(Drosophila), " genotypes" )) ))
polygon(x = c(new[,1], rev(new[,1])), y = c(pred.dll[,2], rev(pred.dll[,3])), col="green")
lines(x = new[,1], y = pred.dll[,1], lwd=2, lty=1)

# And for the wild-type
lines(x = new[,1], y = pred.wt[,1], lwd=3) # this would be the same as abline(model.1)
lines(x = new[,1], y = pred.wt[,3], lwd=2, lty=2)
lines(x = new[,1], y = pred.wt[,2], lwd=2, lty=2)


# and add the points back on
with(dll.data[dll.data$temp==25,], points(jitter(SCT)~tarsus, pch=16, cex=0.75, col=c("green", "black")[dll.data$genotype] ))

legend(x=0.135, y=15, 
  legend = c(expression(italic("Distal-less/+")), "wild-type"),
  col= c("green", "black"), pch=16, cex=1.5)
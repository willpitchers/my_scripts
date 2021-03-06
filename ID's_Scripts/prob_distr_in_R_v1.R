# Last updated Sept21st 2010. Ian Dworkin

# Working with probability distributions and density functions in R - the basics
# At the bottom of the script is a list of the names of most of the common probability distributions you might use in R (and what library they are in if they are not part of base R)


### Getting our data set in
setwd("/Users/ian/R/R scripts/Dll data/") 
dll.data = read.csv("dll.csv", header=TRUE)   #data frame input
dll.data <- na.omit(dll.data)


#######
# Some basic mathematical functions that sometimes come up when working with probability distributions (i.e for "counting")
factorial(x)  # Factorial i.e. x*(x-1)*(x-2)*....*(x-n+2)*1
gamma(x)  # gamma function, generalization of factorials. NOT THE GAMMA PROBABILITY DISTRIBUTION
choose(n,k)  # n choose k
beta(a,b)  # beta function. NOT THE BETA PROBABILITY DISTRIBUTION
##############




# for most of the standard distributions (normal, binomial, gamma, F etc.) there are 4 functions (illustrated with Gaussian/Normal)

# dnorm, pnorm, qnorm, rnorm

# dnorm - the density function
# if you want the height of the density function (or the probability for discrete functions) of getting a particular value (or on a particular interval for continuous distributions) given a particular set of parameter values (i.e mean=0, sd=1), you would use dnorm

# i.e
dnorm(0, mean=0, sd=1) # or alternatively....
dnorm(0,0,1) # what is the height of the density function (proportional to the probability) on the interval including zero (how big is the interval?) for a normal distribution with mean 0 and sd 1.

dnorm (2,0,1) # not surprisingly the "relative" probability is much less for a value of 2, for the same distribution as above


# we can look more thoroughly at the density function 
par(mfrow=c(2,1))
curve(dnorm(x, mean=0, sd=1), from=-5, to=10) #  f(x)~ N(0,1)
curve(dnorm(x,mean=5, sd=1.5), from=-5, to=10) #  f(x)~ N(5,1.5)

# Instead of wanting to known a point (or interval) probability, what if we want to know what the cumulative probability is for a particular value, with respect to a particular distribution (i.e. what is the probability).

pnorm(2, mean=0,sd=1, lower.tail=T) # what is the probability that the value is less than 2 for f(x)~N(0,1) # Thus the probability that an observation is sampled from a distribion  ~ N(0, 1) is 2 or less is ~98%

pnorm(2, 0, 1, lower.tail=F)  # similarly to see prob that the value is greater than 2
  # Or from the other side, the probability that an observation is sampled from a distribution of ~ N(0,1) is 2 or greater is ~ 2.3%. Hopefully 

# You use this all the time, whenever you calculate a p-value
model.1 <- lm(SCT ~ genotype, data=dll.data)
anova(model.1)

# Let's generate the p-value directly from the f distribution
pf(q = 50.316, df1 = 1, df2 = 1916, lower.tail=F) # lower.tail=F since we want to know the probability of getting this value (50.316), or something more extreme.



# You can use also use the "p"dist class of functions to calculate the probability in an interval for a continous probability function.  
# For instance if you want to calculate the probability of observing a value between 10-15 from a normal distribution with mean=10 and sd=3.3 
pnorm(15,10, 3.3) - pnorm(10,10, 3.3) # This calculates the probability from the lower tail.

par(mfrow=c(2,1))
curve(dnorm(x), from=-5, to=5, ylim=c(0,1)) #  f(x)~ N(0,1)
curve(pnorm(x), from=-5, to=5, add=T, col="red")


# The recipricol function (for utility) will provide a value for "x", at a given probability value. i.e. at the 95% percentile of the distribution what would the parameter value be given a particular f(x)

qnorm(0.95,0,1, lower.tail=T) # 1.644854
# not surprising if you put this value back in pnorm you will get back the prob (0.95)
pnorm(1.644854,0,1)

#Let's think about this from a "p-value" centric point of view
# If we knew we wanted to have an alpha=0.05, for an F distribution with df1=1 (numerator) and df2=49 (denominator), what would be the F value "threshold" for this alpha
qf(p=0.95, df1 = 1, df2 = 49, lower.tail=T) # Thus an empirical F > 4.04 would suggest to us that we should reject the null hypothesis.

 # the main utility of pnorm and qnorm are for looking at potential extreme values compared to the theoretical distribution, and the p"put your dist here" and q"put your dist here" are particularly useful for comparing observed versus the predicted values from the distributions of test statistics.
 
 # compose examples for qf, pf and df for fdist
 
 
 
 
# perhaps the most generally useful function in my mind is the random number generator based on particular distributions (good for simulations, power analyses etc...)

par(mfrow=c(1,1))
rran <- rnorm(500,0,1) # generate 500 random samples from the f(x)~N(0,1)
hist(rran, freq=F, xlim=c(-3,3), ylim=c(0,0.5))  # histogram of the 500 randomly generated samples
curve(dnorm(x,0,1), from=-3, to=3, col="red", add=T) # just to add the theoretical distribution onto the random samples!!!!


# From the lecture... here is the code for the figures I made....
#  normal distribution (to induce a boundary condition)
par(mfrow=c(3,1))
x <- rnorm(1000, 2.0,2) # N~(1,1)
hist(x, xlim=c(0,12)) # this shows a "truncated" normal distribution where values below 0 are not used
# Zero inflated normal by inducing a boundary condition
y <- ifelse(x<0, 0, x) # zero inflated normal
hist(y, xlim=c(0,12))

#  Similar figure using a binomial. Which is better?
q <- rbinom(1000, 100, 0.005)
hist(q, xlim=c(0,12))



#assorted and sundry
# what does a bimodal normal look like
par(mfrow=c(2,1))
curve(dnorm(x, 5, 1), 0, 15)
curve(dnorm(x, 8, 1), 0, 15, add=T, col="red")

curve((dnorm(x, 8, 1) + dnorm(x, 5,1)), 0, 15)

# what happens to a t distribution as sample size increases?
curve(dt(x, 3), -5, 5, ylim=c(0,0.5))
curve(dt(x, 20), -5, 5, col="red", add=T)
curve(dt(x, 2100), -5, 5, col="blue", add=T)



# A couple of notes on using the r"dist" functions (i.e. rnorm)
# there are often a number of ways of specifying the equivalent vector of random numbers 

# For instance given a vector x
x <- seq(1:100)

# say we want add some random variation to x (for a simulated regression problem for instance)

# you could do this
set.seed(1); y1 <- x + rnorm(length(x),0, 3)  # set.seed is not nescessary, I am just using it to pull the same set of random numbers
  # here for each value of x, we are adding a single random number from a normal distribution with mean 0, and sd=1 to create the vector y1. If R was not a vectorized language, then you would use an approach like this with a loop (for/while) to generate such a vector
 
 # the other way to get an equivalent result (in this case the exact same result since I am setting the seed for the random number generator)
set.seed(1); y2 <- rnorm(length(x),x, 3)
  # While the results are equivalent, what we are doing is a little bit different. Here we are creating a vector y2, which has has number of rows equal to x (length(x)), and with a different mean for each value of the vector, starting at the first value of x (2) all the way to the final value in x (20), all of the values of y will have a standard deviation of 1. This is considered the vectorized version of this.

# this shows that they are identical  - both seem to take similar time (based on system.time)
y1
y2

par(mfrow=c(2,1))
plot(y1~x)
plot(y2~x)
# Normally you would not use set.seed() unless you had particular reasons to do so ( you want to use the same set of randomly generated numbers for comparison beween simulations)


#   Looking at the distribution for discrete data

# If you try something like
curve(dpois(x,lambda=11), 0, 20)
  
# it just looks funky. Not surprising since it is computing the value of the function at non-integer values which make no sense for a poisson.

# this is better
x.discrete <- seq(0,20, by=1)
plot(dpois(x.discrete,lambda=11), type="h", lwd=5) # a bit better
barplot(dpois(x.discrete, 10)) # My personal preference

sum(dpois(x.discrete,lambda=11)) # pretty close to one as expected for a discrete distribution 
  
  
############### Some questions raised from lecture 1 on probability  
# Why can the probablities sum up to greater than 1 for a probability density function if the area under the curve =1

a1 <- seq(1,10, 0.1)
sum(dnorm(a1,5,1))

a2 <- seq(1,10, 1)
sum(dnorm(a2,5,1))


# Same distribution, sampling at different intervals. To get the absolute (instead of relative probabilities) divide out by this sum.


x <- rnorm(100000, 5,1) # random variables from N(5,1)
plot(density(x),xlim=c(0,10), ylim=c(0,0.5), col="blue", type="p", lwd=0.5)
hist(x, freq=F, add=T)
curve(dnorm(x,5,1), 0,10, add=T, col="red", lwd=2)

# but wait.....
z1 <- seq(0,10, 1)
plot(dnorm(z1,5,1) ~ z1, col="blue", lwd=2)
curve(dnorm(x,5,1), 0,10, add=T, col="red", lwd=2)

z2 <- seq(0,10, 0.1)
plot(dnorm(z2,5,1) ~ z2, col="blue", lwd=2)
curve(dnorm(x,5,1), 0,10, add=T, col="red", lwd=2)

# but
sum(dnorm(z1,5,1))
sum(dnorm(z2,5,1))

# Can you explain what R is doing?

z1.table <- cbind(z1, dnorm(z1,5,1))
z2.table <- cbind(z2, dnorm(z2,5,1))

# take a look at the tables, can you see what R is doing.....



# R is using the same interval size to do its computation.... That is why 10X then number of pnts gives 10X greater probability.


# What histograms show in R
rm(x)
x <-rnorm(10000,5,1)
crap <- hist(x, plot=F)
crap$counts
crap$counts/sum(crap$counts)
par(mfrow=c(3,1))
hist(x, main= "The raw counts")
hist(x, freq=F, main="Scaled so that area under 'curve'=1")
barplot(crap$counts/sum(crap$counts), names.arg = crap$mids, main="proportions = counts/N")





#### Below is more of reference sheet to find distributions, and for conguacy.

# distributions (add your own d,p,q,r)
# norm      - Normal/Gaussian (i.e. dnorm, rnorm)
# binom     - Binomial  (i.e dbinom, rbinom)
# pois      - Poisson
# exp       - Exponential (Special case of the gamma with alpha=1)
# norm      - Normal/Gaussian
# gamma     - Gamma
# nbinom    - Negative binomial
# beta      - Beta
# lnorm     - log normal
# f         - F distribution
# chisq     - chi square distribution (special case of gamma with alpha=df/2 & beta=0.5 (rate in R))
# t         - T (student) distributions
# weibull   - Weibull
# cauchy    - Cauchy
# geom      - Geometric (Geometric is a special case of a Negative Binomial with alpha=1)
# hyper     - Hypergeometric
# multinom  - Multinomial
# logis     - Logistic
# signrank  - Distribution of the Wilcoxon Signed rank statistic
# wilcox    - Wilcoxon Rank Sum Statistic
# unif      - uniform distribution (A uniform on [0,1] is a special case of a beta(alpha=beta=1))
# mvrnorm   - Multivariate Normal Distribution (MASS library)
# dirichlet - dirichlet distribution (gtools library)
# wish      - Wishart distribution (MCMCpack library)
# iwish     - Inverse Wishart (MCMCpack library). Also see IW in MCMCglmm
# invgamma  - Inverse Gamma (MCMCpack). Also see igamma in pscl library.
# invchisq  - (Scaled) Inverse Chi Squared disrtibution. You can also use iwish()

# quick reference for conjugacy ()
# As a reminder The prior and the posterior take the same form.
# i.e. So if the we use the poisson to model the likelihood, and use the gamma for the prior, the posterior will be gamma.
# i.e. If we use the binomial to model the likelihood p(Data|model), and use the beta for a prior, then the posterior p(model|data) will also be prior.

# discrete likelihoods
# Binomial likelihood - the beta distribution is the conjugate prior
# Poisson likelihood - the gamma distribution is the conjugate prior
# multinomial likelihood - the dirichlet distribution is the conjugate prior (dirichlet is the multivariate extension of the beta)

# Continuous likelihoods
# Normal is the conjugate prior for the mean for the likelihood of a normal distribution with fixed variance
# Gamma is the conjugate prior for the INVERSE of the variance for a normal distribution.
# Thus the inverse gamma is the conjugate prior of the variance for a normal distribution.
# Gamma is the conjugate prior for the mean parameter for the Poisson.


# Multivariate (continuous and discrete)
# multinomial likelihood - the dirichlet distribution is the conjugate prior (dirichlet is the multivariate extension of the beta)
# Wishart is the conjugate prior for the INVERSE covariance matrix (Wishart is the multivariate extension of the gamma)
# Inverse Wishart is the conjugate prior for the covariance matrix.



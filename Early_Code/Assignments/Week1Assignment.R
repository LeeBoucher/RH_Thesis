################################################################################
# Program: Week1Assignment
# Project: Boucher/Maxwell Senior Thesis
# Description: Introduction to MC methods.
#
# Author: Eric Reyes
# Date: 10 Sep 2013
#
# Notes:

################################################################################
# Problem 1:
#   Estimate the value of an integral using simulation techniques.

# Step 0: Set Seed
#   As simulation relies on random generation, repeating the simulation will
#   lead to different results (though hopefully similar). To make the results
#   repeatable, it is a good idea to set a 'seed' which basically tells R
#   where to start in the generation process so that the same data can be 
#   generated.
#
#   A seed can be any integer. I typically choose a date so that I am not always
#   using the same seed, but it does not really matter.

set.seed(20130916)


# Step 1: Generate Data
#   Define the sample size (n) and generate n Unif(0,1.5) random variates.
#   Hint: use the runif command to generate random variables.

n <- 10000
X <- runif(n,min=0,max=1.5)


# Step 2: Estimate
#   Plug each X into the appropriate function and take the mean in order to
#   estimate the integral.

estimate <- mean(1.5*exp(exp(X)))



################################################################################
# Problem 2:
#   Simulate the sampling distribution of a sum of exponentials.

# Step 0: Set Seed
#   If the entire program is run from start to finish, the above seed is
#   sufficient.

# Step 1: Generate Data
#   Generate m=1000 datasets, each consisting of n=40 values. It is easiest to
#   store the results in a large matrix where each row represents a sample. Use
#   rexp() to generate the data.

n <- 40
m <- 10000
lambda <- 0.2

X <- matrix(rexp(n*m,rate=lambda),nrow=m,ncol=n)


# Step 2: Compute Sample Mean
#   While 'for loops' in R are possible, they can be inefficient. It is
#   generally (though not always) better to use an apply statement of some kind.
#   Using apply(), compute the sample mean of each row since each row of X
#   represents a different sample.

x.bar <- apply(X,1,mean)

# Step 3: Create Histogram
#   Construct a histogram and then overlay the true density. In order to do
#   that, we will use the hist() function with the 'freq' parameter set to 
#   FALSE so that the density version is created. Then, we can use R's built-in
#   densities to overlay.

# Histogram
#   Use hist() command with freq=FALSE.
hist(x.bar,freq=FALSE,xlab="Sample Mean",
     main="Sampling Distribution of Mean of Exponentials")


# Overlay Density
#   The curve() function with the parameter 'add' set to TRUE will add a curve
#   to an existing plot. R has built-in density functions. We can use dgamma()
#   to construct the density function.
curve(dgamma(x,shape=n,scale=(lambda*n)**(-1)),add=TRUE)



################################################################################
# Problem 3:
#   Estimate bias in an estimator for variance of exponential.

# Step 0: Set Seed

# Step 1: Generate Data
#   Use rexp() and matrix() to generate m=1000 samples of size n=30.

n <- 30
m <- 10000

X <- matrix(rexp(n*m,rate=lambda),nrow=m,ncol=n)


# Step 2: Compute Estimator
#   Use apply() to compute the mean for each sample and then square it to get
#   the estimator for the variance.

theta.hat <- apply(X,1,mean)**2


# Step 3: Estimate Bias
#   Take the mean of the estimator above and subtract off the true value of the
#   parameter (which is known to be 25 in this case) to get bias.

bias <- mean(theta.hat - 25)
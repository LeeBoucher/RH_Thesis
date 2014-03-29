################################################################################
# Program: Week2
# Project: Boucher Senior Thesis
# Description: Conduct a simulation study comparing the mean and median.
#
# Author: Eric Reyes
# Date: 18 Sep 2013
#
# Notes:

################################################################################
# Problem 1:
#   Write a function that, given two vectors y and x, returns the LS
#   estimate of the slope from a regression of y on x.

b.fct <- function(y,x){
  lmfit = lm(formula = y ~ x)
  slope = lmfit$coefficients[[2]]
}

################################################################################
# Problem 2:
#   Generate a series of response vectors from a linear model in which the
#   error term does not follow a normal distribution.

# Step 0: Set Seed and Constants
set.seed(20130920)

m <- 1000
n <- 10
x <- seq(n)
                                                        
# Step 1: Generate Error
#   Generate m replications of the error term, where the error term is a sample
#   of size n from an exponential distribution with rate 1.

E <- matrix(rexp(m*n, rate=1),nrow=m,ncol=n)

# Step 2: Create the Responses
#   Form the linear model y = 5x + error in order to compute the responses.

Y <- t(5*matrix(seq(n),nrow=n,ncol=m)) + E

################################################################################
# Problem 3:
#   Determine the sampling distribution of the least squares estimator of the 
#   slope.

# Step 1: Esimate the Slope
#   For each replicate dataset, estimate the slope from the regression line of
#   y on x.

slopes <- apply(Y,1,function(y) b.fct(y,x))

# Step 2: Examine Sampling Distribution
#   Look at a histogram of estimates to get a feel for the sampling distribution
#   of the slope.

hist(slopes,freq=FALSE,xlab="Sample slopes",
     main="Sample slopes of linear relationship with exponentially distributed error")

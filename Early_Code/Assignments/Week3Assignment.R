################################################################################
# Program: Week3
# Project: Boucher Senior Thesis
# Description: Conduct a simulation study comparing full least squares to 
# marginal correlation for variable ordering. 
#
# Author: Kimberly Boucher
# Date: 1 Oct 2013
#
# Notes:

n <- 20 # number of y_i's
p <- 5 # number of covariates
m <- 1 # number of datasets

beta <- c(4, 2, 1, 0.5, 0)
beta0 <- rep(0, p)

set.seed(20131001)

sample_data.generate <- function(p, n, m, b, b0) {
  X <<- matrix(rnorm(n*p, mean=0, sd=1),nrow=p,ncol=n)
  Xt <<- t(X)
  e <<- rnorm(p, mean=0, sd=sqrt(3))
  y <<- b0 + t(X) %*% b + e
}

sample_data.generate(p, n, m, beta, beta0)

################################################################################
# Problem 1:
#   Write a function that that takes three parameters - a vector y (the 
#   response), a matrix X of the covariates - and returns an ordering of the 
#   variables. The third parameter of the function dictates whether that order 
#   is based on the least squares fit or based on the marginal correlations.

fit.order <- function(y,X,method){
  if ((method == "ls") | (method == "lsf")) {
    corr_method <- "kendall"
  } else if (method == "marginal_corr") {
    corr_method <- "pearson"
  }
  correlations <- cor(X, y, use="everything", method=corr_method)
  correlations <- abs(correlations)
  #print("correlations = ");print(correlations) # debugging
    #cat("correlations = ", correlations, "\n") # debugging
    #cat(sprintf("correlations = %f\n", correlations)) # debugging
  order <- order(-correlations)
    #print("order = ");print(order) # debugging
    #print(cat("order = ", order)) # debugging
  #cat("order = ", order) # debugging
    #sprintf("order = ", order) # debugging
    #message("order = ", order) # debugging
  #list <- list(cor=correlations, ord=order)
}

#marginal <- fit.order(y,Xt,"marginal_corr")$ord
marginal <- fit.order(y,Xt,"marginal_corr")
ls <- fit.order(y,Xt,"ls")

################################################################################
# Problem 2:
#   Generate m = 1000 replicate datasets, each with n = 15 observations involving 
#   a response vector (determined by the above mentioned model) and 5 covariate
#   vectors (which can be stored as a matrix X). For each replicate dataset, fit 
#   the model using least squares and record the ordering and determine the 
#   marginal correlations and record the ordering.

m <- 1000
n <- 15

ls <- matrix(rep(0, m*p), nrow=m, ncol=p)
marginal <- matrix(rep(0, m*p), nrow=m, ncol=p)

for (i in 1:m) {
  sample_data.generate(p, n, m, beta, beta0)
  marginal[i,] <- fit.order(y, Xt, "marginal_corr")
  ls[i,] <- fit.order(y, Xt, "ls")
}

################################################################################
# Problem 3:
#   For the orderings generated above, compute Kendall's tau between the ordering
#   and the order of the true coefficients.

true_ordering = seq(5)
#kendall_tau_marginal <- apply(marginal, 1, function(x) cor(x, true_ordering, 
#                                                           use="everything", 
#                                                           "kendall"))
#kendall_tau_ls <- apply(ls, 1, function(x) cor(x, true_ordering, 
#                                               use="everything", "kendall"))
kendall_tau_marginal <- cor(t(marginal), true_ordering, use="everything", "kendall")
kendall_tau_ls <- cor(t(ls), true_ordering, use="everything", "kendall")

################################################################################
# Problem 4:
#   Make a boxplot of the tau values for each of the two methods. Does one method
#   appear to give a better ranking for this problem?

measure <- c(kendall_tau_marginal, kendall_tau_ls)
category <- c(0*kendall_tau_marginal, rep(1, length(kendall_tau_ls)))
boxplot(measure, category)

################################################################################
# Program: Simulation_2013_10_01
# Project: Boucher Senior Thesis
# Description: Conduct a preliminary simulation study for CLS ordering vs SIS
#
# Author: Kimberly Boucher
# Date: 1 Oct 2013
#
# Notes:

################################################################################
# Set initial values, define functions for generating data in linear model

n <- 20 # number of y_i's
p <- 5 # number of covariates
m <- 1000 # number of datasets

set.seed(20131001)

# TODO: make Beta and Beta0 parameters
coefficient_betas.generate <- function(p){
  Beta <<- c(4, 2, 1, 0.5, 0)
  Beta0 <<- rep(0, p)
}

# TODO: make parameters for the distribution for x and e
sample_data.generate <- function(p, n, xd, ed) {
  coefficient_betas.generate(p)
  X <<- matrix(rnorm(n*p, mean=0, sd=1),nrow=p,ncol=n)
  Xt <<- t(X)
  e <<- rnorm(p, mean=0, sd=sqrt(3))
  y <<- Beta0 + t(X) %*% Beta + e
}

sample_data.generate(p, n, N, N)

################################################################################
# Define function to get ordering

fit.order <- function(y,X,method,print=FALSE){
  if ((method == "ls") | (method == "lsf")) {
    correlations <- cor(X, y, use="everything", method="kendall")
    order <- order(-correlations)
  }
  else if ((method == "marginal") | (method == "marginal_corr")) {
    correlations <- cor(X, y, use="everything", method="pearson")
    order <- order(-correlations)
  }
  else if((method == "CLS") | (method == "cls")) {
    # TODO: implement CLS order
    
  }
  else if(method == "SIS") {
    # TODO: check if SIS same as marginal, else implement SIS
    k <- 
    ridge_X_kI <- Xt %*% X + k * diag(n)
    SIS_X_kI <- k * ridge_X_kI
  }
  else if((method == "iter_CLS") | (method == "iter_cls") | (method == "ICLS")) {
    # TODO: implement CLS order
  }
  else if((method == "iter_SIS") | (method == "ISIS")) {
    # TODO: check if SIS same as marginal, else implement SIS
  }
  if (print){
    print("correlations = ");print(correlations) # debugging
    cat("order = ", order) # debugging
  }
  results <- list(cor=correlations, ord=order)
}

################################################################################
# Compare orderings to true ordering by using the Kendall tau

orderings.compare_kendall_tau <- function(m, p, n) {
  ls <- matrix(rep(0, m*p), nrow=m, ncol=p)
  marginal <- matrix(rep(0, m*p), nrow=m, ncol=p)
  
  for (i in 1:m) {
    sample_data.generate(p, n, N, N)
    marginal[i,] <- fit.order(y, Xt, "marginal_corr")$ord
    ls[i,] <- fit.order(y, Xt, "ls")$ord
  }
  
  true_ordering = seq(p)
  kendall_tau_marginal <- cor(t(marginal), true_ordering, use="everything", "kendall")
  kendall_tau_ls <- cor(t(ls), true_ordering, use="everything", "kendall")
  
  measure <- c(kendall_tau_marginal, kendall_tau_ls)
  category <- c(0*kendall_tau_marginal, rep(1, length(kendall_tau_ls)))
  boxplot(measure, category)
  
}

orderings.compare_kendall_tau(m, p, n)

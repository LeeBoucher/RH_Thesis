################################################################################
# Program: Simulation_2013_11_05_functions
# Project: Boucher Senior Thesis
# Description: Conduct a preliminary simulation study for CLS ordering vs SIS
#
# Author: Kimberly Boucher
# Date: 22 Oct 2013
#
# Notes:

################################################################################

library(mvtnorm)
library(Matrix)

# Set initial values, define functions for generating data in linear model

initialize_values <- function(seed, n, m, p) {
  default_seed <- 20131105
  default_n <- 60
  default_m <- 500
  default_p <- 1000
  
  if(missing(seed)) { seed <- default_seed }
  if(missing(n)) { n <- default_n }
  if(missing(m)) { m <- default_m }
  if(missing(p)) { p <- default_p }
  
  set.seed(seed)
  n <<- n # number of y_i's/observations
  m <<- m # number of replicate data sets
  p <<- p # number of predictors
  corr <<- matrix(data=NA, nrow=p, ncol=m)
  varcov <<- varcov.generate(p=p)
  
  y <<- rep(NA, n)
  e <<- rep(NA, n)
  beta1 <<- rep(NA, p)
  beta0 <<- rep(NA, p)
  X <<- matrix(NA, nrow=n, ncol=p)
}

# initialize_default_values <- function() {
#   set.seed(20131105)
#   n <<- 60 # number of y_i's/observations
#   m <<- 500 # number of replicate data sets
#   p <<- 1000 # number of predictors
#   corr <<- matrix(data=NA, nrow=p, ncol=m)
#   varcov <<- varcov.generate(p=p)
#   
#   y <<- rep(NA, n)
#   e <<- rep(NA, n)
#   beta1 <<- rep(NA, p)
#   beta0 <<- rep(NA, p)
#   X <<- matrix(NA, nrow=n, ncol=p)
# }

varcov.generate <- function(p, blocks, default_c) {
  if(missing(default_c)) { default_c <- 0.6 }
  if(missing(blocks)) { blocks = c(default_c, p) }
  c <- matrix(data=c(blocks), ncol=2, byrow=T) # correlation of predictors in and size of each block
  if(sum(c[,2]) < p) {
    b_default <-c(default_c, p - sum(c[,2]))
    blocks <- c(blocks, b_default)
    c <- matrix(data=blocks, ncol=2, byrow=T)
  }
  varcov <- matrix(bdiag(apply(c, 1, function(x) {matrix(data=rep(x[1],x[2]**2), nrow=x[2]) + diag(rep(1-x[1], x[2]))})), nrow=p)
}

tau_weights <- function(w) {
  if(missing(w)) {
    tau <- 0.5;
    # case where all weights equal to 1 - least squares is when tau=1
  }
}

CLS <- function(y, X, tau) {
  if(missing(tau)) {  tau <- tau_weights()  }
  X <- scale(X)
  y <- scale(y)
  beta.hat <- solve(tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))) %*% t(X) %*% y
  # cheaper inverse computation for non-small cases using singular value decomposition as in lm.ridge
  # maybe also possibility to use QR; not as good because easier to update matrix of which inverse is being taken
  
}


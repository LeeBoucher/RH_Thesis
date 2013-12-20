################################################################################
# Program: Simulation_functions
# Project: Boucher Senior Thesis
# Description: Conduct a preliminary simulation study for CLS ordering vs SIS
#
# Author: Kimberly Boucher
# Date: 
#
# Notes:

################################################################################

# library(mvtnorm)
library(Matrix)

CLS <- function(y, X, tau) {
  if(missing(tau)) { tau <- tau_weights() }
  X <- scale(X)
  y <- scale(y)
  beta.hat <- solve(tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))) %*% t(X) %*% y
  # cheaper inverse computation for non-small cases using singular value decomposition as in lm.ridge
  # maybe also possibility to use QR; not as good because easier to update matrix of which inverse is being taken
}

tau_weights <- function(w) {
  if(missing(w)) {
    tau <- 0.5;
    # case where all weights equal to 1 - least squares is when tau=1
  }
  
}

distance_corr <- function() {
  
}


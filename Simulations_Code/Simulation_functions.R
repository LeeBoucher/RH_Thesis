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
# library(energy) # TODO: go back to this; temporarily using the source copy to not install library on remote machine
source("C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\OtherPeoplesCode\\energy.R")


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

distance_corr <- function(y,X) {
  # helpful reference wikipedia page: http://en.wikipedia.org/wiki/Distance_correlation#Distance_correlation
  # currently using functions from the "energy" package
#   dcs <- DCOR(y=y, x=X)
  dcs <- DCOR(y=y, x=X, index=dcorr_index)
  dist_corr <- dcs$dCov / sqrt( dcs$dVarX * dcs$dVarY)
}

weighted_kendall_tau <- function(beta.hat, beta) {
  
}

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
library(MASS)
# library(energy) # TODO: go back to this; temporarily using the source copy to not install library on remote machine
source(paste(directory_path, paste("OtherPeoplesCode","energy.R", sep=separator), sep=separator))

CLS_Pearson_Objective_Matrix <- function(X, tau) {
  if(missing(tau)) { tau <- tau_weights() }
  obj_matrix <- tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))
#   inv_obj_matrix <- chol2inv(chol(obj_matrix))
}

CLS_Pearson_Objective_Inv_Matrix <- function(X, tau) {
  inv_obj_matrix <- chol2inv(chol(CLS_Pearson_Objective_Matrix(X=X)))
}

CLS_Pearson_coefficients_for_y <- function(X) {
  inv_obj_matrix_times_XT <- CLS_Pearson_Objective_Inv_Matrix(X=X) %*% t(X)
#   inv_obj_matrix_times_XT <- CLS_Pearson_Objective_Matrix(X=X) %*% t(X)
}



CLS_Distance_Objective_Matrix <- function(X, tau) {
  if(missing(tau)) { tau <- tau_weights() }
  obj_matrix <- tau * Rxx_dist(X) + (1-tau)*diag(rep(1, p)) # http://en.wikipedia.org/wiki/Covariance_matrix for Rxx
  #   inv_obj_matrix <- chol2inv(chol(obj_matrix))
}

CLS_Distance_Objective_Inv_Matrix <- function(X, tau) {
  inv_obj_matrix <- chol2inv(chol(CLS_Distance_Objective_Matrix(X=X)))
}

# CLS <- function(y, X, tau) {
#   if(missing(tau)) { tau <- tau_weights() }
# #   X <- scale(X)
# #   y <- scale(y)
#   obj_matrix <- tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))
# #   inv_obj_matrix <- solve(obj_matrix)
#   inv_obj_matrix <- chol2inv(chol(obj_matrix))
#   beta.hat <- inv_obj_matrix %*% t(X) %*% y
#   # cheaper inverse computation for non-small cases using singular value decomposition as in lm.ridge
#   # maybe also possibility to use QR; not as good because easier to update matrix of which inverse is being taken
#   return(beta.hat)
# }

tau_weights <- function(w) {
  if(missing(w)) {
    tau <- 0.5; # case where all weights equal to 1 - least squares is when tau=1
  }
  
}

distance_corr <- function(y,X) {
#   # helpful reference wikipedia page: http://en.wikipedia.org/wiki/Distance_correlation#Distance_correlation
#   # currently using functions from the "energy" package
# #   dcs <- DCOR(y=y, x=X)
#   dcs <- DCOR(y=y, x=X, index=dcorr_index)
#   dist_corr <- dcs$dCov / sqrt( dcs$dVarX * dcs$dVarY)
#   dist_corr <- DCOR(y=y, x=X, index=dcorr_index)$dCor # only good for single-variable in X
  apply(X, 2, function(x) DCOR(y=y,x=x,index=dcorr_index)$dCor)
}

weighted_kendall_tau <- function(beta.hat, beta) {
  
}


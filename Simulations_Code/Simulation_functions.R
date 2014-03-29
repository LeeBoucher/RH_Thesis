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

library(Matrix)
library(MASS)
source(paste(directory_path, 
             paste("OtherPeoplesCode","energy.R", sep=separator), sep=separator))

CLS_Pearson_Objective_Matrix <- function(X, tau) {
  if(missing(tau)) { tau <- tau_weights() }
  obj_matrix <- tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))
}

CLS_Pearson_Objective_Inv_Matrix <- function(X) {
  inv_obj_matrix <- chol2inv(chol(CLS_Pearson_Objective_Matrix(X=X)))
}

CLS_Pearson_coefficients_for_y <- function(X) {
  inv_obj_matrix_times_XT <- CLS_Pearson_Objective_Inv_Matrix(X=X) %*% t(X)
}



CLS_Distance_Objective_Matrix <- function(X, tau) {
  if(missing(tau)) { tau <- tau_weights() }
  obj_matrix <- tau * Rxx_dist(X) + (1-tau)*diag(rep(1, p)) 
      # wikipedia for Rxx http://en.wikipedia.org/wiki/Covariance_matrix
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
#   # cheaper inverse computation for non-small cases using 
#   # singular value decomposition as in lm.ridge
#   # maybe also possibility to use QR; not as good because easier to update 
#   # matrix of which inverse is being taken
#   return(beta.hat)
# }

SIS_beta_hat <- function(tX, y) {
#   tX <- t(X)
# #   n <- length(y)
# #   p <- dim(tX)[2] # This is supposed to be the number of observations. (Smaller)
# #       # Also, this SIS package is really screwy. 
# #   print(n)
# #   print(p)
# #   sis_results <- SIS(list(x=tX,y=y), family=gaussian(), xtune=tX, ytune=y)
#   # TODO Double check that gaussian in all cases http://www.statmethods.net/advstats/glm.html
# #   return(sis_results)
# #   sis_results <- SIS(list(x=tX,y=y), model='glm')
#   sis_results <- SIS(list(x=X,y=y), model='glm')
# #   sis_results <- SIS(list(x=X,y=y), model='glm', family=NULL, method='efron', vartype=0, nsis=NULL, rank.method='obj', eps0=1e-5, inittype='NoPen', tune.method='BIC', folds=NULL, post.tune.method='CV',post.tune.folds=NULL, DOISIS=TRUE, ISIStypeCumulative=FALSE, maxloop=5, xtune=NULL, ytune=NULL, detail=FALSE)
  beta_hat <- tX %*% y
}

tau_weights <- function(w) {
  if(missing(w)) {
    tau <- 0.5; # case where all weights equal to 1 - least squares is when tau=1
  }
  # TODO if time
}

distance_corr <- function(y,X) {
#   # helpful reference wikipedia page for distance correlation 
#   # http://en.wikipedia.org/wiki/Distance_correlation#Distance_correlation
# #   dcs <- DCOR(y=y, x=X)
#   dcs <- DCOR(y=y, x=X, index=dcorr_index)
#   dist_corr <- dcs$dCov / sqrt( dcs$dVarX * dcs$dVarY)
#   dist_corr <- DCOR(y=y, x=X, index=dcorr_index)$dCor # only good for single-variable in X
  apply(X, 2, function(x) DCOR(y=y,x=x,index=dcorr_index)$dCor)
}

weighted_kendall_tau <- function(beta.hat, beta) {
  
}


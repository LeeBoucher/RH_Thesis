# function: rmvnorm
# description: Generate random variates from a multivariate normal density.
#                  X ~ N[p](mu, Sigma), mu is p-by-1 and Sigma is p-by-p
#
# parameters:
#   n          Scalar. The number of replicates to generate.
#   mu         Vector. The means for each component of the resulting variate.
#   Sigma      Matrix. The covariance matrix of the resulting variate
#               (default=diag(length(mu)), independence with variance 1).
#
# returns: 
#   Matrix of dim(n,length(mu)) representing the design matrix. This matrix has
#   one attribute: Sigma, the covariance matrix of one independent observation
#   (row of the design matrix) corresponding to the Sigma supplied in the 
#   parameter list.
rmvnorm <- function(n,mu,Sigma=diag(length(mu))){
  ### Error Checks
  # All parameters of correct form.
  if(!is.numeric(n) || !is.numeric(mu) || !is.numeric(Sigma)){
    stop("All parameters must be numeric.")
  }
  
  # Sigma and mu of conformable dimensions
  Sigma <- as.matrix(Sigma)
  if(nrow(Sigma)!=ncol(Sigma) || nrow(Sigma)!=length(mu)){
    stop("'mu' and 'Sigma' are non-conformable arguments.")
  }
  
  # n must be a scalar
  n <- n[1]
  
  
  ### Generate base variables
  X <- matrix(rnorm(length(mu)*n),nrow=length(mu),ncol=n)
  
  
  ### Correct base variables
  # Use variance-covariance matrix to create correct multiplier:
  #   OUT[i,] = AX[,i] + mu, where Sigma = AA'
  A <- t(chol(Sigma))
  
  # Create correct matrix
  X <- t(A%*%X + mu)
  attr(X,"Sigma") <- Sigma
  return(X)
}
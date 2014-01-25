################################################################################
# Program: Simulation_run
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
if(cluster) { library(multicore) }

# Set initial values, define functions for generating data in linear model

initialize_values <- function(n, m, p, t=FALSE, s=1, dcorr_index=1, rs=0.3, seed=20131105) {
  set.seed(seed)
  
  defaults <- list(n=60, m=500, p=1000)
  if(t) {
    defaults <- list(n=6, m=50, p=100)
  }
  
  if(missing(n)) { n <- defaults$n }
  if(missing(m)) { m <- defaults$m }
  if(missing(p)) { p <- defaults$p }
  
  n <<- n # number of y_i's/observations
  m <<- m # number of replicate data sets
  p <<- p # number of predictors
  dcorr_index <<- dcorr_index # index for distance correlation
  rs <<- rs
  
}

initialize_cases <- function() {
  
  varcov_cases <- list()
  beta1_cases <- list()
  beta0_cases <- list()
  xforms_cases <- list()
  
  data_transformations <<- list("identity"=function(x) x, "x^2"=function(x) x^2, "e^x"=function(x) exp(x))
  
  varcov_cases[["all_independent"]] <- varcov.generate(p=p, blocks=c(0,p))
  varcov_cases[["equicorrelated_0.3"]] <- varcov.generate(p=p, blocks=c(0.3,p))
  varcov_cases[["equicorrelated_0.6"]] <- varcov.generate(p=p, blocks=c(0.6,p))
  varcov_cases[["equicorrelated_0.9"]] <- varcov.generate(p=p, blocks=c(0.9,p))
  #   block_cases[["block_equicorrelated_p/5_0.6_0.3"]] <- varcov.generate(p=p, blocks=list(c(0.3, 0.8*p), c(0.6, 0.2*p)))
  
  beta1_cases[["no_signal"]] <- rep(0,p)
  beta1_cases[["single_signal"]] <- c(1, rep(0, p-1))
  beta1_cases[["10_signals_strength_same"]] <- c(rep(1,10), rep(0,p-10))
  beta1_cases[["10_signals_strength_int_powers_of_2"]] <- c(sapply(c(4:-5), function(x) 2^x), rep(0, p-10))
  
  beta0_cases[["zero_shift"]] <- rep(0,n)
  
  xforms_cases[["no_transformation"]] <- c(rep(1,p))
  xforms_cases[["alternating_x_and_x^2"]] <- c(rep(c(1,2),p/2))
  xforms_cases[["alternating_x_and_e^x"]] <- c(rep(c(1,3),p/2))
  
  varcov_cases <<- varcov_cases
  beta1_cases <<- beta1_cases
  beta0_cases <<- beta0_cases
  xforms_cases <<- xforms_cases
  
}

single_iteration_all_cases <- function() {
  cases <- list()
  
  tau <- 0.5 # TODO: This is not ideal way to set
  mu <- rep(0,p)
  
  rawX <- matrix(data=NA, nrow=n, ncol=p)
  for (vc in varcov_cases) {
    case <- list()
#     case[["varcov"]] <- vc
    rawX <- rmvnorm(n, mu=mu, Sigma=vc)
    
    X <- matrix(data=NA, nrow=n, ncol=p)
    for (xforms in xforms_cases) {
#       case[["xforms"]] <- xforms
      X <- matrix(mapply(function(x, i) data_transformations[[xforms[[i]]]](x), rawX, col(rawX)), nrow = nrow(rawX))
      
      obj_matrix <- tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))
      inv_obj_matrix <- chol2inv(chol(obj_matrix))
      inv_obj_matrix_times_XT <- inv_obj_matrix %*% t(X)
      # Rxx_dist <- distance_corr(X=X, y=X) # TODO: ??
      # distance_corr_inverse <- chol2inv(chol(tau * Rxx_dist + diag(1-tau,p))
      
      for (b1 in beta1_cases) {
#         case[["beta1"]] <- b1
        
        y <- generate_y(X=X, beta1=b1, varcov=vc)
#         case[["y"]] <- y
        
        case[["beta_hat_CLS_pearson"]] <- inv_obj_matrix_times_XT %*% y
        # case[["beta_hat_CLS_dist"]] <- distance_corr_inverse %*% distance_corr(y=y, X=X)
        # case[["beta_hat_SIS"]] <- # TODO
        
        cases[[length(cases) + 1]] <- case
      }
    }
  }
  return(cases)
}

generate_y <- function(X, beta1, varcov, beta0=rep(0,n)) {
  
  if(sqrt(sum(beta1^2)) == 0) {
    e.sd <- 1
  } else {
    ss <- ((1-rs^2)*t(beta1) %*% varcov %*% beta1)/(rs^2)
    v <- var(X %*% beta1)
    e.sd <- v/(v + ss)
  }
  e <- c(data=rnorm(n, mean=0, sd=e.sd))
  
  y <- e + X %*% beta1 + beta0
  
  return(y)
}

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
  return(varcov)
}

iterate_m_times <- function() {
  if(cluster){ mclapply(1:m, function(x) single_iteration_all_cases(), mc.preschedule=TRUE) 
  } else { lapply(1:m, function(x) single_iteration_all_cases()) }
}

# case_description <- function(vc_ci,xf_ci, b1_ci) {
#   vc_n <- names(varcov_cases)[[vc_ci]]
#   xf_n <- names(xforms_cases)[[xf_ci]]
#   b1_n <- names(beta1_cases)[[b1_ci]]
#   name <- paste(c(vc_n, xf_n, b1_n), collapse=" | ", sep="")
#   return(name)
# }

organize_data_by_case <- function(d) {
  case_descriptions <- list()
  for(vc in names(varcov_cases)) {
    for (xforms in names(xforms_cases)) {
      for (b1 in names(beta1_cases)) {
        name <- paste(c(vc, xforms, b1), collapse=" | ", sep="")
        case_descriptions[[length(case_descriptions) + 1]] <- name
      }
    }
  }
  data_organized_by_case <- list()
  for(description in case_descriptions) {
    data_organized_by_case[[description]] <- list()
  }
  
  for(iteration in c(1:length(all_data))) {
    for(case in c(1:length(iteration))) {
      data_organized_by_case[[case]][[length(data_organized_by_case[[case]]) + 1]] <- all_data[[iteration]][[case]]
    }
  }
  
  return(data_organized_by_case)
}

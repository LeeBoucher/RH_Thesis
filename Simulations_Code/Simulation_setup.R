################################################################################
# Program: Simulation_setup
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
  
#   cases <<- generate_cases()
  
#   corr.pearson <<- matrix(data=NA, nrow=p, ncol=m)
#   corr.dist <<- matrix(data=NA, nrow=p, ncol=m)
#   varcov <<- matrix(data=NA, nrow=p, ncol=p)
#   y <<- rep(NA, n)
#   e <<- rep(NA, n)
#   beta1 <<- rep(NA, p)
#   beta1hat <<- rep(NA, p)
#   beta0 <<- rep(NA, n)
#   beta0hat <<- rep(NA, n)
#   X <<- matrix(NA, nrow=n, ncol=p)
#   raw_X <<- matrix(NA, nrow=n, ncol=p)
  
#   data_by_case_run <<- list()
  
}

initialize_cases <- function() {
  
  varcov_cases <<- list()
  beta1_cases <<- list()
  beta0_cases <<- list()
  data_transformations <<- list("identity"=function(x) x, "x^2"=function(x) x^2, "e^x"=function(x) exp(x))
  xforms_cases <<- list()
  
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
  
}

generate_cases <- function() {
  cases <- list()
  
#   block_cases <- list()
#   block_cases[["all_independent"]] <- c(0,p)
#   block_cases[["equicorrelated_0.3"]] <- c(0.3, p)
#   block_cases[["equicorrelated_0.6"]] <- c(0.6, p)
#   block_cases[["equicorrelated_0.9"]] <- c(0.9, p)
# #   block_cases[["block_equicorrelated_p/5_0.6_0.3"]] <- list(c(0.3, 0.8*p), c(0.6, 0.2*p))
#   
#   beta1_cases <- list()
#   beta1_cases[["no_signal"]] <- rep(0,p)
#   beta1_cases[["single_signal"]] <- c(1, rep(0, p-1))
#   beta1_cases[["10_signals_strength_same"]] <- c(rep(1,10), rep(0,p-10))
#   beta1_cases[["10_signals_strength_int_powers_of_2"]] <- c(sapply(c(4:-5), function(x) 2^x), rep(0, p-10))
#   
#   xforms_cases <- list()
# #   x_forms <- list("identity"=function(x) x, "x^2"=function(x) x^2, "e^x"=function(x) exp(x))
#   xforms_cases[["no_transformation"]] <- c(rep(1,p))
#   xforms_cases[["alternating_x_and_x^2"]] <- c(rep(c(1,2),p/2))
#   xforms_cases[["alternating_x_and_e^x"]] <- c(rep(c(1,3),p/2))
  
#   b0 <- rep(0,n)
#   for(block_case in block_cases) {
#     vc <- varcov.generate(p=p, blocks=block_case)
#     x_col_order <- c(1:p)
#     
#     for (beta1_case in 1:length(beta1_cases)) {
#       b1 <- beta1_cases[[beta1_case]]
#       
#       for (xforms_case in xforms_cases) {
#         xforms <- xforms_case
#         # TODO: also pass string description?
#         case <- list(beta1=b1, beta0=b0, varcov=vc, x_col_order=x_col_order, xforms=xforms)
#         cases[[length(cases) + 1]] <- case
#       }
#     }
#   }
  
  tau <- 0.5 # TODO: This is not ideal way to set
  mu <- rep(0,p)
  
  for (vc in varcov_cases) {
    case <- list()
    case[["varcov"]] <- vc
    rawX <- rmvnorm(n, mu=mu, Sigma=vc)
    
    for (xforms in xforms_cases) {
      case[["xforms"]] <- xforms
      X <- matrix(mapply(function(x, i) data_transformations[[xforms[[i]]]](x), rawX, col(X)), nrow = nrow(X))
      
      
#       XTX <- t(X) %*% X   # Don't want to chew up memory unnecessarily
      objective_matrix <- tau * t(X) %*% X + (1-tau)*diag(diag(t(X) %*% X))
      inv_obj_matrix <- chol2inv(chol(obj_matrix))
#       rm(objective_matrix)
      inv_obj_matrix_times_XT <- inv_obj_matrix %*% t(X)
#       Rxx_dist <- distance_corr(X=X, y=X) # TODO: ??
#       distance_corr_inverse <- chol2inv(chol(tau * Rxx_dist + diag(1-tau,p))
#       rm(inv_obj_matrix)
      
      for (b1 in beta1_cases) {
        case[["beta1"]] <- b1
        
        y <- generate_y(X=X, beta1=b1)
        case[["y"]] <- y
        
        case[["beta_hat_CLS_pearson"]] <- inv_obj_matrix_times_XT %*% y
#         case[["beta_hat_CLS_dist"]] <- distance_corr_inverse %*% distance_corr(y=y, X=X)
#         case[["beta_hat_SIS"]] <- # TODO
        
        cases[[length(cases) + 1]] <- case
      }
    }
#     rm(rawX)
  }
  
  return(cases)
}

get_case <- function(case) { 
  case <- cases[[case]]
  recalc <- case$recalc
  varcov <- case$varcov
  beta0 <- case$beta0
  beta1 <- case$beta1
  x_col_order <- case$x_col_order
  xforms <- case$xforms
  
  return(list(beta1=beta1, beta0=beta0, varcov=varcov, x_col_order=x_col_order, xforms=xforms))
}

# generate_data <- function(varcov, beta1, beta0, x_col_order, xforms, rs_) {
generate_y <- function(X, beta1, beta0=rep(0,n)) {
#   X <- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
#   X <- matrix(mapply(function(x, i) data_transformations[[xforms[[i]]]](x), X, col(X)), nrow = nrow(X))
  
  if(sqrt(sum(beta1^2)) == 0) {
    e.sd <- 1
  } else {
    ss <- ((1-rs^2)*t(beta1) %*% varcov %*% beta1)/(rs^2)
    v <- var(X %*% beta1)
    e.sd <- v/(v + ss)
  }
  e <- c(data=rnorm(n, mean=0, sd=e.sd))
  
  y <- e + X %*% beta1 + beta0
  
#   return(list(X=X, y=y))
  return(y)
}

run_case <- function(beta1, beta0, varcov, x_col_order, xforms) { 
  case <- list(beta1=beta1, varcov=varcov, x_col_order=x_col_order, xforms=xforms, beta1hat=NA)
  rand_Xy_data <- generate_data(varcov=varcov, beta1=beta1, beta0=beta0, x_col_order=x_col_order, xforms=xforms)
  X <- rand_Xy_data[[1]]
  y <- rand_Xy_data[[2]]
  case$beta1hat <- CLS(y=y, X=X)
  case <- list(beta1=case$beta1, beta1hat=case$beta1hat)
  return(case)
}

# generate_raw_X_data <- function(varcov) {
#   # optimize for recalc only when necessary
#   X <- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
# }
# 
# generate_data <- function(case, index, beta1_=NULL, beta0_=NULL, varcov_=NULL, x_col_order_=NULL, xforms_=NULL) {
# # This is all too stupidly general and unneccessary; however, without it, the 
# # function just calls rmvnorm. Make this function for special cases later IFF necessary.
# # generate_data <- function(case_, beta1_=NULL, beta0_=NULL, varcov_=NULL, x_col_order_=NULL, xforms_=NULL) {
# # generate_data <- function() {
# # #   beta1 <<- rep(0,p)
# # #   beta0 <<- rep(0,n)
# # #   e <<- c(data=rnorm(n, mean=0, sd=1))
# # #   X <<- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
# # #   y <<- e + X %*% beta1 + beta0
# #   
# #   e <<- c(data=rnorm(n, mean=0, sd=1)) # needs different variance when response depends on predictors
# #   #  X<-matrix(data=rnorm(n*p, mean=0, sd=1), nrow=n, ncol=p)
# #   #  X<-abs(matrix(data=rnorm(n*p, mean=0, sd=1), nrow=n, ncol=p)) 
# #   # useful for distance measure, since that is just abs of pearson corr coeff when all normal
# #   X <<- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
# #   # highly correlated predictors, compound symmetric aka equicorrelated generated by multivariate normal
# # #   beta0 <<- rep(0,p)
# #   y <<- e + X %*% beta1 + beta0 # just noise
#   
# #   if(get_case(case=case, beta1=beta1_, beta0=beta0_, varcov=varcov_, x_col_order=x_col_order_, xforms=xforms_)) {
# #     raw_X <<- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
# #   }
#   
#   case <- get_case(case=case, beta1=beta1_, beta0=beta0_, varcov=varcov_, x_col_order=x_col_order_, xforms=xforms_)
#   
# #   if(get_case$recalc) {
# #     raw_X <- rmvnorm(n, mu=rep(0,p), Sigma=varcov)
# #   }
# #   
# #   X <- raw_X
#   
# #   X <<- matrix(data=rnorm(n*p, mean=0, sd=1), nrow=n, ncol=p)
#   
# #   if(t(beta1) %*% beta1 == 0) {
# #     e.sd <<- 1
# #   } else {
# #     ss <- ((1-rs^2)*t(beta1) %*% varcov %*% beta1)/(rs^2)
# #     v <- var(X %*% beta1)
# #     e.sd <<- v/(v + ss)
# #   }
# #   e <<- c(data=rnorm(n, mean=0, sd=e.sd))
#   
# #   y <<- e + X %*% beta1 + beta0
# #   generate_y(X, beta1, beta0, e)
#   
# #   generate_y(X=X, case=case)
#   
# }
# 
# generate_y <- function(X, varcov, beta1=NULL, beta0=NULL, x_col_order=NULL, xforms=NULL, rs_) {
# # generate_y <- function(X, case, rs_, beta1_=NULL, beta0_=NULL, x_col_order_=NULL, xforms_=NULL) {
# # generate_y <- function(X, beta1, beta0, e) {
# #   get_case(case=case, beta1_=beta1_, beta0_=beta0_, varcov=NULL, x_col_order_=x_col_order_, xforms_=xforms_)
#   if(missing(rs_)) { rs_ <- rs}
#   
#   if(sqrt(sum(beta1^2)) == 0) {
#     e.sd <- 1
#   } else {
#     ss <- ((1-rs_^2)*t(beta1) %*% varcov %*% beta1)/(rs_^2)
#     v <- var(X %*% beta1)
#     e.sd <- v/(v + ss)
#   }
#   e <- c(data=rnorm(n, mean=0, sd=e.sd))
#   
#   y <- e + X %*% beta1 + beta0
#   
#   return(list(y=y, e=e))
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

single_iteration_all_cases <- function() { 
  lapply(cases, function(x) run_case(beta1=x$beta1, beta0=x$beta0, varcov=x$varcov, x_col_order=x$x_col_order, xforms=x$xforms)) 
}

iterate_m_times <- function() {
  if(cluster){ mclapply(1:m, function(x) single_iteration_all_cases(), mc.preschedule=TRUE) 
  } else { lapply(1:m, function(x) single_iteration_all_cases()) }
}

organize_data_by_case <- function(d) {
  data_organized_by_case <- list()
  for(c in 1:length(cases)) {
    case_params <- list(beta1=NA)
#     case_params <- list(varcov=NA, x_col_order=NA, xforms=NA, beta1=NA)
#     case_params$varcov <- cases[[c]][["varcov"]]
    case_params$beta1 <- cases[[c]][["beta1"]]
#     case_params$x_col_order <- cases[[c]][["x_col_order"]]
#     case_params$xforms <- cases[[c]][["xforms"]]
    results <- list()
    for(r in 1:m) {
      name <- paste("beta1hat", r, sep="")
      results[[name]] <- d[[r]][[c]][["beta1hat"]]
    }
    case <- list(case_params, results)
    data_organized_by_case[[length(data_organized_by_case) + 1]] <- case
  }
  return(data_organized_by_case)
}

maxes_meds <- function(maxes_plots=T,meds_plots=T) {
  maxes.dist <<- apply(abs(corr.dist), 2, max)
  maxes.pearson <<- apply(abs(corr.pearson), 2, max)
  meds.dist <<- apply(abs(corr.dist), 2, median)
  meds.pearson <<- apply(abs(corr.pearson), 2, median)
#   if(maxes_plots || meds_plots){ 
  # TODO: fix the condition or the maxes_meds_plots function to make 
  # maxes plots and medians plots actually separated options
  maxes_med_plots()
#     }
}

maxes_med_plots <- function() {
#   par(mfcol=c(2,2))
#   plot(density(maxes.dist))
#   plot(density(maxes.pearson))
#   plot(density(meds.dist))
#   plot(density(meds.pearson))
  
#   plot(density(maxes.dist), main="Maxes", col="blue", xlim=c(0,1))
#   lines(density(maxes.pearson), col="green")
#   plot(density(meds.dist), main="Medians", col="blue", xlim=c(0,1))
#   lines(density(meds.pearson), col="green")
  
  graph_title = paste("Maximum and Median Correlations 
    with Pearson Correlation and 
    Distance Correlation (index=", dcorr_index, ")")
  
  plots <- c("Distance Maxes", "Distance Medians", "Pearson Maxes", "Pearson Medians")
  plot_colors <- c("blue2", "cornflowerblue", "chartreuse4", "chartreuse2")
  names(plot_colors) = plots
#   range <- c(0,1)
  range_lower <- min(c(min(meds.pearson), min(meds.dist)))
  range_upper <- max(c(max(maxes.pearson), max(maxes.dist)))
  range_diff <- range_upper - range_lower
  range_padding <- 0.1
  range_extend <- range_padding * range_diff
  range <- c(max(0, range_lower - range_extend), min(1, range_upper + range_extend))
  
  plot(density(maxes.dist), main=graph_title, col=plot_colors["Distance Maxes"], xlim=range)
#   lines(density(maxes.dist), col=plot_colors["Distance Maxes"])
  lines(density(maxes.pearson), col=plot_colors["Pearson Maxes"])
  lines(density(meds.dist), col=plot_colors["Distance Medians"])
  lines(density(meds.pearson), col=plot_colors["Pearson Medians"])
  legend('bottomleft', legend=plots, lty=1, col=plot_colors, bty='n', cex=0.75)
  
#   # TODO: make this whole section not suck
#   maxes_graph_title = paste("Maximum Pearson Correlations and 
#     Distance Correlations (index=", dcorr_index, ")")
#   meds_graph_title = paste("Median Pearson Correlations and 
#     Distance Correlations (index=", dcorr_index, ")")
#   maxes_plots <- c(plots[1], plots[3])
#   meds_plots <- c(plots[2], plots[4])
#   maxes_range_lower <- min(c(min(maxes.pearson), min(maxes.dist)))
#   maxes_range_upper <- max(c(max(maxes.pearson), max(maxes.dist)))
#   maxes_range_diff <- maxes_range_upper - maxes_range_lower
#   maxes_range_extend <- range_padding * maxes_range_diff
#   meds_range_lower <- min(c(min(meds.pearson), min(meds.dist)))
#   meds_range_upper <- max(c(max(meds.pearson), max(meds.dist)))
#   meds_range_diff <- meds_range_upper - meds_range_lower
#   meds_range_extend <- range_padding * meds_range_diff
#   maxes_range <- c(max(0, maxes_range_lower - maxes_range_extend), min(1, maxes_range_upper + maxes_range_extend))
#   meds_range <- c(max(0, meds_range_lower - meds_range_extend), min(1, meds_range_upper + meds_range_extend))
#   
# #   maxes_range <- c(0.5,1)
# #   meds_range <- c(0.2,1)
#   maxes_range <- c(0,1)
#   meds_range <- c(0,1)
#   
#   par(mfrow=c(1,2))
#   plot(density(maxes.dist), main=maxes_graph_title, col=plot_colors["Distance Maxes"], xlim=maxes_range)
#   lines(density(maxes.pearson), col=plot_colors["Pearson Maxes"])
#   legend('bottomleft', legend=maxes_plots, lty=1, col=c(plot_colors[maxes_plots[1]], plot_colors[maxes_plots[2]]),
#          bty='n', cex=0.75) # TODO: these legends are scaling and aligning weird
#   
#   plot(density(meds.dist), main=meds_graph_title, col=plot_colors["Distance Medians"], xlim=meds_range)
#   lines(density(meds.pearson), col=plot_colors["Pearson Medians"])
#   legend('bottomleft', legend=meds_plots, lty=1, col=c(plot_colors[meds_plots[1]], plot_colors[meds_plots[2]]),
#          bty='n', cex=0.75)
}


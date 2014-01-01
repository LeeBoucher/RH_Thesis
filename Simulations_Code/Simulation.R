################################################################################
# Program: Simulation
# Project: Boucher Senior Thesis
# Description: Conduct a preliminary simulation study for CLS ordering vs SIS
#
# Author: Kimberly Boucher
# Date: 
#
# Notes:

################################################################################

run_time <- Sys.time()

library(Matrix)

directory_path <- "C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\"
source(paste(directory_path, "Simulation_functions.R", sep=""))
source(paste(directory_path, "Simulation_setup.R", sep=""))
source(paste(directory_path, "OtherPeoplesCode\\RandomNormal.R", sep=""))

# initialize_values(m=3)
# initialize_values(t=TRUE, m=3)
# initialize_values(t=TRUE)
initialize_values(n=2, m=5, p=10)

run_case <- function(beta1, beta0, varcov, x_col_order, xforms, recalc) { 
  case <- list(beta1=beta1, beta1hat=NA, varcov=varcov, x_col_order=x_col_order, xforms=xforms)
  X <- rmvnorm(n, mu=rep(0,p), Sigma=case$varcov) # optimize this to recalc only when necessary
  y <- generate_y(X=X, varcov=case$varcov, beta1=case$beta1, beta0=beta0)[[1]]
  case$beta1hat <- CLS(y=y, X=X)
  case <- list(beta1=case$beta1, beta1hat=case$beta1hat)
  return(case)
}

single_iteration_all_cases <- function() { 
  lapply(cases, function(x) run_case(x$beta1, x$beta0, x$varcov, x$x_col_order, x$xforms)) 
}

all_data <- lapply(1:m, function(x) single_iteration_all_cases())

all_data_organized_by_case <- organize_data_by_case(all_data)

# for(j in 1:m) {
#   for(c in 1:length(cases)) { # TODO: make it so that cases don't overwrite each other on anything we want to keep
#     generate_data(case=c)
#     beta1hat <- CLS(y=y, X=X)
# #     corr.pearson[,j] <- abs(cor(y,X))
# #     for(k in 1:p) {
# #       corr.dist[k,j] <- distance_corr(y,X[,k])
# #     }
# #     corr.dist[,j] <- apply(X, 2, function(X) distance_corr(y=y, X=X))
#     
#   }
# }

# maxes_meds()

run_time <- Sys.time() - run_time
print(run_time)

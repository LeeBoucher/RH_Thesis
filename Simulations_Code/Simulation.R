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

# initialize_values(m=15)
# initialize_values(n=6, m=5, p=100)
# initialize_values(t=TRUE, m=5)
initialize_values(t=TRUE)

for(j in 1:m) {
  for(c in 1:num_cases) { # TODO: make it so that cases don't overwrite each other on anything we want to keep
    generate_data(case=c)
    beta1hat <- CLS(y=y, X=X)
    corr.pearson[,j] <- abs(cor(y,X))
#     for(k in 1:p) {
#       corr.dist[k,j] <- distance_corr(y,X[,k])
#     }
    corr.dist[,j] <- apply(X, 2, function(X) distance_corr(y=y, X=X))
    
  }
}

maxes_meds()

run_time <- Sys.time() - run_time
print(run_time)

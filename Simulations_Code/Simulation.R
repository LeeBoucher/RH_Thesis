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

library(Matrix)

source("C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\Simulation_functions.R")
source("C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\Simulation_setup.R")
source("C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\OtherPeoplesCode\\RandomNormal.R")

run_time <- Sys.time()

initialize_values(t=TRUE)

for(c in 1:num_cases) {
  for(j in 1:m) {
    generate_data(case=c)
    beta1hat <- CLS(y=y, X=X)
    corr.pearson[,j] <- abs(cor(y,X))
    for(k in 1:p) {
      corr.dist[k,j] <- distance_corr(y,X[,k])
    }
  }
}

maxes_meds()

run_time <- Sys.time() - run_time
print(run_time)

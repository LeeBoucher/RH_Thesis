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

directory_path <- "C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\"
source(paste(directory_path, "Simulation_functions.R", sep=""))
source(paste(directory_path, "Simulation_setup.R", sep=""))
source(paste(directory_path, "OtherPeoplesCode\\RandomNormal.R", sep=""))

initialize_values(t=TRUE)

run_time <- Sys.time()

for(c in 1:length(cases)) { # TODO: make it so that cases don't overwrite each other on anything we want to keep
  for(j in 1:m) {
    generate_data(case=c)
    for(index in 1:4) {
      generate_y(X=X, case=c, index=index)
      beta1hat <- CLS(y=y, X=X)
      corr.pearson[,j] <- abs(cor(y,X))
      for(k in 1:p) {
        corr.dist[k,j] <- distance_corr(y,X[,k])
      }
    }
  }
  
#   maxes_meds()
}

# maxes_meds()

run_time <- Sys.time() - run_time
print(run_time)

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

cluster <- T

if (cluster) { directory_path <- "~/Private/Thesis/Simulations_Code/" 
} else { directory_path <- "C:\\Users\\boucheka\\Documents\\Boucher_Thesis\\Simulations_Code\\" }
source(paste(directory_path, "Simulation_functions.R", sep=""))
source(paste(directory_path, "Simulation_setup.R", sep=""))
if(cluster) { source(paste(directory_path, "OtherPeoplesCode/RandomNormal.R", sep="")) 
} else { source(paste(directory_path, "OtherPeoplesCode\\RandomNormal.R", sep="")) }

initialize_values()
# initialize_values(m=7)
# initialize_values(t=TRUE, m=3)
# initialize_values(t=TRUE)
# initialize_values(n=2, m=5, p=10)

cases <- generate_cases()

all_data <- iterate_m_times()

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

run_time <- Sys.time() - run_time
print(run_time)

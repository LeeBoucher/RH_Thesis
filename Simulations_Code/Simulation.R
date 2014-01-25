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

directory_path <- getwd()
cluster <- length(grep("csse.rose-hulman.edu", directory_path, fixed=TRUE)) != 0
if(cluster) {
  separator <- "/"
} else { 
  separator <- "\\"
  directory_path <- normalizePath(directory_path, winslash=separator, mustWork=TRUE)
}
# source(paste(directory_path, "Simulation_functions.R", sep=separator))
# source(paste(directory_path, "Simulation_setup.R", sep=separator))
source(paste(directory_path, "Simulation_run.R", sep=separator))
source(paste(directory_path, paste("OtherPeoplesCode", "RandomNormal.R", sep=separator), sep=separator))

if(cluster){ initialize_values() 
} else { initialize_values(t=TRUE) }
# initialize_values(n=2, m=5, p=10)
# initialize_values(m=1)
# initialize_values(m=3)
# initialize_values(t=TRUE, m=3)

initialize_cases()

all_data <- iterate_m_times()

# all_data_organized_by_case <- organize_data_by_case(all_data)

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

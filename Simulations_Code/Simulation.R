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
library(energy)

directory_path <- getwd()
cluster <- length(grep("csse.rose-hulman.edu", directory_path, fixed=TRUE)) != 0
if(cluster) {
  separator <- "/"
} else { 
  separator <- "\\"
  directory_path <- normalizePath(directory_path, winslash=separator, mustWork=TRUE)
}
source(paste(directory_path, "Simulation_functions.R", sep=separator))
# source(paste(directory_path, "Simulation_setup.R", sep=separator))
source(paste(directory_path, "Simulation_run.R", sep=separator))
source(paste(directory_path, paste("OtherPeoplesCode", "RandomNormal.R", sep=separator), sep=separator))

time_run <- Sys.time()

if(cluster){ initialize_values() } else { initialize_values(t=TRUE) }
initialize_values(n=2, m=5, p=10)
initialize_values(m=1)
# initialize_values(m=10)
# initialize_values(t=TRUE, m=10)

initialize_cases()

# all_data_organized_by_case <- organize_data_by_case(iterate_m_times())
# 
# all_data <- iterate_m_times()
# all_data_organized_by_case <- organize_data_by_case(all_data)
# all_data_organized_by_kind <- organize_data_by_kind(all_data)

all_data_organized_by_kind <- organize_data_by_kind(iterate_m_times())

write_and_save_data()

time_run <- Sys.time() - time_run
print(time_run)

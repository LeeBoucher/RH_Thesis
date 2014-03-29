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

time_total <- time_run <- Sys.time()
library(Matrix)
# library(energy)

separator <- "/"
<<<<<<< HEAD
directory_path <- paste(getwd(), "Simulations_Code", sep=separator)
results_dir_path <- paste(getwd(), "Results", sep=separator)
=======
directory_path <- paste(getwd(), "Simulations_code", sep=separator)
>>>>>>> 95a8c3389257983f93f8f87fe3230052540d77d7
source(paste(directory_path, "Simulation_functions.R", sep=separator))
source(paste(directory_path, "Simulation_run.R", sep=separator))
source(paste(directory_path, paste("OtherPeoplesCode", "RandomNormal.R", sep=separator), sep=separator))

time_run <- Sys.time()

initialize_values()
# initialize_values(t=TRUE)
initialize_values(n=2, m=5, p=10)
# initialize_values(m=1)
# initialize_values(m=10)
# initialize_values(t=TRUE, m=10)
initialize_values(t=TRUE, m=1)

initialize_cases()

all_data_organized_by_kind <- organize_data_by_kind(iterate_m_times())

# write_and_save_data(result_dir=results_dir_path)

time_run <- Sys.time() - time_run
print("Computation time: ")
print(time_run)
time_total <- Sys.time() - time_total
print("Total time: ")
print(time_total)

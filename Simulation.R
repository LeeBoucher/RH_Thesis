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

source("C:\\Users\\boucheka\\Documents\\Thesis\\Simulations_Code\\Simulation_functions.R")
source("C:\\Users\\boucheka\\Documents\\Thesis\\Simulations_Code\\Simulation_setup.R")
source("C:\\Users\\boucheka\\Documents\\Thesis\\Simulations_Code\\RandomNormal.R")

initialize_values(t=TRUE)

generate_cases()

for(c in 1:cases) {
  get_case(case=c)
  for(j in 1:m) {
    generate_data()
    beta1hat <- CLS(y=y, X=X)
    corr.pearson[,j] <- cor(y,X)
#     corr.dist[,j] <- distance_corr(y,X)
  }
}

maxes_meds_plots(corr=corr.pearson)

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

beta1 <- rep(0,p)
beta0 <- rep(0,n)

for(j in 1:m) {
  generate_data()
  beta1hat <- CLS(y=y, X=X)
  corr[,j] <- cor(y,X)
}

maxes_meds_plots()


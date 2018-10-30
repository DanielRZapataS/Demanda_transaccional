## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="Q_Cajeros_1.6> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utilities.R")
# Set up paths
set_environment() 
# runing model
demandaTransaccional()

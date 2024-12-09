#Create data folder and output folders in working directory
dir.create("Data")
dir.create("Output")
dir.create("Output/Plots")

#Load required libraries
install.packages("remotes")
remotes::install_github("BirdsCanada/naturecounts")
library(naturecounts)
library(tidverse)
library(naturecounts) 
library(readxl)

#source the functions
source("PSSS_BMDE.R")


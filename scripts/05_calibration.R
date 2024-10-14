##==============================================================================
## Project: QuEST
## Script to process raw data
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library("devtools")
#install_github("meireles/spectrolab")
#install.packages("spectrolab") # No 
library(spectrolab)
#install.packages("RcppArmadillo")
#install.packages("dev.tools")
#install.packages("pls")
#library(devtools)
library(pls)
#install_github(repo = "meireles/spectrolab") # Install analysis package
# Make sure to hit "no" for install
#install.packages("devtools")
#install.packages("devtools", repos = "http://cran.us.r-project.org")
#install.packages("merTools")
#require(RcppArmadillo)
library(merTools)


#install_github(repo = "griffithdan/plantspec") # Install analysis package
#library(plantspec)
#install_github(repo = "griffithdan/plantspecDB") # Install data package 
#library(plantspecDB)

#library(spectrolab) # no longer available with this verison of R
library(data.table)
library(xts)
library(pls)
library(readxl)
library(devtools)
#library(plantspec)
#library(plantspecDB)
#library(merTools)

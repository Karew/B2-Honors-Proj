#00_ExplorePlantEcoPhys
#Author: Dave Moore
#Date: 10/08/2015
#Purpose: Test the Plantecophys package - a stand alone package to model common leaf gas exchange measurements


# updated plantecophys to developer version 0.6.6
#library(devtools)
#install_bitbucket("remkoduursma/plantecophys")

#Manual: https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 

library (plantecophys)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid) #required for 'unit'
#Load data
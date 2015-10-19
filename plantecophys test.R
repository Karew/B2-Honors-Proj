#Karen tries PlantEcoPhys
#Date: 10/12/2015
#Purpose: Trying to use some of the plantecophys package on Karen's data, specifically the fitaci 
#function
#Using data from file: "b2 pop a18 09-18-15" from the "data" folder

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
a18_data=read.table("data//b2 pop a18 kw 09-18-15",skip = 18,sep="\t", header=FALSE)
print(a18_data)
a18_data_headers=read.table("data//b2 pop a18 kw 09-18-15",skip = 16, nrows = 1,
                            stringsAsFactors = FALSE,sep="\t", header=FALSE)
a18_columns=as.list(a18_data_headers)
colnames(a18_data)=a18_columns

#Fit an aci curve to the data using measured T values
a18_aci=fitaci(a18_data, Tcorrect = FALSE)
plot(a18_aci)

#Summary of the fit
summary(a18_aci)

#Get Vcmax and Jmax specifically
coef(a18_aci)

#Compare the modelled A with measured A using a line with slope of 1
with(a18_aci$df,plot(Amodel,Ameas))
abline(0,1)

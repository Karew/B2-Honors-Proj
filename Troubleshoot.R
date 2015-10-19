#Karen troubleshoots the problem files
#Date: 10/19/2015
#Purpose: Trying to troubleshoot why some files don't work in plantecophys w/ fitaci
#Using data from file: "b2 pop b06 09-11-15" from the "data" folder

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
b06_data=read.table("data//b2 pop b06 kw 09-11-15",skip = 18,sep="\t", header=FALSE)
#Above line gives us the error we're trying to troubleshoot.

#add a "fill=TRUE" argument:
b06_data=read.table("data//b2 pop b06 kw 09-11-15",skip = 18,sep="\t", fill = TRUE, header=FALSE)
print(b06_data)
#seems to work now

#Read in headers
b06_data_headers=read.table("data//b2 pop b06 kw 09-11-15",skip = 16, nrows = 1,
                            stringsAsFactors = FALSE,sep="\t", header=FALSE)

#set the headers as column names
b06_columns=as.list(b06_data_headers)
colnames(b06_data)=b06_columns

#Fit an aci curve to the data using measured T values
b06_aci=fitaci(b06_data, Tcorrect = FALSE)
plot(b06_aci)
#fitaci doesn't like the NA
#if you change line #23 to say skip=21, then it will work properly

#Summary of the fit
summary(b06_aci)

#Get Vcmax and Jmax specifically
coef(b06_aci)

#Compare the modelled A with measured A using a line with slope of 1
with(b06_aci$df,plot(Amodel,Ameas))
abline(0,1)

#Author: Karen Wang
#Date: 11/08/2015
#Purpose: Working script for reading in files, doing QC, fitting A/Ci. 
#Then creates summary df for work with stats

# #Installing PEcAn.photosynthesis as a stand alone
# if (!require("PEcAn.photosynthesis",character.only = TRUE))
# {
#   library(devtools)
#   library(ggplot2)
#   install_github("PecanProject/pecan/modules/photosynthesis") 
# }
# #Create objects called filepath & tempFilelist
# filepath="./data/"
# tempFilelist = list.files(filepath,pattern="b2 pop")
# 
# ##Reading data files using PEcAn.photosynthesis function read.Licor
# #Preferred way: load files to a list
# setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
# cpath = getwd()
# inbetween = "/data/"
# #Creates KarenFiles object that is a list of the file locations
# KarenFiles = paste0(cpath,inbetween,tempFilelist)
# 
# ##Perform QA/QC to get rid of outliers
# #Make master list and apply read.Licor
# KarenMaster = lapply(KarenFiles, read.Licor)
# #Run QA/QC on all files in KarenMaster
# #for all the files in KarenMaster
# for(i in 1:length(KarenMaster)){
#   KarenMaster[[i]] = Licor.QC(KarenMaster[[i]],curve = "ACi")
# }
# 
# #Save the QC
# save(KarenMaster, file="KarenMaster.Rda")

load("KarenMaster.Rda")

#Combine data into one dataframe
KarenACI_qc = do.call("rbind", KarenMaster)

#Filter out the failed QC's with Mike Dietz's code
if("QC" %in% colnames(KarenACI_qc)){
  KarenACI_qc = KarenACI_qc[-which(KarenACI_qc$QC < 1),]  
} else{
  QC = rep(1,nrow(KarenACI_qc))
  KarenACI_qc = cbind(KarenACI_qc,QC)
}

save (KarenACI_qc, file="KarenACI_qc.RDA")

#plot the data w/ ggplot
library(ggplot2)
KarenACI_qc = droplevels(KarenACI_qc,fname="b2 pop f05 kw 09-18-15 redo")
unique(KarenACI_qc$fname)


#using KarenACI_qc AFTER filtering using Mike's do loop to get ride of points that failed the QC
All_Acis= fitacis(KarenACI_qc, "fname")

#plotting your ACI curves using plantecophys 
plot(All_Acis, how="manyplots")

#Get Vcmax and Jmax 
Variables = coef(All_Acis)

#Create df with location and genotype:
loc = c('a18','b06','g11','e10','f05','f08')
geno = c('European','European','European','Missouri x Washington','Missouri x Washington','Missouri x Washington')
loc_and_geno = data.frame(loc,geno, stringsAsFactors = FALSE)

#Pull date & loc from the fname 
#Add date & loc columns to the Variables df
Variables$date = substring(Variables$fname, 14, 22)
Variables$loc = substring(Variables$fname, 8,10)

#Merge the loc_and_geno and Variables df's
All_Data = merge(loc_and_geno,Variables, by = "loc")
#Dropping the unecessary variables to get our summary dataframe
All_Data = All_Data[c("date", "loc","geno", "Vcmax", "Jmax")]

#Changing the date from character to date object
All_Data$date = strptime(All_Data$date, "%m-%d-%y")
All_Data$date = as.Date(All_Data$date)

#Trying out some time series plots
library(dplyr)
just_Euro = dplyr::filter(All_Data, geno=="European")
plot(just_Euro$date, just_Euro$Vcmax)

just_MoWa = dplyr::filter(All_Data, geno=="Missouri x Washington")
plot(just_MoWa$date, just_MoWa$Vcmax)
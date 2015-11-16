#Author: Karen Wang
#Date: 11/15/2015
#Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
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

# load("KarenMaster.Rda")
# 
# #Combine data into one dataframe
# KarenACI_qc = do.call("rbind", KarenMaster)
# 
# #Filter out the failed QC's with Mike Dietz's code
# if("QC" %in% colnames(KarenACI_qc)){
#   KarenACI_qc = KarenACI_qc[-which(KarenACI_qc$QC < 1),]  
# } else{
#   QC = rep(1,nrow(KarenACI_qc))
#   KarenACI_qc = cbind(KarenACI_qc,QC)
# }
# 
# #fixing KarenACI_qc
# KarenACI_qc = droplevels(KarenACI_qc,fname="b2 pop f05 kw 09-18-15 redo")
# unique(KarenACI_qc$fname)
# 
# save (KarenACI_qc, file="KarenACI_qc.RDA")

# #Loading in the more data
# filepath2="./data2/"
# tempFilelist2 = list.files(filepath2,pattern="b2 pop")
# 
# setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
# cpath2 = getwd()
# inbetween2 = "/data2/"
# 
# KarenFiles2 = paste0(cpath2,inbetween2,tempFilelist2)
# 
# KarenMaster2 = lapply(KarenFiles2, read.Licor)
# 
# for(i in 1:length(KarenMaster2)){
#   KarenMaster2[[i]] = Licor.QC(KarenMaster2[[i]],curve = "ACi")
# }
# 
# #Save the QC
# save(KarenMaster2, file="KarenMaster2.Rda")
setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
# load("KarenACI_qc.RDA")
# load("KarenMaster2.Rda")
# 
# #Combine data into one dataframe
# KarenACI_qc2 = do.call("rbind", KarenMaster2)
# 
# if("QC" %in% colnames(KarenACI_qc2)){
#   KarenACI_qc2 = KarenACI_qc2[-which(KarenACI_qc2$QC < 1),]  
# } else{
#   QC = rep(1,nrow(KarenACI_qc2))
#   KarenACI_qc2 = cbind(KarenACI_qc2,QC)
# }
# save(KarenACI_qc2, file = "KarenACI_qc2.RDA")
# load("KarenACI_qc2.RDA")
# #Combining the two data loads
# Updated_ACI_qc = rbind(KarenACI_qc, KarenACI_qc2)
# save(Updated_ACI_qc, file = "Updated_ACI_qc.RDA")
# #using Updated_ACI_qc AFTER filtering using Mike's do loop to get rid of points that failed the QC
# library(plantecophys)
# All_Acis= fitacis(Updated_ACI_qc, "fname")
# save(All_Acis, file = "All_Acis.RDA")
load(file = "All_Acis.RDA")

#plotting your ACI curves using plantecophys 
plot(All_Acis, how="manyplots")

#Get Vcmax and Jmax 
Variables = coef(All_Acis)

#Create df with location and genotype:
Location = c('a18','b06','g11','e10','f05','f08')
Genotype = c('European','European','European','Missouri x Washington','Missouri x Washington','Missouri x Washington')
loc_and_geno = data.frame(Location,Genotype, stringsAsFactors = FALSE)

#Pull date & loc from the fname 
#Add date & loc columns to the Variables df
Variables$Date = substring(Variables$fname, 14, 22)
Variables$Location = substring(Variables$fname, 8,10)

#Merge the loc_and_geno and Variables df's
All_Data = merge(loc_and_geno,Variables, by = "Location")
#Dropping the unecessary variables to get our summary dataframe
All_Data = All_Data[c("Date", "Location","Genotype", "Vcmax", "Jmax")]

#Changing the date from character to date object
All_Data$Date = strptime(All_Data$Date, "%m-%d-%y")
All_Data$Date = as.Date(All_Data$Date)

#Trying out some time series plots
library(dplyr)
just_Euro = dplyr::filter(All_Data, Genotype=="European")
plot(just_Euro$Date, just_Euro$Vcmax)

just_MoWa = dplyr::filter(All_Data, Genotype=="Missouri x Washington")
plot(just_MoWa$Date, just_MoWa$Vcmax)

#Trying to get the thickness data into the main data frame
load(file = "thickness_df.RDA")
merge(All_Data,thickness_df, by = c("Location", "Date"))

#Author: Karen Wang
#Date: 11/20/2015 * modified 11/29/15 to read in Asat
#Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
#Then creates summary df for work with stats

  library(devtools)
  library(ggplot2)
  library(plyr)
  library(dplyr)
  library(scales)
  library(plantecophys)

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
load("KarenACI_qc.RDA")
load("KarenACI_qc2.RDA")
load("KarenACI_qc3.RDA")
load("KarenACI_qc4.RDA")
load("KarenACI_qc5.RDA")
load("KarenACI_qc6.RDA")
#Combining the six df's
Final_ACI_qc = rbind(KarenACI_qc, KarenACI_qc2, KarenACI_qc3, KarenACI_qc4, KarenACI_qc5, KarenACI_qc6)
#Dropping the "fail all" qc's because they mess up the fitacis function
Final_ACI_qc=droplevels(Final_ACI_qc,c(fname = "b2 pop f08 kw 11-13-15","b2 pop e10 kw 11-13-15","b2 pop f05 kw 10-31-15"))
save(Final_ACI_qc, file = "Final_ACI_qc.RDA")

#fitaci w/ plantecophys
load("Final_ACI_qc.RDA")
# Final_Acis= fitacis(Final_ACI_qc, "fname")
# save(Final_Acis, file = "Final_Acis.RDA")
load(file = "Final_Acis.RDA")

#plotting all 52 ACI curves using plantecophys 
plot(Final_Acis, how="manyplots")

#Get Vcmax and Jmax 
Variables = coef(Final_Acis)

#Pull date & loc from the fname 
#Add date & loc columns to the Variables df
Variables$Date = substring(Variables$fname, 14, 22)
Variables$Location = substring(Variables$fname, 8,10)
#Changing the date from character to date object
Variables$Date = strptime(Variables$Date, "%m-%d-%y")
Variables$Date = as.Date(Variables$Date)

#Adding thickness to the main data frame
load("~/B2-Honors-Proj/data/thickness_df.RDA")
Variables = dplyr::mutate(Variables, Location = toupper(Variables$Location))
Thick_and_Data = merge(Variables,thickness_df, by = c("Location", "Date"), all = TRUE)
save(Thick_and_Data, file = "Thick_and_Data.RDA")

#Merge in LMA data
load("Thick_and_Data.RDA")
load("~/B2-Honors-Proj/data/LMA_df.RDA")
LMA_thick_data = merge(Thick_and_Data, LMA_df, by = c("Location", "Date"), all = TRUE)

#Create df with location and genotype:
Location = c('A18','B06','G11','E10','F05','F08')
Genotype = c('European','European','European','Missouri x Washington','Missouri x Washington','Missouri x Washington')
loc_and_geno = data.frame(Location,Genotype, stringsAsFactors = FALSE)

#Merge in the loc_and_geno df
LMA_thick_data = merge(loc_and_geno,LMA_thick_data, by = "Location")
#Dropping the unecessary variables to get our summary dataframe
LMA_thick_data =LMA_thick_data[c("Date", "Location","Genotype", "Vcmax", "Jmax", "Vcmax_SE", "Jmax_SE", "Avg.Thickness.mm","LMA")]
save(LMA_thick_data, file = "~/B2-Honors-Proj/LMA_thick_data.RDA")

#Filter for getting Asat
Asat_df = dplyr::filter(Final_ACI_qc, CO2R >= 390)
Asat_df = dplyr::filter(try, CO2R <= 410)
Asat_df = Asat_df[c("fname", "Photo")]
names(Asat_df)[names(Asat_df)=="Photo"] <- "Asat"

load("Thick_and_Data.RDA")
load("~/B2-Honors-Proj/data/LMA_df.RDA")
LMA_thick_data = merge(Thick_and_Data, LMA_df, by = c("Location", "Date"), all = TRUE)

Location = c('A18','B06','G11','E10','F05','F08')
Genotype = c('European','European','European','Missouri x Washington','Missouri x Washington','Missouri x Washington')
loc_and_geno = data.frame(Location,Genotype, stringsAsFactors = FALSE)

LMA_thick_data = merge(loc_and_geno,LMA_thick_data, by = "Location")
Asat_data = merge(LMA_thick_data, Asat_df, by = "fname")
Asat_data =Asat_data[c("Date", "Location","Genotype","Asat")]
save(Asat_data, file = "Asat_data.RDA")

#Making a df for conductance:
by_fname = dplyr::group_by(Final_ACI_qc, fname)
mean_Cond = dplyr::summarise(by_fname, mean(Cond))
mean_Cond$Date = substring(mean_Cond$fname, 14, 22)
mean_Cond$Location = substring(mean_Cond$fname, 8,10)
mean_Cond = dplyr::mutate(mean_Cond, Location = toupper(mean_Cond$Location))

Location = c('A18','B06','G11','E10','F05','F08')
Genotype = c('European','European','European','Missouri x Washington','Missouri x Washington','Missouri x Washington')
loc_and_geno = data.frame(Location,Genotype, stringsAsFactors = FALSE)

Cond = merge(loc_and_geno, mean_Cond, by = "Location")
save(Cond, file = "Cond.RDA")
#Author: Karen Wang
#Date: 11/06/2015
#Purpose: Read in all Licor files and do QC with PEcAn, fit A/Ci curve with plantecophys

#Installing PEcAn.photosynthesis as a stand alone
if (!require("PEcAn.photosynthesis",character.only = TRUE))
{
  library(devtools)
  library(ggplot2)
  install_github("PecanProject/pecan/modules/photosynthesis") 
}

#Create objects called filepath & tempFilelist
filepath="./data/"
tempFilelist = list.files(filepath,pattern="b2 pop")

##Reading data files using PEcAn.photosynthesis function read.Licor
#Preferred way: load files to a list
setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
cpath = getwd()
inbetween = "/data/"
#Creates KarenFiles object that is a list of the file locations
KarenFiles = paste0(cpath,inbetween,tempFilelist)

#One way to do it (for use with ggplot)
myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.Licor(x)))

##Drawing some plots with GGPLOT2
library(ggplot2)
#Sets up the type of plot
aci_plot = ggplot(data=myfiles, aes(x=Ci, y=Photo))

##This draws each curve in a different frame using "facet_wrap"
aci_plot + facet_wrap(~ fname) + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")
#See some outliers and weird stuff going, so need to perform a QC

##Perform QA/QC to get rid of outliers
#Make master list and apply read.Licor
KarenMaster = lapply(KarenFiles, read.Licor)
#Run QA/QC on all files in KarenMaster
#for all the files in KarenMaster
for(i in 1:length(KarenMaster)){
  KarenMaster[[i]] = Licor.QC(KarenMaster[[i]],curve = "ACi")
}
#This may take a while, just let it go.

#Save the QC
save(KarenMaster, file="KarenMaster.Rda")

load("KarenMaster.Rda")

#Combine data into one dataframe
KarenACI_qc = do.call("rbind", KarenMaster)
# 
# ## USE DPLYR to extract the QC passes (QC=1)
# library(dplyr)
# dplyr::tbl_df(KarenACI_qc)
# KarenACI_qc_filt = filter(KarenACI_qc, QC == 1)


##Another way to do it
if("QC" %in% colnames(KarenACI_qc)){
 KarenACI_qc = KarenACI_qc[-which(KarenACI_qc$QC < 1),]  
} else{
 QC = rep(1,nrow(KarenACI_qc))
 KarenACI_qc = cbind(KarenACI_qc,QC)
 }

save (KarenACI_qc, file="KarenACI_qc.RDA")
library(plantecophys)

##Stuck here, getting error about having zero observations 
##in my group variable

# #Troubleshoot using just one curve that I know can be fitted 
# #with fitaci
# test_Aci = dplyr::filter(KarenACI_qc, fname == "b2 pop a18 kw 09-18-15 edit")
# single_Aci = fitaci(test_Aci, Tcorrect=FALSE)
# plot(single_Aci)
# #which works

#sets up the type of plot
aci_plot <- ggplot(data=KarenACI_qc, aes(x=Ci, y=Photo))


#this draws all the curves on one plot
aci_plot + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")

aci_plot + facet_wrap(~ fname) + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")
#using fitacis from plantecophys to fit all curves at once.

#find out the bad ACI curve

#create df from Master list

KarenACI_qc_check = do.call("rbind", KarenMaster)

library(doBy)

summaryBy(QC~fname, data=KarenACI_qc_check,  FUN=c(mean,var,length))

#It's "b2 pop f05 kw 09-18-15 redo" !

KarenACI_qc=droplevels(KarenACI_qc,fname="b2 pop f05 kw 09-18-15 redo")
unique(KarenACI_qc$fname)


#using KarenACI_qc AFTER filtering using Mike's do loop to get ride of points that failed the QC
All_Acis= fitacis(KarenACI_qc, "fname")

#plotting your ACI curves using plantecophys 
plot(All_Acis, how="manyplots")

#Get Vcmax and Jmax 
Variables = coef(All_Acis)

#Interesting correlation:
plot(Variables$Jmax,Variables$Vcmax)

#some things to look at 
#https://github.com/CourtneyCampany/EucPVE/blob/master/clean%20Scripts/fitaci.R 

#we need to do some stats

# need to compare genotypes
# need to compare between weeks
# is the difference between weeks the same for both genotypes

#we need a df with LOCATION and Genotype
#we need a df with week or date

#BUILD df with location and genotype
#CREATE LOC and Data within the summary dataset of 
#join with summary dataset of parameters (vcmax and Jmax)

# Look up how R deals with date and time. 
# > substr("karen",1,1)
# [1] "k"
# > substr("karen",1,3)
# [1] "kar"
# > substr("karen",3,3)
# [1] "r"
# > substr("karen",3,2)
# [1] ""
# > substr("karen",3,5)
# [1] "ren"

#WULLSCHLEGER, S. D. (1993). Biochemical limitations to carbon assimilation in C3 plants—a retrospective analysis of the A/Ci curves from 109 species. Journal of Experimental Botany, 44(5), 907-920.
#Rogers, A. (2014). The use and misuse of V c, max in Earth System Models. Photosynthesis research, 119(1-2), 15-29.

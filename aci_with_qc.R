#Author: Karen Wang
#Date: 11/02/2015
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

## USE DPLYR to extract the QC passes (QC=1)
library(dplyr)
dplyr::tbl_df(KarenACI_qc)
dplyr::filter(KarenACI_qc, QC == 1)

## Another way to do it
# if("QC" %in% colnames(KarenACI_qc)){
#   KarenACI_qc = KarenACI_qc[-which(KarenACI_qc$QC < 1),]  
# } else{
#   QC = rep(1,nrow(KarenACI_qc))
#   KarenACI_qc = cbind(KarenACI_qc,QC)
# }

library(plantecophys)

##Stuck here, getting error about having zero observations 
##in my group variable
#using fitacis from plantecophys to fit all curves at once.
KarenAcis= fitacis(KarenACI_qc, "fname")


#plotting your ACI curves using plantecophys 
plot(KarenAcis, how="manyplots")

#Get Vcmax and Jmax 
coef(KarenAcis)


#Author: Karen Wang
#Date: 10/26/2015
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
#One way to do it
#myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.Licor(x)))

#Preferred way: load files to a list
setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
cpath = getwd()
inbetween = "/data/"
#Creates KarenFiles object that is a list of the file locations
KarenFiles = paste0(cpath,inbetween,tempFilelist)

##Drawing some plots with GGPLOT2
library(ggplot2)
#Sets up the type of plot
aci_plot = ggplot(data=myfiles, aes(x=Ci, y=Photo))


# #This draws all the curves on one plot (not preferred)
# #aci_plot + 
# #  geom_point(colour="black", size = 2.5) +
# #theme_classic() +
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=22,face="bold")) + 
#   theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
#   theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
#   ylab("Assimilation (umol/m2/sec)")+
#   xlab("Ci")

#this draws each curve in a different frame using "facet_wrap"
aci_plot + facet_wrap(~ fname) + 
  geom_point(colour="black", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci")

##Perform QA/QC to get rid of outliers
#Make master list and apply read.Licor
KarenMaster = lapply(KarenFiles, read.Licor)
#Run QA/QC on all files in KarenMaster
#for all the files in KarenMaster
for(i in 1:length(KarenMaster)){
  KarenMaster[[i]] = Licor.QC(KarenMaster[[i]],curve = "ACi")
}
#this may take a while, just let it go.


#using fitacis from plantecophys to fit all curves at once.
Myacis= fitacis(myfiles, "fname")


#plotting your ACI curves using plantecophys 
plot(Myacis, how="manyplots")

#Get Vcmax and Jmax 
coef(Myacis)

#using dplyr

TPUest = ungroup(myfiles) %>% 
  
  arrange(fname,Ci) %>%
  group_by(fname) %>% 
  
  mutate(deltaPhoto = Photo - lag(Photo, default = 0)) %>% #calculate difference in photosynthesis from Ci to CI
  
  mutate(TPUlim = as.numeric(deltaPhoto < 0)) #indexing whether there is TPU limitation
#think about "select" "filter"



# #extract coefficients from the data
# IsopreneACI_coef <- coef(IsopreneACI_fitsbycurve)
# 
# 
# #select variables that define treatments
# Trts_IsopreneACI00 = IsopreneACIs_outlyrsRmoved %>%
#   select(line, dateMeas, ACIgroups, Tleaf)
# 
# #reduce to unique definition rows
# Trts_IsopreneACI = Trts_IsopreneACI00 %>%
#   distinct(ACIgroups)
# 
# #merge data frames to regain the Genotype and Temperature
# IsopreneACI_coef_byTRT01 = inner_join(Trts_IsopreneACI,IsopreneACI_coef, by="ACIgroups" )
# 
# IsopreneACI_coef_byTRT = IsopreneACI_coef_byTRT01  %>%
#   mutate(Genotype=line, MeasDate=dateMeas)   %>%
#   group_by(Genotype, Tleaf)   %>%
#   select(-dateMeas,-line)
# 
# 
# save(IsopreneACI_coef_byTRT, file="IsopreneACI_coef_byTRT.Rda")
# save(IsopreneACI_coef_byTRT01, file="IsopreneACI_coef_byTRT01.Rda")
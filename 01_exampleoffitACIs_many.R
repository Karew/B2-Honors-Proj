#Author Dave Moore
#Date: 10/14/2015
#Purpose Example on use of dplyr and plantecophys package


#intall devtools
#install rjags 
#install Rtools

#installing PEcAn.photosynthesis as a stand alone
if (!require("PEcAn.photosynthesis",character.only = TRUE))
{
  library(devtools)
  library(ggplot2)
  install_github("PecanProject/pecan/modules/photosynthesis") 
}

#note you need rjags installed in R and also JAGS (stand alone application) installed on your computer
#note you will also need Rtools installed.

#note you will get an error 

#create objects called filepath & tempFilelist
filepath="./data/"
tempFilelist = list.files(filepath,pattern="*.*")

#reading data files using pecan.photosynthesis function read.licor
myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.Licor(x)))


#drawing some plots with GGPLOT2

#sets up the type of plot
aci_plot <- ggplot(data=myfiles, aes(x=Ci, y=Photo))


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
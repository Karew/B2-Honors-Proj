library(plantecophys)
library(PEcAn.photosynthesis)
#Loading in the more data
filepath3="./data3/"
tempFilelist3 = list.files(filepath3,pattern="b2 pop")

setwd("C:/Users/Karen/Documents/B2-Honors-Proj")
cpath3 = getwd()
inbetween3 = "/data3/"

KarenFiles3 = paste0(cpath3,inbetween3,tempFilelist3)

KarenMaster3 = lapply(KarenFiles3, read.Licor)

#dropping the file for f05 on 11/13/15 because it was one log for the Asat 
#and wouldn't do an A/Ci
KarenMaster3[15]=NULL

for(i in 1:length(KarenMaster3)){
  KarenMaster3[[i]] = Licor.QC(KarenMaster3[[i]],curve = "ACi")
}

#Save the QC
save(KarenMaster3, file="KarenMaster3.Rda")
#Combine data into one dataframe
load("KarenMaster3.Rda")
KarenACI_qc3 = do.call("rbind", c(KarenMaster3[1:6], KarenMaster3[8], KarenMaster3[14:17], KarenMaster3[19:20], KarenMaster3[22]))

if("QC" %in% colnames(KarenACI_qc3)){
  KarenACI_qc3 = KarenACI_qc3[-which(KarenACI_qc3$QC < 1),]  
} else{
  QC = rep(1,nrow(KarenACI_qc3))
  KarenACI_qc3 = cbind(KarenACI_qc3,QC)
}
KarenACI_qc3$study=NULL
KarenACI_qc3$plot=NULL
save(KarenACI_qc3, file = "KarenACI_qc3.RDA")

##DOING ANOTHER LOAD FOR THE REST OF THE A/CI's that had different prompts
load("KarenMaster3.Rda")
KarenACI_qc4 = do.call("rbind", c(KarenMaster3[7], KarenMaster3[18]))

if("QC" %in% colnames(KarenACI_qc4)){
  KarenACI_qc4 = KarenACI_qc4[-which(KarenACI_qc4$QC < 1),]  
} else{
  QC = rep(1,nrow(KarenACI_qc4))
  KarenACI_qc4 = cbind(KarenACI_qc4,QC)
}
KarenACI_qc4$node=NULL
KarenACI_qc4$aux2=NULL
KarenACI_qc4$aux1=NULL

save(KarenACI_qc4, file = "KarenACI_qc4.RDA")

load("KarenMaster3.Rda")
KarenACI_qc5 = data.frame(KarenMaster3[12])
KarenACI_qc5$study.experiment.initials=NULL
KarenACI_qc5$plot=NULL

if("QC" %in% colnames(KarenACI_qc5)){
  KarenACI_qc5 = KarenACI_qc5[-which(KarenACI_qc5$QC < 1),]  
} else{
  QC = rep(1,nrow(KarenACI_qc5))
  KarenACI_qc5 = cbind(KarenACI_qc5,QC)
}
save(KarenACI_qc5, file = "KarenACI_qc5.RDA")

load("KarenMaster3.Rda")
KarenACI_qc6 = do.call("rbind", c(KarenMaster3[9], KarenMaster3[13], KarenMaster3[21]))

if("QC" %in% colnames(KarenACI_qc6)){
  KarenACI_qc6 = KarenACI_qc6[-which(KarenACI_qc6$QC < 1),]  
} else{
  QC = rep(1,nrow(KarenACI_qc6))
  KarenACI_qc6 = cbind(KarenACI_qc6,QC)
}
KarenACI_qc6$study=NULL
KarenACI_qc6$plot=NULL
save(KarenACI_qc6, file = "KarenACI_qc6.RDA")


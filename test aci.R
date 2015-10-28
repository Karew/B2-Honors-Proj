# #code scraps
# 
# 
# To save a file
save(aci_2014_list, file="aci_2014_list_qc.Rda")

load("aci_2014_list_qc.Rda")


#ian's code for photosynthesis
# https://github.com/davidjpmoore/B2Pop_Physiology

# Dave's isoprene code'
#https://github.com/davidjpmoore/Isoprene2015_B2Pop


KarenACI_qc <- do.call("rbind", KarenMaster)

library(dplyr)
# 
# USE DPLYR to filter out the QC^=1

#using fitacis from plantecophys to fit all curves at once.
Myacis= fitacis(KarenACI_qc, "fname")




# Karen Wang
# B2 Honors Project

#This reads in data
aci_test = read.table(
  "data//b2 pop a18 09-18-15",skip = 18,
  sep="\t", header=FALSE)

#This reads in headers
aci_test_header = read.table(
  "data//b2 pop a18 09-18-15",skip = 16, nrows = 1,stringsAsFactors = FALSE,
  sep="\t", header=FALSE)

#This lists our column names
aci_columns= as.list(aci_test_header)
colnames(aci_test)=aci_columns

#This plots Ci on the x and A on the y
plot(aci_test$Ci,aci_test$Photo,main = "A Ci Curve", ylab = "Photosynthesis", xlab = "Ci")

#making a list of all the files I want to read in from data folder
aci_file_list=list.files(path="./data/",pattern=NULL)

#starting a for loop 
for(n in 1:length(aci_file_list))
{
  print(aci_file_list[n])  
}
#need to make loop that will load in all my data
for(n in 1:length(aci_file_list))
{
  
}
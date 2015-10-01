#read in data
aci_test <- read.table(
  "data//b2 pop a18 09-18-15",skip = 18,
  sep="\t", header=FALSE)
#read in headers
aci_test_header <- read.table(
  "data//b2 pop a18 09-18-15",skip = 16, nrows = 1,stringsAsFactors = FALSE,
  sep="\t", header=FALSE)
#list
aci_columns= as.list(aci_test_header)
colnames(aci_test)=aci_columns

#plot
plot(aci_test$Ci,aci_test$Photo)
#making a list of all the files i want to read in
aci_file_list=list.files(path="./data/",pattern=NULL)


for(n in 1:length(aci_file_list))
{
print(aci_file_list[n])  
}
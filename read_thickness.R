#Author: Karen Wang
#Date: 11/15/2015
#Purpose:reading in the leaf thickness data from an Excel csv 
#saving it as a data frame 
setwd("C:/Users/Karen/Documents/B2-Honors-Proj/data/")
thickness_data=read.table("b2 thickness.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
thickness_df= as.data.frame(thickness_data)
thickness_df=thickness_df[c("Date", "Location", "Node", "Avg.Thickness.mm")]
thickness_df$Date = strptime(thickness_df$Date, "%m/%d/%y")
thickness_df$Date = as.Date(thickness_df$Date)
save(thickness_df, file = "thickness_df.RDA")

#read in LMA from Excel csv
setwd("C:/Users/Karen/Documents/B2-Honors-Proj/data/")
LMA_data=read.table("b2 LMA.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
LMA_df= as.data.frame(LMA_data)
LMA_df=LMA_df[c("Date", "Location", "LMA")]
LMA_df$Date = strptime(LMA_df$Date, "%m/%d/%Y")
LMA_df$Date = as.Date(LMA_df$Date)
save(LMA_df, file = "LMA_df.RDA")


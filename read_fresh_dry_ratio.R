#Author: Karen Wang
#Date: 11/22/2015 *modified 11/29/15 to get the leaf mass per volume
#Purpose: Builds ratio of fresh to dry weight into the main dataframe

#read in Excel csv
setwd("C:/Users/Karen/Documents/B2-Honors-Proj/data/")
fresh_dry_ratio=read.table("b2 LMA.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
fresh_dry_ratio= as.data.frame(fresh_dry_ratio)
fresh_dry_ratio=fresh_dry_ratio[c("Date", "Location", "Fresh_Weight_mg", "Dry_Weight_mg")]
fresh_dry_ratio$Date = strptime(fresh_dry_ratio$Date, "%m/%d/%Y")
fresh_dry_ratio$Date = as.Date(fresh_dry_ratio$Date)

#set up the ratio column in the df
Ratio = fresh_dry_ratio$Fresh_Weight_mg/fresh_dry_ratio$Dry_Weight_mg
fresh_dry_ratio = cbind(fresh_dry_ratio, Ratio)
fresh_dry_ratio=fresh_dry_ratio[c("Date", "Location", "Ratio", "Fresh_Weight_mg", "Dry_Weight_mg")]

#merge the ratio df with the main df
load("~/B2-Honors-Proj/main_df.RDA")
main_df=merge(main_with_met, fresh_dry_ratio, by = c("Date", "Location"), all = TRUE)

#LEAF MASS PER VOLUME - a metric that we kind of made up where we multiply
#LMA by inverse leaf thickness
load("~/B2-Honors-Proj/main_df.RDA")
inverse_thick = 1/main_df$Avg.Thickness.mm
LMV = main_df$LMA * inverse_thick
main_df = cbind(main_df, LMV)

setwd("~/B2-Honors-Proj")
save(main_df, file = "main_df.RDA")


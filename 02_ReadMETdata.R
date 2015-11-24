#Purpose: to read in met data and carry out basic analysis
#Author Dave Moore
#Date: 03/12/2015 *modified 11/23/2015

#B2 Met tower data should be stored on iPlant under B2Poplars/MASTERDATA/MetData

#download B2PlantationMetData092013_112015.csv & associated README file

#HEADER
# TIMESTAMP  RECORD	AirTC_Avg	RH	WS_ms_Avg	WindDir	Rain_mm_Tot	PAR_Den_Avg	PAR_Tot_Tot	PAR_Den
# TS	RN	Deg C	%	meters/second	degrees	mm	umol/s/m^2	mmol/m^2	umol/s/m^2
# Avg	Smp	Avg	Smp	Tot	Avg	Tot	Smp

#NOTE: from Ian Shiach: Time zone is Arizona (Mountain Standard). (3/9/2015), the time was one hour fast (the 11:45AM point was really 10:45AM). Time was reset to Arizona (Mountain Standard) on 3/9/2015.

#READ MET DATA FROM CSV FILE
Met_dat = read.csv("data//B2PlantationMetData092013_112015.csv",skip=5,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

#READ MET DATA HEADER FROM CSV FILE
Met_header = read.csv("data//B2PlantationMetData092013_112015.csv",skip=1,nrows=1,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)
#create LIST from header data frame
MetCols = as.list(Met_header)

#READ MET DATA UNITS FROM CSV FILE
Met_units  = read.csv("data//B2PlantationMetData092013_112015.csv",skip=2,nrows=2,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

#apply column headers to Met_dat
colnames(Met_dat) <- MetCols

#apply column headers to Met_units
colnames(Met_units) <- MetCols

#strip out the the leading text of each element of TIMESTEP
datestamp = gsub( " .*$", "", Met_dat$TIMESTAMP )
#apply date format to the text
Met_dat$rDate <- as.Date(datestamp,"%Y-%m-%d")

#apply date format to TIMESTAMP directly | this works I haven't tried extracting the time of day from it yet
Met_dat$rDateT <- as.Date(Met_dat$TIMESTAMP, "%Y-%m-%d %H:%M")

# Plots - these are VERY BIG files
library(ggplot2)
 p <- ggplot(Met_dat, aes(rDateT, AirTC_Avg))
 p + geom_point(colour = "red", size = 3)

#Need to filter the relevant time period
#Need to build a df that has data and avg. air temp
library(dplyr)
relevant_met = dplyr::filter(Met_dat, RECORD >="199549")
relevant_met = dplyr::filter(relevant_met, RECORD <= "217980")
save(relevant_met, file = "relevant_met.RDA")

# Plot
met_plot <- ggplot(relevant_met, aes(rDateT, AirTC_Avg))
met_plot + geom_point(colour = "red", size = 3)

load("~/B2-Honors-Proj/relevant_met.RDA")
Sep_11 = dplyr::filter(relevant_met, RECORD >="199549")
Sep_11 = dplyr::filter(Sep_11, RECORD <="199836")
met_date1 = substring(Sep_11$TIMESTAMP, 1,10)
met_avg_temp1 = mean(Sep_11$AirTC_Avg)
Sep_11 = as.data.frame(cbind(met_date1[1], met_avg_temp1[1]))
Sep_11$V1 = as.Date(Sep_11$V1)

Sep_18 = dplyr::filter(relevant_met, RECORD >="201565")
Sep_18 = dplyr::filter(Sep_18, RECORD <="201852")
met_date2 = substring(Sep_18$TIMESTAMP, 1,10)
met_avg_temp2 = mean(Sep_18$AirTC_Avg)
Sep_18 = as.data.frame(cbind(met_date2[1], met_avg_temp2[1]))
Sep_18$V1 = as.Date(Sep_18$V1)

Sep_25 = dplyr::filter(relevant_met, RECORD >="203581")
Sep_25 = dplyr::filter(Sep_25, RECORD <="203868")
met_date3 = substring(Sep_25$TIMESTAMP, 1,10)
met_avg_temp3 = mean(Sep_25$AirTC_Avg)
Sep_25 = as.data.frame(cbind(met_date3[1], met_avg_temp3[1]))
Sep_25$V1 = as.Date(Sep_25$V1)

Oct_02 = dplyr::filter(relevant_met, RECORD >="205597")
Oct_02 = dplyr::filter(Oct_02, RECORD <="205884")
met_date4 = substring(Oct_02$TIMESTAMP, 1,10)
met_avg_temp4 = mean(Oct_02$AirTC_Avg)
Oct_02 = as.data.frame(cbind(met_date4[1], met_avg_temp4[1]))
Oct_02$V1 = as.Date(Oct_02$V1)

Oct_09 = dplyr::filter(relevant_met, RECORD >="207613")
Oct_09 = dplyr::filter(Oct_09, RECORD <="207900")
met_date5 = substring(Oct_09$TIMESTAMP, 1,10)
met_avg_temp5 = mean(Oct_09$AirTC_Avg)
Oct_09 = as.data.frame(cbind(met_date5[1], met_avg_temp5[1]))
Oct_09$V1 = as.Date(Oct_09$V1)

Oct_16 = dplyr::filter(relevant_met, RECORD >="209629")
Oct_16 = dplyr::filter(Oct_16, RECORD <="209916")
met_date6 = substring(Oct_16$TIMESTAMP, 1,10)
met_avg_temp6 = mean(Oct_16$AirTC_Avg)
Oct_16 = as.data.frame(cbind(met_date6[1], met_avg_temp6[1]))
Oct_16$V1 = as.Date(Oct_16$V1)

Oct_23 = dplyr::filter(relevant_met, RECORD >="211645")
Oct_23 = dplyr::filter(Oct_23, RECORD <="211932")
met_date7 = substring(Oct_23$TIMESTAMP, 1,10)
met_avg_temp7 = mean(Oct_23$AirTC_Avg)
Oct_23 = as.data.frame(cbind(met_date7[1], met_avg_temp7[1]))
Oct_23$V1 = as.Date(Oct_23$V1)

Oct_31 = dplyr::filter(relevant_met, RECORD >="213949")
Oct_31 = dplyr::filter(Oct_31, RECORD <="214236")
met_date8 = substring(Oct_31$TIMESTAMP, 1,10)
met_avg_temp8 = mean(Oct_31$AirTC_Avg)
Oct_31 = as.data.frame(cbind(met_date8[1], met_avg_temp8[1]))
Oct_31$V1 = as.Date(Oct_31$V1)

Nov_06 = dplyr::filter(relevant_met, RECORD >="215677")
Nov_06 = dplyr::filter(Nov_06, RECORD <="215964")
met_date9 = substring(Nov_06$TIMESTAMP, 1,10)
met_avg_temp9 = mean(Nov_06$AirTC_Avg)
Nov_06 = as.data.frame(cbind(met_date9[1], met_avg_temp9[1]))
Nov_06$V1 = as.Date(Nov_06$V1)

Nov_13 = dplyr::filter(relevant_met, RECORD >="217693")
Nov_13 = dplyr::filter(Nov_13, RECORD <="217980")
met_date10 = substring(Nov_13$TIMESTAMP, 1,10)
met_avg_temp10 = mean(Nov_13$AirTC_Avg)
Nov_13 = as.data.frame(cbind(met_date10[1], met_avg_temp10[1]))
Nov_13$V1 = as.Date(Nov_13$V1)

met_df=rbind(Sep_11, Sep_18, Sep_25, Oct_02, Oct_09, Oct_16, Oct_23, Oct_31, Nov_06, Nov_13)

#Merge met_df into the main df
load("~/B2-Honors-Proj/LMA_thick_data.RDA")
main_with_met = merge(LMA_thick_data, met_df, by.x ="Date", by.y ="V1", all = TRUE)
names(main_with_met)[names(main_with_met)=="V2"] <- "Avg_Temp"
main_with_met$Avg_Temp = as.numeric(as.character(main_with_met$Avg_Temp))
save(main_with_met, file = "main_df.RDA")
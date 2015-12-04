#Author: Karen Wang
#Date: 11/29/2015
#Purpose: Plots and stats

library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(grid)

load("~/B2-Honors-Proj/main_df.RDA")
load("~/B2-Honors-Proj/Final_Acis.RDA")
load("~/B2-Honors-Proj/Asat_data.RDA")
load("~/B2-Honors-Proj/Final_ACI_qc.RDA")

#PLOT AVG VCMAX VS. TIME 
#Separating the genotypes
just_Euro = dplyr::filter(main_df, Genotype=="European")
just_MoWa = dplyr::filter(main_df, Genotype=="Missouri x Washington")

#MO/WA Vcmax time series with error bars and color scale by temp
Sept_11 = dplyr::filter(just_MoWa, Date == "2015-09-11")
Vcmax_mean = mean(Sept_11$Vcmax)
Vcmax_se = sum(Sept_11$Vcmax_SE)
Sept_11 = as.data.frame(cbind(Sept_11,Vcmax_mean, Vcmax_se))
Sept_11 = head(Sept_11,1)
Sept_18 = dplyr::filter(just_MoWa, Date == "2015-09-18")
Vcmax_mean = mean(Sept_18$Vcmax)
Vcmax_se = sum(Sept_18$Vcmax_SE)
Sept_18 = as.data.frame(cbind(Sept_18,Vcmax_mean, Vcmax_se))
Sept_18 = head(Sept_18,1)
Sept_25 = dplyr::filter(just_MoWa, Date == "2015-09-25")
Vcmax_mean = mean(Sept_25$Vcmax)
Vcmax_se = sum(Sept_25$Vcmax_SE)
Sept_25 = as.data.frame(cbind(Sept_25,Vcmax_mean, Vcmax_se))
Sept_25 = head(Sept_25,1)
Oct_02 = dplyr::filter(just_MoWa, Date == "2015-10-02")
Vcmax_mean = mean(Oct_02$Vcmax)
Vcmax_se = sum(Oct_02$Vcmax_SE)
Oct_02 = as.data.frame(cbind(Oct_02,Vcmax_mean, Vcmax_se))
Oct_02 = head(Oct_02,1)
Oct_09 = dplyr::filter(just_MoWa, Date == "2015-10-09")
Vcmax_mean = mean(Oct_09$Vcmax)
Vcmax_se = sum(Oct_09$Vcmax_SE)
Oct_09 = as.data.frame(cbind(Oct_09,Vcmax_mean, Vcmax_se))
Oct_09 = head(Oct_09,1)
Oct_16 = dplyr::filter(just_MoWa, Date == "2015-10-16")
Vcmax_mean = mean(Oct_16$Vcmax)
Vcmax_se = sum(Oct_16$Vcmax_SE)
Oct_16 = as.data.frame(cbind(Oct_16,Vcmax_mean, Vcmax_se))
Oct_16 = head(Oct_16,1)
Oct_23 = dplyr::filter(just_MoWa, Date == "2015-10-23")
Vcmax_mean = mean(Oct_23$Vcmax)
Vcmax_se = sum(Oct_23$Vcmax_SE)
Oct_23 = as.data.frame(cbind(Oct_23,Vcmax_mean, Vcmax_se))
Oct_23 = head(Oct_23,1)
Oct_31 = dplyr::filter(just_MoWa, Date == "2015-10-31")
Oct_31[is.na(Oct_31)] <- 0
Vcmax_mean = mean(Oct_31$Vcmax)
Vcmax_se = sum(Oct_31$Vcmax_SE)
Oct_31 = as.data.frame(cbind(Oct_31,Vcmax_mean, Vcmax_se))
Oct_31 = head(Oct_31,1)
Nov_06 = dplyr::filter(just_MoWa, Date == "2015-11-06")
Nov_06[is.na(Nov_06)] <- 0
Vcmax_mean = mean(Nov_06$Vcmax)
Vcmax_se = sum(Nov_06$Vcmax_SE)
Nov_06 = as.data.frame(cbind(Nov_06,Vcmax_mean, Vcmax_se))
Nov_06 = head(Nov_06,1)
Nov_13 = dplyr::filter(just_MoWa, Date == "2015-11-13")
Nov_13[is.na(Nov_13)] <- 0
Vcmax_mean = mean(Nov_13$Vcmax)
Vcmax_se = sum(Nov_13$Vcmax_SE)
Nov_13 = as.data.frame(cbind(Nov_13,Vcmax_mean, Vcmax_se))
Nov_13 = head(Nov_13,1)

MoWa_Vcmax = rbind(Sept_11, Sept_18, Sept_25, Oct_02, Oct_09, Oct_16, Oct_23, Oct_31, Nov_06)
MoWa_Vcmax = MoWa_Vcmax[c("Date", "Genotype", "Avg_Temp_degC", "Min_Temp_degC",
                          "Avg.Thickness.mm", "LMA", "LMV", "Fresh_Weight_mg", 
                          "Dry_Weight_mg", "Ratio", "Vcmax_mean", "Vcmax_se")]

MoWa_Vcmax_plot = ggplot(MoWa_Vcmax, aes(Date, Vcmax_mean))
MoWa_Vcmax_plot + geom_point(aes(colour = Avg_Temp_degC), size=5) +
  geom_abline(aes(intercept=31.41971, slope=0)) +
  scale_colour_gradient(low="deepskyblue1", high="red") +
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Vcmax time series") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  geom_errorbar(aes(ymax=Vcmax_mean+Vcmax_se, ymin=Vcmax_mean-Vcmax_se),width=3) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#EURO Vcmax time series with error bars and color scale by temp
Sept_11.2 = dplyr::filter(just_Euro, Date == "2015-09-11")
Vcmax_mean2 = mean(Sept_11.2$Vcmax)
Vcmax_se2 = sum(Sept_11.2$Vcmax_SE)
Sept_11.2 = as.data.frame(cbind(Sept_11.2,Vcmax_mean2, Vcmax_se2))
Sept_11.2 = head(Sept_11.2,1)
Sept_18.2 = dplyr::filter(just_Euro, Date == "2015-09-18")
Vcmax_mean2 = mean(Sept_18.2$Vcmax)
Vcmax_se2 = sum(Sept_18.2$Vcmax_SE)
Sept_18.2 = as.data.frame(cbind(Sept_18.2,Vcmax_mean2, Vcmax_se2))
Sept_18.2 = head(Sept_18.2,1)
Sept_25.2 = dplyr::filter(just_Euro, Date == "2015-09-25")
Vcmax_mean2 = mean(Sept_25.2$Vcmax)
Vcmax_se2 = sum(Sept_25.2$Vcmax_SE)
Sept_25.2 = as.data.frame(cbind(Sept_25.2,Vcmax_mean2, Vcmax_se2))
Sept_25.2 = head(Sept_25.2,1)
Oct_02.2 = dplyr::filter(just_Euro, Date == "2015-10-02")
Vcmax_mean2 = mean(Oct_02.2$Vcmax)
Vcmax_se2 = sum(Oct_02.2$Vcmax_SE)
Oct_02.2 = as.data.frame(cbind(Oct_02.2,Vcmax_mean2, Vcmax_se2))
Oct_02.2 = head(Oct_02.2,1)
Oct_09.2 = dplyr::filter(just_Euro, Date == "2015-10-09")
Vcmax_mean2 = mean(Oct_09.2$Vcmax)
Vcmax_se2 = sum(Oct_09.2$Vcmax_SE)
Oct_09.2 = as.data.frame(cbind(Oct_09.2,Vcmax_mean2, Vcmax_se2))
Oct_09.2 = head(Oct_09.2,1)
Oct_16.2 = dplyr::filter(just_Euro, Date == "2015-10-16")
Vcmax_mean2 = mean(Oct_16.2$Vcmax)
Vcmax_se2 = sum(Oct_16.2$Vcmax_SE)
Oct_16.2 = as.data.frame(cbind(Oct_16.2,Vcmax_mean2, Vcmax_se2))
Oct_16.2 = head(Oct_16.2,1)
Oct_23.2 = dplyr::filter(just_Euro, Date == "2015-10-23")
Vcmax_mean2 = mean(Oct_23.2$Vcmax)
Vcmax_se2 = sum(Oct_23.2$Vcmax_SE)
Oct_23.2 = as.data.frame(cbind(Oct_23.2,Vcmax_mean2, Vcmax_se2))
Oct_23.2 = head(Oct_23.2,1)
Oct_31.2 = dplyr::filter(just_Euro, Date == "2015-10-31")
Oct_31.2[is.na(Oct_31.2)] <- 0
Vcmax_mean2 = mean(Oct_31.2$Vcmax)
Vcmax_se2 = sum(Oct_31.2$Vcmax_SE)
Oct_31.2 = as.data.frame(cbind(Oct_31.2,Vcmax_mean2, Vcmax_se2))
Oct_31.2 = head(Oct_31.2,1)
Nov_06.2 = dplyr::filter(just_Euro, Date == "2015-11-06")
Vcmax_mean2 = mean(Nov_06.2$Vcmax)
Vcmax_se2 = sum(Nov_06.2$Vcmax_SE)
Nov_06.2 = as.data.frame(cbind(Nov_06.2,Vcmax_mean2, Vcmax_se2))
Nov_06.2 = head(Nov_06.2,1)
Nov_13.2 = dplyr::filter(just_Euro, Date == "2015-11-13")
Vcmax_mean2 = mean(Nov_13.2$Vcmax)
Vcmax_se2 = sum(Nov_13.2$Vcmax_SE)
Nov_13.2 = as.data.frame(cbind(Nov_13.2,Vcmax_mean2, Vcmax_se2))
Nov_13.2 = head(Nov_13.2,1)

Euro_Vcmax = rbind(Sept_11.2, Sept_18.2, Sept_25.2, Oct_02.2, Oct_09.2, Oct_16.2, Oct_23.2, Oct_31.2, Nov_06.2, Nov_13.2)
Euro_Vcmax = Euro_Vcmax[c("Date", "Genotype", "Avg_Temp_degC", "Min_Temp_degC",
                          "Avg.Thickness.mm", "LMA", "LMV", "Fresh_Weight_mg", 
                          "Dry_Weight_mg", "Ratio", "Vcmax_mean2", "Vcmax_se2")]

Euro_Vcmax_plot = ggplot(Euro_Vcmax, aes(Date, Vcmax_mean2))
Euro_Vcmax_plot + geom_point(aes(colour = Avg_Temp_degC), size=5) +
  scale_colour_gradient(low="deepskyblue1", high="red") +
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("European genotype Vcmax time series") +
  geom_abline(aes(intercept=46.51862, slope=0)) +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  geom_errorbar(aes(ymax=Vcmax_mean2+Vcmax_se2, ymin=Vcmax_mean2-Vcmax_se2),width=3) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT JMAX VS. TIME 
#MO/WA Jmax time series with error bars
Sept_11.j = dplyr::filter(just_MoWa, Date == "2015-09-11")
Jmax_mean = mean(Sept_11.j$Jmax)
Jmax_se = sum(Sept_11.j$Jmax_SE)
Sept_11.j = as.data.frame(cbind(Sept_11.j,Jmax_mean, Jmax_se))
Sept_11.j = head(Sept_11.j,1)
Sept_18.j = dplyr::filter(just_MoWa, Date == "2015-09-18")
Jmax_mean = mean(Sept_18.j$Jmax)
Jmax_se = sum(Sept_18.j$Jmax_SE)
Sept_18.j = as.data.frame(cbind(Sept_18.j,Jmax_mean, Jmax_se))
Sept_18.j = head(Sept_18.j,1)
Sept_25.j = dplyr::filter(just_MoWa, Date == "2015-09-25")
Jmax_mean = mean(Sept_25.j$Jmax)
Jmax_se = sum(Sept_25.j$Jmax_SE)
Sept_25.j = as.data.frame(cbind(Sept_25.j,Jmax_mean, Jmax_se))
Sept_25.j = head(Sept_25.j,1)
Oct_02.j = dplyr::filter(just_MoWa, Date == "2015-10-02")
Jmax_mean = mean(Oct_02.j$Jmax)
Jmax_se = sum(Oct_02.j$Jmax_SE)
Oct_02.j = as.data.frame(cbind(Oct_02.j,Jmax_mean, Jmax_se))
Oct_02.j = head(Oct_02.j,1)
Oct_09.j = dplyr::filter(just_MoWa, Date == "2015-10-09")
Jmax_mean = mean(Oct_09.j$Jmax)
Jmax_se = sum(Oct_09.j$Jmax_SE)
Oct_09.j = as.data.frame(cbind(Oct_09.j,Jmax_mean, Jmax_se))
Oct_09.j = head(Oct_09.j,1)
Oct_16.j = dplyr::filter(just_MoWa, Date == "2015-10-16")
Jmax_mean = mean(Oct_16.j$Jmax)
Jmax_se = sum(Oct_16.j$Jmax_SE)
Oct_16.j = as.data.frame(cbind(Oct_16.j,Jmax_mean, Jmax_se))
Oct_16.j = head(Oct_16.j,1)
Oct_23.j = dplyr::filter(just_MoWa, Date == "2015-10-23")
Jmax_mean = mean(Oct_23.j$Jmax)
Jmax_se = sum(Oct_23.j$Jmax_SE)
Oct_23.j = as.data.frame(cbind(Oct_23.j,Jmax_mean, Jmax_se))
Oct_23.j = head(Oct_23.j,1)
Oct_31.j = dplyr::filter(just_MoWa, Date == "2015-10-31")
Oct_31.j[is.na(Oct_31.j)] <- 0
Jmax_mean = mean(Oct_31.j$Jmax)
Jmax_se = sum(Oct_31.j$Jmax_SE)
Oct_31.j = as.data.frame(cbind(Oct_31.j,Jmax_mean, Jmax_se))
Oct_31.j = head(Oct_31.j,1)
Nov_06.j = dplyr::filter(just_MoWa, Date == "2015-11-06")
Nov_06.j[is.na(Nov_06.j)] <- 0
Jmax_mean = mean(Nov_06.j$Jmax)
Jmax_se = sum(Nov_06.j$Jmax_SE)
Nov_06.j = as.data.frame(cbind(Nov_06.j,Jmax_mean, Jmax_se))
Nov_06.j = head(Nov_06.j,1)
Nov_13.j = dplyr::filter(just_MoWa, Date == "2015-11-13")
Nov_13.j[is.na(Nov_13.j)] <- 0
Jmax_mean = mean(Nov_13.j$Jmax)
Jmax_se = sum(Nov_13.j$Jmax_SE)
Nov_13.j = as.data.frame(cbind(Nov_13.j,Jmax_mean, Jmax_se))
Nov_13.j = head(Nov_13.j,1)

MoWa_Jmax = rbind(Sept_11.j, Sept_18.j, Sept_25.j, Oct_02.j, Oct_09.j, Oct_16.j, Oct_23.j, Oct_31.j, Nov_06.j)
MoWa_Jmax = MoWa_Jmax[c("Date", "Genotype", "Avg_Temp_degC", "Min_Temp_degC",
                          "Avg.Thickness.mm", "LMA", "LMV", "Fresh_Weight_mg", 
                          "Dry_Weight_mg", "Ratio", "Jmax_mean", "Jmax_se")]
MoWa_Jmax_plot = ggplot(MoWa_Jmax, aes(Date, Jmax_mean))
MoWa_Jmax_plot + geom_point(aes(colour = MoWa_Jmax$Avg_Temp_degC), size=5) +
  scale_colour_gradient(low="deepskyblue1", high="red") +
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Jmax time series") +
  geom_abline(aes(intercept=75.96318, slope=0)) +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  geom_errorbar(aes(ymax=Jmax_mean+Jmax_se, ymin=Jmax_mean-Jmax_se),width=3) +
  ylab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#EUROPEAN Jmax time series with error bars
Sept_11.2j = dplyr::filter(just_Euro, Date == "2015-09-11")
Jmax_mean2 = mean(Sept_11.2j$Jmax)
Jmax_se2 = sum(Sept_11.2j$Jmax_SE)
Sept_11.2j = as.data.frame(cbind(Sept_11.2j,Jmax_mean2, Jmax_se2))
Sept_11.2j = head(Sept_11.2j,1)
Sept_18.2j = dplyr::filter(just_Euro, Date == "2015-09-18")
Jmax_mean2 = mean(Sept_18.2j$Jmax)
Jmax_se2 = sum(Sept_18.2j$Jmax_SE)
Sept_18.2j = as.data.frame(cbind(Sept_18.2j,Jmax_mean2, Jmax_se2))
Sept_18.2j = head(Sept_18.2j,1)
Sept_25.2j = dplyr::filter(just_Euro, Date == "2015-09-25")
Jmax_mean2 = mean(Sept_25.2j$Jmax)
Jmax_se2 = sum(Sept_25.2j$Jmax_SE)
Sept_25.2j = as.data.frame(cbind(Sept_25.2j,Jmax_mean2, Jmax_se2))
Sept_25.2j = head(Sept_25.2j,1)
Oct_02.2j = dplyr::filter(just_Euro, Date == "2015-10-02")
Jmax_mean2 = mean(Oct_02.2j$Jmax)
Jmax_se2 = sum(Oct_02.2j$Jmax_SE)
Oct_02.2j = as.data.frame(cbind(Oct_02.2j,Jmax_mean2, Jmax_se2))
Oct_02.2j = head(Oct_02.2j,1)
Oct_09.2j = dplyr::filter(just_Euro, Date == "2015-10-09")
Jmax_mean2 = mean(Oct_09.2j$Jmax)
Jmax_se2 = sum(Oct_09.2j$Jmax_SE)
Oct_09.2j = as.data.frame(cbind(Oct_09.2j,Jmax_mean2, Jmax_se2))
Oct_09.2j = head(Oct_09.2j,1)
Oct_16.2j = dplyr::filter(just_Euro, Date == "2015-10-16")
Jmax_mean2 = mean(Oct_16.2j$Jmax)
Jmax_se2 = sum(Oct_16.2j$Jmax_SE)
Oct_16.2j = as.data.frame(cbind(Oct_16.2j,Jmax_mean2, Jmax_se2))
Oct_16.2j = head(Oct_16.2j,1)
Oct_23.2j = dplyr::filter(just_Euro, Date == "2015-10-23")
Jmax_mean2 = mean(Oct_23.2j$Jmax)
Jmax_se2 = sum(Oct_23.2j$Jmax_SE)
Oct_23.2j = as.data.frame(cbind(Oct_23.2j,Jmax_mean2, Jmax_se2))
Oct_23.2j = head(Oct_23.2j,1)
Oct_31.2j = dplyr::filter(just_Euro, Date == "2015-10-31")
Oct_31.2j[is.na(Oct_31.2j)] <- 0
Jmax_mean2 = mean(Oct_31.2j$Jmax)
Jmax_se2 = sum(Oct_31.2j$Jmax_SE)
Oct_31.2j = as.data.frame(cbind(Oct_31.2j,Jmax_mean2, Jmax_se2))
Oct_31.2j = head(Oct_31.2j,1)
Nov_06.2j = dplyr::filter(just_Euro, Date == "2015-11-06")
Nov_06.2j[is.na(Nov_06.2j)] <- 0
Jmax_mean2 = mean(Nov_06.2j$Jmax)
Jmax_se2 = sum(Nov_06.2j$Jmax_SE)
Nov_06.2j = as.data.frame(cbind(Nov_06.2j,Jmax_mean2, Jmax_se2))
Nov_06.2j = head(Nov_06.2j,1)
Nov_13.2j = dplyr::filter(just_Euro, Date == "2015-11-13")
Nov_13.2j[is.na(Nov_13.2j)] <- 0
Jmax_mean2 = mean(Nov_13.2j$Jmax)
Jmax_se2 = sum(Nov_13.2j$Jmax_SE)
Nov_13.2j = as.data.frame(cbind(Nov_13.2j,Jmax_mean2, Jmax_se2))
Nov_13.2j = head(Nov_13.2j,1)

Euro_Jmax = rbind(Sept_11.2j, Sept_18.2j, Sept_25.2j, Oct_02.2j, Oct_09.2j, Oct_16.2j, Oct_23.2j, Oct_31.2j, Nov_06.2j, Nov_13.2j)
Euro_Jmax = Euro_Jmax[c("Date", "Genotype", "Avg_Temp_degC", "Min_Temp_degC",
                        "Avg.Thickness.mm", "LMA", "LMV", "Fresh_Weight_mg", 
                        "Dry_Weight_mg", "Ratio", "Jmax_mean2", "Jmax_se2")]
Euro_Jmax_plot = ggplot(Euro_Jmax, aes(Date, Jmax_mean2))
Euro_Jmax_plot + geom_point(aes(colour = Avg_Temp_degC), size=5) +
  scale_colour_gradient(low="deepskyblue1", high="red") +
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("European genotype Jmax time series") +
  geom_abline(aes(intercept=97.99166, slope=0)) +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  geom_errorbar(aes(ymax=Jmax_mean2+Jmax_se2, ymin=Jmax_mean2-Jmax_se2),width=3) +
  ylab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT VCMAX VS. JMAX WITH DOUBLE ERROR BARS 
#Build a df with the means for Vcmax and Jmax instead of the raw data
MoWa_Jmax=MoWa_Jmax[c("Date", "Jmax_mean","Jmax_se")]
summary_stats = merge(MoWa_Vcmax, MoWa_Jmax, by = "Date")
Euro_Jmax=Euro_Jmax[c("Date", "Jmax_mean2","Jmax_se2")]
summary_stats2 = merge(Euro_Vcmax, Euro_Jmax, by = "Date")
names(summary_stats2)[names(summary_stats2)=="Vcmax_mean2"] <- "Vcmax_mean"
names(summary_stats2)[names(summary_stats2)=="Vcmax_se2"] <- "Vcmax_se"
names(summary_stats2)[names(summary_stats2)=="Jmax_mean2"] <- "Jmax_mean"
names(summary_stats2)[names(summary_stats2)=="Jmax_se2"] <- "Jmax_se"
summary_stats = rbind(summary_stats,summary_stats2)
save(summary_stats, file = "summary_stats.RDA")
#Plot mean Vcmax v. mean Jmax
#coef(lm(Vcmax_mean ~ Jmax_mean, data = summary_stats))
plot_Vcmax_Jmax = ggplot(summary_stats, aes(Jmax_mean, Vcmax_mean))
plot_Vcmax_Jmax + geom_point(aes(colour=Genotype), size = 5) + 
  geom_errorbar(aes(ymax=Vcmax_mean+Vcmax_se, ymin=Vcmax_mean-Vcmax_se)) +
  geom_errorbarh(aes(xmax=Jmax_mean+Jmax_se, xmin=Jmax_mean-Jmax_se)) +
  geom_abline(intercept=-9.4863, slope=0.5580) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  xlab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  ggtitle("CO2 Limitation vs. Light Limitation to Carbon Assimilation") +
    theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #LMA TIME SERIES
# #not too compelling
# plot_day_LMA = ggplot(main_df, aes(Date, LMA))
# plot_day_LMA + geom_point(aes(colour = Genotype), size=3)  +
#   ylab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#LMV TIME SERIES
#get the avg LMV's for each week from the genotype replicates
main_df[is.na(main_df)] <- 0

just_Euro = dplyr::filter(main_df, Genotype=="European")
just_MoWa = dplyr::filter(main_df, Genotype=="Missouri x Washington")
Sept_11 = dplyr::filter(just_MoWa, Date == "2015-09-11")
LMV_mean = mean(Sept_11$LMV)
Sept_11 = as.data.frame(cbind(Sept_11,LMV_mean))
Sept_11 = head(Sept_11,1)
Sept_18 = dplyr::filter(just_MoWa, Date == "2015-09-18")
LMV_mean = mean(Sept_18$LMV)
Sept_18 = as.data.frame(cbind(Sept_18,LMV_mean))
Sept_18 = head(Sept_18,1)
Sept_25 = dplyr::filter(just_MoWa, Date == "2015-09-25")
LMV_mean = mean(Sept_25$LMV)
Sept_25 = as.data.frame(cbind(Sept_25,LMV_mean))
Sept_25 = head(Sept_25,1)
Oct_02 = dplyr::filter(just_MoWa, Date == "2015-10-02")
LMV_mean = mean(Oct_02$LMV)
Oct_02 = as.data.frame(cbind(Oct_02,LMV_mean))
Oct_02 = head(Oct_02,1)
Oct_09 = dplyr::filter(just_MoWa, Date == "2015-10-09")
LMV_mean = mean(Oct_09$LMV)
Oct_09 = as.data.frame(cbind(Oct_09,LMV_mean))
Oct_09 = head(Oct_09,1)
Oct_16 = dplyr::filter(just_MoWa, Date == "2015-10-16")
LMV_mean = mean(Oct_16$LMV)
Oct_16 = as.data.frame(cbind(Oct_16,LMV_mean))
Oct_16 = head(Oct_16,1)
Oct_23 = dplyr::filter(just_MoWa, Date == "2015-10-23")
LMV_mean = mean(Oct_23$LMV)
Oct_23 = as.data.frame(cbind(Oct_23,LMV_mean))
Oct_23 = head(Oct_23,1)
Oct_31 = dplyr::filter(just_MoWa, Date == "2015-10-31")
LMV_mean = mean(Oct_31$LMV)
Oct_31 = as.data.frame(cbind(Oct_31,LMV_mean))
Oct_31 = head(Oct_31,1)
Nov_06 = dplyr::filter(just_MoWa, Date == "2015-11-06")
LMV_mean = mean(Nov_06$LMV)
Nov_06 = as.data.frame(cbind(Nov_06,LMV_mean))
Nov_06 = head(Nov_06,1)
Nov_13 = dplyr::filter(just_MoWa, Date == "2015-11-13")
LMV_mean = mean(Nov_13$LMV)
Nov_13 = as.data.frame(cbind(Nov_13,LMV_mean))
Nov_13 = head(Nov_13,1)
MoWa_LMV = rbind(Sept_11, Sept_18, Sept_25, Oct_02, Oct_09, Oct_16, Oct_23, Oct_31, Nov_06, Nov_13)
#Euro LMV's
Sept_11 = dplyr::filter(just_Euro, Date == "2015-09-11")
LMV_mean = mean(Sept_11$LMV)
Sept_11 = as.data.frame(cbind(Sept_11,LMV_mean))
Sept_11 = head(Sept_11,1)
Sept_18 = dplyr::filter(just_Euro, Date == "2015-09-18")
LMV_mean = mean(Sept_18$LMV)
Sept_18 = as.data.frame(cbind(Sept_18,LMV_mean))
Sept_18 = head(Sept_18,1)
Sept_25 = dplyr::filter(just_Euro, Date == "2015-09-25")
LMV_mean = mean(Sept_25$LMV)
Sept_25 = as.data.frame(cbind(Sept_25,LMV_mean))
Sept_25 = head(Sept_25,1)
Oct_02 = dplyr::filter(just_Euro, Date == "2015-10-02")
LMV_mean = mean(Oct_02$LMV)
Oct_02 = as.data.frame(cbind(Oct_02,LMV_mean))
Oct_02 = head(Oct_02,1)
Oct_09 = dplyr::filter(just_Euro, Date == "2015-10-09")
LMV_mean = mean(Oct_09$LMV)
Oct_09 = as.data.frame(cbind(Oct_09,LMV_mean))
Oct_09 = head(Oct_09,1)
Oct_16 = dplyr::filter(just_Euro, Date == "2015-10-16")
LMV_mean = mean(Oct_16$LMV)
Oct_16 = as.data.frame(cbind(Oct_16,LMV_mean))
Oct_16 = head(Oct_16,1)
Oct_23 = dplyr::filter(just_Euro, Date == "2015-10-23")
LMV_mean = mean(Oct_23$LMV)
Oct_23 = as.data.frame(cbind(Oct_23,LMV_mean))
Oct_23 = head(Oct_23,1)
Oct_31 = dplyr::filter(just_Euro, Date == "2015-10-31")
LMV_mean = mean(Oct_31$LMV)
Oct_31 = as.data.frame(cbind(Oct_31,LMV_mean))
Oct_31 = head(Oct_31,1)
Nov_06 = dplyr::filter(just_Euro, Date == "2015-11-06")
LMV_mean = mean(Nov_06$LMV)
Nov_06 = as.data.frame(cbind(Nov_06,LMV_mean))
Nov_06 = head(Nov_06,1)
Nov_13 = dplyr::filter(just_Euro, Date == "2015-11-13")
LMV_mean = mean(Nov_13$LMV)
Nov_13 = as.data.frame(cbind(Nov_13,LMV_mean))
Nov_13 = head(Nov_13,1)
Euro_LMV = rbind(Sept_11, Sept_18, Sept_25, Oct_02, Oct_09, Oct_16, Oct_23, Oct_31, Nov_06, Nov_13)
avg_LMV = rbind(MoWa_LMV, Euro_LMV)
#getting rid of unnecessary columns
avg_LMV = as.data.frame(c(avg_LMV[1], avg_LMV[3:8], avg_LMV[10:14], avg_LMV[16]))
save(avg_LMV, file = "avg_LMV.RDA")
#filter out the 0's as NA's because those are from before we started punching
avg_LMV = dplyr::filter(avg_LMV, LMV_mean > 0)
#There is reason to drop some of the low outliers - on 10/09/15, G11 
#was fresh-weighed with only 1 punch instead of standard 2
#same thing happened on 10/16/15 with A18
#on 10/23/15, E10's thickness was not measured
avg_LMV = dplyr::filter(avg_LMV, LMV_mean > 0.55)
plot_avg_LMV = ggplot(avg_LMV, aes(Date, LMV_mean))
plot_avg_LMV + geom_point(aes(colour = Genotype), size=5) +
  ylab((bquote('LMV' ~ mg ~ '/' ~ mm^{3}))) +
  ggtitle("Leaf mass per volume time series") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #LMA VS. VCMAX
# #not too compelling
# plot_LMA_Vcmax = ggplot(main_df, aes(LMA, Vcmax))
# plot_LMA_Vcmax + geom_point(aes(colour = Genotype), size=3)  +
#   xlab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
#   ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#LMV VS. VCMAX
load("~/B2-Honors-Proj/summary_stats.RDA")
load("~/B2-Honors-Proj/avg_LMV.RDA")
summary_stats = as.data.frame(c(summary_stats[1:4], summary_stats[11:14]))
avg_LMV = as.data.frame(c(avg_LMV[1:2], avg_LMV[13]))
summary_stats = merge(summary_stats,avg_LMV, by = c("Date", "Genotype"), all = TRUE)
summary_stats = dplyr::filter(summary_stats,LMV_mean > 0)
plot_LMV_Vcmax = ggplot(summary_stats, aes(LMV_mean, Vcmax_mean))
plot_LMV_Vcmax + geom_point(aes(colour = Genotype), size=5) +
  xlab((bquote('LMV' ~ mg ~ '/' ~ mm^{3}))) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  ggtitle("Leaf mass per volume vs. Vcmax") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT THICKNESS TIME SERIES
load("~/B2-Honors-Proj/data/Thick_and_Data.RDA")
Thick_and_Data = dplyr::filter(Thick_and_Data, Avg.Thickness.mm > 0)
Thick_and_Data[is.na(Thick_and_Data)] = 0
needfix = dplyr::filter(Thick_and_Data, Genotype ==0, Location == "B06")
needfix$Genotype <- replace(needfix$Genotype, needfix$Genotype=="0", "European")
needfix2 = dplyr::filter(Thick_and_Data, Genotype ==0, Location == "E10")
needfix2$Genotype <- replace(needfix2$Genotype, needfix2$Genotype=="0", "Missouri x Washington")
needfix3 = dplyr::filter(Thick_and_Data, Genotype ==0, Location == "F05")
needfix3$Genotype <- replace(needfix3$Genotype, needfix3$Genotype=="0", "Missouri x Washington")
needfix4 = dplyr::filter(Thick_and_Data, Genotype ==0, Location == "F08")
needfix4$Genotype <- replace(needfix4$Genotype, needfix4$Genotype=="0", "Missouri x Washington")
Thick_and_Data=rbind(Thick_and_Data,needfix, needfix4, needfix3, needfix2)
Thick_and_Data = dplyr::filter(Thick_and_Data, Genotype != 0)
group = dplyr::group_by(Thick_and_Data, Date, Genotype)
avg_thick = dplyr::summarise(group, mean(Avg.Thickness.mm))
names(avg_thick)[names(avg_thick)=="mean(Avg.Thickness.mm)"] <- "Avg.Thickness.mm"
plot_day_thick = ggplot(avg_thick, aes(Date, Avg.Thickness.mm))
plot_day_thick + geom_point(aes(colour = Genotype), size=5)  +
  ylab("Average Leaf Thickness(mm)") +
  ggtitle("Leaf Thickness time series") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT THICKNESS VS. VCMAX
thick_vcmax = merge(summary_stats, avg_thick, by = c("Date", "Genotype"))
plot_thick_vcmax = ggplot(thick_vcmax, aes(Avg.Thickness.mm, Vcmax_mean))
plot_thick_vcmax + geom_point(aes(colour = Genotype), size=5)  +
  xlab("Average Leaf Thickness(mm)") +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  ggtitle("Leaf Thickness vs. Vcmax") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #PLOT THICKNESS VS. LMA
# #not compelling
# plot_thick_LMA = ggplot(main_df, aes(Avg.Thickness.mm, LMA))
# plot_thick_LMA + geom_point(aes(colour = Genotype), size=3) + 
#   xlab("Average Leaf Thickness (mm)") +  
#   ylab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT AVG TEMP VS. TIME
plot_met_time = ggplot(main_df, aes(Date, Avg_Temp_degC))
plot_met_time + geom_point(size=5) + 
  ylab("Average Temperature (C)") +  
  geom_path() +
  ggtitle("Average Temperature over time") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT MIN TEMP VS. TIME
load("~/B2-Honors-Proj/main_df.RDA")
plot_mintemp_time = ggplot(main_df, aes(Date, Min_Temp_degC))
plot_mintemp_time + geom_point(size=5) + 
  ylab("Minimum Temperature (C)") +  
  geom_path() +
  ggtitle("Minimum Temperature over time") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #FRESH:DRY TIME SERIES
# #creating df with avg Fresh and Dry Weights
# #not too compelling
# avg_ratio = dplyr::filter(main_df, Ratio > 0, LMV > 0.55)
# avg_ratio = dplyr::group_by(avg_ratio, Date, Genotype)
# avg_ratio = dplyr::summarise(avg_ratio, mean(Ratio))
# names(avg_ratio)[names(avg_ratio)=="mean(Ratio)"] <- "Ratio"

# ratio_plot = ggplot(avg_ratio, aes(Date, Ratio))
# ratio_plot + geom_point(aes(colour = Genotype), size=3) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #FRESH WEIGHT TIME SERIES
# #not too compelling
# avg_fresh = dplyr::filter(main_df, Fresh_Weight_mg > 0)
# avg_fresh = dplyr::group_by(avg_fresh, Date, Genotype)
# avg_fresh = dplyr::summarise(avg_fresh, mean(Fresh_Weight_mg))
# names(avg_fresh)[names(avg_fresh)=="mean(Fresh_Weight_mg)"] <- "Fresh_Weight_mg"
# fresh_time = ggplot(avg_fresh, aes(Date, Fresh_Weight_mg))
# fresh_time + geom_point(aes(colour = Genotype), size=3) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #DRY WEIGHT TIME SERIES
# #not compelling
# avg_dry = dplyr::filter(main_df, Dry_Weight_mg > 0)
# avg_dry = dplyr::group_by(avg_dry, Date, Genotype)
# avg_dry = dplyr::summarise(avg_dry, mean(Dry_Weight_mg))
# names(avg_dry)[names(avg_dry)=="mean(Dry_Weight_mg)"] <- "Dry_Weight_mg"
# dry_time = ggplot(avg_dry, aes(Date, Dry_Weight_mg))
# dry_time + geom_point(aes(colour = Genotype), size=3) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#ASAT TIME SERIES
#SAME STORY AS THE VCMAX TIME SERIES
group_Asat = dplyr::group_by(Asat_data, Genotype, Date)
Both_Asat = summarise(group_Asat, mean(Asat))
MoWa_Asat = dplyr::filter(Both_Asat, Genotype == "Missouri x Washington")
names(MoWa_Asat)[names(MoWa_Asat)=="mean(Asat)"] <- "Asat_mean"

Euro_Asat = dplyr::filter(Both_Asat, Genotype == "European")
names(Euro_Asat)[names(Euro_Asat)=="mean(Asat)"] <- "Asat_mean"

MoWa_Asat_time = ggplot(MoWa_Asat, aes(Date, Asat_mean))
MoWa_Asat_time + geom_point(size=3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Euro_Asat_time = ggplot(Euro_Asat, aes(Date, Asat_mean))
Euro_Asat_time + geom_point(size=3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Quick look at conductance:
load("~/B2-Honors-Proj/Cond.RDA")
names(Cond)[names(Cond)=="mean(Cond)"] <- "Avg_Conductance"
Cond$Date = strptime(Cond$Date, "%m-%d-%y")
Cond$Date = as.Date(Cond$Date)

MoWa_Cond = dplyr::filter(Cond, Genotype == "Missouri x Washington")
by_MowaDate = dplyr::group_by(MoWa_Cond, Date)
mean_MoWaCond = dplyr::summarise(by_MowaDate, mean(Avg_Conductance))
names(mean_MoWaCond)[names(mean_MoWaCond)=="mean(Avg_Conductance)"] <- "Avg_Conductance"

MoWa_Cond_plot = ggplot(mean_MoWaCond, aes(Date, Avg_Conductance))
MoWa_Cond_plot + geom_point(size=3) + 
  ggtitle("MO/WA genotype Conductance time series") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Euro_Cond = dplyr::filter(Cond, Genotype == "European")
by_EuroDate = dplyr::group_by(Euro_Cond, Date)
mean_EuroCond = dplyr::summarise(by_EuroDate, mean(Avg_Conductance))
names(mean_EuroCond)[names(mean_EuroCond)=="mean(Avg_Conductance)"] <- "Avg_Conductance"

Euro_Cond_plot = ggplot(mean_EuroCond, aes(Date, Avg_Conductance))
Euro_Cond_plot + geom_point(size=3) + 
  ggtitle("European genotype Conductance time series") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Curious about location:
main_df[is.na(main_df)] <- 0
group_by_loc = dplyr::filter(main_df, Vcmax !=0)
group_by_loc = dplyr::group_by(main_df, Location)
group_by_loc = dplyr::summarise(group_by_loc, mean(Vcmax))
names(group_by_loc)[names(group_by_loc)=="mean(Vcmax)"] <- "Vcmax_mean"

loc_plot = ggplot(data=group_by_loc, aes(x=Location, y=Vcmax_mean)) 
loc_plot + geom_bar(stat="identity") +
  ggtitle("Mean Vcmax by Location") +
  theme(plot.title=element_text(family="Times", face="bold", size=30)) +
  theme(axis.title = element_text(size= rel(2.0)))+
  theme(legend.key.size = unit(2.5, "cm")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




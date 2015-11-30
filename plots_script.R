#Author: Karen Wang
#Date: 11/22/2015
#Purpose: Plots using data from the LMA_thick_data.RDA

library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

load("~/B2-Honors-Proj/main_df.RDA")
load("~/B2-Honors-Proj/Final_Acis.RDA")
load("~/B2-Honors-Proj/Asat_data.RDA")

#PLOT VCMAX VS. TIME 
#Separating the genotypes
just_Euro = dplyr::filter(main_with_met, Genotype=="European")
just_MoWa = dplyr::filter(main_with_met, Genotype=="Missouri x Washington")

#Basic stats for MoWa geno
MoWa_stats = ddply(just_MoWa, c("Date"),summarise,
                   N = length(Vcmax),
                   mean = mean(Vcmax),
                   sd = sd(Vcmax),
                   se = sd/sqrt(N)
)
#Renaming the stats columns to specify they're for Vcmax
names(MoWa_stats)[names(MoWa_stats)=="N"] <- "Vcmax_N"
names(MoWa_stats)[names(MoWa_stats)=="mean"] <- "Vcmax_mean"
names(MoWa_stats)[names(MoWa_stats)=="sd"] <- "Vcmax_sd"
names(MoWa_stats)[names(MoWa_stats)=="se"] <- "Vcmax_se"

MoWa_stats=merge(just_MoWa, MoWa_stats, by = "Date")

#Same for Euro geno
Euro_stats = ddply(just_Euro, c("Date"),summarise,
                   N = length(Vcmax),
                   mean = mean(Vcmax),
                   sd = sd(Vcmax),
                   se = sd/sqrt(N)
)
names(Euro_stats)[names(Euro_stats)=="N"] <- "Vcmax_N"
names(Euro_stats)[names(Euro_stats)=="mean"] <- "Vcmax_mean"
names(Euro_stats)[names(Euro_stats)=="sd"] <- "Vcmax_sd"
names(Euro_stats)[names(Euro_stats)=="se"] <- "Vcmax_se"
Euro_stats=merge(just_Euro, Euro_stats, by = "Date")

#Vcmax time series with error bars
plot_MoWa = ggplot(MoWa_stats, aes(Date, Vcmax))
plot_MoWa + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Vcmax time series") +
  geom_errorbar(aes(ymax=Vcmax+Vcmax_se, ymin=Vcmax-Vcmax_se),width=1) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_Euro = ggplot(Euro_stats, aes(Date, Vcmax))
plot_Euro + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"),labels = date_format("%m/%d"))+
  ggtitle("European genotype Vcmax time series")+
  geom_errorbar(aes(ymax=Vcmax+Vcmax_se, ymin=Vcmax-Vcmax_se),width=1) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT JMAX VS. TIME 
#Jmax stats 
MoWa_Jmax = ddply(MoWa_stats, c("Date"),summarise,
                   N = length(Jmax),
                   mean = mean(Jmax),
                   sd = sd(Jmax),
                   se = sd/sqrt(N)
)
#Renaming the stats columns to specify they're for Jmax
names(MoWa_Jmax)[names(MoWa_Jmax)=="N"] <- "Jmax_N"
names(MoWa_Jmax)[names(MoWa_Jmax)=="mean"] <- "Jmax_mean"
names(MoWa_Jmax)[names(MoWa_Jmax)=="sd"] <- "Jmax_sd"
names(MoWa_Jmax)[names(MoWa_Jmax)=="se"] <- "Jmax_se"
MoWa_stats=merge(MoWa_stats, MoWa_Jmax, by = "Date")

plot_MoWa_Jmax = ggplot(MoWa_stats, aes(Date, Jmax))
plot_MoWa_Jmax + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Jmax time series") +
  geom_errorbar(aes(ymax=Jmax+Jmax_se, ymin=Jmax-Jmax_se),width=1) +
  ylab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#For Euro geno
Euro_Jmax = ddply(Euro_stats, c("Date"),summarise,
                   N = length(Jmax),
                   mean = mean(Jmax),
                   sd = sd(Jmax),
                   se = sd/sqrt(N)
)
names(Euro_Jmax)[names(Euro_Jmax)=="N"] <- "Jmax_N"
names(Euro_Jmax)[names(Euro_Jmax)=="mean"] <- "Jmax_mean"
names(Euro_Jmax)[names(Euro_Jmax)=="sd"] <- "Jmax_sd"
names(Euro_Jmax)[names(Euro_Jmax)=="se"] <- "Jmax_se"
Euro_stats=merge(Euro_stats, Euro_Jmax, by = "Date")

plot_Euro_Jmax = ggplot(Euro_stats, aes(Date, Jmax))
plot_Euro_Jmax + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("European genotype Jmax time series") +
  geom_errorbar(aes(ymax=Jmax+Jmax_se, ymin=Jmax-Jmax_se),width=1) +
  ylab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT VCMAX VS. JMAX
coef(lm(Vcmax ~ Jmax, data = LMA_thick_data))
plot_Vcmax_Jmax = ggplot(LMA_thick_data, aes(Jmax, Vcmax))
plot_Vcmax_Jmax + geom_point(aes(colour=Genotype, size = 5)) + 
  geom_abline(intercept=-9.49, slope=0.5538) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  xlab(bquote('Jmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT LMA TIME SERIES
plot_day_LMA = ggplot(LMA_thick_data, aes(Date, LMA))
plot_day_LMA + geom_point(aes(colour=Genotype, size = 5)) +
  ylab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#PLOT LMA VS. VCMAX
plot_LMA_Vcmax = ggplot(LMA_thick_data, aes(LMA, Vcmax))
plot_LMA_Vcmax + geom_point(aes(colour=Genotype, size = 5)) +
  xlab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT THICKNESS TIME SERIES
plot_day_thick = ggplot(LMA_thick_data, aes(Date, Avg.Thickness.mm))
plot_day_thick + geom_point(aes(colour=Genotype, size = 5)) +
  ylab("Average Leaf Thickness(mm)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT THICKNESS VS. VCMAX
plot_thick_Vcmax = ggplot(LMA_thick_data, aes(Avg.Thickness.mm, Vcmax))
plot_thick_Vcmax + geom_point(aes(colour=Genotype, size = 5)) +
  xlab("Average Leaf Thickness(mm)") +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT THICKNESS VS. LMA
plot_thick_LMA = ggplot(LMA_thick_data, aes(Avg.Thickness.mm, LMA))
plot_thick_LMA + geom_point(aes(colour=Genotype, size = 5)) + 
  xlab("Average Leaf Thickness (mm)") +  
  ylab((bquote('LMA' ~ mg ~ '/' ~ mm^{2}))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT AVG TEMP VS. TIME
load("~/B2-Honors-Proj/main_df.RDA")
plot_met_time = ggplot(main_with_met, aes(Date, Avg_Temp))
plot_met_time + geom_point(aes(size = 5)) + 
  ylab("Average Temperature (C)") +  
  geom_path() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT TIME SERIES OF VCMAX WITH COLOR AS INDEX OF AVG TEMP
plot_met_Vcmax = ggplot(main_with_met, aes(Date, Vcmax))
plot_met_Vcmax + geom_point(aes(colour = Avg_Temp)) +
  scale_colour_gradient(low="deepskyblue1", high="red") +
  ylab(bquote('Vcmax ('*mu~ 'mol' ~ m^-2~s^-1*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT FRESH:DRY TIME SERIES
curious = ggplot(main_df, aes(Date, Ratio))
curious + geom_point(aes(colour=Genotype, size = 5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PLOT FRESH TIME SERIES
HMM = ggplot(main_df, aes(Date, Fresh_Weight_mg))
HMM + geom_point(aes(colour=Genotype, size = 5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


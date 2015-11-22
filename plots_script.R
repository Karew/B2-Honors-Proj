#Author: Karen Wang
#Date: 11/21/2015
#Purpose: Plots using data from the LMA_thick_data.RDA

library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

load("~/B2-Honors-Proj/LMA_thick_data.RDA")

#PLOT VCMAX VS. TIME 
#Separating the genotypes
just_Euro = dplyr::filter(LMA_thick_data, Genotype=="European")
just_MoWa = dplyr::filter(LMA_thick_data, Genotype=="Missouri x Washington")

#Basic stats for MoWa geno
MoWa_stats = ddply(just_MoWa, c("Date"),summarise,
                   N = length(Vcmax),
                   mean = mean(Vcmax),
                   sd = sd(Vcmax),
                   se = sd/sqrt(N)
)
MoWa_stats=merge(just_MoWa, MoWa_stats, by = "Date")

#For Euro geno
Euro_stats = ddply(just_Euro, c("Date"),summarise,
                   N = length(Vcmax),
                   mean = mean(Vcmax),
                   sd = sd(Vcmax),
                   se = sd/sqrt(N)
)
Euro_stats=merge(just_Euro, Euro_stats, by = "Date")

#Time series with error bars
plot_MoWa = ggplot(MoWa_stats, aes(Date, Vcmax))
plot_MoWa + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Vcmax time series") +
  geom_errorbar(aes(ymax=Vcmax+se, ymin=Vcmax-se),width=0.2)

plot_Euro = ggplot(Euro_stats, aes(Date, Vcmax))
plot_Euro + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"),labels = date_format("%m/%d"))+
  ggtitle("European genotype Vcmax time series")+
  geom_errorbar(aes(ymax=Vcmax+se, ymin=Vcmax-se),width=0.2)

#PLOT JMAX VS. TIME WITH ERROR BARS
#Jmax stats 
MoWa_Jmax = ddply(MoWa_stats, c("Date"),summarise,
                   N = length(Jmax),
                   mean = mean(Jmax),
                   sd = sd(Jmax),
                   se = sd/sqrt(N)
)
MoWa_stats=merge(MoWa_stats, MoWa_Jmax, by = "Date")

plot_MoWa_Jmax = ggplot(MoWa_stats, aes(Date, Jmax))
plot_MoWa_Jmax + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("MO/WA genotype Jmax time series") +
  geom_errorbar(aes(ymax=Jmax+se.y, ymin=Jmax-se.y),width=0.2)

#For Euro geno
Euro_Jmax = ddply(Euro_stats, c("Date"),summarise,
                   N = length(Jmax),
                   mean = mean(Jmax),
                   sd = sd(Jmax),
                   se = sd/sqrt(N)
)
Euro_stats=merge(Euro_Jmax, Euro_stats, by = "Date", "")
plot_Euro_Jmax = ggplot(Euro_stats, aes(Date, Jmax))
plot_Euro_Jmax + geom_point(aes(colour=Location, size=5)) + 
  scale_x_date(breaks = date_breaks(width = "1 week"), labels = date_format("%m/%d")) + 
  ggtitle("European genotype Jmax time series") +
  geom_errorbar(aes(ymax=Jmax+se.y, ymin=Jmax-se.y),width=0.2)

#PLOT VCMAX VS. JMAX
coef(lm(Vcmax ~ Jmax, data = LMA_thick_data))
plot_Vcmax_Jmax = ggplot(LMA_thick_data, aes(Jmax, Vcmax))
plot_Vcmax_Jmax + geom_point(aes(colour=Genotype, size = 5)) + 
  geom_abline(intercept=-9.49, slope=0.5538)

#PLOT LMA TIME SERIES
plot_day_LMA = ggplot(LMA_thick_data, aes(Date, LMA))
plot_day_LMA + geom_point(aes(colour=Genotype, size = 5))

#PLOT LMA VS. VCMAX
plot_LMA_Vcmax = ggplot(LMA_thick_data, aes(LMA, Vcmax))
plot_LMA_Vcmax + geom_point(aes(colour=Genotype, size = 5))

#PLOT THICKNESS TIME SERIES
plot_day_thick = ggplot(LMA_thick_data, aes(Date, Avg.Thickness.mm))
plot_day_thick + geom_point(aes(colour=Genotype, size = 5))

#PLOT THICKNESS VS. VCMAX
plot_thick_Vcmax = ggplot(LMA_thick_data, aes(Avg.Thickness.mm, Vcmax))
plot_thick_Vcmax + geom_point(aes(colour=Genotype, size = 5))



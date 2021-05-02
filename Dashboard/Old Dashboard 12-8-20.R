#this is an old version of the dashboard, before we started knitting to html

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)


data = read_excel("full7100withduration.xlsx")
nonaccptdata = data[data$Accepted == 'No',]
accptdata = data[data$Accepted == 'Yes',]

data$Duration.group <- rep(NA, nrow(data))

data <-mutate(data, Duration.group = case_when(
  data$Duration < 6 & data$Duration >= 0 ~ "0-6",
  data$Duration < 12 & data$Duration >= 6 ~ "06-12",
  data$Duration < 18 & data$Duration >= 12 ~ "12-18",
  data$Duration < 24 & data$Duration >= 18 ~ "18-24",
  data$Duration < 36 & data$Duration >= 24 ~ "24-36",
  data$Duration < 48 & data$Duration >= 36 ~ "36-48",
  data$Duration >= 48 ~ "48+"
))

data$Duration.group <- as.factor(data$Duration.group)
data$DONOR_ID <- as.factor(data$DONOR_ID)

#PH
ggplot(accptdata) + aes(x = Duration.group, y = ABG_PH) + 
  geom_count(aes(size = after_stat(prop), group = 1)) + 
  scale_size_area(max_size = 10) +
  geom_hline(yintercept=7.35, color='red') + 
  geom_hline(yintercept=7.45, color='red') + 
  xlab('Time since Brain Death (hours)') + ylab('pH')
ggplot(nonaccptdata) + aes(x = Duration.group, y = ABG_PH) + 
  geom_count(aes(size = after_stat(prop), group = 1)) + 
  scale_size_area(max_size = 10) +
  geom_hline(yintercept=7.35, color='red') + 
  geom_hline(yintercept=7.45, color='red') + 
  xlab('Time since Brain Death (hours)') + ylab('pH')

#PAO2
ggplot(data) + aes(x = Duration.group, y = PAO2) + 
  geom_count(aes(size = after_stat(prop), group = 1), na.rm = TRUE) + 
  scale_size_area(max_size = 10) +
  coord_cartesian(ylim = c(0,600)) +
  geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=120, color='red') + 
  xlab('Time since Brain Death (hours)') + ylab('PAO2')

#PEEP
ggplot(data, na.rm = TRUE) + aes(x = Duration.group, y = PEEP) + 
  geom_count(aes(size = after_stat(prop), group = 1)) + 
  scale_size_area(max_size = 10) +
  geom_hline(yintercept=5, color='red') + 
  geom_hline(yintercept=8, color='red') + 
  xlab('Time since Brain Death (hours)') + ylab('PEEP')

#HGB
ggplot(data) + aes(x = Duration.group, y = HGB) + 
  geom_count(aes(size = after_stat(prop), group = 1), na.rm = TRUE) + 
  scale_size_area(max_size = 5) +
  geom_hline(yintercept=7, color='red') + 
  geom_hline(yintercept=15, color='red') + 
  xlab('Time since Brain Death (hours)') + ylab('HGB')



#hex plot of PH
ggplot(accptdata, aes(Duration, ABG_PH)) + geom_hex(bins = 500, binwidth = c(75,.1)) +
  xlab('Time since Brain Death (hours)') + ylab('pH') +
  coord_cartesian(xlim = c(-2000,200)) + geom_hline(yintercept=7.35, color='red') + 
  geom_hline(yintercept=7.45, color='red')
#hex plot of PH
ggplot(nonaccptdata, aes(Duration, ABG_PH)) + geom_hex(bins = 500, binwidth = c(75,.1)) +
  xlab('Time since Brain Death (hours)') + ylab('pH') +
  coord_cartesian(xlim = c(-2000,200)) + geom_hline(yintercept=7.35, color='red') + 
  geom_hline(yintercept=7.45, color='red')




################################################################################


phmax = data[data$ABG_PH == max(data$ABG_PH, na.rm=T),]
phmax
pao2max = data[data$PAO2 == max(data$PAO2, na.rm = T),]
pao2max

max(data$ABG_PH, na.rm=T)


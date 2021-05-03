#making plots for different variables and age ranges for non-accepted hearts

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)


data = read_excel("full7100withduration.xlsx")

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

# Select donor data for hearts not accepted 
#nonaccptdata = data[data$Accepted == 'No',]

#There are 2981 unique donors with hearts that were not accepted

#pH
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$ABG_PH) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=7.35, color='red') + 
  geom_hline(yintercept=7.45, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('pH')

      #demonstration of geom_smooth
      donorA = nonaccptdata[nonaccptdata$DONOR_ID == '525349',]
      ggplot() +
        aes(x = donorA$Duration, y = donorA$AVG_BP_SYST) + 
        geom_hline(yintercept=100, color='red') + 
        geom_hline(yintercept=135, color='red') + geom_smooth()

#PAO2
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$PAO2) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=120, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('PAO2')

#PEEP
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$PEEP) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=5, color='red') + 
  geom_hline(yintercept=8, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('PEEP')

#HGB
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$HGB) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=7, color='red') + 
  geom_hline(yintercept=15, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)')

#HCT
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$HCT) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=32, color='red') + 
  geom_hline(yintercept=45, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)')

#Blood pressure and Heartbeat ranges
  #Add age column and calculate for each unique donor
  data$Age = rep(NA, nrow(data))
  #format BrainDeath and DOB_DON as a date
  data$BrainDeath = as.POSIXct(data$BrainDeath,format="%Y-%m-%d")
  data$DOB_DON = as.POSIXct(data$DOB_DON,format="%Y-%m-%d")
  data = data %>% group_by(DONOR_ID) %>% mutate(Age = BrainDeath - DOB_DON)
  #convert age from seconds to years
  data$Age = data$Age / (3600 * 24 * 365)
  data$Age = as.numeric(data$Age)
  
  #0-6 months category
  zerosixmonths = data[(data$Age <= 0.5),]
  sixtwelvemonths = data[(data$Age > 0.5 & data$Age <= 1),]
  onethreeyears = data[(data$Age > 1 & data$Age <= 3),]
  threesixyears = data[(data$Age > 3 & data$Age <= 6),]
  sixtwelveyears = data[(data$Age > 6 & data$Age <= 12),]
  twleveplusyears = data[(data$Age > 12),]

#AVG_BP_SYST for 0-6 months age
ggplot() +
  aes(x = zerosixmonths$Duration, y = zerosixmonths$AVG_BP_SYST) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_hline(yintercept=70, color='red') + 
  geom_hline(yintercept=90, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 0-6 Months')
#AVG_BP_SYST for 6-12 months age
ggplot() +
  aes(x = sixtwelvemonths$Duration, y = sixtwelvemonths$AVG_BP_SYST, 
      group = sixtwelvemonths$DONOR_ID, color = sixtwelvemonths$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=80, color='red') + 
  geom_hline(yintercept=100, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 6-12 Months')
#AVG_BP_SYST for 1-3 years age
ggplot() +
  aes(x = onethreeyears$Duration, y = onethreeyears$AVG_BP_SYST, 
      group = onethreeyears$DONOR_ID, color = onethreeyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=90, color='red') + 
  geom_hline(yintercept=105, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 1-3 Years')
#AVG_BP_SYST for 3-6 years age
ggplot() +
  aes(x = threesixyears$Duration, y = threesixyears$AVG_BP_SYST, 
      group = threesixyears$DONOR_ID, color = threesixyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=95, color='red') + 
  geom_hline(yintercept=110, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 3-6 Years')
#AVG_BP_SYST for 6-12 years age
ggplot() +
  aes(x = sixtwelveyears$Duration, y = sixtwelveyears$AVG_BP_SYST, 
      group = sixtwelveyears$DONOR_ID, color = sixtwelveyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=100, color='red') + 
  geom_hline(yintercept=120, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 6-12 Years')
#AVG_BP_SYST for 12+ years age
ggplot() +
  aes(x = twleveplusyears$Duration, y = twleveplusyears$AVG_BP_SYST, 
      group = twleveplusyears$DONOR_ID, color = twleveplusyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=100, color='red') + 
  geom_hline(yintercept=135, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Systolic BP for Age 12+ Years')

#AVG_BP_DIAYST for 0-6 months age
ggplot() +
  aes(x = zerosixmonths$Duration, y = zerosixmonths$AVG_BP_DIAST, 
      group = zerosixmonths$DONOR_ID, color = zerosixmonths$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=45, color='red') + 
  geom_hline(yintercept=65, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 0-6 Months')
#AVG_BP_DIAYST for 6-12 months age
ggplot() +
  aes(x = sixtwelvemonths$Duration, y = sixtwelvemonths$AVG_BP_DIAST, 
      group = sixtwelvemonths$DONOR_ID, color = sixtwelvemonths$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=55, color='red') + 
  geom_hline(yintercept=65, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 6-12 Months')
#AVG_BP_DIAYST for 1-3 years age
ggplot() +
  aes(x = onethreeyears$Duration, y = onethreeyears$AVG_BP_DIAST, 
      group = onethreeyears$DONOR_ID, color = onethreeyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=55, color='red') + 
  geom_hline(yintercept=70, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 1-3 Years')
#AVG_BP_DIAYST for 3-6 years age
ggplot() +
  aes(x = threesixyears$Duration, y = threesixyears$AVG_BP_DIAST, 
      group = threesixyears$DONOR_ID, color = threesixyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=75, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 3-6 Years')
#AVG_BP_DIAYST for 6-12 years age
ggplot() +
  aes(x = sixtwelveyears$Duration, y = sixtwelveyears$AVG_BP_DIAST, 
      group = sixtwelveyears$DONOR_ID, color = sixtwelveyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=75, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 6-12 Years')
#AVG_BP_DIAYST for 12+ years age
ggplot() +
  aes(x = twleveplusyears$Duration, y = twleveplusyears$AVG_BP_DIAST, 
      group = twleveplusyears$DONOR_ID, color = twleveplusyears$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=65, color='red') + 
  geom_hline(yintercept=85, color='red') + coord_cartesian(xlim = c(0,175)) + 
  xlab('Time since Brain Death (hours)') + ylab('Avg Diastolic BP for Age 12+ Years')

#AVG_PULSE for 0-6 months age
ggplot() +
  aes(x = zerosixmonths$Duration, y = zerosixmonths$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=110, color='red') + 
  geom_hline(yintercept=160, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 0-6 Months')
#AVG_PULSE for 6-12 months age
ggplot() +
  aes(x = sixtwelvemonths$Duration, y = sixtwelvemonths$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=90, color='red') + 
  geom_hline(yintercept=160, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 6-12 Months')
#AVG_PULSE for 1-3 years age
ggplot() +
  aes(x = onethreeyears$Duration, y = onethreeyears$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=80, color='red') + 
  geom_hline(yintercept=150, color='red') + coord_cartesian(xlim = c(0,100))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 1-3 Years')
#AVG_PULSE for 3-6 years age
ggplot() +
  aes(x = threesixyears$Duration, y = threesixyears$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=70, color='red') + 
  geom_hline(yintercept=120, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 3-6 Years')
#AVG_PULSE for 6-12 years age
ggplot() +
  aes(x = sixtwelveyears$Duration, y = sixtwelveyears$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=110, color='red') + coord_cartesian(xlim = c(0,125))+ 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 6-12 Years')
#AVG_PULSE for 12+ years age
ggplot() +
  aes(x = twleveplusyears$Duration, y = twleveplusyears$AVG_PULSE_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=60, color='red') + 
  geom_hline(yintercept=100, color='red') + coord_cartesian(xlim = c(0,175)) + 
  xlab('Time since Brain Death (hours)') + ylab('Avg Heart Beats per Min for Age 12+ Years')

#CVP
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$CVP_INT_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=5, color='red') + 
  geom_hline(yintercept=12, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('CVP (cmH2O)')

#BODYTEMP
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$BODYTEMP_RANGE_START) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=36, color='red') + 
  geom_hline(yintercept=38, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('Temperature (Celsius)')

#Inotropic Score for 0-6 months age
ggplot() +
  aes(x = zerosixmonths$Duration, y = zerosixmonths$Score) +
  coord_cartesian(xlim = c(0,100), ylim = c(-3000,5000)) + 
  xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 0-6 Months')
#Inotropic Score for 6-12 months age
ggplot() +
  aes(x = sixtwelvemonths$Duration, y = sixtwelvemonths$Score) + 
      coord_cartesian(xlim = c(0,100), ylim = c(0,5000)) + 
        xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 6-12 Months')
#Inotropic Score for 1-3 years age
ggplot() +
  aes(x = onethreeyears$Duration, y = onethreeyears$Score) + 
      coord_cartesian(xlim = c(0,100), ylim = c(2000,7000)) + 
        xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 1-3 Years')
#Inotropic Score for 3-6 years age
ggplot() +
  aes(x = threesixyears$Duration, y = threesixyears$Score) + 
      coord_cartesian(xlim = c(0,100), ylim = c(-3000,250000)) + 
        xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 3-6 Years')
#Inotropic Score for 6-12 years age
ggplot() +
  aes(x = sixtwelveyears$Duration, y = sixtwelveyears$Score) + 
      coord_cartesian(xlim = c(0,100), ylim = c(0,30000)) + 
        xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 6-12 Years')
#Inotropic Score for 12+ years age
ggplot() +
  aes(x = twleveplusyears$Duration, y = twleveplusyears$Score) + 
      coord_cartesian(xlim = c(0,100), ylim = c(0,200000)) + 
        xlab('Time since Brain Death (hours)') + geom_smooth() + ylab('VIS for Age 12+ Years')

#SGOT
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$SGOT) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=100, color='red') + 
  coord_cartesian(xlim = c(0,200), ylim = c(0,7500)) + xlab('Time since Brain Death (hours)') +
  ylab('SGOT (AST) (u/L)')

#SGPT
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$SGPT, 
      group = nonaccptdata$DONOR_ID, color = nonaccptdata$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + geom_hline(yintercept=100, color='red') + 
  coord_cartesian(xlim = c(0,200)) + xlab('Time since Brain Death (hours)')
  + ylab('SGPT (AST) (u/L)')

#SODIUM170
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$SODIUM170) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=130, color='red') + 
  geom_hline(yintercept=150, color='red') + coord_cartesian(xlim = c(0,200), ylim = c(100,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('Sodium 170 (mmEq/L)')

#CREATININE
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$CREATININE) + 
  stat_summary(fun.y = mean, geom = "point") + 
  geom_hline(yintercept=0.5, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('Creatinine (mg/dL)')

#POTASSIUM
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$POTASSIUM) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=3, color='red') + 
  geom_hline(yintercept=5.5, color='red') + coord_cartesian(xlim = c(0,200))+ 
  xlab('Time since Brain Death (hours)') + ylab('Potassium (mmol/L)')

#BILIRUBIN
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$BILIRUBIN) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=1, color='red') + 
  coord_cartesian(xlim = c(0,200)) + xlab('Time since Brain Death (hours)') +
  ylab('Bilirubin (mg/dL)')

#BILIRUBIN INDIRECT
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$BILIRUBIN_INDIRECT) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=1, color='red') + 
  coord_cartesian(xlim = c(0,200), ylim = c(0,100)) + xlab('Time since Brain Death (hours)')
  + ylab('Bilirubin Indirect (mg/dL)')

#PROTHROMBIN
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$PROTHROMBIN) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=45, color='red') + 
  coord_cartesian(xlim = c(0,200)) + xlab('Time since Brain Death (hours)') +
  ylab('Prothrombin (seconds)')

#INR
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$INR) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=1, color='red') + 
  coord_cartesian(xlim = c(0,200), ylim = c(0,50)) + xlab('Time since Brain Death (hours)') +
  ylab('INR')

#CKMB
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$CKMB) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=25, color='red') + 
  coord_cartesian(xlim = c(0,200)) + xlab('Time since Brain Death (hours)') + 
  ylab('CKMB (ng/mL)')

#TROPONIN I
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$TROPONINI) + 
  stat_summary(fun.y = mean, geom = "point") + geom_hline(yintercept=0.08, color='red') + 
  coord_cartesian(xlim = c(0,200)) + xlab('Time since Brain Death (hours)') + 
  ylab('Troponin (ng/mL)')

#TROPONIN T
ggplot() +
  aes(x = nonaccptdata$Duration, y = nonaccptdata$TROPONINT, 
      group = nonaccptdata$DONOR_ID, color = nonaccptdata$DONOR_ID) + 
  stat_summary(fun.y = mean, geom = "point") +
  coord_cartesian(xlim = c(0,125)) + xlab('Time since Brain Death (hours)')

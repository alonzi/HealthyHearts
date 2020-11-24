data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)

panels = read_excel("DonorNet LabPanels.xlsx")
values = read_excel("DonorNet LabValues.xlsx")
crosswalk = read.csv(file = 'STAR-shared/STAR-shared/cross-walk.csv')


#---------------------------------------------------------------------------#
# Add in Brain Death/Time and calculate duration
#---------------------------------------------------------------------------#
#Deceased Donor data
data = read.csv(file = 'deceased_donor_cleaned.csv')
#data = load_data("deceased_donor_data")
donor.info = data[,c("DONOR_ID","BRAIN_DEATH_DATE","BRAIN_DEATH_TIME")]

#combine datasets using DONOR_ID
panels = merge(panels, donor.info, by="DONOR_ID")
values = merge(values, donor.info, by="DONOR_ID")

#format date and time column
panels$DT = as.POSIXct(panels$DT,format="%Y-%m-%dT%H:%M")
values$DT = as.POSIXct(values$DT,format="%Y-%m-%dT%H:%M")

#combine brain death and brain time columns
panels$BrainDeath = as.POSIXct(paste(panels$BRAIN_DEATH_DATE, panels$BRAIN_DEATH_TIME), format="%Y-%m-%d %H:%M:%S")
values$BrainDeath = as.POSIXct(paste(values$BRAIN_DEATH_DATE, values$BRAIN_DEATH_TIME), format="%Y-%m-%d %H:%M:%S")

#Add duration column and calculate for each unique donor
panels$Duration = rep(NA, nrow(panels))
values$Duration = rep(NA, nrow(values))

panels = panels %>% group_by(DONOR_ID) %>% mutate(Duration = DT - BrainDeath)
values = values %>% group_by(DONOR_ID) %>% mutate(Duration = DT - BrainDeath)

#convert duration from seconds to hours
panels$Duration = panels$Duration / 3600
panels$Duration = as.numeric(panels$Duration)
values$Duration = values$Duration / 3600
values$Duration = as.numeric(values$Duration)


#---------------------------------------------------------------------------#
# Panels data
#---------------------------------------------------------------------------#
SGOT = panels$SGOT
SGPT = panels$SGPT
SODIUM170 = panels$SODIUM170
CREATININE = panels$CREATININE
POTASSIUM = panels$POTASSIUM
BILIRUBIN = panels$BILIRUBIN
BILIRUBIN_INDIRECT = panels$BILIRUBIN_INDIRECT
PROTHROMBIN = panels$PROTHROMBIN
INR = panels$INR
#repeated process: get rid of the NAs, change strings to decimals, remove the heading
SGOT = na.omit(SGOT)
SGOT = as.numeric(SGOT)
SGOT = SGOT[2:length(SGOT)]
SGPT = na.omit(SGPT)
SGPT = as.numeric(SGPT)
SGPT = SGPT[2:length(SGPT)]
SODIUM170 = na.omit(SODIUM170)
SODIUM170 = as.numeric(SODIUM170)
SODIUM170 = SODIUM170[2:length(SODIUM170)]
CREATININE = na.omit(CREATININE)
CREATININE = as.numeric(CREATININE)
CREATININE = CREATININE[2:length(CREATININE)]
POTASSIUM = na.omit(POTASSIUM)
POTASSIUM = as.numeric(POTASSIUM)
POTASSIUM = POTASSIUM[2:length(POTASSIUM)]
BILIRUBIN = na.omit(BILIRUBIN)
BILIRUBIN = as.numeric(BILIRUBIN)
BILIRUBIN = BILIRUBIN[2:length(BILIRUBIN)]
BILIRUBIN_INDIRECT = na.omit(BILIRUBIN_INDIRECT)
BILIRUBIN_INDIRECT = as.numeric(BILIRUBIN_INDIRECT)
BILIRUBIN_INDIRECT = BILIRUBIN_INDIRECT[2:length(BILIRUBIN_INDIRECT)]
PROTHROMBIN = na.omit(PROTHROMBIN)
PROTHROMBIN = as.numeric(PROTHROMBIN)
PROTHROMBIN = PROTHROMBIN[2:length(PROTHROMBIN)]
INR = na.omit(INR)
INR = as.numeric(INR)
INR = INR[2:length(INR)]

#histograms to visualize each variable
ggplot(as.data.frame(SGOT), 
       aes(x=SGOT)) + coord_cartesian(xlim=c(0, 1500)) +
  geom_histogram(bins = 800) +
  labs(x = "SGOT (u/L)", y = "Frequency")

ggplot(as.data.frame(SGPT), 
       aes(x=SGPT)) + coord_cartesian(xlim=c(0, 1500)) +
  geom_histogram(bins = 800) +
  labs(x = "SGPT (u/L)", y = "Frequency")

ggplot(as.data.frame(SODIUM170), 
       aes(x=SODIUM170)) + coord_cartesian(xlim=c(0, 250)) +
  geom_histogram(bins = 1600) +
  labs(x = "Sodium 170 (mmEq/L)", y = "Frequency")

ggplot(as.data.frame(CREATININE), 
       aes(x=CREATININE)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 800) +
  labs(x = "Creatinine (mg/dL)", y = "Frequency")

ggplot(as.data.frame(POTASSIUM), 
       aes(x=POTASSIUM)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Potassium (mmol/L)", y = "Frequency")

ggplot(as.data.frame(BILIRUBIN), 
       aes(x=BILIRUBIN)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Bilirubin (mg/dL)", y = "Frequency")

ggplot(as.data.frame(BILIRUBIN_INDIRECT), 
       aes(x=BILIRUBIN_INDIRECT)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Bilirubin Indirect (mg/dL)", y = "Frequency")

ggplot(as.data.frame(PROTHROMBIN), 
       aes(x=PROTHROMBIN)) + coord_cartesian(xlim=c(0, 50)) +
  geom_histogram(bins = 1600) +
  labs(x = "Prothrombin (seconds)", y = "Frequency")

ggplot(as.data.frame(INR), 
       aes(x=INR)) + coord_cartesian(xlim=c(0, 5)) +
  geom_histogram(bins = 1600) +
  labs(x = "INR", y = "Frequency")




#---------------------------------------------------------------------------#
# Values data
#---------------------------------------------------------------------------#
CKMB = values$CKMB
TROPONINI = values$TROPONINI
TROPONINT = values$TROPONINT

#repeated process: get rid of the NAs, change strings to decimals, remove the header
CKMB = na.omit(CKMB)
CKMB = as.numeric(CKMB)
CKMB = CKMB[2:length(CKMB)]
TROPONINI = na.omit(TROPONINI)
TROPONINI = as.numeric(TROPONINI)
TROPONINI = TROPONINI[2:length(TROPONINI)]
TROPONINT = na.omit(TROPONINT)
TROPONINT = as.numeric(TROPONINT)
TROPONINT = TROPONINT[2:length(TROPONINT)]

#histograms to visualize each variable
ggplot(as.data.frame(CKMB), 
       aes(x=CKMB)) + coord_cartesian(xlim=c(0, 350)) +
  geom_histogram(bins = 800) +
  labs(x = "CKMB (ng/mL)", y = "Frequency")

ggplot(as.data.frame(TROPONINI), 
       aes(x=TROPONINI))  + coord_cartesian(xlim=c(0, 40)) +
  geom_histogram(bins = 800) +
  labs(x = "Troponini (ng/mL)", y = "Frequency")

ggplot(as.data.frame(TROPONINT), 
       aes(x=TROPONINT)) + coord_cartesian(xlim=c(0, 100)) + 
  geom_histogram(bins = 800) +
  labs(x = "Troponint (ng/mL)", y = "Frequency")


finalLabPanels = merge(panels,crosswalk, by.x = "DONOR_ID", by.y = "DONOR_ID")
write.csv(finalLabPanels,
          "C:/Users/student/Documents/Healthy Hearts/labpanels_crosswalk_joined.csv", 
          row.names = FALSE)
finalLabValues = merge(values,crosswalk, by.x = "DONOR_ID", by.y = "DONOR_ID")
write.csv(finalLabValues,
          "C:/Users/student/Documents/Healthy Hearts/labvalues_crosswalk_joined.csv", 
          row.names = FALSE)


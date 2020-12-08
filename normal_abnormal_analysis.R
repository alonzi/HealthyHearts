data_dir = "C:/Users/Megan/Documents/Capstone/Data/Full Longitudinal Analysis"
setwd(data_dir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(ggpubr)
library(xlsx)

#---------------------------------------------------------------------------#
# Read in data
#---------------------------------------------------------------------------#

data <- read.csv("full7100withduration.csv")
data.after.BD <- data[which(data$Duration>=0),]

donor.data <- read_csv("Donor_Data.csv")
donor.info <- donor.data[,c("DONOR_ID","age.group","age")]

data.after.BD <- merge(data.after.BD, donor.info, by="DONOR_ID")

#---------------------------------------------------------------------------#
# Bin duration
#---------------------------------------------------------------------------#

data.after.BD$Duration.group <- rep(NA, nrow(data.after.BD))

data.after.BD <-mutate(data.after.BD, Duration.group = case_when(
  data.after.BD$Duration < 6 & data.after.BD$Duration >= 0 ~ "0-6",
  data.after.BD$Duration < 12 & data.after.BD$Duration >= 6 ~ "06-12",
  data.after.BD$Duration < 18 & data.after.BD$Duration >= 12 ~ "12-18",
  data.after.BD$Duration < 24 & data.after.BD$Duration >= 18 ~ "18-24",
  data.after.BD$Duration < 36 & data.after.BD$Duration >= 24 ~ "24-36",
  data.after.BD$Duration < 48 & data.after.BD$Duration >= 36 ~ "36-48",
  data.after.BD$Duration >= 48 ~ "48+"
))

data.after.BD$Duration.group <- as.factor(data.after.BD$Duration.group)
data.after.BD$DONOR_ID <- as.factor(data.after.BD$DONOR_ID)

#---------------------------------------------------------------------------#
# Separate into accepted and rejected
#---------------------------------------------------------------------------#

accepted.after.BD <- data.after.BD[which(data.after.BD$Accepted == "Yes"),]
rejected.after.BD <- data.after.BD[which(data.after.BD$Accepted == "No"),]

#---------------------------------------------------------------------------#
# Plots - Compare abnormal/normal measurements at each binned duration
#---------------------------------------------------------------------------#

#PAO2
accepted.after.BD$PAO2.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, PAO2.group = case_when(
  accepted.after.BD$PAO2 < 60 | accepted.after.BD$PAO2 > 120 ~ "Abnormal",
  accepted.after.BD$PAO2 >= 60 & accepted.after.BD$PAO2 <= 120 ~ "Normal"
))

rejected.after.BD$PAO2.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, PAO2.group = case_when(
  rejected.after.BD$PAO2 < 60 | rejected.after.BD$PAO2 > 120 ~ "Abnormal",
  rejected.after.BD$PAO2 >= 60 & rejected.after.BD$PAO2 <= 120 ~ "Normal"
))

PAO2.A <- ggplot(data=subset(accepted.after.BD, !is.na(PAO2.group)), aes(x=Duration.group, fill=PAO2.group, na.rm = TRUE))+geom_bar(position="fill") + ggtitle("Accepted Donors") + xlab("Time since Brain Death (hours)")
PAO2.R <- ggplot(data=subset(rejected.after.BD, !is.na(PAO2.group)), aes(x=Duration.group, fill=PAO2.group, na.rm = TRUE))+geom_bar(position="fill") + ggtitle("Rejected Donors")+ xlab("Time since Brain Death (hours)")

ggarrange(PAO2.A, PAO2.R, nrow = 1, ncol = 2)

# ABG_PH
accepted.after.BD$PH.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, PH.group = case_when(
  accepted.after.BD$ABG_PH < 7.35 | accepted.after.BD$ABG_PH > 7.45 ~ "Abnormal",
  accepted.after.BD$ABG_PH >= 7.35 & accepted.after.BD$ABG_PH <= 7.45 ~ "Normal"
))

rejected.after.BD$PH.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, PH.group = case_when(
  rejected.after.BD$ABG_PH < 7.35 | rejected.after.BD$ABG_PH > 7.45 ~ "Abnormal",
  rejected.after.BD$ABG_PH >= 7.35 & rejected.after.BD$ABG_PH <= 7.45 ~ "Normal"
))

pH.A <- ggplot(data=subset(accepted.after.BD, !is.na(PH.group)), aes(x=Duration.group, fill=PH.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
pH.R <- ggplot(data=subset(rejected.after.BD, !is.na(PH.group)), aes(x=Duration.group, fill=PH.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(pH.A, pH.R, nrow = 1, ncol = 2)

# PEEP
accepted.after.BD$PEEP.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, PEEP.group = case_when(
  accepted.after.BD$PEEP < 5 | accepted.after.BD$PEEP > 8 ~ "Abnormal",
  accepted.after.BD$PEEP >= 5 & accepted.after.BD$PEEP <= 8 ~ "Normal"
))

rejected.after.BD$PEEP.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, PEEP.group = case_when(
  rejected.after.BD$PEEP < 5 | rejected.after.BD$PEEP > 8 ~ "Abnormal",
  rejected.after.BD$PEEP >= 5 & rejected.after.BD$PEEP <= 8 ~ "Normal"
))

PEEP.A <- ggplot(data=subset(accepted.after.BD, !is.na(PEEP.group)), aes(x=Duration.group, fill=PEEP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
PEEP.R <- ggplot(data=subset(rejected.after.BD, !is.na(PEEP.group)), aes(x=Duration.group, fill=PEEP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(PEEP.A, PEEP.R, nrow = 1, ncol = 2)

#HGB
accepted.after.BD$HGB.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, HGB.group = case_when(
  accepted.after.BD$HGB < 7 | accepted.after.BD$HGB > 15 ~ "Abnormal",
  accepted.after.BD$HGB >= 7 & accepted.after.BD$HGB <= 15 ~ "Normal"
))

rejected.after.BD$HGB.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, HGB.group = case_when(
  rejected.after.BD$HGB < 7 | rejected.after.BD$HGB > 15 ~ "Abnormal",
  rejected.after.BD$HGB >= 7 & rejected.after.BD$HGB <= 15 ~ "Normal"
))

HGB.A <- ggplot(data=subset(accepted.after.BD, !is.na(HGB.group)), aes(x=Duration.group, fill=HGB.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
HGB.R <- ggplot(data=subset(rejected.after.BD, !is.na(HGB.group)), aes(x=Duration.group, fill=HGB.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(HGB.A, HGB.R, nrow = 1, ncol = 2)


#HCT
accepted.after.BD$HCT.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, HCT.group = case_when(
  accepted.after.BD$HCT < 32 | accepted.after.BD$HCT > 45 ~ "Abnormal",
  accepted.after.BD$HCT >= 32 & accepted.after.BD$HCT <= 45 ~ "Normal"
))


rejected.after.BD$HCT.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, HCT.group = case_when(
  rejected.after.BD$HCT < 32 | rejected.after.BD$HCT > 45 ~ "Abnormal",
  rejected.after.BD$HCT >= 32 & rejected.after.BD$HCT <= 45 ~ "Normal"
))

HCT.A <- ggplot(data=subset(accepted.after.BD, !is.na(HCT.group)), aes(x=Duration.group, fill=HCT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
HCT.R <- ggplot(data=subset(rejected.after.BD, !is.na(HCT.group)), aes(x=Duration.group, fill=HCT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(HCT.A, HCT.R, nrow = 1, ncol = 2)

#AVG_BP_DIAST
accepted.after.BD$Diast.BP.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Diast.BP.group = case_when(
  accepted.after.BD$AVG_BP_DIAST < 45 | accepted.after.BD$AVG_BP_DIAST > 65 & accepted.after.BD$age.group == "0-6 months" ~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85  & accepted.after.BD$age.group == "0-6 months" ~ "Normal",
  
  accepted.after.BD$AVG_BP_DIAST < 55 | accepted.after.BD$AVG_BP_DIAST > 65 & accepted.after.BD$age.group == "6-12 months"~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85 & accepted.after.BD$age.group == "6-12 months"~ "Normal",
  
  accepted.after.BD$AVG_BP_DIAST < 55 | accepted.after.BD$AVG_BP_DIAST > 70 & accepted.after.BD$age.group == "1-3 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85 & accepted.after.BD$age.group == "1-3 years" ~ "Normal",
  
  accepted.after.BD$AVG_BP_DIAST < 60 | accepted.after.BD$AVG_BP_DIAST > 75 & accepted.after.BD$age.group == "3-6 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85 & accepted.after.BD$age.group == "3-6 years" ~ "Normal",
  
  accepted.after.BD$AVG_BP_DIAST < 60 | accepted.after.BD$AVG_BP_DIAST > 75 & accepted.after.BD$age.group == "6-12 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85 & accepted.after.BD$age.group == "6-12 years" ~ "Normal",
  
  accepted.after.BD$AVG_BP_DIAST < 65 | accepted.after.BD$AVG_BP_DIAST > 85 & accepted.after.BD$age.group == "12+ years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_DIAST >= 65 & accepted.after.BD$AVG_BP_DIAST <= 85 & accepted.after.BD$age.group == "12+ years" ~ "Normal"
))

rejected.after.BD$Diast.BP.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Diast.BP.group = case_when(
  rejected.after.BD$AVG_BP_DIAST < 45 | rejected.after.BD$AVG_BP_DIAST > 65 & rejected.after.BD$age.group == "0-6 months" ~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85  & rejected.after.BD$age.group == "0-6 months" ~ "Normal",
  
  rejected.after.BD$AVG_BP_DIAST < 55 | rejected.after.BD$AVG_BP_DIAST > 65 & rejected.after.BD$age.group == "6-12 months"~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85 & rejected.after.BD$age.group == "6-12 months"~ "Normal",
  
  rejected.after.BD$AVG_BP_DIAST < 55 | rejected.after.BD$AVG_BP_DIAST > 70 & rejected.after.BD$age.group == "1-3 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85 & rejected.after.BD$age.group == "1-3 years" ~ "Normal",
  
  rejected.after.BD$AVG_BP_DIAST < 60 | rejected.after.BD$AVG_BP_DIAST > 75 & rejected.after.BD$age.group == "3-6 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85 & rejected.after.BD$age.group == "3-6 years" ~ "Normal",
  
  rejected.after.BD$AVG_BP_DIAST < 60 | rejected.after.BD$AVG_BP_DIAST > 75 & rejected.after.BD$age.group == "6-12 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85 & rejected.after.BD$age.group == "6-12 years" ~ "Normal",
  
  rejected.after.BD$AVG_BP_DIAST < 65 | rejected.after.BD$AVG_BP_DIAST > 85 & rejected.after.BD$age.group == "12+ years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_DIAST >= 65 & rejected.after.BD$AVG_BP_DIAST <= 85 & rejected.after.BD$age.group == "12+ years" ~ "Normal"
))

Diast.A <- ggplot(data=subset(accepted.after.BD, !is.na(Diast.BP.group)), aes(x=Duration.group, fill=Diast.BP.group, na.rm = TRUE)) + 
  geom_bar(position="fill") + ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Diast.R <- ggplot(data=subset(rejected.after.BD, !is.na(Diast.BP.group)), aes(x=Duration.group, fill=Diast.BP.group, na.rm = TRUE)) + 
  geom_bar(position="fill") + ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Diast.A, Diast.R, nrow = 1, ncol = 2)

#AVG_BP_SYST
accepted.after.BD$Syst.BP.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Syst.BP.group = case_when(
  accepted.after.BD$AVG_BP_SYST < 70 | accepted.after.BD$AVG_BP_SYST > 90 & accepted.after.BD$age.group == "0-6 months" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 70 & accepted.after.BD$AVG_BP_SYST <= 90 & accepted.after.BD$age.group == "0-6 months" ~ "Normal",
  
  accepted.after.BD$AVG_BP_SYST < 80 | accepted.after.BD$AVG_BP_SYST > 100 & accepted.after.BD$age.group == "6-12 months" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 80 & accepted.after.BD$AVG_BP_SYST <= 100 & accepted.after.BD$age.group == "6-12 months" ~ "Normal",
  
  accepted.after.BD$AVG_BP_SYST < 90 | accepted.after.BD$AVG_BP_SYST > 105 & accepted.after.BD$age.group == "1-3 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 90 & accepted.after.BD$AVG_BP_SYST <= 105 & accepted.after.BD$age.group == "1-3 years" ~ "Normal",
  
  accepted.after.BD$AVG_BP_SYST < 95 | accepted.after.BD$AVG_BP_SYST > 110 & accepted.after.BD$age.group == "3-6 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 95 & accepted.after.BD$AVG_BP_SYST <= 110 & accepted.after.BD$age.group == "3-6 years" ~ "Normal",
  
  accepted.after.BD$AVG_BP_SYST < 100 | accepted.after.BD$AVG_BP_SYST > 120 & accepted.after.BD$age.group == "6-12 years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 100 & accepted.after.BD$AVG_BP_SYST <= 120 & accepted.after.BD$age.group == "6-12 years"~ "Normal",
  
  accepted.after.BD$AVG_BP_SYST < 100 | accepted.after.BD$AVG_BP_SYST > 135 & accepted.after.BD$age.group == "12+ years" ~ "Abnormal",
  accepted.after.BD$AVG_BP_SYST >= 100 & accepted.after.BD$AVG_BP_SYST <= 135 & accepted.after.BD$age.group == "12+ years" ~ "Normal"
))

rejected.after.BD$Syst.BP.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Syst.BP.group = case_when(
  rejected.after.BD$AVG_BP_SYST < 70 | rejected.after.BD$AVG_BP_SYST > 90 & rejected.after.BD$age.group == "0-6 months" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 70 & rejected.after.BD$AVG_BP_SYST <= 90 & rejected.after.BD$age.group == "0-6 months" ~ "Normal",
  
  rejected.after.BD$AVG_BP_SYST < 80 | rejected.after.BD$AVG_BP_SYST > 100 & rejected.after.BD$age.group == "6-12 months" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 80 & rejected.after.BD$AVG_BP_SYST <= 100 & rejected.after.BD$age.group == "6-12 months" ~ "Normal",
  
  rejected.after.BD$AVG_BP_SYST < 90 | rejected.after.BD$AVG_BP_SYST > 105 & rejected.after.BD$age.group == "1-3 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 90 & rejected.after.BD$AVG_BP_SYST <= 105 & rejected.after.BD$age.group == "1-3 years" ~ "Normal",
  
  rejected.after.BD$AVG_BP_SYST < 95 | rejected.after.BD$AVG_BP_SYST > 110 & rejected.after.BD$age.group == "3-6 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 95 & rejected.after.BD$AVG_BP_SYST <= 110 & rejected.after.BD$age.group == "3-6 years" ~ "Normal",
  
  rejected.after.BD$AVG_BP_SYST < 100 | rejected.after.BD$AVG_BP_SYST > 120 & rejected.after.BD$age.group == "6-12 years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 100 & rejected.after.BD$AVG_BP_SYST <= 120 & rejected.after.BD$age.group == "6-12 years"~ "Normal",
  
  rejected.after.BD$AVG_BP_SYST < 100 | rejected.after.BD$AVG_BP_SYST > 135 & rejected.after.BD$age.group == "12+ years" ~ "Abnormal",
  rejected.after.BD$AVG_BP_SYST >= 100 & rejected.after.BD$AVG_BP_SYST <= 135 & rejected.after.BD$age.group == "12+ years" ~ "Normal"
))

Syst.A <- ggplot(data=subset(accepted.after.BD, !is.na(Syst.BP.group)), aes(x=Duration.group, fill=Syst.BP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Syst.R <- ggplot(data=subset(rejected.after.BD, !is.na(Syst.BP.group)), aes(x=Duration.group, fill=Syst.BP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Syst.A, Syst.R, nrow = 1, ncol = 2)

# AVG_PULSE_RANGE_START
accepted.after.BD$Pulse.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Pulse.group = case_when(
  accepted.after.BD$AVG_PULSE_RANGE_START < 110 | accepted.after.BD$AVG_PULSE_RANGE_START > 160 & accepted.after.BD$age.group == "0-6 months" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 110 & accepted.after.BD$AVG_PULSE_RANGE_START <= 160 & accepted.after.BD$age.group == "0-6 months" ~ "Normal",
  
  accepted.after.BD$AVG_PULSE_RANGE_START < 90 | accepted.after.BD$AVG_PULSE_RANGE_START > 160 & accepted.after.BD$age.group == "6-12 months" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 90 & accepted.after.BD$AVG_PULSE_RANGE_START <= 160 & accepted.after.BD$age.group == "6-12 months" ~ "Normal",
  
  accepted.after.BD$AVG_PULSE_RANGE_START < 80 | accepted.after.BD$AVG_PULSE_RANGE_START > 150 & accepted.after.BD$age.group == "1-3 years" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 80 & accepted.after.BD$AVG_PULSE_RANGE_START <= 150 & accepted.after.BD$age.group == "1-3 years" ~ "Normal",
  
  accepted.after.BD$AVG_PULSE_RANGE_START < 70 | accepted.after.BD$AVG_PULSE_RANGE_START > 120 & accepted.after.BD$age.group == "3-6 years" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 70 & accepted.after.BD$AVG_PULSE_RANGE_START <= 120 & accepted.after.BD$age.group == "3-6 years" ~ "Normal",
  
  accepted.after.BD$AVG_PULSE_RANGE_START < 60 | accepted.after.BD$AVG_PULSE_RANGE_START > 110 & accepted.after.BD$age.group == "6-12 years" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 60 & accepted.after.BD$AVG_PULSE_RANGE_START <= 110 & accepted.after.BD$age.group == "6-12 years" ~ "Normal",
  
  accepted.after.BD$AVG_PULSE_RANGE_START < 60 | accepted.after.BD$AVG_PULSE_RANGE_START > 100 & accepted.after.BD$age.group == "12+ years" ~ "Abnormal",
  accepted.after.BD$AVG_PULSE_RANGE_START >= 60 & accepted.after.BD$AVG_PULSE_RANGE_START <= 100 & accepted.after.BD$age.group == "12+ years" ~ "Normal"
))

rejected.after.BD$Pulse.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Pulse.group = case_when(
  rejected.after.BD$AVG_PULSE_RANGE_START < 110 | rejected.after.BD$AVG_PULSE_RANGE_START > 160 & rejected.after.BD$age.group == "0-6 months" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 110 & rejected.after.BD$AVG_PULSE_RANGE_START <= 160 & rejected.after.BD$age.group == "0-6 months" ~ "Normal",
  
  rejected.after.BD$AVG_PULSE_RANGE_START < 90 | rejected.after.BD$AVG_PULSE_RANGE_START > 160 & rejected.after.BD$age.group == "6-12 months" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 90 & rejected.after.BD$AVG_PULSE_RANGE_START <= 160 & rejected.after.BD$age.group == "6-12 months" ~ "Normal",
  
  rejected.after.BD$AVG_PULSE_RANGE_START < 80 | rejected.after.BD$AVG_PULSE_RANGE_START > 150 & rejected.after.BD$age.group == "1-3 years" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 80 & rejected.after.BD$AVG_PULSE_RANGE_START <= 150 & rejected.after.BD$age.group == "1-3 years" ~ "Normal",
  
  rejected.after.BD$AVG_PULSE_RANGE_START < 70 | rejected.after.BD$AVG_PULSE_RANGE_START > 120 & rejected.after.BD$age.group == "3-6 years" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 70 & rejected.after.BD$AVG_PULSE_RANGE_START <= 120 & rejected.after.BD$age.group == "3-6 years" ~ "Normal",
  
  rejected.after.BD$AVG_PULSE_RANGE_START < 60 | rejected.after.BD$AVG_PULSE_RANGE_START > 110 & rejected.after.BD$age.group == "6-12 years" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 60 & rejected.after.BD$AVG_PULSE_RANGE_START <= 110 & rejected.after.BD$age.group == "6-12 years" ~ "Normal",
  
  rejected.after.BD$AVG_PULSE_RANGE_START < 60 | rejected.after.BD$AVG_PULSE_RANGE_START > 100 & rejected.after.BD$age.group == "12+ years" ~ "Abnormal",
  rejected.after.BD$AVG_PULSE_RANGE_START >= 60 & rejected.after.BD$AVG_PULSE_RANGE_START <= 100 & rejected.after.BD$age.group == "12+ years" ~ "Normal"
))

Pulse.A <- ggplot(data=subset(accepted.after.BD, !is.na(Pulse.group)), aes(x=Duration.group, fill=Pulse.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Pulse.R <- ggplot(data=subset(rejected.after.BD, !is.na(Pulse.group)), aes(x=Duration.group, fill=Pulse.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Pulse.A, Pulse.R, nrow = 1, ncol = 2)

#CVP_INT_RANGE_START
accepted.after.BD$CVP.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, CVP.group = case_when(
  accepted.after.BD$CVP_INT_RANGE_START < 5 | accepted.after.BD$CVP_INT_RANGE_START > 12 ~ "Abnormal",
  accepted.after.BD$CVP_INT_RANGE_START >= 5 & accepted.after.BD$CVP_INT_RANGE_START <= 12 ~ "Normal"
  
))

rejected.after.BD$CVP.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, CVP.group = case_when(
  rejected.after.BD$CVP_INT_RANGE_START < 5 | rejected.after.BD$CVP_INT_RANGE_START > 12  ~ "Abnormal",
  rejected.after.BD$CVP_INT_RANGE_START >= 5 & rejected.after.BD$CVP_INT_RANGE_START <= 12  ~ "Normal"
  
))

CVP.A <- ggplot(data=subset(accepted.after.BD, !is.na(CVP.group)), aes(x=Duration.group, fill=CVP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
CVP.R <- ggplot(data=subset(rejected.after.BD, !is.na(CVP.group)), aes(x=Duration.group, fill=CVP.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(CVP.A, CVP.R, nrow = 1, ncol = 2)

#BODYTEMP_RANGE_START - Similar
accepted.after.BD$BodyTemp.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, BodyTemp.group = case_when(
  accepted.after.BD$BODYTEMP_RANGE_START < 36 | accepted.after.BD$BODYTEMP_RANGE_START > 38 ~ "Abnormal",
  accepted.after.BD$BODYTEMP_RANGE_START >= 36 & accepted.after.BD$BODYTEMP_RANGE_START <= 38 ~ "Normal"
))

rejected.after.BD$BodyTemp.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, BodyTemp.group = case_when(
  rejected.after.BD$BODYTEMP_RANGE_START < 36 | rejected.after.BD$BODYTEMP_RANGE_START > 38 ~ "Abnormal",
  rejected.after.BD$BODYTEMP_RANGE_START >= 36 & rejected.after.BD$BODYTEMP_RANGE_START <= 38 ~ "Normal"
))

BodyTemp.A <- ggplot(data=subset(accepted.after.BD, !is.na(BodyTemp.group)), aes(x=Duration.group, fill=BodyTemp.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
BodyTemp.R <- ggplot(data=subset(rejected.after.BD, !is.na(BodyTemp.group)), aes(x=Duration.group, fill=BodyTemp.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(BodyTemp.A, BodyTemp.R, nrow = 1, ncol = 2)

#SGOT - Similar
accepted.after.BD$SGOT.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, SGOT.group = case_when(
  accepted.after.BD$SGOT < 100  ~ "Normal",
  accepted.after.BD$SGOT >= 100 ~ "Abnormal"
))

rejected.after.BD$SGOT.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, SGOT.group = case_when(
  rejected.after.BD$SGOT < 100  ~ "Normal",
  rejected.after.BD$SGOT >= 100 ~ "Abnormal"
))

SGOT.A <- ggplot(data=subset(accepted.after.BD, !is.na(SGOT.group)), aes(x=Duration.group, fill=SGOT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
SGOT.R <- ggplot(data=subset(rejected.after.BD, !is.na(SGOT.group)), aes(x=Duration.group, fill=SGOT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(SGOT.A, SGOT.R, nrow = 1, ncol = 2)

#SGPT
accepted.after.BD$SGPT.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, SGPT.group = case_when(
  accepted.after.BD$SGPT < 100  ~ "Normal",
  accepted.after.BD$SGPT >= 100 ~ "Abnormal"
))

rejected.after.BD$SGPT.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, SGPT.group = case_when(
  rejected.after.BD$SGPT < 100  ~ "Normal",
  rejected.after.BD$SGPT >= 100 ~ "Abnormal"
))

SGPT.A <- ggplot(data=subset(accepted.after.BD, !is.na(SGPT.group)), aes(x=Duration.group, fill=SGPT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
SGPT.R <- ggplot(data=subset(rejected.after.BD, !is.na(SGPT.group)), aes(x=Duration.group, fill=SGPT.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(SGPT.A, SGPT.R, nrow = 1, ncol = 2)

#SODIUM170
accepted.after.BD$Sodium.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Sodium.group = case_when(
  accepted.after.BD$SODIUM170 < 130 | accepted.after.BD$SODIUM170 > 150 ~ "Abnormal",
  accepted.after.BD$SODIUM170 >= 130 & accepted.after.BD$SODIUM170 <= 150 ~ "Normal"
))

rejected.after.BD$Sodium.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Sodium.group = case_when(
  rejected.after.BD$SODIUM170 < 130 | rejected.after.BD$SODIUM170 > 150 ~ "Abnormal",
  rejected.after.BD$SODIUM170 >= 130 & rejected.after.BD$SODIUM170 <= 150 ~ "Normal"
))

Sodium.A <- ggplot(data=subset(accepted.after.BD, !is.na(Sodium.group)), aes(x=Duration.group, fill=Sodium.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Sodium.R <- ggplot(data=subset(rejected.after.BD, !is.na(Sodium.group)), aes(x=Duration.group, fill=Sodium.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Sodium.A, Sodium.R, nrow = 1, ncol = 2)

#CREATININE - Rejected weirdly higher
accepted.after.BD$Creatinine.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Creatinine.group = case_when(
  accepted.after.BD$CREATININE < 0.5 | accepted.after.BD$CREATININE > 0.9 & accepted.after.BD$age >= 10 ~ "Abnormal",
  accepted.after.BD$CREATININE >= 0.5 & accepted.after.BD$CREATININE <= 0.9 & accepted.after.BD$age >= 10 ~ "Normal",
  
  accepted.after.BD$CREATININE > 0.5 & accepted.after.BD$age < 10 ~ "Abnormal",
  accepted.after.BD$CREATININE <= 0.5 & accepted.after.BD$age < 10 ~ "Normal"
))

rejected.after.BD$Creatinine.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Creatinine.group = case_when(
  rejected.after.BD$CREATININE < 0.5 | rejected.after.BD$CREATININE > 0.9 & rejected.after.BD$age >= 10 ~ "Abnormal",
  rejected.after.BD$CREATININE >= 0.5 & rejected.after.BD$CREATININE <= 0.9 & rejected.after.BD$age >= 10 ~ "Normal",
  
  rejected.after.BD$CREATININE > 0.5 & rejected.after.BD$age < 10 ~ "Abnormal",
  rejected.after.BD$CREATININE <= 0.5 & rejected.after.BD$age < 10 ~ "Normal"
))

Creat.A <- ggplot(data=subset(accepted.after.BD, !is.na(Creatinine.group)), aes(x=Duration.group, fill=Creatinine.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Creat.R <- ggplot(data=subset(rejected.after.BD, !is.na(Creatinine.group)), aes(x=Duration.group, fill=Creatinine.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Creat.A, Creat.R, nrow = 1, ncol = 2)

#POTASSIUM - Similar
accepted.after.BD$Potassium.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Potassium.group = case_when(
  accepted.after.BD$POTASSIUM < 3 | accepted.after.BD$POTASSIUM > 5.5 ~ "Abnormal",
  accepted.after.BD$POTASSIUM >= 3 & accepted.after.BD$POTASSIUM <= 5.5 ~ "Normal"
))

rejected.after.BD$Potassium.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Potassium.group = case_when(
  rejected.after.BD$POTASSIUM < 3 | rejected.after.BD$POTASSIUM > 5.5 ~ "Abnormal",
  rejected.after.BD$POTASSIUM >= 3 & rejected.after.BD$POTASSIUM <= 5.5 ~ "Normal"
))


Potas.A <- ggplot(data=subset(accepted.after.BD, !is.na(Potassium.group)), aes(x=Duration.group, fill=Potassium.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Potas.R <- ggplot(data=subset(rejected.after.BD, !is.na(Potassium.group)), aes(x=Duration.group, fill=Potassium.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Potas.A, Potas.R, nrow = 1, ncol = 2)

#BILIRUBIN
accepted.after.BD$Bilirubin.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Bilirubin.group = case_when(
  accepted.after.BD$BILIRUBIN >=1  | accepted.after.BD$BILIRUBIN_INDIRECT >= 1 ~ "Abnormal",
  accepted.after.BD$BILIRUBIN < 1 | accepted.after.BD$BILIRUBIN_INDIRECT < 1 ~ "Normal"
))

rejected.after.BD$Bilirubin.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Bilirubin.group = case_when(
  rejected.after.BD$BILIRUBIN >=1  | rejected.after.BD$BILIRUBIN_INDIRECT >= 1 ~ "Abnormal",
  rejected.after.BD$BILIRUBIN < 1 | rejected.after.BD$BILIRUBIN_INDIRECT < 1 ~ "Normal"
))

Bil.A <- ggplot(data=subset(accepted.after.BD, !is.na(Bilirubin.group)), aes(x=Duration.group, fill=Bilirubin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Bil.R <- ggplot(data=subset(rejected.after.BD, !is.na(Bilirubin.group)), aes(x=Duration.group, fill=Bilirubin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Bil.A, Bil.R, nrow = 1, ncol = 2)

#PROTHROMBIN
accepted.after.BD$Prothrombin.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Prothrombin.group = case_when(
  accepted.after.BD$PROTHROMBIN >= 45 ~ "Abnormal",
  accepted.after.BD$PROTHROMBIN < 45 ~ "Normal"
))

rejected.after.BD$Prothrombin.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Prothrombin.group = case_when(
  rejected.after.BD$PROTHROMBIN >= 45 ~ "Abnormal",
  rejected.after.BD$PROTHROMBIN < 45 ~ "Normal"
))

Pro.A <- ggplot(data=subset(accepted.after.BD, !is.na(Prothrombin.group)), aes(x=Duration.group, fill=Prothrombin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Pro.R <- ggplot(data=subset(rejected.after.BD, !is.na(Prothrombin.group)), aes(x=Duration.group, fill=Prothrombin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Pro.A, Pro.R, nrow = 1, ncol = 2)

#INR
accepted.after.BD$INR.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, INR.group = case_when(
  accepted.after.BD$INR >= 1 ~ "Abnormal",
  accepted.after.BD$INR < 1 ~ "Normal"
))

rejected.after.BD$INR.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, INR.group = case_when(
  rejected.after.BD$INR >= 1 ~ "Abnormal",
  rejected.after.BD$INR < 1 ~ "Normal"
))

INR.A <- ggplot(data=subset(accepted.after.BD, !is.na(INR.group)), aes(x=Duration.group, fill=INR.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
INR.R <- ggplot(data=subset(rejected.after.BD, !is.na(INR.group)), aes(x=Duration.group, fill=INR.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(INR.A, INR.R, nrow = 1, ncol = 2)

#CKMB
accepted.after.BD$CKMB.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, CKMB.group = case_when(
  accepted.after.BD$CKMB >= 25 ~ "Abnormal",
  accepted.after.BD$CKMB < 25 ~ "Normal"
))

rejected.after.BD$CKMB.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, CKMB.group = case_when(
  rejected.after.BD$CKMB >= 25 ~ "Abnormal",
  rejected.after.BD$CKMB < 25 ~ "Normal"
))

CKMB.A <- ggplot(data=subset(accepted.after.BD, !is.na(CKMB.group)), aes(x=Duration.group, fill=CKMB.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
CKMB.R <- ggplot(data=subset(rejected.after.BD, !is.na(CKMB.group)), aes(x=Duration.group, fill=CKMB.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(CKMB.A, CKMB.R, nrow = 1, ncol = 2)

#TROPONINI
accepted.after.BD$Troponin.group <- rep(NA, nrow(accepted.after.BD))
accepted.after.BD <-mutate(accepted.after.BD, Troponin.group = case_when(
  accepted.after.BD$TROPONINI >= .08 ~ "Abnormal",
  accepted.after.BD$TROPONINI < .08 ~ "Normal"
))

rejected.after.BD$Troponin.group <- rep(NA, nrow(rejected.after.BD))
rejected.after.BD <-mutate(rejected.after.BD, Troponin.group = case_when(
  rejected.after.BD$TROPONINI >= .08 ~ "Abnormal",
  rejected.after.BD$TROPONINI < .08 ~ "Normal"
))

Trop.A <- ggplot(data=subset(accepted.after.BD, !is.na(Troponin.group)), aes(x=Duration.group, fill=Troponin.group))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")
Trop.R <- ggplot(data=subset(rejected.after.BD, !is.na(Troponin.group)), aes(x=Duration.group, fill=Troponin.group))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")

ggarrange(Trop.A, Trop.R, nrow = 1, ncol = 2)

combined <- rbind(accepted.after.BD, rejected.after.BD)
combined <- mutate(combined, Accepted = case_when(
  combined$Accepted == "Yes" ~ "Accepted",
  combined$Accepted == "No" ~ "Rejected"
))

#---------------------------------------------------------------------------#
# Frequency calculations - Compare abnormal/normal measurements at each binned duration
#---------------------------------------------------------------------------#


ftable(table(combined$Accepted,combined$Troponin.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Pulse.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Creatinine.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Bilirubin.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$CVP.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Diast.BP.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Syst.BP.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$SGPT.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$PAO2.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$HCT.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$PH.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$HGB.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$BodyTemp.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Sodium.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$SGOT.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Potassium.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$CKMB.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$PEEP.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$Prothrombin.group, combined$Duration.group))
ftable(table(combined$Accepted,combined$INR.group, combined$Duration.group))

pulse.box.A1 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "0-6 months"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 110, linetype="dashed", color = "red") +geom_hline(yintercept = 160, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.A2 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "6-12 months"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 90, linetype="dashed", color = "red") +geom_hline(yintercept = 160, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.A3 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "1-3 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 80, linetype="dashed", color = "red") +geom_hline(yintercept = 150, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.A4 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "3-6 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 70, linetype="dashed", color = "red") +geom_hline(yintercept = 120, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.A5 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "6-12 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 110, linetype="dashed", color = "red") +geom_hline(yintercept = 60, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.A6 <- ggplot(data = accepted.after.BD[which(accepted.after.BD$age.group == "12+ years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 100, linetype="dashed", color = "red") +geom_hline(yintercept = 60, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R1 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "0-6 months"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 110, linetype="dashed", color = "red") +geom_hline(yintercept = 160, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R2 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "6-12 months"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 90, linetype="dashed", color = "red") +geom_hline(yintercept = 160, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R3 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "1-3 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 80, linetype="dashed", color = "red") +geom_hline(yintercept = 150, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R4 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "3-6 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 70, linetype="dashed", color = "red") +geom_hline(yintercept = 120, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R5 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "6-12 years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 110, linetype="dashed", color = "red") +geom_hline(yintercept = 60, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
pulse.box.R6 <- ggplot(data = rejected.after.BD[which(rejected.after.BD$age.group == "12+ years"),], aes(x = Duration.group, y = AVG_PULSE_RANGE_START)) + 
  geom_boxplot() + ylab("Pulse (bpm)") + geom_hline(yintercept = 100, linetype="dashed", color = "red") +geom_hline(yintercept = 60, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,225))
ggarrange(pulse.box.A1, pulse.box.A2, pulse.box.A3, pulse.box.A4, pulse.box.A5, pulse.box.A6,
          pulse.box.R1, pulse.box.R2, pulse.box.R3, pulse.box.R4, pulse.box.R5, pulse.box.R6, nrow = 2, ncol = 6)

Creat.box.A1 <-ggplot(data = accepted.after.BD[which(accepted.after.BD$age < 10),], aes(x = Duration.group, y = CREATININE)) + 
  geom_boxplot() + ylab("CREATININE") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red")  + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5)) + ggtitle("Accepted under 10")
Creat.box.A2 <-ggplot(data = accepted.after.BD[which(accepted.after.BD$age >= 10),], aes(x = Duration.group, y = CREATININE)) + 
  geom_boxplot() + ylab("CREATININE") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +geom_hline(yintercept = 0.9, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ ggtitle("Accepted over 10")
Creat.box.R1 <- ggplot(data = rejected.after.BD[which(accepted.after.BD$age < 10),], aes(x = Duration.group, y = CREATININE)) + 
  geom_boxplot() + ylab("CREATININE") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ ggtitle("Rejected under 10")
Creat.box.R2 <- ggplot(data = rejected.after.BD[which(accepted.after.BD$age >= 10),], aes(x = Duration.group, y = CREATININE)) + 
  geom_boxplot() + ylab("CREATININE") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +geom_hline(yintercept = 0.9, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ ggtitle("Rejected over 10")
ggarrange(Creat.box.A1, Creat.box.A2, Creat.box.R1, Creat.box.R2, nrow = 2, ncol = 2)

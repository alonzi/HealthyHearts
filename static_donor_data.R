#---------------------------------------------------------------------------#
# This code creates a csv file with static information about all pediatric 
# heart donors
# 
# Age group, time of brain death, accepted or rejected
#---------------------------------------------------------------------------#

#load libraries
library(tidyverse)
library(lubridate)

#cross-walk.csv and thor_non_standard.tsv are stored in data_dir
#deceased_donor.csv from the original DonorNet files is stored in original_data_dir
data_dir = "C:/Users/Megan/Documents/Capstone/Data/Full Longitudinal Analysis"
original_data_dir = "C:/Users/Megan/Documents/Capstone/Data/Original"

#cross-walk.csv contains the 4500 donors whose hearts were accepted and have corresponding recipients
#thor_non_standard.tsv contains all donors, accepted or rejected

setwd(data_dir)

old.crosswalk <- read_csv("cross-walk.csv")
new.crosswalk <- read_tsv("thor_non_standard.tsv")

new.crosswalk$Accepted <- ifelse(is.na(match(new.crosswalk$DONOR_ID, 
                                old.crosswalk$DONOR_ID)),"No", "Yes")
old.crosswalk$Accepted <- "Yes"

new.crosswalk <- merge(new.crosswalk, old.crosswalk, all = TRUE)

setwd(original_data_dir)
deceased.donor <- read.csv("deceased_donor_data.tsv", sep = "\t")
static.data <- deceased.donor[,c("DONOR_ID","BRAIN_DEATH_DATE","BRAIN_DEATH_TIME","AGE_DON")]

static.data <- merge(new.crosswalk, static.data, by="DONOR_ID")

# Merge/format brain death and time columns

static.data$BrainDeath <- as.POSIXct(paste(static.data$BRAIN_DEATH_DATE, static.data$BRAIN_DEATH_TIME), format="%Y-%m-%d %H:%M:%S")

#new age.group variabe bins ages from 0-6 months, 6-12 months, 1-3 years, 3-6 years, 6-12 years, and 12+ years old
static.data$age.group <- rep(NA, nrow(static.data))

#Determine age.group for donors 1 year and older using AGE_DON from deceased_donor file 
static.data <-mutate(static.data, age.group = case_when(
  static.data$AGE_DON < 1 ~ "<1 year",
  static.data$AGE_DON < 3 & static.data$AGE_DON >= 1 ~ "1-3 years",
  static.data$AGE_DON < 6 & static.data$AGE_DON >= 3 ~ "3-6 years",
  static.data$AGE_DON < 12 & static.data$AGE_DON >= 6 ~ "6-12 years",
  static.data$AGE_DON >= 12 ~ "12+ years"
))

# format Donor DOB column and calculate age in weeks (to use for donors < 1 year old)
static.data$DOB_DON <- as.POSIXct(static.data$DOB_DON, format = "%Y-%m-%d")
static.data$age <- rep(NA, nrow(static.data))
static.data$age <- difftime(static.data$BrainDeath, static.data$DOB_DON, units = "weeks" )

#Determine age.group for donors less than 1 year
static.data <-mutate(static.data, age.group = case_when(
  static.data$age < 26 ~ "0-6 months",
  static.data$age < 52 & static.data$age >= 26 ~ "6-12 months",
  TRUE ~ age.group
))


# Drop unnecessary columns

drops <- c("BRAIN_DEATH_DATE","BRAIN_DEATH_TIME", "DON_ID", "TRR_ID_CODE","DOB_DON","DOB","AGE_DON","age" )
static.data <- static.data[ , !(names(static.data) %in% drops)]

write_csv(static.data,"static_donor_info")

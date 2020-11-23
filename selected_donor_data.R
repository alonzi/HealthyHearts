data_dir = "C:/Users/Megan/Documents/Capstone/Data"
setwd(data_dir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(dplyr)
library(lubridate)

#---------------------------------------------------------------------------#
# Read in data
#---------------------------------------------------------------------------#

data.full <- read.csv("first4500analysis.csv")
data <- data.full[data.full$DONOR_ID %in% c(344847, 344863, 344907, 344938, 345101, 345102, 345159, 345268, 345424, 345426),]

deceased.donor <- read.csv("deceased_donor_data.tsv", sep = "\t")

donor.info <- deceased.donor[,c("DONOR_ID","BRAIN_DEATH_DATE","BRAIN_DEATH_TIME")]

#combine datasets using DONOR_ID
data <- merge(data, donor.info, by="DONOR_ID")

#---------------------------------------------------------------------------#
# Format DT and BrainDeath time
#---------------------------------------------------------------------------#

#combine brain death and time columns
data$BrainDeath <- as.POSIXct(paste(data$BRAIN_DEATH_DATE, data$BRAIN_DEATH_TIME), format="%Y-%m-%d %H:%M:%S")

#combine BEGIN_DT and DT columns (different column names in merged files)
data <-mutate(data, DT = case_when(
  BEGIN_DT == "" ~ DT,
  BEGIN_DT != "" ~ BEGIN_DT
))

data$DT <- parse_date_time(data$DT, c("Ymd HMS"))
#format date and time column
data$DT <- as.POSIXct(data$DT,
                      format="%Y-%m-%d %H:%M:%S" #format time
)

#Add duration column and calculate for each unique donor
data$Duration <- rep(NA, nrow(data))

data <-
  data %>%
  mutate(Duration = DT - BrainDeath)

#convert duration from seconds to hours
data$Duration <- data$Duration / 3600
data$Duration <- as.numeric(data$Duration)

#---------------------------------------------------------------------------#
# Remove unnecessary columns
#---------------------------------------------------------------------------#
drops <- c("BRAIN_DEATH_DATE","BRAIN_DEATH_TIME", "BEGIN_DT","END_DT","RECTYPE", "Duration_Indicators", "AGENT_END_DT", "Duration_inotropic" )
data <- data[ , !(names(data) %in% drops)]

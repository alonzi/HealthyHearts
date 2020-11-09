data_dir = "C:/Users/Megan/Documents/Capstone/Data"
setwd(data_dir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(tidyverse)
library(tidyr)
library(haven)
library(dplyr)
library(lubridate)
library(xlsx)

#---------------------------------------------------------------------------#
# Read in data
#---------------------------------------------------------------------------#

indicators <- read.csv("INDICATORS.tsv", sep = "\t")

#percent of CVP data below 19
cvp.range <- indicators[which(indicators$CVP_INT_RANGE_START <= 22),]
nrow(cvp.range)/ (nrow(indicators) - nrow(indicators[which(is.na(indicators$CVP_INT_RANGE_START)),]))
# < 19 - 98.1%, < 22 - 99.3%

nrow(cvp.range)/ (nrow(indicators) - nrow(indicators[which(is.na(indicators$CVP_INT_RANGE_START)),]))


#---------------------------------------------------------------------------#
# Add in Brain Death/Time and calculate duration
#---------------------------------------------------------------------------#
data_dir = "C:/Users/Megan/Documents/Capstone/"
setwd(data_dir)

zipdir = file.path(data_dir, "SAS Dataset-202009.zip")

#-- List of SAS files in the zip directory
unzip(zipdir, list=TRUE) %>% 
  filter(str_detect(Name, "\\.sas7bdat")) %>% pull(Name)

#-- Function to read in STAR data
# fname: name of file (with .sas7bdat extension)
# vars: vector of column names to keep (use NULL to keep all)
# keep_ids: vector of DONOR_ID values to keep (use NULL to keep all)
# .zipdir: directory of zipped data
load_data <- function(fname, vars=NULL, keep_ids=NULL, 
                      .zipdir=zipdir) {
  #- find full file name
  fname = ifelse(str_detect(fname, "\\.sas7bdat"), 
                 yes = fname, 
                 no = str_c(fname, ".sas7bdat"))
  full_fname = unzip(zipdir, list=TRUE) %>% 
    filter(str_detect(Name, fname)) %>% pull(Name)
  #- load data + select columns
  tmp = read_sas(unzip(.zipdir, files=full_fname)) 
  #- select columns
  if(!is.null(vars)) {
    tmp = tmp %>% select(!!vars)
  }  
  #- replace "" with explicit NA
  tmp = tmp %>% mutate_if(is.character, zap_empty)
  #- keep interesting DONOR_ID's 
  if(!is.null(keep_ids)) {
    tmp = tmp %>% filter(DONOR_ID %in% !!keep_ids) 
  }  
  #- find non-empty rows
  ok = tmp %>% select(-DONOR_ID) %>% 
    apply(1, function(x) !all(is.na(x))) %>% which()
  #- return data
  tmp %>% slice(ok)
}

data = load_data("deceased_donor_data")
donor.info <- data[,c("DONOR_ID","BRAIN_DEATH_DATE","BRAIN_DEATH_TIME")]

#combine datasets using DONOR_ID
indicators <- merge(indicators, donor.info, by="DONOR_ID")

#format date and time column
indicators$BEGIN_DT <- as.POSIXct(indicators$BEGIN_DT,
                                 format="%Y-%m-%dT%H:%M" #format time
)

#combine brain death and time columns
indicators$BrainDeath <- as.POSIXct(paste(indicators$BRAIN_DEATH_DATE, indicators$BRAIN_DEATH_TIME), format="%Y-%m-%d %H:%M:%S")

#Add duration column and calculate for each unique donor
indicators$Duration <- rep(NA, nrow(indicators))

indicators <-
  indicators %>%
  group_by(DONOR_ID) %>%
  mutate(Duration = BEGIN_DT - BrainDeath)

#convert duration from seconds to hours
indicators$Duration <- indicators$Duration / 3600
indicators$Duration <- as.numeric(indicators$Duration)

#---------------------------------------------------------------------------#
# Blood Pressure - Systolic and Diastolic
#---------------------------------------------------------------------------#
summary(indicators$AVG_PULSE_RANGE_START, na.rm = TRUE)
summary(indicators$AVG_PULSE_RANGE_END, na.rm = TRUE)

#---------------------------------------------------------------------------#
# Pulse
#---------------------------------------------------------------------------#
summary(indicators$AVG_PULSE_RANGE_START, na.rm = TRUE)
summary(indicators$AVG_PULSE_RANGE_END, na.rm = TRUE)


#---------------------------------------------------------------------------#
# CVP
#---------------------------------------------------------------------------#
cvp.box <- ggplot(indicators, aes(y=CVP_INT_RANGE_START)) + geom_boxplot()
min(indicators$CVP_INT_RANGE_START, na.rm = TRUE) #0
mean(indicators$CVP_INT_RANGE_START, na.rm = TRUE) #8.947908
max(indicators$CVP_INT_RANGE_START, na.rm = TRUE) #50

#CVP of 19 is unusually high, and will not go above 22
cvp.range <- indicators[which(indicators$CVP_INT_RANGE_START <= 19),]
nrow(cvp.range)/ (nrow(indicators) - nrow(indicators[which(is.na(indicators$CVP_INT_RANGE_START)),]))
#98.1% of the data is below 19

cvp.range <- indicators[which(indicators$CVP_INT_RANGE_START <= 22),]
nrow(cvp.range)/ (nrow(indicators) - nrow(indicators[which(is.na(indicators$CVP_INT_RANGE_START)),]))
# 99.3% of the data is below 22

#---------------------------------------------------------------------------#
# Body temperature
#---------------------------------------------------------------------------#


ggplot(indicators, aes(y=BODYTEMP_RANGE_START)) + geom_boxplot()
#high point at 400 - find and remove
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
max_temp <- which(indicators$BODYTEMP_RANGE_START == my.max(indicators$BODYTEMP_RANGE_START))
indicators[max_temp,] #missing a decimal point
indicators$BODYTEMP_RANGE_START[502883] <- 38.7

#convert all to C
ggplot(as.data.frame(indicators$BODYTEMP_RANGE_START), aes(x=indicators$BODYTEMP_RANGE_START)) + geom_histogram(na.rm = TRUE)

indicators <-mutate(indicators, BODYTEMP_RANGE_START = case_when(
  BODYTEMP_RANGE_START > 50 ~ (BODYTEMP_RANGE_START - 32) * (5/9),
  BODYTEMP_RANGE_START <= 50 ~ BODYTEMP_RANGE_START
))

ggplot(as.data.frame(indicators$BODYTEMP_RANGE_END), aes(x=indicators$BODYTEMP_RANGE_END)) + geom_histogram(na.rm = TRUE)


indicators <-mutate(indicators, BODYTEMP_RANGE_END = case_when(
  BODYTEMP_RANGE_END > 50 ~ (BODYTEMP_RANGE_END - 32) * (5/9),
  BODYTEMP_RANGE_END <= 50 ~ BODYTEMP_RANGE_END
))

indicators <- indicators[-c(19)]

#percent of temperature data above 38.5 or below 36
temp.range <- indicators[which(indicators$BODYTEMP_RANGE_START <= 38.5),]
temp.range <- indicators[which(indicators$BODYTEMP_RANGE_START >= 36),]
nrow(temp.range)/ (nrow(indicators) - nrow(indicators[which(is.na(indicators$BODYTEMP_RANGE_START)),]))
# 83.92%

#---------------------------------------------------------------------------#
# Remove unnecessary columns
#---------------------------------------------------------------------------#

#Separate brain death and time columns
#Temperature unit column - all celsius now

drops <- c("BRAIN_DEATH_DATE","BRAIN_DEATH_TIME","BODYTEMP_UNIT")
indicators <- indicators[ , !(names(indicators) %in% drops)]

#---------------------------------------------------------------------------#
# Write ranges to excel file
#---------------------------------------------------------------------------#
indicators.ranges <- 
  data.frame(
    name = "AVG_BP_DIAST",
    mean = mean(indicators$AVG_BP_DIAST, na.rm=TRUE),
    min = min(indicators$AVG_BP_DIAST,  na.rm=TRUE),
    max = max(indicators$AVG_BP_DIAST,  na.rm=TRUE),
    units = "mmHg"
  )
indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "HIGH_BP_DIAST", mean = mean(indicators$HIGH_BP_DIAST, na.rm=TRUE),
                    min = min(indicators$HIGH_BP_DIAST,  na.rm=TRUE),
                    max = max(indicators$HIGH_BP_DIAST,  na.rm=TRUE),
                    units = "mmHg"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "LOW_BP_DIAST", mean = mean(indicators$LOW_BP_DIAST, na.rm=TRUE),
                    min = min(indicators$LOW_BP_DIAST,  na.rm=TRUE),
                    max = max(indicators$LOW_BP_DIAST,  na.rm=TRUE),
                    units = "mmHg"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "AVG_BP_SYST", mean = mean(indicators$AVG_BP_SYST, na.rm=TRUE),
                    min = min(indicators$AVG_BP_SYST,  na.rm=TRUE),
                    max = max(indicators$AVG_BP_SYST,  na.rm=TRUE),
                    units = "mmHg"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "HIGH_BP_SYST", mean = mean(indicators$HIGH_BP_SYST, na.rm=TRUE),
                    min = min(indicators$HIGH_BP_SYST,  na.rm=TRUE),
                    max = max(indicators$HIGH_BP_SYST,  na.rm=TRUE),
                    units = "mmHg"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "LOW_BP_SYST", mean = mean(indicators$LOW_BP_SYST, na.rm=TRUE),
                    min = min(indicators$LOW_BP_SYST,  na.rm=TRUE),
                    max = max(indicators$LOW_BP_SYST,  na.rm=TRUE),
                    units = "mmHg"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "AVG_PULSE_RANGE_END", mean = mean(indicators$AVG_PULSE_RANGE_END, na.rm=TRUE),
                    min = min(indicators$AVG_PULSE_RANGE_END,  na.rm=TRUE),
                    max = max(indicators$AVG_PULSE_RANGE_END,  na.rm=TRUE),
                    units = "bpm"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "AVG_PULSE_RANGE_START", mean = mean(indicators$AVG_PULSE_RANGE_START, na.rm=TRUE),
                    min = min(indicators$AVG_PULSE_RANGE_START,  na.rm=TRUE),
                    max = max(indicators$AVG_PULSE_RANGE_START,  na.rm=TRUE),
                    units = "bpm"
         ))
indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "CVP_INT_RANGE_START", mean = mean(indicators$CVP_INT_RANGE_START, na.rm=TRUE),
                    min = min(indicators$CVP_INT_RANGE_START,  na.rm=TRUE),
                    max = max(indicators$CVP_INT_RANGE_START,  na.rm=TRUE),
                    units = "cmH2O"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "CVP_INT_RANGE_END", mean = mean(indicators$CVP_INT_RANGE_END, na.rm=TRUE),
                    min = min(indicators$CVP_INT_RANGE_END,  na.rm=TRUE),
                    max = max(indicators$CVP_INT_RANGE_END,  na.rm=TRUE),
                    units = "cmH2O"
         ))


indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "BODYTEMP_RANGE_START", mean = mean(indicators$BODYTEMP_RANGE_START, na.rm=TRUE),
                    min = min(indicators$BODYTEMP_RANGE_START,  na.rm=TRUE),
                    max = max(indicators$BODYTEMP_RANGE_START,  na.rm=TRUE),
                    units = "degrees Celsius"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "BODYTEMP_RANGE_END", mean = mean(indicators$BODYTEMP_RANGE_END, na.rm=TRUE),
                    min = min(indicators$BODYTEMP_RANGE_END,  na.rm=TRUE),
                    max = max(indicators$BODYTEMP_RANGE_END,  na.rm=TRUE),
                    units = "degrees Celsius"
         ))

indicators.ranges <-
  rbind( indicators.ranges,
         data.frame(name = "Duration", mean = mean(indicators$Duration, na.rm=TRUE),
                    min = min(indicators$Duration,  na.rm=TRUE),
                    max = max(indicators$Duration,  na.rm=TRUE),
                    units = "hours"
         ))

write.xlsx(indicators.ranges, file = "donor_ranges.xlsx", 
           sheetName="indicators", append=TRUE)
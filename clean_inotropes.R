data_dir = "C:/Users/Megan/Documents/Capstone/Data/Full Longitudinal Analysis"
original_dir = "C:/Users/Megan/Documents/Capstone/Data/Original"

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(ggplot2)

#---------------------------------------------------------------------------#
# Read in data
#---------------------------------------------------------------------------#
setwd(data_dir)

donor.data <- read_csv("Donor_Data.csv")
donor.info <- donor.data[,c("DONOR_ID","WGT_KG_DON_CALC")]

setwd(original_dir)

inotropic <- read_tsv("INOTROPIC.tsv")

inotropic <- merge(inotropic, donor.info, by="DONOR_ID")

#---------------------------------------------------------------------------#
# Recode agents
#---------------------------------------------------------------------------#

# mispelled, under different names, or different cases
#use table(inotropic$AGENT)[order(as.numeric(table(inotropic$AGENT)))] as necessary
inotropic$DOSEUNITS[which(inotropic$AGENT == "VASOPRESSIN, units/kg/hr")] <- "units/kg/hr"
inotropic$DOSEUNITS[which(inotropic$AGENT == "vasopressin-miliunits/hr")] <- "milliunits/hr"
inotropic$DOSEUNITS[which(inotropic$AGENT == "vasopressin (milliunits")] <- "milliunits/hr"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vas")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "va")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3))  == "pit")] <- "Vasopressin" #pitressin

#Dobutamine
inotropic$AGENT[which(toupper(inotropic$AGENT) == "DOBUTAMINE")] <- "Dobutamine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "dobutimine")] <- "Dobutamine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "dobutrex")] <- "Dobutamine" #Dubotrex

#Norepinephrine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 8)) == "levophed")] <- "Norepinephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "norep")] <- "Norepinephrine"
inotropic$AGENT[which(inotropic$AGENT == "Levo")] <- "Norepinephrine"

#ephinephrine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "epin")] <- "Epinephrine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "epi")] <- "Epinephrine"

# Milrinone
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "mil")] <- "Milrinone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "prim")] <- "Milrinone" #primacor

#---------------------------------------------------------------------------#
# Correct units of dosages for agents used in inotropic score
#---------------------------------------------------------------------------#

setwd(data_dir)
ranges <- read_csv("variable_ranges.csv")

inotropic.ranges <- ranges[which(ranges$variable == "Dopamine" | ranges$variable == "Dobutamine" | ranges$variable == "Epinephrine" | ranges$variable == "Milrinone" | ranges$variable == "Vasopressin" |ranges$variable == "Norepinephrine"), ]
inotropic.ranges <- inotropic.ranges %>%
  group_by(variable) %>%
  arrange(age_group) %>%
  slice_head()

#only need agents which are to be used in inotropic score
inotropic.score <- inotropic[which(inotropic$AGENT == "Dopamine" | inotropic$AGENT == "Dobutamine" | inotropic$AGENT == "Epinephrine" | inotropic$AGENT == "Milrinone" | inotropic$AGENT == "Vasopressin" |inotropic$AGENT == "Norepinephrine"), ]

inotropic.score$ORIGINAL_VAL <- inotropic.score$AGENT_VAL

#convert everything but vasopressin into mcg/kg/min
inotropic.score <-mutate(inotropic.score, AGENT_VAL = case_when(
  DOSEUNITS == "mcg/kg/min"  ~ AGENT_VAL,
  DOSEUNITS == "mcg/hr" ~ AGENT_VAL / 60 / WGT_KG_DON_CALC,
  DOSEUNITS == "mcg/min" ~ AGENT_VAL / WGT_KG_DON_CALC,
  DOSEUNITS == "mg/min" ~ 1000 * AGENT_VAL / WGT_KG_DON_CALC,
  DOSEUNITS == "units/hr" ~ AGENT_VAL
))

#Vasopressin can only be entered in as units/time
#convert to units/kg/min
#do not assume units are accurate -> check ranges and convert as necessary
inotropic.score <-mutate(inotropic.score, AGENT_VAL = case_when(
  DOSEUNITS == "units/hr" & AGENT == "Vasopressin" & AGENT_VAL <= .002 ~ AGENT_VAL, #already in units/kg/min
  DOSEUNITS == "units/hr" & AGENT == "Vasopressin" & AGENT_VAL <= 2 & AGENT_VAL > 0.3 ~ AGENT_VAL / 1000, #in milliunits/kg/min
  DOSEUNITS == "units/hr" & AGENT == "Vasopressin" ~ AGENT_VAL / WGT_KG_DON_CALC, #otherwise assume units/time (no weight involved)
  DOSEUNITS == "units/kg/hr" & AGENT == "Vasopressin" ~ AGENT_VAL / 60,
  DOSEUNITS == "milliunits/hr" & AGENT == "Vasopressin" ~ AGENT_VAL / 60 / 1000 / WGT_KG_DON_CALC,
  DOSEUNITS != "units/hr" & AGENT == "Vasopressin" ~ as.numeric(NA),
  TRUE ~ AGENT_VAL
))


inotropic.score <-mutate(inotropic.score, AGENT_VAL = case_when(
  AGENT == "Vasopressin" & AGENT_VAL <= .002 ~ AGENT_VAL, #already in units/kg/min
  AGENT == "Vasopressin" & AGENT_VAL <= .12 & AGENT_VAL >=.01  ~ AGENT_VAL / 60, #from units/kg/hour
  AGENT == "Vasopressin" & AGENT_VAL <= 2 & AGENT_VAL >= 0.3 ~ AGENT_VAL / 1000, #from milliunits/kg/min
  TRUE ~ AGENT_VAL, 

))

#unable to convert to mcg/min from units/hr disregard these entries
inotropic.score[which(inotropic.score$AGENT != "Vasopressin" & inotropic.score$DOSEUNITS == "units/hr"),c("AGENT_VAL"
)] <- as.numeric(NA)


#format date and time column
inotropic.score$DT <- as.POSIXct(inotropic.score$DT,
                                 format="%Y-%m-%dT%H:%M" #format time
)
inotropic.score$AGENT_END_DT <- as.POSIXct(inotropic.score$AGENT_END_DT,
                                 format="%Y-%m-%dT%H:%M" #format time
)

#---------------------------------------------------------------------------#
# Calculate VIS inotrope dosage for each agent at each measurement
#---------------------------------------------------------------------------#

# a is inotrope we want to calculate dosages for at each measurement in the data (in character form)
agent_column <- function(a) {
  temp <- inotropic.score
  
  agent.min <- inotropic.ranges[which(inotropic.ranges$variable == a),]$lower
  agent.max <- inotropic.ranges[which(inotropic.ranges$variable == a),]$upper
  
  temp <-
    temp %>%
    mutate(AGENT_VAL = case_when(
      (AGENT_VAL < agent.min | AGENT_VAL > agent.max) & AGENT == a ~ as.numeric(NA),
      TRUE ~ AGENT_VAL
    ))
  
  temp$agent <- rep(NA, nrow(temp)) #this column will have the amount of that inotrope being given to the patient at each measurement
  temp$agent.end.DT <- rep(NA, nrow(temp)) #this column will have the end time of the most recent dosage of this inotrope - to be compared to the DT of that measurement
  temp$agent.end.DT <- temp$AGENT_END_DT
  
  temp <-
    temp %>%
    group_by(DONOR_ID) %>%
    arrange(DT) %>%
    mutate(agent.end.DT = case_when(
      # code entries that have no end time to a date in the far future -> will be > any DT
      AGENT == a ~ if_else(is.na(agent.end.DT), as.POSIXct("3000-01-01 12:00:00"), agent.end.DT),
      AGENT != a ~ as.POSIXct(NA)
    ))
  
  temp <-
    temp %>%
    group_by(DONOR_ID) %>%
    arrange(DT) %>%
    fill(agent.end.DT)
    #those rows that do not have an end time for the specific inotrope will be given the same one as the row above it
    #this occurs because each AGENT_END_DT in the original data set only refers the new inotrope being given in the
    #that measurement - separating this into different columns by the inotrope and filling in empty entries
  
  temp <-
    temp %>%
    group_by(DONOR_ID) %>%
    arrange(DT) %>%
    mutate(agent.end.DT = if_else(agent.end.DT > DT, agent.end.DT, as.POSIXct(NA)))
    #set the inotrope's end time as NA if it already occurred with respect to the DT of the row's measurement
  
  temp <-
    temp %>%
    group_by(DONOR_ID) %>%
    arrange(DT) %>%
    mutate(agent = case_when(
      AGENT == a & !is.na(agent.end.DT) ~ AGENT_VAL, #if the row originally was setting a new dosage for this inotrope, set it's value to be that of AGENT_VAL
      is.na(agent.end.DT) ~ 0, #if the row was for encoding a different inotrope , and if the inotrope dosage has already ended by that measurement, set its value to 0
      TRUE ~ as.numeric(NA) #if the inotrope dosage has not yet ended, set to NA - will be filled in later
    ))
  
  temp <-
    temp %>%
    group_by(DONOR_ID) %>%
    arrange(DT) %>%
    fill(agent)
    #fill in the NAs with the dosage amount from the previous row for each donor
  
  return(temp)
}


inotropic.score <- agent_column("Dopamine")
inotropic.score$Dopamine <- inotropic.score$agent

inotropic.score <- agent_column("Dobutamine")
inotropic.score$Dobutamine <- inotropic.score$agent

inotropic.score <- agent_column("Epinephrine")
inotropic.score$Epinephrine <- inotropic.score$agent

inotropic.score <- agent_column("Milrinone")
inotropic.score$Milrinone <- inotropic.score$agent

inotropic.score <- agent_column("Vasopressin")
inotropic.score$Vasopressin <- inotropic.score$agent

inotropic.score <- agent_column("Norepinephrine")
inotropic.score$Norepinephrine <- inotropic.score$agent

# VIS= dopamine dose + dobutamine dose + 100 x epinephrine dose + 10 x milrinone dose + 10,000 x vasopressin dose + 100 x norepinephrine dose

inotropic.score$Score <- inotropic.score$Dopamine + inotropic.score$Dobutamine + 100*inotropic.score$Epinephrine + 10*inotropic.score$Milrinone + 10000*inotropic.score$Vasopressin + 100*inotropic.score$Norepinephrine

drops <- c("WGT_KG_DON_CALC","AGENT","AGENT_VAL","DOSEUNITS","ARCHIVED__DURATION","agent.end.DT","ORIGINAL_VAL","agent")
inotropic.score.final <- inotropic.score[ , !(names(inotropic.score) %in% drops)]

setwd(data_dir)
write.csv(inotropic.score.final,"inotropic.score_cleaned.csv", row.names=FALSE)

data_dir = "C:/Users/Megan/Documents/Capstone/Data"
setwd(data_dir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(tidyverse)
library(haven)
library(dplyr)
library(lubridate)

#---------------------------------------------------------------------------#
# Read in inotrope data
#---------------------------------------------------------------------------#

inotropic <- read.csv("INOTROPIC.tsv", sep = "\t")

#recode agents that are mispelled, under different names, or different cases
#use table(inotropic$AGENT)[order(as.numeric(table(inotropic$AGENT)))] as necessary

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vas")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vaa")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vao")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vso")] <- "Vasopressin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "argi")] <- "Vasopressin"

inotropic$AGENT[which(toupper(inotropic$AGENT) == "NEOSYNEPHRINE")] <- "Neosynephrine"
inotropic$AGENT[which(inotropic$AGENT == "Neosynephrine (Phenylephrine)")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "pheny")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "phene")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "pheni")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "neos")] <- "Neosynephrine"
inotropic$AGENT[which(toupper(inotropic$AGENT) == "NEO")] <- "Neosynephrine"
inotropic$AGENT[which(inotropic$AGENT == "Neo - Synephrine")] <- "Neosynephrine"
inotropic$AGENT[which(inotropic$AGENT == "Neo 40mcg-Levo 8mg/250cc")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "pheb")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "phel")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "phey")] <- "Neosynephrine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "phyn")] <- "Neosynephrine"


inotropic$AGENT[which(toupper(inotropic$AGENT) == "DOBUTAMINE")] <- "Dobutamine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "dobutimine")] <- "Dobutamine"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 8)) == "levophed")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "norep")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "nor-e")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "norp")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "nori")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "nor e")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "noepi")] <- "Levophed"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "nrepi")] <- "Levophed"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "pit")] <- "Pitressin"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "ins")] <- "Insulin"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "levoth")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "levoet")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "levopt")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "leveot")] <- "Levothyroxine"

inotropic$AGENT[which(inotropic$AGENT == "Levo")] <- "Levophed"
inotropic$AGENT[which(inotropic$AGENT == "levo 4-10 mcg/min")] <- "Levophed"
inotropic$AGENT[which(inotropic$AGENT == "Levohped (dose 0-46)")] <- "Levophed"
inotropic$AGENT[which(inotropic$AGENT == "Lvoephed")] <- "Levophed"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "cardiz")] <- "Cardizem"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "cardia")] <- "Cardizem"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 5)) == "carde")] <- "Cardene"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 7)) == "cardine")] <- "Cardene"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 8)) == "caredene")] <- "Cardene"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "nic")] <- "Cardene"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "nip")] <- "Nipride"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "esm")] <- "Esmolol"

#ephinephrine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "epi-")] <- "Epi-Cal"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "epi/")] <- "Epi-Cal"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "epi ca")] <- "Epi-Cal"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "epin")] <- "Epinephrine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "epi")] <- "Epinephrine"

#T4
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "t4")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "t-")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "t 4")] <- "Levothyroxine"

# Amiodarone
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "amio")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "amid")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "amir")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 11)) == "amirodarone")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 11)) == "aminodarone")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "amin")] <- "Aminophylline"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "amm")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "amn")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "amo")] <- "Amiodarone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "am,")] <- "Amiodarone"

# Milrinone
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "mil")] <- "Milrinone"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "mirlinone")] <- "Milrinone"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "ino")] <- "Dopamine"

# Propofol
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "prop")] <- "Propofol"

# Fentanyl
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "fent")] <- "Fentanyl"

#thyroxine -> levothyroxine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "thy")] <- "Levothyroxine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "lecothyroxine")] <- "Levothyroxine"

# Labetalol
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "lab")] <- "Labetalol"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "lebatolol")] <- "Labetalol"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "lebatalol")] <- "Labetalol"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "lebetolol")] <- "Labetalol"

# heparin
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "he")] <- "Heparin"

#nitroglycerin
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "nitrog")] <- "Nitroglycerin"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "nitro")] <- "Nitroglycerin"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "nitr0glycerine")] <- "Nitroglycerin"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "nitroclycerin")] <- "Nitroglycerin"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "ntiroglycerin")] <- "Nitroglycerin"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "ntg")] <- "Nitroglycerin"

#Nipride
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "nitrop")] <- "Nipride"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "nitrpr")] <- "Nipride"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "nirpir")] <- "Nipride"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 6)) == "niardi")] <- "Nipride"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "nir")] <- "Nipride"

#Nimbex
inotropic$AGENT[which(tolower(inotropic$AGENT) == "nimbex")] <- "Nimbex"

#lidocaine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "lid")] <- "Lidocaine"

# Isoproterenol (Isuprel)
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "is")] <- "Isoproterenol"

#primacor
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "prim")] <- "Milrinone"

#sodium bicarbonate
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 13)) == "sodium bicarb")] <- "Sodium Bicarbonate"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "na bicarb.")] <- "Sodium Bicarbonate"
inotropic$AGENT[which(inotropic$AGENT == "NAHCO3")] <- "Sodium Bicarbonate"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "nac")] <- "Cardene"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "na")] <- "Narcan"

#cordarone
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "co")] <- "Cordarone"

#clevidipine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "clev")] <- "Cleviprex"

#diprivan
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "dip")] <- "Propofol"

#cordarone -> Amiodarone
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "cor")] <- "Amiodarone"

#versed
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "ver")] <- "Versed"

#diltiazem -> cardizem
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "dilt")] <- "Cardizem"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "dil")] <- "Dilaudid"

#pentobarbital
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "pe")] <- "Pentobarbital"

#morphine
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "mor")] <- "Morphine"

#terbutaline
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "ter")] <- "Terbutaline"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "trid")] <- "Tridil"

inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "T$")] <- "Levothyroxine"

#lopressor (look at toprol and metaprol)
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "lop")] <- "Lopressor"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "meto")] <- "Lopressor"

#precedex
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "pre")] <- "Precedex"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "prot")] <- "Protonix"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 4)) == "proc")] <- "Procainamide"


#DDAVP
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "dd")] <- "DDVAP"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "des")] <- "DDVAP"

#angiotensin
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "ang")] <- "Angiotensin"

#vecuronium
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "vec")] <- "Vecuronium"

#T3
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 2)) == "t3")] <- "T3"

#Dubotrex
inotropic$AGENT[which(tolower(inotropic$AGENT) == "dobutrex")] <- "Dobutamine"

#novolin
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "nov")] <- "Novolin"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "hum")] <- "Novolin"

#furosemide
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "fur")] <- "Furosemide"
inotropic$AGENT[which(tolower(substr(inotropic$AGENT, 1, 3)) == "las")] <- "Furosemide"

inotropic$AGENT[which(tolower(inotropic$AGENT) == "levophed")] <- "Norepinephrine"
inotropic$AGENT[which(tolower(inotropic$AGENT) == "pitressin")] <- "Vasopressin"



#---------------------------------------------------------------------------#
# STAR Data
# See STAR File Documentation.xls for details
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


#---------------------------------------------------------------------------#
# Add weight in kg and time of brain death to inotropic dataset
#---------------------------------------------------------------------------#
data = load_data("deceased_donor_data")
donor.info <- data[,c("DONOR_ID","WGT_KG_DON_CALC","BRAIN_DEATH_DATE","BRAIN_DEATH_TIME")]

#INOTROPIC SCORE includes dopamine, dobutamine, epinephrine, milrinone, vasopressin, and norepinephrine 
inotropic.score <- inotropic[which(inotropic$AGENT == "Dopamine" | inotropic$AGENT == "Dobutamine" | inotropic$AGENT == "Epinephrine" | inotropic$AGENT == "Milrinone" | inotropic$AGENT == "Vasopressin" |inotropic$AGENT == "Norepinephrine"), ]
nrow(inotropic.score)/nrow(inotropic) #86.12% of the data is represented by these dosages

#combine datasets using DONOR_ID
inotropic.score <- merge(inotropic.score, donor.info, by="DONOR_ID")


#---------------------------------------------------------------------------#
# Adjust DOSEUNITS to fit inotropic score
#---------------------------------------------------------------------------#

#convert everything but vasopressin into mcg/kg/hr
inotropic.score <-mutate(inotropic.score, AGENT_VAL = case_when(
  DOSEUNITS == "mcg/kg/min" && AGENT != "Vasopressin" ~ AGENT_VAL,
  DOSEUNITS == "mcg/hr" && AGENT != "Vasopressin" ~ AGENT_VAL / 60 / WGT_KG_DON_CALC,
  DOSEUNITS == "mcg/min" && AGENT != "Vasopressin" ~ AGENT_VAL / WGT_KG_DON_CALC,
  DOSEUNITS == "mg/min" && AGENT != "Vasopressin" ~ 1000 * AGENT_VAL / WGT_KG_DON_CALC
))

#unable to convert from units/hr without concentration -> disregard these entries
inotropic.score <-mutate(inotropic.score, AGENT_VAL = case_when(
    AGENT == "Vasopressin" & 
    DOSEUNITS != "units/hr" | AGENT != "Vasopressin" & 
    DOSEUNITS == "units/hr" ~ NA
))


#---------------------------------------------------------------------------#
# Recode time entries to duration from first test for a patient
#---------------------------------------------------------------------------#

#format date and time column
inotropic.score$DT <- as.POSIXct(inotropic.score$DT,
           format="%Y-%m-%dT%H:%M" #format time
)

#fix misentries
inotropic.score[which(inotropic.score$DONOR_ID == 331602)[1], c("DT")] <- as.POSIXct("2009-05-01 13:00:00", format="%Y-%m-%d %H:%M:%OS")
inotropic.score[which(inotropic.score$DONOR_ID == 331602)[3], c("DT")] <- as.POSIXct("2009-11-17 10:00:00", format="%Y-%m-%d %H:%M:%OS")
inotropic.score[which(inotropic.score$DONOR_ID == 587121)[1],] <- NA

#Add duration column and calculate for each unique donor
inotropic.score$Duration <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Duration = DT - first(DT))
inotropic.score[is.na(inotropic.score$Duration),c("Duration")] <- as.duration(0)
inotropic.score[which(inotropic.score$Duration < 0),c("Duration")] <- as.duration(0)

#convert duration from seconds to hours
inotropic.score$Duration <- inotropic.score$Duration / 3600
inotropic.score$Duration <- as.numeric(inotropic.score$Duration)

#---------------------------------------------------------------------------#
# Recode each entry to have dosage amounts for each inotrope
#---------------------------------------------------------------------------#

#Dopamine
# if the agent added in that entry is Dopamine, use that value
# otherwise, use value of Dopamine from previous row for that patient
# fill NA rows with 0 (first entry)

inotropic.score$Dopamine <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Dopamine = ifelse(AGENT == "Dopamine", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Dopamine)

inotropic.score[is.na(inotropic.score$Dopamine),c("Dopamine")] <- 0

#Repeat for other inotropes

#Dobutamine
inotropic.score$Dobutamine <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Dobutamine = ifelse(AGENT == "Dobutamine", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Dobutamine)

inotropic.score[is.na(inotropic.score$Dobutamine),c("Dobutamine")] <- 0

#Epinephrine
inotropic.score$Epinephrine <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Epinephrine = ifelse(AGENT == "Epinephrine", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Epinephrine)

inotropic.score[is.na(inotropic.score$Epinephrine),c("Epinephrine")] <- 0

#Milrinone
inotropic.score$Milrinone <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Milrinone = ifelse(AGENT == "Milrinone", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Milrinone)

inotropic.score[is.na(inotropic.score$Milrinone),c("Milrinone")] <- 0

#Vasopressin
inotropic.score$Vasopressin <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Vasopressin = ifelse(AGENT == "Vasopressin", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Vasopressin)

inotropic.score[is.na(inotropic.score$Vasopressin),c("Vasopressin")] <- 0

#Norepinephrine
inotropic.score$Norepinephrine <- rep(NA, nrow(inotropic.score))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  mutate(Norepinephrine = ifelse(AGENT == "Norepinephrine", AGENT_VAL, NA))

inotropic.score <-
  inotropic.score %>%
  group_by(DONOR_ID) %>%
  fill(Norepinephrine)

inotropic.score[is.na(inotropic.score$Norepinephrine),c("Norepinephrine")] <- 0

# VIS= dopamine dose + dobutamine dose + 100 x epinephrine dose + 10 x milrinone dose + 10,000 x vasopressin dose + 100 x norepinephrine dose

inotropic.score$Score <- inotropic.score$Dopamine + inotropic.score$Dobutamine + 100*inotropic.score$Epinephrine + 10*inotropic.score$Milrinone + 10000*inotropic.score$Vasopressin + 100*inotropic.score$Norepinephrine


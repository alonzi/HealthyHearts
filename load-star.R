#loading Star Data - Based off of Prof Porter's load-dnet.R file

#-- Set path to DonorNet data
data_dir = "C:/Users/student/Documents/UVA/4thYear/Capstone/Data"

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(tidyverse)
library(haven)
library(readxl)

zipdir = file.path(data_dir, "STAR-Data-Using.zip")

unzip(zipdir, list=TRUE) %>% 
  filter(str_detect(Name, "\\.sas7bdat")) %>% pull(Name)


#-- Function to read in DonorNet data
# fname: name of file (with .sas7bdat extension)
# vars: vector of column names to keep (use NULL to keep all)
# keep_ids: vector of DONOR_ID values to keep (use NULL to keep all)
# .zipdir: directory of zipped data
load_data <- function(fname, vars=NULL, keep_ids=NULL, usewl = FALSE, 
                      .zipdir=zipdir) {
  #- load data + select columns
  if(!is.null(vars)){
    tmp = read_sas(unzip(.zipdir, files=str_c(fname, ".sas7bdat"))) %>% 
      select(!!vars)
  }
  else{
    tmp = read_sas(unzip(.zipdir, files=str_c(fname, ".sas7bdat")))
  }
  #- replace "" with explicit NA
  tmp = tmp %>% mutate_if(is.character, zap_empty)
  #- keep interesting DONOR_ID's 
  if(!is.null(keep_ids)) {
    tmp = tmp %>% filter(DONOR_ID %in% !!keep_ids) 
  }  
  #- find non-empty rows
  if(usewl == FALSE){
    ok = tmp %>% select(-DONOR_ID) %>% 
      apply(1, function(x) !all(is.na(x))) %>% which()
  }
  else{
    ok = tmp %>% select(-WL_ID_CODE) %>% 
      apply(1, function(x) !all(is.na(x))) %>% which()
  }
  
  #- return data
  tmp %>% slice(ok)
}

#---------------------------------------------------------------------------#
# deceased_donor_data
#---------------------------------------------------------------------------#
data = load_data("deceased_donor_data")

data1 = data[-which(substr(names(data), 1, 8) == "CONSENT_")]
data1 = data1[-which(substr(names(data1), 1, 8) == "COVID19_")]
data1 = data1[-which(substr(names(data1), 1, 3) == "IN_")]
data1 = data1[-which(substr(names(data1), 1, 4) == "KIB_")]
data1 = data1[-which(substr(names(data1), 1, 4) == "KIL_")]
data1 = data1[-which(substr(names(data1), 1, 4) == "KIR_")]
data1 = data1[-which(substr(names(data1), 1, 13) == "LEFT_KI_PUMP_")]
data1 = data1[-which(substr(names(data1), 1, 16) == "LENGTH_LEFT_LUNG")]
data1 = data1[-which(substr(names(data1), 1, 17) == "LENGTH_RIGHT_LUNG")]
data1 = data1[-which(substr(names(data1), 1, 3) == "LI_")]
data1 = data1[-which(substr(names(data1), 1, 5) == "LIS1_")]
data1 = data1[-which(substr(names(data1), 1, 5) == "LIS2_")]
data1 = data1[-which(substr(names(data1), 1, 6) == "LT_KI_")]
data1 = data1[-which(names(data1) == "LT_LUNG_MACHINE_PERFUSION")]
data1 = data1[-which(substr(names(data1), 1, 4) == "LUB_")]
data1 = data1[-which(substr(names(data1), 1, 4) == "LUL_")]
data1 = data1[-which(substr(names(data1), 1, 4) == "LUR_")]
data1 = data1[-which(substr(names(data1), 1, 6) == "NUM_IN")]
data1 = data1[-which(substr(names(data1), 1, 6) == "NUM_KI")]
data1 = data1[-which(substr(names(data1), 1, 6) == "NUM_LI")]
data1 = data1[-which(substr(names(data1), 1, 6) == "NUM_LU")]

data1 = data1[-which(substr(names(data1), 1, 7) == "NUM_ORG")]
data1 = data1[-which(substr(names(data1), 1, 6) == "NUM_PA")]
data1 = data1[-which(substr(names(data1), 1, 3) == "PA_")]
data1 = data1[-which(substr(names(data1), 1, 5) == "PAS1_")]
data1 = data1[-which(substr(names(data1), 1, 5) == "PAS2_")]
data1 = data1[-which(substr(names(data1), 1, 8) == "RIGHT_KI")]
data1 = data1[-which(substr(names(data1), 1, 5) == "RT_KI")]
data1 = data1[-which(names(data1) == "RT_LUNG_MACHINE_PERFUSION")]

names(data1)
write_tsv(data1, "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv/deceased_donor_data.tsv")

#---------------------------------------------------------------------------#
# thoracic_stat1A
#---------------------------------------------------------------------------#
data = load_data("thoracic_stat1a", usewl = TRUE)

write_tsv(data, "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv/thoracic_stat1a.tsv")

#---------------------------------------------------------------------------#
# thoracic_stat1B
#---------------------------------------------------------------------------#
data = load_data("thoracic_stat1b", usewl = TRUE)

write_tsv(data, "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv/thoracic_stat1b.tsv")



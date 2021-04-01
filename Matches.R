data_dir = "~/4th year/STS Capstone/donorData/full_join"
setwd(data_dir)
library("readxl")
library(ggplot2)
library(tidyverse)

echo = read_csv("echo_w_analysis.csv")
donor_data = read_csv("Donor_Data.csv")

crosswalk = read.csv("full_crosswalk.csv")

echo_don_id <-echo$`Donor ID`
crosswalk_don_id <- crosswalk$DON_ID
length(intersect(echo_don_id, crosswalk_don_id))

donor_donor_id <- donor_data$DONOR_ID
crosswalk_donor_id <- crosswalk$DONOR_ID
length(intersect(donor_donor_id, crosswalk_donor_id))



length(which(echo_don_id %in% crosswalk_don_id == FALSE))
#127
length(which(donor_donor_id %in% crosswalk_donor_id == FALSE))
#0

duplicated(donor_donor_id)
donor_cross2 = full_join(donor_data,donor_data, by=c("DONOR_ID", "Accepted"))

donor_don <-donor_cross$DON_ID

length(which(echo_don_id %in% donor_don == FALSE))


data_dir = "~/4th year/STS Capstone/"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)

#used after sorting echo to top of data
#may have parsing issues
data = read_csv("full7100wEcho.csv")
data$DT = as.POSIXct(data$DT, format = "%m/%d/%Y %H:%M" )
data$BrainDeath = as.POSIXct(data$BrainDeath, format = "%m/%d/%Y %H:%M" )


#up to here is how to read stuff 

#Add duration column and calculate for each unique donor
data$Duration = rep(NA, nrow(data))
data$Duration = data$DT - data$BrainDeath
#data = data %>% group_by(DONOR_ID) %>% mutate(Duration = DT - BrainDeath)
#convert duration from seconds to hours
data$Duration = data$Duration / 3600
data$Duration = as.numeric(data$Duration)

just_echo_matches <- data[which(data$`Repeat Instrument` == "ECHO"),]

#write to csv file
write.csv(data, "~/4th year/STS Capstone/full7100wEchoduration.csv", row.names = FALSE)
write.csv(just_echo_matches, "~/4th year/STS Capstone/EchoMatchesw7100.csv", row.names = FALSE)



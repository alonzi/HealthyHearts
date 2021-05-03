#this file calculates duration in hours between brain death and the time the data was taken
#from the file read in in line 12 and writes it to a csv file

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)

#Deceased Donor data
data = read_excel("full7100wEcho.xlsx")

#Add duration column and calculate for each unique donor
data$Duration = rep(NA, nrow(data))
data = data %>% group_by(DONOR_ID) %>% mutate(Duration = DT - BrainDeath)

#convert duration from seconds to hours
data$Duration = data$Duration / 3600
data$Duration = as.numeric(data$Duration)

#write to csv file
write.csv(data,
          "C:/Users/student/Documents/Healthy Hearts/full7100wEchoduration.csv", 
          row.names = FALSE)

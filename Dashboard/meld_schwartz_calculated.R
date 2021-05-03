#this file calculates the MELD score and Schwartz score for each row of data in the 
#file read in in line 12 and writes it to a new csv file

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)

#full7100wEchoduration data
data = read_excel("full7100wEchodurationheight.xlsx", na="NA")

#Change all Creatinine scores less than 0.2 to 0.2
for (i in 1:nrow(data)){
  if (!is.na(data$CREATININE[i])){
    if (data$CREATININE[i] <= 0.2){
      data$CREATININE[i] = 0.2
    }
  }
}

#Add MELD score column and calculate for each row
data$MELD = rep(NA, nrow(data))
data$MELD = 3.78 * log(data$BILIRUBIN) + 11.2 * log(data$INR) + 9.57 * 
  log(data$CREATININE) + 6.43

#Add Schwartz formula column and calculate for each row
data$SCHWARTZ = rep(NA, nrow(data))
data$SCHWARTZ = 0.413 * data$Height / (data$CREATININE)

#write to csv file
write.csv(data,
          "C:/Users/student/Documents/Healthy Hearts/full7100withall.csv", 
          row.names = FALSE)

# ok so I need to build a function for imputation
# we have to make sure duration is an input for the prediction of future echos so if there
# is an echo at the time 10 we should be able to say what it is at time 15
# build up start small and simple and then make it more complex

# create another df to account for incomplete cases at the duration of each age group 
# to fill in the blanks

library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(ggpubr)
library(MASS)
library(lubridate)
library(lindia)
library(mice)
library(imputeTS)

# read in the data
source_dir = "C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone"
data_dir = "C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone"
setwd(data_dir)
data <- read.csv("EchoMatchesw7100.csv")
ranges <- read_csv("variable_ranges.csv")

# clean data using function
setwd(source_dir)
source("SourceCodeHearts.R")
data.clean <- echo_clean(data)
data.echo <- echo_sort(data.clean)

#***********************************************************
#
#		Data Imputation
#
#***********************************************************


nas.missing <- var_missing(data.echo)
# ok so a lot of data is missing, it may be better to start by age group and then build up
nas.missing.byage <- var_missing_AG(data.echo)

var_drop <- rm_var95(data.echo) #list of variables who have more than 95% of data missing
data.echo.95 <- data.echo[!names(data.echo) %in% var_drop] #df of only variables with less than 95% of data missing
data.cat <- data.echo[,c(45:53)] #only categorical data to do imputation and bind later
data.num <- data.echo[-c(45:53)] #only numerical data to do imputation and bind later
data.num.95 <- data.num[!names(data.num) %in% var_drop]

#a significant number of variables have 80-90% of the data missing so i will ignore them
# for one imputation and not ignore them for another

#### No age group imputation ####
echo.random <- na_random(data.echo) #imputing all data
echo.95.random <- na_random(data.echo.95) #imputing data of all age groups with var <95% missing

echo.num.mean <- na_mean(data.num, option = "mean") #imputing using mean for all data
echo.num.median <- na_mean(data.num, option = "median") #imputing median for all data
echo.mean <- cbind(echo.num.mean,data.cat) #bind back to categorical
echo.median <- cbind(echo.num.median,data.cat) #bind back to categorical

echo.num.95.mean <- na_mean(data.num.95, option = "mean") #imputing using mean for all data
echo.num.95.med <- na_mean(data.num.95, option = "median") #imputing median for all data
echo.95.mean <- cbind(echo.num.95.mean,data.cat) #bind back to categorical
echo.95.median <- cbind(echo.num.95.median,data.cat) #bind back to categorical

#### Imputation by age group ####

#start the data imputation, I will do random, mean, and then investigate other methods
# break it down by age group to reduce bias
echo.0to6Months <- data.echo[which(data.echo$age.group == "0-6 months"),]
echo.6to12Months <- data.echo[which(data.echo$age.group == "6-12 months"),]
echo.1to3Years <- data.echo[which(data.echo$age.group == "1-3 years"),]
#echo.3to6Years <- data.echo[which(data.echo$age.group == "3-6 years"),] no data
echo.6to12Years <- data.echo[which(data.echo$age.group == "6-12 years"),]
echo.12plusYears <- data.echo[which(data.echo$age.group == "12+ years"),]

# 0-6 Months
echo.0to6Months.random <- na.random(echo.0to6Months) # Random Imputation
echo.0to6Months.95 <- echo.0to6Months[!names(echo.0to6Months) %in% var_drop]
echo.0to6Months.95.rand <- na_random(echo.0to6Months.95) # Random Imputation

echo.0to6Months.mean <- na.mean(echo.0to6Months, option = "mean") # Mean Imputation
echo.0to6Months.med <- na.mean(echo.0to6Months, option = "median") # Median Imputation

echo.0to6Months.95.mean <- na.mean(echo.0to6Months.95, option = "mean") # Mean Imputation for <95%
echo.0to6Months.95.med <- na.mean(echo.0to6Months.95, option = "median") # Median Imputation for <95%

# 6-12 Months
echo.6to12Months.random <- na.random(echo.6to12Months) # Random Imputation
echo.6to12Months.95 <- echo.6to12Months[!names(echo.6to12Months) %in% var_drop]
echo.6to12Months.95.rand <- na_random(echo.6to12Months.95) # Random Imputation

echo.6to12Months.mean <- na.mean(echo.6to12Months, option = "mean") # Mean Imputation
echo.6to12Months.med <- na.mean(echo.6to12Months, option = "median") # Median Imputation

echo.6to12Months.95.mean <- na.mean(echo.6to12Months.95, option = "mean") # Mean Imputation for <95%
echo.6to12Months.95.med <- na.mean(echo.6to12Months.95, option = "median") # Median Imputation for <95%

# 1-3 Years
echo.1to3Years.random <- na.random(echo.1to3Years) # Random Imputation
echo.1to3Years.95 <- echo.1to3Years[!names(echo.1to3Years) %in% var_drop]
echo.1to3Years.95.rand <- na_random(echo.1to3Years.95) # Random Imputation

echo.1to3Years.mean <- na.mean(echo.1to3Years, option = "mean") # Mean Imputation
echo.1to3Years.med <- na.mean(echo.1to3Years, option = "median") # Median Imputation

echo.1to3Years.95.mean <- na.mean(echo.1to3Years.95, option = "mean") # Mean Imputation for <95%
echo.1to3Years.95.med <- na.mean(echo.1to3Years.95, option = "median") # Median Imputation for <95%

# 6-12 Years
echo.6to12Years.random <- na.random(echo.6to12Years) # Random Imputation
echo.6to12Years.95 <- echo.6to12Years[!names(echo.6to12Years) %in% var_drop]
echo.6to12Years.95.rand <- na_random(echo.6to12Years.95) # Random Imputation

echo.6to12Years.mean <- na.mean(echo.6to12Years, option = "mean") # Mean Imputation
echo.6to12Years.med <- na.mean(echo.6to12Years, option = "median") # Median Imputation

echo.6to12Years.95.mean <- na.mean(echo.6to12Years.95, option = "mean") # Mean Imputation for <95%
echo.6to12Years.95.med <- na.mean(echo.6to12Years.95, option = "median") # Median Imputation for <95%

# 12 plus Years
echo.12plusYears.random <- na.random(echo.12plusYears) # Random Imputation
echo.12plusYears.95 <- echo.12plusYears[!names(echo.12plusYears) %in% var_drop]
echo.12plusYears.95.rand <- na_random(echo.12plusYears.95) # Random Imputation

echo.12plusYears.mean <- na.mean(echo.12plusYears, option = "mean") # Mean Imputation
echo.12plusYears.med <- na.mean(echo.12plusYears, option = "median") # Median Imputation

echo.12plusYears.95.mean <- na.mean(echo.12plusYears.95, option = "mean") # Mean Imputation for <95%
echo.12plusYears.95.med <- na.mean(echo.12plusYears.95, option = "median") # Median Imputation for <95%

#bind the age groups back together
echo.age.random <- rbind(echo.0to6Months.random,echo.6to12Months.random,echo.1to3Years.random,echo.6to12Years.random,echo.12plusYears.random)
echo.95.age.random <- rbind(echo.0to6Months.95.rand,echo.6to12Months.95.rand,echo.1to3Years.95.rand,echo.6to12Years.95.rand,echo.12plusYears.95.rand)
echo.age.mean <- rbind(echo.0to6Months.mean,echo.6to12Months.mean,echo.1to3Years.mean,echo.6to12Years.mean,echo.12plusYears.mean)
echo.95.age.mean <- rbind(echo.0to6Months.95.mean,echo.6to12Months.95.mean,echo.1to3Years.95.mean,echo.6to12Years.95.mean,echo.12plusYears.95.mean)
echo.age.med <- rbind(echo.0to6Months.med,echo.6to12Months.med,echo.1to3Years.med,echo.6to12Years.med,echo.12plusYears.med)
echo.95.age.med <- rbind(echo.0to6Months.95.med,echo.6to12Months.95.med,echo.1to3Years.95.med,echo.6to12Years.95.med,echo.12plusYears.95.med)

#echo.median
#echo.95.median

# ok I am going to do a quick summary on the different variables to check for variability
# mean and random both do not work in terms of variable ranges so the median is the best estimate





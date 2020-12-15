datadir <- "~/4th year/STS Capstone/"
setwd(datadir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(ggpubr)
library(xlsx)

echo_data <- read_csv("PRODSOMRUNOS-WholeTamale_DATA_LABELS_2020-12-10_1426.csv")

#add index column to help with sorting
echo_data$index <- which(echo_data$`Donor ID` == echo_data$`Donor ID`)

#Check for qualitative issues
qualitative_rejects <- which((echo_data$`Global Left Ventricular Dysfunction` != "Normal - 0") | (echo_data$`Global Right Ventricular Dysfunction` != "Normal - 0")
       | echo_data$`Focal Left Ventricular Free Wall Dysfunction` != "Normal - 0" | echo_data$`Focal Right Ventricular Free Wall Dysfunction`
       != "Normal - 0" | echo_data$`Focal Interventricular Septal Dysfunction` != "Normal - 0")

echo_data_qualitative_accept <- echo_data[-c(qualitative_rejects),]

#Check for numerical rejects


#Find Ejection Fraction rejections (as they are more important than shortening)
#Remove Biplane rejections (tier 1)

echo_data2 <- echo_data[-c(which(echo_data$`Biplane Ejection Fraction %` < 50)),]

#Remove 4 Chamber rejections
echo_data3 <- echo_data2[-c(which(echo_data2$`4 Chamber Ejection Fraction %`< 50 & is.na(echo_data2$`Biplane Ejection Fraction %`))),]

#Remove Qualitative rejections
echo_data_ejection <- echo_data3[-c(which(echo_data3$`Qualitative Ejection Fraction%` < 50 & is.na(echo_data3$`Biplane Ejection Fraction %` & is.na(echo_data3$`4 Chamber Ejection Fraction %`)))),]

#Move on to shortening fraction
echo_data_quantitative_accept <- echo_data_ejection[-c(which(echo_data_ejection$`Shortening Fraction %` < 27)),]

subjective_index <-echo_data_qualitative_accept$index
objective_index <- echo_data_quantitative_accept$index

subjective_index
objective_index

totalacceptance_indexes <- intersect(subjective_index,objective_index) #both accepted
qualitative_exclusive <- setdiff(subjective_index,objective_index) #values that are only accepted by qualitative values
quantitative_exclusive <-setdiff(objective_index, subjective_index) #values that are only accepted by quantitative values

possible_acceptances <- c(totalacceptance_indexes,qualitative_exclusive,quantitative_exclusive)
totalabnormal <- setdiff(echo_data$index, possible_acceptances)

length(qualitative_exclusive)
length(quantitative_exclusive)

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

echo_data <- read_csv("PRODSOMRUNOS_DATA_LABELS_2021-02-15_0902.csv")

#add index column to help with sorting
echo_data$index <- which(echo_data$`Donor ID` == echo_data$`Donor ID`)
echo_data$Qualitative_Status <- NA
echo_data$Quantitative_Status <- NA

#Old analysis

# #Check for qualitative issues
# qualitative_rejects <- which((echo_data$`Global Left Ventricular Dysfunction` != "Normal - 0") | (echo_data$`Global Right Ventricular Dysfunction` != "Normal - 0")
#        | echo_data$`Focal Left Ventricular Free Wall Dysfunction` != "Normal - 0" | echo_data$`Focal Right Ventricular Free Wall Dysfunction`
#        != "Normal - 0" | echo_data$`Focal Interventricular Septal Dysfunction` != "Normal - 0")
# 
# echo_data_qualitative_accept <- echo_data[-c(qualitative_rejects),]
# 
# #Check for numerical rejects
# 
# 
# #Find Ejection Fraction rejections (as they are more important than shortening)
# #Remove Biplane rejections (tier 1)
# 
# echo_data2 <- echo_data[-c(which(echo_data$`Biplane Ejection Fraction %` < 55)),]
# 
# #Remove 4 Chamber rejections
# echo_data3 <- echo_data2[-c(which(echo_data2$`4 Chamber Ejection Fraction %`< 55 & is.na(echo_data2$`Biplane Ejection Fraction %`))),]
# 
# #Remove Qualitative rejections
# echo_data_ejection <- echo_data3[-c(which(echo_data3$`Qualitative Ejection Fraction%` < 55 & is.na(echo_data3$`Biplane Ejection Fraction %`) & is.na(echo_data3$`4 Chamber Ejection Fraction %`))),]
# 
# #Move on to shortening fraction
# echo_data_quantitative_accept <- echo_data_ejection[-c(which(echo_data_ejection$`Shortening Fraction %` < 28 & is.na(echo_data_ejection$`Qualitative Ejection Fraction%`) & is.na(echo_data_ejection$`Biplane Ejection Fraction %`) & is.na(echo_data_ejection$`4 Chamber Ejection Fraction %`))),]
# 
# subjective_index <-echo_data_qualitative_accept$index
# objective_index <- echo_data_quantitative_accept$index
# 
# subjective_index
# objective_index
# 
# totalacceptance_indexes <- intersect(subjective_index,objective_index) #both accepted
# qualitative_exclusive <- setdiff(subjective_index,objective_index) #values that are only accepted by qualitative values
# quantitative_exclusive <-setdiff(objective_index, subjective_index) #values that are only accepted by quantitative values
# 
# possible_acceptances <- c(totalacceptance_indexes,qualitative_exclusive,quantitative_exclusive)
# totalabnormal <- setdiff(echo_data$index, possible_acceptances)
# 
# length(qualitative_exclusive)
# length(quantitative_exclusive)
# 


#New analysis 


qualitative_modsev <- which((echo_data$`Global Left Ventricular Dysfunction` == ("Moderate - 2")) | 
                              (echo_data$`Global Left Ventricular Dysfunction` == ("Severe - 3")) | 
                              (echo_data$`Global Right Ventricular Dysfunction` == "Moderate - 2")|
                              (echo_data$`Global Right Ventricular Dysfunction` == "Severe - 3")
                             | echo_data$`Focal Left Ventricular Free Wall Dysfunction` == "Moderate - 2" | 
                              echo_data$`Focal Left Ventricular Free Wall Dysfunction` == "Severe - 3" |
                              echo_data$`Focal Right Ventricular Free Wall Dysfunction`== "Moderate - 2" |
                              echo_data$`Focal Right Ventricular Free Wall Dysfunction`== "Severe" | 
                              echo_data$`Focal Interventricular Septal Dysfunction` == "Moderate - 2" | 
                              echo_data$`Focal Interventricular Septal Dysfunction` == "Severe - 3")

echo_data_sans_qualmodsev <- echo_data[-c(qualitative_modsev),]


qualitative_mild <- which(( echo_data_sans_qualmodsev$`Global Left Ventricular Dysfunction` == ("Mild - 1")) | 
                            (echo_data_sans_qualmodsev$`Global Right Ventricular Dysfunction` == ("Mild - 1")) | 
                            (echo_data_sans_qualmodsev$`Focal Left Ventricular Free Wall Dysfunction` == "Mild - 1")|
                            (echo_data_sans_qualmodsev$`Focal Right Ventricular Free Wall Dysfunction` == "Mild - 1")
                          | echo_data_sans_qualmodsev$`Focal Interventricular Septal Dysfunction` == "Mild - 1" )

echo_data_qualmild <- echo_data[c(qualitative_mild),]
echo_data_qualmodsev <- echo_data[c(qualitative_modsev),]
echo_data_qualnormal <- echo_data_sans_qualmodsev[-c(qualitative_mild),]

echo_data_qualmild$Qualitative_Status <- 1
echo_data_qualmodsev$Qualitative_Status <- 2
echo_data_qualnormal$Qualitative_Status <- 0

#rebuild the set after coding new values
temp1 <- rbind(echo_data_qualmild, echo_data_qualmodsev)
new_echo <- rbind(temp1,echo_data_qualnormal)



modsev_echo_data2 <- new_echo[-c(which(new_echo$`Biplane Ejection Fraction %` < 45)),]
biplane_modsev <- new_echo[c(which(new_echo$`Biplane Ejection Fraction %` < 45)),]
biplane_modsev$Quantitative_Status <- 2

#Remove 4 Chamber rejections
modsev_echo_data3 <- modsev_echo_data2[-c(which(modsev_echo_data2$`4 Chamber Ejection Fraction %` < 45 & is.na(modsev_echo_data2$`Biplane Ejection Fraction %`))),]
fourchamb_modsev <- modsev_echo_data2[c(which(modsev_echo_data2$`4 Chamber Ejection Fraction %` < 45 & is.na(modsev_echo_data2$`Biplane Ejection Fraction %`))),]
fourchamb_modsev$Quantitative_Status <- 2

#Remove Qualitative rejections
modsev_echo_data4 <- modsev_echo_data3[-c(which(modsev_echo_data3$`Qualitative Ejection Fraction%` < 45 & is.na(modsev_echo_data3$`Biplane Ejection Fraction %`) & is.na(modsev_echo_data3$`4 Chamber Ejection Fraction %`))),]
qual_modsev <- modsev_echo_data3[c(which(modsev_echo_data3$`Qualitative Ejection Fraction%` < 45 & is.na(modsev_echo_data3$`Biplane Ejection Fraction %`) & is.na(modsev_echo_data3$`4 Chamber Ejection Fraction %`))),]
qual_modsev$Quantitative_Status <- 2

#Move on to shortening fraction
modsev_echo_data5 <- modsev_echo_data4[-c(which(modsev_echo_data4$`Shortening Fraction %` < 23 & is.na(modsev_echo_data4$`Qualitative Ejection Fraction%`) & is.na(modsev_echo_data4$`Biplane Ejection Fraction %`) & is.na(modsev_echo_data4$`4 Chamber Ejection Fraction %`))),]
short_modsev <- modsev_echo_data4[c(which(modsev_echo_data4$`Shortening Fraction %` < 23 & is.na(modsev_echo_data4$`Qualitative Ejection Fraction%`) & is.na(modsev_echo_data4$`Biplane Ejection Fraction %`) & is.na(modsev_echo_data4$`4 Chamber Ejection Fraction %`))),]
short_modsev$Quantitative_Status <- 2


#Now check for mild quantitative(45 to 55/23-28)

mild_echo_data_1 <- modsev_echo_data5[-c(which(modsev_echo_data5$`Biplane Ejection Fraction %` < 55)),]
biplane_mild <- modsev_echo_data5[c(which(modsev_echo_data5$`Biplane Ejection Fraction %` < 55)),]
biplane_mild$Quantitative_Status <- 1

#Remove 4 Chamber rejections
mild_echo_data_2 <- mild_echo_data_1[-c(which(mild_echo_data_1$`4 Chamber Ejection Fraction %` < 55 & is.na(mild_echo_data_1$`Biplane Ejection Fraction %`))),]
fourchamb_mild <- mild_echo_data_1[c(which(mild_echo_data_1$`4 Chamber Ejection Fraction %` < 55 & is.na(mild_echo_data_1$`Biplane Ejection Fraction %`))),]
fourchamb_mild$Quantitative_Status <- 1

#Remove Qualitative rejections
mild_echo_data_3 <- mild_echo_data_2[-c(which(mild_echo_data_2$`Qualitative Ejection Fraction%` < 55 & is.na(mild_echo_data_2$`Biplane Ejection Fraction %`) & is.na(mild_echo_data_2$`4 Chamber Ejection Fraction %`))),]
qual_mild <- mild_echo_data_2[c(which(mild_echo_data_2$`Qualitative Ejection Fraction%` < 55 & is.na(mild_echo_data_2$`Biplane Ejection Fraction %`) & is.na(mild_echo_data_2$`4 Chamber Ejection Fraction %`))),]
qual_mild$Quantitative_Status <- 1

#Move on to shortening fraction
quan_normal <- mild_echo_data_3[-c(which(mild_echo_data_3$`Shortening Fraction %` < 28 & is.na(mild_echo_data_3$`Qualitative Ejection Fraction%`) & is.na(mild_echo_data_3$`Biplane Ejection Fraction %`) & is.na(mild_echo_data_3$`4 Chamber Ejection Fraction %`))),]
short_mild <- mild_echo_data_3[c(which(mild_echo_data_3$`Shortening Fraction %` < 28 & is.na(mild_echo_data_3$`Qualitative Ejection Fraction%`) & is.na(mild_echo_data_3$`Biplane Ejection Fraction %`) & is.na(mild_echo_data_3$`4 Chamber Ejection Fraction %`))),]
short_mild$Quantitative_Status <- 1
quan_normal$Quantitative_Status <- 0

final_set <- rbind(biplane_modsev,biplane_mild,fourchamb_mild,fourchamb_modsev,qual_modsev,qual_mild,short_mild,short_modsev,quan_normal)


#Data points
qlnorm_qnsev <- length(which((final_set$Qualitative_Status == "Normal") & (final_set$Quantitative_Status == "Moderate/Severe")))
qlnorm_qnmild <- length(which((final_set$Qualitative_Status == "Normal") & (final_set$Quantitative_Status == "Mild")))
qlnorm_qnnorm <- length(which((final_set$Qualitative_Status == "Normal") & (final_set$Quantitative_Status == "Normal")))
qlmild_qnsev <- length(which((final_set$Qualitative_Status == "Mild") & (final_set$Quantitative_Status == "Moderate/Severe")))
qlmild_qnmild <- length(which((final_set$Qualitative_Status == "Mild") & (final_set$Quantitative_Status == "Mild")))
qlmild_qnnorm <- length(which((final_set$Qualitative_Status == "Mild") & (final_set$Quantitative_Status == "Normal")))
qlsev_qnsev <- length(which((final_set$Qualitative_Status == "Moderate/Severe") & (final_set$Quantitative_Status == "Moderate/Severe")))
qlsev_qnmild <- length(which((final_set$Qualitative_Status == "Moderate/Severe") & (final_set$Quantitative_Status == "Mild")))
qlsev_qnnorm <- length(which((final_set$Qualitative_Status == "Moderate/Severe") & (final_set$Quantitative_Status == "Normal")))

#make DT column
final_set$DT <- as.POSIXct(paste(final_set$`Study Date`, final_set$`Study Time`), format="%m/%d/%y %H:%M:%S")

#drop unnecessary columns
final_set<- subset(final_set, select = -c(`Study Date`,`Study Time`))

write.csv(final_set, file = "~/4th year/STS Capstone/donorData/full_join/echo_w_analysis.csv", row.names = FALSE)

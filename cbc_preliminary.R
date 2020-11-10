
data_dir = "~/UVA/Fall_2020/Capstone/DonorNet-Shared"


library(tidyverse)
library(haven)
library(readxl)

setwd(data_dir)

data.cbc = read.table(file = "CBC.tsv.gz")


#### Create table of under 30 donors and recovered hearts #####


library(openxlsx)

cbc.donor.30 <- data.frame()

setwd("~/UVA/Fall_2020/Capstone/STAR-shared")

donorData <- read.csv(file = 'deceased_donor_data.tsv', sep = '\t')

newDonorData <- donorData[which(donorData$AGE_DON <= 30 & donorData$NUM_HR_RECOV == 1),]

setwd(data_dir)
cbc.donor.30 = cbc.data[cbc.data$DONOR_ID %in% newDonorData$DONOR_ID,]

#Write into a new excel file
write.xlsx(cbc.donor.30, file = "DonorNet CBC.xlsx")



#### data exploration ####

summary(cbc.donor.30)

boxplot(cbc.donor.30$HGB)
boxplot(cbc.donor.30$HCT)


unique.cbc <- unique(cbc.donor.30$DONOR_ID)

#Count how many outliers of hgb
upperq.hgb = quantile(cbc.donor.30$HGB, na.rm = TRUE)[4]

IQR.hgb <- quantile(cbc.donor.30$HGB, na.rm = TRUE)[4] - quantile(cbc.donor.30$HGB, na.rm = TRUE)[2]

upper.whisker.hgb = min(max(cbc.donor.30$HGB, na.rm = TRUE), (upperq.hgb + 1.5 * IQR.hgb))

length(which(cbc.donor.30$HGB > upper.whisker.hgb))


#Count how many outliers of hct
upperq.hct = quantile(cbc.donor.30$HCT, na.rm = TRUE)[4]

IQR.hct <- quantile(cbc.donor.30$HCT, na.rm = TRUE)[4] - quantile(cbc.donor.30$HCT, na.rm = TRUE)[2]

upper.whisker.hct = min(max(cbc.donor.30$HCT, na.rm = TRUE), (upperq.hct + 1.5 * IQR.hct))

length(which(cbc.donor.30$HCT > upper.whisker.hct))





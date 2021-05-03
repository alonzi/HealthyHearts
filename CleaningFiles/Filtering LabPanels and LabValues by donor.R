#this file gets the data from labpanels and labvalues for the donors we care about
#(that is, donors below the age of 30 and from whom a heart was recovered)

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)
donorData <- read.csv(file = 'deceased_donor_data.tsv', sep = '\t')
panels = read.table(file = "Donornet-Shared/Donornet-Shared/LabPanels.tsv.gz")
values = read.table(file = "Donornet-Shared/Donornet-Shared/LabValues.tsv.gz")

#final Data set with only people under the age of 30 and their heart was recovered
newDonorData <- donorData[which(donorData$AGE_DON <= 30 & donorData$NUM_HR_RECOV == 1),]
summary(newDonorData)
colnames(newDonorData)

#filter LabPanels and LabValues datasets by Donor IDs in the new Donor Data set
newPanels = panels[panels$V1 %in% newDonorData$DONOR_ID,]
newValues = values[values$V1 %in% newDonorData$DONOR_ID,]

#save new datasets to excel files
library(openxlsx)
write.xlsx(newPanels, file = "DonorNet LabPanels.xlsx")
write.xlsx(newValues, file = "DonorNet LabValues.xlsx")

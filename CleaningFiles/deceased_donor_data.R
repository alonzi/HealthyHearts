traindir <- "~/4th year/SYS 4021/Data/"
sourcedir <-"~/4th year/SYS 4021/Source"

setwd(traindir)
donorData <- read.csv(file = 'deceased_donor_data.tsv', sep = '\t')


summary(donorData)
colnames(donorData)





#final Data set with only people under the age of 30 and their heart was recovered

newDonorData <- donorData[which(donorData$AGE_DON <= 30 & donorData$NUM_HR_RECOV == 1),]


write.csv(newDonorData,"~/4th year/SYS 4021/Data/deceased_donor_cleaned.csv", row.names = FALSE)
newDonorData$TATTOOS_DON


#finding max and min of all columns

results <- sapply(newdonorData, max, na.rm = TRUE)
results <- sapply(newdonorData, min, na.rm = TRUE)
results <- sapply(newdonorData, mean, na.rm = TRUE)

install.packages('xlsx')
library(xlsx)
results
#open and close the file after each storage of variable (you need to run this multiple times depending on which variable you want)
write.xlsx(as.data.frame(results), file = "results.xlsx")

#cigarette use
Cigs <- rep(NA, nrow(newDonorData))
Cigs[which(newDonorData$HIST_CIG_DON == 'Y')] <- 'Yes'
Cigs[which(newDonorData$HIST_CIG_DON == 'N')] <- 'No'
Cigs[which(newDonorData$HIST_CIG_DON == 'U')] <- 'Unknown'
Cigs[which(is.na(newDonorData$HIST_CIG_DON))] <- 'Unknown'

Cigs


barplot(table(Cigs), main = "History of Cigarette Usage?", )


Risk <- rep(NA, nrow(newDonorData))
Risk[which(newDonorData$CDC_RISK_HIV_DON == 'Y')] <- 'Yes'
Risk[which(newDonorData$CDC_RISK_HIV_DON == 'N')] <- 'No'
Risk[which(newDonorData$CDC_RISK_HIV_DON == 'U')] <- 'Unknown'
Risk[which(is.na(newDonorData$CDC_RISK_HIV_DON))] <- 'Unknown'

Risk

barplot(table(Risk), main = "Marked as High Risk Patient", )

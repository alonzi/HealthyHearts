traindir <- "~/4th year/SYS 4021/Data/"
sourcedir <-"~/4th year/SYS 4021/Source"

setwd(traindir)
donorData <- read.csv(file = 'deceased_donor_data.tsv', sep = '\t')
donorData <- read.tsv("deceased_donor_data.tsv")

summary(donorData)
colnames(donorData)

#cigarette use
donorData[which(!is.na(donorData[98])),98]
Cigs <- rep(NA, nrow(donorData))
Cigs[which(donorData$HIST_CIG_DON == 'Y')] <- 'Yes'
Cigs[which(donorData$HIST_CIG_DON == 'N')] <- 'No'
Cigs[which(donorData$HIST_CIG_DON == 'U')] <- 'Unknown'
Cigs[which(is.na(donorData$HIST_CIG_DON))] <- 'Unknown'

Cigs

hist(Cigs)
barplot(table(Cigs), main = "History of Cigarette Usage?", )

donorData[which(!is.na(donorData$CDC_RISK_HIV_DON)), 107]
donorData1 <- na.omit(donorData)
max(donorData1$DDR1)


#finding max and min of all columns

results <- sapply(donorData, max, na.rm = TRUE)
results <- sapply(donorData, min, na.rm = TRUE)
results <- sapply(donorData, mean, na.rm = TRUE)

install.packages('xlsx')
library(xlsx)
results
#open and close the file after each storage of variable
write.xlsx(as.data.frame(results), file = "results.xlsx")

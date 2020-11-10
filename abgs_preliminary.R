

#ABGS

#-- Set path to DonorNet data
data_dir = "~/UVA/Fall_2020/Capstone/DonorNet-Shared"

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(tidyverse)
library(haven)
library(readxl)


setwd(data_dir)

data.abgs = read.table(file = "ABGS.tsv.gz")

##### End of Set Up #####


#### Create table of under 30 donors and recovered hearts #####
library(openxlsx)

abgs.donor.30 <- data.frame()

setwd("~/UVA/Fall_2020/Capstone/STAR-shared")

donorData <- read.csv(file = 'deceased_donor_data.tsv', sep = '\t')

newDonorData <- donorData[which(donorData$AGE_DON <= 30 & donorData$NUM_HR_RECOV == 1),]

setwd(data_dir)
abgs.donor.30 = data.abgs[data.abgs$DONOR_ID %in% newDonorData$DONOR_ID,]

#Write into a new excel file
write.xlsx(abgs.donor.30, file = "DonorNet ABGS.xlsx")



##### Relative Time Variable ########

library(lubridate)


#Create new variable to be the time since initial reading

unique_donorIDs <- unique(abgs.donor.30$DONOR_ID)
num_donorIDs <- length(unique_donorIDs)

#time since initial reading

fake_data.abgs <- as.data.frame(abgs.donor.30) #data frame to mess around with

data_sorted.abgs <- data.frame()

values.df.abgs <- data.frame()

temp_df.abgs <- data.frame()  #temporary df

i <- 1 #unique identifiers
j <- 1 #row in large data set
patient_entries <- 0 #the number of entries for that patient

#this sorts by time of entry within each patient and shows the time since initial reading
while(i <= num_donorIDs){
  if(unique_donorIDs[i] == fake_data.abgs$DONOR_ID[j]){ #if it is the same patient
    dftemp_row <- as.data.frame(fake_data[j,]) #row which we are extracting
    temp_df <- rbind(temp_df,dftemp_row) 
    j <- j+1
    patient_entries <- patient_entries+1
  }
  else{
    temp_df.abgs <- temp_df.abgs[(order(temp_df$DT)),]
    as.POSIXct(temp_d.abgsf$DT, "%Y-%m-%d %H:%M:%S")
    
    temp_donorIDs <- unique_donorIDs[i]
    temp_pH_min <- min(temp_df.abgs$ABG_PH)
    temp_pH_mean <- mean(temp_df.abgs$ABG_PH)
    temp_pH_max <- max(temp_df.abgs$ABG_PH)
    temp_PAO2_min <- min(temp_df.abgs$PAO2)
    temp_PAO2_mean <- mean(temp_df.abgs$PAO2)
    temp_PAO2_max <- max(temp_df.abgs$PAO2)
    temp_PEEP_min <- min(temp_df.abgs$PEEP)
    temp_PEEP_mean <- mean(temp_df.abgs$PEEP)
    temp_PEEP_max <- max(temp_df.abgs$PEEP)
    
    temp_value.abgs <- data.frame()
    
    temp_value.abgs <- cbind(temp_donorIDs,temp_pH_max,temp_pH_mean,temp_pH_min,temp_PAO2_max,temp_PAO2_mean,temp_PAO2_min,temp_PEEP_max,temp_PEEP_mean,temp_PEEP_min)
    values.df.abgs <- rbind(values.df.abgs,temp_value.abgs)
    
    for(counter in 1:patient_entries){ #creates relative time for each patient in hours
      temp_df.abgs$rel_time_hours[counter] <- difftime(temp_df.abgs$DT[counter],temp_df.abgs$DT[1], units = "hours")
    }
    data_sorted.abgs <- rbind(data_sorted.abgs,temp_df)
    temp_df.abgs <- data.frame()  #resets for new patient
    patient_entries <- 0
    i <- i+1
    
  }
}

#### Variable Exploration for ranges ####



# Finds the pH range for each patient so find the min max of each

maxmin_df <- data.frame()

summary(values.df.abgs, na.rm = TRUE)

while(i <= num_donorIDs){
  if(unique_donorIDs[i] == fake_data.abgs$DONOR_ID[j]){ #if it is the same patient
    dftemp_row.abgs <- as.data.frame(fake_data.abgs[j,]) #row which we are extracting
    temp_df.abgs <- rbind(temp_df.abgs,dftemp_row.abgs) 
    j <- j+1
    patient_entries <- patient_entries+1
  }
  else{
    temp_max <- max(temp_df.abgs$ABG_PH)
    temp_min <- min(temp_df.abgs$ABG_PH)
    maxmin_df[i,1] <- temp_max
    maxmin_df[i,2] <- temp_min
    maxmin_df[i,3] <- temp_max - temp_min
    colnames(maxmin_df) <- c("Max", "Min", "Range")
    temp_df.abgs <- data.frame()  #resets for new patient
    patient_entries <- 0
    i <- i+1
  }
}


#### Total Range values #####


summary(abgs.donor.30, na.rm = TRUE)

boxplot(abgs.donor.30$ABG_PH)
summary(abgs.donor.30$ABG_PH)

lowerq = quantile(abgs.donor.30$PAO2, na.rm = TRUE)[2]
upperq = quantile(abgs.donor.30$PAO2, na.rm = TRUE)[4]

which(abgs.donor.30$ABG_PH > 8.00)

#3 data points with ph >8


# Looking at the same for PA02
boxplot(abgs.donor.30$PAO2)
which(abgs.donor.30$PAO2 > quantile(abgs.donor.30$PAO2, na.rm = TRUE)[4])

quantile(abgs.donor.30$PAO2, na.rm = TRUE)[5]

boxplot(abgs.donor.30$PAO2)

IQR.PAO2 <- quantile(abgs.donor.30$PAO2, na.rm = TRUE)[4] - quantile(abgs.donor.30$PAO2, na.rm = TRUE)[2]

upper.whisker.PAO2 = min(max(abgs.donor.30$PAO2, na.rm = TRUE), (upperq + 1.5 * IQR))
lower.whisker.PAO2 = max(min(abgs.donor.30$PAO2, na.rm = TRUE), (lowerq - 1.5 * IQR))


length(which(abgs.donor.30$PAO2 > upper.whisker.PAO2))

#Looking at the same for Peep
upperq.peep = quantile(abgs.donor.30$PEEP, na.rm = TRUE)[4]

IQR.peep <- quantile(abgs.donor.30$PEEP, na.rm = TRUE)[4] - quantile(abgs.donor.30$PEEP, na.rm = TRUE)[2]

upper.whisker.peep = min(max(abgs.donor.30$PEEP, na.rm = TRUE), (upperq.peep + 1.5 * IQR.peep))

length(which(abgs.donor.30$PEEP > upper.whisker.peep))

boxplot(abgs.donor.30$PEEP)



# source code for all of the functions for both data imputation and GLM

#okay so i need to write a couple of functions that we can use to plug in variables
# ok lets make a function that cleans the data

#creates new data framed with new age groups and cleans the data
echo_clean <- function(df){
  df_names <- c("BrainDeath","DOB_DON","Duration")
  if(is.data.frame(df) == TRUE){
    if((df_names[1] %in% names(df) == TRUE) && (df_names[2] %in% names(df) == TRUE) && (df_names[3] %in% names(df) == TRUE)){
      echo_age(df)
    }
    else{
      stop("Dataframe doesn't contain time of Brain Death, Donor DOB, or Duration")
    }
  }
  else{
    stop("Object is not a dataframe")
    }
}

# a new function to create age group so you can use ifelse above
echo_age <- function(df){
  #cuts duration to 3 days before brain death
  df.age <- as.data.frame(df[which(df$Duration>=-72),])
  df.age$Age <- rep(NA, nrow(df.age))
  df.age$BrainDeathDate <- rep(NA, nrow(df.age))
  df.age$BrainDeathDate <- strptime(df.age$BrainDeath, "%m/%d/%Y")
  df.age$DOB_DON <- strptime(df.age$DOB_DON, "%m/%d/%Y")
  df.age$Age <- difftime(df.age$BrainDeathDate, df.age$DOB_DON, units = "weeks")
  
  #convert Age from weeks to years
  df.age$Age = df.age$Age / 52
  
  #Create age groups
  df.age$age.group <- rep(NA, nrow(df.age))
  df.age <- mutate(df.age, age.group = case_when(
    df.age$Age < 1 ~ "<1 year",
    df.age$Age < 3 & df.age$Age >= 1 ~ "1-3 years",
    df.age$Age < 6 & df.age$Age >= 3 ~ "3-6 years",
    df.age$Age < 12 & df.age$Age >= 6 ~ "6-12 years",
    df.age$Age >= 12 ~ "12+ years",
    is.na(df.age$DOB_DON) == TRUE ~ "Missing Data" #This is because the glm for age.groups won't work if there are NAs so I assigned this to missing DOB_DON
  ))
  
  df.age <-mutate(df.age, age.group = case_when(
    df.age$Age < 0.5 ~ "0-6 months",
    df.age$Age < 1 & df.age$Age >= 0.5 ~ "6-12 months",
    TRUE ~ age.group
  ))
  as.factor(df.age$age.group)
  
  df.age <- df.age[,!(names(df.age) %in% c("BrainDeathDate","Repeat.Instrument","Comment.","Complete."))]
  return(df.age)
}

#the next function I will build is to organize the data by each patient and duration within each patient
echo_sort <- function(df){
  df_names_1 <- c("DONOR_ID","Duration")
  if(is.data.frame(df) == TRUE){
    if((df_names_1[1] %in% names(df) == TRUE) && (df_names_1[2] %in% names(df) == TRUE)){
      df <- df[order(df[,c("DONOR_ID")], df[,"Duration"]),] #first by patient id and then by duration
      #new variables for the qualitative status for the next echo reading for the patient
      df$qual.nextecho <- rep(NA, nrow(df)) 
      df$quant.nextecho <- rep(NA, nrow(df)) 
      
      #make sure the reading of the next echo is the same patient
      observations <- length(df$DONOR_ID) - 1
      for(p in 1:observations){
        p1 <- df[["DONOR_ID"]][p]
        p2 <- df[["DONOR_ID"]][p+1]
        if(p1 == p2){ 
          #if the current patient in the index is the same as the next patient then quant/qual.nextecho should be set as the immediate next quant/qual status
          #next qualitative echo status 
          df[["qual.nextecho"]][p] <- df[["Qualitative_Status"]][p+1]
          #next quant echo status
          df[["quant.nextecho"]][p] <- df[["Quantitative_Status"]][p+1]
          
        }
        else{
          #if the next echo is not the same patient, then we set it to na and then remove from data set (maybe) so glm can run on it
          df[["qual.nextecho"]][p] <- NA
          df[["quant.nextecho"]][p] <- NA
        }
      }
      return(df)
    }
    else{
      stop("Dataframe doesn't contain Donor ID or Duration")
    }
  }
  else{
    stop("Object is not a dataframe")
  }
  
}

#function that calculates the percentage of missing data for each variable
var_missing <- function(df){
  if(is.data.frame(df) == TRUE){
      num_vars <- ncol(df) #number of columns
      obs <- nrow(df) #number of observations
      vars <- matrix(data = NA, nrow = 1, ncol = num_vars)
      colnames(vars) <- names(df)
      for(i in 1:num_vars){
        na_count <- sum(is.na(df[[i]])) #how many nas are in that column
        vars[1,i] <- na_count/obs #gives the percentage of the amount of nas
      }
      return(vars)
    }
  else{
    stop("Input is not a data frame")
  }
}

#function that does the same as missing variables but sorts it by duration group
var_missing_dur <- function(df){
  df.0dur <- df[which(df$Duration <= 0),]
  df.3dur <- df[which(df$Duration <= 3),]
  df.6dur <- df[which(df$Duration <= 6),]
  df.12dur <- df[which(df$Duration <= 12),]
  df.18dur <- df[which(df$Duration <= 18),]
  df.24dur <- df[which(df$Duration <= 24),]
  df.36dur <- df[which(df$Duration <= 36),]
  df.48dur <- df[which(df$Duration <= 48),]
  df.72dur <- df[which(df$Duration <= 72),]
  
  list_DF <- list(df.0dur,df.3dur,df.6dur,df.12dur,df.18dur,df.24dur,df.36dur,df.48dur,df.72dur)
  varmat_dur <- matrix(data = NA, nrow = (length(list_DF)), ncol = ncol(df))
  for(i in 1:length(list_DF)){
    varmat_dur[i,] <- var_missing(list_DF[[i]]) #creates matrix of % var missing by age group
  }
  tot_missing <- var_missing(df)
  tot_var <- rbind(varmat_dur,tot_missing)  #combines duration matrix with total % missing
  rownames(tot_var) <- c("Time <= 0 hours","Time <= 3 hours","Time <= 6 hours","Time <= 12 hours","Time <= 18 hours","Time <= 24 hours","Time <= 36 hours","Time <= 48 hours","Time <= 72 hours","Total Missing")
  return(tot_var)
}

#returns a vector of the most recent measurements for that donor at that time
mrm_donor_time <- function(df, donor_id, time) {
  
  donor_info <- df[which(df$DONOR_ID == donor_id), ]
  donor_duration <- donor_info[which(donor_info$Duration <= time), ]
  if(((max(donor_duration$Duration) <= time) == TRUE) & (is.infinite(max(donor_duration$Duration)) == FALSE)){
    
    #checks if there is a measurement at or before that time
    donor_duration <- donor_duration[order(donor_duration[,c("Duration")],decreasing = TRUE),]
    #getting duration value of the measurement equal to or closest to specified duration
    #next observation carried forward
    if(nrow(donor_duration) > 1){ #checks for multiple observations 
      donor_duration <- donor_duration %>% mutate(donor_duration,na.locf(donor_duration,fromLast = TRUE, na.rm = FALSE))
    }
    else{
      donor_duration <- donor_duration
    }
    most_recent <- donor_duration[which.max(donor_duration$Duration), ]
    return(most_recent)
  }
  else{
    temp_row <- donor_info[1,] #temporary row that will be removed in a later function that calls this one
    temp_row <- rep(NA)
    return(temp_row)
  }
}

#function that gives a data frame of all most recent measurements at certain times
mrm_df_time <- function(df,time){
  uni_dons <- unique(df$DONOR_ID)
  data_mrm <- mrm_donor_time(df,uni_dons[1],time)
  #for larger datasets, this is going to be very slow. I could not figure out a faster way given the time we had left to finish
  # so i would recommend finding a better way than using a for loop
  for(i in 2:(length(uni_dons))){
    temp_vec <- mrm_donor_time(df, uni_dons[i], time)
    data_mrm <- rbind(data_mrm,temp_vec)
  }
  data_mrm <- data_mrm[rowSums(is.na(data_mrm)) != ncol(data_mrm),] #removes empty rows
  return(data_mrm)
}

#this function creates a data frame of the most recent measurements as discrete times
# it differs from the others because it will include the measurements from previous time,
# but it is still the most recent at those discrete times
mrm_df <- function(df){
  times <- c(0,3,6,12,18,24,36,48,72)
  df_all <- data.frame()
  for(i in 1:length(times)){
    df_temp <- mrm_df_time(df,times[i])
    df_all <- rbind(df_all,df_temp)
  }
  df_all <- df_all[!duplicated(df_all),]
  return(df_all)
}


#function that given the donor id and duration time, outputs the row of measurements
#from that dataframe at the same or closest time before duration given
donor_info_time <- function(df, donor_id, time) {
  
  donor_info = df[which(df$DONOR_ID == donor_id), ]
  donor_duration = donor_info[which(donor_info$Duration <= time), ]
  #getting duration value of the measurement equal to or closest to specified duration
  most_recent = donor_duration[which.max(donor_duration$Duration), ]
  closest_val = most_recent$Duration
  
  #in the case of multiple measurements at same time, returning both
  closest_meas = donor_duration[which(donor_duration$Duration == closest_val), ]
  #print(most_recent)
  #return message if there is no measurement 
  if (dim(closest_meas)[1] == 0){
    return("No measurements taken on or before specified time after brain death.")
  }
  else{
    return(closest_meas)
  }
  
}

#calculating schwartz score
schwartz_calc <- function(df){
  df$Schwartz <- (0.413*df$Height) / df$CREATININE
}
df_new$Schwartz <- schwartz_calc(df_new)

#calcluating MELD score
meld_calc <- function(df){
  df$MELD <- 3.78*log(df$BILIRUBIN) + 11.2*log(df$INR) + 9.57*log(df$CREATININE) + 6.43
}



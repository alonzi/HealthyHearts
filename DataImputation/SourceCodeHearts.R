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

#function that does the same as missing variables but sorts it by age group
var_missing_AG <- function(df){
  if(is.data.frame(df) == TRUE){
    if(c("age.group") %in% names(df) == TRUE){
      df.0to6Months <- df[which(df$age.group == "0-6 months"),]
      df.6to12Months <- df[which(df$age.group == "6-12 months"),]
      df.1to3Years <- df[which(df$age.group == "1-3 years"),]
      df.3to6Years <- df[which(df$age.group == "3-6 years"),]
      df.6to12Years <- df[which(df$age.group == "6-12 years"),]
      df.12plusYears <- df[which(df$age.group == "12+ years"),]
      list_DF <- list(df.0to6Months,df.6to12Months,df.1to3Years,df.3to6Years,df.6to12Years,df.12plusYears)
      varmat_age <- matrix(data = NA, nrow = (length(list_DF)), ncol = ncol(df))
      for(i in 1:length(list_DF)){
        varmat_age[i,] <- var_missing(list_DF[[i]]) #creates matrix of % var missing by age group
      }
      tot_missing <- var_missing(df)
      tot_var <- rbind(varmat_age,tot_missing)  #combines age group matrix with total % missing
      rownames(tot_var) <- c("0-6 months","6-12 months","1-3 years","3-6 years","6-12 years","12+ years","Total Missing")
      return(tot_var)
    }
    else{
      stop("Does not contain age group")
    }
  }
  else{
    stop("Input is not a data frame")
  }
}

# create function that removes variables with more than 95% data missing
rm_var95 <- function(df){
  if(is.data.frame(df) == TRUE){
    miss_varmat <- var_missing(df)
    varmat_col <- ncol(miss_varmat)
    drop_col <- c()
    for(i in 1:varmat_col){
      if(miss_varmat[1,i] > 0.95){
        drop_col <- c(colnames(miss_varmat)[i],drop_col)
      }
      else{}
    }
    return(drop_col)
  }
  else{
    stop("Input is not a data frame")
  }

}

#imputing data based on median
impute_med <- function(df){
  if(is.data.frame(df) == TRUE){
    df.cat <- df[,c(45:53)] #only categorical data to do imputation and bind later
    df.num <- df[-c(45:53)] #only numerical data to do imputation and bind later
    
    df.num.median <- na_mean(df.num, option = "median") #imputing median for all data
    df.median <- cbind(df.num.median,df.cat) #bind back to categorical

    return(df.median)
  }
  
  else{
    stop("Input is not a data frame")
  }
}


#imputing data based on the median and age groups
impute_age_med <- function(df){
  if(is.data.frame(df) == TRUE){
    
    df.0to6Months <- df[which(df$age.group == "0-6 months"),]
    df.6to12Months <- df[which(df$age.group == "6-12 months"),]
    df.1to3Years <- df[which(df$age.group == "1-3 years"),]
    #df.3to6Years <- df[which(df$age.group == "3-6 years"),] no data
    df.6to12Years <- df[which(df$age.group == "6-12 years"),]
    df.12plusYears <- df[which(df$age.group == "12+ years"),]
    
    df.0to6Months.med <- na.mean(df.0to6Months, option = "median") # Median Imputation
    df.6to12Months.med <- na.mean(df.6to12Months, option = "median") # Median Imputation
    df.1to3Years.med <- na.mean(df.1to3Years, option = "median") # Median Imputation
    df.6to12Years.med <- na.mean(df.6to12Years, option = "median") # Median Imputation
    df.12plusYears.med <- na.mean(df.12plusYears, option = "median") # Median Imputation

    df.age.med <- rbind(df.0to6Months.med,df.6to12Months.med,df.1to3Years.med,df.6to12Years.med,df.12plusYears.med)
    return(df.age.med)
    }
  
  else{
    stop("Input is not a data frame")
  }
}
#impute median of variables with less than 95% null and age groups
impute_age_med95 <- function(df){
  if(is.data.frame(df) == TRUE){
    var_drop <- rm_var95(df) #list of variables who have more than 95% of data missing
    
    
    df.0to6Months <- df[which(df$age.group == "0-6 months"),]
    df.0to6Months.95 <- df.0to6Months[!names(df.0to6Months) %in% var_drop]
    df.6to12Months <- df[which(df$age.group == "6-12 months"),]
    df.6to12Months.95 <- df.6to12Months[!names(df.6to12Months) %in% var_drop]
    df.1to3Years <- df[which(df$age.group == "1-3 years"),]
    df.1to3Years.95 <- df.1to3Years[!names(df.1to3Years) %in% var_drop]
    #df.3to6Years <- df[which(df$age.group == "3-6 years"),] no data
    df.6to12Years <- df[which(df$age.group == "6-12 years"),]
    df.6to12Years.95 <- df.6to12Years[!names(df.6to12Years) %in% var_drop]
    df.12plusYears <- df[which(df$age.group == "12+ years"),]
    df.12plusYears.95 <- df.12plusYears[!names(df.12plusYears) %in% var_drop]
    
    df.0to6Months.95.med <- na.mean(df.0to6Months.95, option = "median") # Median Imputation for <95%
    df.6to12Months.95.med <- na.mean(df.6to12Months.95, option = "median") # Median Imputation for <95%
    df.1to3Years.95.med <- na.mean(df.1to3Years.95, option = "median") # Median Imputation for <95%
    df.6to12Years.95.med <- na.mean(df.6to12Years.95, option = "median") # Median Imputation for <95%
    df.12plusYears.95.med <- na.mean(df.12plusYears.95, option = "median") # Median Imputation for <95%
    
    df.95.age.med <- rbind(df.0to6Months.95.med,df.6to12Months.95.med,df.1to3Years.95.med,df.6to12Years.95.med,df.12plusYears.95.med)
    return(df.95.age.med)
  }
  else{
    stop("Input is not a data frame")
  }
}


#imputing data based on median
impute_med95 <- function(df){
  if(is.data.frame(df) == TRUE){
    var_drop <- rm_var95(df) #list of variables who have more than 95% of data missing
    df.cat <- df[,c(45:53)] #only categorical data to do imputation and bind later
    df.num <- df[-c(45:53)] #only numerical data to do imputation and bind later
    df.num.95 <- df[!names(df.num) %in% var_drop] #df of only variables with less than 95% of data missing
    
    df.num.95.med <- na_mean(df.num.95, option = "median") #imputing median for all data
    df.95.median <- cbind(df.num.95.med,df.cat) #bind back to categorical
    return(df.95.median)
  }
  
  else{
    stop("Input is not a data frame")
  }
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



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





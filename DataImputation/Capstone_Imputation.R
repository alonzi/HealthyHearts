# import necessary packages 
library(dplyr)
library(ggplot2)
library(tidyr) 
library(zoo)

# import data and make it a dataframe
data_dir <- "C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone"
setwd(data_dir)
data7100 <- read.csv("EchoMatchesw7100.csv")
df_new <- as.data.frame(data7100)
#add swartz and meld score
df_new$Schwartz <- rep(NA, nrow(df_new))
df_new$MELD <- rep(NA, nrow(df_new))

#calculating schwartz score
schwartz_calc <- function(df){
  df$Schwartz <- (0.413*df$Height) / df$CREATININE
}
df_new$Schwartz <- schwartz_calc(df_new)

#calcluating MELD score
meld_calc <- function(df){
  df$MELD <- 3.78*log(df$BILIRUBIN) + 11.2*log(df$INR) + 9.57*log(df$CREATININE) + 6.43
}
df_new$MELD <- meld_calc(df_new)

show(df_new)

# drop the useless features for future analysis

drop_col <- c("X61",
              "X62",
              "Complete.",
              "Comment.",
              "Repeat.Instrument",
              "Repeat.Instance")
df_new <- df_new[ , !(names(df_new) %in% drop_col)]



# age calculation

echo_age <- function(df){
  #cuts duration to 3 days before brain death
  df_new.age <- as.data.frame(df_new[which(df_new$Duration>=-72),])
  df_new.age$Age <- rep(NA, nrow(df_new.age))
  df_new.age$BrainDeathDate <- rep(NA, nrow(df_new.age))
  df_new.age$BrainDeathDate <- strptime(df_new.age$BrainDeath, "%m/%d/%Y")
  df_new.age$DOB_DON <- strptime(df_new.age$DOB_DON, "%m/%d/%Y")
  df_new.age$Age <- difftime(df_new.age$BrainDeathDate, df_new.age$DOB_DON, units = "weeks") #here is years actually
  #convert Age from weeks to years
  df_new.age$Age = df_new.age$Age / 52
  #Create age groups
  df_new.age$age.group <- rep(NA, nrow(df_new.age))
  df_new.age <- mutate(df_new.age, age.group = case_when(
    df_new.age$Age < 1 ~ "0-1",
    df_new.age$Age < 3 & df_new.age$Age >= 1 ~ "1-3",
    df_new.age$Age < 6 & df_new.age$Age >= 3 ~ "3-6",
    df_new.age$Age < 12 & df_new.age$Age >= 6 ~ "6-12",
    df_new.age$Age >= 12 ~ "12+",
    is.na(df_new.age$DOB_DON) == TRUE ~ "" #This is because the glm for age.groups won't work if there are NAs so I assigned this to missing DOB_DON
  ))
  
  df_new.age <-mutate(df_new.age, age.group = case_when(
    df_new.age$Age < 0.5 ~ "0-0.5",
    df_new.age$Age < 1 & df_new.age$Age >= 0.5 ~ "0.5-1",
    TRUE ~ age.group
  ))
  as.factor(df_new.age$age.group)
  df_new.age <- df_new.age[,!(names(df_new.age) %in% c("BrainDeathDate","Repeat.Instrument","Comment.","Complete."))]
  #df_new.age <- as.character(df_new.age$DOB_DON)
  return(df_new.age)
}
df_new <- echo_age(df_new)
#needed to change to character bc of bugs, but I am not sure why this fixes it
df_new$DOB_DON <- as.character(df_new$DOB_DON) 

# drop the values NA's in the "Accepted" column
df_new <- df_new %>% drop_na(Accepted)


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

df_new_mrm <- mrm_df(df_new)

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

missing_df_mrm <- var_missing_dur(df_new_mrm)
#writing a csv so table can be uploaded into the paper
write.csv(missing_df_mrm,"C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone/missing_mrm.csv", row.names = TRUE)

# find out the NA ratios

missing.values <- df_new_mrm %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

## `summarise()` regrouping output by 'key', 'total' (override with `.groups` argument)
levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of Missing Values", x =
         'Features', y = "% of Missing Values")

percentage.plot




# # remove columns with more than 75% NA ratio -> only 14 variables are left
#Theta_75 = 0.75
#df_new = df_new[,!sapply(df_new, function(x) mean(is.na(x))) > Theta_75]

#decided t0 change it to 0.8 because score has a little over 75% missing and I wanted to include it
# 35 variables left instead on 14
Theta = 0.8
df_new_mrm = df_new_mrm[,!sapply(df_new_mrm, function(x) mean(is.na(x))) > Theta]


# convert non-numerical values to numerical
df_new_mrm$Accepted <- ifelse(df_new_mrm$Accepted=="Yes",1,0)
df_new_mrm$Mitral.valve.insufficiency <- substring(df_new_mrm$Mitral.valve.insufficiency, nchar(as.character(df_new_mrm$Mitral.valve.insufficiency)) - 0)
df_new_mrm$Aortic.valve.insufficiency <- substring(df_new_mrm$Aortic.valve.insufficiency, nchar(as.character(df_new_mrm$Aortic.valve.insufficiency)) - 0)
df_new_mrm$Tricuspid.valve.insufficiency <- substring(df_new_mrm$Tricuspid.valve.insufficiency, nchar(as.character(df_new_mrm$Tricuspid.valve.insufficiency)) - 0)
df_new_mrm$Pulmonary.valve.insufficiency <- substring(df_new_mrm$Pulmonary.valve.insufficiency, nchar(as.character(df_new_mrm$Pulmonary.valve.insufficiency)) - 0)
df_new_mrm$Global.Right.Ventricular.Dysfunction <- substring(df_new_mrm$Global.Right.Ventricular.Dysfunction, nchar(as.character(df_new_mrm$Global.Right.Ventricular.Dysfunction)) - 0)
df_new_mrm$Global.Left.Ventricular.Dysfunction <- substring(df_new_mrm$Global.Left.Ventricular.Dysfunction, nchar(as.character(df_new_mrm$Global.Left.Ventricular.Dysfunction)) - 0)

summary(df_new_mrm)

write.csv(df_new_mrm,"C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone/7100_cleaned.csv", row.names = FALSE)

str(df_new_mrm)
# split the full df_new df to based on their data type -> categorical and numerical
df_new_mrm <- read.csv("C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone/7100_cleaned.csv") # the df_new has to be re-read due to an unknown error


# reorder the columns so that cate and num can be perfectly split
# removed milosprine because it only has NAs
df_new_mrm <- df_new_mrm[,c(1,2,3,4,5,6,7,8,
                    9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,34,
                    26,27,28,29,30,31,32,33,35)]
                    
names(df_new_mrm)

df_num <- as.data.frame(df_new_mrm[9:25])
summary(df_num)

df_cate <- as.data.frame(df_new_mrm[26:34])

df_cate$age.group <- as.character(df_cate$age.group)
df_cate$age.group[which(df_cate$age.group=="0-0.5")] <- "1"
df_cate$age.group[which(df_cate$age.group=="0.5-1")] <- "2"
df_cate$age.group[which(df_cate$age.group=="1-3")] <- "3"
df_cate$age.group[which(df_cate$age.group=="3-6")] <- "4"
df_cate$age.group[which(df_cate$age.group=="6-12")] <- "5"
df_cate$age.group[which(df_cate$age.group=="12+")] <- "6"
df_cate$age.group <- as.numeric(df_cate$age.group)

summary(df_cate)

df_cate
# df_donor_info and other info
df_donor_info <- as.data.frame(df_new_mrm[1:8])

# imputation by using the mice package
library(mice)
library(VIM)
library(lattice)

# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/


methods(mice)

# impute for numerical value
imp_num <- mice(df_num, meth="pmm", m = 10)
imp_num$meth
completedData_num <- complete(imp_num,1)
densityplot(imp_num, title(main = "Density plot for Numerical Measurements"))

# impute for categorical value
imp_cate <- mice(df_cate, meth="pmm", m = 10)
imp_cate$meth
completedData_cate <- complete(imp_cate,1)
densityplot(imp_cate, title(main = "Density plot for Categorical Measurements"))

# join the datasets
basic_info <- cbind(df_donor_info)
echo_data <- cbind(completedData_num,completedData_cate)
imputed_7100 <- cbind(basic_info,echo_data)

write.csv(imputed_7100,"C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone/imputed_7100.csv", row.names = FALSE)






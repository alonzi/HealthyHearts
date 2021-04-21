# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/logistic-regression-analysis-r/tutorial/
# https://online.datasciencedojo.com/blog/logistic-regression-in-r-a-classification-technique-to-predict-credit-card-default
# https://rpubs.com/abhaypadda/logistic-regression-using-titanic-data

## Load the necessary packages
library(dplyr)
library(dummies)
library(caTools)
library(MASS)
library(zoo)


## Load the data and delete the unuseful columns
data_dir <- "C:/Users/ianpe/Documents/UVA/Fourth Year/Capstone"
setwd(data_dir)
df <- read.csv("imputed_7100.csv")

#loading functions to create dataframes at different discrete times
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

#data frame of most recent measurements at 24 hours after braindeath
df.24dur <- mrm_df_time(df,24) 

### going to use 24 hours as the test df
drop_col <- c(1:4,6:8,18)
#df <- df %>% select(-c(DONOR_ID,DT,DONOR_ID,BrainDeath,Vasopressin,TRR_ID_CODE,DOB_DON,DOB,ï..,DON_ID))
df.24dur <- df.24dur[,-drop_col] #the line above is buggy but those are the dropped columns
#vasopressin is dropped because the distribution is skewed

## Check data structure
str(df.24dur)

## Let's check for any missing values in the data
colSums(is.na(df.24dur))

## Checking for empty values
colSums(df.24dur =='')

## Standardizaiton for the numerical data
df.24dur <- df.24dur %>%
  mutate_at(c(2:17), funs(c(scale(.))))

## Check number of uniques values for each of the column to find out columns which we can convert to factors
sapply(df.24dur, function(x) length(unique(x)))

as.numeric(unlist(df.24dur))

str(df.24dur)

## Converting the categorical data to factors
for (i in c("Mitral.valve.insufficiency","Aortic.valve.insufficiency","Tricuspid.valve.insufficiency",
            "Pulmonary.valve.insufficiency","Global.Left.Ventricular.Dysfunction","Global.Right.Ventricular.Dysfunction",
            "Qualitative_Status","Quantitative_Status","age.group")){
  df.24dur[,i]=as.factor(df.24dur[,i])
}

## Create dummy variables for categorical variables
str(df.24dur)
names(df.24dur)
df.24dur <- dummy.data.frame(df.24dur, names=c("Mitral.valve.insufficiency","Aortic.valve.insufficiency","Tricuspid.valve.insufficiency",
                                           "Pulmonary.valve.insufficiency","Global.Left.Ventricular.Dysfunction","Global.Right.Ventricular.Dysfunction",
                                           "Qualitative_Status","Quantitative_Status","age.group"), sep="_")


## Some features need to be deleted after hot-encoding due to singularity.
df.24dur = subset(df.24dur, select = -c(Mitral.valve.insufficiency_2,Aortic.valve.insufficiency_1,Tricuspid.valve.insufficiency_2,
                                    Pulmonary.valve.insufficiency_1,Global.Left.Ventricular.Dysfunction_3,Global.Right.Ventricular.Dysfunction_1,
                                    Qualitative_Status_2,Quantitative_Status_2,age.group_6))






## Splitting training and test data for 24 hours
df_2 <- df.24dur[,-2]
sample_2 <- sample.int(n = nrow(df_2), size = floor(.80*nrow(df_2)), replace = F)
train_2 <- df_2[sample_2, ]
test_2 <- df_2[-sample_2, ]


summary(df.24dur)
str(df.24dur)
set.seed(100) 

sample <- sample.int(n = nrow(df.24dur), size = floor(.80*nrow(df.24dur)), replace = F)
train <- df.24dur[sample, ]
test  <- df.24dur[-sample, ]

train


## Model Creation
model <- glm(Accepted ~ Age + Weight + Height + AVG_BP_SYST + AVG_BP_DIAST + AVG_PULSE_RANGE_START +
               CVP_INT_RANGE_START + BODYTEMP_RANGE_START + Global.Left.Ventricular.Dysfunction_0 +
               Global.Left.Ventricular.Dysfunction_1 + Global.Left.Ventricular.Dysfunction_2 +
               Qualitative_Status_0 + Quantitative_Status_0 + Qualitative_Status_1 + 
               Quantitative_Status_1 + Score + X4.Chamber.Ejection.Fraction.. + Shortening.Fraction..,family=binomial(link='logit'),data=train,maxit = 200)


## Model Summary
summary(model)

## Using anova() to analyze the table of deviance
anova(model, test="Chisq")

## Predicting Test Data 
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

result

## Confusion matrix
library(caret)
accepted <- as.factor(test$Accepted)
result <- as.factor(result)
confusionMatrix(data=result, reference=as.factor(test$Accepted))

sum(test$Accepted)


## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model, newdata=df.24dur, type="response")
ROCRpred <- prediction(predictions, df.24dur$Accepted)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))#, title(main = "ROC Curve for Predicted Values"))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

## correlation matrix and visulizations

## plot 1
library(Hmisc)
library(PerformanceAnalytics)

res <- cor(df.24dur)
round(res, 2)

## plot 2
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(100)
heatmap(x = res, col = col, symm = TRUE, main = "Correlation between Factors 24 hours after brain death")

## plot 3
library(heatmaply)

heatmaply_cor(
  cor(df.24dur),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)

## plot 4
library(tidyverse)

cormat <- round(cor(df.24dur),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

lower_tri <- res
lower_tri[lower.tri(lower_tri)] <- NA #OR upper.tri function
lower_tri
corr_res <- reshape2::melt(lower_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "bottom", title.hjust = 0.5,legend.box = "horizontal"))


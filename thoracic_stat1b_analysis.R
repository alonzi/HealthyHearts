data_dir = "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv"
setwd(data_dir)
require(data.table)
library(ggplot2)
stat1a = as.data.frame(fread("thoracic_stat1b.tsv"))
names(stat1a)

stat1a = stat1a[which(stat1a$AGE_GROUP == "Pediatric"), ] 
#only about 5300 entries

summary(stat1a)
names(stat1a)

#Frequency Graphs for Binary Variables

par(mfrow=c(3,4))
var = names(stat1a)
for(i in 1:10) {
  barplot(table(stat1a[, var[i]]), main=var[i])
}
print(var[1:10])
#only VAD (Left and/or right ventricular assist device implanted) and CONT_IV_INOTROP had all 0 (are these just adult values?)


# look at other statistics
stat1a$REQ_LOW_INOTROPE <- factor(stat1a$REQ_LOW_INOTROPE, labels = c("0", "1"))
summary(stat1a$REQ_LOW_INOTROPE)
#22.74% Requires infusion of a low-dose single inotrope (Ped A)

stat1a$REQ1_INOTROPIC_AGENT <- factor(stat1a$REQ1_INOTROPIC_AGENT, labels = c("0", "1"))
summary(stat1a$REQ1_INOTROPIC_AGENT)
#20.63% have Infusion of inotropes but does not qualify for status 1A

stat1a$GROWTH_FAILURE <- factor(stat1a$GROWTH_FAILURE, labels = c("0", "1"))
summary(stat1a$GROWTH_FAILURE)
#25.95% of Patient are experiencing growth failure (Ped C)

stat1a$REQ3_CRITERIA_NOT_MET <- factor(stat1a$REQ3_CRITERIA_NOT_MET, labels = c("0", "1"))
summary(stat1a$REQ3_CRITERIA_NOT_MET)
#8.66% do not meet criteria for Status 1B but has an urgency and potential for benefit comparable to other status 1A candidates   (Ped 1B exception)

length(table(stat1a$WL_ID_CODE))
#3730 unique patients 

par(mfrow=c(1,1))
hist(table(stat1a$WL_ID_CODE), main="Histogram of number of entries per patient", xlim=c(0,50))
#most multiple entries are less than 5

#splitting data by Waitlist ID
stat1a_split <- split(stat1a, stat1a$WL_ID_CODE)

class(stat1a_split)

stat1a_mult <- list()
for(i in 1:9425) {
  num <- lapply(stat1a_split[i], NROW)
  num <- num[[1]]
  if(num > 1){
    stat1a_mult = append(stat1a_mult, stat1a_split[i])
  }
}
#922 with multiple time entries

length(which(apply(stat1a_mult[[2]], 2, function(x) length(unique(x))) != 1))

test <- stat1a_mult[[1]]
class(test)
stat1a_mult_change <- list()
for(i in 1:922) {
  len = length(which(apply(stat1a_mult[[i]], 2, function(x) length(unique(x))) != 1))
  if(len != 2){
    stat1a_mult_change = append(stat1a_mult_change, stat1a_mult[i])
  }
}

#recoding to factors
summary(stat1a$VAD)
stat1a$VAD[which(stat1a$VAD == 0)] <- "0"
stat1a$VAD <- as.factor(stat1a$VAD)
summary(stat1a$VAD)

summary(stat1a$CONT_IV_INOTROPES)
stat1a$CONT_IV_INOTROPES[which(stat1a$CONT_IV_INOTROPES == 0)] <- "0"
stat1a$CONT_IV_INOTROPES <- as.factor(stat1a$CONT_IV_INOTROPES)
summary(stat1a$CONT_IV_INOTROPES)

summary(stat1a$CONT_IV_INOTROPES)
stat1a$CONT_IV_INOTROPES[which(stat1a$CONT_IV_INOTROPES == 0)] <- "0"
stat1a$CONT_IV_INOTROPES <- as.factor(stat1a$CONT_IV_INOTROPES)
summary(stat1a$CONT_IV_INOTROPES)

stat1a$OTHER[which(stat1a$OTHER == 0)] <- "0"
stat1a$OTHER[which(stat1a$OTHER == 1)] <- "1"
stat1a$OTHER <- as.factor(stat1a$OTHER)
summary(stat1a$OTHER)

stat1a$LESS_SIX_MONS[which(stat1a$LESS_SIX_MONS == 0)] <- "0"
stat1a$LESS_SIX_MONS[which(stat1a$LESS_SIX_MONS == 1)] <- "1"
stat1a$LESS_SIX_MONS <- as.factor(stat1a$LESS_SIX_MONS)
summary(stat1a$LESS_SIX_MONS)

stat1a$REQ2_CARDIOMYOPATHY[which(stat1a$REQ2_CARDIOMYOPATHY == 0)] <- "0"
stat1a$REQ2_CARDIOMYOPATHY[which(stat1a$REQ2_CARDIOMYOPATHY == 1)] <- "1"
stat1a$REQ2_CARDIOMYOPATHY <- as.factor(stat1a$REQ2_CARDIOMYOPATHY)
summary(stat1a$REQ2_CARDIOMYOPATHY)

summary(stat1a)




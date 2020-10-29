data_dir = "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv"
setwd(data_dir)
require(data.table)
library(ggplot2)
stat1a = as.data.frame(fread("thoracic_stat1a.tsv"))
names(stat1a)

stat1a = stat1a[which(stat1a$AGE_GROUP == "Pediatric"), ] 

summary(stat1a)
summary(stat1a$CHD)

names(stat1a)

#Frequency Graphs for Binary Variables

par(mfrow=c(3,5))
var = names(stat1a)
for(i in 1:14) {
  barplot(table(stat1a[, var[i]]), main=var[i])
}
print(var[1:15])
#first 14 variables: all 0 or N/A
#why are there 14k+ N/As for the first 15 variables? Are variables 2-14 just for adults?

par(mfrow=c(3,4))
for(i in 15:26) {
  barplot(table(stat1a[, var[i]]), main=var[i])
  
}
print(var[15:26])

# look at other statistics
stat1a$REQ_INF_INOTROPE <- factor(stat1a$REQ_INF_INOTROPE, labels = c("0", "1"))
summary(stat1a$REQ_INF_INOTROPE)
#40.47% require infusion of a single high-dose intravenous inotrope

stat1a$REQ_MECH_ASSIST_DEV <- factor(stat1a$REQ_MECH_ASSIST_DEV, labels = c("0", "1"))
summary(stat1a$REQ_MECH_ASSIST_DEV)
#15.9% require assistance with a mechanical assist device (Ped B)


length(table(stat1a$WL_ID_CODE))
#9425 unique patients 

par(mfrow=c(1,1))
hist(table(stat1a$WL_ID_CODE), main="Histogram of number of entries per patient", xlim=c(0,100))

#congenital heart disease values
barplot(table(stat1a$CHD), main="CHD")
length(table(stat1a$CHD))
table(stat1a$CHD)
#135 different CHD codes

high_chd = table(stat1a$CHD)[which(table(stat1a$CHD) > 100)]
high_chd
## 8232 with no CHD
# 2079 with code 2 -
# 352 with code 2048
# Codes are not the best indicator of what's going on, comments might be better?


table(stat1a$CHD_OTHER_SPECIFY)
length(table(stat1a$CHD_OTHER_SPECIFY))
chd_comments <- data.frame(stat1a$CHD_OTHER_SPECIFY)
chd_classifications <- names(table(chd_comments))
chd_classifications
#184 different comments on CHO

##recoding comments that are for the same CHD condition
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Anomalous left coronary artery from the pulmonary artery")] = "ALCAPA"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Anomalous left coronary artery from the pulmonary artery - ALCAPA")] = "ALCAPA"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Anomalous origin of left coronary artery from Pulmonary artery")] = "Anomalous origin of left coronary artery from pulmonary artery"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Tricuspid atresia")] = "Tricuspid Atresia"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "tricuspid atresia")] = "Tricuspid Atresia"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Shone Complex")] = "Shone's Complex"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Shone's complex")] = "Shone's Complex"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Shones's complex")] = "Shone's Complex"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "tricuspid atresa, hypoplastic RV, small VSD, s/p BTS, BDG, & lateral tunneled Fontan, failing Fonta")] = "tricuspid atresia, hypoplastic RV, small VSD, s/p BTS, BDG & lateral tunneled Fontan, failing Fontan"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "pulmonary atresia")] = "Pulmonary atresia"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "PA")] = "Pulmonary atresia"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "mitral atresia")] = "Mitral atresia"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Marfan's Syndrome")] = "Marfan Syndrome"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "LVOT obstruction")] = "LVOT Obstruction"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "LV non-compaction cardiomyopathy")] = "LV Non-Compaction Cardiomyopathy"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Left Ventricular Noncompaction, Aortopulmonary window")] = "Left Ventricular Noncompaction, Aortopulmonary Window"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Hypoplastic Right Heart")] = "Hypoplastic Right Heart Syndrome"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Hypoplastic right heart")] = "Hypoplastic Right Heart Syndrome"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Double inlet left ventricle")] = "Double Inlet Left Ventricle"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "double inlet left ventricle")] = "Double Inlet Left Ventricle"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Dilated cardiomyopathy")] = "Dilated Cardiomyopathy"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "critical aortic stenosis")] = "Critical Aortic Stenosis"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "CRITICAL AORTIC STENOSIS")] = "Critical Aortic Stenosis"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Critical aortic stenosis")] = "Critical Aortic Stenosis"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Complete Atrioventricular Canal defect, Dysplastic L AV valve, Secundum ASD, Bicuspid Aortic Valve")] = "Complete Atrioventricular Canal defect, Dysplastic left AV valve, Secundum ASD,Bicuspid Aortic Valve"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Complete Atrioventricular Canal defect, Dysplastic left AV valve, secundum ASD, Bicuspid Aortic valv")] = "Complete Atrioventricular Canal defect, Dysplastic left AV valve, Secundum ASD,Bicuspid Aortic Valve"
stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "Complete Atrioventricular Canal defect; Dysplastic AV valve, Secundum ASD, Bicuspid Aortic Valve")] = "Complete Atrioventricular Canal defect, Dysplastic left AV valve, Secundum ASD,Bicuspid Aortic Valve"

stat1a$CHD_OTHER_SPECIFY[which(stat1a$CHD_OTHER_SPECIFY == "CARDIO MYOPATHY")] = "Cardiomyopathy of an infant"

#after recoding of similar things
length(table(stat1a$CHD_OTHER_SPECIFY))
table(stat1a$CHD_OTHER_SPECIFY)
chd_comments <- data.frame(stat1a$CHD_OTHER_SPECIFY)
chd_classifications <- names(table(chd_comments))
chd_classifications
#after recoding, still 157 different comments
#are the comments or the codes better to use? and if so what do some of them mean (2)?

#req4 congenital heart disease & iv inotrope (binary variable)
barplot(table(stat1a$REQ4_CHD_DIAGNOSIS), main="CHD")
length(table(stat1a$REQ4_CHD_DIAGNOSIS))
table(stat1a$REQ4_CHD_DIAGNOSIS)
# ~11% fit req 4 condition

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

length(which(apply(stat1a_mult[[2]], 2, function(x) length(unique(x))) != 1))

test <- stat1a_mult[[1]]
class(test)
stat1a_mult_change <- list()
for(i in 1:6809) {
  len = length(which(apply(stat1a_mult[[i]], 2, function(x) length(unique(x))) != 1))
  if(len != 2){
    stat1a_mult_change = append(stat1a_mult_change, stat1a_mult[i])
  }
}
# ~2500 patients that have multiple timestamps had changes in values
# some of these changes might be attributed to coding of 0 vs. N/A in possible adult columns so this value might be smaller later

#next steps: look at specific variables that have large differences between patients
# inotropes, CHD, etc.





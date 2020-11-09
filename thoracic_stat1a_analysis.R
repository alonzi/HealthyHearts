data_dir = "C:/Users/student/Documents/UVA/4thYear/Capstone/Data/STAR-tsv"
setwd(data_dir)
require(data.table)
library(ggplot2)
stat1a = as.data.frame(fread("thoracic_stat1a.tsv"))
names(stat1a)

stat1a = stat1a[which(stat1a$AGE_GROUP == "Pediatric"), ] 

summary(stat1a)

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

# Recoding first 14 variables (all either 0 or N/A)
summary(stat1a$MECH_CIRC_ACUTE)
stat1a$MECH_CIRC_ACUTE[which(stat1a$MECH_CIRC_ACUTE == 0)] <- "0"
stat1a$MECH_CIRC_ACUTE <- as.factor(stat1a$MECH_CIRC_ACUTE)
summary(stat1a$MECH_CIRC_ACUTE)

stat1a$MECH_CIRC_ACUTE_VAD[which(stat1a$MECH_CIRC_ACUTE_VAD == 0)] <- "0"
stat1a$MECH_CIRC_ACUTE_VAD <- as.factor(stat1a$MECH_CIRC_ACUTE_VAD)
summary(stat1a$MECH_CIRC_ACUTE_VAD)

stat1a$TAH[which(stat1a$TAH == 0)] <- "0"
stat1a$TAH <- as.factor(stat1a$TAH)
summary(stat1a$TAH)

stat1a$IAB_PUMP[which(stat1a$IAB_PUMP == 0)] <- "0"
stat1a$IAB_PUMP <- as.factor(stat1a$IAB_PUMP)
summary(stat1a$IAB_PUMP)

stat1a$EMO[which(stat1a$EMO == 0)] <- "0"
stat1a$EMO <- as.factor(stat1a$EMO)
summary(stat1a$EMO)

stat1a$MECH_CIRC_30DAYS[which(stat1a$MECH_CIRC_30DAYS == 0)] <- "0"
stat1a$MECH_CIRC_30DAYS <- as.factor(stat1a$MECH_CIRC_30DAYS)
summary(stat1a$MECH_CIRC_30DAYS)

stat1a$THROMB[which(stat1a$THROMB == 0)] <- "0"
stat1a$THROMB <- as.factor(stat1a$THROMB)
summary(stat1a$THROMB)

stat1a$DEVICE_INFECT[which(stat1a$DEVICE_INFECT == 0)] <- "0"
stat1a$DEVICE_INFECT <- as.factor(stat1a$DEVICE_INFECT)
summary(stat1a$DEVICE_INFECT)

stat1a$DEV_MALFUNCT[which(stat1a$DEV_MALFUNCT == 0)] <- "0"
stat1a$DEV_MALFUNCT <- as.factor(stat1a$DEV_MALFUNCT)
summary(stat1a$DEV_MALFUNCT)

stat1a$LIFE_THREAT_VENT_ARRHY[which(stat1a$LIFE_THREAT_VENT_ARRHY == 0)] <- "0"
stat1a$LIFE_THREAT_VENT_ARRHY <- as.factor(stat1a$LIFE_THREAT_VENT_ARRHY)
summary(stat1a$LIFE_THREAT_VENT_ARRHY)

stat1a$OTHER_DEV_THERAPY[which(stat1a$OTHER_DEV_THERAPY == 0)] <- "0"
stat1a$OTHER_DEV_THERAPY <- as.factor(stat1a$OTHER_DEV_THERAPY)
summary(stat1a$OTHER_DEV_THERAPY)

stat1a$MECH_VENT[which(stat1a$MECH_VENT == 0)] <- "0"
stat1a$MECH_VENT <- as.factor(stat1a$MECH_VENT)
summary(stat1a$MECH_VENT)

stat1a$CONT_INFUS[which(stat1a$CONT_INFUS == 0)] <- "0"
stat1a$CONT_INFUS <- as.factor(stat1a$CONT_INFUS)
summary(stat1a$CONT_INFUS)


## Barplots of vars 15 - 26
par(mfrow=c(3,4))
for(i in 15:26) {
  barplot(table(stat1a[, var[i]]), main=var[i])
  
}
print(var[15:26])


# Recoding variables 15 - 26 (all 0,1, or NA)
stat1a$EXCEPTION[which(stat1a$EXCEPTION == 0)] <- "0"
stat1a$EXCEPTION[which(stat1a$EXCEPTION == 1)] <- "1"
stat1a$EXCEPTION <- as.factor(stat1a$EXCEPTION)
summary(stat1a$EXCEPTION)

stat1a$REQ_VENTILATOR[which(stat1a$REQ_VENTILATOR == 0)] <- "0"
stat1a$REQ_VENTILATOR[which(stat1a$REQ_VENTILATOR == 1)] <- "1"
stat1a$REQ_VENTILATOR <- as.factor(stat1a$REQ_VENTILATOR)
summary(stat1a$REQ_VENTILATOR)

stat1a$REQ_MECH_ASSIST_DEV[which(stat1a$REQ_MECH_ASSIST_DEV == 0)] <- "0"
stat1a$REQ_MECH_ASSIST_DEV[which(stat1a$REQ_MECH_ASSIST_DEV == 1)] <- "1"
stat1a$REQ_MECH_ASSIST_DEV <- as.factor(stat1a$REQ_MECH_ASSIST_DEV)
summary(stat1a$REQ_MECH_ASSIST_DEV)

stat1a$REQ_BALLON_PUMP[which(stat1a$REQ_BALLON_PUMP == 0)] <- "0"
stat1a$REQ_BALLON_PUMP[which(stat1a$REQ_BALLON_PUMP == 1)] <- "1"
stat1a$REQ_BALLON_PUMP <- as.factor(stat1a$REQ_BALLON_PUMP)
summary(stat1a$REQ_BALLON_PUMP)

stat1a$LESS_6M_HRT_DISEASE[which(stat1a$LESS_6M_HRT_DISEASE == 0)] <- "0"
stat1a$LESS_6M_HRT_DISEASE[which(stat1a$LESS_6M_HRT_DISEASE == 1)] <- "1"
stat1a$LESS_6M_HRT_DISEASE <- as.factor(stat1a$LESS_6M_HRT_DISEASE)
summary(stat1a$LESS_6M_HRT_DISEASE)

stat1a$REQ_INF_INOTROPE[which(stat1a$REQ_INF_INOTROPE == 0)] <- "0"
stat1a$REQ_INF_INOTROPE[which(stat1a$REQ_INF_INOTROPE == 1)] <- "1"
stat1a$REQ_INF_INOTROPE <- as.factor(stat1a$REQ_INF_INOTROPE)
summary(stat1a$REQ_INF_INOTROPE)

stat1a$REQ1_CONTINUOUS_VENTILATION[which(stat1a$REQ1_CONTINUOUS_VENTILATION == 0)] <- "0"
stat1a$REQ1_CONTINUOUS_VENTILATION[which(stat1a$REQ1_CONTINUOUS_VENTILATION == 1)] <- "1"
stat1a$REQ1_CONTINUOUS_VENTILATION <- as.factor(stat1a$REQ1_CONTINUOUS_VENTILATION)
summary(stat1a$REQ1_CONTINUOUS_VENTILATION)

stat1a$REQ2_BALLOONPUMP[which(stat1a$REQ2_BALLOONPUMP == 0)] <- "0"
stat1a$REQ2_BALLOONPUMP[which(stat1a$REQ2_BALLOONPUMP == 1)] <- "1"
stat1a$REQ2_BALLOONPUMP <- as.factor(stat1a$REQ2_BALLOONPUMP)
summary(stat1a$REQ2_BALLOONPUMP)

stat1a$REQ3_DUCTAL_DEPENDENCY[which(stat1a$REQ3_DUCTAL_DEPENDENCY == 0)] <- "0"
stat1a$REQ3_DUCTAL_DEPENDENCY[which(stat1a$REQ3_DUCTAL_DEPENDENCY == 1)] <- "1"
stat1a$REQ3_DUCTAL_DEPENDENCY <- as.factor(stat1a$REQ3_DUCTAL_DEPENDENCY)
summary(stat1a$REQ3_DUCTAL_DEPENDENCY)

stat1a$REQ4_CHD_DIAGNOSIS[which(stat1a$REQ4_CHD_DIAGNOSIS == 0)] <- "0"
stat1a$REQ4_CHD_DIAGNOSIS[which(stat1a$REQ4_CHD_DIAGNOSIS == 1)] <- "1"
stat1a$REQ4_CHD_DIAGNOSIS <- as.factor(stat1a$REQ4_CHD_DIAGNOSIS)
summary(stat1a$REQ4_CHD_DIAGNOSIS)

stat1a$REQ5_CIRCULATORY_DEVICE[which(stat1a$REQ5_CIRCULATORY_DEVICE == 0)] <- "0"
stat1a$REQ5_CIRCULATORY_DEVICE[which(stat1a$REQ5_CIRCULATORY_DEVICE == 1)] <- "1"
stat1a$REQ5_CIRCULATORY_DEVICE <- as.factor(stat1a$REQ5_CIRCULATORY_DEVICE)
summary(stat1a$REQ5_CIRCULATORY_DEVICE)

stat1a$REQ6_CRITERIA_NOT_MET[which(stat1a$REQ6_CRITERIA_NOT_MET == 0)] <- "0"
stat1a$REQ6_CRITERIA_NOT_MET[which(stat1a$REQ6_CRITERIA_NOT_MET == 1)] <- "1"
stat1a$REQ6_CRITERIA_NOT_MET <- as.factor(stat1a$REQ6_CRITERIA_NOT_MET)
summary(stat1a$REQ6_CRITERIA_NOT_MET)


# look at other statistics
summary(stat1a$REQ_INF_INOTROPE)
#40.47% require infusion of a single high-dose intravenous inotrope

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

summary(stat1a)



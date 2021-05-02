#this file retrieves the min and max values of each variable, 
#and also the min and max values broken down by accepted and rejected donors

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")
library(ggplot2)
library(tidyverse)


data = read_excel("full7100withduration.xlsx")
nonaccptdata = data[data$Accepted == 'No',]
accptdata = data[data$Accepted == 'Yes',]

#--------------------------------------------------------------#
#
# Overall Extreme Values
#
#--------------------------------------------------------------#
donor.extremes = 
  data.frame(
    "Name" = "ABG_PH",
    "Max of Accepted" = max(accptdata$ABG_PH, na.rm=T),
    "Max of Non-Accepted" = max(nonaccptdata$ABG_PH, na.rm=T),
    "Min of Accepted" = min(accptdata$ABG_PH, na.rm=T),
    "Min of Non-Accepted" = min(nonaccptdata$ABG_PH, na.rm=T),
    "Units" = "pH"
  )
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "PAO2",
              "Max of Accepted" = max(as.numeric(accptdata$PAO2), na.rm=T),
              "Max of Non-Accepted" = max(as.numeric(nonaccptdata$PAO2), na.rm=T),
              "Min of Accepted" = min(as.numeric(accptdata$PAO2), na.rm=T),
              "Min of Non-Accepted" = min(as.numeric(nonaccptdata$PAO2), na.rm=T),
              "Units" = "mmHg"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "PEEP",
                      "Max of Accepted" = max(as.numeric(accptdata$PEEP), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$PEEP), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$PEEP), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$PEEP), na.rm=T),
                      "Units" = "cmH2O"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "HGB",
                      "Max of Accepted" = max(as.numeric(accptdata$HGB), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$HGB), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$HGB), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$HGB), na.rm=T),
                      "Units" = "g/dL"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "HCT",
                      "Max of Accepted" = max(as.numeric(accptdata$HCT), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$HCT), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$HCT), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$HCT), na.rm=T),
                      "Units" = "%"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "AVG_BP_SYST",
                      "Max of Accepted" = max(as.numeric(accptdata$AVG_BP_SYST), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$AVG_BP_SYST), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$AVG_BP_SYST), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$AVG_BP_SYST), na.rm=T),
                      "Units" = "mmHg"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "AVG_BP_DIAST",
                      "Max of Accepted" = max(as.numeric(accptdata$AVG_BP_DIAST), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$AVG_BP_DIAST), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$AVG_BP_DIAST), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$AVG_BP_DIAST), na.rm=T),
                      "Units" = "mmHg"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "AVG_PULSE_RANGE_START",
                      "Max of Accepted" = max(as.numeric(accptdata$AVG_PULSE_RANGE_START), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$AVG_PULSE_RANGE_START), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$AVG_PULSE_RANGE_START), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$AVG_PULSE_RANGE_START), na.rm=T),
                      "Units" = "bpm"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "CVP_INT_RANGE_START",
                      "Max of Accepted" = max(as.numeric(accptdata$CVP_INT_RANGE_START), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$CVP_INT_RANGE_START), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$CVP_INT_RANGE_START), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$CVP_INT_RANGE_START), na.rm=T),
                      "Units" = "cmH2O"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "BODYTEMP_RANGE_START",
                      "Max of Accepted" = max(as.numeric(accptdata$BODYTEMP_RANGE_START), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$BODYTEMP_RANGE_START), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$BODYTEMP_RANGE_START), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$BODYTEMP_RANGE_START), na.rm=T),
                      "Units" = "C"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "SGOT",
                      "Max of Accepted" = max(as.numeric(accptdata$SGOT), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$SGOT), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$SGOT), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$SGOT), na.rm=T),
                      "Units" = "u/L"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "SGPT",
                      "Max of Accepted" = max(as.numeric(accptdata$SGPT), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$SGPT), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$SGPT), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$SGPT), na.rm=T),
                      "Units" = "u/L"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "SODIUM170",
                      "Max of Accepted" = max(as.numeric(accptdata$SODIUM170), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$SODIUM170), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$SODIUM170), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$SODIUM170), na.rm=T),
                      "Units" = "mmEq/L"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "CREATININE",
                      "Max of Accepted" = max(as.numeric(accptdata$CREATININE), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$CREATININE), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$CREATININE), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$CREATININE), na.rm=T),
                      "Units" = "mg/dL"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "POTASSIUM",
                      "Max of Accepted" = max(as.numeric(accptdata$POTASSIUM), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$POTASSIUM), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$POTASSIUM), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$POTASSIUM), na.rm=T),
                      "Units" = "mmol/L"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "BILIRUBIN",
                      "Max of Accepted" = max(as.numeric(accptdata$BILIRUBIN), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$BILIRUBIN), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$BILIRUBIN), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$BILIRUBIN), na.rm=T),
                      "Units" = "mg/dL"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "BILIRUBIN_INDIRECT",
                      "Max of Accepted" = max(as.numeric(accptdata$BILIRUBIN_INDIRECT), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$BILIRUBIN_INDIRECT), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$BILIRUBIN_INDIRECT), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$BILIRUBIN_INDIRECT), na.rm=T),
                      "Units" = "mg/dL"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "PROTHROMBIN",
                      "Max of Accepted" = max(as.numeric(accptdata$PROTHROMBIN), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$PROTHROMBIN), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$PROTHROMBIN), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$PROTHROMBIN), na.rm=T),
                      "Units" = "s"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "INR",
                      "Max of Accepted" = max(as.numeric(accptdata$INR), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$INR), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$INR), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$INR), na.rm=T),
                      "Units" = "INR"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "CKMB",
                      "Max of Accepted" = max(as.numeric(accptdata$CKMB), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$CKMB), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$CKMB), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$CKMB), na.rm=T),
                      "Units" = "ng/mL"
         ))
donor.extremes =
  rbind( donor.extremes,
         data.frame(  "Name" = "TROPONINI",
                      "Max of Accepted" = max(as.numeric(accptdata$TROPONINI), na.rm=T),
                      "Max of Non-Accepted" = max(as.numeric(nonaccptdata$TROPONINI), na.rm=T),
                      "Min of Accepted" = min(as.numeric(accptdata$TROPONINI), na.rm=T),
                      "Min of Non-Accepted" = min(as.numeric(nonaccptdata$TROPONINI), na.rm=T),
                      "Units" = "ng/mL"
         ))

#--------------------------------------------------------------#
#
# Non-Accepted Donor Extreme Values
#
#--------------------------------------------------------------#

unqlistn = unique(nonaccptdata[c("DONOR_ID")])
nonaccptdata.donor.extremes = rep(NA,43)

for (id in unqlistn$DONOR_ID) {
  nonaccptdata.donor.extremes = 
    rbind( nonaccptdata.donor.extremes,
      data.frame(
        "Donor ID" = id,
        "Max pH" = max(as.numeric(nonaccptdata$ABG_PH[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min pH" = min(as.numeric(nonaccptdata$ABG_PH[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max PAO2" = max(as.numeric(nonaccptdata$PAO2[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min PAO2" = min(as.numeric(nonaccptdata$PAO2[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max PEEP" = max(as.numeric(nonaccptdata$PEEP[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min PEEP" = min(as.numeric(nonaccptdata$PEEP[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max HGB" = max(as.numeric(nonaccptdata$HGB[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min HGB" = min(as.numeric(nonaccptdata$HGB[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max HCT" = max(as.numeric(nonaccptdata$HCT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min HCT" = min(as.numeric(nonaccptdata$HCT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max AVG_BP_SYST" = max(as.numeric(nonaccptdata$AVG_BP_SYST[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min AVG_BP_SYST" = min(as.numeric(nonaccptdata$AVG_BP_SYST[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max AVG_BP_DIAST" = max(as.numeric(nonaccptdata$AVG_BP_DIAST[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min AVG_BP_DIAST" = min(as.numeric(nonaccptdata$AVG_BP_DIAST[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max AVG_PULSE_RANGE_START" = max(as.numeric(nonaccptdata$AVG_PULSE_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min AVG_PULSE_RANGE_START" = min(as.numeric(nonaccptdata$AVG_PULSE_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max CVP_INT_RANGE_START" = max(as.numeric(nonaccptdata$CVP_INT_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min CVP_INT_RANGE_START" = min(as.numeric(nonaccptdata$CVP_INT_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max BODYTEMP_RANGE_START" = max(as.numeric(nonaccptdata$BODYTEMP_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min BODYTEMP_RANGE_START" = min(as.numeric(nonaccptdata$BODYTEMP_RANGE_START[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max SGOT" = max(as.numeric(nonaccptdata$SGOT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min SGOT" = min(as.numeric(nonaccptdata$SGOT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max SGPT" = max(as.numeric(nonaccptdata$SGPT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min SGPT" = min(as.numeric(nonaccptdata$SGPT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max SODIUM170" = max(as.numeric(nonaccptdata$SODIUM170[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min SODIUM170" = min(as.numeric(nonaccptdata$SODIUM170[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max CREATININE" = max(as.numeric(nonaccptdata$CREATININE[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min CREATININE" = min(as.numeric(nonaccptdata$CREATININE[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max POTASSIUM" = max(as.numeric(nonaccptdata$POTASSIUM[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min POTASSIUM" = min(as.numeric(nonaccptdata$POTASSIUM[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max BILIRUBIN" = max(as.numeric(nonaccptdata$BILIRUBIN[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min BILIRUBIN" = min(as.numeric(nonaccptdata$BILIRUBIN[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max BILIRUBIN_INDIRECT" = max(as.numeric(nonaccptdata$BILIRUBIN_INDIRECT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min BILIRUBIN_INDIRECT" = min(as.numeric(nonaccptdata$BILIRUBIN_INDIRECT[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max PROTHROMBIN" = max(as.numeric(nonaccptdata$PROTHROMBIN[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min PROTHROMBIN" = min(as.numeric(nonaccptdata$PROTHROMBIN[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max INR" = max(as.numeric(nonaccptdata$INR[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min INR" = min(as.numeric(nonaccptdata$INR[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max CKMB" = max(as.numeric(nonaccptdata$CKMB[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min CKMB" = min(as.numeric(nonaccptdata$CKMB[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Max TROPONINI" = max(as.numeric(nonaccptdata$TROPONINI[which(nonaccptdata$DONOR_ID == id)]), na.rm=T),
        "Min TROPONINI" = min(as.numeric(nonaccptdata$TROPONINI[which(nonaccptdata$DONOR_ID == id)]), na.rm=T)
      ))
}
nonaccptdata.donor.extremes = nonaccptdata.donor.extremes[-1,]

#--------------------------------------------------------------#
#
# Accepted Donor Extreme Values
#
#--------------------------------------------------------------#

unqlista = unique(accptdata[c("DONOR_ID")])
accptdata.donor.extremes = rep(NA,43)

for (id in unqlista$DONOR_ID) {
  accptdata.donor.extremes = 
    rbind( accptdata.donor.extremes,
           data.frame(
             "Donor ID" = id,
             "Max pH" = max(as.numeric(accptdata$ABG_PH[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min pH" = min(as.numeric(accptdata$ABG_PH[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max PAO2" = max(as.numeric(accptdata$PAO2[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min PAO2" = min(as.numeric(accptdata$PAO2[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max PEEP" = max(as.numeric(accptdata$PEEP[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min PEEP" = min(as.numeric(accptdata$PEEP[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max HGB" = max(as.numeric(accptdata$HGB[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min HGB" = min(as.numeric(accptdata$HGB[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max HCT" = max(as.numeric(accptdata$HCT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min HCT" = min(as.numeric(accptdata$HCT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max AVG_BP_SYST" = max(as.numeric(accptdata$AVG_BP_SYST[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min AVG_BP_SYST" = min(as.numeric(accptdata$AVG_BP_SYST[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max AVG_BP_DIAST" = max(as.numeric(accptdata$AVG_BP_DIAST[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min AVG_BP_DIAST" = min(as.numeric(accptdata$AVG_BP_DIAST[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max AVG_PULSE_RANGE_START" = max(as.numeric(accptdata$AVG_PULSE_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min AVG_PULSE_RANGE_START" = min(as.numeric(accptdata$AVG_PULSE_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max CVP_INT_RANGE_START" = max(as.numeric(accptdata$CVP_INT_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min CVP_INT_RANGE_START" = min(as.numeric(accptdata$CVP_INT_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max BODYTEMP_RANGE_START" = max(as.numeric(accptdata$BODYTEMP_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min BODYTEMP_RANGE_START" = min(as.numeric(accptdata$BODYTEMP_RANGE_START[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max SGOT" = max(as.numeric(accptdata$SGOT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min SGOT" = min(as.numeric(accptdata$SGOT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max SGPT" = max(as.numeric(accptdata$SGPT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min SGPT" = min(as.numeric(accptdata$SGPT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max SODIUM170" = max(as.numeric(accptdata$SODIUM170[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min SODIUM170" = min(as.numeric(accptdata$SODIUM170[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max CREATININE" = max(as.numeric(accptdata$CREATININE[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min CREATININE" = min(as.numeric(accptdata$CREATININE[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max POTASSIUM" = max(as.numeric(accptdata$POTASSIUM[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min POTASSIUM" = min(as.numeric(accptdata$POTASSIUM[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max BILIRUBIN" = max(as.numeric(accptdata$BILIRUBIN[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min BILIRUBIN" = min(as.numeric(accptdata$BILIRUBIN[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max BILIRUBIN_INDIRECT" = max(as.numeric(accptdata$BILIRUBIN_INDIRECT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min BILIRUBIN_INDIRECT" = min(as.numeric(accptdata$BILIRUBIN_INDIRECT[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max PROTHROMBIN" = max(as.numeric(accptdata$PROTHROMBIN[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min PROTHROMBIN" = min(as.numeric(accptdata$PROTHROMBIN[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max INR" = max(as.numeric(accptdata$INR[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min INR" = min(as.numeric(accptdata$INR[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max CKMB" = max(as.numeric(accptdata$CKMB[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min CKMB" = min(as.numeric(accptdata$CKMB[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Max TROPONINI" = max(as.numeric(accptdata$TROPONINI[which(accptdata$DONOR_ID == id)]), na.rm=T),
             "Min TROPONINI" = min(as.numeric(accptdata$TROPONINI[which(accptdata$DONOR_ID == id)]), na.rm=T)
           ))
}
accptdata.donor.extremes = accptdata.donor.extremes[-1,]


#---------------------------------------------------------------------------#
# Function extval_info
# takes in data, variable name, min normal range value, & max normal range value
# prints box plot and gets average maximum and minimum value and % of donors 
# with max and min within normal ranges
#---------------------------------------------------------------------------#

extval_info <- function(d, var_name, min_range_val, max_range_val) {
  #removing n/a vals 
  data_var = d[!is.na(d[var_name]),]
  
  #separating accepted and not accepted data
  data_accepted = data_var[which(data_var$Accepted == "Yes"), ] 
  data_non_accepted = data_var[which(data_var$Accepted == "No"),]
  
  #getting donor IDs for accepted & not accepted
  data_var_names_acc = as.array(names(table(data_accepted$DONOR_ID)))
  data_var_names_nonacc = as.array(names(table(data_non_accepted$DONOR_ID)))
  
  #getting maximum variable value for each donor for accepted and not accepted
  group <- as.data.table(data_accepted)
  max_values_acc = group[group[, .I[which.max(var_name)], by=DONOR_ID]$V1]
  max_values_acc = max_values_acc[[var_name]]
  
  group <- as.data.table(data_non_accepted)
  max_values_nonacc = group[group[, .I[which.max(var_name)], by=DONOR_ID]$V1]
  max_values_nonacc = max_values_nonacc[[var_name]]
  
  #getting minimum variable value for each donor for accepted and not accepted
  group <- as.data.table(data_accepted)
  min_values_acc = group[group[, .I[which.min(var_name)], by=DONOR_ID]$V1]
  min_values_acc = min_values_acc[[var_name]]
  
  group <- as.data.table(data_non_accepted)
  min_values_nonacc = group[group[, .I[which.min(var_name)], by=DONOR_ID]$V1]
  min_values_nonacc = min_values_nonacc[[var_name]]
  
  #boxplot of max variable value for variable for both accepted and not accepted
  boxplot(max_values_acc, max_values_nonacc, names=c("Accepted", "Not Accepted"), main=var_name)
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #boxplot of min variable value for variable for both accepted and not accepted
  boxplot(min_values_acc, min_values_nonacc, names=c("Accepted", "Not Accepted"), main=var_name)
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #getting average max value for accepted and non accepted
  mean_max_info = data.frame("Average Max Value", round(mean(max_values_acc), digits=2), round(mean(max_values_nonacc), digits=2))
  
  #getting average min value for accepted and non accepted
  mean_min_info = data.frame("Average Min Value", round(mean(min_values_acc), digits=2), round(mean(min_values_nonacc), digits=2))
  
  #getting percent of accepted donors with max within normal range
  max_values_acc = as.data.frame(max_values_acc)
  names(max_values_acc)[1] <- "values"
  
  total_num_max_acc = nrow(max_values_acc)
  if(!is.null(max_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values <= max_range_val),])
    names(max_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values >= min_range_val),])
  }
  num_max_acc = nrow(max_values_acc)
  perc_max_acc = num_max_acc / total_num_max_acc * 100
  perc_max_acc = paste(round(perc_max_acc, digits=2), "%")
  
  #getting percent of accepted donors with min within normal range
  min_values_acc = as.data.frame(min_values_acc)
  names(min_values_acc)[1] <- "values"
  
  total_num_min_acc = nrow(min_values_acc)
  if(!is.null(max_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values <= max_range_val),])
    names(min_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values >= min_range_val),])
  }
  num_min_acc = nrow(min_values_acc)
  perc_min_acc = num_min_acc / total_num_min_acc * 100
  perc_min_acc = paste(round(perc_min_acc, digits=2), "%")
  
  #getting percent of not accepted donors with max within normal range
  max_values_nonacc = as.data.frame(max_values_nonacc)
  names(max_values_nonacc)[1] <- "values"
  
  total_num_max_nonacc = nrow(max_values_nonacc)
  if(!is.null(max_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values <= max_range_val),])
    names(max_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values >= min_range_val),])
  }
  num_max_nonacc = nrow(max_values_nonacc)
  perc_max_nonacc = num_max_nonacc / total_num_max_nonacc * 100
  perc_max_nonacc = paste(round(perc_max_nonacc, digits=2), "%")
  
  #getting percent of not accepted donors with min within normal range
  min_values_nonacc = as.data.frame(min_values_nonacc)
  names(min_values_nonacc)[1] <- "values"
  
  total_num_min_nonacc = nrow(min_values_nonacc)
  if(!is.null(max_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values <= max_range_val),])
    names(min_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values >= min_range_val),])
  }
  num_min_nonacc = nrow(min_values_nonacc)
  perc_min_nonacc = num_min_nonacc / total_num_min_nonacc * 100
  perc_min_nonacc = paste(round(perc_min_nonacc, digits=2), "%")
  
  #arranging stats in table  
  range_max_info = data.frame("% of Donors with Max within Range", perc_max_acc, perc_max_nonacc)
  range_min_info = data.frame("% of Donors with Min within Range", perc_min_acc, perc_min_nonacc)
  names(mean_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(mean_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  var_info = rbind(mean_max_info, mean_min_info, range_max_info, range_min_info)
  print(var_info)
}

extval_info(data.after.BD, "ABG_PH", 7.35, 7.45)



mean(1:3)
(1:3) %>% mean()


testframe = data.after.BD[which((data.after.BD$DONOR_ID == 377913) | (data.after.BD$DONOR_ID == 344096)) ,]
summary(testframe$DONOR_ID)

foo <- function(d, var_name, var_min, var_max){
  var = enquo(var_name)
  
  X = d %>% 
  group_by(DONOR_ID) %>% 
  dplyr::summarize(Accepted = Accepted[1], 
                   min = min(!!var, na.rm=TRUE), 
                   max = max(!!var, na.rm=TRUE) ) #%>% 
  #mutate_at(c(min, max), function(x) ifelse(is.finite(x), x, NA))
  
  Y = ...
}

foo(data.after.BD, ABG_PH)

########################TEST OF EXTVAL#######################################
extval_info <- function(d, var_name, min_range_val, max_range_val) {
  #d=testframe, var_name="ABG_PH", min_range_val=7.35, max_range_val=7.45
  
  #removing n/a vals 
  data_var = testframe[!is.na(testframe[var_name]),]
  
  #separating accepted and not accepted data
  data_accepted = data_var[which(data_var$Accepted == "Yes"), ] 
  data_non_accepted = data_var[which(data_var$Accepted == "No"),]
  
  #getting donor IDs for accepted & not accepted
  data_var_names_acc = as.array(names(table(data_accepted$DONOR_ID)))
  data_var_names_nonacc = as.array(names(table(data_non_accepted$DONOR_ID)))
  summary(data_accepted$DONOR_ID)
  
  #getting maximum variable value for each donor for accepted and not accepted
  group <- as.data.table(data_accepted)
  max_values_acc = group[group[, .I[which.max(var_name)], by=DONOR_ID]$V1]
  max_values_acc = max_values_acc[[var_name]]
  
  group <- as.data.table(data_non_accepted)
  max_values_nonacc = group[group[, .I[which.max(var_name)], by=DONOR_ID]$V1]
  max_values_nonacc = max_values_nonacc[[var_name]]
  
  #getting minimum variable value for each donor for accepted and not accepted
  group <- as.data.table(data_accepted)
  min_values_acc = group[group[, .I[which.min(var_name)], by=DONOR_ID]$V1]
  min_values_acc = min_values_acc[[var_name]]
  
  group <- as.data.table(data_non_accepted)
  min_values_nonacc = group[group[, .I[which.min(var_name)], by=DONOR_ID]$V1]
  min_values_nonacc = min_values_nonacc[[var_name]]
  
  #boxplot of max variable value for variable for both accepted and not accepted
  boxplot(max_values_acc, max_values_nonacc, names=c("Accepted", "Not Accepted"), main=var_name)
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #boxplot of min variable value for variable for both accepted and not accepted
  boxplot(min_values_acc, min_values_nonacc, names=c("Accepted", "Not Accepted"), main=var_name)
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #getting average max value for accepted and non accepted
  mean_max_info = data.frame("Average Max Value", round(mean(max_values_acc), digits=2), round(mean(max_values_nonacc), digits=2))
  
  #getting average min value for accepted and non accepted
  mean_min_info = data.frame("Average Min Value", round(mean(min_values_acc), digits=2), round(mean(min_values_nonacc), digits=2))
  
  #getting percent of accepted donors with max within normal range
  max_values_acc = as.data.frame(max_values_acc)
  names(max_values_acc)[1] <- "values"
  
  total_num_max_acc = nrow(max_values_acc)
  if(!is.null(max_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values <= max_range_val),])
    names(max_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values >= min_range_val),])
  }
  num_max_acc = nrow(max_values_acc)
  perc_max_acc = num_max_acc / total_num_max_acc * 100
  perc_max_acc = paste(round(perc_max_acc, digits=2), "%")
  
  #getting percent of accepted donors with min within normal range
  min_values_acc = as.data.frame(min_values_acc)
  names(min_values_acc)[1] <- "values"
  
  total_num_min_acc = nrow(min_values_acc)
  if(!is.null(max_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values <= max_range_val),])
    names(min_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values >= min_range_val),])
  }
  num_min_acc = nrow(min_values_acc)
  perc_min_acc = num_min_acc / total_num_min_acc * 100
  perc_min_acc = paste(round(perc_min_acc, digits=2), "%")
  
  #getting percent of not accepted donors with max within normal range
  max_values_nonacc = as.data.frame(max_values_nonacc)
  names(max_values_nonacc)[1] <- "values"
  
  total_num_max_nonacc = nrow(max_values_nonacc)
  if(!is.null(max_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values <= max_range_val),])
    names(max_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values >= min_range_val),])
  }
  num_max_nonacc = nrow(max_values_nonacc)
  perc_max_nonacc = num_max_nonacc / total_num_max_nonacc * 100
  perc_max_nonacc = paste(round(perc_max_nonacc, digits=2), "%")
  
  #getting percent of not accepted donors with min within normal range
  min_values_nonacc = as.data.frame(min_values_nonacc)
  names(min_values_nonacc)[1] <- "values"
  
  total_num_min_nonacc = nrow(min_values_nonacc)
  if(!is.null(max_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values <= max_range_val),])
    names(min_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values >= min_range_val),])
  }
  num_min_nonacc = nrow(min_values_nonacc)
  perc_min_nonacc = num_min_nonacc / total_num_min_nonacc * 100
  perc_min_nonacc = paste(round(perc_min_nonacc, digits=2), "%")
  
  #arranging stats in table  
  range_max_info = data.frame("% of Donors with Max within Range", perc_max_acc, perc_max_nonacc)
  range_min_info = data.frame("% of Donors with Min within Range", perc_min_acc, perc_min_nonacc)
  names(mean_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(mean_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  var_info = rbind(mean_max_info, mean_min_info, range_max_info, range_min_info)
  print(var_info)
}
extval_info(data.after.BD, "ABG_PH", 7.35, 7.45)

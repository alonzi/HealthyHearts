data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")

panels = read_excel("DonorNet LabPanels.xlsx")
values = read_excel("DonorNet LabValues.xlsx")

library(ggplot2)

#panels data
v3 = panels$V3
v4 = panels$V4
v5 = panels$V5
v6 = panels$V6
v7 = panels$V7
v8 = panels$V8
v9 = panels$V9
v10 = panels$V10
v11 = panels$V11
#repeated process: get rid of the NAs, change strings to decimals, remove the heading
v3 = na.omit(v3)
v3 = as.numeric(v3)
v3 = v3[2:length(v3)]
v4 = na.omit(v4)
v4 = as.numeric(v4)
v4 = v4[2:length(v4)]
v5 = na.omit(v5)
v5 = as.numeric(v5)
v5 = v5[2:length(v5)]
v6 = na.omit(v6)
v6 = as.numeric(v6)
v6 = v6[2:length(v6)]
v7 = na.omit(v7)
v7 = as.numeric(v7)
v7 = v7[2:length(v7)]
v8 = na.omit(v8)
v8 = as.numeric(v8)
v8 = v8[2:length(v8)]
v9 = na.omit(v9)
v9 = as.numeric(v9)
v9 = v9[2:length(v9)]
v10 = na.omit(v10)
v10 = as.numeric(v10)
v10 = v10[2:length(v10)]
v11 = na.omit(v11)
v11 = as.numeric(v11)
v11 = v11[2:length(v11)]

#histograms to visualize each variable
ggplot(as.data.frame(v3), 
       aes(x=v3)) + coord_cartesian(xlim=c(0, 1500)) +
  geom_histogram(bins = 800) +
  labs(x = "SGOT (u/L)", y = "Frequency")

ggplot(as.data.frame(v4), 
       aes(x=v4)) + coord_cartesian(xlim=c(0, 1500)) +
  geom_histogram(bins = 800) +
  labs(x = "SGPT (u/L)", y = "Frequency")

ggplot(as.data.frame(v5), 
       aes(x=v5)) + coord_cartesian(xlim=c(0, 250)) +
  geom_histogram(bins = 1600) +
  labs(x = "Sodium 170 (mmEq/L)", y = "Frequency")

ggplot(as.data.frame(v6), 
       aes(x=v6)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 800) +
  labs(x = "Creatinine (mg/dL)", y = "Frequency")

ggplot(as.data.frame(v7), 
       aes(x=v7)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Potassium (mmol/L)", y = "Frequency")

ggplot(as.data.frame(v8), 
       aes(x=v8)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Bilirubin (mg/dL)", y = "Frequency")

ggplot(as.data.frame(v9), 
       aes(x=v9)) + coord_cartesian(xlim=c(0, 10)) +
  geom_histogram(bins = 1600) +
  labs(x = "Bilirubin Indirect (mg/dL)", y = "Frequency")

ggplot(as.data.frame(v10), 
       aes(x=v10)) + coord_cartesian(xlim=c(0, 50)) +
  geom_histogram(bins = 1600) +
  labs(x = "Prothrombin (seconds)", y = "Frequency")

ggplot(as.data.frame(v11), 
       aes(x=v11)) + coord_cartesian(xlim=c(0, 5)) +
  geom_histogram(bins = 1600) +
  labs(x = "INR", y = "Frequency")




#values data
v3 = values$V3
v4 = values$V4
v5 = values$V5

#repeated process: get rid of the NAs, change strings to decimals, remove the header
v3 = na.omit(v3)
v3 = as.numeric(v3)
v3 = v3[2:length(v3)]
v4 = na.omit(v4)
v4 = as.numeric(v4)
v4 = v4[2:length(v4)]
v5 = na.omit(v5)
v5 = as.numeric(v5)
v5 = v5[2:length(v5)]

#histograms to visualize each variable
ggplot(as.data.frame(v3), 
       aes(x=v3)) + coord_cartesian(xlim=c(0, 350)) +
  geom_histogram(bins = 800) +
  labs(x = "CKMB (ng/mL)", y = "Frequency")

ggplot(as.data.frame(v4), 
       aes(x=v4))  + coord_cartesian(xlim=c(0, 40)) +
  geom_histogram(bins = 800) +
  labs(x = "Troponini (ng/mL)", y = "Frequency")

ggplot(as.data.frame(v5), 
       aes(x=v5)) + coord_cartesian(xlim=c(0, 100)) + 
  geom_histogram(bins = 800) +
  labs(x = "Troponint (ng/mL)", y = "Frequency")

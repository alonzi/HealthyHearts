data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library(openxlsx)

#DonorNet LabPanels data
panels = read.table(file = "Donornet-Shared/Donornet-Shared/LabPanels.tsv.gz")

#dividing data into columns to remove NAs
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

# adding the variable name, mean, min, max, and units
panels.ranges = 
  data.frame(
    name = "SGOT",
    mean = mean(v3),
    min = min(v3),
    max = max(v3),
    units = "u/L"
  )
panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "SGPT", 
                    mean = mean(v4),
                    min = min(v4),
                    max = max(v4),
                    units = "u/L"
         ))
panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Sodium 170", 
                    mean = mean(v5),
                    min = min(v5),
                    max = max(v5),
                    units = "mmEq/L"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Creatinine", 
                    mean = mean(v6),
                    min = min(v6),
                    max = max(v6),
                    units = "mg/dL"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Potassium", 
                    mean = mean(v7),
                    min = min(v7),
                    max = max(v7),
                    units = "mmol/L"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Bilirubin", 
                    mean = mean(v8),
                    min = min(v8),
                    max = max(v8),
                    units = "mg/dL"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Bilirubin Indirect", 
                    mean = mean(v9),
                    min = min(v9),
                    max = max(v9),
                    units = "mg/dL"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "Prothrombin", 
                    mean = mean(v10),
                    min = min(v10),
                    max = max(v10),
                    units = "seconds"
         ))

panels.ranges =
  rbind( panels.ranges,
         data.frame(name = "INR", 
                    mean = mean(v11),
                    min = min(v11),
                    max = max(v11),
                    units = "INR"
         ))

write.xlsx(panels.ranges, file = "donor_ranges.xlsx", 
           sheetName="LabPanels", append=TRUE)
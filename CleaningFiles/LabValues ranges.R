#this file gets the range of each variable in the labvalues dataset

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library(openxlsx)

#DonorNet LabValues data
values = read.table(file = "Donornet-Shared/Donornet-Shared/LabValues.tsv.gz")

#dividing data into columns to remove NAs
v3 = values$V3
v4 = values$V4
v5 = values$V5

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

# adding the variable name, mean, min, max, and units
values.ranges = 
  data.frame(
    name = "CKMB",
    mean = mean(v3),
    min = min(v3),
    max = max(v3),
    units = "ng/mL"
  )
values.ranges =
  rbind( values.ranges,
         data.frame(name = "Troponini", 
                    mean = mean(v4),
                    min = min(v4),
                    max = max(v4),
                    units = "ng/mL"
         ))
values.ranges =
  rbind( values.ranges,
         data.frame(name = "Troponint", 
                    mean = mean(v5),
                    min = min(v5),
                    max = max(v5),
                    units = "ng/mL"
         ))

write.xlsx(values.ranges, file = "donor_ranges.xlsx", 
           sheetName="LabValues", append=TRUE)

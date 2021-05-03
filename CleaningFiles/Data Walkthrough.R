#this file walks through labpanels, labvalues, and echo data

data_dir = "C:/Users/student/Documents/Healthy Hearts"
setwd(data_dir)

library("readxl")

echo = read.csv(file = "PRODSOMRUNOS-InitialRun_DATA_LABELS_2020-10-13_0658.csv")
panels = read_excel("DonorNet LabPanels.xlsx")
values = read_excel("DonorNet LabValues.xlsx")

summary(echo)
echo_nd <- echo[!(duplicated(echo[, c("Donor.ID")])),]

x4box = boxplot(echo$X4.Chamber.Ejection.Fraction.., na.rm = TRUE, horizontal = TRUE, axes = FALSE, staplewex = 1, xlab = "")
text(x=fivenum(echo$X4.Chamber.Ejection.Fraction..), labels = fivenum(echo$X4.Chamber.Ejection.Fraction..), y=1.25)
mtext("Percentage", side = 3, line = -3)

bipbox = boxplot(echo$Biplane.Ejection.Fraction.., na.rm = TRUE, horizontal = TRUE, axes = FALSE, staplewex = 1, xlab = "")
text(x=fivenum(echo$Biplane.Ejection.Fraction..), labels = fivenum(echo$Biplane.Ejection.Fraction..), y=1.25)
mtext("Percentage", side = 3, line = -3)

shorbox = boxplot(echo$Shortening.Fraction.., na.rm = TRUE, horizontal = TRUE, axes = FALSE, staplewex = 1, xlab = "")
text(x=fivenum(echo$Shortening.Fraction..), labels = fivenum(echo$Shortening.Fraction..), y=1.25)
mtext("Percentage", side = 3, line = -3)

library(ggplot2)
(ggplot(as.data.frame(table(echo$Mitral.valve.insufficiency)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Aortic.valve.insufficiency)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Tricuspid.valve.insufficiency)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Pulmonary.valve.insufficiency)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Global.Left.Ventricular.Dysfunction)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Global.Right.Ventricular.Dysfunction)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))

(ggplot(as.data.frame(table(echo$Focal.Interventricular.Septal.Dysfunction)), aes(x = Var1, y= Freq)) 
  + labs(x = "Severity", y = "Frequency") 
  + geom_bar(stat="identity"))




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################







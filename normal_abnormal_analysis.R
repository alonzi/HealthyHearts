data_dir = "C:/Users/Megan/Documents/Capstone/Data/Full Longitudinal Analysis"
setwd(data_dir)

#---------------------------------------------------------------------------#
# Load Required Packages
#---------------------------------------------------------------------------#
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(ggpubr)
library(xlsx)

#---------------------------------------------------------------------------#
# Read in data
#---------------------------------------------------------------------------#

data <- read.csv("full7100withduration.csv")
data.after.BD <- data[which(data$Duration>=-24),]

donor.data <- read_csv("Donor_Data.csv")
donor.info <- donor.data[,c("DONOR_ID","age.group","age")]

data.after.BD <- merge(data.after.BD, donor.info, by="DONOR_ID")

ranges <- read_csv("variable_ranges.csv")

#---------------------------------------------------------------------------#
# Bin duration
#---------------------------------------------------------------------------#

data.after.BD$Duration.group <- rep(NA, nrow(data.after.BD))

data.after.BD <-mutate(data.after.BD, Duration.group = case_when(
  data.after.BD$Duration < 0 & data.after.BD$Duration >= -24 ~ "-24-0",
  data.after.BD$Duration < 6 & data.after.BD$Duration >= 0 ~ "0-6",
  data.after.BD$Duration < 12 & data.after.BD$Duration >= 6 ~ "06-12",
  data.after.BD$Duration < 18 & data.after.BD$Duration >= 12 ~ "12-18",
  data.after.BD$Duration < 24 & data.after.BD$Duration >= 18 ~ "18-24",
  data.after.BD$Duration < 36 & data.after.BD$Duration >= 24 ~ "24-36",
  data.after.BD$Duration < 48 & data.after.BD$Duration >= 36 ~ "36-48",
  data.after.BD$Duration >= 48 ~ "48+"
))

data.after.BD$Duration.group <- as.factor(data.after.BD$Duration.group)
data.after.BD$DONOR_ID <- as.factor(data.after.BD$DONOR_ID)


#---------------------------------------------------------------------------#
# Plots - Compare abnormal/normal measurements at each binned duration
#---------------------------------------------------------------------------#

#function - can be repeated for each variable
#requires downloading sheet 2 of variable ranges as a csv and renaming it variable_ranges.csv
#Creatinine and Bilirubin must be done manually
proportion_normal_bar <- function(variable) {
  temp <- data.after.BD
  i <- grep(variable, colnames(temp))
  
  temp$classification <- rep(NA, nrow(temp))
  
  temp <-mutate(temp, classification = case_when(
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "0-6 months"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "0-6 months"),]$upper & temp$age.group == "0-6 months" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "0-6 months"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "0-6 months"),]$upper  & temp$age.group == "0-6 months"~ "Normal",
    
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "6-12 months"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "6-12 months"),]$upper & temp$age.group == "6-12 months" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "6-12 months"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "6-12 months"),]$upper  & temp$age.group == "6-12 months"~ "Normal",
      
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "1-3 years"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "1-3 years"),]$upper & temp$age.group == "1-3 years" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "1-3 years"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "1-3 years"),]$upper  & temp$age.group == "1-3 years"~ "Normal",
      
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "3-6 years"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "3-6 years"),]$upper & temp$age.group == "3-6 years" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "3-6 years"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "3-6 years"),]$upper  & temp$age.group == "3-6 years"~ "Normal",
      
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "6-12 years"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "6-12 years"),]$upper & temp$age.group == "6-12 years" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "6-12 years"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "6-12 years"),]$upper  & temp$age.group == "6-12 years"~ "Normal",
      
    temp[,i ] < ranges[which(ranges$variable == variable & ranges$age_group == "12+ years"),]$lower | temp[,i ] > ranges[which(ranges$variable == variable & ranges$age_group == "12+ years"),]$upper & temp$age.group == "12+ years" ~ "Abnormal",
    temp[,i ] >= ranges[which(ranges$variable == variable & ranges$age_group == "12+ years"),]$lower & temp[,i ] <= ranges[which(ranges$variable == variable & ranges$age_group == "12+ years"),]$upper  & temp$age.group == "12+ years"~ "Normal",
    
      ))
  plot <- ggarrange(ggplot(data=subset(temp[which(temp$Accepted == "Yes"),], !is.na(classification)), aes(x=Duration.group, fill=classification, na.rm = TRUE))+geom_bar(position="fill") + ggtitle("Accepted Donors") + xlab("Time since Brain Death (hours)"), 
            ggplot(data=subset(temp[which(temp$Accepted == "No"),], !is.na(classification)), aes(x=Duration.group, fill=classification, na.rm = TRUE))+geom_bar(position="fill") + ggtitle("Rejected Donors")+ xlab("Time since Brain Death (hours)"), 
            nrow = 1, ncol = 2)
  
  plot <- annotate_figure(plot,
                  top = text_grob(paste0("Proportion of Normal & Abnormal ", variable, " measurements over time"), color = "#00BFC4", face = "bold", size = 16)
  )
  
  temp <- mutate(temp, Accepted = case_when(
    temp$Accepted == "Yes" ~ "Accepted",
    temp$Accepted == "No" ~ "Rejected"
  ))
  
  return(list(plot,ftable(table(temp$Accepted,temp$classification, temp$Duration.group))))
}
proportion_normal_bar("PAO2")
proportion_normal_bar("SGOT")
proportion_normal_bar("SGPT")
proportion_normal_bar("ABG_PH")
proportion_normal_bar("PEEP")
proportion_normal_bar("HGB")
proportion_normal_bar("AVG_BP_SYST")
proportion_normal_bar("AVG_BP_DIAST")
proportion_normal_bar("AVG_PULSE_RANGE_START")
proportion_normal_bar("CVP_INT_RANGE_START")
proportion_normal_bar("BODYTEMP_RANGE_START")
proportion_normal_bar("SODIUM170")
proportion_normal_bar("POTASSIUM")
proportion_normal_bar("PROTHROMBIN")
proportion_normal_bar("INR")
proportion_normal_bar("CKMB")
proportion_normal_bar("TROPONINI")


#CREATININE

data.after.BD$Creatinine.group <- rep(NA, nrow(data.after.BD))
data.after.BD <-mutate(data.after.BD, Creatinine.group = case_when(
  data.after.BD$CREATININE < 0.5 | data.after.BD$CREATININE > 0.9 & data.after.BD$age >= 10 ~ "Abnormal",
  data.after.BD$CREATININE >= 0.5 & data.after.BD$CREATININE <= 0.9 & data.after.BD$age >= 10 ~ "Normal",
  
  data.after.BD$CREATININE > 0.5 & data.after.BD$age < 10 ~ "Abnormal",
  data.after.BD$CREATININE <= 0.5 & data.after.BD$age < 10 ~ "Normal"
))

Creat.A <- ggplot(data=subset(data.after.BD[which(data.after.BD$Accepted == "Yes"),], !is.na(Creatinine.group)), aes(x=Duration.group, fill=Creatinine.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")+labs(fill="classification")
Creat.R <- ggplot(data=subset(data.after.BD[which(data.after.BD$Accepted == "No"),], !is.na(Creatinine.group)), aes(x=Duration.group, fill=Creatinine.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")+labs(fill = "classification")

plot <- ggarrange(Creat.A, Creat.R, nrow = 1, ncol = 2)
annotate_figure(plot,
                top = text_grob(paste0("Proportion of Normal & Abnormal Creatinine measurements over time"), color = "#00BFC4", face = "bold", size = 16)
)
ftable(table(data.after.BD$Accepted,data.after.BD$Creatinine.group, data.after.BD$Duration.group))

# BILIRUBIN

data.after.BD$Bilirubin.group <- rep(NA, nrow(data.after.BD))
data.after.BD <-mutate(data.after.BD, Bilirubin.group = case_when(
  data.after.BD$BILIRUBIN >=1  | data.after.BD$BILIRUBIN_INDIRECT >= 1 ~ "Abnormal",
  data.after.BD$BILIRUBIN < 1 | data.after.BD$BILIRUBIN_INDIRECT < 1 ~ "Normal"
))

Bil.A <- ggplot(data=subset(data.after.BD[which(data.after.BD$Accepted == "Yes"),], !is.na(Bilirubin.group)), aes(x=Duration.group, fill=Bilirubin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Accepted Donors")+xlab("Time since Brain Death (hours)")+labs(fill="classification")
Bil.R <- ggplot(data=subset(data.after.BD[which(data.after.BD$Accepted == "No"),], !is.na(Bilirubin.group)), aes(x=Duration.group, fill=Bilirubin.group, na.rm = TRUE))+geom_bar(position="fill")+ ggtitle("Rejected Donors")+xlab("Time since Brain Death (hours)")+labs(fill="classification")
plot <- ggarrange(Bil.A, Bil.R, nrow = 1, ncol = 2)
annotate_figure(plot,
                top = text_grob(paste0("Proportion of Normal & Abnormal Bilirubin measurements over time"), color = "#00BFC4", face = "bold", size = 16)
)
ftable(table(data.after.BD$Accepted,data.after.BD$Bilirubin.group, data.after.BD$Duration.group))


#---------------------------------------------------------------------------#
# Boxplots by age
#---------------------------------------------------------------------------#

#again, have to manually do Creatinine
boxplot_age <- function(var) {
  ages <- unique(ranges[which(ranges$variable == var),]$age_group)
  index <- 0
  unit <- slice_head(ranges[which(ranges$variable == var),])$units
  
  for (a in ages) {
    index <- index + 1
    min.range <- ranges[which(ranges$variable == var & ranges$age_group == a),]$lower
    max.range <- ranges[which(ranges$variable == var & ranges$age_group == a),]$upper
    
    A.box <- ggplot(data = data.after.BD[which(data.after.BD$age.group == a & data.after.BD$Accepted == "Yes"),], aes_string(x = "Duration.group", y = var, fill="Duration.group")) + scale_fill_grey(start = 0.9, end = 0.1) +
      geom_boxplot() + ylab(paste0(a, " (", unit, ")")) + geom_hline(yintercept = min.range, linetype="dashed", color = "red") +geom_hline(yintercept = max.range, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red")+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+ 
      labs(fill = "Hours past Brain Death")
    
    assign(paste0("A",index), A.box )
    
    R.box <- ggplot(data = data.after.BD[which(data.after.BD$age.group == a & data.after.BD$Accepted == "No"),], aes_string(x = "Duration.group", y = var, fill="Duration.group")) + scale_fill_grey(start = 0.9, end = 0.1) + 
      geom_boxplot() + ylab(paste0(a, " (", unit, ")"))+ geom_hline(yintercept = min.range, linetype="dashed", color = "red") +geom_hline(yintercept = max.range, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red")+
      theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+ 
      labs(fill = "Hours past Brain Death")
    
    assign(paste0("R",index), R.box )
    
  }
  
  plot <- ggarrange(A1, A2, A3, A4, A5, A6, R1, R2, R3, R4, R5, R6, nrow = 2, ncol = 6, common.legend = TRUE)
  plot <- annotate_figure(plot,
                  top = text_grob(paste0("Boxplots of ", var, " measurements by age"), color = "blue", face = "bold", size = 16),
                  left = text_grob("Accepted Donors", color = "blue", rot=90, y=0.6)
  )
  plot <- annotate_figure(plot,
                          left = text_grob("Rejected Donors", color = "blue", rot=90, y=0.25, vjust = 2),
                          )
  
  return(plot)
  
}

boxplot_age("PAO2")
boxplot_age("SGOT")
boxplot_age("SGPT")
boxplot_age("ABG_PH")
boxplot_age("PEEP")
boxplot_age("HGB")
boxplot_age("AVG_BP_SYST")
boxplot_age("AVG_BP_DIAST")
boxplot_age("AVG_PULSE_RANGE_START")
boxplot_age("CVP_INT_RANGE_START")
boxplot_age("BODYTEMP_RANGE_START")
boxplot_age("SODIUM170")
boxplot_age("POTASSIUM")
boxplot_age("PROTHROMBIN")
boxplot_age("INR")
boxplot_age("CKMB")
boxplot_age("TROPONINI")
boxplot_age("BILIRUBIN")
boxplot_age("BILIRUBIN_INDIRECT")

#CREATININE

Creat.box.A1 <-ggplot(data = data.after.BD[which(data.after.BD$age < 10 & data.after.BD$Accepted == "Yes"),], aes(x = Duration.group, y = CREATININE, fill=Duration.group)) + scale_fill_grey(start = 0.9, end = 0.1) + 
  geom_boxplot() + ylab("CREATININE (mg/dL)") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red")  + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5)) + 
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(fill = "Hours past Brain Death")
Creat.box.A2 <-ggplot(data = data.after.BD[which(data.after.BD$age >= 10 & data.after.BD$Accepted == "Yes"),], aes(x = Duration.group, y = CREATININE, fill=Duration.group)) + scale_fill_grey(start = 0.9, end = 0.1) + 
  geom_boxplot() + ylab("CREATININE (mg/dL)") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +geom_hline(yintercept = 0.9, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ 
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(fill = "Hours past Brain Death")
Creat.box.R1 <- ggplot(data = data.after.BD[which(data.after.BD$age < 10 & data.after.BD$Accepted == "No"),], aes(x = Duration.group, y = CREATININE, fill=Duration.group)) + scale_fill_grey(start = 0.9, end = 0.1) + 
  geom_boxplot() + ylab("CREATININE (mg/dL)") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ 
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(fill = "Hours past Brain Death")
Creat.box.R2 <- ggplot(data = data.after.BD[which(data.after.BD$age >= 10  & data.after.BD$Accepted == "No"),], aes(x = Duration.group, y = CREATININE, fill=Duration.group)) + scale_fill_grey(start = 0.9, end = 0.1) + 
  geom_boxplot() + ylab("CREATININE (mg/dL)") + geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +geom_hline(yintercept = 0.9, linetype="dashed", color = "red") + stat_summary(geom = "point", fun = mean, color = "red") + 
  coord_cartesian(ylim = c(0,5))+ 
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(fill = "Hours past Brain Death")

plot <- ggarrange(Creat.box.A1, Creat.box.A2, Creat.box.R1, Creat.box.R2, nrow = 2, ncol = 2, common.legend = TRUE)

plot <- annotate_figure(plot,
                        top = text_grob(paste0("Boxplots of ", var, " measurements by age"), color = "blue", face = "bold", size = 16),
                        left = text_grob("Accepted Donors", color = "blue", rot=90, y=0.6)
)
annotate_figure(plot,
                left = text_grob("Rejected Donors", color = "blue", rot=90, y=0.25, vjust = 2),
)

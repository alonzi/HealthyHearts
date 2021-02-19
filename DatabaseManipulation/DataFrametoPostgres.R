traindir <- "~/4th year/STS Capstone/donorData/"
sourcedir <-"~/4th year/SYS 4021/Source"

setwd(traindir)
crosswalk <- read.csv(file = 'cross-walk.csv')
abgs <- read.csv(file = 'abgs_joined_crosswalk_updated.csv')
cbc <- read.csv(file = 'cbc_joined_crosswalk_updated.csv')
deceased_donor <- read.csv(file = 'crosswalk_joined_deceased_donor.csv')
indicators <- read.csv(file = 'indicators_joined_crosswalk.csv')
inotropic.score <- read.csv(file = 'inotropic.score_joined_crosswalk.csv')
lab_panels <- read.csv(file = 'labpanels_crosswalk_joined.csv')
lab_values <- read.csv(file = 'labvalues_crosswalk_joined.csv')

library(DBI)

# #this is what I used on my local machine
# con <- dbConnect(RPostgres::Postgres(),dbname = 'HealthyHearts', 
#                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
#                 port = 5432, # or any other port specified by your DBA
#                 user = 'postgres',
#                 password = 'Hearts123')
# 
# dbListTables(con) #checks for the tables
# 
# dbWriteTable(con, "crosswalk", crosswalk, overwrite = TRUE)
# dbWriteTable(con, "abgs", abgs, overwrite = TRUE)
# dbWriteTable(con, "cbc", cbc, overwrite = TRUE)
# dbWriteTable(con, "deceaseddonor", deceased_donor, overwrite = TRUE)
# dbWriteTable(con, "indicators", indicators, overwrite = TRUE)
# dbWriteTable(con, "inotropicscore", inotropic.score, overwrite = TRUE)
# dbWriteTable(con, "labpanels", lab_panels, overwrite = TRUE)
# dbWriteTable(con, "labvalues", lab_values, overwrite = TRUE)
# 
# #dbAppendTable() works to add to a table. You can overwrite the original by using overwrite = true



# New Data set
traindir <- "~/4th year/STS Capstone/donorData/full_join"

setwd(traindir)

crosswalk2 <- read.csv(file = 'full_crosswalk.csv')
abgs2 <- read.csv(file = 'abgs_crosswalk_full.csv')
cbc2 <- read.csv(file = 'cbc_crosswalk_full.csv')
indicators2 <- read.csv(file = 'indicators_joined_full_crosswalk.csv')
inotropic.score2 <- read.csv(file = 'inotropic.score_joined_full_crosswalk.csv')
lab_panels2 <- read.csv(file = 'labpanels_full_crosswalk_joined.csv')
lab_values2 <- read.csv(file = 'labvalues_full_crosswalk_joined.csv')
echo2 <- read.csv(file = 'echo_w_analysis.csv', check.names = FALSE)

#rename columns
colnames(echo2)[names(echo2) == "Donor ID"] <- "DON_ID"

#remove extraneous columns
lab_panels2<- subset(lab_panels2, select = -c(BRAIN_DEATH_DATE,BRAIN_DEATH_TIME))
lab_values2<- subset(lab_values2, select = -c(BRAIN_DEATH_DATE,BRAIN_DEATH_TIME))
echo2<- subset(echo2, select = -c(index))

con2 <- dbConnect(RPostgres::Postgres(),dbname = 'HealthyHearts2', 
                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'Hearts123')

#missing deceased donor
dbWriteTable(con2, "crosswalk21", crosswalk2, overwrite = TRUE)
dbWriteTable(con2, "abgs2", abgs2, overwrite = TRUE)
dbWriteTable(con2, "cbc2", cbc2, overwrite = TRUE)
dbWriteTable(con2, "indicators2", indicators2, overwrite = TRUE)
dbWriteTable(con2, "inotropicscore2", inotropic.score2, overwrite = TRUE)
dbWriteTable(con2, "labpanels2", lab_panels2, overwrite = TRUE)
dbWriteTable(con2, "labvalues2", lab_values2, overwrite = TRUE)
dbWriteTable(con2, "echo2", echo2, overwrite = TRUE)


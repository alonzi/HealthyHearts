traindir <- "~/4th year/STS Capstone/donorData/"
sourcedir <-"~/4th year/SYS 4021/Source"

setwd(traindir)
crosswalk <- read.csv(file = 'cross-walk.csv')
abgs <- read.csv(file = 'abgs_joined_crosswalk.csv')
cbc <- read.csv(file = 'cbc_joined_crosswalk.csv')
deceased_donor <- read.csv(file = 'crosswalk_joined_deceased_donor.csv')
indicators <- read.csv(file = 'indicators_joined_crosswalk.csv')
inotropic.score <- read.csv(file = 'inotropic.score_joined_crosswalk.csv')
lab_panels <- read.csv(file = 'labpanels_crosswalk_joined.csv')
lab_values <- read.csv(file = 'labvalues_crosswalk_joined.csv')

install.packages("RPostgres")
library(DBI)

#this is what I used on my local machine
con <- dbConnect(RPostgres::Postgres(),dbname = 'HealthyHearts', 
                host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                port = 5432, # or any other port specified by your DBA
                user = 'postgres',
                password = 'Hearts123')

dbListTables(con) #checks for the tables

dbWriteTable(con, "crosswalk", crosswalk, overwrite = TRUE)
dbWriteTable(con, "abgs", abgs, overwrite = TRUE)
dbWriteTable(con, "cbc", cbc, overwrite = TRUE)
dbWriteTable(con, "deceasedDonor", deceased_donor, overwrite = TRUE)
dbWriteTable(con, "indicators", indicators, overwrite = TRUE)
dbWriteTable(con, "inotropicScore", inotropic.score, overwrite = TRUE)
dbWriteTable(con, "labPanels", lab_panels, overwrite = TRUE)
dbWriteTable(con, "labValues", lab_values, overwrite = TRUE)

#dbAppendTable() works to add to a table. You can overwrite the original by using overwrite = true
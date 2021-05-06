* DataFrametoPostgres.R is a file that takes in multiple csv files that are joined with the crosswalk file and writes them to a postgres database using the DBI package. 
We used a PostgreSQL database with pgAdmin client to manipulate the data. 

* jointest2.sql is the SQL file that is used to join the data sets once they are in the database.

* The two duration calculated files (John and Wesley) were used after reading in the joined file csv file created from Postgres to add a column for Duration (DT minus Braindeath)
There were two files as different methods dealt with parsing errors better.

##FOR FUTURE PROJECTS:
We suggest using the same DBI package found in DataFrametoPostgres to read directly from the database to calculate duration,
rather than writing a csv file from the database to prevent PARSING ERRORS.

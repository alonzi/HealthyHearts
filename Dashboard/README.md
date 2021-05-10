Donor-summary.Rmd and layout-results.Rmd are the two important files for the dashboard. Generally, the dashboard is an interactive html document that displays overall trends in the accepted and rejected donor data, broken down in various ways.


* Donor-summary.Rmd - contains the bulk of the code to produce the html Dashboard; requires variable_ranges.xlsx, Donor_Data.csv, full7100withall.csv
* layout-results.Rmd - helper file for Donor-summary.Rmd
* meld_schwartz_calculated.R - calculates the MELD score and Schwartz score for each row of the data from the file read in and writes to new csv file

* Old Dashboard 12-15-20.Rmd and Old Dashboard 12-18-20.Rmd are old versions of the dashboard
* Old Dashboard 12-8-20.R is an older version of the dashboard before we began knitting to html
* preliminary extreme values function.R - creates a preliminary version of the extreme values function ultimately used in the dashboard


Donor-summary.Rmd:

If you run each line of code by itself, it will not work. You must knit the document with the button in the top left of RStudio.

* The first and second code chunks set the data directory, load packages, and set some html formatting options. There is documentation online to figure out what formatting options there are.

* The third code chunk is where you set the dataset being read in and the variable ranges file.

* The fourth code chunk is where you can set which variables have plots included in the dashboard, and it is where the graph intervals are set at periods of 6 or 12 hours, where the age range divisions are made, and where the upper and lower limits of the normal range are coded.

* The fifth code chunk sets up the four different types of plots in the html dashboard: general boxplots, abnormal/normal percentage bar graphs, abnormal/normal percentage tables, and boxplots of minimum, maximum, and most recent observations.

* The sixth code chunk links Donor-summary.Rmd to layout-results.Rmd.

* The seventh code chunk is a very important bit of code. It allows you to change the cache settings. If they are set to true, the whole document will knit to html much more quickly and will not reflect some of the changes you may have made to the document. If they are set to false, all the new changes will be reflected, and it will take the html much longer to knit. Change this to suit your needs. Additionally, there are two other cache setting at the beginning of the third and fourth code chunks.

* The eighth, ninth, tenth, eleventh, and twelfth code chunks actually run the code to create the four different plots for each variable. Respectively, these code chunks are associated with kidney and blood variables, liver variables, heart variables, inotropic variables, and body temperature variables. 

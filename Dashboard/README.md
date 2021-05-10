READ-ME for Dashboard

Donor-summary.Rmd and layout-results.Rmd are the two important files for the dashboard. Generally, the dashboard is an interactive html document that displays overall trends in the accepted and rejected donor data, broken down in various ways.

Donor-summary.Rmd contains the bulk of the code to produce the html Dashboard. layout-results.Rmd is a helper file. If you run each line of code by itself, it will not work. You must knit the document with the button in the top left of RStudio.



Donor-summary.Rmd:

The first and second code chunks set the data directory, load packages, and set some html formatting options. There is documentation online to figure out what formatting options there are.

The third code chunk is where you set the dataset being read in and the variable ranges file.

The fourth code chunk is where you can set which variables have plots included in the dashboard, and it is where the graph intervals are set at periods of 6 or 12 hours, where the age range divisions are made, and where the upper and lower limits of the normal range are coded.

The fifth code chunk sets up the four different types of plots in the html dashboard: general boxplots, abnormal/normal percentage bar graphs, abnormal/normal percentage tables, and boxplots of minimum, maximum, and most recent observations.

The sixth code chunk links Donor-summary.Rmd to layout-results.Rmd.

The seventh code chunk is a very important bit of code. It allows you to change the cache settings. If they are set to true, the whole document will knit to html much more quickly and will not reflect some of the changes you may have made to the document. If they are set to false, all the new changes will be reflected, and it will take the html much longer to knit. Change this to suit your needs. Additionally, there are two other cache setting at the beginning of the third and fourth code chunks.

The eighth, ninth, tenth, eleventh, and twelfth code chunks actually run the code to create the four different plots for each variable. Respectively, these code chunks are associated with kidney and blood variables, liver variables, heart variables, inotropic variables, and body temperature variables. 

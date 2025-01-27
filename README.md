# Visualising air pollution data
The app is available in : https://adil-sahab-16.shinyapps.io/Shinyapps_io/
##### This project was done as part of R programming assignment for my Master’s program. The ask was to create a R-Shiny app which visualises air pollution data collected across multiple sites in the Czech Republic from 2013 to 2019. The air quality standards in the EU (as of 2021) are: 
•	Fine particulates (PM2.5) Yearly average of at most 25μg/m3. 
•	Particulates (PM10) Daily average exceeding 50μg/m3 observed on at most 35 days a year, and yearly average of at most 40μg/m3. 
•	Sulphur dioxide (SO2) Hourly concentration exceeding 350μg/m3 for at most 24 hours per year, and average daily concentration exceeding 125μg/m3 on at most 3 days per year. 
•	Nitrogendioxide(NO2) Hourly concentration exceeding 200μg/m3 for at most 18hours per year, and average yearly concentration of at most 40μg/m3. 
###### Data:
The data is available as a collection of CSV files, as downloaded from European Environmental Agency (see https://www.eea.europa.eu/themes/air). The file Stations.csv contains information about each station. There is one CSV file for each station and each pollutant (PM2.5, PM10, SO2 and NO2) measured at the station (not every pollutant is measured at every station). The filenames are of the form <EoICode>_<PollutantCode>.csv. Each row of these files contains hourly measurements of that pollutant at that station. For some stations there only is data for a subset of the time points. 

###### App Functionality:
The Shiny app visualises the data with the following functionality:
- The core functionality of the app is to produce time series plots of relevant summary statistics against time. 
- The user can select for which pollutant and for which stations (atmost3) the quantities should be shown.
- After the pollutant is selected, the app shows the stations only at which the pollutant was measured. 
- The app lets the user decide what aggregation to plot, from the following options:
  - Raw hourly data (no aggregation)
  - Daily averages
  - Daily maxima
  - Number of hours per day for which a given threshold is exceeded
  - Number of hours per year for which a given threshold is exceeded
For aggregations involving a threshold, the user can choose this threshold. 
- The user can choose how time is to be handled and what the x-Axis should represent. Choices include :
  - Calendar time
  - Date within the year (going from Jan 1st to Dec 31st)
  - Day or hour within the week (going from 0 to 7 (days) or 0 to 168 (hours))
  - Hour in the day (going from 0 to 24 (hours)). 
- If the choice of pollutant and aggregation matches a criterion from the EU air quality standard, then the plot shows a horizontal line at the threshold for that criterion. For example, if the user wants to plot the raw hourly data for SO2, then a horizontal line is drawn at y = 350. 
- The app also shows a second plot with the location of the measuring station(s). 
- Below the plot, a table shows the data used in the plot in wide format (ie. The columns correspond to measuring stations). 
- The user can also download the table as a CSV file and the plots and table together as a Word document (generated using rmarkdown). 
###### R Code:
- The file ui.R specifies the layout (“user interface”) of the app.
- The file server.R contains the “backend” performing the calculations and producing the plots and other outputs.
- The file report.Rmd helps the app download the report as a Word document.

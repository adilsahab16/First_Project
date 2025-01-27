library(ggplot2)
library(maps)
library(mapdata)
library(DT)
library(rmarkdown)
library(dplyr)
library(shiny)
library(lubridate)
library(scales)

pollutants <- c("PM2.5"="pm2.5", "PM10"="pm10",
             "SO2"="so2", "NO2"="no2")

aggregation <- c("Raw hourly data"="raw_hourly_data", "Daily averages"="daily_averages",
                "Daily maxima"="daily_maxima",
                "Hours per day for exceeded threshold"="hours_day_threshold",
                "Hours per year for exceeded threshold"="hours_year_threshold")

time_hourly <- c("Calendar Time"="calendar_time", "Date within the year"="date_within_year",
                 "Day within week"="day_within_week",
                 "Hour in the day"="hour_day")

time_daily<- c("Day within week"="day_within_week")

time_day_threshold <- c("Calendar Time"="calendar_time", "Date within the year"="date_within_year",
                        "Day within week"="day_within_week")

time_year_threshold <- c("Calendar Time"="calendar_time")


stations_pm10 <- c("CZ0ABRE","CZ0ACHO","CZ0AKAL","CZ0AKOB","CZ0ALEG","CZ0ALIB",
                                      "CZ0APRU","CZ0AREP","CZ0ARIE","CZ0ASMI","CZ0ASTO","CZ0ASUC",
                                      "CZ0AVRS","CZ0AVYN","CZ0BBDN","CZ0BBNE","CZ0BBNF","CZ0BBNI",
                                      "CZ0BBNV","CZ0BBNY","CZ0BKUC","CZ0BLOC","CZ0BMIS","CZ0BVYS",
                                      "CZ0BZNO","CZ0CCBD","CZ0CCHU","CZ0CHVO","CZ0CPRA","CZ0CTAB",
                                      "CZ0CVOD","CZ0EMTP","CZ0EPAU","CZ0ESEZ","CZ0ESVR","CZ0EUOR",
                                      "CZ0HHKB","CZ0HHKT","CZ0HJIC","CZ0HPLO","CZ0HRNK","CZ0HTRT",
                                      "CZ0HVEL","CZ0JJIH","CZ0JKOS","CZ0JKRI","CZ0JTRE","CZ0KCHM",
                                      "CZ0KKVA","CZ0KSOM","CZ0LCLM","CZ0LFRT","CZ0LJIZ","CZ0LJNM",
                                      "CZ0LLIL","CZ0LRAD","CZ0LSOU","CZ0LTAS","CZ0MBEL","CZ0MDST",
                                      "CZ0MJES","CZ0MLOS","CZ0MOLJ","CZ0MPRR","CZ0MPST","CZ0PKUJ",
                                      "CZ0PPLA","CZ0PPLB","CZ0PPLE","CZ0PPLL","CZ0PPLS","CZ0PPLV",
                                      "CZ0PSTA","CZ0SBER","CZ0SBRL","CZ0SKLM","CZ0SKLS","CZ0SKUH",
                                      "CZ0SMBO","CZ0SPBR","CZ0SPRI","CZ0SROR","CZ0TBKR","CZ0TBRS",
                                      "CZ0TCEL","CZ0TCER","CZ0TCTN","CZ0TFMI","CZ0THAR","CZ0THAT",
                                      "CZ0TKAR","CZ0TNUJ","CZ0TOCB","CZ0TOFF","CZ0TOPO","CZ0TOPR",
                                      "CZ0TORV","CZ0TOSG","CZ0TOVK","CZ0TOZR","CZ0TPIS","CZ0TRYC",
                                      "CZ0TSTD","CZ0TTRK","CZ0TTRO","CZ0TVER","CZ0TVRZ","CZ0UCEC",
                                      "CZ0UCHM","CZ0UDCM","CZ0UDOK","CZ0UKRU","CZ0ULOM","CZ0ULTT",
                                      "CZ0UMED","CZ0UMOM","CZ0URVH","CZ0USNZ","CZ0UTPM","CZ0UTUS",
                                      "CZ0UULD","CZ0UULK","CZ0UULM","CZ0UVAL","CZ0ZTNV","CZ0ZUHR",
                                      "CZ0ZVMZ","CZ0ZVSH","CZ0ZZLN")
stations_pm10 <- as.data.frame(stations_pm10)

stations_pm2.5 <- c("CZ0ALEG","CZ0ALIB","CZ0ARIE","CZ0ASMI","CZ0ASTO",
                                       "CZ0BBDN","CZ0BBNI","CZ0BBNV","CZ0BBNY","CZ0BKUC",
                                       "CZ0BMIS","CZ0BZNO","CZ0CCBD","CZ0CCHU","CZ0EMTP",
                                       "CZ0EPAU","CZ0ESVR","CZ0HHKB","CZ0HHKT","CZ0HJIC",
                                       "CZ0JJIH","CZ0JKOS","CZ0JKRI","CZ0KSOM","CZ0LFRT",
                                       "CZ0LLIL","CZ0MBEL","CZ0MDST","CZ0MOLJ","CZ0MPRR",
                                       "CZ0PKUJ","CZ0PPLA","CZ0PPLB","CZ0PPLE","CZ0PPLL",
                                       "CZ0PPLS","CZ0PPLV","CZ0SBER","CZ0SKLM","CZ0SMBO",
                                       "CZ0SROR","CZ0TBKR","CZ0TBRS","CZ0TCEL","CZ0TCTN",
                                       "CZ0TFMI","CZ0THAR","CZ0THAT","CZ0TKAR","CZ0TOCB",
                                       "CZ0TOPO","CZ0TOPR","CZ0TOSG","CZ0TOVK","CZ0TOZR",
                                       "CZ0TRYC","CZ0TSTD","CZ0TSUD","CZ0TTRK","CZ0TTRO",
                                       "CZ0TVER","CZ0TVRZ","CZ0UDCM","CZ0UDOK","CZ0ULOM",
                                       "CZ0UMOM","CZ0UTPM","CZ0UTUS","CZ0UULD","CZ0UULK",
                                       "CZ0ZTNV","CZ0ZVMZ","CZ0ZZLN")
stations_pm2.5 <- as.data.frame(stations_pm2.5)

stations_so2 <- c("CZ0ALIB","CZ0ARIE","CZ0ASUC","CZ0BBNY","CZ0BMIS",
                                "CZ0CCBD","CZ0CCHU","CZ0CPRA","CZ0EPAO","CZ0EPAU",
                                "CZ0JJIH","CZ0JKOS","CZ0KPRB","CZ0KSOM","CZ0LFRT",
                                "CZ0LLIL","CZ0MJES","CZ0MPRR","CZ0PKUJ","CZ0PPLA",
                                "CZ0PPLB","CZ0PPLE","CZ0PPLL","CZ0PPLS","CZ0PPLV",
                                "CZ0SKLS","CZ0SROR","CZ0TBKR","CZ0TCTN","CZ0TKAR",
                                "CZ0TOFF","CZ0TOPO","CZ0TOPR","CZ0TOZR","CZ0TRYC",
                                "CZ0TSTD","CZ0TVER","CZ0UDCM","CZ0UDOK","CZ0UKRU",
                                "CZ0ULOM","CZ0ULTT","CZ0UMED","CZ0USNZ","CZ0UTPM",
                                "CZ0UTUS","CZ0UULK","CZ0UULM","CZ0ZTNV","CZ0ZZLN")
stations_so2 <- as.data.frame(stations_so2)

stations_no2 <- c("CZ0ABRE","CZ0ACHO","CZ0AKAL","CZ0AKOB","CZ0ALEG",
                                "CZ0ALIB","CZ0APRU","CZ0AREP","CZ0ARIE","CZ0ASMI",
                                "CZ0ASUC","CZ0AVYN","CZ0BBDN","CZ0BBNV","CZ0BBNY",
                                "CZ0BMIS","CZ0BZNO","CZ0CCBD","CZ0CCHU","CZ0CPRA",
                                "CZ0CTAB","CZ0EMTP","CZ0EPAO","CZ0HPLO","CZ0JJIH",
                                "CZ0JKOS","CZ0KPRB","CZ0KSOM","CZ0LLIL","CZ0MJES",
                                "CZ0MLOS","CZ0MOLJ","CZ0PKUJ","CZ0PPLA","CZ0PPLB",
                                "CZ0PPLE","CZ0PPLL","CZ0PPLS","CZ0PPLV","CZ0SBER",
                                "CZ0SKLM","CZ0SKLS","CZ0SMBO","CZ0SPBR","CZ0SPRI",
                                "CZ0SROR","CZ0TBKR","CZ0TCER","CZ0TCTN","CZ0TFMI",
                                "CZ0TKAR","CZ0TOCB","CZ0TOFF","CZ0TOPO","CZ0TOPR",
                                "CZ0TOVK","CZ0TOZR","CZ0TRYC","CZ0TSTD","CZ0TTRK",
                                "CZ0TVER","CZ0UDCM","CZ0UDOK","CZ0ULOM","CZ0ULTT",
                                "CZ0UMOM","CZ0USNZ","CZ0UTUS","CZ0UULD","CZ0UULK",
                                "CZ0UULM","CZ0ZTNV","CZ0ZUHR","CZ0ZZLN")
stations_no2 <- as.data.frame(stations_no2)

addNone <- function(v) {
  c("none"="None",v)
}


fluidPage(
    sidebarPanel(
      selectInput("pollutant", "Pollutant",addNone(pollutants),selected="none"
      ),
      conditionalPanel(
        condition = "input.pollutant == 'pm2.5'",
        selectizeInput("stations_pm2.5", "Stations", stations_pm2.5,
                       multiple=TRUE,options = list(maxItems = 3))
      ),
      conditionalPanel(
        condition = "input.pollutant == 'pm10'",
        selectizeInput("stations_pm10", "Stations", stations_pm10, multiple=TRUE,
                       options = list(maxItems = 3))
      ),
      conditionalPanel(
        condition = "input.pollutant == 'so2'",
        selectizeInput("stations_so2", "Stations", stations_so2, multiple=TRUE,
                       options = list(maxItems = 3))
      ),
      conditionalPanel(
        condition = "input.pollutant == 'no2'",
        selectizeInput("stations_no2", "Stations", stations_no2, multiple=TRUE,
                       options = list(maxItems = 3))
      ),
      conditionalPanel(
        condition = "input.pollutant != 'None'",
        selectInput("aggregation", "Aggregation",aggregation
        )
      ),
      conditionalPanel(
        #condition = "input.aggregation == 'raw_hourly_data'",
        condition = c("input.aggregation == 'raw_hourly_data' && input.pollutant != 'None'"),
        selectInput("time_hourly", "X-axis",time_hourly
        )
      ),
      conditionalPanel(
        #condition = "input.aggregation == 'daily_averages'",
        condition = c("input.aggregation == 'daily_averages' && input.pollutant != 'None'"),
        selectInput("daily_averages", "X-axis",time_daily
        )
      ),
      conditionalPanel(
        #condition = "input.aggregation == 'daily_maxima'",
        condition = c("input.aggregation == 'daily_maxima' && input.pollutant != 'None'"),
        selectInput("daily_maxima", "X-axis",time_daily
        )
      ),
      conditionalPanel(
        #condition = "input.aggregation == 'hours_day_threshold'",
        condition = c("input.aggregation == 'hours_day_threshold' && input.pollutant != 'None'"),
        selectInput("hours_day_threshold", "X-axis",time_day_threshold,selected="none"
        )
      ),
      conditionalPanel(
        #condition = "input.aggregation == 'hours_year_threshold'",
        condition = c("input.aggregation == 'hours_year_threshold' && input.pollutant != 'None'"),
        selectInput("hours_year_threshold", "X-axis",time_year_threshold
        )
      ),
      conditionalPanel(
        condition = c("input.aggregation == 'hours_year_threshold' || input.aggregation == 'hours_day_threshold'"),
        numericInput("threshold", "Threshold",value = 100
        )
      ),
      conditionalPanel(
        condition = c("input.pollutant != 'None'"),
        downloadButton("download1", "Download Table (csv)")
      ),
      conditionalPanel(
        condition = c("input.pollutant != 'None'"),
        downloadButton("downloadReportButton", "Generate Report (docx)")
      )
    ),
    mainPanel(
      h4("Visualising air pollution data across multiple sites in the Czech Republic 
         from 2013 to 2019"),
      plotOutput("plot"),
      plotOutput("map",),
      conditionalPanel(
        condition = ("input.pollutant != 'None'"),
        h5("Table Summary of the Station(s) selected")
      ),
      dataTableOutput("table")
      
    )
  )
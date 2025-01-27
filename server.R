library(ggplot2)
library(maps)
library(mapdata)
library(DT)
library(rmarkdown)
library(dplyr)
library(shiny)
library(lubridate)
library(scales)

function(input, output) {
  #setwd("~/Desktop/UOG - Msc Data Analytics/Year 1/R Programming/conversion folder")
  stations_data <- readRDS("__Stations.rds")
  dataset <- reactive({
    tryCatch({
    b_string_no2 <- paste(toupper(input$stations_no2),"_",toupper(input$pollutant)
                          ,".rds",sep="")
    
    if (count(as.data.frame(input$stations_no2))==0){
      b_no2 <- rbind()
    }
    else if (count(as.data.frame(input$stations_no2))==1){
      b1 <- readRDS(b_string_no2[1])
      b_no2 <- rbind(b1)
      
    }
    else if (count(as.data.frame(input$stations_no2))==2) {
      b1 <- readRDS(b_string_no2[1])
      b2 <- readRDS(b_string_no2[2])
      b_no2 <- rbind(b1,b2)
      
    }
    else if (count(as.data.frame(input$stations_no2))==3){
      b1 <- readRDS(b_string_no2[1])
      b2 <- readRDS(b_string_no2[2])
      b3 <- readRDS(b_string_no2[3])
      b_no2 <- rbind(b1,b2,b3)
      
    }
    
    b_string_pm10 <- paste(toupper(input$stations_pm10),"_",toupper(input$pollutant)
                           ,".rds",sep="")
    if (count(as.data.frame(input$stations_pm10))==0){
      b_pm10 <- rbind()
    }
    else if (count(as.data.frame(input$stations_pm10))==1){
      b1_pm10 <- readRDS(b_string_pm10[1])
      b_pm10 <- rbind(b1_pm10)
    }
    else if (count(as.data.frame(input$stations_pm10))==2) {
      b1_pm10 <- readRDS(b_string_pm10[1])
      b2_pm10 <- readRDS(b_string_pm10[2])
      b_pm10 <- rbind(b1_pm10,b2_pm10)
    }
    else if (count(as.data.frame(input$stations_pm10))==3){
      b1_pm10 <- readRDS(b_string_pm10[1])
      b2_pm10 <- readRDS(b_string_pm10[2])
      b3_pm10 <- readRDS(b_string_pm10[3])
      b_pm10 <- rbind(b1_pm10,b2_pm10,b3_pm10)
    }
    
    b_string_pm2.5 <- paste(toupper(input$stations_pm2.5),"_",toupper(input$pollutant)
                            ,".rds",sep="")
    if (count(as.data.frame(input$stations_pm2.5))==0){
      b_pm2.5 <- rbind()
    }
    else if (count(as.data.frame(input$stations_pm2.5))==1){
      b1_pm2.5 <- readRDS(b_string_pm2.5[1])
      b_pm2.5 <- rbind(b1_pm2.5)
    }
    else if (count(as.data.frame(input$stations_pm2.5))==2) {
      b1_pm2.5 <- readRDS(b_string_pm2.5[1])
      b2_pm2.5 <- readRDS(b_string_pm2.5[2])
      b_pm2.5 <- rbind(b1_pm2.5,b2_pm2.5)
    }
    else if (count(as.data.frame(input$stations_pm2.5))==3){
      b1_pm2.5 <- readRDS(b_string_pm2.5[1])
      b2_pm2.5 <- readRDS(b_string_pm2.5[2])
      b3_pm2.5 <- readRDS(b_string_pm2.5[3])
      b_pm2.5 <- rbind(b1_pm2.5,b2_pm2.5,b3_pm2.5)
    }
    
    b_string_so2 <- paste(toupper(input$stations_so2),"_",toupper(input$pollutant)
                          ,".rds",sep="")
    if (count(as.data.frame(input$stations_so2))==0){
      b_so2 <- rbind()
    }
    else if (count(as.data.frame(input$stations_so2))==1){
      b1_so2 <- readRDS(b_string_so2[1])
      b_so2 <- rbind(b1_so2)
    }
    else if (count(as.data.frame(input$stations_so2))==2) {
      b1_so2 <- readRDS(b_string_so2[1])
      b2_so2 <- readRDS(b_string_so2[2])
      b_so2 <- rbind(b1_so2,b2_so2)
    }
    else if (count(as.data.frame(input$stations_so2))==3){
      b1_so2 <- readRDS(b_string_so2[1])
      b2_so2 <- readRDS(b_string_so2[2])
      b3_so2 <- readRDS(b_string_so2[3])
      b_so2 <- rbind(b1_so2,b2_so2,b3_so2)
    }
    
    
    
    if ((toupper(input$pollutant)=="PM2.5")){
      b_pm2.5
    }
    else if (toupper(input$pollutant)=="PM10"){
      b_pm10
    }
    else if (toupper(input$pollutant)=="SO2"){
      b_so2
    }
    else if (toupper(input$pollutant)=="NO2"){
      b_no2
    }
    },error = function(e) {showNotification(
"Pollutant not available for the selected Station(s).Please clear the Station field for previously selected Pollutant(s)",
      '',type = "error")
     return()
     }
     )
  })
  aqs_criterion <- reactive({
    if (toupper(input$pollutant)=="PM10" && input$aggregation=="daily_averages"){
      50
    }
    else if(toupper(input$pollutant)=="SO2" && input$aggregation=="raw_hourly_data"){
      350
    }
    else if(toupper(input$pollutant)=="SO2" && input$aggregation=="daily_averages"){
      125
    }
    else if(toupper(input$pollutant)=="SO2" && input$aggregation=="hours_year_threshold"){
      24
    }
    else if(toupper(input$pollutant)=="NO2" && input$aggregation=="raw_hourly_data"){
      200
    }
    else if(toupper(input$pollutant)=="NO2" && input$aggregation=="hours_year_threshold"){
      18
    }
    else {
      NULL
    }
  })

   output$plot <- renderPlot( {
     if (toupper(input$pollutant)=="NONE"){
       return(NULL)}
     
     if (is.null(dataset())){return(NULL)}
    else  {
     temp <- dataset() %>% inner_join(stations_data,by=c("AirQualityStationEoICode"="EoICode")) %>%
       select(Year,Month,Day,Hour,Concentration,StationName) %>% 
       mutate(calendar_year=decimal_date(ymd(paste(Year, Month, Day, sep="-")))) %>%
       mutate(actual_date=as.Date(paste(Year, Month, Day, sep="-"))) %>%
       mutate(day_within_year=as.Date(strptime((paste(Month, Day, sep="/")), '%m/%d'))) %>%
       mutate(day_within_week=(wday(actual_date)-1) + (Hour/24)) 
     temp4 <- temp %>%
       filter(!(is.na(Concentration))) %>%
       select(Year,Month,Day,Hour,Concentration,StationName) %>% 
       mutate(actual_date=as.Date(paste(Year, Month, Day, sep="-"))) %>%
       mutate(category = ifelse(Concentration>input$threshold, 1, 0)) %>% 
       select (c(actual_date,StationName,category,Hour)) %>% distinct() %>%
       group_by(actual_date,StationName) %>% 
       summarise(hour_sum=sum(category)) %>%
       mutate(day_within_year=(as.Date(strptime((format(actual_date,'%m/%d')), '%m/%d')))) %>%
       mutate(day_within_week=(wday(actual_date)-1))
     temp5 <- temp %>%
       filter(!(is.na(Concentration))) %>%
       mutate(actual_date=as.Date(paste(Year, Month, Day, sep="-"))) %>%
       mutate(category = ifelse(Concentration>input$threshold, 1, 0)) %>% 
       select (c(actual_date,StationName,category,Hour)) %>% distinct() %>%
       group_by(year_actual_date=year(actual_date),StationName) %>% 
       summarise(hour_sum=sum(category))
     hourly_title <- paste(toupper(input$pollutant),"(Raw Hourly Data)",sep=" ")
     daily_avg_title <- paste(toupper(input$pollutant),"(Daily Average)",sep=" ")
     daily_max_title <- paste(toupper(input$pollutant),"(Daily Maxima)",sep=" ")
     day_threshold_title <- paste(toupper(input$pollutant),
        "(Number of hours per day for which a given threshold is exceeded)",sep=" ")
     year_threshold_title <- paste(toupper(input$pollutant),
        "(Number of hours per year for which a given threshold is exceeded)",sep=" ")
    }
  if (input$aggregation=="raw_hourly_data" && input$time_hourly=="calendar_time" )  {
  
    ggplot(data=temp) + 
       aes(x=actual_date,  y=Concentration) +
       geom_line(aes(colour=StationName)) +
      scale_x_date(breaks = seq(as.Date("2013-01-01"), as.Date("2019-12-31"), by="1 years"), 
                   labels=date_format("%Y"))+
      geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
      labs(x = "Date",y="Concentration",subtitle = hourly_title,
           title = "Plotted as time series against calendar time")
      
  }
  else if (input$aggregation=="raw_hourly_data" && input$time_hourly=="date_within_year" )  {
       ggplot(data=temp) + 
         aes(x=day_within_year,  y=Concentration) +
         geom_point(aes(colour=StationName))+
         scale_x_date(breaks = seq(min(temp$day_within_year,na.rm = TRUE), 
                                   max(temp$day_within_year,na.rm = TRUE), by="1 months"), 
                      date_labels = "%B")+
      geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
      labs(x = "Month",y="Concentration",subtitle = hourly_title,
           title = "Plotted against hour in the year")
  }
  else if (input$aggregation=="raw_hourly_data" && input$time_hourly=="day_within_week" )  {
    ggplot(data=temp) + 
      aes(x=day_within_week,  y=Concentration) +
      geom_point(aes(colour=StationName))+
      scale_x_continuous(breaks=seq(0,6,1),
                         labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
      geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
      labs(x = "Day",y="Concentration",subtitle = hourly_title,
           title = "Plotted against hour in the week")
  }
  else if (input$aggregation=="raw_hourly_data" && input$time_hourly=="hour_day" )  {
    ggplot(data=temp) + 
      aes(x=Hour,  y=Concentration) +
      geom_point(aes(colour=StationName)) +
      scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1))+
      geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
      labs(x = "Hour",y="Concentration",subtitle = hourly_title,
           title = "Plotted against hour in the day")
     }
  else if (input$aggregation=="daily_averages")  {
    temp2 <- temp  %>%
      filter(!(is.na(Concentration))) %>%
      select(Year,Month,Day,Hour,Concentration,StationName) %>% 
      mutate(actual_date=as.Date(paste(Year, Month, Day, sep="-"))) %>%
      group_by(actual_date,StationName) %>% 
      summarise(avg_concentration=mean(Concentration)) %>%
      mutate(day_within_week=wday(actual_date, label=TRUE))
    
    ggplot(data=temp2) + 
      aes(x=day_within_week,  y=avg_concentration) +
      geom_point(aes(colour=StationName))+
      scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
      geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
      labs(x = "Day",y="Concentration",subtitle = daily_avg_title,
           title = "Plotted against day in the week")
    
       }
     else if (input$aggregation=="daily_maxima")  {
       temp3 <- temp %>%
         filter(!(is.na(Concentration))) %>%
         select(Year,Month,Day,Hour,Concentration,StationName) %>% 
         mutate(actual_date=as.Date(paste(Year, Month, Day, sep="-"))) %>%
         group_by(actual_date,StationName) %>% 
         summarise(max_concentration=max(Concentration)) %>%
         mutate(day_within_week=wday(actual_date, label=TRUE))
       
       ggplot(data=temp3) + 
         aes(x=day_within_week,  y=max_concentration) +
         geom_point(aes(colour=StationName))+
         scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
         labs(x = "Day",y="Concentration",subtitle = daily_max_title,
              title = "Plotted against day in the week")
     } 
     else if (input$aggregation=="hours_day_threshold" && input$hours_day_threshold=="calendar_time"){
       ggplot(data=temp4) + 
           aes(x=actual_date,  y=hour_sum) +
           geom_line(aes(colour=StationName))+
         scale_x_date(breaks = seq(as.Date("2013-01-01"), as.Date("2019-12-31"), by="1 years"), 
                      labels=date_format("%Y"))+
         labs(x = "Date",y="Hours",title = day_threshold_title)
       }
    else if (input$aggregation=="hours_day_threshold" && input$hours_day_threshold=="date_within_year"){
         ggplot(data=temp4) + 
           aes(x=day_within_year,  y=hour_sum) +
           geom_point(aes(colour=StationName))+
           scale_x_date(breaks = seq(min(temp4$day_within_year,na.rm = TRUE), 
                                     max(temp4$day_within_year,na.rm = TRUE), by="1 months"), 
                        date_labels = "%B")+
        labs(x = "Month",y="Hours",title = day_threshold_title)
       }
    else if (input$aggregation=="hours_day_threshold" && input$hours_day_threshold=="day_within_week"){
         ggplot(data=temp4) + 
           aes(x=day_within_week,  y=hour_sum) +
           geom_point(aes(colour=StationName))+
           scale_x_continuous(breaks=seq(0,6,1),
                              labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
        labs(x = "Day",y="Hours",title = day_threshold_title)
       }
     else if (input$aggregation=="hours_year_threshold"){
       ggplot(data=temp5) + 
         aes(x=year_actual_date,  y=hour_sum) +
         geom_line(aes(colour=StationName))+
         scale_x_continuous(breaks=seq(2013,2019,1))+
         geom_hline(yintercept=aqs_criterion(), linetype="dashed", color = "black")+
         labs(x = "Date",y="Hours",title = year_threshold_title)
       }
   } )
   output$map <- renderPlot( {
     if (toupper(input$pollutant)=="NONE"){
       return(NULL)}
     
     if (is.null(dataset())){return(NULL)}
     else  {
       stations_data_selected <- stations_data %>% 
         filter( EoICode %in% unique(dataset()$AirQualityStationEoICode))
       
      ggplot() + geom_polygon(data=map_data("world", "Czech Republic"), aes(x=long, y = lat, group = group)) + 
         coord_fixed(1.3)+ 
         geom_point(data = stations_data_selected, aes(x = Longitude, y = Latitude,
                                                       colour=StationName))+
        labs(title = "Plot with the location of the measuring station(s)")
     }
   })
   output$table <- DT::renderDataTable({
     
     if (toupper(input$pollutant)=="NONE"){
       return(NULL)}
     
     if (is.null(dataset())){return(NULL)}
     else  {
       stations_data_selected <- stations_data %>% 
         filter( EoICode %in% unique(dataset()$AirQualityStationEoICode))
       
       stations_data_transpose <- as.data.frame(t(as.matrix(stations_data_selected)))
       names(stations_data_transpose) <- paste("Station",1:ncol(stations_data_transpose))
       stations_data_transpose
     }
   })
   output$download1 <- downloadHandler(
     filename = "Stations_data.csv",
     content = function(file) {
       if (toupper(input$pollutant)=="NONE"){
         return(NULL)}
       
       if (is.null(dataset())){return(NULL)}
       else  {
         stations_data_selected <- stations_data %>% 
           filter( EoICode %in% unique(dataset()$AirQualityStationEoICode))
         
         stations_data_transpose <- as.data.frame(t(as.matrix(stations_data_selected)))
         names(stations_data_transpose) <- paste("Station",1:ncol(stations_data_transpose))
       write.csv(stations_data_transpose, file, )
       
       }
     }
    )
   output$downloadReportButton <- downloadHandler( 
     filename = "report.docx",
     content = function(file) {
       render("report.Rmd", 
              output_format="word_document", output_file=file
              , params=list(dt_dataset=dataset(),
                            dt_aqs_criterion=aqs_criterion(),
                            aggregation=input$aggregation,
                            time_hourly=input$time_hourly,
                            hours_day_threshold=input$hours_day_threshold,
                            pollutant=input$pollutant,
                            threshold=input$threshold),
              envir = new.env(parent = globalenv())
              )
  }
  )
  }
  

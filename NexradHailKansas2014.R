###Program to download SWDI data on hail
#Start off by specifying the working directory as well as calling 7 libraries:
#   
# sqldf: Library for using SQL code
# RColorBrewer: A color palette library
# leaflet: An interface to the Leaflet.js library
# googleVis: An interface to the Google Charts library
# rgdal:Library for manipulating spatial vector files

setwd("~/StormProject")

library(sqldf)
library(RColorBrewer)
library(leaflet)
library(googleVis)
library(rgdal)

swdi_pull <- function(start_date,end_date,series){
  
  ##translate the string into a date, and range
  start <- as.Date(start_date,"%Y-%m-%d")
  range <- as.numeric(as.Date(end_date,"%Y-%m-%d")-as.Date(start_date,"%Y-%m-%d"))
  
  ##Placeholder for the result
  raw <- data.frame()
  
  ##Loop through Day 0 through the full range of days
  for(i in seq(0, range, 30)){
    
    ##Load in parameters, hit API
    print(i)
    period <- start + i
    increment0 <- paste(format(period,"%Y"),format(period,"%m"),format(period,"%d"),sep="")
    increment1 <- paste(format(period+30,"%Y"),format(period+30,"%m"),format(period+30,"%d"),sep="")
    temp <- read.csv(paste("http://www.ncdc.noaa.gov/swdiws/csv/",series,"/",
                           increment0,":",increment1,sep=""))
    
    ##If the API kicks back a result
    if(ncol(temp)!=1 && colnames(temp)[1]!="summary"){
      raw <- rbind(raw,temp)
      raw <- raw[!is.na(raw$LAT),]
    }
  }
  
  ##Clean up time steps -- remove data outside of specified period
  raw$DATE<-as.Date(substr(raw$ZTIME,1,10),"%Y-%m-%d")
  raw<-raw[raw$DATE<=as.Date(end_date,"%Y-%m-%d"),]
  raw$HOUR<-substr(raw$ZTIME,12,13)
  raw<-raw[,c("ZTIME","DATE","HOUR","WSR_ID","CELL_ID","PROB","SEVPROB","MAXSIZE","LAT","LON")]
  return(raw)
}

##Set Parameters
start_date = "2007-01-01"
end_date = "2015-12-31"
range <- as.Date(end_date,"%Y-%m-%d")-as.Date(start_date,"%Y-%m-%d")
series = "nx3hail"

raw <- swdi_pull(start_date,end_date,series)
save(raw, file = "Nexrad_allstates_extract_2007_2015.rda")
##Cut down bounding box to limit data to Kansas (approximately)

#Kansas almost completely covers a rectangle bounded by
#Latitude 37°N to 40°N and Longitude 94°35′ W to 102°3′ W
#except the northeast corner is in Missouri
##Source: https://en.wikipedia.org/wiki/Kansas      
##Latitude approximately equals -94.583 by -103.050 (divide minutes by 60)
raw_Kansas <- raw[raw$LON <= (-94.583) & raw$LON >= (-103.050) 
           & raw$LAT >= 37.000 & raw$LAT <= 40.000, ]
save(raw_Kansas, file = "Nexrad_Kansas_extract_2007_2015.rda")


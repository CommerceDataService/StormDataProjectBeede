###Program to download annual Storm Events details and locations data 
#filter out some of details fields (in particular the text event descriptions
#and merge them together by Episode and Event ID.

setwd("~/StormProject")

library(dplyr, warn.conflicts = FALSE)
library(R.utils)

#later use RE's to make the construction of data file names more flexible as the
#files get updated

for(yearly in 2008:2015){
  for(series in c("details", "locations")){
    URL <- "http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
    
    if (yearly != 2015){
       filenogz <- paste("StormEvents_", series, "-ftp_v1.0_d", yearly, "_c20160223.csv",
                      sep="")
    } else {
       filenogz <- paste("StormEvents_", series, "-ftp_v1.0_d", yearly, "_c20160318.csv",
                      sep="")
    }   
    filegz <- paste(filenogz,".gz", sep="")
    newfilename1 <- paste("StormEvents", series, sep="_")
    download.file(paste(URL, filegz, sep=""), destfile=filegz)
    gunzip(filegz, overwrite = TRUE, exdir=getwd)
    assign(newfilename1, read.csv(filenogz))
  }
  # StormEvents_details <- select(StormEvents_details, BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, END_YEARMONTH, 
  #     END_DAY, END_TIME, EPISODE_ID, EVENT_ID, STATE, STATE_FIPS, EVENT_TYPE, 
  #     CZ_TYPE, CZ_FIPS, CZ_NAME, CZ_TIMEZONE, 
  #     INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
  #     DAMAGE_CROPS, SOURCE, MAGNITUDE, BEGIN_LAT, BEGIN_LON, END_LAT, END_LON, 
  #     EPISODE_TITLE, EPISODE_NARRATIVE, EVENT_NARRATIVE)

  print(dim(StormEvents_details))
  print(length(unique(StormEvents_details$EVENT_ID)))
  print(dim(StormEvents_locations))
  print(length(unique(StormEvents_locations$EVENT_ID)))
  print(sum(is.na(StormEvents_locations$EVENT_ID)))
  print(sum(is.na(StormEvents_locations$LATITUDE)))
  
  merged_SE <- merge(StormEvents_details, StormEvents_locations,
                     id = c("EPISODE_ID","EVENT_ID"))
  save(merged_SE, file = paste("merged_SE_", yearly, ".rda", sep=""))
}

  

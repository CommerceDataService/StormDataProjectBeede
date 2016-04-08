###Program to download annual Storm Events data
#Start off by specifying the working directory 
#   

setwd("~/StormProject")

#save output to a file
con <- file("StormEvents_2008_2015.log")
sink(con, type = "output")


library(dplyr, warn.conflicts = FALSE)
library(R.utils)

for(series in c("details", "locations", "fatalities")){
  
  for(yearly in c(2008:2015)){
    URL <- "http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
    filegz <- paste("StormEvents_", series, "-ftp_v1.0_d", yearly, "_c20160223.csv.gz",
                    sep="")
    filenogz <- paste("StormEvents_", series, "-ftp_v1.0_d", yearly, "_c20160223.csv",
                      sep="")
    newfilename <- paste("StormEvents_", series, "_", yearly, sep="")
    download.file(paste(URL, filegz, sep=""), destfile=filegz)
    gunzip(filegz, overwrite = TRUE, exdir=getwd)
    SData <- read.csv(filenogz)
#    fileSDdata <- paste(filenogz, ".rda", sep="")
    save(SData, file = paste(newfilename, ".rda", sep=""))
    
    } 
}


  

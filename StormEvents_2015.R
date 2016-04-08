###Program to download annual Storm Events data
#Start off by specifying the working directory 
#   

setwd("~/StormProject")

#save output to a file
# con <- file("StormEvents_2015.log")
# sink(con, type = "output")


library(dplyr, warn.conflicts = FALSE)
library(R.utils)

for(yearly in 2015:2015){
  for(series in c("details", "locations")){
    URL <- "http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
    filenogz <- paste("StormEvents_", series, "-ftp_v1.0_d", yearly, "_c20160318.csv",
                      sep="")
    filegz <- paste(filenogz,".gz", sep="")
    newfilename1 <- paste("StormEvents", series, sep="_")
    download.file(paste(URL, filegz, sep=""), destfile=filegz)
    gunzip(filegz, overwrite = TRUE, exdir=getwd)
    assign(newfilename1, read.csv(filenogz))
  } 
  merged_SE <- full_join(StormEvents_details, StormEvents_locations)
  save(merged_SE, file = paste("merged_SE_", yearly, ".rda", sep=""))
  
}

  

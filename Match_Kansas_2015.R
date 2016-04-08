setwd("~/StormProject")

#save output to a file
# con <- file("Match_Kansas_2015.log")
# sink(con, type = "output")

library(dplyr)

load("StormEvents_locations_2015.rda")
dim(SData)
# unlink("Match_Kansas_2015.log")

fraction = 0.25
##Round coordinates
SData$LON <- round(SData$LONGITUDE/fraction)*fraction
SData$LAT <- round(SData$LATITUDE/fraction)*fraction

deduped_events <- SData %>% 
  group_by(YEARMONTH, EPISODE_ID, EVENT_ID, LON, LAT) %>% 
  summarise(min_loc_index = min(LOCATION_INDEX), max_loc_index = max(LOCATION_INDEX))

#get count of instances if each LON/LAT
count_loc <- count(deduped_events, LON, LAT, sort=TRUE)


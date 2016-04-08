setwd("~/StormProject")

#save output to a file
# con <- file("Match_2015.log")
# sink(con, type = "output")

library(dplyr)

load("StormEvents_locations_2015.rda")
dim(SData)
x <- count(SData, EPISODE_ID, EVENT_ID)
# unlink("Match_2015.log")

fraction = 0.25
##Round coordinates
SData$LON <- round(SData$LONGITUDE/fraction)*fraction
SData$LAT <- round(SData$LATITUDE/fraction)*fraction

deduped_events <- SData %>% 
  group_by(YEARMONTH, EPISODE_ID, EVENT_ID, LON, LAT) %>% 
  summarise(min_loc_index = min(LOCATION_INDEX), max_loc_index = max(LOCATION_INDEX))

#get count of instances if each LON/LAT
count_loc <- count(deduped_events, LON, LAT, sort=TRUE)
x <- sort(count_loc$n, decreasing = TRUE)

load("DeDuplicateHailData_2015.rda")

merged_hail <- full_join(deduped_day, deduped_events)


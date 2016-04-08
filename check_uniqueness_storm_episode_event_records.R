setwd("~/StormProject")

#save output to a file
# con <- file("Match_2015.log")
# sink(con, type = "output")

library(dplyr)

load("StormEvents_locations_2015.rda")
dim(SData)
x <- count(SData, EPISODE_ID, EVENT_ID)
dim(x)

load("StormEvents_details_2015.rda")
dim(SData)
x <- count(SData, EPISODE_ID, EVENT_ID)
dim(x)
#program to deduplicate hail data (multiple readings from different stations for same
#hail event)

setwd("~/StormProject")

#save output to a file
con <- file("DeDuplicateHailData_Kansas_2015.log")
sink(con, type = "output")

library(dplyr)


fraction = 0.25
load("Nexrad_Hail_Kansas2015.rda")
##Round coordinates
raw$LON <- round(raw$LON/fraction)*fraction
raw$LAT <- round(raw$LAT/fraction)*fraction
summary(raw$PROB)
##De-duplicate by day, latitude and longitude

# deduped_day <- sqldf("SELECT DATE, LON, LAT, MAXSIZE
#                      FROM raw
#                      GROUP BY DATE, LON, LAT, MAXSIZE")


deduped_day <- raw %>% group_by(DATE, LON, LAT) %>% summarise(minSEVPROB = min(SEVPROB), 
    maxSEVPROB = max(SEVPROB), minMAXSIZE = min(MAXSIZE), maxMAXSIZE = max(MAXSIZE))
save(deduped_day, file = "DeDuplicateHailData_Kansas_2015.rda")
##prepare 2015 Storm Events data for Hail to be merged with SWDI Hail data for 2015
setwd("~/StormProject")
load("merged_SE_2015.rda")
library(dplyr)
library(lubridate)
print(dim(merged_SE)) 
merged_SE <- mutate(merged_SE, EVTYPEcap = toupper(EVENT_TYPE))
print(dim(merged_SE))
merged_SE <- merged_SE[grep("HAIL", merged_SE$EVTYPEcap), ]
print(dim(merged_SE))
# summary(merged_SE_HAIL$MAGNITUDE)
# sum(is.na(merged_SE_HAIL$MAGNITUDE))
# table(merged_SE_HAIL$STATE)
# table(merged_SE_HAIL$STATE_FIPS)
# sum(is.na(merged_SE_HAIL$STATE_FIPS))

#Cut down bounding box to continental US not including Alaska
merged_SE <- merged_SE[merged_SE$LONGITUDE<(-50) & merged_SE$LONGITUDE>(-140) & 
                         merged_SE$LATITUDE > 25 & merged_SE$LATITUDE <= 49,]


print(dim(merged_SE))
print(table(merged_SE$CZ_TIMEZONE))
                    
# merged_SE <- mutate(merged_SE, 
#                     new_beg_date = with_tz(dmy_hms(BEGIN_DATE_TIME), "timezone"))
 # merged_SE <- mutate(merged_SE, 
 #         new_beg_date = paste(as.character(BEGIN_DATE_TIME), 
 #         substr(as.character(CZ_TIMEZONE), 1, 3), sep = " "))
 # merged_SE <- mutate(merged_SE, 
 #  new_beg_date = with_tz(dmy_hms(new_beg_date, tz=as.character(timezone), "GMT"))
             
  # new_beg_date = ymd(with_tz(paste(as.character(BEGIN_DATE_TIME), 
  #                                  substr(CZ_TIMEZONE, 1, 3), sep = " "))))
# merged_SE <- mutate(merged_SE, DATE = as.Date(paste("2015", 
#                           substr(as.character(merged_SE$BEGIN_YEARMONTH), 5, 6),
#                           as.character(merged_SE$BEGIN_DAY), sep="-")))
# merged_SE <- mutate(merged_SE, DATE = as.Date(paste("2015", merged_SE <- mutate(merged_SE, 
merged_SE <- mutate(merged_SE, 
                    timezone = substr(as.character(CZ_TIMEZONE), 1, 3))
olson_tz <- c("America/Chicago","America/New_York","America/Denver","America/Los_Angeles")
list_tz <- c("CST","EST", "MST", "PST")

for(i in seq_along(list_tz)){
  print(i)
  print(list_tz[i])
  print(olson_tz[i])
  x <- which(merged_SE$timezone == list_tz[i])
  print(length(x))
  print(x[1:10])
  print(length(merged_SE$BEGIN_DATE_TIME[x]))
  merged_SE$new_beg_date[x] <-
    as.POSIXlt(dmy_hms(as.character(merged_SE$BEGIN_DATE_TIME[x]), tz = olson_tz[i]))
  merged_SE$new_beg_date_gmt[x] <-
    with_tz(merged_SE$new_beg_date[x],"GMT")

}
# 
#   
# merged_SE <- mutate(new_beg_date = replace(new_beg_date, , NA))
# merged_SE$new_beg_date[grep("EST", merged_SE$timezone)] = 
#   with_tz(dmy_hms(as.character(merged_SE$BEGIN_DATE_TIME), 
#                   tz = "America/New_York"),"GMT")
# merged_SE$olson_tz[grep("CST", merged_SE$olson_tz)] = "America/Chicago"
# merged_SE$olson_tz[grep("MST", merged_SE$olson_tz)] = "America/Denver"
# merged_SE$olson_tz[grep("PST", merged_SE$olson_tz)] = "America/Los_Angeles"
# 
# print(table(merged_SE$olson_tz))
# merged_SE$new_beg_date = dmy_hms(merged_SE$BEGIN_DATE_TIME, tz = CST)
# with_tz(dmy_hms(BEGIN_DATE_TIME, tz="olson_tz"), tzone = "UTC"))
#                            substr(as.character(BEGIN_YEARMONTH), 5, 6),
#                            as.character(BEGIN_DAY), sep="-")))


##Does one-quarter degree make the most sense in this context?
fraction = 0.25
##Round coordinates
merged_SE <- mutate(merged_SE, LON = round(LONGITUDE/fraction)*fraction,
                    LAT = round(LATITUDE/fraction)*fraction)
sum(is.na(merged_SE$INJURIES_DIRECT))
sum(is.na(merged_SE$INJURIES_INDIRECT))

merged_SE <- mutate(merged_SE, 
                    injuries = as.numeric((INJURIES_DIRECT + INJURIES_INDIRECT) != 0))
sum(is.na(merged_SE$DEATHS_DIRECT))
sum(is.na(merged_SE$DEATHS_INDIRECT))
merged_SE <- mutate(merged_SE, 
                    deaths = as.numeric((DEATHS_DIRECT + DEATHS_INDIRECT) != 0))
merged_SE <- mutate(merged_SE, DAMAGE_PROPERTY = as.character(DAMAGE_PROPERTY),
                    DAMAGE_CROPS = as.character(DAMAGE_CROPS))
merged_SE <- mutate(merged_SE, property = as.numeric((substr(DAMAGE_PROPERTY, 1, 4) != "0.00")
                    & (DAMAGE_PROPERTY != "") & (DAMAGE_PROPERTY != " ")))
merged_SE <- mutate(merged_SE, crops = as.numeric((substr(DAMAGE_CROPS, 1, 4) != "0.00")
                    & (DAMAGE_CROPS != "") & (DAMAGE_CROPS != " ")))
merged_SE <- mutate(merged_SE, anydamage = as.numeric((injuries == 1) | (deaths == 1) | 
                                             (property == 1) | (crops == 1)))
titlelist <- c("INJURIES_DIRECT", "INJURIES_INDIRECT", 
  "DEATHS_DIRECT",  "DEATHS_INDIRECT", "DAMAGE_PROPERTY", "DAMAGE_CROPS", 
  "injuries", "deaths", "property", "crops", "anydamage")
merged_SE[1:50, titlelist]

#dedupe the data by DATE, LAT, LON
merged_SE <- merged_SE %>% group_by(DATE, LON, LAT) %>% 
  summarise(mininjuries = min(injuries), maxinjuries = max(injuries), 
            mindeaths = min(deaths), maxdeaths = max(deaths), 
            minproperty = min(property), maxproperty = max(property), 
            mincrops = min(crops), maxcrops = max(crops), 
            minanydamage = min(anydamage), maxanydamage = max(anydamage))
merged_SE <- ungroup(merged_SE)
merged_SE <- as.data.frame(merged_SE)
merged_SE$inSE <- 1
print(dim(merged_SE))
print(summary(merged_SE))
print(table(merged_SE$minanydamage, merged_SE$maxanydamage))
save(merged_SE, file = "DeDuplicateHailStormEventData_2015.rda")
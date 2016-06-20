#merge_ACS_SWDI_SE_LODES.R
setwd("~/StormProject")
library(dplyr)
library(lubridate)
load(file="SWDI_SE_ACS.rda")
dim(SWDI_SE_ACS)
SWDI_SE_ACS <- mutate(SWDI_SE_ACS, Tract_GEOID = as.character(Tract_GEOID))
load(file = "~/StormProject/LODES/LodesData2011.rda")
dim(lodesdf)
lodesdf <- rename(lodesdf, Tract_GEOID = tractID)
lodesdf <- select(lodesdf, Tract_GEOID, tractemp, countyemp)
SWDI_SE_ACS_LODES <- left_join(SWDI_SE_ACS, lodesdf)
SWDI_SE_ACS_LODES$tractemp[is.na(SWDI_SE_ACS_LODES$tractemp)] <- 0
SWDI_SE_ACS_LODES$countyemp[is.na(SWDI_SE_ACS_LODES$countyemp)] <- 0

#more RHS variables
SWDI_SE_ACS_LODES <- mutate(SWDI_SE_ACS_LODES, 
                      Tract_empdens = tractemp/Tract_ALAND_SQMI, 
                      County_empdens = countyemp/County_ALAND_SQMI
) 
SWDI_SE_ACS_LODES$datamonth <- 
        as.factor(month(SWDI_SE_ACS_LODES$DATE, label=TRUE, abbr = TRUE))

dim(SWDI_SE_ACS_LODES)
save(SWDI_SE_ACS_LODES, file="SWDI_SE_ACS_LODES.rda")
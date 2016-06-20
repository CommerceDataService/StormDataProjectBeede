#merge_ACS_SWDI_SE_LODES.R
setwd("~/StormProject")
library(dplyr)
library(lubridate)
load(file="SWDI_SE_ACS_1deg.rda")
dim(SWDI_SE_ACS_1deg)
SWDI_SE_ACS_1deg <- mutate(SWDI_SE_ACS_1deg, Tract_GEOID = as.character(Tract_GEOID))
load(file = "~/StormProject/LODES/LodesData2011.rda")
dim(lodesdf)
lodesdf <- rename(lodesdf, Tract_GEOID = tractID)
lodesdf <- select(lodesdf, Tract_GEOID, tractemp, countyemp)
SWDI_SE_ACS_LODES_1deg <- left_join(SWDI_SE_ACS_1deg, lodesdf)
SWDI_SE_ACS_LODES_1deg$tractemp[is.na(SWDI_SE_ACS_LODES_1deg$tractemp)] <- 0
SWDI_SE_ACS_LODES_1deg$countyemp[is.na(SWDI_SE_ACS_LODES_1deg$countyemp)] <- 0

#more RHS variables
SWDI_SE_ACS_LODES_1deg <- mutate(SWDI_SE_ACS_LODES_1deg, 
                      Tract_empdens = tractemp/Tract_ALAND_SQMI, 
                      County_empdens = countyemp/County_ALAND_SQMI
) 
SWDI_SE_ACS_LODES_1deg$datamonth <- 
        as.factor(month(SWDI_SE_ACS_LODES_1deg$DATE, label=TRUE, abbr = TRUE))

dim(SWDI_SE_ACS_LODES_1deg)
save(SWDI_SE_ACS_LODES_1deg, file="SWDI_SE_ACS_LODES_1deg.rda")
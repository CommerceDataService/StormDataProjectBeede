#Prepare ACS data for merging with NOAA data
#Round coordinates and summarize to de-duplicate
library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
setwd("~/StormProject")

load("ACS_2006_2010.rda")
fraction = 0.25

##Round coordinates
all_acs_gaz$LON <- round(all_acs_gaz$INTPTLONG/fraction)*fraction
all_acs_gaz$LAT <- round(all_acs_gaz$INTPTLAT/fraction)*fraction

all_acs_gaz <- mutate(all_acs_gaz, popxincome = B19013_001*POP10)


all_acs_gaz_round <- all_acs_gaz %>% group_by(LON, LAT) %>% 
        summarise(tot_income = sum(popxincome, na.rm=TRUE),
                  tot_pop = sum(POP10, na.rm=TRUE),
                  tot_area = sum(ALAND_SQMI, na.rm=TRUE))
all_acs_gaz_round <- mutate(all_acs_gaz_round, mean_income = tot_income/tot_pop,
                            pop_dens = tot_pop/tot_area)

print(summary(all_acs_gaz_round))

save(all_acs_gaz_round, file = "ACS_2006_2010_round.rda")

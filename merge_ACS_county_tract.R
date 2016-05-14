#program merges ACS county and tract data
library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
setwd("~/StormProject")

load(file = "ACS_2006_2010_counties.rda")
all_acs_gaz_counties <- select(all_acs_gaz_counties, -inacs, -ingaz)
names(all_acs_gaz_counties) <- paste0("County_", names(all_acs_gaz_counties))
all_acs_gaz_counties <- mutate(all_acs_gaz_counties, inTract = 1)

load(file = "ACS_2006_2010.rda")
all_acs_gaz <- select(all_acs_gaz, -inacs, -ingaz)
names(all_acs_gaz) <- paste0("Tract_", names(all_acs_gaz))
all_acs_gaz <- mutate(all_acs_gaz, County_GEOID = substr(Tract_GEOID, 1, 5),
                      inCounty = 1)

acs_County_Tract <- merge(all_acs_gaz_counties, all_acs_gaz,all = TRUE)
print("table of inCounty vs. inTract")
print(table(acs_County_Tract$inCounty, acs_County_Tract$inTract, useNA = "ifany"))
acs_County_Tract <- select(acs_County_Tract, -County_Row.names, -Tract_Row.names,
                           -inCounty, -inTract)
save(acs_County_Tract, file = "ACS_2006_2010_counties_Tracts.rda")
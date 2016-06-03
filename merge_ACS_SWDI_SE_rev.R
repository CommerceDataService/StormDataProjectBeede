#merge_ACS_SWDI_SE_rev.R
#Merge ACS data with SWDI data
#By Census tract
setwd("~/StormProject") 

library(dplyr, warn.conflicts = TRUE, quietly = FALSE)

load(file = "ACS_2006_2010_counties_Tracts.rda")
acs_County_Tract <- mutate(acs_County_Tract, inACS = 1)
#dimensions of ACS data
print(dim(acs_County_Tract))

load("SWDI_SE_geo.rda")
#dimensions of SWDI_SE data
print(dim(S_geocoded))
S_geocoded <- filter(S_geocoded, is.na(FIPS) == FALSE)
S_geocoded <- mutate(S_geocoded, inSWDI_SE = 1, 
                     GEOID = substr(FIPS, 1, 11))
#dimensions of SWDI_SE data
print(dim(S_geocoded))

SWDI_SE_ACS <- merge(acs_County_Tract, S_geocoded, by.x = "Tract_GEOID",
                     by.y = "GEOID", all = TRUE)
#table of inACS vs. inSWDI_SE
print(table(SWDI_SE_ACS$inACS, SWDI_SE_ACS$inSWDI_SE, useNA = "ifany"))

test <- SWDI_SE_ACS[is.na(SWDI_SE_ACS$inSWDI_SE),]
#number of unique tract GEOIDs that aren't in SWDI_SE data
print(length(unique(test$Tract_GEOID)))
#number of unique county GEOIDs that aren't in SWDI_SE data
print(length(unique(test$County_GEOID)))
rm(test)

SWDI_SE_ACS <- filter(SWDI_SE_ACS, inSWDI_SE == 1)
#dimensions of SWDI_SE_ACS data
print(dim(SWDI_SE_ACS))

#delete records with missing Tract population and median incomes
SWDI_SE_ACS <- 
        filter(SWDI_SE_ACS, !(is.na(Tract_POP10) | is.na(Tract_B19013_001)))
#dimensions of SWDI_SE_ACS data
print(dim(SWDI_SE_ACS))

SWDI_SE_ACS <- select(SWDI_SE_ACS, -County_USPS, -County, 
                      -Tract_USPS, inACS)
print(table(SWDI_SE_ACS$inSE, SWDI_SE_ACS$inSWDI_SE))

#construct RHS variables
SWDI_SE_ACS <- mutate(SWDI_SE_ACS, 
                   Tract_popdens = Tract_POP10/Tract_ALAND_SQMI, 
                   County_popdens = County_POP10/County_ALAND_SQMI,
                   Tract_GEOID = as.factor(Tract_GEOID),
                   County_GEOID = as.factor(County_GEOID),
                   State = as.factor(State_abb),
                   datayear = as.factor(datayear),
   Tract_shr_water = Tract_AWATER_SQMI/(Tract_AWATER_SQMI+Tract_ALAND_SQMI),
   County_shr_water = County_AWATER_SQMI/(County_AWATER_SQMI+County_ALAND_SQMI)
)
save(SWDI_SE_ACS, file="SWDI_SE_ACS.rda")

#percent in SE in main data set
print(100*table(SWDI_SE_ACS$inSE)[2]/sum(table(SWDI_SE_ACS$inSE)))
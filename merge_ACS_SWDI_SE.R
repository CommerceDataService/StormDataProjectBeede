#Merge ACS data with SWDI data
#By Census tract
#and create training/test and 
library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
library(caret)
setwd("~/StormProject")

load(file = "ACS_2006_2010_counties_Tracts.rda")
acs_County_Tract <- mutate(acs_County_Tract, inACS = 1)
print("dimensions of ACS data")
print(dim(acs_County_Tract))

load("SWDI_SE_geo.rda")
print("dimensions of SWDI_SE data")
print(dim(S_geocoded))
S_geocoded <- filter(S_geocoded, is.na(FIPS) == FALSE)
S_geocoded <- mutate(S_geocoded, inSWDI_SE = 1, 
                     GEOID = substr(FIPS, 1, 11))
print("dimensions of SWDI_SE data")
print(dim(S_geocoded))

SWDI_SE_ACS <- merge(acs_County_Tract, S_geocoded, by.x = "Tract_GEOID",
                     by.y = "GEOID", all = TRUE)
print("table of inACS vs. inSWDI_SE")
print(table(SWDI_SE_ACS$inACS, SWDI_SE_ACS$inSWDI_SE, useNA = "ifany"))

test <- SWDI_SE_ACS[is.na(SWDI_SE_ACS$inSWDI_SE),]
print("number of unique tract GEOIDs that aren't in SWDI_SE data")
print(length(unique(test$Tract_GEOID)))
print("number of unique county GEOIDs that aren't in SWDI_SE data")
print(length(unique(test$County_GEOID)))
rm(test)

SWDI_SE_ACS <- filter(SWDI_SE_ACS, inSWDI_SE == 1)
print("dimensions of SWDI_SE_ACS data")
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
#create random number variables 
numobs <- dim(SWDI_SE_ACS)[1]
set.seed(73195520)
SWDI_SE_ACS$randnum1 <- runif(numobs)
save(SWDI_SE_ACS, file="SWDI_SE_ACS.rda")

print("percent in SE in main data set")
print(100*table(SWDI_SE_ACS$inSE)[2]/sum(table(SWDI_SE_ACS$inSE)))

#split the data file into training and test
inTrain <- createDataPartition(y=SWDI_SE_ACS$inSE,
                               p=0.7, list=FALSE)
training <- SWDI_SE_ACS[inTrain,]
print("dimensions of training data set:")
print(dim(training))
print("percent in SE in main data set")
print(100*table(training$inSE)[2]/sum(table(training$inSE)))
save(training, file="training.rda")

testing <- SWDI_SE_ACS[-inTrain,]
print("dimensions of testing data set:")
print(dim(testing))
print("percent in SE in main data set")
print(100*table(testing$inSE)[2]/sum(table(testing$inSE)))
save(testing, file="testing.rda")
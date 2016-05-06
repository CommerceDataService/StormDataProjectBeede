#Storm events - download ACS data
library(acs)
library(dplyr)

#augment state list with DC
statelist <- c(state.abb, "DC")
#loop through 
all_acs <- data.frame()
for(stateabb in statelist){
  print(stateabb)
  tractlist <- geo.make(state=stateabb, county="*", tract="*")
  tractdata <- acs.fetch(2010, span = 5, geography=tractlist, 
                         table.number = "B19013", 
                         key = "33610323e713d6ee2ad71f7b4625dc31fc695b2d")
  state_acs <- merge(estimate(tractdata), geography(tractdata), by.x=0, by.y = "NAME")
  state_acs$GEOID <- paste(as.character(state_acs$state), sprintf("%03d", state_acs$county), 
                         state_acs$tract, sep="")
  state_acs <- select(state_acs, -state, -county, -tract)
  all_acs <- rbind(all_acs, state_acs)
}

gaz <- read.delim("Gaz_tracts_national.txt")
gaz <- select(gaz, -ALAND, -AWATER)
gaz$GEOID <- as.character(gaz$GEOID)
all_acs_gaz <- merge(all_acs, gaz, all.x = TRUE)
x <- all_acs_gaz$POP10 == 0
z <- all_acs_gaz$POP10 == 0
y <- is.na(all_acs_gaz$B19013_001)

print(table(x, y))
print(paste("number of tracts with missing median income = ", 
            sum(y), sep = ""))
print(paste("number of tracts with zero population = ", 
            sum(x), sep = ""))

save(all_acs_gaz, file = "ACS_2006_2010.rda")

fraction = 0.25

##Round coordinates
all_acs_gaz$LON <- round(all_acs_gaz$INTPTLONG/fraction)*fraction
all_acs_gaz$LAT <- round(all_acs_gaz$INTPTLAT/fraction)*fraction
all_acs_gaz_round <- all_acs_gaz %>% group_by(LON, LAT) %>% summarise(minSEVPROB = min(SEVPROB), 
      maxSEVPROB = max(SEVPROB), minMAXSIZE = min(MAXSIZE), maxMAXSIZE = max(MAXSIZE))
#Storm events - download ACS data
library(acs, warn.conflicts = TRUE, quietly = FALSE)
library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
setwd("~/StormProject")

#augment state list with DC
statelist <- c(state.abb, "DC")
#loop through states and get tract-level estimates of median hh income
all_acs <- data.frame()
#stateabb <- "AL"
for(stateabb in statelist){
  print(stateabb)
  countylist <- geo.make(state=stateabb, county="*")
  countydata <- acs.fetch(2010, span = 5, geography=countylist, 
                         table.number = "B19013", 
                         key = "33610323e713d6ee2ad71f7b4625dc31fc695b2d")
  state_acs <- merge(estimate(countydata), geography(countydata), by.x=0, by.y = "NAME")
  state_acs$GEOID <- paste(sprintf("%02d", state_acs$state), 
                           state_acs$county, 
                           sep="")
  state_acs <- select(state_acs, -state, -county)
  all_acs <- rbind(all_acs, state_acs)
}
all_acs <- mutate(all_acs, inacs = 1)
print("number of missing cells in all_acs")
print(sum(is.na(all_acs)))
#get Census gazetteer data on pop, hhs, land and water surface areas
gaz <- read.delim("Gaz_counties_national.txt")
gaz <- select(gaz, -ALAND, -AWATER)
gaz <- mutate(gaz, GEOID = sprintf("%05.0f", as.numeric(GEOID)),
              ingaz = 1)
print("number of missing cells in gaz")
print(sum(is.na(gaz)))
all_acs_gaz_counties <- merge(all_acs, gaz, all.x = TRUE)
print("table of inacs vs. ingaz")
print(table(all_acs_gaz_counties$inacs, 
            all_acs_gaz_counties$ingaz, useNA = "ifany"))
x <- all_acs_gaz_counties$POP10 == 0
z <- all_acs_gaz_counties$POP10 == 0
y <- is.na(all_acs_gaz_counties$B19013_001)

print("crosstab of number of counties with zero pop vs. missing med.income")
print(table(x, y, useNA = "ifany"))
print(paste("number of counties with missing median income = ", 
            sum(y), sep = ""))
print(paste("number of counties with zero population = ", 
            sum(x), sep = ""))

save(all_acs_gaz_counties, file = "ACS_2006_2010_counties.rda")
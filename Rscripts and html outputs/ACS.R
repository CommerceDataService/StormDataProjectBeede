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
  tractlist <- geo.make(state=stateabb, county="*", tract="*")
  tractdata <- acs.fetch(2010, span = 5, geography=tractlist, 
                         table.number = "B19013", 
                         key = "33610323e713d6ee2ad71f7b4625dc31fc695b2d")
  state_acs <- merge(estimate(tractdata), geography(tractdata), by.x=0, by.y = "NAME")
  state_acs$GEOID <- paste(sprintf("%02d", state_acs$state), 
                        sprintf("%03d", state_acs$county), 
                        state_acs$tract, sep="")
  state_acs <- select(state_acs, -state, -county, -tract)
  all_acs <- rbind(all_acs, state_acs)
}
all_acs <- mutate(all_acs, inacs = 1)
print("number of missing cells in all_acs")
print(sum(is.na(all_acs)))
#get Census gazeteer data on pop, hhs, land and water surface areas
gaz <- read.delim("Gaz_tracts_national.txt")
gaz <- select(gaz, -ALAND, -AWATER)
gaz <- mutate(gaz, GEOID = sprintf("%011.0f", as.numeric(GEOID)),
              ingaz = 1)
print("number of missing cells in gaz")
print(sum(is.na(gaz)))
all_acs_gaz <- merge(all_acs, gaz, all.x = TRUE)
print("table of inacs vs. ingaz")
print(table(all_acs_gaz$inacs, all_acs_gaz$ingaz, useNA = "ifany"))
x <- all_acs_gaz$POP10 == 0 
z <- all_acs_gaz$POP10 == 0
y <- is.na(all_acs_gaz$B19013_001)

print("crosstab of number of tracts with zero pop vs. missing med.income")
print(table(x, y, useNA = "ifany"))
print(paste("number of tracts with missing median income = ", 
            sum(y), sep = ""))
print(paste("number of tracts with zero population = ", 
            sum(x), sep = ""))

save(all_acs_gaz, file = "ACS_2006_2010.rda")
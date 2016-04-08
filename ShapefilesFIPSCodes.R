#Hail damages project
#2. Obtain shapefiles and FIPS codes
#comments & code adapted from http://commercedataservice.github.io/tutorial_noaa_hail/
#deduplicate data, obtain county-level data (for starters), develop unit of obs.

setwd("~/StormProject")

library(sqldf)
library(RColorBrewer)
library(leaflet)
library(googleVis)
library(rgdal)

# As the SWDI data is point-level data that will be processed into equal-interval 
# grid points, we will want to add spatial context to the data by spatially joining 
# points to county boundary files. The US Census Bureau provides boundary shapefiles 
# through their website 
# (http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip). 
# To efficiently load it in, we’ll write a simple function to download, unzip and 
# load a shapefile.

shape_direct <- function(url, shp) {
  temp = tempfile()
  download.file(url, temp) ##download the URL taret to the temp file
  unzip(temp,exdir="C:/Users/David/Documents/StormProject/ShapeFiles") ##unzip that file
  return(readOGR("C:/Users/David/Documents/StormProject/ShapeFiles", layer=shp))
}

#To run the shape_direct function, we just need the url and the shapefile name.
shp <- shape_direct(url="http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip",
                    shp= "cb_2014_us_county_20m")

# In addition, we’re going to pull in a reference table that links Federal Information 
# Processing System (FIPS) codes that contain numeric identifiers for states. This’ll 
# be useful for clearly indicating in plain language which counties are in a given state.

fips <- read.delim("http://www2.census.gov/geo/docs/reference/state.txt",sep="|")
fips <- fips[,c("STATE","STUSAB")]  ##Keep the FIPS code and state abbreviation
fips$STATE <- as.numeric(fips$STATE) ##Convert FIPS code to numeric
fips$STUSAB <- as.character(fips$STUSAB) ##Convert state name to character

save(fips, file = "FipsCodes.rda")
save(shp, file = "ShapeFileData.rda")     
paste("Number of records in Hail data: ",nrow(raw),sep="")
paste("Number of counties in shapefile: ",nrow(as.data.frame(shp)))
#program to deduplicate hail data (multiple readings from different stations for same
#hail event)

setwd("~/StormProject")
#library(rgdal)

#save output to a file
# con <- file("DeDuplicateHailData_2015.log")
# sink(con, type = "output")

library(dplyr)
#load("ShapeFileData.rda") #spatial data frame called shp
load("Nexrad_Hail_2015.rda") #dataframe called "raw"
print(dim(raw))
#Cut down bounding box to continental US not including Alaska
raw <- raw[raw$LON<(-50) & raw$LON>(-140) & raw$LAT > 25 & raw$LAT <= 49,]
print(dim(raw))
##Does one-quarter degree make the most sense in this context?
fraction = 0.25

##Round coordinates
raw$LON <- round(raw$LON/fraction)*fraction
raw$LAT <- round(raw$LAT/fraction)*fraction
print(summary(raw$PROB))
print(summary(raw$SEVPROB))
##De-duplicate by day, latitude and longitude

# deduped_day <- sqldf("SELECT DATE, LON, LAT, MAXSIZE
#                      FROM raw
#                      GROUP BY DATE, LON, LAT, MAXSIZE")

print(dim(raw))
singles <- raw %>% group_by(DATE, LON, LAT) %>% summarise(minSEVPROB = min(SEVPROB), 
    maxSEVPROB = max(SEVPROB), minMAXSIZE = min(MAXSIZE), maxMAXSIZE = max(MAXSIZE))
singles$inSWDI <- 1
singles <- ungroup(singles)
singles <- as.data.frame(singles)
print(dim(singles))
print(summary(singles$minSEVPROB))
print(summary(singles$maxSEVPROB))
print(summary(singles$minMAXSIZE))
print(summary(singles$maxMAXSIZE))
save(singles, file = "DeDuplicateSWDIHailData_2015.rda")

# ##Set up spatial
# points<-singles
# coordinates(points)=~LON+LAT
# proj4string(points)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# shp <- spTransform(shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") )
# 
# ##Spatial join -- really simple!
# a<-over(points,shp)
# 
# ##Join the results back to singles
# #singles <- cbind(singles,a[,c("STATEFP","NAME")])
# singles <- cbind(singles,a)
# singles$NAME <- as.character(singles$NAME)
# 
# #Merge state abbreviation via FIPS
# load("FipsCodes.rda")
# singles$STATEFP <-as.numeric(singles$STATEFP)
# singles <- merge(singles,fips,by.x="STATEFP",by.y="STATE",all.x=T, sort=F)
# singles$loc <- paste(singles$NAME,", ",singles$STUSAB,sep="")
# singles$loc[is.na(singles$NAME)]<-""
# singles <- singles[!is.na(singles$STUSAB),]
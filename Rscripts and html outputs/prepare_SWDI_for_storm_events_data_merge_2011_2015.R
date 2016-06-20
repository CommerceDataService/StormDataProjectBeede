#program to deduplicate hail data (multiple readings from different stations for same
#hail event)

setwd("~/StormProject")

library(dplyr)
for (year in 2011:2015) {
  print(paste("year = ", year, sep=""))
  load(paste("Nexrad_Hail_", year, ".rda", sep="")) #dataframe called "raw"
  print(paste("dimension of Nexrad_Hail_", year, ".rda", ": ", "number of rows = ", 
              dim(raw)[1], " number of cols = ", dim(raw)[2], sep="")) 
  #Cut down bounding box to continental US not including Alaska
  raw <- raw[raw$LON<(-50) & raw$LON>(-140) & raw$LAT > 25 & raw$LAT <= 49,]
  print(paste("number of rows after limit to lower 48 states = ", 
              dim(raw)[1], sep=""))
  ##Does one-quarter degree make the most sense in this context?
  fraction = 0.25
  
  ##Round coordinates
  raw$LON <- round(raw$LON/fraction)*fraction
  raw$LAT <- round(raw$LAT/fraction)*fraction
  print("summary of PROB variable")
  print(summary(raw$PROB))
  print("summary of SEVPROB variable")
  print(summary(raw$SEVPROB))
  print("freq table of SEVPROB values")
  print(table(raw$SEVPROB))

  ##De-duplicate by day, latitude and longitude
  singles <- raw %>% group_by(DATE, LON, LAT) %>% summarise(minSEVPROB = min(SEVPROB), 
      maxSEVPROB = max(SEVPROB), minMAXSIZE = min(MAXSIZE), maxMAXSIZE = max(MAXSIZE))
  singles$inSWDI <- 1
  singles <- ungroup(singles)
  singles <- as.data.frame(singles)
  print("dimensions of singles after de-duping")
  print(dim(singles))

  print("summary of minSEVPROB variable")    
  print(summary(singles$minSEVPROB))
  print("freq table of minSEVPROB values")
  print(table(raw$minSEVPROB))
  print("summary of maxSEVPROB variable") 
  print(summary(singles$maxSEVPROB))
  print("freq table of maxSEVPROB values")
  print(table(raw$maxSEVPROB))  
  print("summary of minMAXSIZE variable") 
  print(summary(singles$minMAXSIZE))
  print("summary of maxMAXSIZE variable") 
  print(summary(singles$maxMAXSIZE))
  
  save(singles, file = paste("DeDuplicateSWDIHailData_",year, ".rda",
                               sep = ""))
}
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
  raw_tenthdeg <- raw[raw$LON<(-50) & raw$LON>(-140) & raw$LAT > 25 & raw$LAT <= 49,]
  print(paste("number of rows after limit to lower 48 states = ", 
              dim(raw_tenthdeg)[1], sep=""))
  ##Does one-tenth degree make the most sense in this context?
  fraction = 0.1
  
  ##Round coordinates
  raw_tenthdeg$LON <- round(raw_tenthdeg$LON/fraction)*fraction
  raw_tenthdeg$LAT <- round(raw_tenthdeg$LAT/fraction)*fraction
  print("summary of PROB variable")
  print(summary(raw_tenthdeg$PROB))
  print("summary of SEVPROB variable")
  print(summary(raw_tenthdeg$SEVPROB))
  print("freq table of SEVPROB values")
  print(table(raw_tenthdeg$SEVPROB))

  ##De-duplicate by day, latitude and longitude
  singles_tenthdeg <- raw_tenthdeg %>% group_by(DATE, LON, LAT) %>% summarise(minSEVPROB = min(SEVPROB), 
      maxSEVPROB = max(SEVPROB), minMAXSIZE = min(MAXSIZE), maxMAXSIZE = max(MAXSIZE))
  singles_tenthdeg$inSWDI <- 1
  singles_tenthdeg <- ungroup(singles_tenthdeg)
  singles_tenthdeg <- as.data.frame(singles_tenthdeg)
  print("dimensions of singles_tenthdeg after de-duping")
  print(dim(singles_tenthdeg))

  print("summary of minSEVPROB variable")    
  print(summary(singles_tenthdeg$minSEVPROB))
  print("freq table of minSEVPROB values")
  print(table(raw_tenthdeg$minSEVPROB))
  print("summary of maxSEVPROB variable") 
  print(summary(singles_tenthdeg$maxSEVPROB))
  print("freq table of maxSEVPROB values")
  print(table(raw_tenthdeg$maxSEVPROB))  
  print("summary of minMAXSIZE variable") 
  print(summary(singles_tenthdeg$minMAXSIZE))
  print("summary of maxMAXSIZE variable") 
  print(summary(singles_tenthdeg$maxMAXSIZE))
  
  save(singles_tenthdeg, file = paste("DeDuplicateSWDIHailData_",year, "_tenthdeg", ".rda",
                               sep = ""))
}
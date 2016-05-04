#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2011-2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one-quarter degree)
setwd("~/StormProject")
library(dplyr)
##Placeholder file to collect results
batched <- data.frame()
for (year in 2015:2015) {
  load(paste("DeDuplicateHailStormEventData_", year, ".rda", sep = ""))
  load(paste("DeDuplicateSWDIHailData_", year, ".rda", sep = ""))
  merge_SWDI_SE_all <- merge(singles, merged_SE, id = c("DATE", "LON", "LAT"), 
                         all = TRUE)
  merge_SWDI_SE_all$datayear <- year
  for (var in c("inSE", "inSWDI", "mininjuries", "maxinjuries", "mindeaths",
                "maxdeaths", "minproperty", "maxproperty", "mincrops", "maxcrops",
                "minanydamage", "maxanydamage")) {
    print(var)
    paste("merge_SWDI_SE_all$",var,"[is.na(merge_SWDI_SE_all$", var, ")] <- 0", 
          sep="")
  }
  print("freq table for whether obs is in SWDI, SE, or both")
  print(table(merge_SWDI_SE_all$inSWDI, merge_SWDI_SE_all$inSE))
  
  x <- merge_SWDI_SE_all[merge_SWDI_SE_all$inSE==1 & merge_SWDI_SE_all$inSWDI == 1,]
  print("summary maxSEVPROB if in both SWDI and SE")
  print(summary(x$maxSEVPROB))
  print("summary minSEVPROB if in both SWDI and SE")
  print(summary(x$minSEVPROB))
  x <- merge_SWDI_SE_all[merge_SWDI_SE_all$inSE==0 & merge_SWDI_SE_all$inSWDI == 1,]
  print("summary maxSEVPROB if in SWDI but not in SE")
  print(summary(x$maxSEVPROB))
  print("summary minSEVPROB if in SWDI but not in SE")
  print(summary(x$minSEVPROB))
  batched <- rbind(batched, merge_SWDI_SE_all)
}
save(batched, file = "SWDI_SE_merge_2011_2015.rda")
print("frequency table by year")
print(table(batched$datayear))
#merge_SWDI_SE_all <- arrange(merge_SWDI_SE_all, DATE, LON, LAT, desc(inSE))
# merge_SWDI_SE$maxanydamage[is.na(merge_SWDI_SE$maxanydamage)] <- 0
# print(table(merge_SWDI_SE$inSWDI))
# print(table(merge_SWDI_SE$inSE))
# print(table(merge_SWDI_SE$maxanydamage))
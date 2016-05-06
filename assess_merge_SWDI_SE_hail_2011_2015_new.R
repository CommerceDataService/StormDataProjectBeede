#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2011-2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one-quarter degree)
setwd("~/StormProject")
library(dplyr)
##Placeholder file to collect results
batched <- data.frame()
for (year in 2011:2015) {
  print(year)
  load(paste("DeDuplicateHailStormEventData_", year, ".rda", sep = ""))
  load(paste("DeDuplicateSWDIHailData_", year, ".rda", sep = ""))
  
  num_SE <- dim(merged_SE)[1]
  print(paste("number of obs in SE =", num_SE, sep=" "))
  merge_SWDI_SE_all <- merge(singles, merged_SE, id = c("DATE", "LON", "LAT"), 
                         all.x = TRUE)
  merge_SWDI_SE_all$datayear <- year
  merge_SWDI_SE_all[is.na(merge_SWDI_SE_all)] <- 0

  print("freq table for whether obs is in SWDI, SE, or both")
  print(table(merge_SWDI_SE_all$inSWDI, merge_SWDI_SE_all$inSE))
  num_SE_matched <- sum(merge_SWDI_SE_all$inSE)
  print(paste("match rate for SE data = ", num_SE_matched/num_SE, sep=""))
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
print(table(batched$maxanydamage))
print(paste("share of obs with any damage = ", 
            (sum(batched$maxanydamage)/dim(batched[1])), sep = ""))

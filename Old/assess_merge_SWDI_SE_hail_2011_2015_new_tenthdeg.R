#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2011-2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one-tenth degree)
setwd("~/StormProject")
library(dplyr)
##Placeholder file to collect results
batched_tenthdeg <- data.frame()
for (year in 2011:2015) {
  print(year)
  load(paste("DeDuplicateHailStormEventData_", year, "_tenthdeg.rda", sep = ""))
  load(paste("DeDuplicateSWDIHailData_", year, "_tenthdeg.rda", sep = ""))
  
  num_SE_tenthdeg <- dim(merged_SE_tenthdeg)[1]
  print(paste("number of obs in SE =", num_SE_tenthdeg, sep=" "))
  merge_SWDI_SE_all_tenthdeg <- merge(singles_tenthdeg, merged_SE_tenthdeg, id = c("DATE", "LON", "LAT"), 
                         all.x = TRUE)
  merge_SWDI_SE_all_tenthdeg$datayear <- year
  merge_SWDI_SE_all_tenthdeg[is.na(merge_SWDI_SE_all_tenthdeg)] <- 0

  #freq table for whether obs is in SWDI, SE, or both
  print(table(merge_SWDI_SE_all_tenthdeg$inSWDI, merge_SWDI_SE_all_tenthdeg$inSE))
  num_SE_tenthdeg_matched <- sum(merge_SWDI_SE_all_tenthdeg$inSE)
  print(paste("match rate for SE data = ", num_SE_tenthdeg_matched/num_SE_tenthdeg, sep=""))
  x <- merge_SWDI_SE_all_tenthdeg[merge_SWDI_SE_all_tenthdeg$inSE==1 & merge_SWDI_SE_all_tenthdeg$inSWDI == 1,]
  #summary maxSEVPROB if in both SWDI and SE
  print(summary(x$maxSEVPROB))
  #summary minSEVPROB if in both SWDI and SE
  print(summary(x$minSEVPROB))
  x <- merge_SWDI_SE_all_tenthdeg[merge_SWDI_SE_all_tenthdeg$inSE==0 & merge_SWDI_SE_all_tenthdeg$inSWDI == 1,]
  #summary maxSEVPROB if in SWDI but not in SE
  print(summary(x$maxSEVPROB))
  #summary minSEVPROB if in SWDI but not in SE
  print(summary(x$minSEVPROB))
  batched_tenthdeg <- rbind(batched_tenthdeg, merge_SWDI_SE_all_tenthdeg)
}
save(batched_tenthdeg, file = "SWDI_SE_merge_2011_2015_tenthdeg.rda")
#frequency table by year")
print(table(batched_tenthdeg$datayear))
print(table(batched_tenthdeg$maxanydamage))
print(paste("share of obs with any damage = ", 
            (sum(batched_tenthdeg$maxanydamage)/dim(batched_tenthdeg[1])), sep = ""))

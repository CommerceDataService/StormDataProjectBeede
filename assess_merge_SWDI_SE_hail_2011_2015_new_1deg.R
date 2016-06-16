#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2011-2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one degree)
setwd("~/StormProject")
library(dplyr)
##Placeholder file to collect results
batched_1deg <- data.frame()
for (year in 2011:2015) {
  print(year)
  load(paste("DeDuplicateHailStormEventData_", year, "_1deg.rda", sep = ""))
  load(paste("DeDuplicateSWDIHailData_", year, "_1deg.rda", sep = ""))
  
  num_SE_1deg <- dim(merged_SE_1deg)[1]
  print(paste("number of obs in SE =", num_SE_1deg, sep=" "))
  merge_SWDI_SE_all_1deg <- merge(singles_1deg, merged_SE_1deg, id = c("DATE", "LON", "LAT"), 
                         all.x = TRUE)
  merge_SWDI_SE_all_1deg$datayear <- year
  merge_SWDI_SE_all_1deg[is.na(merge_SWDI_SE_all_1deg)] <- 0

  #freq table for whether obs is in SWDI, SE, or both
  print(table(merge_SWDI_SE_all_1deg$inSWDI, merge_SWDI_SE_all_1deg$inSE))
  num_SE_1deg_matched <- sum(merge_SWDI_SE_all_1deg$inSE)
  print(paste("match rate for SE data = ", num_SE_1deg_matched/num_SE_1deg, sep=""))
  x <- merge_SWDI_SE_all_1deg[merge_SWDI_SE_all_1deg$inSE==1 & merge_SWDI_SE_all_1deg$inSWDI == 1,]
  #summary maxSEVPROB if in both SWDI and SE
  print(summary(x$maxSEVPROB))
  #summary minSEVPROB if in both SWDI and SE
  print(summary(x$minSEVPROB))
  x <- merge_SWDI_SE_all_1deg[merge_SWDI_SE_all_1deg$inSE==0 & merge_SWDI_SE_all_1deg$inSWDI == 1,]
  #summary maxSEVPROB if in SWDI but not in SE
  print(summary(x$maxSEVPROB))
  #summary minSEVPROB if in SWDI but not in SE
  print(summary(x$minSEVPROB))
  batched_1deg <- rbind(batched_1deg, merge_SWDI_SE_all_1deg)
}
save(batched_1deg, file = "SWDI_SE_merge_2011_2015_1deg.rda")
#frequency table by year")
print(table(batched_1deg$datayear))
print(table(batched_1deg$maxanydamage))
print(paste("share of obs with any damage = ", 
            (sum(batched_1deg$maxanydamage)/dim(batched_1deg[1])), sep = ""))

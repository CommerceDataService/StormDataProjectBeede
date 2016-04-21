#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one-quarter degree)
setwd("~/StormProject")
library(dplyr)
load("DeDuplicateHailStormEventData_2015.rda")
load("DeDuplicateSWDIHailData_2015.rda")
merge_SWDI_SE_all <- merge(singles, merged_SE, id = c("DATE", "LON", "LAT"), 
                       all = TRUE)

merge_SWDI_SE_all$inSE[is.na(merge_SWDI_SE_all$inSE)] <- 0
merge_SWDI_SE_all$inSWDI[is.na(merge_SWDI_SE_all$inSWDI)] <- 0
print(table(merge_SWDI_SE_all$inSWDI, merge_SWDI_SE_all$inSE))
merge_SWDI_SE_all <- arrange(merge_SWDI_SE_all, DATE, LON, LAT, desc(inSE))
# merge_SWDI_SE$maxanydamage[is.na(merge_SWDI_SE$maxanydamage)] <- 0
# print(table(merge_SWDI_SE$inSWDI))
# print(table(merge_SWDI_SE$inSE))
# print(table(merge_SWDI_SE$maxanydamage))
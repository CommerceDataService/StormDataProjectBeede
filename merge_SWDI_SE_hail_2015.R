#merge deduped SWDI Hail data to deduped Storm Event Hail data for 2015 by 
#DATE/LON/LAT (where LON and LAT are rounded to the nearest one-quarter degree)
library(pROC)
load("DeDuplicateHailStormEventData_2015.rda")
load("DeDuplicateSWDIHailData_2015.rda")
merge_SWDI_SE <- merge(singles, merged_SE, id = c("DATE", "LON", "LAT"), all.x = TRUE)
print(table(merge_SWDI_SE$inSWDI))
print(table(merge_SWDI_SE$inSE))
merge_SWDI_SE$maxanydamage[is.na(merge_SWDI_SE$maxanydamage)] <- 0
merge_SWDI_SE$inSE[is.na(merge_SWDI_SE$inSE)] <- 0
merge_SWDI_SE$inSWDI[is.na(merge_SWDI_SE$inSWDI)] <- 0
print(table(merge_SWDI_SE$inSWDI))
print(table(merge_SWDI_SE$inSE))
print(table(merge_SWDI_SE$maxanydamage))
fit0 <- glm(inSE ~ I(maxSEVPROB - mean(maxSEVPROB)) + 
                       I(maxMAXSIZE - mean(maxMAXSIZE)), data=merge_SWDI_SE, 
            family="binomial")
print(summary(fit0))
print(exp(confint(fit0)))
fit1 <- glm(maxanydamage ~ I(maxSEVPROB - mean(maxSEVPROB)) + 
                               I(maxMAXSIZE - mean(maxMAXSIZE)), data=merge_SWDI_SE, 
            family="binomial")
print(summary(fit1))
print(exp(confint(fit1)))
print(roc(fit0$y , fit0$fitted.values,auc=T, plot = T))
print(roc(fit1$y , fit1$fitted.values,auc=T, plot = T))
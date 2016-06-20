# Run simple logistic model on training set
# using 10-fold cross-validation

library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
library(SuperLearner)
setwd("~/StormProject")
load("training.rda")
#construct RHS variables
training <- mutate(training, 
                   Tract_popdens = Tract_POP10/Tract_ALAND_SQMI, 
                   County_popdens = County_POP10/County_ALAND_SQMI,
                   Tract_GEOID <- as.factor(Tract_GEOID),
                   County_GEOID <- as.factor(County_GEOID),
                   State <- as.factor(State_abb),
                   datayear <- as.factor(datayear),
           Tract_shr_water <- Tract_AWATER_SQMI/(Tract_AWATER_SQMI+Tract_ALAND_SQMI),
   County_shr_water <- County_AWATER_SQMI/(County_AWATER_SQMI+County_ALAND_SQMI)
                        )
load("listsplits.rda")
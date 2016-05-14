#program to create validRows list as defined in the SuperLearner package:
# 
# validRows: A List. Use this to pass pre-specified rows for the sample splits. 
# The length of the list should be V and each entry in the list should contain 
# a vector with the row numbers of the corresponding validation sample.

library(dplyr, warn.conflicts = TRUE, quietly = FALSE)
library(SuperLearner)
setwd("~/StormProject")
load("training.rda")

V <- 10 #number of splits

#k <- 1
listsplits <- vector("list", V)
for(k in 1:V){
        print(k)
    training  <- mutate(training, x = (inSE == 1) | 
                (randnum1 < k/10 & (k-1)/10 <= randnum1))
 listsplits[[k]] <- which(training$x)                     
}
save(listsplits, file="listsplits.rda")
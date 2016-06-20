# Run glmnet, svm, random forests
# using special 10-fold cross-validation resampling method
setwd("~/StormProject")
library(dplyr)
# library(glmnet)
# library(randomForest)
library(e1071)
library(kernlab)
library(caret)
library(parallel, quietly = TRUE)
library(doParallel, quietly = TRUE)

load("SWDI_SE_ACS.rda")
SWDI_SE_ACS <- select(SWDI_SE_ACS, inSE, Tract_popdens, #populaton density
       County_popdens,
       #Tract_GEOID, #factor variable for FIPS code
       #County_GEOID,
       State, #factor variable for state postal code
       datayear, #year the data originated in SWDI
       #Tract_shr_water, #share of total surface area is covered by water
       #County_shr_water,
       County_B19013_001, #median household income
       Tract_B19013_001,
       maxSEVPROB, #probability of severe event (maximum across deduped obs)
       maxMAXSIZE #estimated size of hail (maximum across deduped obs)
)

set.seed(123)
numobs <- dim(SWDI_SE_ACS)[1]
SWDI_SE_ACS$rand1 <- runif(numobs)
#SWDI_SE_ACS$rand2 <- runif(numobs)
SWDI_SE_ACS$id <- 1:numobs
#Splitting
  split <- 0.7
  train <- SWDI_SE_ACS[SWDI_SE_ACS$rand1 <= split, ]
  test <- SWDI_SE_ACS[SWDI_SE_ACS$rand1 > split, ]
  #Quick test on proportion
  mean(train$inSE)
  mean(test$inSE) 
  train$rand1 <- NULL
  test$rand1 <- NULL

  xtrain = train[, -dim(train)[2]] #get rid of id variable
  ytrain = xtrain$inSE
  xtrain_mat <- model.matrix(inSE ~ ., xtrain)[,-1]

  xtest = test[, -dim(test)[2]] #get rid of id variable before making matrix
  xtest_mat = model.matrix(inSE ~ ., xtest)[,-1]  
  preProcValues <- preProcess(xtrain_mat, method = c("center", "scale")) 
  xtrainproc <- predict(preProcValues, xtrain_mat)
  xtestproc <- predict(preProcValues, xtest_mat)   

  xtrainproc = cbind(ytrain, xtrainproc, runif(dim(xtrainproc)[1]))
  
#Create 10 breaks in the data
  interval = 0.1

#Create a placeholder dataset
  test_results <- test[,c("id","inSE")]
  
numclass <- 1  
#Top layer loop -- process temporary partitions
for( k in seq(interval,interval,interval)){  ####be sure to comment this out later!
#    for( k in seq(interval,1,interval)){

    print(k)
    picktemp <- xtrainproc[,1] == 1  | 
    (xtrainproc[,dim(xtrainproc)[2]] <= k & xtrainproc[,dim(xtrainproc)[2]] > k - interval)
    temp <- xtrainproc[picktemp, -dim(xtrainproc)[2]]

    #Quick check on size of partitions
    print(nrow(temp)/nrow(xtrainproc))
  
    #create data matrix for RHS variables and factor vector for dependent variable
    y = as.factor(temp[,1])
    x = temp[, -1]

    # cluster <- makeCluster(detectCores() - 1)
    # registerDoParallel(cluster)
    

        #CLASSIFIER 3:  support vector machines
          #Rudimentary svm1
          # fit = svm(x = x, y = y, kernel = "radial", gamma = 1,
          #           cost = 1)
          # print(summary(fit))

          #CV
          # fitControl <- trainControl(method="cv",
          #                            number = 5
          #                            #allowParallel = TRUE
          #                            )
          cvfit <- svm(x=x, y=y, scale=FALSE, type='C', kernel = 'linear', 
                         cross=5
                         )
          # stopCluster(cluster)
          print(cvfit)
          print(summary(cvfit))

          #Predicted
          prob = predict(cvfit, newdata = xtestproc)

          #Collect the results on the test set
          test_results <- cbind(test_results,prob)
  }
  
  #Voting
  voting_list <- test_results
  for(k in 3:ncol(voting_list)){
    voting_list[,k]<-voting_list[,k]>0.5
  }
  
  voting_list$totals<- rowSums(voting_list[,3:ncol(voting_list)])
  voting_list <- voting_list[, c("inSE","totals")]
  voting_list <- mutate(voting_list, majority = totals > numclass*10/2)
  table(voting_list$inSE,voting_list$majority)

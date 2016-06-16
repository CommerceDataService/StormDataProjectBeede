# Run glmnet, gam, random forests
# using special 10-fold cross-validation resampling method
# uses ACS data and 1/4 degree lat/lon rounding
setwd("~/StormProject")
library(dplyr)
library(glmnet)
library(randomForest)
# library(e1071)
# library(kernlab)
library(caret)
library(gam)
# library(parallel, quietly = TRUE)
# library(doParallel, quietly = TRUE)

load("SWDI_SE_ACS_LODES_1deg.rda")
SWDI_SE_ACS_LODES_1deg <- select(SWDI_SE_ACS_LODES_1deg, inSE, Tract_popdens, #populaton density
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
       #Tract_empdens, 
       #County_empdens
       #datamonth
)

set.seed(123)
numobs <- dim(SWDI_SE_ACS_LODES_1deg)[1]
SWDI_SE_ACS_LODES_1deg$rand1 <- runif(numobs)
SWDI_SE_ACS_LODES_1deg$id <- 1:numobs
#Splitting
  split <- 0.7
  train <- SWDI_SE_ACS_LODES_1deg[SWDI_SE_ACS_LODES_1deg$rand1 <= split, ]
  test <- SWDI_SE_ACS_LODES_1deg[SWDI_SE_ACS_LODES_1deg$rand1 > split, ]
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
  # preProcValues <- preProcess(xtrain_mat, method = c("center", "scale")) 
  # xtrainproc <- predict(preProcValues, xtrain_mat)
  # xtestproc <- predict(preProcValues, xtest_mat)   

  xtrain_mat = cbind(ytrain, xtrain_mat, runif(dim(xtrain_mat)[1]))
  
#Create 10 breaks in the data
  interval = 0.1

#Create a placeholder dataset
  test_results <- test[,c("id","inSE")]
  
numclass <- 3  
#Top layer loop -- process temporary partitions
#for( k in seq(interval,interval,interval)){  ####be sure to comment this out later!
    for( k in seq(interval,1,interval)){
#k <- interval
    print(k)
    picktemp <- xtrain_mat[,1] == 1  | 
    (xtrain_mat[,dim(xtrain_mat)[2]] <= k & xtrain_mat[,dim(xtrain_mat)[2]] > k - interval)
    temp <- xtrain_mat[picktemp, -dim(xtrain_mat)[2]]

    #Quick check on size of partitions
    print(nrow(temp)/nrow(xtrain_mat))
  
    #create data matrix for RHS variables and factor vector for dependent variable
    y = as.factor(temp[,1])
    x = temp[, -1]

     # cluster <- makeCluster(detectCores() - 1)
     # registerDoParallel(cluster)

    #CLASSIFIER 1: ridge regression
    
    #CV
    cvfit = cv.glmnet(x, y, family = "binomial", alpha=0, type.measure = "class")
    plot(cvfit)
    
    #Predicted
    prob = predict(cvfit, newx = xtest_mat, s = "lambda.min", type="response")
    
    #Collect the results on the test set
    test_results <- cbind(test_results,prob)    
        
    #CLASSIFIER 2:  random forests
    #Rudimentary random forests
    fit = randomForest(x=x, y=y, ntree=101)
    plot(fit)
    prob = predict(fit, newdata = xtest_mat, type = "prob")[,2]   
    #prob <- as.numeric(prob) - 1
    #Collect the results on the test set
    test_results <- cbind(test_results,prob)

    #CLASSIFIER 3:  generalized linear model with splines
    gamGrid <-  expand.grid(df=(1:10))
    fitControl <- trainControl(method="cv",
                                     number = 10
                                     #allowParallel = TRUE
                                     )
    cvfit <- train(x=x, y=y, method = 'gamSpline',
                 trControl=fitControl,
                 tuneGrid = gamGrid
                 )
    print(cvfit)
    # stopCluster(cluster)
    #print(cvfit)
    print(summary(cvfit$results))

    #Predicted
    prob = predict(cvfit, newdata = xtest_mat, type="prob")[,2]

    #Collect the results on the test set
    test_results <- cbind(test_results,prob)
  }
        #save(test_results, file="test_results_hail_10JUN2016_3.rda")
  #Voting
  voting_list <- as.data.frame(data.matrix(test_results))
  save(voting_list, file = "Prediction_Hail_ACS_1deg_3pred.rda")
  for(k in 3:ncol(voting_list)){
    voting_list[,k]<-voting_list[,k]>0.5
  }

  voting_list$totals<- rowSums(voting_list[,3:ncol(voting_list)])
  voting_list <- voting_list[, c("inSE","totals")]
  voting_list <- mutate(voting_list, majority = totals > numclass*10/2)
  tablest <- table(voting_list$inSE,voting_list$majority)
  tablest
  #calculate recall
  tablest[2,2]/(tablest[2,1] + tablest[2,2])

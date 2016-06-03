# Run random forests
# using special 10-fold cross-validation resampling method
setwd("~/StormProject")
library(dplyr)
library(glmnet)
library(randomForest)
library(e1071)

load("SWDI_SE_ACS.rda")
SWDI_SE_ACS <- select(SWDI_SE_ACS, inSE, Tract_popdens, #populaton density
       County_popdens,
       #Tract_GEOID, #factor variable for FIPS code
       #County_GEOID,
       State, #factor variable for state postal code
       datayear, #year the data originated in SWDI
       Tract_shr_water, #share of total surface area is covered by water
       County_shr_water,
       County_B19013_001, #median household income
       Tract_B19013_001,
       maxSEVPROB, #probability of severe event (maximum across deduped obs)
       maxMAXSIZE #estimated size of hail (maximum across deduped obs)
)

set.seed(123)
numobs <- dim(SWDI_SE_ACS)[1]
SWDI_SE_ACS$rand1 <- runif(numobs)
SWDI_SE_ACS$rand2 <- runif(numobs)
SWDI_SE_ACS$id <- 1:numobs
#Splitting
  split <- 0.7
  train <- SWDI_SE_ACS[SWDI_SE_ACS$rand1 <= split, ]
  test <- SWDI_SE_ACS[SWDI_SE_ACS$rand1 > split, ]
  test$rand1 <- NULL
  test$rand2 <- NULL
  # test_df <- test[,c(2:11)]
  # test_mat <- as.matrix(test_df)

  test_df = test[, -12]
  test_mat = model.matrix(inSE ~ ., test_df)[,-1]  
    
#Quick test on proportion
  mean(train$inSE)
  mean(test$inSE)
  
#Create 10 breaks in the data
  interval = 0.1

#Create a placeholder dataset
  test_results <- test[,c("id","inSE")]
  
#Number of classifiers - sure to change this when including all classifiers
  numclass <- 1   
  
#Top layer loop -- process temporary partitions
#  for( k in seq(interval,interval,interval)){ 
    for( k in seq(interval,1,interval)){

    print(k)
    temp <- train[train$inSE == 1 | (train$rand2 <=k & train$rand2 >k - interval),]
    
    #remove rand1 and rand2
    temp$rand1 <- NULL
    temp$rand2 <- NULL
    
    #Quick check on size of partitions
    print(nrow(temp)/nrow(train))
  
    #create data matrix for RHS variables and factor vector for dependent variable
    y = as.factor(temp$inSE)
    x = temp[, -12]
    x = model.matrix(inSE ~ ., x)[,-1]
    #Inner loop # Insert classifier here
    
      # #CLASSIFIER 1: ridge regression
      # 
      #       #Rudimentary classifier 
      #     fit = glmnet(x, y, family = "binomial", alpha=0)
      #     plot(fit)
      #     
      #   #CV
      #     cvfit = cv.glmnet(x, y, family = "binomial", type.measure = "class")
      #     plot(cvfit)
      #     
      #   #Predicted
      #     prob1 = predict(cvfit, newx = test_mat, s = "lambda.min", type = "response")
      #     
      #   #Collect the results on the test set
      #     test_results <- cbind(test_results,prob1)
          
          
       #CLASSIFIER 2:  random forests
          #Rudimentary random forests
          #mtry = 8 because it approximates sqrt(p=60)
          fit = randomForest(x=x, y=y, mtry=8, importance=TRUE)
          plot(fit)

          #CV
          cvfit = rfcv(x, y, cv.fold=10, mtry=8, importance=TRUE)
          plot(cvfit)

          #Predicted
          prob2 = predict(cvfit, newx = test_mat, type = "response")

          #Collect the results on the test set
          test_results <- cbind(test_results,prob2)
          
        # #CLASSIFIER 3:  support vector machines
        #   #Rudimentary svm
        #   fit = randomForest(x, y, family = "binomial", alpha=0)
        #   plot(fit)
        #   
        #   #CV
        #   cvfit = rfcv(x, y, family = "binomial", type.measure = "class")
        #   plot(cvfit)
        #   
        #   #Predicted
        #   prob2 = predict(cvfit, newx = test_mat, s = "lambda.min", type = "response")
        #   
        #   #Collect the results on the test set
        #   test_results <- cbind(test_results,prob2)
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

setwd("~/StormProject")
library(dplyr)
library(ROCR)
numclass <- 3
#two data sets from Census: ACS and LODES                
#for(datasrc in c("ACS", "LODES")){
datasrc <- "ACS"
print(datasrc)
#        for(gran in c("qtrdeg", "1deg")){
gran <- "1deg"
print(gran)
                load(paste("Prediction_Hail_", datasrc, "_", gran, "_3pred.rda",
                           sep = ""))
                voting_list <- voting_list[,-1] #drop id variable
                test_results <- voting_list
                y_hats <- voting_list
                # for(i in (0:3)){
                #         if(i == 0){
                        print("ensemble") 
                                print("majority")
                                ncol_test <- ncol(test_results)
                                for(k in 2:ncol_test){
                                        test_results[,k]<-test_results[,k]>0.5
                                }
                                test_results$totals<- rowSums(test_results[,2:ncol_test])
                                test_results$means<- rowMeans(test_results[,2:ncol_test])
                                voting_list$means <- rowMeans(voting_list[,2:ncol_test])
                                majority <- test_results[, c("inSE","totals")]
                                majority <- mutate(majority, majority = totals > numclass*10/2)
                                tablest <- table(majority$inSE,majority$majority)
                                print(tablest)
                                #calculate recall (aka sensitivity)
                                recall <- tablest[2,2]/(tablest[2,1] + tablest[2,2])
                                print("recall")
                                print(recall)
                                #calculate specificity
                                specificity <- tablest[1,1]/(tablest[1,1] + tablest[1,2])
                                print("specificity")
                                print(specificity)
                                #calculate specificity/recall
                                print("specificity/recall")                                
                                print(specificity/recall)
                                predfit <- prediction(predictions=test_results$means, labels=test_results$inSE)
                                perffit <- performance(predfit, measure="tpr", x.measure="fpr")
                                plot(perffit, print.cutoffs.at=seq(0,1,by=0.05))
                                aucfit <- performance(predfit, measure="auc")
                                print("AUC")
                                print(aucfit$y.values)
                                predfit <- prediction(predictions=voting_list$means, labels=voting_list$inSE)
                                perffit <- performance(predfit, measure="tpr", x.measure="fpr")
                                plot(perffit, print.cutoffs.at=seq(0,1,by=0.1))
                                aucfit <- performance(predfit, measure="auc")
                                print("AUC")
                                print(aucfit$y.values)                                
                        #}
                        else{
                                colvec = seq(from=2+i, length.out = 10, by=3)
                                print(colvec)
                        }
                #} #end of gran block        
        #} #end of datasrc block
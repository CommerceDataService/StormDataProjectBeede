setwd("~/StormProject")
library(dplyr)
library(ROCR)
library(utils)
#initialize dataframe for final output
listperf <- list()
df <- data.frame(meth=character(),
                 algor=character(), 
                 datasrc=character(),
                 gran=character(),
                 measure=character(), 
                 estmt=numeric()
) 
#two data sets from Census: ACS and LODES plus NONE   
for(datasrc in c("ACS", "LODES", "NONE", "ALL")){
        print(datasrc)
        for(gran in c("qtrdeg", "1deg")){
                print(gran)
                load(paste("Prediction_Hail_", datasrc, "_", gran, "_3pred.rda",
                           sep = ""))
                voting_list <- voting_list[,-1] #drop id variable
                ncol_vote <- ncol(voting_list)               
                i <- 0
                for(algor in c("ensemble", "ridge", "randomForest", "gamsplines")){
                        print(algor)
                        if(i == 0) {
                                numclass <- 3
                                colvec = (2: ncol_vote)
                        } else {
                                numclass <- 1
                                colvec = seq(from=i+1, length.out = 10, by=3)
                        } 
                        print(colvec)
                        new_list <- voting_list[,c(1, colvec)]
                        ncol_new <- ncol(new_list)
                        for (meth in c("ave", "majority")){
                                if(meth == "ave"){
                                        new_list$means <- rowMeans(new_list[,2:ncol_new])
                                        new_list$class <- new_list$means > 0.5    
                                } else {
                                        for(k in 2:ncol_new){
                                                new_list[,k]<-new_list[,k]>0.5
                                        }
                                        new_list$totals<- rowSums(new_list[,2:ncol_new])
                                        new_list$means<- rowMeans(new_list[,2:ncol_new])                                        
                                        new_list <- mutate(new_list, class = totals > numclass*10/2)
                                        new_list$totals <- NULL
                                }
                                tablest <- table(new_list$inSE,new_list$class)
                                print(tablest)
                                measure <- "rcall"
                                rcall <- tablest[2,2]/(tablest[2,1] + tablest[2,2])
                               print("rcall")
                               estmt <- rcall
                                print(estmt)
                                df <- rbind(df, 
                                            as.data.frame(cbind(meth, algor, datasrc, gran, measure, estmt)))
                                specificity <- tablest[1,1]/(tablest[1,1] + tablest[1,2])
                                print("specificity")
                                print(specificity)
                                #calculate specificity/rcall
                                measure <- "specificity/rcall"
                                estmt <- specificity/rcall
                                print("specificity/rcall")                                
                                print(estmt)
                                df <- rbind(df, 
                                            as.data.frame(cbind(meth, algor, datasrc, gran, measure, estmt)))
                                
                                predfit <- prediction(predictions=new_list$means, labels=new_list$inSE)
                                perffit <- performance(predfit, measure="tpr", x.measure="fpr")
                                print(plot(perffit, print.cutoffs.at=seq(0,1,by=0.1), colorize=TRUE))
                                listperf <- c(listperf, perffit)
                                aucfit <- performance(predfit, measure="auc")
                                measure <- "AUC"
                                print(measure)
                                estmt <- aucfit@y.values[[1]][1]
                                print(estmt)
                                df <- rbind(df, 
                                            as.data.frame(cbind(meth, algor, datasrc, gran, measure, estmt)))
                        } #end of meth loop
                        
                        
                        i <- i + 1
                } #end of algor loop

      


                } #end of gran block        
        } #end of datasrc block
save(listperf, file="listperf_ALL.rda")
save(df, file="PerfMeasuresP_Hats_ALL.rda")
write.csv(df, file="PerfMeasuresP_Hats_ALL.csv")

#Performance measures for storm event classifiers
library("ROCR") 

#initialize dataframe for final output
df <- data.frame(Measure=character(), 
                 Classifier=character(), 
                 DataSet=character(),
                 SpatAg=character(),
                 Value=numeric()
                 ) 
#two methods - average p-hats vs. classify with 0.5 and then average
for(tech in c("average", "majority")){ 

#two data sets from Census: ACS and LODES                
for(datasrc in c("ACS", "LODES")){
        for(gran in c("qtrdeg", "1deg")){
                load(paste("Prediction_Hail_", datasrc, "_", "gran", "_3pred.rda",
                        sep = ""))
        }
        
        
}
        
numclass <- 3        
for(i in (0:3)){
        if(i == 0){
                colvec <- 1:30
                voting_list$totals<- rowSums(voting_list[,3:ncol(voting_list)])
                voting_list <- voting_list[, c("inSE","totals")]
                voting_list <- mutate(voting_list, majority = totals > numclass*10/2)
                tablest <- table(voting_list$inSE,voting_list$majority)
                tablest
                #calculate recall
                tablest[2,2]/(tablest[2,1] + tablest[2,2])
        }
        else{
        colvec = seq(from=2+i, length.out = 10, by=3)
        }
        print(colvec)
}        
        
} # end of tech loop
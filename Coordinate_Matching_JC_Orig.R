title: "Coordinate Matching"
author: "Jeff Chen"
date: "April 19, 2016"
output: html_document
---
  This code is a 2-dimensional example of path-based matching. For the Severe Weather project, this would need to be expanded to 3-dimensions (x, y, and date). 
Start off by setting parameters for simulation
```{r}
#Set Parameters
int = 0.05 ##rounding parameter -- this is the 0.25 degree rounding value
n = 10 ##Number of records for the storm events data
n_swdi = 2000 ##Number of records for SWDI 
```
Create datasets
```{r}
swdi = data.frame(storm_id = round(n_swdi*runif(n_swdi)),
                  x = round(runif(n_swdi)/int)*int,
                  y = round(runif(n_swdi)/int)*int,
                  hail_size = round(runif(n_swdi)*4,1)
)
#Note that SE data has a start pair and end pair
storm_events = data.frame(storm_id = round(1000*runif(n)),
                          x0= runif(n),
                          y0= runif(n),
                          x1= runif(n),
                          y1= runif(n)
)
```
Develop wrapper for the approx() function. Approx interpolates a line between two points. The wrapper accepts the following parameters
- data. name of dataset
- x0. index # of start x-coord
- x1. index # of end x-coord
- y0. index # of start y-coord
- y1. index # of end y-coord
- row_num. Row number of a record in storm events data
- int. Rounding parameter for gridding
```{r}
step <- function(data,x0,x1,y0,y1,row_num,int){
  
  #First step creates a 2x2 dataframe using the parameters
  temp <- data.frame(x = c(data[row_num,c(x0)],data[row_num,c(x1)]), y = c(data[row_num,c(y0)],data[row_num,c(y1)]))
  
  #Pass the dataframe through approx(), extract the x coord, y coord and round to the nearest 'int' (interval) value
  temp2 <- data.frame(x = round(approx(temp$x, temp$y)$x/int)*int, y = round(approx(temp$x, temp$y)$y/int)*int)
  
  #Dedupe file
  return(temp2[!duplicated(temp2),])
}
```
This is what the function will return
```{r}
rownum = 2
step(storm_events,2,4,3,5,rownum,int)
```
Loop through each row of the storm events, 
```{r}
##Placeholder file to collect results
batched <- data.frame()
#Loop through storm events
for(k in 1:nrow(storm_events)){
  
  #get the interpolated line
  temp <-step(storm_events,2,4,3,5,k,int)
  
  #Merge against SWDI for any matches
  merged <- merge(swdi,temp, by.x=c("x","y"),by.y=c("x","y"))
  
  #Add the merged results into batched
  batched <- rbind(batched,merged)
  
  print(paste("Results of row #",k, " in Storm Events"))
  ##Plot results
  plot(swdi$x, swdi$y)
  points(storm_events$x0[k],storm_events$y0[k],col="blue",pch = 19)
  points(storm_events$x1[k],storm_events$y1[k],col="red",pch = 19)
  points(merged$x, merged$y, col="green",pch = 19)
}
print(paste("Total number of new records found in simulated SWDI: ", nrow(batched)))
```
This should allow for interpolation of results. The goal is to find all SWDI records that match the path of a storm events record.
##prepare 2011-2015 Storm Events data for Hail to be merged with SWDI Hail 
##data for 2011-2015
setwd("~/StormProject")
library(dplyr)
library(lubridate)
library(geosphere)
for (year in 2011:2015) {
  print(paste("year = ", year, sep=""))
  load(paste("merged_SE_", year, ".rda", sep=""))
  print(paste("dimension of merged_SE_", year, ".rda", ": ", "number of rows = ", 
              dim(merged_SE)[1], " number of cols = ", dim(merged_SE)[2], sep="")) 
  merged_SE <- mutate(merged_SE, EVTYPEcap = toupper(EVENT_TYPE))
  merged_SE <- merged_SE[grep("HAIL", merged_SE$EVTYPEcap),]
  print(paste("number of rows of hail-only events = ", dim(merged_SE)[1], sep=""))
  summary(merged_SE$MAGNITUDE)
  print(paste("no. obs w/missing hail size = ", sum(is.na(merged_SE$MAGNITUDE)),
              sep=""))
  # table(merged_SE_HAIL$STATE)
  # table(merged_SE_HAIL$STATE_FIPS)
  # sum(is.na(merged_SE_HAIL$STATE_FIPS))
  
  #Cut down bounding box to continental US not including Alaska
  merged_SE <- merged_SE[merged_SE$LONGITUDE<(-50) & merged_SE$LONGITUDE>(-140) & 
                           merged_SE$LATITUDE > 25 & merged_SE$LATITUDE <= 49,]
  
  print(paste("number of rows after limit to lower 48 states = ", 
              dim(merged_SE)[1], sep=""))

  #in this section trying to assess how different beginning and ending coordinates are
  #FIRST: how often do begin and end points differ (I think they differ less for hail 
  #than for, say, other storm events)
  print(paste("no. obs with begin lat != end lat = ",
              sum(merged_SE$BEGIN_LAT != merged_SE$END_LAT), sep=""))
  print(paste("no. obs with begin lon != end lon = ",
              sum(merged_SE$BEGIN_LON != merged_SE$END_LON), sep=""))

  #assess extent to which LONGITUDE & LATITUDE differ from BEGIN/END LAT and LON
  LAT_LON <- select(merged_SE, EPISODE_ID, EVENT_ID, BEGIN_LON, BEGIN_LAT, END_LON, 
                    END_LAT, LONGITUDE, LATITUDE)
  x <- filter(LAT_LON, BEGIN_LON != LONGITUDE & BEGIN_LAT != LATITUDE)
  print(paste("no. obs with BEGIN_LON != LONGITUDE & BEGIN_LAT != LATITUDE",
              dim(x), sep=" ")) 
  y <- filter(x, END_LON != LONGITUDE & END_LAT != LATITUDE)
  print(paste("no. obs with END_LON != LONGITUDE & END_LAT != LATITUDE",
              dim(y), sep=" ")) 
  
  x <- filter(LAT_LON, END_LON != LONGITUDE & END_LAT != LATITUDE)
  print(paste("no. obs with END_LON != LONGITUDE & END_LAT != LATITUDE",
              dim(x), sep=" "))   
  y <- filter(x, BEGIN_LON != LONGITUDE & BEGIN_LAT != LATITUDE)
  print(paste("no. obs with BEGIN_LON != LONGITUDE & BEGIN_LAT != LATITUDE",
              dim(y), sep=" ")) 

  #identify and characterize records with different BEGIN_ and END_ LAT and LON
  #by calculating distance in meters between begin and end points
  begin_mat <- as.matrix(select(merged_SE, BEGIN_LON, BEGIN_LAT))
  end_mat <- as.matrix(select(merged_SE, END_LON, END_LAT))
  pathlen <- distHaversine(begin_mat, end_mat)
  print("summary of all distances between begin and end points")
  print(summary(pathlen))
  print(paste("no. obs with positive distance between begin and end points",
              sum(pathlen>0), sep=" "))
  print("summary of all positive distances between begin and end points")
  summary(pathlen[pathlen>0])
  
  print("table of time zones in hail data")
  print(table(merged_SE$CZ_TIMEZONE))
                      
#create number of hours to adjust date/times to GMT  
  merged_SE <- mutate(merged_SE, 
                BEGIN_DATE_TIME = as.character(BEGIN_DATE_TIME),
                END_DATE_TIME = as.character(END_DATE_TIME),
                hours_tz = as.numeric(substr(as.character(merged_SE$CZ_TIMEZONE),5,5)))
  
  print("table of required time zone hour adjustments")
  print(table(merged_SE$hours_tz))
  
  merged_SE <- mutate(merged_SE,
                      new_beg_time = dmy_hms(BEGIN_DATE_TIME) + hours(hours_tz),
                      new_end_time = dmy_hms(END_DATE_TIME) + hours(hours_tz))
  merged_SE <- mutate(merged_SE,
                      DATE = date(new_beg_time))
  #assessment of whether it is worthwhile to reshape the data by beg vs end times
  x <- date(merged_SE$new_end_time) - date(merged_SE$new_beg_time)
  x <- as.period(x, "days")
  x <- sort(x)
  print("assessment of whether it is worthwhile to reshape data by beg vs end times")
  print(tail(x, 50))
  
  ##Does one-quarter degree make the most sense in this context?
  #set rounding factor for lat/lon
  int = 0.25
  ##Round coordinates
  merged_SE <- mutate(merged_SE, BEGIN_LON = round(BEGIN_LON/int)*int,
                      BEGIN_LAT = round(BEGIN_LAT/int)*int,
                      END_LON = round(END_LON/int)*int,
                      END_LAT = round(END_LAT/int)*int)
  
  print(paste("no. of missing direct injuries =", 
              sum(is.na(merged_SE$INJURIES_DIRECT)), sep=" "))
  print(paste("no. of missing indirect injuries = ", 
              sum(is.na(merged_SE$INJURIES_INDIRECT)), sep=" "))

  merged_SE <- mutate(merged_SE, 
                      injuries = as.numeric((INJURIES_DIRECT + INJURIES_INDIRECT) != 0))
  print(paste("no. of missing direct deaths = ", 
              sum(is.na(merged_SE$DEATHS_DIRECT)), sep=""))
  print(paste("no. of missing indirect deaths = ", 
              sum(is.na(merged_SE$DEATHS_INDIRECT)), sep=""))
  merged_SE <- mutate(merged_SE, 
                      deaths = as.numeric((DEATHS_DIRECT + DEATHS_INDIRECT) != 0))
  merged_SE <- mutate(merged_SE, DAMAGE_PROPERTY = as.character(DAMAGE_PROPERTY),
                      DAMAGE_CROPS = as.character(DAMAGE_CROPS))
  merged_SE <- mutate(merged_SE, property = as.numeric((substr(DAMAGE_PROPERTY, 1, 4) != "0.00")
                      & (DAMAGE_PROPERTY != "") & (DAMAGE_PROPERTY != " ")))
  merged_SE <- mutate(merged_SE, crops = as.numeric((substr(DAMAGE_CROPS, 1, 4) != "0.00")
                      & (DAMAGE_CROPS != "") & (DAMAGE_CROPS != " ")))
  merged_SE <- mutate(merged_SE, anydamage = as.numeric((injuries == 1) | (deaths == 1) | 
                                               (property == 1) | (crops == 1)))
  titlelist <- c("INJURIES_DIRECT", "INJURIES_INDIRECT", 
    "DEATHS_DIRECT",  "DEATHS_INDIRECT", "DAMAGE_PROPERTY", "DAMAGE_CROPS", 
    "injuries", "deaths", "property", "crops", "anydamage")
  print("look at first 50 damages records")
  print(merged_SE[1:50, titlelist])
  
  #dedupe the data by DATE, LAT, LON
  merged_SE <- merged_SE %>% group_by(DATE, BEGIN_LON, BEGIN_LAT, 
                                      END_LON, END_LAT) %>% 
    summarise(mininjuries = min(injuries), maxinjuries = max(injuries), 
              mindeaths = min(deaths), maxdeaths = max(deaths), 
              minproperty = min(property), maxproperty = max(property), 
              mincrops = min(crops), maxcrops = max(crops), 
              minanydamage = min(anydamage), maxanydamage = max(anydamage))
  merged_SE <- ungroup(merged_SE)
  merged_SE <- as.data.frame(merged_SE)
  print("dimension of data fram after de-duping")
  print(dim(merged_SE))
  
  #split dataframe in one where begin and end points are the same and one where diff
  merged_SE_same = merged_SE[(merged_SE$BEGIN_LAT == merged_SE$END_LAT) &
                               (merged_SE$BEGIN_LON == merged_SE$END_LON),]
  merged_SE_same = merged_SE_same[, -c(4:5)]
  merged_SE_same = rename(merged_SE_same, LON = BEGIN_LON, LAT = BEGIN_LAT)
  
  print("str(merged_SE_same")
  print(str(merged_SE_same))
  merged_SE_diff = merged_SE[(merged_SE$BEGIN_LAT != merged_SE$END_LAT) |
                               (merged_SE$BEGIN_LON != merged_SE$END_LON),]
  
  # merged_SE_diff$BEGIN_LON_OLD=merged_SE_diff$BEGIN_LON
  # merged_SE_diff$BEGIN_LAT_OLD=merged_SE_diff$BEGIN_LAT 
  # merged_SE_diff$END_LON_OLD=merged_SE_diff$END_LON 
  # merged_SE_diff$END_LAT_OLD=merged_SE_diff$END_LAT
  merged_SE_diff$id <- rownames(merged_SE_diff)
  print("str(merged_SE_diff")
  print(str(merged_SE_diff))
  
  #create "swath" using begin- and endpoints and adapting Jeff's code
  step <- function(data,x0,x1,y0,y1,row_num,id) {
    
    #First step creates a 2x2 dataframe using the parameters
    temp <- data.frame(x = c(data[row_num,c(x0)],data[row_num,c(x1)]), 
                       y = c(data[row_num,c(y0)],data[row_num,c(y1)]))
  #  print(temp)
    
    #Pass the dataframe through approx(), extract the x coord, y coord
    temp2 <- data.frame(x = round(approx(temp$x, temp$y, ties = "ordered")$x/int)*int, 
                        y = round(approx(temp$x, temp$y, ties = "ordered")$y/int)*int)
  #  print(temp2)
    #Dedupe file
    temp3 <- temp2[!duplicated(temp2),]
  #  print(temp3)
    
    #add a column called 'id' for matching back to orig data
    temp3$id <- data[row_num, c("id")]
  #  print(temp3)
    return(temp3)
    
  }
  
  ##Placeholder file to collect results
  batched <- data.frame()
  
  #Loop through storm events
  for(k in 1:nrow(merged_SE_diff)){
  
  #  print(k)
    
    #get the interpolated line
    temp <-step(merged_SE_diff,2,4,3,5,k,id)
  #  print(temp)
  
     #Add the interpolated results into batched
    batched <- rbind(batched,temp)
  
  }
  
  batched <- rename(batched, LON = x, LAT = y)
  print("head and str of batched")
  print(head(batched))
  print(str(batched))
  
  #Merge by rownum against storm events data
  merged_SE_diff <- merged_SE_diff[,-c(2:5)]
  merged_SE_diff <- merge(merged_SE_diff, batched, by.x=c("id"),by.y=c("id"))
  merged_SE_diff <- select(merged_SE_diff, -id)
  print("str of merged_SE_diff")
  print(str(merged_SE_diff))
  
  
  merged_SE <- rbind(merged_SE_same, merged_SE_diff)
  print("str of merged_SE after recombining with same and diff dfs")
  print(str(merged_SE))
  
  merged_SE$inSE <- 1
  print(summary(merged_SE))
  print("table of minanydamage vs. maxanydamage")
  print(table(merged_SE$minanydamage, merged_SE$maxanydamage))
  save(merged_SE, file = paste("DeDuplicateHailStormEventData_",year, ".rda",
        sep = ""))
}
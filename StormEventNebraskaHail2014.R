
http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/
if (!file.exists("summarySCC_PM25.rds")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)
  unzip(temp, files = "summarySCC_PM25.rds")
  NEI <- readRDS("summarySCC_PM25.rds")
} else {
  NEI <- readRDS("summarySCC_PM25.rds")
}

http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_locations-ftp_v1.0_d2014_c20160223.csv.gz
## Synopsis
This report identifies which storm event categories account for the greatest 
total economic and health impacts in the United States from January 1996 through November 2011.  The first section of the report reviews the considerable amount 
of data processing needed to make the problem more manageable.  In particular, records before 1996 were dropped because that was the first year that data on a more comprehensive set of storm events were collected.  Then the hundreds of
storm event types in the data were collapsed into a set of 15 storm event categories that each included related storm event types.  Economic damages (to property and crops) were converted to 2011 dollars in order to account for inflation over the study period and summed to a single economic damage estimate.  Health effects were estimated separately for injuries and fatalities.  Floods accounted for the greatest economic damages (more than $188 billion in 2011 dollars).  Tornadoes accounted for the largest number of injuries between 1996 and 2011 (almost 21,000).  Finally, extreme temperatures (heat or cold) accounted for the largest number of fatalities (almost 2,200).

## Data Processing
First, I set the working directory:
```{r workdir, echo=TRUE}
setwd("~/JHU Data Science Specialization/Reproducible Research/Project2")
```
Second, I read in the NOAA Storm Data and load dplyr package:
```{r dataread, echo=TRUE}
if (!file.exists("StormData.csv")) {
        temp <- tempfile()
        download.file(paste("https://d396qusza40orc.cloudfront.net/",
                "repdata%2Fdata%2FStormData.csv.bz2", sep=""), temp)
        unzip(temp, files = "StormData.csv")
        StormDataDF <- read.csv("StormData.csv")
} else {
        StormDataDF <- read.csv("StormData.csv")
}
library(dplyr, warn.conflicts = FALSE)
```
Next, I converted the date that storm events began (BGN_DATE) from factor to 
date variableand drop records before 1/1/1996 because that is when the National Climatic Data Center began to collect data on a wider range (48) of 
[storm event categories](http://www.ncdc.noaa.gov/stormevents/details.jsp).  
To reduce some of the multiplicity of event types due to variations in capitalization, I converted the values of EVENTYPE to uppercase.  Then I 
selected only the variables needed for the analysis based on the [file layout](https://ire.org/media/uploads/files/datalibrary/samplefiles/Storm%20Events/layout08.doc) discovered by classmate Robert Carman in the course discussion
thread "Project 2 Tips.""
```{r datashrink, echo=TRUE}
library(lubridate)
StormDataDF <- mutate(StormDataDF, 
        Year = year(mdy_hms(StormDataDF$BGN_DATE)))
StormData1996 <- filter(StormDataDF, Year >= 1996)
#create capitalized version of EVTYPE
StormData1996 <- mutate(StormData1996, EVTYPEcap = toupper(EVTYPE))
StormData1996 <- select(StormData1996, EVTYPE, EVTYPEcap, FATALITIES, INJURIES, 
        PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, Year)
```
Dropping the pre-1996 storm events reduced the size of the storm event dataset 
from `r nrow(StormDataDF)` to `r nrow(StormData1996)` observations and reduced 
the number of unique storm events (as recorded in the EVTYPEcap variable) from 
`r n_distinct(StormDataDF$EVTYPE)` to `r n_distinct(StormData1996$EVTYPEcap)`
.  This is still a lot of storm events (more than the expected 48 listed in the [storm event descriptions](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).  Some of the multiplicity of storm events is because of variant spellings and abbreviations, requiring some additional clean-up of the EVTYPEcap variable; in addition, it may make sense to combine related or similar storm events into a smaller, more manageable set of events.

To help narrow down the number of event categories, it is helpful to do some
exploratory data analysis.  First, I looked at the number of event types in descending order with cumulative totals to see which individual events most frequently occured in the data.  As a check, I also looked at the events with property or crop damage in the $millions or $billions and also which had the highest fatalities or injuries to make sure I wasn't missing low-frequency, high-impact events.  I also looked at whether there were any missing values in 
the property and crop damage estimates and in the numbers of fatalities and
injuries.  I found no missing valus for various damage estimates and also can 
see how many values there are for the economic damage multiplier variables (PROPDMGEXP and CROPDMGEXP).
```{r dataexplore, echo=TRUE}
#look at event types in descending order of frequency
event_list <- count(StormData1996, EVTYPEcap, sort = TRUE)
event_list <- mutate(event_list, cumcount = cumsum(n))
event_list
#Identify event types with high property damage
hipropdmg <- filter(StormData1996, PROPDMGEXP == "M" | PROPDMGEXP == "B")
hipropdmg <- count(hipropdmg, EVTYPEcap, sort = TRUE)
hipropdmg
#Identify event types with high crop damage
hicropdmg <- filter(StormData1996, CROPDMGEXP == "M" | CROPDMGEXP == "B")
hicropdmg <- count(hicropdmg, EVTYPEcap, sort = TRUE)
hicropdmg
#Identify event types with high injuries (greater than 10)
hiinj <- filter(StormData1996, INJURIES > 10)
hiinj <- count(hiinj, EVTYPEcap, sort = TRUE)
hiinj
#Identify event types with high fatalities (greater than 10)
hifatal <- filter(StormData1996, FATALITIES > 10)
hifatal <- count(hifatal, EVTYPEcap, sort = TRUE)
hifatal

#Number of economic, health, and deaths data that are missing
sum(is.na(StormData1996$FATALITIES))
sum(is.na(StormData1996$INJURIES))
sum(is.na(StormData1996$CROPDMG))
sum(is.na(StormData1996$ECONDMG))

```
Based on my observation of the frequency and impacts of various event types, I 
created 14 new event types that encompass most of the most important event 
types originally appearing in the data (trying as hard as possible not to be arbitrary).  All the rest (the 15th category) were classifed as "Other." The following table shows the new event types with the corresponding defining substrings in the original EVENTcap.  The table also shows the order in which 
the reclassifications were done because that matters (e.g., I assigned "FREEZING 
FOG" to "WINTER" rather than to "FOG because it seemed like a better fit).

###Table 1:  Recoding of Storm Event Types
```{r table1, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
library(pander)
tabl <- "  
|newEVTYPE          | Kewords in EVENTcap                                        |
|-------------------|------------------------------------------------------------|
|HAIL               | HAIL                                                       |
|WIND               | WIND, BURST                                                |
|FLOOD              | FLOOD, FLD, DAM BREAK, SEICHE                              |
|TORNADO            | TORNADO, FUNNEL, CLOUD, WATER, SPOUT, GUSTNADO             |
|WINTER             | SNOW, ICE, ICY, FROST, FREEZ, WINT, MIXED, GLAZE, SLEET,   | |WINTER             | BLIZZARD                                                   |
|RAIN               | RAIN, PRECIP, TSTM, THUNDERSTORM, WET                      |
|FIRE               | FIRE, SMOKE, ASH                                           |
|DROUGHT            | DROUGHT, DUST, DRY                                         |
|EXTREME TEMPS      | HEAT, WARM, HOT, COLD, CHILL, HYPOTH, COOL, TEMPER         | |FOG                | FOG                                                        | |HURRICANE          | HURRICANE, TROPICAL, TYPHOON                               |
|AVALANCHE          | AVALANCHE, SLIDE                                           |
|LAKE/MARINE        | SURF, CURRENT, SURGE, TIDE, TSUNAMI, COASTAL               |
|LIGHTNING          | LIGHTN, LIGHTEN                                            | |OTHER              | all other event types                                      |
"
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion
```


```{r newevents, echo = TRUE}
StormDatanew <- mutate(StormData1996, newEVTYPE = EVTYPEcap)
StormDatanew$newEVTYPE[grep("HAIL", StormDatanew$newEVTYPE)] = "HAIL"
x <- c("WIND", "BURST")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "WIND"
}        
x <- c("FLOOD", "FLD", "DAM BREAK", "SEICHE")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "FLOOD"
}
x <- c("TORNADO", "FUNNEL CLOUD", "WATER SPOUT", "WATERSPOUT", "GUSTNADO")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "TORNADO"
}
x <- c("SNOW", "ICE", "ICY", "FROST", "FREEZ", "WINT", "MIXED",
              "GLAZE", "SLEET", "BLIZZARD")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "WINTER"
}
x <- c("RAIN", "PRECIP", "TSTM", "THUNDERSTORM", "WET")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "RAIN"
}
x <- c("FIRE", "SMOKE", "ASH")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "FIRE"
}
x <- c("DROUGHT", "DUST", "DRY")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = "DROUGHT"
}
x <- c("HEAT", "WARM", "HOT", "COLD", "CHILL", "HYPOTH", "COOL", "TEMPER")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = 
                "EXTREME TEMPERATURES"
}
StormDatanew$newEVTYPE[grep("FOG", StormDatanew$newEVTYPE)] = "FOG"
x <- c("HURRICANE", "TROPICAL", "TYPHOON")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = 
                "HURRICANE"
}
x <- c("AVALANCHE", "SLIDE")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = 
                "AVALANCHE"
}
x <- c("SURF", "CURRENT", "SURGE", "TIDE", "TSUNAMI", "COASTAL")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = 
                "LAKE/MARINE"
}
x <- c("LIGHTN", "LIGHTEN")
for(i in seq_along(x)){
        StormDatanew$newEVTYPE[grep(x[i], StormDatanew$newEVTYPE)] = 
                "LIGHTNING"
}
x <- !(StormDatanew$newEVTYPE == "HAIL" | StormDatanew$newEVTYPE == "WIND"
        | StormDatanew$newEVTYPE == "FLOOD"
        | StormDatanew$newEVTYPE == "TORNADO"
        | StormDatanew$newEVTYPE == "WINTER"
        | StormDatanew$newEVTYPE == "RAIN"
        | StormDatanew$newEVTYPE == "FIRE"
        | StormDatanew$newEVTYPE == "DROUGHT"
        | StormDatanew$newEVTYPE == "EXTREME TEMPERATURES"
        | StormDatanew$newEVTYPE == "FOG"
        | StormDatanew$newEVTYPE == "HURRICANE"
        | StormDatanew$newEVTYPE == "AVALANCHE"
        | StormDatanew$newEVTYPE == "LAKE/MARINE"
        | StormDatanew$newEVTYPE == "LIGHTNING")      
StormDatanew$newEVTYPE[x] = "OTHER"
```
Next calculate economic damages.  To be consistent across years by taking inflation into account, I obtained Consumer Price Index information from the [Bureau of Labor Statistics](www.bls.gov) as shown in Table 2, put the inflation 
indices in a data frame, and computed factors to convert dollars into 2011 
dollars.  I then merged the inflation data to the storm event data by Year.

###Table 2: Consumer Price Indices, 1996-2011 
Year |	Annual Index
-----|--------------
1996 |	156.9				
1997 |	160.5				
1998 |	163.0				
1999 |	166.6				
2000 |	172.2				
2001 |	177.1				
2002 |	179.9				
2003 |	184.0				
2004 |	188.9				
2005 |	195.3				
2006 |	201.6				
2007 |	207.342				
2008 |	215.303				
2009 |	214.537				
2010 |	218.056				
2011 |	224.939				
```{r inflation, echo=TRUE}
CPI <- read.csv("SeriesReport-20160127164508_a1da2c.csv")
CPI$deflator <- CPI$Annual[CPI$Year == 2011]/CPI$Annual
StormDatanew <- merge(CPI, StormDatanew, all=TRUE)
```

Next, create numeric dollar multipliers based on the coded multipliers and then
calculate total economic damage in 2011 dollars.  Then calculate total economic damages, injuries, and fatalities by newEVTYPE:
```{r new_multipliers, echo=TRUE}
StormDatanew$newPROPDMGEXP <- 1
StormDatanew$newPROPDMGEXP[StormDatanew$PROPDMGEXP == "H"] <- 100
StormDatanew$newPROPDMGEXP[StormDatanew$PROPDMGEXP == "K"] <- 1000
StormDatanew$newPROPDMGEXP[StormDatanew$PROPDMGEXP == "M"] <- 1000000
StormDatanew$newPROPDMGEXP[StormDatanew$PROPDMGEXP == "B"] <- 1000000000

StormDatanew$newCROPDMGEXP <- 1
StormDatanew$newCROPDMGEXP[StormDatanew$CROPDMGEXP == "H"] <- 100
StormDatanew$newCROPDMGEXP[StormDatanew$CROPDMGEXP == "K"] <- 1000
StormDatanew$newCROPDMGEXP[StormDatanew$CROPDMGEXP == "M"] <- 1000000
StormDatanew$newCROPDMGEXP[StormDatanew$CROPDMGEXP == "B"] <- 1000000000

StormDatanew <- mutate(StormDatanew, TotEconDmg = 
        deflator*((PROPDMG*newPROPDMGEXP) + (CROPDMG*newCROPDMGEXP)))
#ByNewEVTYPE <- group_by(StormDatanew, as.factor(newEVTYPE))
ByNewEVTYPE <- group_by(StormDatanew, newEVTYPE)
SumNewEVTYPE <- summarize(ByNewEVTYPE, SumEconDmg = sum(TotEconDmg),
                          SumInjuries = sum(INJURIES), 
                        SumFatalities = sum(FATALITIES))
SumNewEVTYPE <- mutate(SumNewEVTYPE, EconDmgBil = SumEconDmg/1000000000)
SumNewEVTYPE$EconDmgBil = round(SumNewEVTYPE$EconDmgBil, digits = 2) 
SumNewEVTYPE
```
Then calculate total economic damages, injuries, and fatalities by newEVTYPE:

## Results
In this section I report visually which storm event categories have the largest
economic and health (in terms of injuries and fatalities) impacts using three bar charts.
First, Figure 1 below shows that `r SumNewEVTYPE$newEVTYPE[which(max(SumNewEVTYPE$EconDmgBil) ==                              SumNewEVTYPE$EconDmgBil)] `s had the highest total (crop and property) economic damage ($`r max(SumNewEVTYPE$EconDmgBil)` billion in 2011 dollars) of 
all the storm event categories.
```{r charts, echo=TRUE}
library("ggplot2")
library("grid")
p <- ggplot(SumNewEVTYPE, 
                aes(x = reorder(newEVTYPE, EconDmgBil), 
                y = EconDmgBil)) + geom_bar(stat="identity")
p <- p + ggtitle("Figure 1: Total Economic Damages in the U.S. by Storm Event Category
        January 1996-November 2011 in Billions of $2011")
p <- p + annotate("text", x=12, y=160, label="maximum\n economic\n damage")
p <- p + annotate("segment", x=13, xend = 15, y=163, yend = 186, 
        color = "red", size=2, arrow=arrow())
p <- p + geom_text(aes(label=EconDmgBil), vjust = -0.2, color = "black")
p <- p + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
p <- p + xlab("Storm Event Categories") + ylab("Economic Damages in Billions of 2011 Dollars")
print(p)
```

Figure 2 shows that `r SumNewEVTYPE$newEVTYPE[which(max(SumNewEVTYPE$SumInjuries) == SumNewEVTYPE$SumInjuries)] `s had the highest total number of injuries (`r sprintf("%5.0f", max(SumNewEVTYPE$SumInjuries))`) of all the storm event categories.
```{r injuries, echo=TRUE}
p <- ggplot(SumNewEVTYPE, 
                aes(x = reorder(newEVTYPE, SumInjuries), 
                y = SumInjuries)) + geom_bar(stat="identity")
p <- p + ggtitle("Figure 2: Total Injuries in the U.S. by Storm Event Category
        January 1996-November 2011")
p <- p + annotate("text", x=12, y=16000, label="maximum\n number of \n injuries")
p <- p + annotate("segment", x=13, xend = 15, y=15000, yend = 20000, 
        color = "red", size=2, arrow=arrow())
p <- p + geom_text(aes(label=SumInjuries), vjust = -0.2, color = "black")
p <- p + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
p <- p + xlab("Storm Event Categories") + ylab("Number of Injuries")
print(p)
```

Finally, Figure 3 shows that `r SumNewEVTYPE$newEVTYPE[which(max(SumNewEVTYPE$SumFatalities) == SumNewEVTYPE$SumFatalities)] `s had the highest total number of fatalities (`r sprintf("%5.0f", max(SumNewEVTYPE$SumFatalities))`) of all the storm event categories.
```{r fatalities, echo=TRUE}
p <- ggplot(SumNewEVTYPE, 
                aes(x = reorder(newEVTYPE, SumFatalities), 
                y = SumFatalities)) + geom_bar(stat="identity")
p <- p + ggtitle("Figure 3: Total Fatalities in the U.S. by Storm Event Category
        January 1996-November 2011")
p <- p + annotate("text", x=12, y=1800, label="maximum\n number of \n fatalities")
p <- p + annotate("segment", x=13, xend = 15, y=1800, yend = 2100, 
        color = "red", size=2, arrow=arrow())
p <- p + geom_text(aes(label=SumFatalities), vjust = -0.2, color = "black")
p <- p + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
p <- p + xlab("Storm Event Categories") + ylab("Number of Fatalities")
print(p)
```


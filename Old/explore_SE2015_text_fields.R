#exploring text fields in 2015 Storm Events details dataset
setwd("~/StormProject")
library(dplyr)
load("C:/Users/David/Documents/StormProject/StormEvents_details_2014.rda")
head(SData)
dim(SData)
SData <- mutate(SData, EVENT_TYPE = toupper(EVENT_TYPE), 
                EPISODE_NARRATIVE = toupper(EPISODE_NARRATIVE),
                EVENT_NARRATIVE = toupper(EVENT_NARRATIVE)
                )
SData <- mutate(SData, EventTypeIsHail = grepl("HAIL", EVENT_TYPE),
                NarrativeIsHail = grepl("HAIL", EPISODE_NARRATIVE) | 
                        grepl("HAIL", EVENT_NARRATIVE)
                        )
table(SData$EventTypeIsHail, SData$NarrativeIsHail)
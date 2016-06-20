# FCC's Census Block Conversions API for geocoding 1-degree NOAA data
# http://www.fcc.gov/developers/census-block-conversions-api
library(dplyr)
library(data.table)

latlong2fipsdb <- function(latitude, longitude) {
        url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
        url <- sprintf(url, latitude, longitude)
        json <- RCurl::getURL(url)
        json <- RJSONIO::fromJSON(json)
        c(json$Block, json$County[2], json$State[2:3])
}

load("SWDI_SE_merge_2011_2015_1deg.rda")
print(dim(batched_1deg))
S_coords_1deg <- select(batched_1deg, LON, LAT)
S_coords_1deg <- distinct(S_coords_1deg)
print(dim(S_coords_1deg))

n <- dim(S_coords_1deg)[1]
y <- data.table(
        FIPS = character(n),
        County = character(n),
        State_abb = character(n),
        State = character(n),
        LAT = numeric(n),
        LON = numeric(n)
        )

        for(i in 1:n){
        print(i)        
             z <- latlong2fipsdb(S_coords_1deg$LAT[i], S_coords_1deg$LON[i]) 
             if(is.null(z[[1]])==FALSE) {
                for(j in 1:4){
                        k=as.integer(j)
                        set(y, i, k, z[j])        
                }
             } else {
                for(j in 1:4){
                        k=as.integer(j)
                        set(y, i, k, NA)        
                     }             
             }
             set(y, i, 5L, S_coords_1deg$LAT[i])
             set(y, i, 6L, S_coords_1deg$LON[i])
             #Sys.sleep(2)
        }
y <- as.data.frame(y)
S_geocoded_1deg <- merge(y, batched_1deg, all = TRUE)
print("dimensions of S_geocoded_1deg")
print(dim(S_geocoded_1deg))
print("number of S_geocoded_1deg records with NAs")
print(sum(is.na(S_geocoded_1deg[1])==TRUE))
save(S_geocoded_1deg, file= "SWDI_SE_geo_1deg.rda")

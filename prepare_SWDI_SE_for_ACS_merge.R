# FCC's Census Block Conversions API
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

load("SWDI_SE_merge_2011_2015.rda")
print(dim(batched))
S_coords <- select(batched, LON, LAT)
S_coords <- distinct(S_coords)
print(dim(S_coords))
#n <- 100
n <- dim(S_coords)[1]
y <- data.table(
        FIPS = character(n),
        County = character(n),
        State_abb = character(n),
        State = character(n),
        LAT = numeric(n),
        LON = numeric(n)
        )
n_mod_200 <- n/200
w <- 1
while(w <= n)
        for(i in 1:n){
        print(i)        
             z <- latlong2fipsdb(S_coords$LAT[i], S_coords$LON[i]) 
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
             set(y, i, 5L, S_coords$LAT[i])
             set(y, i, 6L, S_coords$LON[i])
             #Sys.sleep(2)
        }
y <- as.data.frame(y)
S_geocoded <- merge(y, batched, all = TRUE)
print("dimensions of S_geocoded")
print(dim(S_geocoded))
print("number of S_geocoded records with NAs")
print(sum(is.na(S_geocoded[1])==TRUE))
save(S_geocoded, "SWDI_SE_geo.rda")